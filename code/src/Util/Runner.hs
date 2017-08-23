{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Util.Runner (
    runner
  ) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Foldable (traverse_)

import Reflex.Dom.Core

import Util.Grid

#ifndef ghcjs_HOST_OS
import           Network.Wai.Handler.Warp               (defaultSettings,
                                                         runSettings, setPort,
                                                         setTimeout)
import           Network.WebSockets                     (defaultConnectionOptions)

import           Language.Javascript.JSaddle.Run        (syncPoint)
import           Language.Javascript.JSaddle.WebSockets
import           Network.Wai.Middleware.Static

import           System.FilePath                        ((</>))
import           System.Directory                       (listDirectory)
import qualified Data.Text                              as Text
import qualified Data.Map                               as Map

#endif

wrapW :: Widget x () -> Widget x ()
wrapW w = do
  setupGrid defaultGridConfig
  w

-- want a head section that makes everything in css available
-- want a warp app serving ./css at ./css
-- probably make the css dir an input to a function and give it a default for runner

runner' ::
  FilePath ->
  Int ->
  (forall x. Widget x ()) ->
  IO ()
runner' cssPath port w =
#ifdef ghcjs_HOST_OS
  let
    f = mainWidget $ wrapW w
  in
    f
#else
  do
    cssFiles <- listDirectory $ "." </> cssPath
    let
      f = do
        let
          stylesheet s =
            elAttr "link" (Map.fromList [("rel", "stylesheet"), ("href", s)]) $
              return ()
        mainWidgetWithHead
          (traverse_ (\f -> stylesheet . Text.pack $ cssPath </> f) cssFiles)
          (wrapW w)
      serveFiles = staticPolicy $ hasPrefix cssPath

    app <- jsaddleOr defaultConnectionOptions (f >> syncPoint) jsaddleApp
    runSettings (setPort port (setTimeout 3600 defaultSettings)) $
      serveFiles app
#endif

runner ::
  (forall x. Widget x ())
  -> IO ()
runner =
  runner' "css" 8080
