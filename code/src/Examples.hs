{-# LANGUAGE OverloadedStrings #-}
module Examples (
    examples
  ) where

import Control.Monad.Reader (runReaderT)

import GHCJS.DOM.Types (JSM)

import Util.Attach
import Util.Grid

import Examples.Events (attachEventExamples)
import Examples.Behaviors (attachBehaviorExamples)
import Examples.Dynamics (attachDynamicExamples)
import Examples.RecursiveDo (attachRecursiveDoExamples)
import Examples.Dom (attachDomExamples)
import Examples.Switch (attachSwitchExamples)
import Examples.Collection (attachCollectionExamples)

examples :: JSM ()
examples = do
  attachId_ "grid-setup" $
    setupGrid defaultGridConfig
  attachEventExamples
  attachBehaviorExamples
  attachDynamicExamples
  attachRecursiveDoExamples
  attachDomExamples
  attachSwitchExamples
  attachCollectionExamples

