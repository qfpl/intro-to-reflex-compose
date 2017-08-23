{-# LANGUAGE OverloadedStrings #-}
module Main where

import Util.Attach
import Util.Grid

import Examples.Events (attachEventExamples)
import Examples.Behaviors (attachBehaviorExamples)

main :: IO ()
main = do
  attachId "talk-setup" $
    setupGrid defaultGridConfig
  attachEventExamples Bootstrap
  attachBehaviorExamples
