{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Examples.Components (
    attachComponentExamples
  ) where

import GHCJS.DOM.Types (MonadJSM)

import Examples.Components.Pass1
import Examples.Components.Pass2
import Examples.Components.Pass3

attachComponentExamples ::
  MonadJSM m =>
  m ()
attachComponentExamples = do
  attachComponentExamplesPass1
