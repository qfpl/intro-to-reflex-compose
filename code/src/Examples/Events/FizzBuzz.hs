{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Examples.Events.FizzBuzz (
    attachFizzBuzzExamples
  ) where

import Data.Monoid ((<>))

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core
import GHCJS.DOM.Types (MonadJSM)

import Util.Attach
import Util.Reflex
import Util.Number

import Util.Runner

attachFizzBuzzExamples ::
  MonadJSM m =>
  m ()
attachFizzBuzzExamples = do
  attachId_ "examples-events-fizz-and-buzz" $ demoFizzBuzz $
    fizzAndBuzz
  attachId_ "examples-events-leftmost" $ demoFizzBuzz $
    fizzBuzzLeftmost
  attachId_ "examples-events-mergeWith" $ demoFizzBuzz $
    fizzBuzzMergeWith
  attachId_ "examples-events-merge" $ demoFizzBuzz $
    fizzBuzzMerge
  attachId_ "examples-events-fizzbuzz" $ demoFizzBuzz $
    fizzBuzz
  attachId_ "examples-events-fizzbuzz-flip" $ demoFizzBuzz $
    fizzBuzzFlip

layoutPair ::
  MonadWidget t m =>
  Text ->
  m a ->
  m a
layoutPair label mLabel = do
  el "tr" $ do
    elClass "td" "fizzbuzz-pair-left" . el "pre" $
      text label
    elClass "td" "fizzbuzz-pair-right" . el "pre" $
      mLabel

wrapPairs ::
  MonadWidget t m =>
  m a ->
  m a
wrapPairs =
  el "table"

fizzBuzzPart ::
  MonadWidget t m =>
  Event t () ->
  Event t Text ->
  Text ->
  m ()
fizzBuzzPart eTick eLabel label = do
  dLabel <- holdDyn "" .
            leftmost $
            [ eLabel , "" <$ eTick]
  layoutPair label (dynText dLabel)

fizzAndBuzzBits ::
  MonadWidget t m =>
  m (Event t Int, Event t Text, Event t Text)
fizzAndBuzzBits = do
  num <- layoutPair "eCount" $
    numberInput def

  let
    eCount = floor <$> _numberInput_input num
    eCountText = (Text.pack . show) <$> eCount
    eTick = () <$ eCount

  let
    eFizz = "Fizz" <$ ffilter ((== 0) . (`mod` 3)) eCount

  fizzBuzzPart eTick eFizz "eFizz"

  let
    eBuzz = "Buzz" <$ ffilter ((== 0) . (`mod` 5)) eCount

  fizzBuzzPart eTick eBuzz "eBuzz"

  pure (eCount, eFizz, eBuzz)

fizzAndBuzz ::
  MonadWidget t m =>
  m ()
fizzAndBuzz = wrapPairs $ do
  _ <- fizzAndBuzzBits
  pure ()

fizzBuzzLeftmost ::
  MonadWidget t m =>
  m ()
fizzBuzzLeftmost = wrapPairs $ do
  (eCount, eFizz, eBuzz) <- fizzAndBuzzBits

  let
    eTick = () <$ eCount
    eLeft = leftmost [eFizz, eBuzz]

  fizzBuzzPart eTick eLeft "eLeft"

fizzBuzzMergeWith ::
  MonadWidget t m =>
  m ()
fizzBuzzMergeWith = wrapPairs $ do
  (eCount, eFizz, eBuzz) <- fizzAndBuzzBits

  let
    eTick = () <$ eCount
    eMerge = mergeWith (<>) [eFizz, eBuzz]

  fizzBuzzPart eTick eMerge "eMergeWith"

fizzBuzzMerge ::
  MonadWidget t m =>
  m ()
fizzBuzzMerge = wrapPairs $ do
  (eCount, eFizz, eBuzz) <- fizzAndBuzzBits

  let
    eTick = () <$ eCount

  let
    eMerge      = eFizz <> eBuzz

  fizzBuzzPart eTick eMerge "eMerge"

fizzBuzz ::
  MonadWidget t m =>
  m ()
fizzBuzz = wrapPairs $ do
  (eCount, eFizz, eBuzz) <- fizzAndBuzzBits

  let
    eCountText = (Text.pack . show) <$> eCount
    eTick = () <$ eCount

  let
    eFizzBuzz = leftmost [eFizz <> eBuzz, eCountText]

  fizzBuzzPart eTick eFizzBuzz "eFizzBuzz"

fizzBuzzFlip ::
  MonadWidget t m =>
  m ()
fizzBuzzFlip = wrapPairs $ do
  (eCount, eFizz, eBuzz) <- fizzAndBuzzBits

  let
    eCountText = (Text.pack . show) <$> eCount
    eTick = () <$ eCount

  let
    eMerge      = eFizz <> eBuzz

  fizzBuzzPart eTick eMerge "eMerge"

  let
    eDiff = difference eCountText eMerge

  fizzBuzzPart eTick eDiff "eDiff"

  let
    eFizzBuzzFlip = leftmost [eDiff, eMerge]

  fizzBuzzPart eTick eFizzBuzzFlip "eFizzBuzz"

demoFizzBuzz ::
  MonadWidget t m =>
  m () ->
  m ()
demoFizzBuzz w = divClass "panel panel-default" . divClass "panel-body" $ mdo
  _ <- widgetHold w (w <$ eReset)
  eReset <- el "div" $
    buttonClass "btn btn-default pull-right" "Reset"
  pure ()
