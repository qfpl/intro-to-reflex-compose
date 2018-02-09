{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
module Examples.Switch (
    attachSwitchExamples
  ) where

import Control.Monad (void)

import Control.Lens

import Control.Monad.Trans (liftIO)
import Data.Time (getCurrentTime)

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core
import GHCJS.DOM.Types (MonadJSM)

import Util.Attach
import Util.Grid
import Colour

attachSwitchExamples ::
  MonadJSM m =>
  m ()
attachSwitchExamples = do
  attachId_ "examples-switch-colour-1" $
    switchColourExample switchColour1
  attachId_ "examples-switch-colour-2" $
    switchColourExample switchColour2

  attachId_ "examples-switch-demo-text" $
    demoText
  attachId_ "examples-switch-demo-button" $
    demoButton
  attachId_ "examples-switch-demo-tick" $
    demoTick

  attachId_ "examples-switch-hide-button" $
    hideExample buttonWidget
  attachId_ "examples-switch-hold-button" $
    holdExample buttonWidget

  attachId_ "examples-switch-hide-tick" $
    hideExample tickWidget
  attachId_ "examples-switch-hold-tick" $
    holdExample tickWidget

  attachId_ "examples-switch-workflow-button" $
    workflowExample buttonWidget
  attachId_ "examples-switch-workflow-tick" $
    workflowExample tickWidget

  attachId_ "examples-switch-workflow-1"
    workflowExample1
  attachId_ "examples-switch-workflow-2"
    workflowExample2

switchColour1 ::
  ( Reflex t
  , MonadHold t m
  ) =>
  Event t () ->
  Event t () ->
  Event t Colour ->
  m (Event t Colour, Event t Colour)
switchColour1 eSwitch1 eSwitch2 eInput = do
  eOut1 <- switchHold eInput . leftmost $ [
      eInput <$ eSwitch1
    , never  <$ eSwitch2
    ]

  eOut2 <- switchHold never . leftmost $ [
      never  <$ eSwitch1
    , eInput <$ eSwitch2
    ]

  pure (eOut1, eOut2)

switchColour2 ::
  ( Reflex t
  , MonadHold t m
  ) =>
  Event t () ->
  Event t () ->
  Event t Colour ->
  m (Event t Colour, Event t Colour)
switchColour2 eSwitch1 eSwitch2 eInput = do
  bOut1 <- hold eInput . leftmost $ [
      eInput <$ eSwitch1
    , never  <$ eSwitch2
    ]

  bOut2 <- hold never . leftmost $ [
      never  <$ eSwitch1
    , eInput <$ eSwitch2
    ]

  pure (switch bOut1, switch bOut2)

switchColourExample ::
  MonadWidget t m =>
  (Event t () -> Event t () -> Event t Colour -> m (Event t Colour, Event t Colour)) ->
  m ()
switchColourExample guest = el "div" $ mdo
  eSwitch1 <- el "div" $ do
    eSwitch <- button "Switch"
    drawGrid defaultGridConfig [ Row "eOutput" 1 dOut1 ]
    pure eSwitch

  eSwitch2 <- el "div" $ do
    eSwitch <- button "Switch"
    drawGrid defaultGridConfig [ Row "eOutput" 1 dOut2 ]
    pure eSwitch

  eInput <- mkRedBlueInput

  (eOut1, eOut2) <- guest eSwitch1 eSwitch2 eInput

  dOut1 <- foldDyn (:) [] . leftmost $ [
      Just <$> eOut1
    , Nothing <$ eOut2
    ]

  dOut2 <- foldDyn (:) [] . leftmost $ [
      Just <$> eOut2
    , Nothing <$ eOut1
    ]

  pure ()

textWidget ::
  MonadWidget t m =>
  m (Event t Text)
textWidget = do
  ti <- textInput def
  pure $ ti ^. textInput_input

buttonWidget ::
  MonadWidget t m =>
  m (Event t Text)
buttonWidget = do
  eClick <- button "OK"
  pure $ "OK" <$ eClick

tickWidget ::
  MonadWidget t m =>
  m (Event t Text)
tickWidget = do
  now <- liftIO getCurrentTime
  eTick <- tickLossy 1 now
  el "div" $ text "Ticking..."
  pure $ (Text.pack . show . _tickInfo_n) <$> eTick

demoText ::
  MonadWidget t m =>
  m ()
demoText = do
  eText <- textWidget
  dText <- holdDyn "" eText
  el "div" $
    dynText dText

demoButton ::
  MonadWidget t m =>
  m ()
demoButton = do
  eText <- buttonWidget
  dText <- holdDyn "" eText
  el "div" $
    dynText dText

demoTick ::
  MonadWidget t m =>
  m ()
demoTick = do
  eText <- tickWidget
  dText <- holdDyn "" eText
  el "div" $
    dynText dText

hideExample ::
  MonadWidget t m =>
  m (Event t Text) ->
  m ()
hideExample w = elClass "div" "widget-hold-wrapper" $ do
  eSwitch <- el "div" $
    button "Switch"
  dToggle <- toggle True eSwitch

  let
    dNotToggle = not <$> dToggle

    mkHidden False = "hide"
    mkHidden True  = ""

    dHide1 = mkHidden <$>    dToggle
    dHide2 = mkHidden <$> dNotToggle

  eText1 <- elDynClass "div" dHide1 $
    textWidget

  eText2 <- elDynClass "div" dHide2 $
    w

  let
    eText =
      leftmost [
          gate (current    dToggle) eText1
        , gate (current dNotToggle) eText2
        ]

  dText <- holdDyn "" . leftmost $ [
               eText
             , "" <$ eSwitch
             ]
  el "div" $
    dynText dText

  pure ()

holdExample ::
  MonadWidget t m =>
  m (Event t Text) ->
  m ()
holdExample w = elClass "div" "widget-hold-wrapper" $ do
  eSwitch <- el "div" $
    button "Switch"
  dToggle <- toggle True eSwitch

  let
    dNotToggle = not <$> dToggle
    eShow1  = ffilter id . updated $ dToggle
    eShow2  = ffilter id . updated $ dNotToggle

  deText <- widgetHold textWidget . leftmost $ [
      textWidget <$ eShow1
    , w          <$ eShow2
    ]

  let
    eText  = switchDyn deText

  dText <- holdDyn "" . leftmost $ [
               eText
             , "" <$ eSwitch
             ]

  el "div"$
    dynText dText

  pure ()

workflowExample ::
  forall t m.
  MonadWidget t m =>
  m (Event t Text) ->
  m ()
workflowExample w = elClass "div" "widget-hold-wrapper" $ do
  eSwitch <- el "div" $
    button "Switch"
  dToggle <- toggle True eSwitch

  let
    dNotToggle = not <$> dToggle
    eShow1  = ffilter id . updated $ dToggle
    eShow2  = ffilter id . updated $ dNotToggle

    wf1 :: Workflow t m (Event t Text)
    wf1 = Workflow $ do
      eText <- textWidget
      pure (eText, wf2 <$ eShow2)

    wf2 :: Workflow t m (Event t Text)
    wf2 = Workflow $ do
      eText <- w
      pure (eText, wf1 <$ eShow1)

  deText <- workflow wf1

  let
    eText  = switchDyn deText

  dText <- holdDyn "" . leftmost $ [
               eText
             , "" <$ eSwitch
             ]

  el "div"$
    dynText dText

  pure ()

workflowExample1 ::
  forall t m.
  MonadWidget t m =>
  m ()
workflowExample1 = elClass "div" "widget-hold-wrapper" $ mdo
  let
    wf1 :: Workflow t m (Event t Text)
    wf1 = Workflow $ do
      eText <- textWidget
      pure (eText, wf2 <$ eSwitch)

    wf2 :: Workflow t m (Event t Text)
    wf2 = Workflow $ do
      eText <- buttonWidget
      pure (eText, wf3 <$ eSwitch)

    wf3 :: Workflow t m (Event t Text)
    wf3 = Workflow $ do
      eText <- tickWidget
      pure (eText, wf1 <$ eSwitch)

  deText <- workflow wf1

  eSwitch <- el "div" $
    button "Switch"

  let
    eText  = switchDyn deText

  dText <- holdDyn "" . leftmost $ [
               eText
             , "" <$ eSwitch
             ]

  el "div"$
    dynText dText

  pure ()

workflowExample2 ::
  forall t m.
  MonadWidget t m =>
  m ()
workflowExample2 = elClass "div" "widget-hold-wrapper" $ do

  let
    wf1 :: Workflow t m (Event t Text)
    wf1 = Workflow $ do
      eText <- textWidget
      eNext <- el "div" $ button "Next"
      let eOut = leftmost [eText, "" <$ eNext]
      pure (eOut, wf2 <$ eNext)

    wf2 :: Workflow t m (Event t Text)
    wf2 = Workflow $ do
      eText <- buttonWidget
      eBack <- el "div" $ button "Back"
      eNext <- el "div" $ button "Next"
      let eOut = leftmost [eText, "" <$ eBack, "" <$ eNext]
      pure (eOut, leftmost [wf1 <$ eBack, wf3 <$ eNext])

    wf3 :: Workflow t m (Event t Text)
    wf3 = Workflow $ do
      eText <- tickWidget
      eBack <- el "div" $ button "Back"
      let eOut = leftmost [eText, "" <$ eBack]
      pure (eOut, wf2 <$ eBack)

  deText <- workflow wf1

  let
    eText = switchDyn deText

  dText <- holdDyn "" eText
  el "div"$
    dynText dText

  pure ()

