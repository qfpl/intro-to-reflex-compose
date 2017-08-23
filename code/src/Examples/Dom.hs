{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
module Examples.Dom (
    attachDomExamples
  ) where

import Control.Monad (void)

import Control.Lens
import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core
import GHCJS.DOM.Types (MonadJSM)

import Util.Attach

todoExample ::
  MonadWidget t m =>
  m ()
todoExample =
  el "div" $
    text "TODO"

todoItem0 ::
  MonadWidget t m =>
  Text ->
  m (Event t ())
todoItem0 placeholder =
  el "div" $ do
    el "div" $
      text placeholder
    button "Remove"

todoItemExample1 ::
  MonadWidget t m =>
  m ()
todoItemExample1 = do
  _ <- todoItem0 "TODO"
  pure ()

todoItemExample2 ::
  MonadWidget t m =>
  m ()
todoItemExample2 = el "div" $ mdo
  el "div" $
    dynText dLabel

  eRemove <- el "div" $
    todoItem0 "TODO"

  dLabel <- holdDyn "" $
    "Removed:" <$ eRemove

  pure ()

todoItem3 ::
  MonadWidget t m =>
  Text ->
  m (Event t ())
todoItem3 placeholder =
  elClass "div" "todo-item" $ mdo

    elDynClass "div" dClass $
      text placeholder

    eRemove <- button "Remove"

    dClass <- holdDyn "" $
      "removed" <$ eRemove

    pure eRemove

todoItemExample3 ::
  MonadWidget t m =>
  m ()
todoItemExample3 = do
  _ <- todoItem3 "TODO"
  pure ()

data TodoItemConfig =
  TodoItemConfig {
    _todoItemConfig_initialValue :: Text
  }

data TodoItem t =
  TodoItem {
    _todoItem_dComplete :: Dynamic t Bool
  , _todoItem_eRemove :: Event t ()
  }

makeLenses ''TodoItem

complete ::
  MonadWidget t m =>
  m (Dynamic t Bool)
complete = do
  cb <- checkbox False def
  pure $ cb ^. checkbox_value

edit ::
  MonadWidget t m =>
  Dynamic t Text ->
  Text ->
  m ()
edit dClass initialValue = do
  elDynClass "span" dClass $
    text initialValue

todoItem4 ::
  MonadWidget t m =>
  TodoItemConfig ->
  m (TodoItem t)
todoItem4 (TodoItemConfig initialValue) =
  elClass "div" "todo-item" $ mdo

    cb <- checkbox False def
    let
      dComplete = cb ^. checkbox_value

    elDynClass "div" dRemoveClass $
      text initialValue

    eRemove <- button "Remove"

    dRemoveClass <- holdDyn "" $
      "removed" <$ eRemove

    pure $
      TodoItem dComplete eRemove

todoItemExample4 ::
  MonadWidget t m =>
  m ()
todoItemExample4 = do
  _ <- todoItem4 $ TodoItemConfig "TODO"
  pure ()

todoItem5 ::
  MonadWidget t m =>
  TodoItemConfig ->
  m (TodoItem t)
todoItem5 (TodoItemConfig initialValue) =
  elClass "div" "todo-item" $ mdo

    cb <- checkbox False def
    let
      dComplete = cb ^. checkbox_value
      mkCompleteClass False = ""
      mkCompleteClass True  = "completed "
      dCompleteClass = mkCompleteClass <$> dComplete

    elDynClass "div" (dCompleteClass <> dRemoveClass) $
      text initialValue

    eRemove <- button "Remove"

    dRemoveClass <- holdDyn "" $
      "removed " <$ eRemove

    pure $
      TodoItem dComplete eRemove

todoItemExample5 ::
  MonadWidget t m =>
  m ()
todoItemExample5 = do
  _ <- el "div" $ todoItem5 $ TodoItemConfig "TODO"
  pure ()

getKey :: Reflex t => TextInput t -> Key -> Event t ()
getKey ti k =
  void .
  ffilter ((== k) . keyCodeLookup . fromIntegral) $
  ti ^. textInput_keypress

addItemWidget ::
  MonadWidget t m =>
  m (Event t Text)
addItemWidget = mdo
  ti <- textInput $
    def & textInputConfig_attributes .~
            pure ("placeholder" =: "What shall we do today?")
        & textInputConfig_setValue .~
            ("" <$ eClear)

  let
    bValue = current $ ti ^. textInput_value
    eAtEnter = bValue <@ getKey ti Enter
    eDone = ffilter (not . Text.null) eAtEnter
    eClear = leftmost [void eDone, getKey ti Escape]

  pure eDone

todoItemExample6 ::
  MonadWidget t m =>
  m ()
todoItemExample6 = elClass "div" "add-item-wrapper" $ do

  eText <- el "div"
    addItemWidget

  dText <- holdDyn ""
    eText

  el "div" $
    dynText dText

  pure ()

attachDomExamples ::
  MonadJSM m =>
  m ()
attachDomExamples = do
  attachId_ "examples-dom-todo"
    todoExample
  attachId_ "examples-dom-todoitem-1"
    todoItemExample1
  attachId_ "examples-dom-todoitem-2"
    todoItemExample2
  attachId_ "examples-dom-todoitem-3"
    todoItemExample3
  attachId_ "examples-dom-todoitem-4"
    todoItemExample4
  attachId_ "examples-dom-todoitem-5"
    todoItemExample5
  attachId_ "examples-dom-todoitem-6"
    todoItemExample6
