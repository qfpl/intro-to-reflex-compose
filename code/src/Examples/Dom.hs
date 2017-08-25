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

todoItem1 ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t ())
todoItem1 dText =
  el "div" $ do
    el "div" $ dynText dText
    button "Remove"

todoItemExample2 ::
  MonadWidget t m =>
  m ()
todoItemExample2 = el "div" $ mdo
  eRemove <- el "div" $
    todoItem1 $ pure "TODO" <> dLabel

  dLabel <- holdDyn "" $
    " (Removed)" <$ eRemove

  pure ()

todoItem3 ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t ())
todoItem3 dText =
  elClass "div" "todo-item" $ mdo

    elDynClass "div" dClass $
      dynText dText

    eRemove <- button "Remove"

    dClass <- holdDyn "" $
      "removed" <$ eRemove

    pure eRemove

todoItemExample3 ::
  MonadWidget t m =>
  m ()
todoItemExample3 = do
  _ <- todoItem3 $ pure "TODO"
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

clearComplete ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t ())
clearComplete dAnyComplete =
  let
    mkClass False = "hide"
    mkClass True  = ""
    dClass = mkClass <$> dAnyComplete
  in
    elDynClass "div" dClass $
      button "Clear complete"

clearCompleteExample ::
  MonadWidget t m =>
  m ()
clearCompleteExample = elClass "div" "add-item-wrapper" $ do
  cb1 <- el "div" $ checkbox False def

  cb2 <- el "div" $ checkbox False def

  let
    dComplete1 = cb1 ^. checkbox_value
    dComplete2 = cb2 ^. checkbox_value
    dAnyComplete = (||) <$> dComplete1 <*> dComplete2

  eClearComplete <- el "div" $ clearComplete dAnyComplete

  dText <- holdDyn "" $ "Cleared" <$ eClearComplete
  el "div" $ dynText dText

  pure ()

markAllComplete ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t Bool)
markAllComplete dAllComplete = do
  cb <- checkbox False $
    def & checkboxConfig_setValue .~ updated dAllComplete

  text "Mark all as complete"

  pure $ cb ^. checkbox_change

markAllCompleteExample ::
  MonadWidget t m =>
  m ()
markAllCompleteExample = elClass "div" "add-item-wrapper" $ mdo
  cb1 <- el "div" . checkbox False $
    def & checkboxConfig_setValue .~ eMarkAllComplete

  cb2 <- el "div" . checkbox False $
    def & checkboxConfig_setValue .~ eMarkAllComplete

  let
    dComplete1 = cb1 ^. checkbox_value
    dComplete2 = cb2 ^. checkbox_value
    dAllComplete = (&&) <$> dComplete1 <*> dComplete2

  eMarkAllComplete <- el "div" $ markAllComplete dAllComplete

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
  attachId_ "examples-dom-clear-complete"
    clearCompleteExample
  attachId_ "examples-dom-mark-all-complete"
    markAllCompleteExample
