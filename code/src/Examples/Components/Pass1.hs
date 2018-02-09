{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
module Examples.Components.Pass1 (
    attachComponentExamplesPass1
  ) where

import Control.Monad (void)

import Control.Lens

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex
import Reflex.Dom.Core
import GHCJS.DOM.Types (MonadJSM)

import Util.Attach
import Util.Runner

getKey :: Reflex t => TextInput t -> Key -> Event t ()
getKey ti k =
  void .
  ffilter ((== k) . keyCodeLookup . fromIntegral) $
  ti ^. textInput_keypress

textEdit ::
  MonadWidget t m =>
  Dynamic t (Map Text Text) ->
  Dynamic t Text ->
  m (Event t Text, Event t ())
textEdit dAttrs dValue = mdo
  ti <- textInput $
    def & textInputConfig_attributes   .~ dAttrs
        & textInputConfig_setValue     .~ ("" <$ eDone)

  let
    eBlur = void . ffilter not . updated $ ti ^. textInput_hasFocus

    dValue = ti ^. textInput_value
    eText  = current dValue <@ getKey ti Enter
    eClear = getKey ti Escape
    eDone  = leftmost [void eText, eClear, eBlur]

  pure (eText, eDone)

add ::
  MonadWidget t m =>
  m (Event t Text)
add = do
  (eText, _) <- textEdit (pure $ "placeholder" =: "What shall we do today?") (pure "")
  pure eText

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

markAllComplete ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t Bool)
markAllComplete dAllComplete = do
  cb <- checkbox False $
    def & checkboxConfig_setValue .~ updated dAllComplete
  text "Mark all as complete"
  pure $ cb ^. checkbox_change

complete ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t Bool)
complete dComplete = do
  initial <- sample . current $ dComplete
  cb <- checkbox initial $
    def & checkboxConfig_setValue .~ updated dComplete
  pure $ cb ^. checkbox_change

editRead ::
  MonadWidget t m =>
  Dynamic t Text ->
  Workflow t m (Event t Text)
editRead dText = Workflow $ do
  (e, _) <- el' "div" $ dynText dText
  pure (never, editWrite dText <$ domEvent Dblclick e)

editWrite ::
  MonadWidget t m =>
  Dynamic t Text ->
  Workflow t m (Event t Text)
editWrite dText = Workflow $ do
  (eText, eDone) <- textEdit (pure mempty) dText
  pure (eText, editRead dText <$ eDone)

edit ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t Text, Event t ())
edit dText = do
  deText <- workflow $ editRead dText

  let
    eText = switchDyn deText
    eTextNonEmpty =       ffilter (not . Text.null) eText
    eTextEmpty    = () <$ ffilter        Text.null  eText

  pure (eTextNonEmpty, eTextEmpty)

remove ::
  MonadWidget t m =>
  m (Event t ())
remove =
  button "Remove"

data TodoItem =
  TodoItem {
    _tiComplete :: Bool
  , _tiText     :: Text
  }

makeLenses ''TodoItem

todoItem ::
  MonadWidget t m =>
  Dynamic t TodoItem ->
  m (Event t (TodoItem -> TodoItem), Event t ())
todoItem dTodoItem = do
  dComplete <- holdUniqDyn $
    fmap (view tiComplete) dTodoItem
  dText     <- holdUniqDyn $
    fmap (view tiText)     dTodoItem

  eComplete             <- complete dComplete
  (eText, eRemoveEmpty) <- edit dText
  eRemoveClick          <- remove

  let
    eChange = mergeWith (.) [
                  set tiComplete <$> eComplete
                , set tiText     <$> eText
                ]
    eRemove = leftmost [
                  eRemoveEmpty
                , eRemoveClick
                ]

  pure (eChange, eRemove)

editExample1 ::
  MonadWidget t m =>
  m ()
editExample1 = el "div" $ mdo
  dText <- holdDyn "Test" eText
  (eText, eRemove) <- edit dText

  dCountChange <- count eText
  el "div" $ do
    text "Changes: "
    display dCountChange

  dCountRemove <- count eRemove
  el "div" $ do
    text "Removes: "
    display dCountRemove

  pure ()

editExample2 ::
  MonadWidget t m =>
  m ()
editExample2 = el "div" $ mdo
  dItem <- foldDyn ($) (TodoItem False "Test") eChange

  (eChange, eRemove) <- elClass "div" "todo-item" $
    todoItem dItem

  el "div" $ do
    el "div" $ do
      text "Complete: "
      display $ view tiComplete <$> dItem
    el "div" $ do
      text "Text: "
      display $ view tiText <$> dItem

  dCountChange <- count eChange
  dCountRemove <- count eRemove

  el "div" $ do
    el "div" $ do
      text "Changes: "
      display dCountChange

    el "div" $ do
      text "Removes: "
      display dCountRemove

  pure ()

todoList1 ::
  MonadWidget t m =>
  [TodoItem] ->
  m ()
todoList1 tis = elClass "div" "todo" $ mdo
  let
    initialMap = Map.fromList . zip [0..] $ tis

  eAddText <- add
  let
    eAdd = TodoItem False <$> eAddText

  dAdds <- count eAdd
  let
    dCount = (+ length tis) <$> dAdds

  dModel <- foldDyn ($) initialMap . mergeWith (.) $ [
                Map.insert <$> current dCount <@> eAdd
              -- , Map.mergeWithKey (\_ f x -> Just (f x)) (const mempty) id <$> eChanges
              -- , flip (foldr Map.delete) <$> eRemoves
              -- , (fmap . set tiComplete) <$> eMarkAllComplete
              -- , Map.filter (not . view tiComplete) <$ eClearComplete
              ]

  dMap <- elClass "ul" "todo-list" . list dModel $ \dv ->
            elClass "li" "todo-item" . todoItem $ dv

  let
    dComplete = fmap (Map.elems . fmap (view tiComplete)) dModel
    dAllComplete = fmap and dComplete
    dAnyComplete = fmap or dComplete

  -- eMarkAllComplete <- el "div" $ markAllComplete dAllComplete
  -- eClearComplete   <- el "div" $ clearComplete   dAnyComplete

  let
    eChanges =
      switchDyn . fmap (mergeMap . fmap fst) $ dMap
    eRemoves =
      fmap Map.keys . switchDyn . fmap (mergeMap . fmap snd) $ dMap

  pure ()

todoList2 ::
  MonadWidget t m =>
  [TodoItem] ->
  m ()
todoList2 tis = elClass "div" "todo" $ mdo
  let
    initialMap = Map.fromList . zip [0..] $ tis

  eAddText <- add
  let
    eAdd = TodoItem False <$> eAddText

  dAdds <- count eAdd
  let
    dCount = (+ length tis) <$> dAdds

  dModel <- foldDyn ($) initialMap . mergeWith (.) $ [
                Map.insert <$> current dCount <@> eAdd
              -- , Map.mergeWithKey (\_ f x -> Just (f x)) (const mempty) id <$> eChanges
              , flip (foldr Map.delete) <$> eRemoves
              -- , (fmap . set tiComplete) <$> eMarkAllComplete
              -- , Map.filter (not . view tiComplete) <$ eClearComplete
              ]

  dMap <- elClass "ul" "todo-list" . list dModel $ \dv ->
            elClass "li" "todo-item" . todoItem $ dv

  let
    dComplete = fmap (Map.elems . fmap (view tiComplete)) dModel
    dAllComplete = fmap and dComplete
    dAnyComplete = fmap or dComplete

  -- eMarkAllComplete <- el "div" $ markAllComplete dAllComplete
  -- eClearComplete   <- el "div" $ clearComplete   dAnyComplete

  let
    eChanges =
      switchDyn . fmap (mergeMap . fmap fst) $ dMap
    eRemoves =
      fmap Map.keys . switchDyn . fmap (mergeMap . fmap snd) $ dMap

  pure ()

todoList3 ::
  MonadWidget t m =>
  [TodoItem] ->
  m ()
todoList3 tis = elClass "div" "todo" $ mdo
  let
    initialMap = Map.fromList . zip [0..] $ tis

  eAddText <- add
  let
    eAdd = TodoItem False <$> eAddText

  dAdds <- count eAdd
  let
    dCount = (+ length tis) <$> dAdds

  dModel <- foldDyn ($) initialMap . mergeWith (.) $ [
                Map.insert <$> current dCount <@> eAdd
              , Map.mergeWithKey (\_ f x -> Just (f x)) (const mempty) id <$> eChanges
              , flip (foldr Map.delete) <$> eRemoves
              -- , (fmap . set tiComplete) <$> eMarkAllComplete
              -- , Map.filter (not . view tiComplete) <$ eClearComplete
              ]

  dMap <- elClass "ul" "todo-list" . list dModel $ \dv ->
            elClass "li" "todo-item" . todoItem $ dv

  let
    dComplete = fmap (Map.elems . fmap (view tiComplete)) dModel
    dAllComplete = fmap and dComplete
    dAnyComplete = fmap or dComplete

  -- eMarkAllComplete <- el "div" $ markAllComplete dAllComplete
  -- eClearComplete   <- el "div" $ clearComplete   dAnyComplete

  let
    eChanges =
      switchDyn . fmap (mergeMap . fmap fst) $ dMap
    eRemoves =
      fmap Map.keys . switchDyn . fmap (mergeMap . fmap snd) $ dMap

  pure ()

todoList4 ::
  MonadWidget t m =>
  [TodoItem] ->
  m ()
todoList4 tis = elClass "div" "todo" $ mdo
  let
    initialMap = Map.fromList . zip [0..] $ tis

  eAddText <- add
  let
    eAdd = TodoItem False <$> eAddText

  dAdds <- count eAdd
  let
    dCount = (+ length tis) <$> dAdds

  dModel <- foldDyn ($) initialMap . mergeWith (.) $ [
                Map.insert <$> current dCount <@> eAdd
              , Map.mergeWithKey (\_ f x -> Just (f x)) (const mempty) id <$> eChanges
              , flip (foldr Map.delete) <$> eRemoves
              , (fmap . set tiComplete) <$> eMarkAllComplete
              , Map.filter (not . view tiComplete) <$ eClearComplete
              ]

  dMap <- elClass "ul" "todo-list" . list dModel $ \dv ->
            elClass "li" "todo-item" . todoItem $ dv

  let
    dComplete = fmap (Map.elems . fmap (view tiComplete)) dModel
    dAllComplete = fmap and dComplete
    dAnyComplete = fmap or dComplete

  eMarkAllComplete <- el "div" $ markAllComplete dAllComplete
  eClearComplete   <- el "div" $ clearComplete   dAnyComplete

  let
    eChanges =
      switchDyn . fmap (mergeMap . fmap fst) $ dMap
    eRemoves =
      fmap Map.keys . switchDyn . fmap (mergeMap . fmap snd) $ dMap

  pure ()

attachComponentExamplesPass1 ::
  MonadJSM m =>
  m ()
attachComponentExamplesPass1 = do
  attachId_ "examples-component-pass1-edit"
    editExample1
  attachId_ "examples-component-pass1-todo-item"
    editExample2
  attachId_ "examples-component-pass1-todo-list-1" $
    todoList1 []
  attachId_ "examples-component-pass1-todo-list-2" $
    todoList2 []
  attachId_ "examples-component-pass1-todo-list-3" $
    todoList3 []
  attachId_ "examples-component-pass1-todo-list-4" $
    todoList4 []
  attachId_ "examples-component-pass1-todo-list-5" $
    todoList4 [TodoItem True "Prepare for talk", TodoItem True "Panic", TodoItem False "Give talk"]
