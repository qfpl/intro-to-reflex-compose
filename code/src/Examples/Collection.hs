{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
module Examples.Collection (
    attachCollectionExamples
  ) where

import Control.Monad (void)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom.Core
import GHCJS.DOM.Types (MonadJSM)

import Util.Attach

attachCollectionExamples ::
  MonadJSM m =>
  m ()
attachCollectionExamples = do
  attachId_ "examples-collection-add"
    addExample

  attachId_ "examples-collection-remove"
    removeExample

  attachId_ "examples-collection-remove-complete"
    removeCompleteExample

  attachId_ "examples-collection-edit"
    editExample

getKey :: Reflex t => TextInput t -> Key -> Event t ()
getKey ti k =
  void .
  ffilter ((== k) . keyCodeLookup . fromIntegral) $
  ti ^. textInput_keypress

data EditItemConfig t =
  EditItemConfig {
    _editItemConfig_dValue :: Dynamic t Text
  }

data EditItem t =
  EditItem {
    _editItem_eChange :: Event t Text
  , _editItem_eRemove :: Event t ()
  }

editingShow ::
  MonadWidget t m =>
  EditItemConfig t ->
  Workflow t m (EditItem t)
editingShow eic@(EditItemConfig dValue) = Workflow $ do
  (e, _) <- el' "div" $ dynText dValue
  pure (EditItem never never, editingEdit eic <$ domEvent Dblclick e)

editingEdit ::
  MonadWidget t m =>
  EditItemConfig t ->
  Workflow t m (EditItem t)
editingEdit (EditItemConfig dValue) = Workflow $ do
  initialValue <- sample . current $ dValue
  ti <- textInput $ def & textInputConfig_initialValue .~ initialValue

  let
    eBlur = void . ffilter not . updated $ ti ^. textInput_hasFocus

    dValue = ti ^. textInput_value
    bValue = current dValue
    eDoneNew = bValue <@ leftmost [getKey ti Enter, eBlur]
    eDoneOld = initialValue <$ getKey ti Escape
    eDone = leftmost [eDoneOld, eDoneNew]
    eRemove = void . ffilter Text.null $ eDone

  pure (EditItem eDone eRemove, (editingShow $ EditItemConfig dValue) <$ eDone)

edit ::
  forall t m.
  MonadWidget t m =>
  EditItemConfig t ->
  m (EditItem t)
edit eic = do
  dItem <- workflow $ editingShow eic
  let
    eChange = switch . current . fmap _editItem_eChange $ dItem
    eRemove = switch . current . fmap _editItem_eRemove $ dItem
  pure $ EditItem eChange eRemove

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

data TodoItemConfig t =
  TodoItemConfig {
    _todoItemConfig_dValue         :: Dynamic t Text
  , _todoItemConfig_eClearComplete :: Event t ()
  }

data TodoItem t =
  TodoItem {
    _todoItem_dComplete :: Dynamic t Bool
  , _todoItem_eChange   :: Event t Text
  , _todoItem_eRemove   :: Event t ()
  }

data TodoConfig t =
  TodoConfig {
    _todoConfig_eAdd           :: Event t Text
  , _todoConfig_eClearComplete :: Event t ()
  }

data Todo t =
  Todo {
    _todo_dComplete :: Dynamic t (Map Int Bool)
  }

todoItem ::
  MonadWidget t m =>
  TodoItemConfig t ->
  m (TodoItem t)
todoItem tic =
  mdo
    cb <- checkbox False def
    let
      dComplete = cb ^. checkbox_value
      mkCompleteClass False = ""
      mkCompleteClass True  = "completed "
      dCompleteClass = mkCompleteClass <$> dComplete

    EditItem eChange eRemoveEmpty <- elDynClass "div" dCompleteClass $
      edit . EditItemConfig $ _todoItemConfig_dValue tic

    eRemoveClick <- elClass "div" "remove" $
      button "Remove"

    let
      eRemove = leftmost [
          eRemoveClick
        , eRemoveEmpty
        , gate (current dComplete) $ _todoItemConfig_eClearComplete tic
        ]

    pure $ TodoItem dComplete eChange eRemove

todo1 ::
  MonadWidget t m =>
  TodoConfig t ->
  m (Todo t)
todo1 tc = do

  let
    eAdd = _todoConfig_eAdd tc

  dNextKey <- count eAdd

  dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dNextKey <@> eAdd
    ]

  dmTodoItem <- elClass "ul" "todo-list" . list dMap $ \dv -> do
    elClass "li" "todo-item" . todoItem $ TodoItemConfig dv (_todoConfig_eClearComplete tc)

  let
    dComplete =
      joinDynThroughMap .
      fmap (fmap _todoItem_dComplete) $
      dmTodoItem

  pure $ Todo dComplete

addExample ::
  MonadWidget t m =>
  m ()
addExample = elClass "div" "todo" $ do
  eAdd <- addItemWidget

  _ <- todo1 $ TodoConfig eAdd never

  pure ()

todo2 ::
  MonadWidget t m =>
  TodoConfig t ->
  m (Todo t)
todo2 tc = mdo

  let
    eAdd = _todoConfig_eAdd tc

  dNextKey <- count eAdd

  dmText <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dNextKey <@> eAdd
    , flip (foldr Map.delete) <$> eRemoves
    ]

  dmItems <- elClass "ul" "todo-list" . list dmText $ \dv -> do
    elClass "li" "todo-item" . todoItem $ TodoItemConfig dv (_todoConfig_eClearComplete tc)

  let
    eRemoves =
      fmap Map.keys .
      switch .
      current .
      fmap (mergeMap . fmap _todoItem_eRemove) $
      dmItems
    dComplete =
      joinDynThroughMap .
      fmap (fmap _todoItem_dComplete) $
      dmItems

  pure $ Todo dComplete

removeExample ::
  MonadWidget t m =>
  m ()
removeExample = elClass "div" "todo" $ do
  eAdd <- addItemWidget

  _ <- todo2 $ TodoConfig eAdd never

  pure ()

removeCompleteExample ::
  MonadWidget t m =>
  m ()
removeCompleteExample = elClass "div" "todo "$ mdo
  eAdd <- addItemWidget

  todo <- todo2 $ TodoConfig eAdd eClearComplete

  let
    dAnyComplete = or <$> _todo_dComplete todo
    mkHidden False = "hide"
    mkHidden True  = ""
    dClass = mkHidden <$> dAnyComplete

  eClearComplete <- elDynClass "div" dClass $
    button "Clear complete"

  pure ()

todo3 ::
  MonadWidget t m =>
  TodoConfig t ->
  m (Todo t)
todo3 tc = mdo

  let
    eAdd = _todoConfig_eAdd tc

  dNextKey <- count eAdd

  dmText <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dNextKey <@> eAdd
    , flip (foldr Map.delete) <$> eRemoves
    , flip (foldr (uncurry Map.insert)) <$> eChanges
    ]

  dmItems <- elClass "ul" "todo-list" . list dmText $ \dv -> do
    elClass "li" "todo-item" . todoItem $ TodoItemConfig dv (_todoConfig_eClearComplete tc)

  let
    eRemoves =
      fmap Map.keys .
      switch .
      current .
      fmap (mergeMap . fmap _todoItem_eRemove) $
      dmItems
    eChanges =
      fmap Map.toList .
      switch .
      current .
      fmap (mergeMap . fmap _todoItem_eChange) $
      dmItems
    dComplete =
      joinDynThroughMap .
      fmap (fmap _todoItem_dComplete) $
      dmItems

  pure $ Todo dComplete

editExample ::
  MonadWidget t m =>
  m ()
editExample = elClass "div" "todo" $ mdo
  eAdd <- addItemWidget

  todo <- todo3 $ TodoConfig eAdd eClearComplete

  let
    dAnyComplete = or <$> _todo_dComplete todo
    mkHidden False = "hide"
    mkHidden True  = ""
    dClass = mkHidden <$> dAnyComplete

  eClearComplete <- elDynClass "div" dClass $
    button "Clear complete"

  pure ()
