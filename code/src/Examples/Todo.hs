{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Examples.Todo (
  ) where

import Control.Monad (void)
import Data.Monoid ((<>))

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom.Core

import Util.Runner

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
  Dynamic t Text ->
  Workflow t m (Event t Text)
editingShow dValue = Workflow $ do
  (e, _) <- el' "div" $ dynText dValue
  pure (never, editingEdit dValue <$ domEvent Dblclick e)

editingEdit ::
  MonadWidget t m =>
  Dynamic t Text ->
  Workflow t m (Event t Text)
editingEdit dValue = Workflow $ do
  initialValue <- sample . current $ dValue
  ti <- textInput $ def & textInputConfig_initialValue .~ initialValue

  let
    eBlur = void . ffilter not . updated $ ti ^. textInput_hasFocus

    dValue = ti ^. textInput_value
    bValue = current dValue
    eDoneNew = bValue <@ leftmost [getKey ti Enter, eBlur]
    eDoneOld = initialValue <$ getKey ti Escape
    eDone = leftmost [eDoneOld, eDoneNew]

  pure (eDone, (editingShow dValue) <$ eDone)

edit ::
  forall t m.
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t Text)
edit dValue = do
  deText <- workflow $ editingShow dValue
  pure . switch . current $ deText

addItemWidget ::
  MonadWidget t m =>
  m (Event t Text)
addItemWidget = elClass "div" "add-item" $ mdo
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

markAllComplete ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t ())
markAllComplete dAnyComplete =
  let
    mkClass False = "hide"
    mkClass True  = ""
    dClass = mkClass <$> dAnyComplete
  in
    elDynClass "div" dClass $
      button "Mark all complete"

data TodoItemConfig =
  TodoItemConfig {
    _todoItemConfig_complete :: Bool
  , _todoItemConfig_text     :: Text
  }

makeLenses ''TodoItemConfig

data TodoItem t =
  TodoItem {
    _todoItem_eComplete :: Event t Bool
  , _todoItem_eText     :: Event t Text
  , _todoItem_eRemove   :: Event t ()
  }

makeLenses ''TodoItem

complete ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t Bool)
complete dComplete = do
  initial <- sample . current $ dComplete
  cb <- checkbox initial $ def & checkboxConfig_setValue .~ updated dComplete
  pure $ cb ^. checkbox_change

remove ::
  MonadWidget t m =>
  m (Event t ())
remove =
  button "Remove"

todoItem ::
  MonadWidget t m =>
  Dynamic t TodoItemConfig ->
  m (TodoItem t)
todoItem dIn =
  mdo
    dCompleteIn <-
      holdUniqDyn .
      fmap _todoItemConfig_complete $
      dIn

    eComplete <- complete dCompleteIn

    let
      mkCompleteClass False = ""
      mkCompleteClass True  = "completed "
      dCompleteClass = mkCompleteClass <$> dCompleteIn

    dTextIn <-
      holdUniqDyn .
      fmap _todoItemConfig_text $
      dIn

    eText <- elDynClass "div" dCompleteClass $
      edit dTextIn

    eRemoveClick <- remove

    let
      eRemove = leftmost [
          eRemoveClick
        , void . ffilter Text.null $ eText
        ]

    pure $ TodoItem eComplete eText eRemove

getMapEvents ::
  (Reflex t, Ord k) =>
  Lens' b (Event t a) ->
  Dynamic t (Map k b) ->
  Event t (Map k a)
getMapEvents l =
  switch .
  current .
  fmap (mergeMap . fmap (view l))

applyMapEvents ::
  (Reflex t, Ord k) =>
  Lens' c a ->
  Event t (Map k a) ->
  Event t (Map k c -> Map k c)
applyMapEvents l =
  let
    f = Map.mergeWithKey (\_ a c -> Just $ c & l .~ a) (const Map.empty) id
  in
    fmap f

change ::
  (Reflex t, Ord k) =>
  Lens' b (Event t a) ->
  Lens' c a ->
  Dynamic t (Map k b) ->
  Event t (Map k c -> Map k c)
change g s =
  applyMapEvents s .
  getMapEvents g

todo ::
  MonadWidget t m =>
  [TodoItemConfig] ->
  m ()
todo initial = mdo
  eAdd <- addItemWidget

  dNew <- count eAdd
  let
    dCount = (+ length initial) <$> dNew
    initialMap = Map.fromList . zip [0..] $ initial

  let
    eNewItem = TodoItemConfig False <$> eAdd

  dMap <- foldDyn ($) initialMap .
          mergeWith (.) $ [
            Map.insert <$> current dCount <@> eNewItem
          , flip (foldr Map.delete) <$> eRemove
          , eChanges
          ]

  -- could do the filtering on dMap via the applicative
  -- - less efficient than toggling classes

  dmTodoItems <- elClass "ul" "todo-list" $ list dMap $ elClass "li" "todo-item" . todoItem

  let
    eChangesComplete =
      change todoItem_eComplete todoItemConfig_complete dmTodoItems
    eChangesText =
      change todoItem_eText todoItemConfig_text dmTodoItems
    eChanges =
      eChangesComplete <> eChangesText

  let
    dComplete = fmap (fmap _todoItemConfig_complete) dMap
    dAnyComplete = or <$> dComplete

  eClearComplete <- markAllComplete dAnyComplete

  let
    eRemoveFromItems =
      fmap Map.keys .
      getMapEvents todoItem_eRemove $
      dmTodoItems

    completeKeys =
      Map.keys . Map.filter id
    eRemoveFromClearComplete =
      completeKeys <$> current dComplete <@ eClearComplete
    eRemove =
      eRemoveFromItems <> eRemoveFromClearComplete

  pure ()
