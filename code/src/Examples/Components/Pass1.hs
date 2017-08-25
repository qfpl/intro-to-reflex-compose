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
  ) where

import Control.Monad (void)

import Control.Lens

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex
import Reflex.Dom.Core

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
  m (Event t Text)
edit dText = do
  deText <- workflow $ editRead dText
  pure . switch . current $ deText

remove ::
  MonadWidget t m =>
  m (Event t ())
remove =
  button "Remove"

data TodoItemModel =
  TodoItemModel {
    _timComplete :: Bool
  , _timText     :: Text
  }

makeLenses ''TodoItemModel

todoItem ::
  MonadWidget t m =>
  Dynamic t TodoItemModel ->
  m (Event t (TodoItemModel -> TodoItemModel), Event t ())
todoItem dTodoItemModel = do
  dComplete <- holdUniqDyn $ fmap (view timComplete) dTodoItemModel
  dText     <- holdUniqDyn $ fmap (view timText)     dTodoItemModel

  eComplete    <- complete dComplete
  eText        <- edit dText
  eRemoveClick <- remove

  let
    eTextNonEmpty =       ffilter (not . Text.null) eText
    eTextEmpty    = () <$ ffilter        Text.null  eText

    eChange = mergeWith (.) [
                  set timComplete <$> eComplete
                , set timText     <$> eTextNonEmpty
                ]
    eRemove = leftmost [
                  eTextEmpty
                , eRemoveClick
                ]

  pure (eChange, eRemove)

todoList ::
  MonadWidget t m =>
  [TodoItemModel] ->
  m ()
todoList tims = mdo
  let
    initialMap = Map.fromList . zip [0..] $ tims

  eAddText <- add
  let
    eAdd = TodoItemModel False <$> eAddText

  dAdds <- count eAdd
  let
    dCount = (+ length tims) <$> dAdds

  dModel <- foldDyn ($) initialMap . mergeWith (.) $ [
                Map.insert <$> current dCount <@> eAdd
              , Map.mergeWithKey (\_ f x -> Just (f x)) (const mempty) id <$> eChanges
              , flip (foldr Map.delete) <$> eRemoves
              , (fmap . set timComplete) <$> eMarkAllComplete
              , Map.filter (not . view timComplete) <$ eClearComplete
              ]

  dMap <- elClass "ul" "todo-list" . list dModel $ \dv ->
            elClass "li" "todo-item" . todoItem $ dv

  let
    dComplete = fmap (Map.elems . fmap (view timComplete)) dModel
    dAllComplete = fmap and dComplete
    dAnyComplete = fmap or dComplete

  el "hr" $ pure ()

  eMarkAllComplete <- el "div" $ markAllComplete dAllComplete
  eClearComplete   <- el "div" $ clearComplete   dAnyComplete

  let
    eChanges =
      switch . current . fmap (mergeMap . fmap fst) $ dMap
    eRemoves =
      fmap Map.keys . switch . current . fmap (mergeMap . fmap snd) $ dMap

  pure ()

go ::
  MonadWidget t m =>
  m ()
go =
  todoList [TodoItemModel False "A", TodoItemModel True "B" , TodoItemModel False "C"]
