{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Examples.Components.Pass3 (
  ) where

import Control.Monad (void)
import Data.Semigroup
import Data.Monoid hiding ((<>))

import Control.Monad.Reader (MonadReader(..), ReaderT, runReaderT)

import Control.Lens

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

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

data TodoItemModel =
  TodoItemModel {
    _timComplete :: Bool
  , _timText     :: Text
  }

makeLenses ''TodoItemModel

type CanRemove t k m = (Ord k, MonadReader k m,  EventWriter t (Set k) m)

removeEvent ::
  ( Reflex t
  , CanRemove t k m
  ) =>
  Event t a ->
  m ()
removeEvent e = do
  k <- ask
  tellEvent $ Set.singleton k <$ e

complete ::
  ( CanRemove t k m
  , MonadWidget t m
  ) =>
  Event t Bool ->
  Event t () ->
  Dynamic t Bool ->
  m (Dynamic t Bool)
complete eMarkAllComplete eClearComplete dComplete = do
  initial <- sample . current $ dComplete

  cb <- checkbox initial $
    def & checkboxConfig_setValue .~ leftmost [updated dComplete, eMarkAllComplete]

  let
    dComplete = cb ^. checkbox_value

  removeEvent $ ffilter id (current dComplete <@ eClearComplete)

  pure dComplete

editRead ::
  ( CanRemove t k m
  , MonadWidget t m
  )=>
  Dynamic t Text ->
  Workflow t m ()
editRead dText = Workflow $ do
  (e, _) <- el' "div" $ dynText dText
  pure ((), editWrite dText <$ domEvent Dblclick e)

editWrite ::
  ( CanRemove t k m
  , MonadWidget t m
  ) =>
  Dynamic t Text ->
  Workflow t m ()
editWrite dText = Workflow $ do
  (eText, eDone) <- textEdit (pure mempty) dText
  removeEvent $ () <$ ffilter Text.null  eText
  pure ((), editRead dText <$ eDone)

edit ::
  ( CanRemove t k m
  , MonadWidget t m
  ) =>
  Dynamic t Text ->
  m ()
edit dText =
  void . workflow $ editRead dText

remove ::
  ( CanRemove t k m
  , MonadWidget t m
  ) =>
  m ()
remove = do
  eRemove <- button "Remove"
  removeEvent eRemove

todoItem ::
  ( CanRemove t k m
  , MonadWidget t m
  ) =>
  Event t Bool ->
  Event t () ->
  Dynamic t TodoItemModel ->
  m (Dynamic t Bool)
todoItem eMarkAllComplete eClearComplete dTodoItemModel = do
  dComplete <- holdUniqDyn $ fmap (view timComplete) dTodoItemModel
  dText     <- holdUniqDyn $ fmap (view timText)     dTodoItemModel

  dComplete <- complete eMarkAllComplete eClearComplete dComplete
  edit dText
  remove

  pure dComplete

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
              , flip (foldr Map.delete) <$> eRemoves
              ]

  (dmdComplete, eRemoves) <-
    elClass "ul" "todo-list" . runEventWriterT .
      listWithKey dModel $ \k dv ->
        elClass "li" "todo-item" . flip runReaderT k .
          todoItem eMarkAllComplete eClearComplete $ dv

  let
    dComplete = joinDynThroughMap dmdComplete
    dAllComplete = fmap and dComplete
    dAnyComplete = fmap or dComplete

  el "hr" $ pure ()

  eMarkAllComplete <-
    el "div" $ markAllComplete dAllComplete
  eClearComplete   <-
    el "div" $ clearComplete   dAnyComplete

  pure ()

go ::
  MonadWidget t m =>
  m ()
go =
  todoList [TodoItemModel False "A", TodoItemModel True "B" , TodoItemModel False "C"]
