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
module Examples.Components.Pass2 (
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

data TodoItem =
  TodoItem {
    _tiComplete :: Bool
  , _tiText     :: Text
  }

makeLenses ''TodoItem

data ModelWriter k i =
  ModelWriter {
    _mwChanges :: Map k (i -> i)
  , _mwRemoves :: Set k
  }

makeLenses ''ModelWriter

instance Ord k => Semigroup (ModelWriter k i) where
  (ModelWriter m1 s1) <> (ModelWriter m2 s2) =
    ModelWriter (Map.unionWith (.) m1 m2) (Set.union s1 s2)

instance Ord k => Monoid (ModelWriter k i) where
  mempty = ModelWriter mempty mempty
  mappend = (<>)

type HasModel t k i m = (Ord k, MonadReader k m,  EventWriter t (ModelWriter k i) m)

changeEvent ::
  ( Reflex t
  , HasModel t k i m
  ) =>
  Event t (i -> i) ->
  m ()
changeEvent e = do
  k <- ask
  tellEvent $ (\f -> ModelWriter (Map.singleton k f) mempty) <$> e

removeEvent ::
  ( Reflex t
  , HasModel t k i m
  ) =>
  Event t a ->
  m ()
removeEvent e = do
  k <- ask
  tellEvent $ ModelWriter mempty (Set.singleton k) <$ e

complete ::
  ( HasModel t k TodoItem m
  , MonadWidget t m
  ) =>
  Dynamic t Bool ->
  m ()
complete dComplete = do
  initial <- sample . current $ dComplete
  cb <- checkbox initial $
    def & checkboxConfig_setValue .~ updated dComplete
  changeEvent $ set tiComplete <$> cb ^. checkbox_change

editRead ::
  ( HasModel t k TodoItem m
  , MonadWidget t m
  )=>
  Dynamic t Text ->
  Workflow t m ()
editRead dText = Workflow $ do
  (e, _) <- el' "div" $ dynText dText
  pure ((), editWrite dText <$ domEvent Dblclick e)

editWrite ::
  ( HasModel t k TodoItem m
  , MonadWidget t m
  ) =>
  Dynamic t Text ->
  Workflow t m ()
editWrite dText = Workflow $ do
  (eText, eDone) <- textEdit (pure mempty) dText

  let
    eTextNonEmpty =       ffilter (not . Text.null) eText
    eTextEmpty    = () <$ ffilter        Text.null  eText

  changeEvent $ set tiText <$> eTextNonEmpty
  removeEvent eTextEmpty

  pure ((), editRead dText <$ eDone)

edit ::
  ( HasModel t k TodoItem m
  , MonadWidget t m
  ) =>
  Dynamic t Text ->
  m ()
edit dText =
  void . workflow $ editRead dText

remove ::
  ( HasModel t k i m
  , MonadWidget t m
  ) =>
  m ()
remove = do
  eRemove <- button "Remove"
  removeEvent eRemove

todoItem ::
  ( HasModel t k TodoItem m
  , MonadWidget t m
  ) =>
  Dynamic t TodoItem ->
  m ()
todoItem dTodoItem = do
  dComplete <- holdUniqDyn $ fmap (view tiComplete) dTodoItem
  dText     <- holdUniqDyn $ fmap (view tiText)     dTodoItem

  complete dComplete
  edit dText
  remove

todoList ::
  MonadWidget t m =>
  [TodoItem] ->
  m ()
todoList tis = mdo
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

  (_, eWriter) <-
    elClass "ul" "todo-list" . runEventWriterT . listWithKey dModel $ \k dv ->
      elClass "li" "todo-item" . flip runReaderT k . todoItem $ dv

  let
    dComplete = fmap (Map.elems . fmap (view tiComplete)) dModel
    dAllComplete = fmap and dComplete
    dAnyComplete = fmap or dComplete

  el "hr" $ pure ()

  eMarkAllComplete <- el "div" $ markAllComplete dAllComplete
  eClearComplete   <- el "div" $ clearComplete   dAnyComplete

  let
    eChanges = fmap (view mwChanges) eWriter
    eRemoves = fmap (view mwRemoves) eWriter

  pure ()

go ::
  MonadWidget t m =>
  m ()
go =
  todoList [TodoItem False "A", TodoItem True "B" , TodoItem False "C"]
