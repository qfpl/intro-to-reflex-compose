
# `EventWriter`

##

```haskell
class (Monad m, Semigroup w) => EventWriter t w m where
  tellEvent :: Event t w -> m ()
```

```haskell
runEventWriterT :: (Reflex t, Monad m, Semigroup w) 
                => EventWriterT t w m a 
                -> m (a, Event t w)
```

##

```haskell
data ModelWriter k i =
  ModelWriter {
    _mwChanges :: Map k (i -> i)
  , _mwRemoves :: Set k
  }

makeLenses ''ModelWriter
```

##

```haskell
instance Ord k => Semigroup (ModelWriter k i) where
  (ModelWriter m1 s1) <> (ModelWriter m2 s2) =
    ModelWriter (Map.unionWith (.) m1 m2) (Set.union s1 s2)

instance Ord k => Monoid (ModelWriter k i) where
  mempty = ModelWriter mempty mempty
  mappend = (<>)
```

##

```haskell
type HasModel t k i m = 
  ( Ord k
  , MonadReader k m
  , EventWriter t (ModelWriter k i) m
  )
```

##

```haskell
changeEvent :: (Reflex t, HasModel t k i m) 
            => Event t (i -> i) 
            -> m ()
changeEvent e = do
  k <- ask
  let g f = ModelWriter (Map.singleton k f) mempty
  tellEvent $ g <$> e
```

```haskell
removeEvent :: (Reflex t, HasModel t k i m) 
            => Event t a 
            -> m ()
removeEvent e = do
  k <- ask
  let g = ModelWriter mempty (Set.singleton k)
  tellEvent $ g <$ e
```

##

```haskell
complete ::                           MonadWidget t m 
         => Dynamic t Bool 
         -> m (Event t Bool)
complete dComplete = do
  initial <- sample . current $ dComplete

  cb <- checkbox initial $
    def & checkboxConfig_setValue .~ updated dComplete

  pure $                           cb ^. checkbox_change
```

##

```haskell
complete :: (HasModel t k TodoItem m, MonadWidget t m) 
         => Dynamic t Bool 
         -> m ()
complete dComplete = do
  initial <- sample . current $ dComplete
  
  cb <- checkbox initial $
    def & checkboxConfig_setValue .~ updated dComplete

  pure $                           cb ^. checkbox_change
```

##

```haskell
complete :: (HasModel t k TodoItem m, MonadWidget t m) 
         => Dynamic t Bool 
         -> m ()
complete dComplete = do
  initial <- sample . current $ dComplete
  
  cb <- checkbox initial $
    def & checkboxConfig_setValue .~ updated dComplete

  changeEvent $ set tiComplete <$> cb ^. checkbox_change
```

##

```haskell
editRead ::                           MonadWidget t m 
         => Dynamic t Text 
         -> Workflow t m (Event t Text)
editRead dText = Workflow $ do

  (e, _) <- el' "div" $ dynText dText

  let 
    eDoubleClicked =
      domEvent Dblclick e
    
  pure (never, editWrite dText <$ eDoubleClicked)
```

##

```haskell
editRead :: (HasModel t k TodoItem m, MonadWidget t m) 
         => Dynamic t Text 
         -> Workflow t m (Event t Text)
editRead dText = Workflow $ do

  (e, _) <- el' "div" $ dynText dText

  let 
    eDoubleClicked =
      domEvent Dblclick e
    
  pure (never, editWrite dText <$ eDoubleClicked)
```

##

```haskell
editRead :: (HasModel t k TodoItem m, MonadWidget t m) 
         => Dynamic t Text 
         -> Workflow t m ()
editRead dText = Workflow $ do

  (e, _) <- el' "div" $ dynText dText

  let 
    eDoubleClicked =
      domEvent Dblclick e
    
  pure (never, editWrite dText <$ eDoubleClicked)
```

##

```haskell
editRead :: (HasModel t k TodoItem m, MonadWidget t m) 
         => Dynamic t Text 
         -> Workflow t m ()
editRead dText = Workflow $ do

  (e, _) <- el' "div" $ dynText dText

  let 
    eDoubleClicked =
      domEvent Dblclick e

  pure (()   , editWrite dText <$ eDoubleClicked)
```

##

```haskell
editWrite ::                           MonadWidget t m 
          => Dynamic t Text 
          -> Workflow t m (Event t Text)
editWrite dText = Workflow $ do

  (eText, eDone) <- textEdit (pure mempty) dText








  pure (eText, editRead dText <$ eDone)
```

##

```haskell
editWrite :: (HasModel t k TodoItem m, MonadWidget t m) 
          => Dynamic t Text 
          -> Workflow t m (Event t Text)
editWrite dText = Workflow $ do

  (eText, eDone) <- textEdit (pure mempty) dText








  pure (eText, editRead dText <$ eDone)
```

##

```haskell
editWrite :: (HasModel t k TodoItem m, MonadWidget t m) 
          => Dynamic t Text 
          -> Workflow t m ()
editWrite dText = Workflow $ do

  (eText, eDone) <- textEdit (pure mempty) dText








  pure (eText, editRead dText <$ eDone)
```

##

```haskell
editWrite :: (HasModel t k TodoItem m, MonadWidget t m) 
          => Dynamic t Text 
          -> Workflow t m ()
editWrite dText = Workflow $ do

  (eText, eDone) <- textEdit (pure mempty) dText

  let
    eTextNonEmpty =       ffilter (not . Text.null) eText





  pure (eText, editRead dText <$ eDone)
```

##

```haskell
editWrite :: (HasModel t k TodoItem m, MonadWidget t m) 
          => Dynamic t Text 
          -> Workflow t m ()
editWrite dText = Workflow $ do

  (eText, eDone) <- textEdit (pure mempty) dText

  let
    eTextNonEmpty =       ffilter (not . Text.null) eText


  changeEvent $ set tiText <$> eTextNonEmpty


  pure (eText, editRead dText <$ eDone)
```

##

```haskell
editWrite :: (HasModel t k TodoItem m, MonadWidget t m) 
          => Dynamic t Text 
          -> Workflow t m ()
editWrite dText = Workflow $ do

  (eText, eDone) <- textEdit (pure mempty) dText

  let
    eTextNonEmpty =       ffilter (not . Text.null) eText
    eTextEmpty    = () <$ ffilter        Text.null  eText

  changeEvent $ set tiText <$> eTextNonEmpty


  pure (eText, editRead dText <$ eDone)
```

##

```haskell
editWrite :: (HasModel t k TodoItem m, MonadWidget t m) 
          => Dynamic t Text 
          -> Workflow t m ()
editWrite dText = Workflow $ do

  (eText, eDone) <- textEdit (pure mempty) dText

  let
    eTextNonEmpty =       ffilter (not . Text.null) eText
    eTextEmpty    = () <$ ffilter        Text.null  eText

  changeEvent $ set tiText <$> eTextNonEmpty
  removeEvent eTextEmpty

  pure (eText, editRead dText <$ eDone)
```

##

```haskell
editWrite :: (HasModel t k TodoItem m, MonadWidget t m) 
          => Dynamic t Text 
          -> Workflow t m ()
editWrite dText = Workflow $ do

  (eText, eDone) <- textEdit (pure mempty) dText

  let
    eTextNonEmpty =       ffilter (not . Text.null) eText
    eTextEmpty    = () <$ ffilter        Text.null  eText

  changeEvent $ set tiText <$> eTextNonEmpty
  removeEvent eTextEmpty

  pure (()   , editRead dText <$ eDone)
```

##

```haskell
edit ::                           MonadWidget t m 
     => Dynamic t Text 
     -> m (Event t Text, Event t ())
edit dText = do
  deText <- workflow $ editRead dText

  let
    eText = switch . current $ deText
    eTextNonEmpty =       ffilter (not . Text.null) eText
    eTextEmpty    = () <$ ffilter        Text.null  eText

  pure (eTextNonEmpty, eTextEmpty)
```

##

```haskell
edit :: (HasModel t k TodoItem m, MonadWidget t m) 
     => Dynamic t Text 
     -> m (Event t Text, Event t ())
edit dText = do
  deText <- workflow $ editRead dText

  let
    eText = switch . current $ deText
    eTextNonEmpty =       ffilter (not . Text.null) eText
    eTextEmpty    = () <$ ffilter        Text.null  eText

  pure (eTextNonEmpty, eTextEmpty)
```

##

```haskell
edit :: (HasModel t k TodoItem m, MonadWidget t m) 
     => Dynamic t Text 
     -> m ()
edit dText = do
  deText <- workflow $ editRead dText

  let
    eText = switch . current $ deText
    eTextNonEmpty =       ffilter (not . Text.null) eText
    eTextEmpty    = () <$ ffilter        Text.null  eText

  pure (eTextNonEmpty, eTextEmpty)
```

##

```haskell
edit :: (HasModel t k TodoItem m, MonadWidget t m) 
     => Dynamic t Text 
     -> m ()
edit dText =
  void .    workflow $ editRead dText
  
  
  
  
  
  
  
```

##

```haskell
remove ::                    MonadWidget t m 
       => m (Event t ())
remove =
             button "Remove"
  
```

##

```haskell
remove :: (HasModel t k i m, MonadWidget t m) 
       => m (Event t ())
remove =
             button "Remove"
  
```

##

```haskell
remove :: (HasModel t k i m, MonadWidget t m) 
       => m ()
remove =
             button "Remove"
  
```

##

```haskell
remove :: (HasModel t k i m, MonadWidget t m) 
       => m ()
remove = do
  eRemove <- button "Remove"
  
```

##

```haskell
remove :: (HasModel t k i m, MonadWidget t m) 
       => m ()
remove = do
  eRemove <- button "Remove"
  removeEvent eRemove
```

##

```haskell
todoItem dTodoItem = do
  dComplete <- holdUniqDyn $ fmap (view tiComplete) dTodoItem
  dText     <- holdUniqDyn $ fmap (view tiText)     dTodoItem

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
```

##

```haskell
todoItem dTodoItem = do
  dComplete <- holdUniqDyn $ fmap (view tiComplete) dTodoItem
  dText     <- holdUniqDyn $ fmap (view tiText)     dTodoItem

  complete dComplete
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
```

##

```haskell
todoItem dTodoItem = do
  dComplete <- holdUniqDyn $ fmap (view tiComplete) dTodoItem
  dText     <- holdUniqDyn $ fmap (view tiText)     dTodoItem

  complete dComplete
  edit dText
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
```

##

```haskell
todoItem dTodoItem = do
  dComplete <- holdUniqDyn $ fmap (view tiComplete) dTodoItem
  dText     <- holdUniqDyn $ fmap (view tiText)     dTodoItem

  complete dComplete
  edit dText
  remove

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
```

##

```haskell
todoItem dTodoItem = do
  dComplete <- holdUniqDyn $ fmap (view tiComplete) dTodoItem
  dText     <- holdUniqDyn $ fmap (view tiText)     dTodoItem

  complete dComplete
  edit dText
  remove
  
  
  
  
  
  
  
  
  
  
  
  
```

##

```haskell
  ...

  dmEvents     <- 
    el "ul" .                   list        dMap $ \  dv -> 
      el "li" .                     todoItem $ dv

  ...

  let
    eChanges =
      switch . current . fmap mergeMap . fmap fst $
      dmEvents
    eRemoves =
      fmap Map.keys .
      switch . current . fmap mergeMap . fmap snd $
      dmEvents
  ...
```

##

```haskell
  ...

  dmEvents     <- 
    el "ul" .                   listWithKey dMap $ \  dv -> 
      el "li" .                     todoItem $ dv

  ...

  let
    eChanges =
      switch . current . fmap mergeMap . fmap fst $
      dmEvents
    eRemoves =
      fmap Map.keys .
      switch . current . fmap mergeMap . fmap snd $
      dmEvents
  ...
```

##

```haskell
  ...

  dmEvents     <- 
    el "ul" .                   listWithKey dMap $ \k dv -> 
      el "li" .                     todoItem $ dv

  ...

  let
    eChanges =
      switch . current . fmap mergeMap . fmap fst $
      dmEvents
    eRemoves =
      fmap Map.keys .
      switch . current . fmap mergeMap . fmap snd $
      dmEvents
  ...
```

##

```haskell
  ...

  dmEvents     <- 
    el "ul" .                   listWithKey dMap $ \k dv -> 
      el "li" . flip runReaderT k . todoItem $ dv

  ...

  let
    eChanges =
      switch . current . fmap mergeMap . fmap fst $
      dmEvents
    eRemoves =
      fmap Map.keys .
      switch . current . fmap mergeMap . fmap snd $
      dmEvents
  ...
```

##

```haskell
  ...

  dmEvents     <- 
    el "ul" . runEventWriterT . listWithKey dMap $ \k dv ->
      el "li" . flip runReaderT k . todoItem $ dv

  ...

  let
    eChanges =
      switch . current . fmap mergeMap . fmap fst $
      dmEvents
    eRemoves =
      fmap Map.keys .
      switch . current . fmap mergeMap . fmap snd $
      dmEvents
  ...
```

##

```haskell
  ...

  (_, eWriter) <-
    el "ul" . runEventWriterT . listWithKey dMap $ \k dv ->
      el "li" . flip runReaderT k . todoItem $ dv

  ...

  let
    eChanges =
      switch . current . fmap mergeMap . fmap fst $
      dmEvents
    eRemoves =
      fmap Map.keys .
      switch . current . fmap mergeMap . fmap snd $
      dmEvents
  ...
```

##

```haskell
  ...

  (_, eWriter) <-
    el "ul" . runEventWriterT . listWithKey dMap $ \k dv ->
      el "li" . flip runReaderT k . todoItem $ dv

  ...

  let
    eChanges =
      fmap (view mwChanges) $ 
      eWriter
    eRemoves =
      fmap Map.keys .
      switch . current . fmap mergeMap . fmap snd $
      dmEvents
  ...
```

##

```haskell
  ... 

  (_, eWriter) <-
    el "ul" . runEventWriterT . listWithKey dMap $ \k dv ->
      el "li" . flip runReaderT k . todoItem $ dv
  
  ...

  let
    eChanges = 
      fmap (view mwChanges) $ 
      eWriter
    eRemoves = 
      fmap (view mwRemoves) $
      eWriter

  ...
```
