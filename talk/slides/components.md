
# Components

##

```haskell
data TodoItem =
  TodoItem {
    _tiComplete :: Bool
  , _tiText     :: Text
  }

makeLenses ''TodoItem
```

##

```haskell
todoItem :: MonadWidget t m 
         => Dynamic t TodoItem 
         -> m (Event t (TodoItem -> TodoItem), Event t ())
```

##

```haskell
complete :: MonadWidget t m 
         => Dynamic t Bool 
         -> m (Event t Bool)
complete dComplete = do
  initial <- sample . current $ dComplete

  cb <- checkbox initial $
    def & checkboxConfig_setValue .~ updated dComplete

  pure $ cb ^. checkbox_change
```

##

```haskell
editRead :: MonadWidget t m 
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
textEdit :: MonadWidget t m 
         => Dynamic t (Map Text Text) 
         -> Dynamic t Text 
         -> m (Event t Text, Event t ())

editWrite :: MonadWidget t m 
          => Dynamic t Text 
          -> Workflow t m (Event t Text)
editWrite dText = Workflow $ do

  (eText, eDone) <- textEdit (pure mempty) dText

  pure (eText, editRead dText <$ eDone)
```

##

```haskell
edit :: MonadWidget t m 
     => Dynamic t Text 
     -> m (Event t Text, Event t ())
edit dText = do
  deText <- workflow $ editRead dText

  let
    eText = switchDyn deText
    eTextNonEmpty =       ffilter (not . Text.null) eText
    eTextEmpty    = () <$ ffilter        Text.null  eText

  pure (eTextNonEmpty, eTextEmpty)
```

<div class="demo" id="examples-component-pass1-edit"></div>

##

```haskell
remove :: MonadWidget t m 
       => m (Event t ())
remove =
  button "Remove"
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

<div class="demo" id="examples-component-pass1-todo-item"></div>
