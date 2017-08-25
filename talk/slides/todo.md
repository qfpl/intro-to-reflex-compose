
# TodoMVC

##

We'll start simple

##

```haskell
data TodoItemConfig t =
  TodoItemConfig
  }
```

```haskell
data TodoItem t =
  TodoItem {
    _todoItem_dComplete :: Dynamic t Bool
  , _todoItem_eRemove   :: Event t ()
  }
```

##

```haskell
complete :: MonadWidget t m => m (Dynamic t Bool)
complete = do
  cb <- checkbox False def
  pure $ cb ^. checkbox_value
```

```haskell
edit :: MonadWidget t m => m ()
edit = text "Placeholder"
```

```haskell
remove :: MonadWidget t m => m (Event t ())
remove = button "Remove"
```

##

```haskell
todoItem' :: MonadWidget t m 
          => TodoItemConfig t 
          -> m (TodoItem t)
todoItem' _ = do
  dComplete <- el "td" $ complete
  _ <-         el "td" $ edit
  eRemove <-   el "td" $ remove
  pure $ TodoItem dComplete eRemove
```

```haskell
todoItem :: MonadWidget t m 
         => TodoItemConfig t 
         -> m (TodoItem t)
todoItem tic = el "tr" $ todoItem1' tic
```

##

<div id="examples-todomvc-todoitem-1"></div>

##

Let's add a button to clear complete items

##

```haskell
clearCompleteWidget :: MonadWidget t m 
                    => Dynamic t Bool 
                    -> m (Event t ())
clearCompleteWidget dAnyComplete =
  let
    mkAttrs False = "hidden" =: "true"
    mkAttrs True  = mempty

    dAttrs = mkAttrs <$> dAnyComplete
  in
    elDynAttr "div" dAttrs $
      button "Clear complete"
```

##

And we'll make it work with our todo item

##

```haskell
data TodoItemConfig t =
  TodoItemConfig {

  }
```

##

```haskell
data TodoItemConfig t =
  TodoItemConfig {
    _todoItemConfig_eClearComplete :: Event t ()
  }
```

##

```haskell
todoItem' :: MonadWidget t m 
          => TodoItemConfig t 
          -> m (TodoItem t)
todoItem' _                               = do
  dComplete <-    el "td" $ complete
  _ <-            el "td" $ edit
  eRemove <-      el "td" $ remove
  
  
  
  
  
  
  pure $ TodoItem dComplete eRemove
```

##

```haskell
todoItem' :: MonadWidget t m 
          => TodoItemConfig t 
          -> m (TodoItem t)
todoItem' (TodoItemConfig eClearComplete) = do
  dComplete <-    el "td" $ complete
  _ <-            el "td" $ edit
  eRemoveClick <- el "td" $ remove

  let eRemove = leftmost [
        gate (current dComplete) eClearComplete
      , eRemoveClick
      ]

  pure $ TodoItem dComplete eRemove
```

##

<div id="examples-todomvc-todoitem-2"></div>

##

Let's add a filter for completion status

##

```haskell
data Filter =
    FAll
  | FActive
  | FCompleted
  deriving (Eq, Show)
```

```haskell
matchesCompletion :: Filter -> Bool -> Bool
matchesCompletion FAll = 
  const True
matchesCompletion FActive = 
  not
matchesCompletion FCompleted = 
  id
```

##

```haskell
filterLink :: MonadWidget t m 
           => Filter -> Text -> Text -> Dynamic t Filter 
           -> m (Event t Filter)
filterLink sel label url dFilter =
  let
    urlAttrs = "hrel" =: url

    mkClass f
      | f == sel  = "class" =: "selected"
      | otherwise = mempty

    dAttrs = fmap (<> urlAttrs) . fmap mkClass $ dFilter
  in el "td" $ do
    (l, _) <- elDynAttr' "a" dAttrs $ text label
    pure $ sel <$ domEvent Click l
```

##

```haskell
filterWidget :: MonadWidget t m => m (Dynamic t Filter)
filterWidget = el "table" . el "tr" $ mdo
  linkAll       <-
    filterLink FAll "All" "#/" dFilter
  linkActive    <-
    filterLink FActive "Active" "#/active" dFilter
  linkCompleted <-
    filterLink FCompleted "Completed" "#/completed" dFilter

  dFilter <- holdDyn FAll . leftmost $ [
      linkAll , linkActive , linkCompleted
  ]

  pure dFilter
```

##

And we'll make it work with our todo item

##

```haskell
data TodoItemConfig t =
  TodoItemConfig {

    _todoItemConfig_eClearComplete :: Event t ()
  }
```

##

```haskell
data TodoItemConfig t =
  TodoItemConfig {
    _todoItemConfig_dFilter        :: Dynamic t Filter
  , _todoItemConfig_eClearComplete :: Event t ()
  }
```

##

```haskell
todoItem :: MonadWidget t m 
         => TodoItemConfig t 
         -> m (TodoItem t)
todoItem tic =  do
  

  
  
  
  
  
  
  
  
  
  ti <- el        "tr"        $
    todoItem' tic

  pure ti
```

##

```haskell
todoItem :: MonadWidget t m 
         => TodoItemConfig t 
         -> m (TodoItem t)
todoItem tic = mdo
  let
    dVisible =
      matchesCompletion <$>
        _todoItemConfig_dFilter tic <*>
        _todoItem_dComplete ti

    mkAttrs True  = mempty
    mkAttrs False = "hidden" =: "true"

    dAttrs = mkAttrs <$> dVisible

  ti <- elDynAttr "tr" dAttrs $
    todoItem' tic

  pure ti
```

## 

<div id="examples-todomvc-todoitem-3"></div>

##

Let's add the ability to edit that placeholder

##

```haskell
editingShow :: MonadWidget t m 
            => Text 
            -> m (Event t (), Event t Text)
editingShow initialValue = do
  (e, _) <- el' "div" $ text initialValue
  pure (never, initialValue <$ domEvent Dblclick e)
```

##

```haskell
editingEdit :: MonadWidget t m 
            => Text 
            -> m (Event t (), Event t Text)
editingEdit initialValue = do
  ti <- textInput $ def & 
    textInputConfig_initialValue .~ initialValue

  let eBlur = void . ffilter not . updated $ 
      ti ^. textInput_hasFocus

    bValue = current $ ti ^. textInput_value
    eDoneNew = bValue <@ leftmost [getKey ti Enter, eBlur]
    eDoneOld = initialValue <$ getKey ti Escape
    eDone = leftmost [eDoneOld, eDoneNew]

    eRemove = void . ffilter Text.null $ eDone

  pure (eRemove, eDone)
```

##

```haskell
edit ::             
        MonadWidget t m 
     => Text
        m (          )
edit initialValue =







    text initialValue

```

##

```haskell
edit :: forall t m. 
        MonadWidget t m 
     => Text 
     -> m (Event t ())
edit ec           =
  let
    adjust f = Worflow . fmap (fmap (fmap f))

    wShow, wEdit :: Text -> Workflow t m (Event t ())
    wShow = adjust wEdit . editingShow
    wEdit = adjust wShow . editingEdit
  in do
    deRemove <- workflow $ wShow ec
    pure $ switchPromptlyDyn deRemove
```

##

And we'll make it work with our todo item

##

```haskell
data TodoItemConfig t =
  TodoItemConfig {
    _todoItemConfig_dFilter        :: Dynamic t Filter
  , _todoItemConfig_eClearComplete :: Event t ()

  }
```

##

```haskell
data TodoItemConfig t =
  TodoItemConfig {
    _todoItemConfig_dFilter        :: Dynamic t Filter
  , _todoItemConfig_eClearComplete :: Event t ()
  , _todoItemConfig_initialValue   :: Text
  }
```

##

```haskell
todoItem' :: MonadWidget t m 
          => TodoItemConfig t 
          -> m (TodoItem t)
todoItem' (TodoItemConfig eClearComplete               ) = do
  dComplete <-    el "td" $ complete
  _ <-            el "td" $ edit
  eRemoveClick <- el "td" $ remove

  let
    eRemove = leftmost [
        gate (current dComplete) eClearComplete

      , eRemoveClick
      ]

  pure $ TodoItem dComplete eRemove
```

##

```haskell
todoItem' :: MonadWidget t m 
          => TodoItemConfig t 
          -> m (TodoItem t)
todoItem' (TodoItemConfig _ eClearComplete initialValue) = do
  dComplete <-    el "td" $ complete
  eRemoveEmpty <- el "td" $ edit initialValue
  eRemoveClick <- el "td" $ remove

  let
    eRemove = leftmost [
        gate (current dComplete) eClearComplete
      , eRemoveEmpty
      , eRemoveClick
      ]

  pure $ TodoItem dComplete eRemove
```

##

<div id="examples-todomvc-todoitem-4"></div>

##

TODO the collection management bit
