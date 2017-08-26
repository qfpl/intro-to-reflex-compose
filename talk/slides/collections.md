
# Collections

##

```haskell
add :: MonadWidget t m 
    => m (Event t Text)
```

##

```haskell
  ...

  -- eAddText :: Event t Text
  eAddText <- add











  ...
```

##

```haskell
  ...

  -- eAddText :: Event t Text
  eAddText <- add
  let 
    -- eAdd :: Event t TodoItem
    eAdd = TodoItem False <$> eAddText








  ...
```

##

```haskell
  ...

  -- eAddText :: Event t Text
  eAddText <- add
  let 
    -- eAdd :: Event t TodoItem
    eAdd = TodoItem False <$> eAddText

  -- dCount :: Dynamic t Int
  dCount <- count eAdd





  ...
```

##

```haskell
  ...

  -- eAddText :: Event t Text
  eAddText <- add
  let 
    -- eAdd :: Event t TodoItem
    eAdd = TodoItem False <$> eAddText

  -- dCount :: Dynamic t Int
  dCount <- count eAdd

  -- dMap     :: Dynamic t (Map Int TodoItem)
  dMap <- foldDyn ($) Map.empty $
      Map.insert <$> current dCount <@> eAdd

  ...
```

##

```haskell
todoItem :: MonadWidget t m 
         => Dynamic t TodoItem
         -> m (Event t (TodoItem -> TodoItem), Event t ())
```

##

```haskell
dMap 
  :: Dynamic t (Map Int TodoItem)









  
```

##

```haskell
dMap 
  :: Dynamic t (Map Int TodoItem)

todoItem
  :: MonadWidget t m
  => Dynamic t TodoItem
  -> m (Event t (TodoItem -> TodoItem), Event t ())




    
```

##

```haskell
dMap 
  :: Dynamic t (Map Int TodoItem)

todoItem 
  :: MonadWidget t m
  => Dynamic t TodoItem
  -> m (Event t (TodoItem -> TodoItem), Event t ())

list 
  ::    Dynamic t (Map k   v                                )
  ->   (Dynamic t          v  -> m                        a ) 
  -> m (Dynamic t (Map k                                  a))
```

##

```haskell
dMap 
  :: Dynamic t (Map Int TodoItem)

todoItem 
  :: MonadWidget t m
  => Dynamic t TodoItem
  -> m (Event t (TodoItem -> TodoItem), Event t ())

list 
  ::    D t (Map k   v                                )
  ->   (D t          v  -> m                        a ) 
  -> m (D t (Map k                                  a))
```

##

```haskell
dMap 
  :: Dynamic t (Map Int TodoItem)

todoItem 
  :: MonadWidget t m
  => Dynamic t TodoItem
  -> m (Event t (TodoItem -> TodoItem), Event t ())

list dMap
  ::    D t (Map k   v                                )
  ->   (D t          v  -> m                        a ) 
  -> m (D t (Map k                                  a))
```

##

```haskell
dMap 
  :: Dynamic t (Map Int TodoItem)

todoItem 
  :: MonadWidget t m
  => Dynamic t TodoItem
  -> m (Event t (TodoItem -> TodoItem), Event t ())

list dMap
  ::    D t (Map Int v                                )
  ->   (D t          v  -> m                        a ) 
  -> m (D t (Map k                                  a))
```

##

```haskell
dMap 
  :: Dynamic t (Map Int TodoItem)

todoItem 
  :: MonadWidget t m
  => Dynamic t TodoItem
  -> m (Event t (TodoItem -> TodoItem), Event t ())

list dMap
  ::    D t (Map Int v                                )
  ->   (D t          v  -> m                        a ) 
  -> m (D t (Map Int                                a))
```

##

```haskell
dMap 
  :: Dynamic t (Map Int TodoItem)

todoItem 
  :: MonadWidget t m
  => Dynamic t TodoItem
  -> m (Event t (TodoItem -> TodoItem), Event t ())

list dMap
  ::    D t (Map Int TI                               )
  ->   (D t          v  -> m                        a ) 
  -> m (D t (Map Int                                a))
```

##

```haskell
dMap 
  :: Dynamic t (Map Int TodoItem)

todoItem 
  :: MonadWidget t m
  => Dynamic t TodoItem
  -> m (Event t (TodoItem -> TodoItem), Event t ())

list dMap
  ::    D t (Map Int TI                               )
  ->   (D t          TI -> m                        a ) 
  -> m (D t (Map Int                                a))
```

##

```haskell
dMap 
  :: Dynamic t (Map Int TodoItem)

todoItem 
  :: MonadWidget t m
  => Dynamic t TodoItem
  -> m (Event t (TodoItem -> TodoItem), Event t ())

list dMap 
  ::
       (D t          TI -> m                        a ) 
  -> m (D t (Map Int                                a))
```

##

```haskell
dMap 
  :: Dynamic t (Map Int TodoItem)

todoItem 
  :: MonadWidget t m
  => Dynamic t TodoItem
  -> m (Event t (TodoItem -> TodoItem), Event t ())

list dMap todoItem
  ::
       (D t          TI -> m                        a ) 
  -> m (D t (Map Int                                a))
```

##

```haskell
dMap 
  :: Dynamic t (Map Int TodoItem)

todoItem 
  :: MonadWidget t m
  => Dynamic t TodoItem
  -> m (Event t (TodoItem -> TodoItem), Event t ())

list dMap todoItem
  ::
       (D t          TI -> m (E t (TI -> TI), E t ()) ) 
  -> m (D t (Map Int                                a))
```

##

```haskell
dMap 
  :: Dynamic t (Map Int TodoItem)

todoItem 
  :: MonadWidget t m
  => Dynamic t TodoItem
  -> m (Event t (TodoItem -> TodoItem), Event t ())

list dMap todoItem
  ::
       (D t          TI -> m (E t (TI -> TI), E t ()) ) 
  -> m (D t (Map Int         (E t (TI -> TI), E t ())))
```

##

```haskell
dMap 
  :: Dynamic t (Map Int TodoItem)

todoItem 
  :: MonadWidget t m
  => Dynamic t TodoItem
  -> m (Event t (TodoItem -> TodoItem), Event t ())

list dMap todoItem
  ::

  -> m (D t (Map Int         (E t (TI -> TI), E t ())))
```

##

```haskell
  -- D t (Map Int (E t (TI -> TI), E t ()))
  dmEvents <-           list dMap $ \dv -> 
                          todoItem dv
```

##

```haskell
  -- D t (Map Int (E t (TI -> TI), E t ()))
  dmEvents <- el "ul" . list dMap $ \dv -> 
                el "li" . todoItem $ dv
```

##

<div class="demo" id="examples-component-pass1-todo-list-1"></div>

##

```haskell
  let
    eRemoves =
    









      -- D t (Map Int (E (TI -> TI),  E t ()))
      dmEvents
```

##

```haskell
  let
    eRemoves =
    







      -- D t (Map Int (E t ()))
      fmap snd $
      -- D t (Map Int (E (TI -> TI),  E t ()))
      dmEvents
```

##

```haskell
  let
    eRemoves =
    





      -- D t (E t (Map Int ()))
      fmap mergeMap .
      -- D t (Map Int (E t ()))
      fmap snd $
      -- D t (Map Int (E (TI -> TI),  E t ()))
      dmEvents
```

##

```haskell
  let
    eRemoves =
    



      -- B t (E t (Map Int ()))
      current .
      -- D t (E t (Map Int ()))
      fmap mergeMap .
      -- D t (Map Int (E t ()))
      fmap snd $
      -- D t (Map Int (E (TI -> TI),  E t ()))
      dmEvents
```

##

```haskell
  let
    eRemoves =
    

      -- E t (Map Int ())
      switch .
      -- B t (E t (Map Int ()))
      current .
      -- D t (E t (Map Int ()))
      fmap mergeMap .
      -- D t (Map Int (E t ()))
      fmap snd $
      -- D t (Map Int (E (TI -> TI),  E t ()))
      dmEvents
```

##

```haskell
  let
    eRemoves =
      -- E t [Int]
      fmap Map.keys .
      -- E t (Map Int ())
      switch .
      -- B t (E t (Map Int ()))
      current .
      -- D t (E t (Map Int ()))
      fmap mergeMap .
      -- D t (Map Int (E t ()))
      fmap snd $
      -- D t (Map Int (E (TI -> TI),  E t ()))
      dmEvents
```

##

```haskell
  dMap <- foldDyn ($) Map.empty                 $
      Map.insert <$> current dCount <@> eAdd



  dmEvents <- el "ul" . list dMap $ \dv -> 
    el "li"  todoItem $ dv






  
```

##

```haskell
  dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dCount <@> eAdd

    ]

  dmEvents <- el "ul" . list dMap $ \dv -> 
    el "li"  todoItem $ dv






  
```
##

```haskell
  dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dCount <@> eAdd

    ]

  dmEvents <- el "ul" . list dMap $ \dv -> 
    el "li"  todoItem $ dv

  let
    -- Event t [Int]
    eRemoves =
      fmap Map.keys .
      switch . current . fmap mergeMap . fmap snd $
      dmEvents
```

##

```haskell
  dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dCount <@> eAdd
    , flip (foldr Map.delete) <$> eRemoves
    ]

  dmEvents <- el "ul" . list dMap $ \dv -> 
    el "li"  todoItem $ dv

  let
    -- Event t [Int]
    eRemoves =
      fmap Map.keys .
      switch . current . fmap mergeMap . fmap snd $
      dmEvents
```

##

<div class="demo" id="examples-component-pass1-todo-list-2"></div>

##

```haskell
  dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dCount <@> eAdd
    , flip (foldr Map.delete) <$> eRemoves

    ]

  dmEvents <- el "ul" . list dMap $ \dv -> 
    el "li"  todoItem $ dv

  let
  
  
  
  
    -- Event t [Int]
    eRemoves =
      fmap Map.keys .
      switch . current . fmap mergeMap . fmap snd $
      dmEvents
```

##

```haskell
  dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dCount <@> eAdd
    , flip (foldr Map.delete) <$> eRemoves

    ]

  dmEvents <- el "ul" . list dMap $ \dv -> 
    el "li"  todoItem $ dv

  let
    -- Event t (Map Int (TI -> TI))
    eChanges =
      switch . current . fmap mergeMap . fmap fst $
      dmEvents
    -- Event t [Int]
    eRemoves =
      fmap Map.keys .
      switch . current . fmap mergeMap . fmap snd $
      dmEvents
```

##

```haskell
  dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dCount <@> eAdd
    , flip (foldr Map.delete) <$> eRemoves
    , applyMap <$> eChanges
    ]

  dmEvents <- el "ul" . list dMap $ \dv -> 
    el "li"  todoItem $ dv

  let
    -- Event t (Map Int (TI -> TI))
    eChanges =
      switch . current . fmap mergeMap . fmap fst $
      dmEvents
    -- Event t [Int]
    eRemoves =
      fmap Map.keys .
      switch . current . fmap mergeMap . fmap snd $
      dmEvents
```

##

<div class="demo" id="examples-component-pass1-todo-list-3"></div>

##

```haskell
  dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dCount <@> eAdd
    , flip (foldr Map.delete) <$> eRemoves
    , applyMap <$> eChanges


    ]
    
  ...









  
```

##

```haskell
  dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dCount <@> eAdd
    , flip (foldr Map.delete) <$> eRemoves
    , applyMap <$> eChanges


    ]
    
  ...

  let
    -- dComplete :: Dynamic t [Bool]
    dComplete = 
      fmap (Map.elems . fmap (view tiComplete)) dMap




  
```

##

```haskell
  dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dCount <@> eAdd
    , flip (foldr Map.delete) <$> eRemoves
    , applyMap <$> eChanges


    ]
    
  ...

  let
    -- dComplete :: Dynamic t [Bool]
    dComplete = 
      fmap (Map.elems . fmap (view tiComplete)) dMap
    dAllComplete = fmap and dComplete



  
```

##

```haskell
  dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dCount <@> eAdd
    , flip (foldr Map.delete) <$> eRemoves
    , applyMap <$> eChanges


    ]
    
  ...

  let
    -- dComplete :: Dynamic t [Bool]
    dComplete = 
      fmap (Map.elems . fmap (view tiComplete)) dMap
    dAllComplete = fmap and dComplete


  eMarkAllComplete <- el "div" $ markAllComplete dAllComplete
  
```

##

```haskell
  dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dCount <@> eAdd
    , flip (foldr Map.delete) <$> eRemoves
    , applyMap <$> eChanges
    , (fmap . set tiComplete) <$> eMarkAllComplete

    ]
    
  ...

  let
    -- dComplete :: Dynamic t [Bool]
    dComplete = 
      fmap (Map.elems . fmap (view tiComplete)) dMap
    dAllComplete = fmap and dComplete


  eMarkAllComplete <- el "div" $ markAllComplete dAllComplete
  
```

##

```haskell
  dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dCount <@> eAdd
    , flip (foldr Map.delete) <$> eRemoves
    , applyMap <$> eChanges
    , (fmap . set tiComplete) <$> eMarkAllComplete

    ]
    
  ...

  let
    -- dComplete :: Dynamic t [Bool]
    dComplete = 
      fmap (Map.elems . fmap (view tiComplete)) dMap
    dAllComplete = fmap and dComplete
    dAnyComplete = fmap or dComplete

  eMarkAllComplete <- el "div" $ markAllComplete dAllComplete
  
```

##

```haskell
  dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dCount <@> eAdd
    , flip (foldr Map.delete) <$> eRemoves
    , applyMap <$> eChanges
    , (fmap . set tiComplete) <$> eMarkAllComplete

    ]
    
  ...

  let
    -- dComplete :: Dynamic t [Bool]
    dComplete = 
      fmap (Map.elems . fmap (view tiComplete)) dMap
    dAllComplete = fmap and dComplete
    dAnyComplete = fmap or dComplete

  eMarkAllComplete <- el "div" $ markAllComplete dAllComplete
  eClearComplete   <- el "div" $ clearComplete   dAnyComplete
```

##

```haskell
  dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dCount <@> eAdd
    , flip (foldr Map.delete) <$> eRemoves
    , applyMap <$> eChanges
    , (fmap . set tiComplete) <$> eMarkAllComplete
    , Map.filter (not . view tiComplete) <$ eClearComplete
    ]
    
  ...

  let
    -- dComplete :: Dynamic t [Bool]
    dComplete = 
      fmap (Map.elems . fmap (view tiComplete)) dMap
    dAllComplete = fmap and dComplete
    dAnyComplete = fmap or dComplete

  eMarkAllComplete <- el "div" $ markAllComplete dAllComplete
  eClearComplete   <- el "div" $ clearComplete   dAnyComplete
```

##

<div class="demo" id="examples-component-pass1-todo-list-4"></div>

##

```haskell
todoList :: MonadWidget t m 
         => 
            m ()
todoList     = mdo



  eAddText <- add
  let
    eAdd = TodoItem False <$> eAddText

  dCount <- count eAdd
  
  

  dMap <- foldDyn ($) Map.empty  . mergeWith (.) $ [
                Map.insert <$> current dCount <@> eAdd
                ...
```

##

```haskell
todoList :: MonadWidget t m 
         => [TodoItem]
         -> m ()
todoList     = mdo



  eAddText <- add
  let
    eAdd = TodoItem False <$> eAddText

  dCount <- count eAdd
  
  

  dMap <- foldDyn ($) Map.empty  . mergeWith (.) $ [
                Map.insert <$> current dCount <@> eAdd
                ...
```

##

```haskell
todoList :: MonadWidget t m 
         => [TodoItem]
         -> m ()
todoList tis = mdo



  eAddText <- add
  let
    eAdd = TodoItem False <$> eAddText

  dCount <- count eAdd
  
  

  dMap <- foldDyn ($) Map.empty  . mergeWith (.) $ [
                Map.insert <$> current dCount <@> eAdd
                ...
```

##

```haskell
todoList :: MonadWidget t m 
         => [TodoItem]
         -> m ()
todoList tis = mdo
  let
    initialMap = Map.fromList . zip [0..] $ tis

  eAddText <- add
  let
    eAdd = TodoItem False <$> eAddText

  dCount <- count eAdd
  
  

  dMap <- foldDyn ($) Map.empty  . mergeWith (.) $ [
                Map.insert <$> current dCount <@> eAdd
                ...
```

##

```haskell
todoList :: MonadWidget t m 
         => [TodoItem]
         -> m ()
todoList tis = mdo
  let
    initialMap = Map.fromList . zip [0..] $ tis

  eAddText <- add
  let
    eAdd = TodoItem False <$> eAddText

  dAdds  <- count eAdd
  let
    dCount = (+ length tis) <$> dAdds

  dMap <- foldDyn ($) Map.empty  . mergeWith (.) $ [
                Map.insert <$> current dCount <@> eAdd
                ...
```

##

```haskell
todoList :: MonadWidget t m 
         => [TodoItem] 
         -> m ()
todoList tis = mdo
  let
    initialMap = Map.fromList . zip [0..] $ tis

  eAddText <- add
  let
    eAdd = TodoItem False <$> eAddText

  dAdds  <- count eAdd
  let
    dCount = (+ length tis) <$> dAdds

  dMap <- foldDyn ($) initialMap . mergeWith (.) $ [
                Map.insert <$> current dCount <@> eAdd
                ...
```

##

<div class="demo" id="examples-component-pass1-todo-list-5"></div>

