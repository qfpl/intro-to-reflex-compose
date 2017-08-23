
# Collections

##

```haskell
addItemWidget :: MonadWidget t m 
              => m (Event t Text)
```

##

```haskell
  ...

  -- eAdd     :: Event t Text
  eAdd <- addItemWidget








  ...
```
##

```haskell
  ...

  -- eAdd     :: Event t Text
  eAdd <- addItemWidget

  -- dNextKey :: Dynamic t Int
  dNextKey <- count eAdd





  ...
```

##

```haskell
  ...

  -- eAdd     :: Event t Text
  eAdd <- addItemWidget

  -- dNextKey :: Dynamic t Int
  dNextKey <- count eAdd

  -- dMap     :: Dynamic t (Map Int Text)
  dMap <- foldDyn ($) Map.empty $
      Map.insert <$> current dNextKey <@> eAdd

  ...
```

##

```haskell
todoItem :: MonadWidget t m 
            -- The value of the text for the item
         => Dynamic t Text
            -- Fires when item should be removed
         -> m (Event t ())
```

##

```haskell
dMap 
  :: Dynamic t (Map Int Text)









  
```

##

```haskell
dMap 
  :: Dynamic t (Map Int Text)

todoItem
  :: MonadWidget t m
  => Dynamic t Text
  -> m (Event t ())




    
```

##

```haskell
dMap 
  :: Dynamic t (Map Int Text)

todoItem 
  :: MonadWidget t m
  => Dynamic t Text
  -> m (Event t ())

list 
  ::         Dynamic t (Map k v     )
  ->   (Dynamic t v -> m a   ) 
  -> m (Dynamic t (Map k a         ))
```

##

```haskell
dMap 
  :: Dynamic t (Map Int Text)

todoItem
  :: MonadWidget t m
  => Dynamic t Text
  -> m (Event t ())

list dMap 
  ::
       (Dynamic t Text -> m a) 
  -> m (Dynamic t (Map Int a       ))
```

##

```haskell
dMap 
  :: Dynamic t (Map Int Text)

todoItem
  :: MonadWidget t m
  => Dynamic t Text
  -> m (Event t ())

list dMap todoItem 
  ::

     m (Dynamic t (Map Int (Event t ())))
```

##

```haskell
  -- Dynamic t (Map Int (Event t ()))
  dmItems <-           list dMap $ \dv -> 
                         todoItem dv
```

##

```haskell
  -- Dynamic t (Map Int (Event t ()))
  dmItems <- el "ul" . list dMap $ \dv -> 
               el "li" . todoItem $ dv
```

##

<div class="demo" id="examples-collection-add"></div>

##

```haskell
  let
    eRemoves =
    







      -- Dynamic t (Map Int (Event t ()))
      dmItems
```

##

```haskell
  let
    eRemoves =
    





      -- Dynamic t (Event t (Map Int ()))
      fmap mergeMap $
      -- Dynamic t (Map Int (Event t ()))
      dmItems
```

##

```haskell
  let
    eRemoves =
    



      -- Behavior t (Event t (Map Int ()))
      current .
      -- Dynamic t (Event t (Map Int ()))
      fmap mergeMap $
      -- Dynamic t (Map Int (Event t ()))
      dmItems
```

##

```haskell
  let
    eRemoves =
    

      -- Event t (Map Int ())
      switch .
      -- Behavior t (Event t (Map Int ()))
      current .
      -- Dynamic t (Event t (Map Int ()))
      fmap mergeMap $
      -- Dynamic t (Map Int (Event t ()))
      dmItems
```

##

```haskell
  let
    eRemoves =
      -- Event t [Int]
      fmap Map.keys .
      -- Event t (Map Int ())
      switch .
      -- Behavior t (Event t (Map Int ()))
      current .
      -- Dynamic t (Event t (Map Int ()))
      fmap mergeMap $
      -- Dynamic t (Map Int (Event t ()))
      dmItems
```

##

```haskell
  dmText <- foldDyn ($) Map.empty                 $
      Map.insert <$> current dNextKey <@> eAdd



  dmItems <- el "ul" . list dMap $ \dv -> 
    el "li"  todoItem $ dv








  
```

##

```haskell
  dmText <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dNextKey <@> eAdd

    ]

  dmItems <- el "ul" . list dMap $ \dv -> 
    el "li"  todoItem $ dv








  
```
##

```haskell
  dmText <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dNextKey <@> eAdd

    ]

  dmItems <- el "ul" . list dMap $ \dv -> 
    el "li"  todoItem $ dv

  let
    -- Event t [Int]
    eRemoves =
      fmap Map.keys .
      switch .
      current .
      fmap mergeMap $
      dmItems
```

##

```haskell
  dmText <- foldDyn ($) Map.empty . mergeWith (.) $ [
      Map.insert <$> current dNextKey <@> eAdd
    , flip (foldr Map.delete) <$> eRemoves
    ]

  dmItems <- el "ul" . list dMap $ \dv -> 
    el "li"  todoItem $ dv

  let
    -- Event t [Int]
    eRemoves =
      fmap Map.keys .
      switch .
      current .
      fmap mergeMap $
      dmItems
```

##

<div class="demo" id="examples-collection-remove"></div>

<!--
##

TODO add clear complete

##

<div class="demo" id="examples-collection-remove-complete"></div>
-->



