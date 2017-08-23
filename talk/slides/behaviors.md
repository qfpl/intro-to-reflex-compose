
# `Behavior`

##

```haskell
data Behavior t a



 
```

##

```haskell
data Behavior t a

  ~

t -> a
```

##

```haskell
hold :: MonadHold t m
     => a
     -> Event t a
     -> m (Behavior t a)
```

##

```haskell
sampleBlue                = do
  bColour <- hold
  
```

##

```haskell
sampleBlue                = do
  bColour <- hold Blue
  
```

##

```haskell
sampleBlue eInput         = do
  bColour <- hold Blue
 
```

##

```haskell
sampleBlue eInput         = do
  bColour <- hold Blue eInput
 
```

##

```haskell
tag :: Reflex t 
    => Behavior t a
    -> Event t b
    -> Event t a
```

##

```haskell
sampleBlue eInput         = do
  bColour <- hold Blue eInput
         tag 
```

##

```haskell
sampleBlue eInput         = do
  bColour <- hold Blue eInput
         tag bColour
```

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
         tag bColour
```

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
         tag bColour eSample
```

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
  pure $ tag bColour eSample
```

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
  pure $ tag bColour eSample
```

<div id="examples-behaviors-sampleBlue1"></div>

<!--
##

The state doesn't change until the frame _after_ the firing of the `Event`s in `hold`.

##

We can see that by sampling from the `Behavior` when any of the buttons are pressed
-->

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput

  pure $ tag bColour eSample
```

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
  let eAny = leftmost [                     ]
  pure $ tag bColour eSample
```

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
  let eAny = leftmost [              eSample]
  pure $ tag bColour eSample
```

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
  let eAny = leftmost [      eInput, eSample]
  pure $ tag bColour eSample
```

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
  let eAny = leftmost [() <$ eInput, eSample]
  pure $ tag bColour eSample
```

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
  let eAny = leftmost [() <$ eInput, eSample]
  pure $ tag bColour eAny
```

##

```haskell
sampleBlue eInput eSample = do
  bColour <- hold Blue eInput
  let eAny = leftmost [() <$ eInput, eSample]
  pure $ tag bColour eAny
```

<div id="examples-behaviors-sampleBlue2"></div>

##

```haskell
attach          :: Reflex t 
                => 
                   Behavior t a 
                -> Event t b 
                -> Event t (a, b)
```

##

```haskell
attachWith      :: Reflex t 
                => (a -> b -> c) 
                -> Behavior t a
                -> Event t b
                -> Event t c
```

##

```haskell
attachWithMaybe :: Reflex t
                => (a -> b -> Maybe c) 
                -> Behavior t a
                -> Event t b
                -> Event t c
```

##

```haskell
gate            :: Reflex t 
                => 
                   Behavior t Bool
                -> Event t a
                -> Event t a
```

##

```haskell
instance Functor (Behavior t) where ..
```

##

```haskell
sampleBlue     eInput eSample = do
  bColour <- hold Blue eInput

  pure $ tag        bColour eSample
```

##

```haskell
sampleFlipBlue eInput eSample = do
  bColour <- hold Blue eInput

  pure $ tag        bColour eSample
```

##

```haskell
sampleFlipBlue eInput eSample = do
  bColour <- hold Blue eInput
  let bFlippedColour = flipColour <$> bColour
  pure $ tag        bColour eSample
```

##

```haskell
sampleFlipBlue eInput eSample = do
  bColour <- hold Blue eInput
  let bFlippedColour = flipColour <$> bColour
  pure $ tag bFlippedColour eSample
```

##

```haskell
sampleFlipBlue eInput eSample = do
  bColour <- hold Blue eInput
  let bFlippedColour = flipColour <$> bColour
  pure $ tag bFlippedColour eSample
```

<div id="examples-behaviors-sampleFlipBlue"></div>

##

```haskell
b <- hold (f initial) (f <$> eChange)
pure b
```
~
```haskell
b <- hold initial eChange
pure $ f <$> b
```

##

```haskell
instance Applicative (Behavior t) where ..
```

##

```haskell
sampleBlue       eInput eSample = do
  bColour <- hold    Blue eInput
  pure $ tag bColour     eSample
```

##

```haskell
sampleAlwaysBlue eInput eSample = do
  bColour <- hold    Blue eInput
  pure $ tag bColour     eSample
```

##

```haskell
sampleAlwaysBlue eInput eSample = do
  let bColour = pure Blue
  pure $ tag bColour     eSample
```

##

```haskell
sampleAlwaysBlue eInput eSample = do

  pure $ tag (pure Blue) eSample
```

##

```haskell
sampleAlwaysBlue eInput eSample =

         tag (pure Blue) eSample
```

##

```haskell
sampleAlwaysBlue eInput eSample =

         tag (pure Blue) eSample
```

<div id="examples-behaviors-sampleAlwaysBlue"></div>

##

```haskell
sampleBlue eInput          eSample = do
  bColour  <- hold Blue eInput


  pure $ tag bColour                            eSample
```

##

```haskell
samplePair eInput          eSample = do
  bColour  <- hold Blue eInput


  pure $ tag bColour                            eSample
```

##

```haskell
samplePair eInput1         eSample = do
  bColour1 <- hold Blue eInput1


  pure $ tag bColour1                           eSample
```

##

```haskell
samplePair eInput1 eInput2 eSample = do
  bColour1 <- hold Blue eInput1


  pure $ tag bColour1                           eSample
```

##

```haskell
samplePair eInput1 eInput2 eSample = do
  bColour1 <- hold Blue eInput1
  bColour2 <- hold Blue eInput2

  pure $ tag bColour1                           eSample
```

##

```haskell
samplePair eInput1 eInput2 eSample = do
  bColour1 <- hold Blue eInput1
  bColour2 <- hold Blue eInput2
  let bPair = (,) <$> bColour1 <*> bColour2
  pure $ tag bColour1                           eSample
```

##

```haskell
samplePair eInput1 eInput2 eSample = do
  bColour1 <- hold Blue eInput1
  bColour2 <- hold Blue eInput2
  let bPair = (,) <$> bColour1 <*> bColour2
  pure $ tag bPair                              eSample
```

##

```haskell
samplePair eInput1 eInput2 eSample = do
  bColour1 <- hold Blue eInput1
  bColour2 <- hold Blue eInput2

  pure $ tag ((,) <$> bColour1 <*> bColour2)    eSample
```

##

```haskell
samplePair eInput1 eInput2 eSample = do
  bColour1 <- hold Blue eInput1
  bColour2 <- hold Blue eInput2

  pure $      (,) <$> bColour1 <*> bColour2  <@ eSample
```

##

```haskell
samplePair eInput1 eInput2 eSample = do
  bColour1 <- hold Blue eInput1
  bColour2 <- hold Blue eInput2

  pure $      (,) <$> bColour1 <*> bColour2  <@ eSample
```

<div id="examples-behaviors-samplePair"></div>

##

```haskell
instance Reflex t => 
         Monad (Behavior t) where ..
```

```haskell
instance (Reflex t, Monoid a) => 
         Monoid (Behavior t a) where ..

```

```haskell
instance (Reflex t, Num a) => 
         Num (Behavior t a) where ..
```

```haskell
instance (Reflex t, IsString a) => 
         IsString (Behavior t a) where ..
```
