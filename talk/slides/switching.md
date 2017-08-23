
# Switching widgets

##

```haskell
textWidget   :: MonadWidget t m 
             => m (Event Text)
```

<div class="demo" id="examples-switch-demo-text"></div>

```haskell
buttonWidget :: MonadWidget t m 
             => m (Event Text)
```

<div class="demo" id="examples-switch-demo-button"></div>

```haskell
tickWidget   :: MonadWidget t m 
             => m (Event Text)
```

<div class="demo" id="examples-switch-demo-tick"></div>

##

```haskell
  eSwitch <- el "div" $
    button "Switch"


















  
```

##

```haskell
  eSwitch <- el "div" $
    button "Switch"

  dToggle <- toggle True eSwitch
















  
```

##

```haskell
  eSwitch <- el "div" $
    button "Switch"

  dToggle <- toggle True eSwitch

  let
    dNotToggle = not <$> dToggle













  
```

##

```haskell
  eSwitch <- el "div" $
    button "Switch"

  dToggle <- toggle True eSwitch

  let
    dNotToggle = not <$> dToggle

...

  -- interesting stuff goes here

...







  
```

##

```haskell
  eSwitch <- el "div" $
    button "Switch"

  dToggle <- toggle True eSwitch

  let
    dNotToggle = not <$> dToggle

...

  -- interesting stuff goes here

...

  dText <- holdDyn "" . leftmost $ [
               eText
             , "" <$ eSwitch
             ]


  
```

##

```haskell
  eSwitch <- el "div" $
    button "Switch"

  dToggle <- toggle True eSwitch

  let
    dNotToggle = not <$> dToggle

...

  -- interesting stuff goes here

...

  dText <- holdDyn "" . leftmost $ [
               eText
             , "" <$ eSwitch
             ]

  el "div" $
    dynText dText
```

##

Option 1: Hide things

##

```haskell
  let
    mkHidden False = "hide"
    mkHidden True  = ""















  
```

##

```haskell
  let
    mkHidden False = "hide"
    mkHidden True  = ""

    dHide1 = mkHidden <$>    dToggle
    dHide2 = mkHidden <$> dNotToggle












  
```

##

```haskell
  let
    mkHidden False = "hide"
    mkHidden True  = ""

    dHide1 = mkHidden <$>    dToggle
    dHide2 = mkHidden <$> dNotToggle

  eText1 <- elDynClass "div" dHide1 $
    textWidget









  
```

##

```haskell
  let
    mkHidden False = "hide"
    mkHidden True  = ""

    dHide1 = mkHidden <$>    dToggle
    dHide2 = mkHidden <$> dNotToggle

  eText1 <- elDynClass "div" dHide1 $
    textWidget

  eText2 <- elDynClass "div" dHide2 $
    buttonWidget






  
```

##

```haskell
  let
    mkHidden False = "hide"
    mkHidden True  = ""

    dHide1 = mkHidden <$>    dToggle
    dHide2 = mkHidden <$> dNotToggle

  eText1 <- elDynClass "div" dHide1 $
    textWidget

  eText2 <- elDynClass "div" dHide2 $
    buttonWidget

  let
    eText =
      leftmost [
          gate (current    dToggle) eText1
        , gate (current dNotToggle) eText2
        ]
```

##

<div class="demo" id="examples-switch-hide-button"></div>

##

<div class="demo" id="examples-switch-hide-tick"></div>

##

Option 2: Switch widgets out

##

```haskell
widgetHold :: (MonadAdjust t m, MonadHold t m) 
           => m a 
           -> Event t (m a) 
           -> m (Dynamic t a)
```

##

```haskell
  let
    eShow1  = ffilter id . updated $ dToggle
    eShow2  = ffilter id . updated $ dNotToggle







  
```

##

```haskell
  let
    eShow1  = ffilter id . updated $ dToggle
    eShow2  = ffilter id . updated $ dNotToggle

  deText <- widgetHold textWidget . leftmost $ [


    ]


  
```

##

```haskell
  let
    eShow1  = ffilter id . updated $ dToggle
    eShow2  = ffilter id . updated $ dNotToggle

  deText <- widgetHold textWidget . leftmost $ [
      textWidget   <$ eShow1

    ]


  
```

##

```haskell
  let
    eShow1  = ffilter id . updated $ dToggle
    eShow2  = ffilter id . updated $ dNotToggle

  deText <- widgetHold textWidget . leftmost $ [
      textWidget   <$ eShow1
    , buttonWidget <$ eShow2
    ]


  
```

##

```haskell
  let
    eShow1  = ffilter id . updated $ dToggle
    eShow2  = ffilter id . updated $ dNotToggle

  deText <- widgetHold textWidget . leftmost $ [
      textWidget   <$ eShow1
    , buttonWidget <$ eShow2
    ]

  let
    eText  = switch (current deText)
```

##

<div class="demo" id="examples-switch-hold-button"></div>

##

<div class="demo" id="examples-switch-hold-tick"></div>

##

```haskell
newtype Workflow t m a = Workflow { 
    unWorkflow :: m (a, Event t (Workflow t m a))
  }
```

```haskell
workflow :: (DomBuilder t m, MonadFix m, MonadHold t m) 
         => Workflow t m a 
         -> m (Dynamic t a)
```

##

```haskell
  let
    eShow1  = ffilter id . updated $ dToggle
    eShow2  = ffilter id . updated $ dNotToggle














  
```

##

```haskell
  let
    eShow1  = ffilter id . updated $ dToggle
    eShow2  = ffilter id . updated $ dNotToggle

    wf1 ::            m (Event t Text)
    wf1 =            do
      eText <- textWidget
      pure (eText               )









  
```

##

```haskell
  let
    eShow1  = ffilter id . updated $ dToggle
    eShow2  = ffilter id . updated $ dNotToggle

    wf1 ::            m (Event t Text)
    wf1 =            do
      eText <- textWidget
      pure (eText               )

    wf2 ::            m (Event t Text)
    wf2 =            do
      eText <- buttonWidget
      pure (eText               )




  
```

##

```haskell
  let
    eShow1  = ffilter id . updated $ dToggle
    eShow2  = ffilter id . updated $ dNotToggle

    wf1 :: Workflow t m (Event t Text)
    wf1 =            do
      eText <- textWidget
      pure (eText               )

    wf2 ::            m (Event t Text)
    wf2 =            do
      eText <- buttonWidget
      pure (eText               )




  
```

##

```haskell
  let
    eShow1  = ffilter id . updated $ dToggle
    eShow2  = ffilter id . updated $ dNotToggle

    wf1 :: Workflow t m (Event t Text)
    wf1 = Workflow $ do
      eText <- textWidget
      pure (eText               )

    wf2 ::            m (Event t Text)
    wf2 =            do
      eText <- buttonWidget
      pure (eText               )




  
```

##

```haskell
  let
    eShow1  = ffilter id . updated $ dToggle
    eShow2  = ffilter id . updated $ dNotToggle

    wf1 :: Workflow t m (Event t Text)
    wf1 = Workflow $ do
      eText <- textWidget
      pure (eText               )

    wf2 :: Workflow t m (Event t Text)
    wf2 =            do
      eText <- buttonWidget
      pure (eText               )




  
```

##

```haskell
  let
    eShow1  = ffilter id . updated $ dToggle
    eShow2  = ffilter id . updated $ dNotToggle

    wf1 :: Workflow t m (Event t Text)
    wf1 = Workflow $ do
      eText <- textWidget
      pure (eText               )

    wf2 :: Workflow t m (Event t Text)
    wf2 = Workflow $ do
      eText <- buttonWidget
      pure (eText               )




  
```

##

```haskell
  let
    eShow1  = ffilter id . updated $ dToggle
    eShow2  = ffilter id . updated $ dNotToggle

    wf1 :: Workflow t m (Event t Text)
    wf1 = Workflow $ do
      eText <- textWidget
      pure (eText, wf2 <$ eShow2)

    wf2 :: Workflow t m (Event t Text)
    wf2 = Workflow $ do
      eText <- buttonWidget
      pure (eText               )




  
```

##

```haskell
  let
    eShow1  = ffilter id . updated $ dToggle
    eShow2  = ffilter id . updated $ dNotToggle

    wf1 :: Workflow t m (Event t Text)
    wf1 = Workflow $ do
      eText <- textWidget
      pure (eText, wf2 <$ eShow2)

    wf2 :: Workflow t m (Event t Text)
    wf2 = Workflow $ do
      eText <- buttonWidget
      pure (eText, wf1 <$ eShow1)




  
```

##

```haskell
  let
    eShow1  = ffilter id . updated $ dToggle
    eShow2  = ffilter id . updated $ dNotToggle

    wf1 :: Workflow t m (Event t Text)
    wf1 = Workflow $ do
      eText <- textWidget
      pure (eText, wf2 <$ eShow2)

    wf2 :: Workflow t m (Event t Text)
    wf2 = Workflow $ do
      eText <- buttonWidget
      pure (eText, wf1 <$ eShow1)

  deText <- workflow wf1


  
```

##

```haskell
  let
    eShow1  = ffilter id . updated $ dToggle
    eShow2  = ffilter id . updated $ dNotToggle

    wf1 :: Workflow t m (Event t Text)
    wf1 = Workflow $ do
      eText <- textWidget
      pure (eText, wf2 <$ eShow2)

    wf2 :: Workflow t m (Event t Text)
    wf2 = Workflow $ do
      eText <- buttonWidget
      pure (eText, wf1 <$ eShow1)

  deText <- workflow wf1

  let
    eText  = switch . current $ deText
```

##

<div class="demo" id="examples-switch-workflow-button"></div>

##

```haskell
  let
    wf1 :: Workflow t m (Event t Text)
    wf1 = Workflow $ do
      eText <- textWidget
      pure (eText, wf2 <$ eSwitch)

    wf2 :: Workflow t m (Event t Text)
    wf2 = Workflow $ do
      eText <- buttonWidget
      pure (eText, wf3 <$ eSwitch)

    wf3 :: Workflow t m (Event t Text)
    wf3 = Workflow $ do
      eText <- tickWidget
      pure (eText, wf1 <$ eSwitch)

  deText <- workflow wf1
```

##

<div class="demo" id="examples-switch-workflow-1"></div>

##

```haskell
    ...

    wf2 :: Workflow t m (Event t Text)
    wf2 = Workflow $ do
      eText <- buttonWidget

      eBack <- el "div" $ button "Back"
      eNext <- el "div" $ button "Next"

      let 
        eSwitch = leftmost [wf1 <$ eBack, wf2 <$ eNext]
        eOut    = leftmost [eText, "" <$ eWorkflow]

      pure (eOut, eSwitch)

    ...
```

##

<div class="demo" id="examples-switch-workflow-2"></div>


