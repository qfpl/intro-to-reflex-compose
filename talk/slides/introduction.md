
# Introduction

## What is the virtual DOM?

<!--
Several frameworks have popped up which use the virtual DOM for creating web applications

##

Working on the DOM directly is slow, so the virtual DOM uses an alternate data structure and has efficient diff and patch methods to translate changes in the virtual DOM to changes in the actual DOM

##

There is something nice and functional about being able to work with something that feels like it has the type
-->

##

```haskell
DOM -> DOM
```

<!--
##

The assumption made by a lot of FP approaches to the virtual DOM is that the DOM is stateless 

##

Lots of things in the DOM have state

##

The classic example is a text input

##

If you're treating the DOM as stateless, you're going to be continually eating pain when dealing with stateful things

##

It will most likely get worse when you deal with collections

##

Reactjs has the stateless "elements" and locally-stateful "components"

##

This can help avoid some pain

##

Either way, you still have to deal with a mutating tree which potentially has mutating state at every node and every leaf

##

(With reactjs and components the types are different / a bit more abstract)

##

Historically, that has led to bad times
-->

## What is FRP?

<!--
FRP is a way of creating and using first-class values which manage changes to values over time, in a functional programming setting.

##

Proper FRP has

- precise semantics
- support for continuous time
-->

##

- Precise semantics

##

- Supports continuous time

## What is Reflex?

<!--
Reflex is a Functional Reactive Programming (FRP) library wrttien for Haskell which was developed to do front-end development.

##

There is a big focus on performance in `reflex`.

##

`reflex` has precise semantics but does not support working with continuous time

##

The focus is instead on the observable moments of time

##

These come from mouse clicks and key presses and clock ticks

##

We have a library that does 

> - high performance management of state
> - where the pieces of state are first class values
> - and supports all kinds of higher-order usage and abstaction
-->

##

- Precise semantics

##

- Discrete time

##

- High performance

##

- Higher order usage
