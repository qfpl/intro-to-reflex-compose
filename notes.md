
# Talk tasks

## Switching

- edit shows off workflow, and introduces breaking up an element to use custom events

## Collections

- bolt things together to introduce todoItem

- introduce list and piles of switching
  - use it for additions
  - add removals
  - add updates
  - add clear complete
  - add mark all complete
  
- introduce eventwriter
  - walk through the class and the helpers
  - walk through the refactorings of the components
    - text now triggers a remove from within
  - walk through the changes to todolist

- start encapsulating things
  - don't need to track state changes if components can handle removals themselves
  - text is easy to change, just stop updating the state
  - complete is the interesting one
    - we need the completion state out, so return it as a dynamic
    - show joinDynThroughMap at top level
    - thread the events through to complete
    - make use of them from there

# Code and talk tasks

- show that we can 
  - load the initial state over a websocket
  - send the changes over a websocket
  - apply the changes once they have been confirmed via websocket, or display an error
  - recieve changes made my other connected clients

- show off the backend as well ?
   

