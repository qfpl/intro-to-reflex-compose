
# Talk tasks

## Collections

- start encapsulating things
  - don't need to track state changes if components can handle removals themselves
  - text is easy to change, just stop updating the state
  - complete is the interesting one
    - we need the completion state out, so return it as a dynamic
    - show joinDynThroughMap at top level
    - thread the events through to complete
    - make use of them from there
   

