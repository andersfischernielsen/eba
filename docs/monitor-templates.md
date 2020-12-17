# Monitor templates

Monitor templates are used for keeping track of state changes within the printout of the `eba` CFG.

## Overview 

All monitor templates must implement the following signature: 

```ocaml
module type PrinterSpec = sig
    (** The internal state of the printing monitor **)
    type state
	(** The transition function for the monitor. State changes happen here based on the previous state and the effects found in the input *)
	val transition: state -> e list -> state
	(** An indication of whether the printing monitor should be shown in the printout **)
    val is_in_interesting_section: state -> bool
	(** The initial state of the monitor **)
    val initial_state: state
	(* An indication of whether the monitor accepts an effect found in a CFG step *)
    val is_in_transition_labels: e -> bool
	(* An indication of whether the monitor is in its final state *)
    val is_in_final_state: state -> bool
	(* A string representation of the state of the monitor, displayed in the printout *)
	val string_of_state: state -> string
end
```

For an example of a full implementation of this signature, see [checkAutomataDoubleUnlock.ml](../checkAutomataDoubleUnlock.ml)

The definition of when and how state changes should happen is implemented in the `transition` function. This can efficiently be implemented as a pattern match on what the previous state was, and what the incoming input is. 

# The Printer [CFGPrinter](../cfgPrinter.ml)

Monitor template definitions are passed to the [CFGPrinter.ml](../cfgPrinter.ml). This printer takes an implementation of a monitor template, and the CFG `step`. This step is passed to the CFGPrinter by the outer `eba` analysis logic. 

The CFGPrinter will explore the `eba` CFG and apply any effects found within a `step` in the CFG, provided that these effects are in the `is_in_transition_labels` of the monitor template implementation. The resulting state of applying the transition function of the monitor with the effects found in the `step` is preserved in the CFGPrinter as a mapping from the region a monitor template operates on to the current state of that monitor. 

Whenever effects involving a previously untracked region are encountered, a new instance of the monitor template is instantiated in its initial state, and the transition function is then applied with these effects. 

After the application of the transition function of the monitor template and the new state has been found, the CFGPrinter will filter the monitor template results to remove monitors in their final states. 

Finally, if a monitor reports that `it is_in_interesting_section`, the state of the monitor will be added to the printout.

# Adding a new printing monitor template implementation to `eba` 

Creating a new printing monitor template implementation _should_ be the only thing required in order to add more information to the CFG printout, but modifications or additions might be needed in [CFGPrinter.ml](../cfgPrinter.ml). This depends on the use case and complexity of the printout.
