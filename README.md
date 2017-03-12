# barstools
Useful utilities for BAR projects

## Quick Start

Example use of how to run the GenerateTop transform:

```
sbt
> compile
> project tapeout
> run-main barstools.tapeout.transforms.GenerateTop -i <myfile>.fir -o <myfile>.v --syn-top <mysyntop> --harness-top <myharnesstop>
```

Or as an (almost) one-liner:

```
export FIRRTL_HOME=/path/to/firrtl # path to firrtl (master)
sbt -DFIRRTL_HOME=$FIRRTL_HOME "project tapeout" "run-main barstools.tapeout.transforms.GenerateTop -i MyModule.fir -o MyModule.v --syn-top MyModule --harness-top MyModule"
```

## TODO

Passes/Transforms that could be useful if added here:

* Check that a module was de-duplicated. Useful for MIM CAD flows and currently done in python.

Be sure to publish-local the following repositories:
* ucb-bar/chisel-testers (requires ucb-bar/firrtl-interpreter)
* ucb-bar/firrtl
