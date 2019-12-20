# Chisel GUI - WIP

An experimental GUI to help debug Chisel.
Currently this app allows you to essentially run the TreadleRepl on a specified Firrtl file.

![chisel-gui-image](https://raw.githubusercontent.com/ucb-bar/chisel-gui/master/doc/images/chisel-gui-image.png?sanitize=true)

## How to run.
From the sbt prompt do
```
run samples/gcd.fir
```
if everything is right with the world this should launch the gui window. `samples/gcd.fir` is a Firrtl file.
In the GUI window there are several things that need to be done.

 - First select all the signal names in the left most panel (using <select-all> key command.
 - Click the `Add` button at the bottom of that panel to add them to the next signals to display panel
 - Then add two values to the right most panel for `io_a` and `io_b`
 - Set `io_e` to 1
 - Hit `Poke`
 - Hit `Step`
 - You should see some wave forms at this point
 - Now set `io_e` to 0
 - Now hit `Poke`
 - Now hit `Step` a few times
 
 The wave forms are a little messy but at this point you should at least have the idea.
 
## Next
It would be nice to change the datamode of the wave form viewer to a VCD type.
That way the viewer could display a vcd file directly or could change it's interface
with Treadle to access Treadle' data via a VCD object.
 
