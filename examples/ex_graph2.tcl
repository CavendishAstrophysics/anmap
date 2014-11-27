#
# Example Graph 2:  multiple lines for simple graphs
# display a graph

# start by initilialising the whole graphic.  We select graphic 1
# using the graphic sub-system and set view ports on the display to
# use --- this is only necessary to ensure we are not in some strange
# state, if you have just started Anmap this is not necessary
graphic select graph 1
graphic view-port 0,1,0,1

# initailise the graph itself in the data-display sub-system
data-display
initialise all ; initialise plot

# now specify the file to plot and do the plotting (we first change
# to the directory in which the data is stored, again this is not
# necessary in the simplest case
global Anmap ; cd $Anmap(src)/examples

# ... now the real setup, two lines different line styles
  line 1 file nor20.o.final ; line 1 line-style 1 1 white
  line 2 file nor20.w.final ; line 2 line-style 2 1 red


# now add a key: just turn the key options on:  the default will be
# to use the file names and column numbers in the key.
  set-key-options on
  set-key-options text-style 1 1.54 white 1 1

# add titles
 # set text style to a roman font of twice the default size
 # rendered as white with line width 1
  set-text-style 2 2 white 1
 # now alter the line style for pips etc on the graph
  set-frame text-style 2 1.5 white 1
  x-title Position 
  y-title Intensity (Arbitrary Units)
  title Example Graph 1

# and we can plot it --- we specify the y-range for the plot
  y-range -0.05,0.4
  plot all


