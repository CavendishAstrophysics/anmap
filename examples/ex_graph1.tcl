#
# Example Graph 1:  basic use of the data display sub-system to
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

# ... now the real setup
  line 1 file nor20.o.final

# add titles
 # set text style to a roman font of twice the default size
 # rendered as white with line width 1
  set-text-style 2 2 white 1
 # now alter the line style for pips etc on the graph
  set-frame text-style 2 1.5 white 1
  x-title Position 
  y-title Intensity (Arbitrary Units)
  title Example Graph 1

# and we can plot it --- scaling is taken care or
  plot all
