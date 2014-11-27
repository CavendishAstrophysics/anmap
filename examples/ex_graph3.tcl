#
#
# Graph Example 3
# ---------------
#
# Example script showing use of multiple graphs in data-display
# This example all shows how to use the command language to automate
# many tasks using loops and prompts to the user.  To run this example
# you need only "source" this file, or load it into the scratch pad or
# Anmap text editor and select "Execute All"

# ------------ INITIALISATION ----------
# prompt the user for postscript output or not
 set postscript 0
# puts \r ; set postscript [iocmd geti 'Postscript output (1/0) : ' '0']
 global Anmap

# ensure we are in the correct working directory
set dir [pwd]
cd $Anmap(src)/examples

# select output device for postscript output
if $postscript then {
  graphic 
     device-select 3
     output-device fig3.ps/ps
     open-device
  exit
}

# ------------ SETUP FOR ALL PLOTS ----------
# start work in the data-display sub-system
data-display

 # set line width 
  set lw 2

 # setup for three graphs -- use the graphic sub-system to
 # position the graphs on the page.
 graphic
   clear
   select graph 1 ; view-port 0.31,0.69,0.775,0.975
   select graph 2 ; view-port 0.1,0.48,0.1,0.65
   select graph 3 ; view-port 0.52,0.9,0.1,0.65
  data-display

 # basic initialisation: many basic parameters for the plots are
 # common to each therefore we use a loop to initialise each graph
 # in exactly the same way
  foreach n { 1 2 3 } {
   # select the correct graph
    graphic select graph $n
   # start by re-initialising everything
    init all ;  init plot ; view-port 0 1 0 1
   # setup options for line-styles and text
    set-frame line-style 1 $lw white
    set-frame text-style 2 1.5 white $lw
    set-text 2 1.5 white $lw
    set-title x-title 1 2.4 0.5 0.5
   # setup frame so that only an x-axis is displayed, somewhat
   # unusual, but common in NMR from which this example is chosen
    set-frame y-side 0 ; set-frame y-lab 0
    set-frame x-side 1
   # we also set the X-axis title for all the plots
    x-title Position
  }

# ------------ PLOT LOWER LEFT GRAPH----------

 # do all lines/spectra
 # start with graph 2, define the files to plot and display them
 # note how scaling and auto-scaling are used for comparing the spectral
 # files
  graphic select graph 2
  line 1 file nor20.o.final  ; line 1 y-scale 1.75 
  line 1 line-style 1 $lw white
  line 1 y-auto on
  line 1 x-offset -1
  line 2 file nor20.18  ; line 2 line-style 2 $lw white 
  line 2 y-auto on

 # The spectrum looks like a new plot in fact it is plotted on
 # the same axes, but an offset is applied to make it appear as a
 # separate figure
  line 3 file nor20.11  ; line 3 line-style 1 $lw white 
  line 3 y-auto on
  line 3 y-offset 0.6
  line 3 x-scale 1.0

 # y-range is important in this example as it is also used to effectively
 # control the layout of the plot
  y-range -0.05 1.2
 # plot everything for graph 2
  plot all


# ------------ PLOT LOWER RIGHT GRAPH----------

 # do the same thing for the bottom right plot --- not so much comment
 # this time ...
  graphic select graph 3
  line 1 file nor20.w.final  ; line 1 y-scale 1.7
  line 1 line-style 1 $lw white
  line 1 y-auto on
  line 1 x-offset 4
  line 2 file nor20.15  ; line 2 line-style 2 $lw white 
  line 2 y-auto on
  line 3 file nor20.12  ; line 3 line-style 1 $lw white 
  line 3 y-auto on
  line 3 y-offset 0.6
  y-range -0.05 1.2
  line 3 x-scale 1.0
  plot all


# ------------ PLOT UPPER GRAPH----------

  graphic select graph 1
  line 1 file nor20.9  ; line 1 y-scale 1.0E-6 
  line 1 line-style 1 $lw white 
  plot all

# ------------ ANNOTATE THE WHOLE PLOT ----------

# here we use the annotation sub-system to annotate the whole plot
# for convenience we just use the corrdinate system of the last
# graph drawn
  annotate
    set-plot temp
    set-text 2 1.5 white 2
    text-draw -80 3.5 0 Spectrum
    text-draw -150 -3.5 0 Oil
    text-draw 20 -3.5 0 Water
  exit

# ------------ TIDY UP IF THE OUTPUT WAS TO A POSTSCRIPT FILE  ----------
if $postscript then {
  graphic 
     close-device
     device-select 1
  exit
}

cd $dir

