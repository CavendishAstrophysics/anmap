
Spectrum-analysis-program: Integrate the supplied spectrum

Keys:

     infile       nokeyok             input spectrum file [-]
     xc                               x-column [1]
     yc                               y-column [2]
     xmin                             } range over which to integrate
     xmax                             } [full range of x]


A simple integration of the data within the specified range is performed.
The y data are integrated with respect to x using a trapezoidal rule and
the result of the integration is printed on the standard output.

Example:

Integrate column-3 with respect to column 1 in the range 0,100

  sp_integrate my_data.dat xmin 0.0 xmax 100.0 xc 1 yc 3


