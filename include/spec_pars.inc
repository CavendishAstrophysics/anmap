C
C Parameter definitions for the SPECTRUM package
C
C source for command definitions:
C   tools
       character    tool_definitions*15
       parameter   (tool_definitions = '/spec_tools.def')
C   analysis routines
       character    anal_definitions*13
       parameter   (anal_definitions = '/analysis.def')
C   full file names for definition files
       character    define_tools*35
       parameter   (define_tools =
     *              '/mrao/anmap_v7.5/etc/spec_tools.def')
C   full file names for definition files
       character    define_analysis*33
       parameter   (define_analysis =
     *              '/mrao/anmap_v7.5/etc/analysis.def')

