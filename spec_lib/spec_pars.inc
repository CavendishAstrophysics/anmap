C
C Parameter definitions for the SPECTRUM package
C
C source for command definitions:
C   tools
       character    tool_definitions*22
       parameter   (tool_definitions = '/definitions/tools.def')
C   analysis routines
       character    anal_definitions*25
       parameter   (anal_definitions = '/definitions/analysis.def')
C   full file names for definition files
       character    define_tools*32
       parameter   (define_tools =
     *              '/mrao/spec/definitions/tools.def')
C   full file names for definition files
       character    define_analysis*35
       parameter   (define_analysis =
     *              '/mrao/spec/definitions/analysis.def')
