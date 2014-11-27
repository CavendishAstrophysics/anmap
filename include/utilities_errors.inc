C
C Error definitions for utiltiy routines
C
C No error
       integer     ill_utl_ok
       parameter  (ill_utl_ok = 0)
C
C Illegal inputs to routine: out of bounds etc.
       integer     ill_inputs
       parameter  (ill_inputs = -75001)
C
C Problem reaching required precision for function
       integer     ill_precision
       parameter  (ill_precision = -75002)
