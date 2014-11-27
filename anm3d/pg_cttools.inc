C Include file for PGPLOT lookup tables
C -------------------------------------
C
       real*4     ct_vals(18)
       real*4     power_law, lower_bound, upper_bound
       real*4     col_power_law(3), col_bounds(2,3), col_starts(3)
       real*4     col_index(3)
       equivalence( ct_vals(1), power_law)
       equivalence( ct_vals(2), lower_bound)
       equivalence( ct_vals(3), upper_bound)
       equivalence( ct_vals(4), col_power_law)
       equivalence( ct_vals(7), col_bounds)
       equivalence( ct_vals(13), col_starts)
       equivalence( ct_vals(16), col_index)
C
       common /pgplot_ctmod/ ct_vals
