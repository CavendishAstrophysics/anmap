C Include file for PGPLOT lookup tables
C -------------------------------------
C
       real*4     power_law, lower_bound, upper_bound
       real*4     col_power_law(3), col_bounds(2,3), col_starts(3)
       real*4     col_index(3)
C
       common /pgplot_tv_modify/
     *         power_law, lower_bound, upper_bound,
     *         col_power_law, col_bounds, col_starts,
     *         col_index
C
