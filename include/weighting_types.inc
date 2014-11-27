C
C     Include file giving valid values of the map weighting function.
C
C     Note :  Weighting function types 10-14 are the same as 0-4 except
C             they apply a visibility cutoff.

C         Number of weighting function types
              integer             num_weight
              parameter         ( num_weight = 5 )

C         Valid weighting function types
              character*20        weight_types(0:num_weight)
              integer             no_weighting, super_smooth,
     *                            radial_wt, gaussian_wt,
     *                            radial_gauss_wt, noise_wt

              parameter         ( no_weighting   = 0 )
              parameter         ( super_smooth   = 1 )
              parameter         ( radial_wt      = 2 )
              parameter         ( gaussian_wt    = 3 )
              parameter         ( radial_gauss_wt= 4 )
              parameter         ( noise_wt       = 5 )

              data    weight_types/   'none',
     *                                'super-smooth',
     *                                'radial-only',
     *                                'gaussian-only',
     *                                'radial-gaussian',
     *                                'noise-weighting'    /
