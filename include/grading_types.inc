C
C     Include file giving valid values of the map grading function.
C

C         Number of grading function types
              integer             num_grad
              parameter         ( num_grad = 8 )

C         Valid grading function types
              character*(20)      grad_types(0:num_grad)
              integer             no_grading, radial_gr, gaussian_gr,
     *                            radial_gauss_gr, optimal_gr,
     *                            psw2_gr, l2w2_gr, psw3_gr, l2w3_gr

              parameter         ( no_grading     = 0 )
              parameter         ( radial_gr      = 1 )
              parameter         ( gaussian_gr    = 2 )
              parameter         ( radial_gauss_gr= 3 )
              parameter         ( psw2_gr        = 4 )
              parameter         ( psw3_gr        = 5 )
              parameter         ( l2w2_gr        = 6 )
              parameter         ( l2w3_gr        = 7 )
              parameter         ( optimal_gr     = 8 )

              data    grad_types/ 'none',
     *                            'radial-only',
     *                            'gaussian-only',
     *                            'radial-gaussian',
     *                            'prolate-spheroidal-2',
     *                            'prolate-spheroidal-3',
     *                            'l2-optimal-2',
     *                            'l2-optimal-3',
     *                            'optimal-grading' /

