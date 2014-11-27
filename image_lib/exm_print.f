       SUBROUTINE EXM_PRINT (OPTION, STATUS)
C      -------------------------------------
C
C  Prints part or all of map redtape on the output device.
C  OPTION may be 'TITLE', 'ASTRO', 'COMP', 'MAPPING' or 'ALL'.
C
C  The STATUS value should be zero on entry.
C
*-
       CHARACTER  OPTION*(*)
       INTEGER    STATUS
C
       CHARACTER  CHRA*16, CHDEC*16, STR*20
       REAL*4     RMIN, RMAX
       INTEGER*4  IZERO, IZMIN, IZMAX
       INTEGER    I, J, IUNIT, PRSEC
       INTEGER    L, LD, LN, LP, LR, LT, LU
C
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/chrlib_functions.inc'
C
       REAL*8  PI, RCONVT, DCONVT
       PARAMETER (PI=3.1415926535898D0)
       PARAMETER (RCONVT=PI/12.D0)
       PARAMETER (DCONVT=PI/180.D0)
C
       INTEGER*4  LSFDSC(5), LSFKEY
       CHARACTER  SFNAME*16
       EQUIVALENCE  ( LSFDSC(1), SFNAME )
       EQUIVALENCE  ( LSFDSC(5), LSFKEY )
C
       CHARACTER TYPE(4)*14
       DATA TYPE
     :/'16-bit integer','32-bit integer','32-bit real','64-bit complex'/
C
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_tabfns.inc'
       include '../include/weighting_types.inc'
       include '../include/grading_types.inc'
C
       IF (STATUS.NE.0) RETURN
C
       CALL io_enqout(IUNIT)
C
C  Check map redtape
C
       IF (RTHDR.NE.'MAP' .AND. RTHDR.NE.'APERTURE') THEN
         WRITE(IUNIT,*)'*** invalid map redtape'
         RETURN
       ENDIF
C
C  Print owner
C
       IF (OPTION.EQ.'ALL') THEN
         WRITE(IUNIT,'(/X,2A,5X,2A)')
     :     'Owner : ',RTOWNR,'Last modified by : ',RTUSER
       ENDIF
C
C  Print map title
C
       IF (OPTION.EQ.'TITLE' .OR. OPTION.EQ.'ALL') THEN
         WRITE(IUNIT,*)
         DO I=1,20
           L=chr_lenb(RTITLE(I))
           IF (L.GT.0) WRITE(IUNIT,'(3X,A)')RTITLE(I)(1:L)
         ENDDO
         IF (io_attn(STATUS)) RETURN
       ENDIF
C
C  Print computing redtape
C
       IF (OPTION.EQ.'COMP' .OR. OPTION.EQ.'ALL') THEN
         WRITE(IUNIT,1)
         LU=chr_lenb(EXUNIT)
         LT=chr_lenb(TYPE(IABS(ISWD)))
         RMIN=(ZMIN-ZEROL)*SCALEF
         RMAX=(ZMAX-ZEROL)*SCALEF
         WRITE(IUNIT,2)IXMAX,IYMAX,IUMAP1,IUMAP2,IVMAP1,IVMAP2,
     :     IWMAX,MPBLK,NPMAP,SCALEF
         IF (IABS(ISWD).EQ.1 .OR. IABS(ISWD).EQ.2) THEN
           IZMIN=NINT(ZMIN)
           IZMAX=NINT(ZMAX)
           IZERO=NINT(ZEROL)
           WRITE(IUNIT,3)IZERO
           WRITE(IUNIT,5)'Data minimum',IZMIN,RMIN,IUZMIN,IVZMIN
           WRITE(IUNIT,5)'Data maximum',IZMAX,RMAX,IUZMAX,IVZMAX
         ELSE
           WRITE(IUNIT,4)ZEROL
           WRITE(IUNIT,6)'Data minimum',ZMIN,RMIN,IUZMIN,IVZMIN
           WRITE(IUNIT,6)'Data maximum',ZMAX,RMAX,IUZMAX,IVZMAX
         ENDIF
         WRITE(IUNIT,7)TYPE(IABS(ISWD))(1:LT),EXUNIT(1:LU)
         IF (ISWD.LT.0) THEN
           WRITE(IUNIT,17)IPMAP
         ENDIF
    1    FORMAT(/' Computing redtape'/X,17('-'))
    2    FORMAT(' Map dimensions : ',2X,2I6/
     :          ' U-range',7X,' : ',2X,2I6/
     :          ' V-range',7X,' : ',2X,2I6/
     :          ' Multiplicity',2X,' : ',8X,I6/
     :          ' Length of row  : ',8X,I6,' bytes'/
     :          ' Length of map  : ',8X,I6,' pages'/
     :          ' Scale factor',2X,' : ',1PE14.4)
    3    FORMAT(' Zero level',4X,' : ',I14)
    4    FORMAT(' Zero level',4X,' : ',1PE14.4)
    5    FORMAT(X,A,2X,' : ',I14,1PE15.4,' ext.units',
     :                                      ',  at u,v',2I5)
    6    FORMAT(X,A,2X,' : ',1PE14.4,E15.4,' ext.units',
     :                                      ',  at u,v',2I5)
    7    FORMAT(' Internal data type is',X,A,
     :           ',  external units are',X,A)
   17    FORMAT(' Indexed map, number of non-blank data values',I6)
         IF (io_attn(STATUS)) RETURN
       ENDIF
C
C  Print astronomical redtape
C
       IF (OPTION.EQ.'ASTRO' .OR. OPTION.EQ.'ALL') THEN
         LP=chr_lenb(POLN)
         LN=chr_lenb(NAME)
         WRITE(IUNIT,8)
         WRITE(IUNIT,9)NAME(1:LN),IPOLN,POLN(1:LP),FREQ,NSPAC
    8    FORMAT(/' Astronomical redtape'/X,20('-'))
    9    FORMAT(' Map of ',A/
     :          ' Polarisation   : ',8X,I6,X,A/
     :          ' Frequency      : ',F14.4,' MHz'/
     :          ' No.of spacings : ',8X,I6)
C
         PRSEC=2
         CALL chr_chdtos(RAMAP/RCONVT,PRSEC,CHRA,LR)
         CALL chr_chdtos(DECMAP/DCONVT,PRSEC,CHDEC,LD)
         WRITE(IUNIT,'(/X,A,F6.1,A,2(X,A))')
     :    'RA, Dec    (',REFDAT,')   ',CHRA(1:LR),CHDEC(1:LD)
C
         CALL chr_chdtos(RAOBS/RCONVT,PRSEC,CHRA,LR)
         CALL chr_chdtos(DECOBS/DCONVT,PRSEC,CHDEC,LD)
         WRITE(IUNIT,'(12X,A,F6.1,A,2(X,A))')
     :               '(',OBSDAT,')   ',CHRA(1:LR),CHDEC(1:LD)
C
         IF (RAPNT.LT.0.D0) RAPNT=RAPNT+2.D0*PI
         CALL chr_chdtos(RAPNT/RCONVT,PRSEC,CHRA,LR)
         CALL chr_chdtos(DECPNT/DCONVT,PRSEC,CHDEC,LD)
         WRITE(IUNIT,'(X,A,5X,A,X,A)')
     :            'Pointing direction',CHRA(1:LR),CHDEC(1:LD)
C
         WRITE(IUNIT,'(/X,A,4X,2F10.2)')
     :     'Map centre (gridpoint)',XMC,YMC
         WRITE(IUNIT,'(X,A,F9.4,F10.4)')
     :     'Sampling (arcsec/gridpoint)',USAMP,VSAMP
         WRITE(IUNIT,'(X,A,7X,F10.4,F10.4)')
     :     'Resolution (arcsec)',HPFBWU,HPFBWV
         WRITE(IUNIT,'(X,A,10X,1PE14.4)')
     :     'Flux conversion factor',FLXNRM
C
         IF (IPROJ.EQ.1 .OR. IPROJ.EQ.2) THEN
           WRITE(IUNIT,'(X,A,F10.4)')
     :       'Skew angle, hour angle of map centre',SKEW
         ELSEIF (IPROJ.EQ.3) THEN
           WRITE(IUNIT,'(X,A,F10.4)')
     :       'Skew angle, position angle of v-axis',SKEW
         ENDIF
C
         IF (IPROJ.EQ.1) THEN
           WRITE(IUNIT,'(/X,A)')
     :      'Equatorial projection (aerial coordinates)'
         ELSEIF (IPROJ.EQ.2) THEN
           WRITE(IUNIT,'(/X,A)')
     :      'Equatorial projection (sky coordinates)'
         ELSEIF (IPROJ.EQ.3) THEN
           WRITE(IUNIT,'(X,A)')'Tangent plane projection'
         ENDIF
C
         IF (IPROJ.GT.0) THEN
           WRITE(IUNIT,'(X,A,F8.2)')'Epoch of projection ',EPOCH
         ENDIF
         IF (io_attn(STATUS)) RETURN
       ENDIF
C
C  Print mapping redtape
C
       IF (OPTION.EQ.'MAPPING' .OR. OPTION.EQ.'ALL') THEN
         IF ((MAPTYP.GE.1 .AND. MAPTYP.LE.4)          .AND.
     *       (CONVTP.GE.1 .AND. CONVTP.LE.NUM_CONV)   .AND.
     *       ((GRADTP.GE.0  .AND.GRADTP.LE. NUM_GRAD).OR.
     *        (GRADTP.GE.100.AND.GRADTP.LE.(NUM_GRAD+100)))  .AND.
     *       ((WGHTTP.GE.0 .AND.WGHTTP.LE. NUM_WEIGHT).OR.
     *        (WGHTTP.GE.10.AND.WGHTTP.LE.(NUM_WEIGHT+10)))  .AND.
     *       (NUMLSF.GE.1 .AND. NUMLSF.LE.MAXLSF)            ) THEN
C          Data is valid mapping data.
           WRITE(IUNIT,10)
   10      FORMAT(/' Map making redtape'/X,18('-'))

C          Map type.
           IF (MAPTYP .EQ. 1) THEN
             WRITE(IUNIT,'(X,A)') 'Map type is a map.'
           ELSE IF (MAPTYP .EQ. 2) THEN
             WRITE(IUNIT,'(X,A)') 'Map type is a beam.'
           ELSE IF (MAPTYP .EQ. 3) THEN
             WRITE(IUNIT,'(X,A)') 'Map type is a beam-set.'
*             IF (IBIT(BSETFW,0).NE.0) THEN
*               WRITE(IUNIT,'(X,A,I2,'' beams separated by '',I3,
*     :                         '' gridpoints starting at '', A, I4)')
*     :             'Beam grid u: ', nuset, duset, 'u= ', u0set,
*     :             '          v: ', nvset, dvset, 'v= ', v0set
*             ELSE
*               WRITE(IUNIT,'(X,A)') 'Position of beams is irregular.'
*             ENDIF
*             IF (IBIT(BSETFW,1).NE.0) WRITE(IUNIT,'(X,A,e10.3)')
*     :            'Bandwidth smearing applied for (hz):',
*     :            effective_bandwidth
*             IF (IBIT(BSETFW,2).NE.0) WRITE(IUNIT,'(X,A)')
*     :            'Primary beam correction applied.'
*             IF (IBIT(BSETFW,3).NE.0) WRITE(IUNIT,'(X,A,f8.1)')
*     :        'Integration-time smearing correction applied for (secs)',
*     :        effective_integration
           END IF

C          Convolution function.
           WRITE(IUNIT,'(/,X,2A)')   'Convolution function is ',
     *                               FN_TYPES(CONVTP)
           WRITE(IUNIT,'(X,A,I1,A)') 'Function halfwidth is ',
     *                               CONVHW, ' gridpoints.'
           WRITE(IUNIT,'(X,A,I3,A)') 'Tabulation oversampling is ',
     *                               CONVOS, ' points per gridpoint.'
           IF (CONVTP .EQ. PROL_SPHER) THEN
             WRITE(IUNIT,'(X,A,I1)') 'Spheroidal function alpha is ',
     *                                NINT(CONVPA(1))
           ELSE IF (CONVTP .EQ. GAUSS_SINC) THEN
             WRITE(IUNIT,'(X,A,F4.2)') 'First zero of sinc is at U = ',
     *                                 CONVPA(1)
             WRITE(IUNIT,'(X,A,F4.2,A)')
     *                'Standard deviation of gaussian is ',CONVPA(2),
     *                ' gridpoints.'
           ELSE IF (CONVTP .EQ. L2_OPTIMAL) THEN
             WRITE(IUNIT,'(X,A,I2,A)') 'Optimised map width is ',
     *                                 NINT(CONVPA(1)*200.0), '%'
           END IF

C          Correction function.
           WRITE(IUNIT,'(/,X,2A)') 'Correction function is ',
     *                              CORR_TYPES(CORRTP)

C          Weighting function.
           WRITE(IUNIT,'(/,X,2A)') 'Visibility weighting function is ',
     *                             WEIGHT_TYPES(MOD(WGHTTP,10))
           IF ((MOD(WGHTTP,10) .EQ. SUPER_SMOOTH) .OR.
     *         (MOD(WGHTTP,10) .EQ. NOISE_WT)          ) THEN
             WRITE(IUNIT,'(X,A,I1,A)')
     *       'Smooth box width is ',NINT(WGHTPA(1)*2+1), ' grid points.'
           ELSE IF ( MOD(WGHTTP,10).EQ.GAUSSIAN_WT .OR.
     *               MOD(WGHTTP,10).EQ.RADIAL_GAUSS_WT  ) THEN
             WRITE(IUNIT,'(X,A,F7.2,A)')
     *           'Standard deviation of gaussian is ', WGHTPA(1)*100.0,
     *           '% of aperture plane half-width.'
           END IF
           IF (WGHTTP.GE.10) THEN
              WRITE(IUNIT,'(X,A,F7.2,A)')
     *           'Visibility cutoff at ', WGHTPA(2)*100.0,
     *           '% of aperture plane half-width.'
           END IF

C          Grading function.
           if(gradtp.ge.100) then
              write(iunit,'(/,X,A)') 'Round beam'
              gradtp = gradtp - 100
           endif

           WRITE(IUNIT,'(/,X,2A)') 'Aperture plane grading is ',
     *                             GRAD_TYPES(GRADTP)
           IF (GRADTP.EQ.GAUSSIAN_GR.OR.GRADTP.EQ.RADIAL_GAUSS_GR) THEN
             WRITE(IUNIT,'(X,A,F7.2,A)')
     *          'Standard deviation of gaussian is ', GRADPA(1)*100.0,
     *          '% of aperture plane width.'
           ELSE IF (GRADTP.EQ.PSW2_GR.OR.GRADTP.EQ.PSW3_GR) THEN
             WRITE(IUNIT,'(X,A,I1,A,F7.2,A)')
     *          'Alpha of spheroidal function is ', INT(GRADPA(2)),
     *          ' with ',gradpa(1)*100,'% of aperture filled.'
           ELSE IF (GRADTP.EQ.L2W2_GR.OR.GRADTP.EQ.L2W3_GR) THEN
             WRITE(IUNIT,'(X,A,F6.2,A,F7.2,A)')
     *          'Relative source expansion is ', (1.0/gradpa(2)),
     *          ', assuming ', gradpa(1)*100,
     *          ', assuming ', gradpa(1)*100, '% of aperture filled.'
           ELSE IF (GRADTP.EQ.OPTIMAL_GR) THEN
             WRITE(IUNIT,'(X,A,F6.2,A,F7.2,A)')
     *          'Relative source expansion is ', gradpa(2),
     *          ', assuming ', gradpa(1)*100, '% of aperture filled.'
           END IF

           WRITE(IUNIT,'(/,X,A)') 'Sample files :'
           DO I = 1, NUMLSF
             DO J = 1, 6
               LSFDSC(J) = LSFARR(J,I)
             END DO
             IF (ABS(LSFKEY).LE.1) THEN
               WRITE(IUNIT,'(X,3A)') SFNAME,
     *           '- using default logical sample file.'
             ELSE
               CALL util_extdat( LSFKEY, 1, STR, L )
               WRITE(IUNIT,'(X,3A)') SFNAME,
     *           '- using LSF created on ', STR(1:L)
             END IF
           END DO
         ELSE
C          No valid mapping data.
           WRITE(IUNIT,'(/,A)') ' No valid map making redtape.'
         END IF
       END IF
C
       WRITE(IUNIT,*)
       IF (OPTION.EQ.'ALL' .AND. IUNIT.NE.1) WRITE(IUNIT,'(1H1)')
C
C
       END
