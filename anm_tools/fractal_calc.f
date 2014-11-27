C Fractal: Generate Area-Perimeter relations for an image
*-
       include '/mrao/include/iolib_functions.inc'
       include '../include/anmap_sys_pars.inc'

C define work array
       integer       nm, nb
       integer       msize, bsize
       parameter    (msize = 256*256)
       parameter    (bsize=1024)
       parameter    (nm=16)
       parameter    (nb=3)
       real*4        map_array( nm*msize + nb*bsize )
C error status
       integer       status
C pointers and map identifiers
       integer       imap, ip_map
C counter
       integer       n
C output file unit number
       integer       iunit
C mini redtape for map size etc.
       integer       minirt(8), imax, jmax

C define work arrays used in the fractal determination
       integer       cmax, tot_cmax
       parameter    (cmax = 5000, tot_cmax = 50000)
       integer       ids(1024,1024), class(cmax), clump(cmax)
       integer       area(cmax), bound(cmax)
       integer       tot_area(tot_cmax), tot_bound(tot_cmax)
       real*4        tot_gate(tot_cmax)
       integer       number, new, ntot, ngood
       real*4        gate, higate, lowgate, bad, dgate
       logical       valid(cmax), diagonal

C arrays to hold image properties
       real*4        zmnx(4)
       integer       izmnx(8)

C perform standard initialization
       status = 0
       call anm_start( 0,nm,msize,nb,bsize,status )

C read input map: imap is the catalogue identifier
       call map_getmap('Map : ','Default_Map','READ',imap,status)
       call map_alloc_in(imap,'DIRECT',map_array,ip_map,status)
       call enminirt(minirt,status)
       imax = minirt(5)
       jmax = minirt(6)
       call scnmap2(minirt,map_array(ip_map),minirt,zmnx,izmnx,status)
       if (status.ne.0) goto 999

C Perform user interaction
       lowgate = zmnx(2)
       higate = zmnx(1)
       dgate = (higate - lowgate)/9.0
       bad = zmnx(2)
       call io_getr('Start-gate : ','*',lowgate,status)
       if (lowgate.lt.zmnx(2)) lowgate = zmnx(2)
       call io_getr('Increment-gate : ','*',dgate,status)
       call io_getr('End-gate : ','*',higate,status)
       call io_getr('Exclude-data-level : ','*',bad,status)
       diagonal = io_yesno('Include-diagonally-connected-pixels ? ',
     *                     'no',status)
       if (status.ne.0) goto 999

       gate = lowgate
       ntot = 0
       do while (gate.le.higate .and. gate.le.zmnx(1).and.status.eq.0)
         call fractal_cluster(map_array(ip_map),imax,jmax,gate,bad,
     *                diagonal,ids,class,clump,cmax,number,new,status)
         call fractal_validate(ids,imax,jmax,valid,cmax,new,status)
         call fractal_measure(ids,imax,jmax,valid,cmax,new,
     *                        area,bound,status)
         ngood = 0
         do n = 1,new
            if (valid(n)) then
              ntot = ntot + 1
              ngood = ngood + 1
              tot_area(ntot) = area(n)
              tot_bound(ntot) = bound(n)
              tot_gate(ntot) = gate
            endif
         enddo
         gate = gate + dgate
       enddo

C output results
       call io_opefil(iunit,general_results_file,'WRITE',0,status)
       if (status.eq.0) then
         write(iunit,*)'%ndata ',ntot
         write(iunit,*)'%ncols 3'
         do n=1,ntot
           write(iunit,*) tot_bound(n), tot_area(n), tot_gate(n)
         enddo
       endif
       close(iunit)
999    call map_end_alloc(imap,map_array,status)

C check status value and report an error to the user
       call cmd_err(status,'FRACTAL','Failed ')

C finally perform standard shut-down
       call anm_end( status )

       end
C
C
C+ FRACTAL_CLUSTER

      SUBROUTINE FRACTAL_CLUSTER(IMAGE,IMAX,JMAX,GATE,BAD,DIAGONAL,
     >                           IDS,CLASS,CLUMP,CMAX,NUMBER,NEW,S)
C     -------------------------------------------------------------
C
C Given:
C  image dimensions
       integer    imax,jmax
C  2-d image array
       real*4     image(imax,jmax)
C  minimum value for pixel
       real*4     gate
C  miniumum value for data
       real*4     bad
C  true if diagonal connection valid
       logical    diagonal
C
C Returned:
C  2-d array of cluster numbers
       integer    ids(imax,jmax)
C  dimension of CLUSTER
       integer    cmax
C  map of raw clusters
       integer    class(cmax)
C  map of unique clusters
       integer    clump(cmax)
C  number of (raw) clusters
       integer    number
C  number of (unique) clusters
       integer    new
C
C Updated:
C  error status
       integer    s
C
C
C Modified Hashen & Kopleman algorithm. Looks at pixels to left and
C above each pixel, sets cluster number in IDS (and updates CLUSTER
C to lower of these values). Deals with first row, first and last
C columns as required. Pixels connected diagonally only if DIAGONAL
C is true.
C
C It calls: FRACTAL_TOCLUMP to convert CLASSes to unique CLUMPs (1 to NEW)
C           FRACTAL_RENEW   to update IDS to CLUMPs
C           FRACTAL_CHECK   to check no different adjacent CLUMPs
C
C Dave Green --- MRAO --- 1992 July 14th
C Incorporated into Anmap 1993 July 22nd
C-
C
C
      INTEGER LEFT,UPLEFT,UP,UPRIGHT,I,J,OLD
C
      if (s.ne.0) return

      NUMBER=0
      DO J=1,JMAX
        DO I=1,IMAX
          IF(IMAGE(I,J).LE.GATE)THEN
            IF(IMAGE(I,J).LE.BAD)THEN
              IDS(I,J)=-1
            ELSE
              IDS(I,J)=0
            ENDIF
C
C Set adjacent pixels (do not set upper right or upper left in DIAGONAL
C connections not required)
C
          ELSE
            LEFT=0
            UPLEFT=0
            UP=0
            UPRIGHT=0
            IF(I.GT.1)THEN
              IF(IDS(I-1,J).GT.0)LEFT=IDS(I-1,J)
            ENDIF
            IF(J.GT.1)THEN
              IF(IDS(I,J-1).GT.0)UP=IDS(I,J-1)
              IF(DIAGONAL)THEN
                IF(I.GT.1)THEN
                  IF(IDS(I-1,J-1).GT.0)UPLEFT=IDS(I-1,J-1)
                ENDIF
                IF(I.LT.IMAX)THEN
                  IF(IDS(I+1,J-1).GT.0)UPRIGHT=IDS(I+1,J-1)
                ENDIF
              ENDIF
            ENDIF
C
C If left pixel set, use this. Change upper, and upper right
C classes if different
C
            IF(LEFT.GT.0)THEN
              IDS(I,J)=LEFT
              IF(UP.GT.0)THEN
                CALL FRACTAL_BOTTOM(CLASS,CMAX,UP,OLD,S)
                CALL FRACTAL_BOTTOM(CLASS,CMAX,LEFT,NEW,S)
                IF(OLD.NE.NEW)CLASS(OLD)=LEFT
              ELSEIF(UPRIGHT.GT.0)THEN
                CALL FRACTAL_BOTTOM(CLASS,CMAX,UPRIGHT,OLD,S)
                CALL FRACTAL_BOTTOM(CLASS,CMAX,LEFT,NEW,S)
                IF(OLD.NE.NEW)CLASS(OLD)=LEFT
              ENDIF
C
C If DIAGONAL, and upper left set, use this, change upper right if set
C
            ELSEIF(UPLEFT.GT.0)THEN
              IDS(I,J)=UPLEFT
              IF(UPRIGHT.GT.0.AND.UP.EQ.0)THEN
                CALL FRACTAL_BOTTOM(CLASS,CMAX,UPRIGHT,OLD,S)
                CALL FRACTAL_BOTTOM(CLASS,CMAX,UPLEFT,NEW,S)
                IF(OLD.NE.NEW)CLASS(OLD)=UPLEFT
              ENDIF
C
C Or use upper pixel
C
            ELSEIF(UP.GT.0)THEN
              IDS(I,J)=UP
C
C Or, if DIAGONAL, use upper right pixel
C
            ELSEIF(UPRIGHT.GT.0)THEN
              IDS(I,J)=UPRIGHT
C
C Otherwise, this is the first of a new CLASS...
C
            ELSE
              NUMBER=NUMBER+1
              IF(NUMBER.GT.CMAX)THEN
                NUMBER=CMAX
                PRINT *,'***(FRACTAL) too many clumps: ',number,cmax
              ENDIF
              IDS(I,J)=NUMBER
              CLASS(NUMBER)=NUMBER
            ENDIF
C
          ENDIF
        ENDDO
      ENDDO
C
C Convert the CLASS mappings to unique CLUMPs...
C
      CALL FRACTAL_TOCLUMP(CLASS,CLUMP,CMAX,NUMBER,NEW,S)
C
C      DO I=1,NUMBER
C        WRITE(*,'(1X,3I6)')I,CLASS(I),CLUMP(I)
C      ENDDO
C
C Revise IDS to CLUMP number...
C
      CALL FRACTAL_RENEW(IDS,IMAX,JMAX,CLUMP,CMAX,S)
C
C Check no adjacent different CLUMPS...
C
      CALL FRACTAL_CHECK(IDS,IMAX,JMAX,DIAGONAL,S)
C
      RETURN
      END
C
C+ fractal_toclump
C
      SUBROUTINE FRACTAL_TOCLUMP(CLASS,CLUMP,CMAX,NUMBER,NEW,S)
C     ---------------------------------------------------------
C
C Simplify CLASS mappings...

      INTEGER   CMAX,NUMBER,NEW,S
      INTEGER   CLASS(CMAX),CLUMP(CMAX)
C
      INTEGER   N,M,KOUNT,LOW,OLD
      LOGICAL   OK
      OK=.TRUE.
C
      DO N=1,NUMBER
        M=N
        KOUNT=0
        DO WHILE (CLASS(M).NE.M.AND.OK)
          KOUNT=KOUNT+1
          IF(KOUNT.GT.CMAX)THEN
            WRITE(*,'(1X,''ERROR!! more than CMAX loops in TOCLUMP!'')')
            WRITE(*,'(1X,''CLASS.: '',40I3)')(CLASS(LOW),LOW=1,40)
            WRITE(*,'(1X,''CMAX..: '',I3)')CMAX
            WRITE(*,'(1X,''NUMBER: '',I3)')NUMBER
            WRITE(*,'(1X,''M.....: '',I3)')M
            WRITE(*,'(1X)')
            OK=.FALSE.
          ENDIF
          M=CLASS(M)
        ENDDO
        CLUMP(N)=M
      ENDDO
C
      NEW=0
      DO N=1,NUMBER
        IF(CLUMP(N).GT.0)THEN
          OLD=CLUMP(N)
          NEW=NEW+1
          DO M=N,NUMBER
            IF(CLUMP(M).EQ.OLD)THEN
              CLUMP(M)=-NEW
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
      DO N=1,NUMBER
        CLUMP(N)=-CLUMP(N)
      ENDDO
C
      RETURN
      END
C
C
C+ fractal_renew
C
      SUBROUTINE FRACTAL_RENEW(IDS,IMAX,JMAX,CLUMP,CMAX,S)
C     ----------------------------------------------------
C
C Update IDS for unique CLUMP numbers...
C-
      INTEGER   IMAX,JMAX,CMAX,S
      INTEGER   IDS(IMAX,JMAX),CLUMP(CMAX)
C
      INTEGER   I,J
C
      DO J=1,JMAX
        DO I=1,IMAX
          IF(IDS(I,J).GT.0)THEN
            IDS(I,J)=CLUMP(IDS(I,J))
          ENDIF
        ENDDO
      ENDDO
C
      RETURN
      END
C
C+ FRACTAL_CHECK
C
      SUBROUTINE FRACTAL_CHECK(IDS,IMAX,JMAX,DIAGONAL,S)
C     --------------------------------------------------
C
C Check IDS for consistency...
C
      INTEGER   IMAX,JMAX,S
      INTEGER   IDS(IMAX,JMAX)
      LOGICAL   DIAGONAL
C
      INTEGER   I,J
C
      DO J=1,JMAX
        DO I=2,IMAX
          IF((IDS(I-1,J)*IDS(I,J).GT.0).AND.(IDS(I-1,J).NE.IDS(I,J)))
     >       THEN
            WRITE(*,'(1X,''CHECK warning: '',3I3,'' and '',3I3)')
     >        I-1,J,IDS(I-1,J),I,J,IDS(I,J)
          ENDIF
        ENDDO
      ENDDO
C
      DO I=1,IMAX
        DO J=2,JMAX
          IF((IDS(I,J-1)*IDS(I,J).GT.0).AND.(IDS(I,J-1).NE.IDS(I,J)))
     >    THEN
            WRITE(*,'(1X,''CHECK warning: '',3I3,'' and '',3I3)')
     >        I,J-1,IDS(I,J-1),I,J,IDS(I,J)
          ENDIF
        ENDDO
      ENDDO
C
      IF(DIAGONAL)THEN
        DO I=2,IMAX
          DO J=2,JMAX
            IF((IDS(I-1,J-1)*IDS(I,J).GT.0).AND.
     >         (IDS(I-1,J-1).NE.IDS(I,J)))THEN
              WRITE(*,'(1X,''CHECK warning: '',3I3,'' and '',3I3)')
     >          I-1,J-1,IDS(I-1,J-1),I,J,IDS(I,J)
            ENDIF
          ENDDO
        ENDDO
C
        DO I=1,IMAX-1
          DO J=2,JMAX
            IF((IDS(I+1,J-1)*IDS(I,J).GT.0).AND.
     >         (IDS(I+1,J-1).NE.IDS(I,J)))THEN
              WRITE(*,'(1X,''CHECK warning: '',3I3,'' and '',3I3)')
     >          I+1,J-1,IDS(I+1,J-1),I,J,IDS(I,J)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C
      RETURN
      END
C
C
C+ FRACTAL_BOTTOM
C
      SUBROUTINE FRACTAL_BOTTOM(CLASS,CMAX,NUMBER,LOW,S)
C     ------------------------------------------------
C
C Returns lowest CLASS pointer, LOW, for pointer NUMBER

      INTEGER   CMAX,NUMBER,LOW,S
      INTEGER   CLASS(CMAX)
C
      INTEGER   KOUNT,N
      LOGICAL   OK
      OK=.TRUE.
      KOUNT=0
C
      LOW=NUMBER
      DO WHILE (CLASS(LOW).NE.LOW.AND.OK)
        LOW=CLASS(LOW)
        KOUNT=KOUNT+1
        IF(KOUNT.GT.CMAX)THEN
          WRITE(*,'(1X,''ERROR!! more than CMAX loops in BOTTOM!'')')
          WRITE(*,'(1X,''CLASS.: '',40I3)')(CLASS(N),N=1,40)
          WRITE(*,'(1X,''CMAX..: '',I3)')CMAX
          WRITE(*,'(1X,''NUMBER: '',I3)')NUMBER
          WRITE(*,'(1X,''LOW...: '',I3)')LOW
          WRITE(*,'(1X)')
          OK=.FALSE.
        ENDIF
      ENDDO
C
      RETURN
      END
C
C+ FRACTAL_VALIDATE
C
      SUBROUTINE FRACTAL_VALIDATE(IDS,IMAX,JMAX,VALID,CMAX,NEW,S)
C     -----------------------------------------------------------
C
C Check if clump is wholly within image... and not next to any invalid
C pixels (with IDS set to -1)
C
C-
      INTEGER   IMAX,JMAX,CMAX,NEW,S
      INTEGER   IDS(IMAX,JMAX)
      LOGICAL   VALID(CMAX)
C
      INTEGER   N,I,J
C
      DO N=1,NEW
        VALID(N)=.TRUE.
      ENDDO
C
C Check edges of array...
C
      DO I=1,IMAX
        IF(IDS(I,1).NE.0)VALID(IDS(I,1))=.FALSE.
        IF(IDS(I,JMAX).NE.0)VALID(IDS(I,JMAX))=.FALSE.
      ENDDO
      DO J=1,JMAX
        IF(IDS(1,J).NE.0)VALID(IDS(1,J))=.FALSE.
        IF(IDS(IMAX,J).NE.0)VALID(IDS(IMAX,J))=.FALSE.
      ENDDO
C
C Check for adjacent bad pixels...
C
      DO J=2,JMAX-1
        DO I=2,IMAX-1
          IF(IDS(I-1,J-1).LT.0.OR.IDS(I,J-1).LT.0.OR.
     >       IDS(I+1,J-1).LT.0.OR.IDS(I-1,J).LT.0.OR.
     >       IDS(I+1,J).LT.0.OR.IDS(I-1,J+1).LT.0.OR.
     >       IDS(I,J+1).LT.0.OR.IDS(I+1,J+1).LT.0)
     >         VALID(IDS(I,J))=.FALSE.
        ENDDO
      ENDDO
C
      RETURN
      END
C
C
C+ FRACTAL_MEASURE
C
      SUBROUTINE FRACTAL_MEASURE(IDS,IMAX,JMAX,VALID,
     *                           CMAX,NEW,AREA,BOUND,S)
C     -------------------------------------------------
C
C Check if clump is wholly within image...
C
C-
      INTEGER   IMAX,JMAX,CMAX,NEW,S
      INTEGER   IDS(IMAX,JMAX),AREA(CMAX),BOUND(CMAX)
      LOGICAL   VALID(CMAX)
C
      INTEGER   I,J,N
C
      DO N=1,NEW
        AREA(N)=0
        BOUND(N)=0
        IF(VALID(N))THEN
          DO J=2,JMAX-1
            DO I=2,IMAX-1
              IF(IDS(I,J).EQ.N)THEN
                AREA(N)=AREA(N)+1
                BOUND(N)=BOUND(N)+4
                IF(IDS(I-1,J).EQ.N)BOUND(N)=BOUND(N)-1
                IF(IDS(I+1,J).EQ.N)BOUND(N)=BOUND(N)-1
                IF(IDS(I,J-1).EQ.N)BOUND(N)=BOUND(N)-1
                IF(IDS(I,J+1).EQ.N)BOUND(N)=BOUND(N)-1
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDDO
C
      RETURN
      END


