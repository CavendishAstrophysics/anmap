*$ ERROR code definitions for ANMAP and Related Utilities
*  ======================================================
C
C Version 1.1 Last updated P. Alexander, 17/7/87
C Version 1.2 Including SPOOLING error codes
C Version 1.3 Including Procedure error codes
C Version 1.4 General update of error codes 11/10/87
C Version 1.5 Declarations added for all variables 18/11/87
C Version 2.0 Stack error codes updtes for new routines (9/11/88)
C Version 3.0 Survey Errors added
C Version 3.1 System-Wide error section added 11/7/88
C Version 4.0 New IOLIB system errors added
C
*$ System Independent Errors and IOLIB errors
*  ------------------------------------------

       INTEGER    ILL_INPUT
       PARAMETER (ILL_INPUT = -1000)   ! Error: user input data

       INTEGER    ILL_OPER
       PARAMETER (ILL_OPER = -1001)    ! Calc: Illegal Operation

       INTEGER    ILL_CALC
       PARAMETER (ILL_CALC = -1002)    ! Calc: Arithmetic error

       INTEGER    ILL_MAXPROC
       PARAMETER (ILL_MAXPROC = -1003) ! Max. No. Procedures reached

       INTEGER    ILL_NUMVAR
       PARAMETER (ILL_NUMVAR = -1004)  ! Max. No. Variables reached

       INTEGER    ILL_FUNC
       PARAMETER (ILL_FUNC   = -1005)  ! Illegal language function

       INTEGER    ILL_PARAM
       PARAMETER (ILL_PARAM  = -1006)  ! Illegal parameter to function
C
C
*$ ANMAP System Errors
*  -------------------
C
       INTEGER    ILL_FATAL
C
       PARAMETER (ILL_FATAL = -100000) ! Fatal consistency error
C
       INTEGER    ILL_NODATA,ILL_STKNUM,ILL_STKSZ,ILL_STKLIM,
     *            ILL_STKSIZ,
     *            ILL_STKACC,ILL_MAPMOD,ILL_MAPSIZ,ILL_WRLOCK,
     *            ILL_MAPWRT,ILL_PGDISC,ILL_NOTACC,ILL_NOOUT,
     *            ILL_DEFMAP,ILL_WORKMAP,ILL_PGNUM,ILL_PGNODISC,
     *            ILL_DISOPT, ILL_POLN
       PARAMETER (ILL_NODATA = -8100)  ! no data for stack entry
       PARAMETER (ILL_STKNUM = -8101)  ! invalid stack entry
       PARAMETER (ILL_STKSZ  = -8102)  ! requested stack size too large
       PARAMETER (ILL_STKSIZ = -8105)  ! no more free stack slots
       PARAMETER (ILL_STKLIM = -8103)  ! stack limit exceeded
       PARAMETER (ILL_STKACC = -8104)  ! unknown access code
       PARAMETER (ILL_MAPMOD = -8110)  ! no write access: modified
       PARAMETER (ILL_MAPSIZ = -8112)  ! map too large for stack
       PARAMETER (ILL_MAPWRT = -8113)  ! not available for write
       PARAMETER (ILL_WRLOCK = -8111)  ! no write access: write-locked
       PARAMETER (ILL_DEFMAP = -8120)  ! no default map
       PARAMETER (ILL_WORKMAP = -8121) ! no work map
       PARAMETER (ILL_PGNUM = -8131)   ! pageing error: stack error
       PARAMETER (ILL_PGDISC = -8133)  ! pageing error: no disc file
       PARAMETER (ILL_PGNODISC = -8132)! pageing error: no disc file
       PARAMETER (ILL_NOTACC = -8134)  ! no access requested
       PARAMETER (ILL_NOOUT = -8135)   ! no output requested
       PARAMETER (ILL_DISOPT = -8150)  ! invalid display option
       PARAMETER (ILL_POLN = -8170)    ! invalid polarization code
C
*$ SPOOLING-SYSTEM error codes
*  ===========================
C
       INTEGER    ILL_NOPROC,ILL_SPOPEN,ILL_NOPREN,ILL_QSUSP
       PARAMETER (ILL_NOPROC = -8501)  ! invalid process name
       PARAMETER (ILL_SPOPEN = -8502)  ! spooling file already open
       PARAMETER (ILL_NOPREN = -8503)  ! queue empty
       PARAMETER (ILL_QSUSP = -8504)   ! queue suspended
C
*$ MAP-DISPLAY sub-system
*  ======================
C
       INTEGER    ILL_ONPIC, ILL_CURSE, ILL_INTER, ILL_POS
       PARAMETER (ILL_ONPIC = -8810)   ! Point not on frame
       PARAMETER (ILL_CURSE = -8820)   ! Error reading cursor
       PARAMETER (ILL_INTER = -8821)   ! Cursor reading abandoned
       PARAMETER (ILL_POS   = -8825)   ! Error in input position
C
*$ RESULTS-ANALYSIS sub-system
*  ===========================

       INTEGER    ILL_FORMD
       PARAMETER (ILL_FORMD = -8850)   ! Invalid format description
C
*$ CLEAN Component File routines
*  =============================
C
       INTEGER    ILL_CCSLE, ILL_NOMAP
       PARAMETER (ILL_CCSLE = -8910) ! CLEAN component file empty
       PARAMETER (ILL_NOMAP = -8915) ! No CLEAN CCSL file

C
*$ BATCH and COMMUNICATON System error codes
*  =========================================
C
       INTEGER    ILL_QUENUM
       INTEGER    ILL_MSGNO,ILL_MSGEX
C
       PARAMETER (ILL_QUENUM= -9201) ! Illegal queue number
       PARAMETER (ILL_MSGNO = -9210) ! No communication file
       PARAMETER (ILL_MSGEX = -9211) ! Communication file exists
C
*$ UTIL-LIBRARY error codes
*  ========================
C
       INTEGER    ILL_NOPRC,ILL_AMBPRC,ILL_LEVPRC
       INTEGER    ILL_MAXCMD,ILL_NOHFIL,ILL_NOLOG,ILL_WKSPAC
C
       PARAMETER (ILL_WKSPAC = -9000) ! insufficient work array space
       PARAMETER (ILL_NOPRC = -9001)  ! No procedure file
       PARAMETER (ILL_AMBPRC = -9002) ! Ambiguous procedure name
       PARAMETER (ILL_LEVPRC = -9003) ! Illegal calling level
C
       PARAMETER (ILL_NOLOG  = -9005) ! No log file
C
       PARAMETER (ILL_MAXCMD = -9010) ! Max. No. of commands exceeded
       PARAMETER (ILL_NOHFIL = -9011) ! No help file
C
*$ SURVEY-SYSTEM error code
*  ========================
C
       INTEGER    ILL_BSET, ILL_SOURCE
C
       PARAMETER (ILL_BSET   = -8510) ! Invalid beam set for map
       PARAMETER (ILL_SOURCE = -8512) ! Source not in catalogue
*-
