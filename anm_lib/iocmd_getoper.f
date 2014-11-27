*+ iocmd_getoper

       subroutine iocmd_getoper( interp, line, ll, results, lr, s )
C      ------------------------------------------------------------
C
C Taking all input from line or the terminal, execute a iocmd option
C
C Given:
C   pointer to tcl interpreter
       integer           interp(*)
C   command line
       character*2048    line
       integer           ll
C Returned:
C   results string
       character*2048    results
       integer           lr
C
C Updated:
C   error status
       integer           s
C
C This routine provides an interactive mechanism for accessing a number
C of iolib routines from scripts.
C
C-
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/iolib_constants.inc'
       include '/mrao/include/cmd_lang_define.inc'
       include '/mrao/include/cmd_lang_param.inc'
       include '/mrao/include/cmd_lang_comms.inc'
       include '/mrao/include/cmd_lang_debug.inc'

C local variables:
C   command line buffer
       character*256   command_line, string
       integer         len_cli
C   unit buffer
       integer         iunit
C   options
       integer         nopts
       parameter      (nopts = 17)
       character*60    opts(nopts), opt
       character*60    lopts(nopts)
C   input data
       character*80    prompt, default
       integer         l, l1, l2, i, n, nv, ia(2048)
       real*4          r, ra(2048)
       real*8          d, da(1024)
       equivalence( ia(1), da(1) )
       equivalence( ra(1), da(1) )

C buffer input command line and io system status
       call io_saveio
       call io_enqin(iunit)
       call io_enqcli( command_line, len_cli )
       call io_setin( terminal_in )
       call io_setcli( 
     *      line(1:ll)//' '//command_line(1:len_cli) )
       line = ' '

C get option    
       opts(1) ='geti .......... prompt for integer' 
       opts(2) ='getni ......... prompt for multiple integers' 
       opts(3) ='getr .......... prompt for real' 
       opts(4) ='getnr ......... prompt for multiple reals' 
       opts(5) ='getd .......... prompt for double precision' 
       opts(6) ='getnd ......... prompt for multiple double precisions'
       opts(7) ='get-word ...... prompt for quoted word' 
       opts(8) ='get-string .... prompt for string' 
       opts(9) ='get-options ... prompt for options' 
       opts(10)='get-date ...... prompt for a date' 
       opts(11)='get-list ...... prompt for a date' 
       opts(12)='onoff ......... prompt for on/off' 
       opts(13)='yesno ......... prompt for yes/no' 
       opts(14)='debug  ........ set debug level in code'
       opts(15)='set-cli ....... set interactive command line'
       opts(16)='clear-cli ..... clear the interactive command line'
       opts(17)='enquire-cli ... enquire the interactive command line'


       call io_getopt( 'IoCmd-option (?=list) : ','geti',
     *                  opts, nopts, opt, s )

C decode option
       results = ' '
       if (opt(1:chr_lenb(opt)).eq.'geti') then
         call io_getwrd('Prompt : ','Integer: ',prompt,l,s)
         call io_getwrd('Default : ','0',default,l,s)
         call io_geti(prompt(1:chr_lenb(prompt)+1),default,i,s)
         call chr_chitoc(i,results,l)

       elseif (opt(1:chr_lenb(opt)).eq.'getni') then
         call io_getwrd('Prompt : ','Integer: ',prompt,l,s)
         call io_getwrd('Default : ','0',default,l,s)
         call io_geti('Values : ','1',nv,s)
         call io_getni(prompt(1:chr_lenb(prompt)+1),default,ia,nv,s)
         l1 = 1
         l2 = len(results)
         do n=1,nv
           if (l1.lt.l2) then
             call chr_chitoc(ia(n),results(l1:l2),l)
           endif
           l1 = l + 1
         enddo

       elseif (opt(1:chr_lenb(opt)).eq.'getr') then
         call io_getwrd('Prompt : ','Real: ',prompt,l,s)
         call io_getwrd('Default : ','0',default,l,s)
         call io_getr(prompt(1:chr_lenb(prompt)+1),default,r,s)
         call chr_chrtoc(r,results,l)

       elseif (opt(1:chr_lenb(opt)).eq.'getnr') then
         call io_getwrd('Prompt : ','Real: ',prompt,l,s)
         call io_getwrd('Default : ','0',default,l,s)
         call io_geti('Values : ','1',nv,s)
         call io_getnr(prompt(1:chr_lenb(prompt)+1),default,ra,nv,s)
         l1 = 1
         l2 = len(results)
         do n=1,nv
           if (l1.lt.l2) then
             call chr_chrtoc(ra(n),results(l1:l2),l)
           endif
           l1 = l + 1
         enddo

       elseif (opt(1:chr_lenb(opt)).eq.'getd') then
         call io_getwrd('Prompt : ','Double: ',prompt,l,s)
         call io_getwrd('Default : ','0',default,l,s)
         call io_getd(prompt(1:chr_lenb(prompt)+1),default,d,s)
         call chr_chdtoc(r,results,l)

       elseif (opt(1:chr_lenb(opt)).eq.'getnd') then
         call io_getwrd('Prompt : ','Double: ',prompt,l,s)
         call io_getwrd('Default : ','0',default,l,s)
         call io_geti('Values : ','1',nv,s)
         call io_getnd(prompt(1:chr_lenb(prompt)+1),default,da,nv,s)
         l1 = 1
         l2 = len(results)
         do n=1,nv
           if (l1.lt.l2) then
             call chr_chdtoc(da(n),results(l1:l2),l)
           endif
           l1 = l + 1
         enddo

       elseif (opt(1:chr_lenb(opt)).eq.'get-word') then
         call io_getwrd('Prompt : ','Word: ',prompt,l,s)
         call io_getwrd('Default : ',' ',default,l,s)
         call io_getwrd(prompt(1:chr_lenb(prompt)+1),
     *        default,results,l,s)

       elseif (opt(1:chr_lenb(opt)).eq.'get-string') then
         call io_getwrd('Prompt : ','String: ',prompt,l,s)
         call io_getwrd('Default : ',' ',default,l,s)
         call io_getstr(prompt(1:chr_lenb(prompt)+1),
     *        default,results,s)

       elseif (opt(1:chr_lenb(opt)).eq.'get-options') then
         call io_getwrd('Prompt : ','Option: ',prompt,l,s)
         call io_getwrd('Default : ',' ',default,l,s)
         call io_geti('Number-Options : ','1',nv,s)
         do n=1,nv
           call io_getwrd('Option (''in quotes'') : ',' ',
     *                    lopts(n),l,s)
         enddo
         call io_getopt(prompt(1:chr_lenb(prompt)+1),
     *                  default,lopts,nv,results,s)

       elseif (opt(1:chr_lenb(opt)).eq.'get-date') then
         call io_getwrd('Prompt : ','Date: ',prompt,l,s)
         call io_getwrd('Default : ','0',default,l,s)
         call io_getdat(prompt(1:chr_lenb(prompt)+1),default,ia,s)
         l1 = 1
         l2 = len(results)
         do n=1,3
           if (l1.lt.l2) then
             call chr_chitoc(ia(n),results(l1:l2),l)
           endif
           l1 = l + 1
         enddo

       elseif (opt(1:chr_lenb(opt)).eq.'get-list') then
         call io_getwrd('Prompt : ','Date: ',prompt,l,s)
         call io_getwrd('Default : ','0',default,l,s)
         call io_geti('Number-of-entries : ','1',nv,s)
         call io_getlst(prompt(1:chr_lenb(prompt)+1),default,
     *                  string,ia,nv,l1,s)
         nv = l1
         l1 = 1
         l2 = len(results)
         do n=1,nv
           if (l1.lt.l2) then
             call chr_chitoc(ia(n),results(l1:l2),l)
           endif
           l1 = l + 1
         enddo

       elseif (opt(1:chr_lenb(opt)).eq.'onoff') then
         call io_getwrd('Prompt : ','Option: ',prompt,l,s)
         call io_getwrd('Default : ',' ',default,l,s)
         if (io_onoff(prompt(1:chr_lenb(prompt)+1),default,s)) then
           results = '1'
         else
           results = '0'
         endif

       elseif (opt(1:chr_lenb(opt)).eq.'yesno') then
         call io_getwrd('Prompt : ','Option: ',prompt,l,s)
         call io_getwrd('Default : ',' ',default,l,s)
         if (io_yesno(prompt(1:chr_lenb(prompt)+1),default,s)) then
           results = '1'
         else
           results = '0'
         endif

       elseif (opt(1:chr_lenb(opt)).eq.'debug') then
         call io_geti('Debug level : ','0',debug_level,s)

       elseif (opt(1:chr_lenb(opt)).eq.'set-cli') then
         continue

       elseif (opt(1:chr_lenb(opt)).eq.'clear-cli') then
         call io_setcli( ' ' )

       elseif (opt(1:chr_lenb(opt)).eq.'enquire-cli') then
         call io_enqcli( results, lr )


       endif

       if (s.ne.0) then
         results = ' '
         lr = 0
       endif
       lr = chr_lenb(results)
       results = results(1:lr)//char(0)
       call io_enqcli( line, len_cli )
       call iocmd_err(s,'iocmd_getoper','User input failed')
       end










