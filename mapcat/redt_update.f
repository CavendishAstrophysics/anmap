C
C
*+ redt_update

       subroutine redt_update(type,ipoln,freq,name,unit,status)
C      --------------------------------------------------------
C
C Update the current redtape as required
C
C Given:
C   new map type
       character*(*)    type
C   new polarization code
       integer          ipoln
C   new map frequency
       real*4           freq
C   new name of map quantity
       character*(*)    name
C   new unit of map quantity
       character*(*)    unit
C Returned:
C   Status word
       integer          status
C
C Update the current redtape as required -- select the default value to
C not update a particular item:
C
C Parameter       Description            Default value
C ----------------------------------------------------
C   type          source type            ' '
C   ipoln         polarization code      -1
C   freq          frequency (MHz)        < 0
C   name          map quantity name      ' '
C   unit          map quantity unit      ' '
*-
       include 'mapcat_stack.inc'
       include '/mrao/include/chrlib_functions.inc'

       character*40      string
       integer           len_type, i1
       real*4            en_freq
       integer           en_ipoln
       character*16      en_name, en_unit

C check status on entry
       if (status.ne.0) return

C update type
       len_type = chr_lenb(type)
       len_type = max(4,len_type)
       if (len_type.gt.0) then
         i1 = chr_ilstc(current_source,'-')
         string = current_source(1:i1)
         current_source = string(1:i1)//type(1:len_type)
         call chr_chucas(current_source)
       end if
       call cmd_err(status,'redt_update',' ')

C update other redtape items
       call entype(en_freq,en_ipoln,en_name,en_unit,status)
       if (freq.gt.0.0) en_freq = freq
       if (ipoln.gt.0)  en_ipoln = ipoln
       if (chr_lenb(name).gt.0) en_name = name
       if (chr_lenb(unit).gt.0) en_unit = unit
       call sttype(en_freq,en_ipoln,en_name,en_unit,status)

       end
