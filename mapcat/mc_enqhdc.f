*+ mc_enqhdc

       subroutine mc_enqhdc( ic, item, value, s)
C      -----------------------------------------
C
C Return the header item as a character for the specified CE
C
C Given:
C   catalogue entry CE
       integer        ic
C   item to enquire
       character*(*)  item
C Returned:
C   value of item -- returned as a character
       character*(*)  value
C Updated:
C   error status
       integer        s
C
C The header for the specified CE is interogated and the value of the
C required parameter is returned if found in the header.
C
C-
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/chrlib_functions.inc'
       include 'mapcat_errors.inc'

       integer  lv

       if (s.ne.0) return

       call redt_load(ic,s)
       value = ' '
       if (s.eq.0) then
         if (chr_match(item,'frequency')) then
           call chr_chrtoc( freq, value, lv )
         elseif (chr_match(item,'quantity')) then
           value = name
         elseif (chr_match(item,'polarization')) then
           value = poln
         elseif (chr_match(item,'unit')) then
           value = exunit
         elseif (chr_match(item,'u-size')) then
           call chr_chitoc(ixmax,value,lv)
         elseif (chr_match(item,'v-size')) then
           call chr_chitoc(iymax,value,lv)
         elseif (chr_match(item,'w-size')) then
           value = '1'
         elseif (chr_match(item,'u1')) then
           call chr_chitoc(iumap1,value,lv)
         elseif (chr_match(item,'u2')) then
           call chr_chitoc(iumap2,value,lv)
         elseif (chr_match(item,'v1')) then
           call chr_chitoc(ivmap1,value,lv)
         elseif (chr_match(item,'v2')) then
           call chr_chitoc(ivmap2,value,lv)
         elseif (chr_match(item,'w1')) then
           value = '1'
         elseif (chr_match(item,'w2')) then
           value = '1'
         elseif (chr_match(item,'telescope-code')) then
           call chr_chitoc(rteles,value,lv)
         elseif (chr_match(item,'polarization-code')) then
           call chr_chitoc(ipoln,value,lv)
         elseif (chr_match(item,'projection-code')) then
           call chr_chitoc(iproj,value,lv)
         elseif (chr_match(item,'flux-normalisation')) then
           call chr_chrtoc(flxnrm,value,lv)
         elseif (chr_match(item,'resolution-u')) then
           call chr_chrtoc(hpfbwu,value,lv)
         elseif (chr_match(item,'resolution-v')) then
           call chr_chrtoc(hpfbwv,value,lv)
         elseif (chr_match(item,'resolution-pa')) then
           call chr_chrtoc(beampa,value,lv)

         else
           s = ill_hditem
         endif
       endif
       end
