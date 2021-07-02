function plot(args)
obsdate=subwrd(args,1)
lt2=subwrd(args,2)
'set lon 120 300'
*'set lat 10 70'
'set xlint 20'
'set ylint 10'
'set xlopts 1 4 0.14'
'set ylopts 1 4 0.14'
*'set display greyscale'
'set display color'
'set gxout shaded'
'set grid on'
'set annot 1 10'
'set map 1 1 6'
'set mproj lambert'
*'set cint 20.0'
'set ccols  0 0 0  9  14   4  11   5  13   3  10   7  12   8   2   6   6  6 6 6 6 6'
'set clevs 0 0.5 1.6 2.2 3.1 4.0 5.1 6.3 7.5 9.0 11.0 14.0 21.0 29.0 40.0 55 70'
'set csmooth on'
'set grads off'
'set cthick 6'
'd pdat.1'
'set gxout line'
'set line 1 1 10'
*'run cbarn 1.0'

* Add color bar

'run cbar 0.8 0'

'close 1'
title3=''obsdate' + 'lt2'h'
'set string 1 c 6'
'set strsiz 0.20 0.25'
'draw string 4.2 4.9 'title3
return
