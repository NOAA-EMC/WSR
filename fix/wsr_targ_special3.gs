function main(args)
hr1=subwrd(args,1)
hr2=subwrd(args,2)
yymmddhh=subwrd(args,3)
lon1=subwrd(args,4)
lon2=subwrd(args,5)
lat1=subwrd(args,6)
lat2=subwrd(args,7)
ensemble=subwrd(args,8)
mem=subwrd(args,9)
ndrops1=subwrd(args,10)
ndrops2=subwrd(args,11)
ndrops3=subwrd(args,12)
fl1=subwrd(args,13)
fl2=subwrd(args,14)
fl3=subwrd(args,15)
vrlon=subwrd(args,16)
vrlat=subwrd(args,17)
radvr=subwrd(args,18)
vnormgr=subwrd(args,19)

'enable print ec.out'

*** PLOT GRAPH ***

*'set vpage 0 8.5 4.5 10'
'set vpage 0 11 0 8.5'
'set parea 0.5 10.5 0.75 7.3'

'open wsr_targ_flex.ctl'
'set grads off'
'set display color'
*'set display greyscale'
*'set gxout shaded'

"set rgb 39 150 0 100"
"set rbcols 39 39 39 39 39 39 39 39 39 39 39 39 39 39 39"

'set lon 'lon1' 'lon2
'set lat 'lat1' 'lat2
'set grid on'
'set xlopts 1 6 0.14'
'set ylopts 1 6 0.14'
'set clopts -1 3 0.14'
'set annot 1 10'
'set map 1 1 6'
*'set cint 5'
'set csmooth on'
'set cthick 3'
'd pdat.1'
'set gxout line'

fname=circlevr.d
ret=read(fname)
rec = sublin(ret,2)
xlonll=subwrd(rec,1)
xlatll=subwrd(rec,2)
'query ll2xy 'xlonll' 'xlatll
xdum=sublin(result,1)
xlonold=subwrd(xdum,1)
xlatold=subwrd(xdum,2)
*say xlonold' 'xlatold

'set line 2 1 6'

i=1

fname=circlevr.d
while(i<101)
ret=read(fname)
*rc  = sublin(ret,1)
rec2 = sublin(ret,2)
xlonll=subwrd(rec2,1)
xlatll=subwrd(rec2,2)
*say xlonll' 'xlatll
'query ll2xy 'xlonll' 'xlatll
xdum=sublin(result,1)
xlonxy=subwrd(xdum,1)
xlatxy=subwrd(xdum,2)
*say xlonxy' 'xlatxy
'draw line 'xlonxy' 'xlatxy' 'xlonold' 'xlatold
xlonold=xlonxy
xlatold=xlatxy
i=i+1
endwhile

'set line 2 1 6'

i=1
fname='dropplot1.d'
while(i<ndrops1+1)
ret=read(fname)
*rc  = sublin(ret,1)
rec = sublin(ret,2)
xlonll=subwrd(rec,1)
xlatll=subwrd(rec,2)
*say xlonll' 'xlatll
'query ll2xy 'xlonll' 'xlatll
xdum=sublin(result,1)
xlonxy=subwrd(xdum,1)
xlatxy=subwrd(xdum,2)
*say xlonxy' 'xlatxy
'draw mark 3 'xlonxy' 'xlatxy' 0.1'
xlonold=xlonxy
xlatold=xlatxy
i=i+1
endwhile

'set line 4 1 6'

i=1
fname='dropplot2.d'
while(i<ndrops2+1)
ret=read(fname)
*rc  = sublin(ret,1)
rec = sublin(ret,2)
xlonll=subwrd(rec,1)
xlatll=subwrd(rec,2)
*say xlonll' 'xlatll
'query ll2xy 'xlonll' 'xlatll
xdum=sublin(result,1)
xlonxy=subwrd(xdum,1)
xlatxy=subwrd(xdum,2)
*say xlonxy' 'xlatxy
'draw mark 2 'xlonxy' 'xlatxy' 0.1'
xlonold=xlonxy
xlatold=xlatxy
i=i+1
endwhile

'set line 39 1 6'

i=1
fname='dropplot3.d'
while(i<ndrops3+1)
ret=read(fname)
*rc  = sublin(ret,1)
rec = sublin(ret,2)
xlonll=subwrd(rec,1)
xlatll=subwrd(rec,2)
*say xlonll' 'xlatll
'query ll2xy 'xlonll' 'xlatll
xdum=sublin(result,1)
xlonxy=subwrd(xdum,1)
xlatxy=subwrd(xdum,2)
*say xlonxy' 'xlatxy
'draw mark 4 'xlonxy' 'xlatxy' 0.1'
xlonold=xlonxy
xlatold=xlatxy
i=i+1
endwhile


'query ll2xy 'lon1' 'lat1
xdum=sublin(result,1)
xlon1=subwrd(xdum,1)
xlat2=subwrd(xdum,2)
plothi=xlat2 + 1.0
xstart = 5.5
ystart = 8.0
hsiz=0.11
vsiz=0.15

*title='PSU-NCEP ETKF. Observation Time 'hr1'. Verification Time 'hr2'.'
title='Expected forecast error reduction in verification region (VR) due to adaptive observations around any grid point.'
'set string 1 bc 6'
'set strsiz 'hsiz' 'vsiz
'draw string 'xstart' 'ystart' '%title

*title2='Verifying with 'vnormgr'. 'mem'-member 'yymmddhh' 'ensemble' ensemble.'
title2='Obs. time: 'hr1'  Verif. time 'hr2'  VR: 'vrlat'N, 'vrlon'W, 'radvr'km radius  Verif. var.: 'vnormgr' '  
ystart = ystart - 0.25
'set string 1 bc 6'
'set strsiz 'hsiz' 'vsiz
'draw string 'xstart' 'ystart' '%title2

xstart = xstart - 0.5
ystart = ystart - 0.25
*title3='VR radius 'radvr'km, centered at 'vrlon'W, 'vrlat'N. Best flight tracks are'
title3='PSU-NCEP ETKF based on 'mem'-member 'yymmddhh' 'ensemble' ensemble.  Best flight tracks:'
'set string 1 bc 6'
'set strsiz 'hsiz' 'vsiz
'draw string 'xstart' 'ystart' '%title3

'set strsiz 0.11 0.15'
'set string 2'
*'draw string 8.3 'ystart' 'fl1
'draw string 9.4 'ystart' 'fl1

'set strsiz 0.11 0.15'
'set string 4'
*'draw string 8.6 'ystart' 'fl2
'draw string 9.7 'ystart' 'fl2

'set strsiz 0.11 0.15'
'set string 39'
*'draw string 8.9 'ystart' 'fl3
'draw string 10.0 'ystart' 'fl3

'print'
'quit'
