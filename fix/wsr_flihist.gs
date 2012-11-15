*  Script to draw an XY plot.  
*  Does no Error checking on the input file at all. 
*  Assumes the input file is set up as follows:
*
*    Line 1:  Title
*    Line 2:  X Axis Label
*    Line 3:  Y Axis Label (not drawn; needs a GrADS mod)  
*    Line 4:  Axes Limits:  xmin xmax ymin ymax
*    Line 5:  Axes Labels:  xlow xint ylow yint
*    Rest of lines:  X Y points
*
*  Also assumes that a file has been opened (any file, doesn't
*  matter -- the set command doesn't work until a file has been
*  opened).
*
function main(args)
ensemble=subwrd(args,1)
xmin=subwrd(args,2)
xmax=subwrd(args,3)
ymin=subwrd(args,4)
ymax=subwrd(args,5)
xlow=subwrd(args,6)
xint=subwrd(args,7)
yint=subwrd(args,8)
ensdate=subwrd(args,9)
obsdate=subwrd(args,10)
veridate=subwrd(args,11)
vrlon=subwrd(args,12)
vrlat=subwrd(args,13)
radvr=subwrd(args,14)
mem=subwrd(args,15)
vnormgr=subwrd(args,16)
fl1=subwrd(args,17)
fl2=subwrd(args,18)
fl3=subwrd(args,19) 

'enable print ec.out'
'set vpage 0 11 0 8.5'
*'set parea 1 5 1.5 7.5'

'clear'

'set display color'
'set xlab off'
'set frame off'

*ymin = ymin - 5
*ymax = ymax + 5

fname = flights.d
xdif = xmax - xmin
xdif = 9.0/xdif
ydif = ymax - ymin
ydif = 6.0/ydif
ylow = ymin

first = 0
'set ccolor 1'
'set parea 1 10 1.5 7.5'
'set line 1 1 6'
*'draw line 1 1.5 10 1.5'
*'draw line 10 1.5 10 7.5'
*'draw line 10 7.5 1 7.5'
'draw line 1 7.5 1 1.5'
'set line 1 1 3'

yline = ymax - ymin
'set line 1 5 1'
i=ymin
while (i<ymax+1)
  y = 1.5+(i-ymin)*ydif
  x1 = 1.0+(0.5-xmin)*xdif
  x2 = 1.0+(xmax+0.5-xmin)*xdif
  'draw line 'x1' 'y' 'x2' 'y
  i=i+1
endwhile

*'set gxout bar'
*'set baropts filled'
*'set ccolor 2'

  xold=1.0+(0.5-xmin)*xdif
  yold=1.5-ymin*ydif
  'set line 4 1 6'

'set rbcols auto'
col=1
while (1)
if (col>15) ; col=1; endif
'set line 'col' 1 6'
  col = col + 1
  ret = read(fname)
  rc = sublin(ret,1)
  if (rc>0) 
    if (rc!=2) 
      say 'File I/O Error'
      return
    endif
    break
  endif
  rec = sublin(ret,2)
  x = subwrd(rec,1)
  y = subwrd(rec,2)
  x = 1.0+(x+0.5-xmin)*xdif
  y = 1.5+(y-ymin)*ydif
  y0 = 1.5-ymin*ydif
  y0= 1.5
*  first = 1
*  if (first) 
*     'draw line 'xold' 'yold' 'x' 'y
      'draw recf 'xold' 'y0' 'x' 'y
      'set line 1 1 6'
      'draw rec 'xold' 'y0' 'x' 'y
*    'set line 0'  
*    'draw mark 3 'xold' 'yold' 0.1'
*    'set line 1'
*    'draw mark 2 'xold' 'yold' 0.1'
*  endif
*  first = 1
  xold = x
  yold = y
endwhile
*'set line 0'  
*'draw mark 3 'xold' 'yold' 0.1'
*'set line 1'
*'draw mark 2 'xold' 'yold' 0.1'




'set line 1 1 5'
'set string 1 tc 5'
'set strsiz 0.06 0.09'

xx = xlow*'1.0'
while (xx<=xmax & xx>=xmin)
  x = 1.0+(xx-xmin)*xdif
  'draw line 'x' 1.5 'x' 1.47'
  'draw string 'x' 1.40 'xx
  xx = xx + xint
endwhile
yy = ylow*'1.0'
'set string 1 r 5'
'set strsiz 0.12 0.18'
while (yy<=ymax & yy>=ymin)
  y = 1.5+(yy-ymin)*ydif
  'draw line 0.97 'y' 1.0 'y
  'draw string 0.90 'y' 'yy
  yy = yy + yint
endwhile

xstart = 5.5
ystart = 8.2
hsiz=0.11
vsiz=0.15

title='Expected forecast error reduction in verification region (VR) due to adaptive observations along flight tracks.'
'set string 1 bc 6'
'set strsiz 'hsiz' 'vsiz
'draw string 'xstart' 'ystart' '%title

title2='Obs. time: 'obsdate'  Verif. time 'veridate'  VR: 'vrlat'N, 'vrlon'W, 'radvr'km radius  Verif. var.: 'vnormgr' '
ystart = ystart - 0.25
'set string 1 bc 6'
'set strsiz 'hsiz' 'vsiz
'draw string 'xstart' 'ystart' '%title2

xstart = xstart - 0.5
ystart = ystart - 0.25
title3='PSU-NCEP ETKF based on 'mem'-member 'ensdate' 'ensemble' ensemble.  Best flight tracks:'
'set string 1 bc 6'
'set strsiz 'hsiz' 'vsiz
'draw string 'xstart' 'ystart' '%title3

'set strsiz 0.11 0.15'
'set string 2'
'draw string 9.4 'ystart' 'fl1

'set strsiz 0.11 0.15'
'set string 4'
'draw string 9.7 'ystart' 'fl2

'set strsiz 0.11 0.15'
'set string 39'
'draw string 10.0 'ystart' 'fl3

*title1='Observing 'obsdate'. Verification 'veridate'. 'mem'-member 'ensdate' 'ensemble' ensemble.'
*title2='Verification region at 'vrlonw'W 'vrlat'N, radius 'radvr'km.'
*title3='Flight track number'
*'set string 1 c 6'
*'set strsiz 0.13 0.16'
*'draw string 5.5 8.1 'title1
*'draw string 5.5 7.85 'title2
*'set strsiz 0.18 0.20'
*'draw string 5.5 0.8 'title3
*'set string 2'

title4='Flight track number'
'set string 1 c 6'
'set strsiz 0.18 0.20'
'draw string 5.5 0.8 'title4
'set string 2'

'print'
'quit'
