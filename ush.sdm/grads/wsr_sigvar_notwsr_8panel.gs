* Change history
* 2006/12/14 Yucheng Song
* Modified to add the printim command 
function main(args)
yymmddhh=subwrd(args,1)
obsdate=subwrd(args,2)
ltdiff=subwrd(args,3)
ndrops=subwrd(args,4)
vnormgr=subwrd(args,5)
i=subwrd(args,6)


'enable print ec.out'
'clear'
'set map 1 1 4'

'set vpage 0 4.25 8.35 11'
'open sig1.ctl'
'run wsr_s8 'obsdate' '00
'run wsr_dr 'ndrops
*'run cbarn 1.0'

'set vpage 0 4.25 5.7 8.35'
'open sig2.ctl'
'run wsr_s8 'obsdate' '12
if (ltdiff=12) ; 'run wsr_vr 'i ; endif

'set vpage 0 4.25 3.05 5.7'
'open sig3.ctl'
'run wsr_s8 'obsdate' '24
if (ltdiff=24) ; 'run wsr_vr 'i ; endif

'set vpage 0 4.25 0.4 3.05'
'open sig4.ctl'
'run wsr_s8 'obsdate' '36
if (ltdiff=36) ; 'run wsr_vr 'i ; endif

'set vpage 4.25 8.5 8.35 11'
'open sig5.ctl'
'run wsr_s8 'obsdate' '48
if (ltdiff=48) ; 'run wsr_vr 'i ; endif

'set vpage 4.25 8.5 5.7 8.35'
'open sig6.ctl'
'run wsr_s8 'obsdate' '60
if (ltdiff=60) ; 'run wsr_vr 'i ; endif

'set vpage 4.25 8.5 3.05 5.7'
'open sig7.ctl'
'run wsr_s8 'obsdate' '72
if (ltdiff=72) ; 'run wsr_vr 'i ; endif

'set vpage 4.25 8.5 0.4 3.05'
'open sig8.ctl'
'run wsr_s8 'obsdate' '84
if (ltdiff=84) ; 'run wsr_vr 'i ; endif

'set vpage 0 8.5 0 0.4'
xstart = 4.25
ystart = 0.2
hsiz=0.10
vsiz=0.12


title='Signal Variance for 'vnormgr'.  Observation time 'obsdate'. 'yymmddhh' ensemble.'
'set string 1 bc 6'
'set strsiz 'hsiz' 'vsiz
'draw string 'xstart' 'ystart' '%title

'print'
'disable print'
'printim gifimage.out gif x700 y900 white' 
'quit'
