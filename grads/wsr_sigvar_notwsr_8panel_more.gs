function main(args)
yymmddhh=subwrd(args,1)
obsdate=subwrd(args,2)
ltdiff=subwrd(args,3)
vnormgr=subwrd(args,4)
i=subwrd(args,5)


'enable print ec.out'
'clear'
'set map 1 1 4'

'set vpage 0 4.25 8.35 11'
'open sig9.ctl'
'run wsr_s8 'obsdate' '96
if (ltdiff=96) ; 'run wsr_vr 'i ; endif

if (ltdiff>=108)
'set vpage 0 4.25 5.7 8.35'
'open sig10.ctl'
'run wsr_s8 'obsdate' '108
if (ltdiff=108) ; 'run wsr_vr 'i ; endif
endif

if (ltdiff>=120)
'set vpage 0 4.25 3.05 5.7'
'open sig11.ctl'
'run wsr_s8 'obsdate' '120
if (ltdiff=120) ; 'run wsr_vr 'i ; endif
endif

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
