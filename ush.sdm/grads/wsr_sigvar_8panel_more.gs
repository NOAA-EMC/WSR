function main(args)
yymmddhh=subwrd(args,1)
obsdate=subwrd(args,2)
fli=subwrd(args,3)
ltdiff=subwrd(args,4)
ensemble=subwrd(args,5)
mem=subwrd(args,6)
vercase1=subwrd(args,7)
vercase2=subwrd(args,8)
vercase3=subwrd(args,9)
vercase4=subwrd(args,10)
vtime1=subwrd(args,11)
vtime2=subwrd(args,12)
vtime3=subwrd(args,13)
vtime4=subwrd(args,14)
vnormgr=subwrd(args,15)

'enable print ec.out'
'clear'
'set map 1 1 4'

'set vpage 0 4.25 8.35 11'
'open sig9.ctl'
'run wsr_s8 'obsdate' '96
if (vtime1=96) ; 'run wsr_vr 'vercase1 ; endif
if (vtime2=96) ; 'run wsr_vr 'vercase2 ; endif
if (vtime3=96) ; 'run wsr_vr 'vercase3 ; endif
if (vtime4=96) ; 'run wsr_vr 'vercase4 ; endif

if (ltdiff>=108)
'set vpage 0 4.25 5.7 8.35'
'open sig10.ctl'
'run wsr_s8 'obsdate' '108
if (vtime1=108) ; 'run wsr_vr 'vercase1 ; endif
if (vtime2=108) ; 'run wsr_vr 'vercase2 ; endif
if (vtime3=108) ; 'run wsr_vr 'vercase3 ; endif
if (vtime4=108) ; 'run wsr_vr 'vercase4 ; endif
endif

if (ltdiff>=120)
'set vpage 0 4.25 3.05 5.7'
'open sig11.ctl'
'run wsr_s8 'obsdate' '120
if (vtime1=120) ; 'run wsr_vr 'vercase1 ; endif
if (vtime2=120) ; 'run wsr_vr 'vercase2 ; endif
if (vtime3=120) ; 'run wsr_vr 'vercase3 ; endif
if (vtime4=120) ; 'run wsr_vr 'vercase4 ; endif
endif

if (ltdiff>=132)
'set vpage 0 4.25 0.4 3.05'
'open sig12.ctl'
'run wsr_s8 'obsdate' '132
if (vtime1=132) ; 'run wsr_vr 'vercase1 ; endif
if (vtime2=132) ; 'run wsr_vr 'vercase2 ; endif
if (vtime3=132) ; 'run wsr_vr 'vercase3 ; endif
if (vtime4=132) ; 'run wsr_vr 'vercase4 ; endif
endif

'set vpage 0 8.5 0 0.4'
xstart = 4.25
ystart = 0.2
hsiz=0.10
vsiz=0.12


title='Signal Variance for 'vnormgr'. Flight 'fli'. Observation time 'obsdate'. 'yymmddhh' ensemble.  '
'set string 1 bc 6'
'set strsiz 'hsiz' 'vsiz
'draw string 'xstart' 'ystart' '%title
'print'
'disable print'
'printim gifimage.out gif x700 y900 white'
'quit'
