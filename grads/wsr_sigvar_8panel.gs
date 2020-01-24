function main(args)
yymmddhh=subwrd(args,1)
obsdate=subwrd(args,2)
fli=subwrd(args,3)
ndrops=subwrd(args,4)
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
'open sig1.ctl'
'run wsr_s8 'obsdate' '00
'run wsr_dr 'ndrops
if (vtime1=00) ; 'run wsr_vr 'vercase1 ; endif
if (vtime2=00) ; 'run wsr_vr 'vercase2 ; endif
if (vtime3=00) ; 'run wsr_vr 'vercase3 ; endif
if (vtime4=00) ; 'run wsr_vr 'vercase4 ; endif
*'run cbarn 1.0'

'set vpage 0 4.25 5.7 8.35'
'open sig2.ctl'
'run wsr_s8 'obsdate' '12
if (vtime1=12) ; 'run wsr_vr 'vercase1 ; endif
if (vtime2=12) ; 'run wsr_vr 'vercase2 ; endif
if (vtime3=12) ; 'run wsr_vr 'vercase3 ; endif
if (vtime4=12) ; 'run wsr_vr 'vercase4 ; endif

'set vpage 0 4.25 3.05 5.7'
'open sig3.ctl'
'run wsr_s8 'obsdate' '24
if (vtime1=24) ; 'run wsr_vr 'vercase1 ; endif
if (vtime2=24) ; 'run wsr_vr 'vercase2 ; endif
if (vtime3=24) ; 'run wsr_vr 'vercase3 ; endif
if (vtime4=24) ; 'run wsr_vr 'vercase4 ; endif

'set vpage 0 4.25 0.4 3.05'
'open sig4.ctl'
'run wsr_s8 'obsdate' '36
if (vtime1=36) ; 'run wsr_vr 'vercase1 ; endif
if (vtime2=36) ; 'run wsr_vr 'vercase2 ; endif
if (vtime3=36) ; 'run wsr_vr 'vercase3 ; endif
if (vtime4=36) ; 'run wsr_vr 'vercase4 ; endif

'set vpage 4.25 8.5 8.35 11'
'open sig5.ctl'
'run wsr_s8 'obsdate' '48
if (vtime1=48) ; 'run wsr_vr 'vercase1 ; endif
if (vtime2=48) ; 'run wsr_vr 'vercase2 ; endif
if (vtime3=48) ; 'run wsr_vr 'vercase3 ; endif
if (vtime4=48) ; 'run wsr_vr 'vercase4 ; endif

'set vpage 4.25 8.5 5.7 8.35'
'open sig6.ctl'
'run wsr_s8 'obsdate' '60
if (vtime1=60) ; 'run wsr_vr 'vercase1 ; endif
if (vtime2=60) ; 'run wsr_vr 'vercase2 ; endif
if (vtime3=60) ; 'run wsr_vr 'vercase3 ; endif
if (vtime4=60) ; 'run wsr_vr 'vercase4 ; endif

'set vpage 4.25 8.5 3.05 5.7'
'open sig7.ctl'
'run wsr_s8 'obsdate' '72
if (vtime1=72) ; 'run wsr_vr 'vercase1 ; endif
if (vtime2=72) ; 'run wsr_vr 'vercase2 ; endif
if (vtime3=72) ; 'run wsr_vr 'vercase3 ; endif
if (vtime4=72) ; 'run wsr_vr 'vercase4 ; endif

'set vpage 4.25 8.5 0.4 3.05'
'open sig8.ctl'
'run wsr_s8 'obsdate' '84
if (vtime1=84) ; 'run wsr_vr 'vercase1 ; endif
if (vtime2=84) ; 'run wsr_vr 'vercase2 ; endif
if (vtime3=84) ; 'run wsr_vr 'vercase3 ; endif
if (vtime4=84) ; 'run wsr_vr 'vercase4 ; endif

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
'printim gifimage.out gif x800 y1000 white'
'quit'
