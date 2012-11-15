function main(args)
case=subwrd(args,1)

if (case=1) ; fname=circlevr1.d ; endif
if (case=2) ; fname=circlevr2.d ; endif
if (case=3) ; fname=circlevr3.d ; endif
if (case=4) ; fname=circlevr4.d ; endif
if (case=5) ; fname=circlevr5.d ; endif
if (case=6) ; fname=circlevr6.d ; endif
if (case=7) ; fname=circlevr7.d ; endif
if (case=8) ; fname=circlevr8.d ; endif
if (case=9) ; fname=circlevr9.d ; endif
if (case=10) ; fname=circlevr10.d ; endif

ret=read(fname)
rec = sublin(ret,2)
xlonll=subwrd(rec,1)
xlatll=subwrd(rec,2)
'query ll2xy 'xlonll' 'xlatll
xdum=sublin(result,1)
xlonold=subwrd(xdum,1)
xlatold=subwrd(xdum,2)
*say xlonold' 'xlatold

i=1
if (case=1) ; fname=circlevr1.d ; endif
if (case=2) ; fname=circlevr2.d ; endif
if (case=3) ; fname=circlevr3.d ; endif
if (case=4) ; fname=circlevr4.d ; endif
if (case=5) ; fname=circlevr5.d ; endif
if (case=6) ; fname=circlevr6.d ; endif
if (case=7) ; fname=circlevr7.d ; endif
if (case=8) ; fname=circlevr8.d ; endif 
if (case=9) ; fname=circlevr9.d ; endif
if (case=10) ; fname=circlevr10.d ; endif
while(i<101)
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
'set line 1 1 6'
'draw line 'xlonxy' 'xlatxy' 'xlonold' 'xlatold
xlonold=xlonxy
xlatold=xlatxy
i=i+1
endwhile
