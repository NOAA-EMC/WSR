function drops(args)
ndrops=subwrd(args,1)

i=1
fname='dropplot.d'
while(i<ndrops+1)
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
'draw mark 3 'xlonxy' 'xlatxy' 0.075'
xlonold=xlonxy
xlatold=xlatxy
i=i+1
endwhile
