      program xvvest_allnorms

      allocatable datav(:,:,:,:)
      allocatable sistr(:)
      allocatable sigtop(:)
      allocatable dswkv(:,:)
      allocatable dwv(:,:) 
      double precision,allocatable::xvvest(:,:)

      real uvsq,uv850sq,prcpsq,mslpsq,z500sq
      real uvsd,uv850sd,prcpsd,mslpsd,z500sd

      read(5,*)idim,jdim,mem,ne1,jstr,casecode

      allocate(datav(idim,jdim,mem,12))
      allocate(sistr(jstr))
      allocate(sigtop(jstr))
      allocate(dswkv(idim,jdim))
      allocate(dwv(idim,jstr)) 
      allocate(xvvest(ne1,mem))

      open(99,form='unformatted')
      read(99) datav
c test
      do 520 j=15,17
      do 520 i=100,102
      do 520 k=1,mem
      do 520 kkkk=1,12
          write(25,*) datav(i,j,k,kkkk)
520   continue

      close(99)

c *** Calculate standard deviation of variables ***
      uvsq = 0.0
      uv850sq = 0.0
      prcpsq  = 0.0
      mslpsq  = 0.0
      z500sq = 0.0

      do 15 j=1,jdim
      do 15 i=1,idim
      do 15 nm=1,mem
         uvsq = uvsq + datav(i,j,nm,1)**2 + datav(i,j,nm,2)**2 +
     &                 datav(i,j,nm,3)**2 + datav(i,j,nm,4)**2 +
     &                 datav(i,j,nm,5)**2 + datav(i,j,nm,6)**2
         uv850sq = uv850sq + datav(i,j,nm,1)**2 + datav(i,j,nm,2)**2
         prcpsq  = prcpsq  + datav(i,j,nm,10)**2
         mslpsq  = mslpsq  + datav(i,j,nm,11)**2
         z500sq  = z500sq  + datav(i,j,nm,12)**2
15    continue

      uvsd = sqrt(uvsq/float(6*idim*jdim*mem))
      uv850sd = sqrt(uv850sq/float(2*idim*jdim*mem)) 
      prcpsd  = sqrt(prcpsq/float(idim*jdim*mem))
      mslpsd  = sqrt(mslpsq/float(idim*jdim*mem))
      z500sd  = sqrt(z500sq/float(idim*jdim*mem))

      print*, 'u,v standard deviation = ',uvsd
      print*, 'u,v 850mb standard deviation = ',uv850sd
      print*, 'prcp standard deviation = ',prcpsd
      print*, 'mslp standard deviation = ',mslpsd
      print*, 'z500 standard deviation = ',z500sd

      knum0=1234
      open(knum0,form='formatted')
      write(knum0,*) uvsd
      write(knum0,*) uv850sd
      write(knum0,*) prcpsd
      write(knum0,*) mslpsd
      write(knum0,*) z500sd
      close(knum0)

c *** Interpolate ensemble perturbations onto stretched grid ***
      do 35 nm=1,mem
         do 28 iv=1,12
            do 27 j=1,jdim
            do 27 i=1,idim
               dswkv(i,j)=datav(i,j,nm,iv)
27          continue
            call interp(dswkv,dwv,sistr,sigtop,jstr,idim,jdim)
            do 30 i=1,idim
            do 30 j=1,jstr
               datav(i,j,nm,iv)=dwv(i,j)
30          continue
28       continue
35    continue

      do 1300 nm=1,mem
         ie1=0
         do 1000 j=1,jstr
         do 1000 i=1,idim
            do 1100 iv=1,12
               ie1=ie1+1
               xvvest(ie1,nm)=datav(i,j,nm,iv)
1100        continue
1000     continue
1300  continue

      knum1=8600+casecode
      write(knum1) xvvest

      STOP
      END
   
