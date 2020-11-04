      program calc_ensemble_spread
c     
c     read in ensemble perturbation and calculate the spread
c     programmer: Yucheng Song   

      real,allocatable::datas(:,:,:,:)
      real,allocatable::var(:,:,:)
      real,allocatable::varuv(:,:)
      real,allocatable::varuvt(:,:)
      real,allocatable::varsfc(:,:)

      real uvsq,uv850sq,prcpsq,mslpsq,z500sq
      real uvsd,uv850sd,prcpsd,mslpsd,z500sd

      read(5,*)idim,jdim,mem,memrf,memec,memcm,nv,mintint,maxtint

      allocate(datas(idim,jdim,mem,nv))
      allocate(var(idim,jdim,nv))
      allocate(varuv(idim,jdim))
      allocate(varuvt(idim,jdim))
      allocate(varsfc(idim,jdim))

      do 133 kkk=mintint,maxtint

         read(1500+kkk) datas
         close(1500+kkk)

         do 10 iv=1,nv
         do 10 i=1,idim
         do 10 j=1,jdim
            var(i,j,iv)=0.0
            do 20 nm=1,mem
               var(i,j,iv)=var(i,j,iv)+datas(i,j,nm,iv)**2
20          continue
            var(i,j,iv)=var(i,j,iv)/float(mem)
10       continue

         do 15 i=1,idim
         do 15 j=1,jdim
            uvsq = uvsq + var(i,j,1) + var(i,j,2) +
     &                    var(i,j,3) + var(i,j,4) +
     &                    var(i,j,5) + var(i,j,6)
            uv850sq = uv850sq + var(i,j,1) + var(i,j,2)
            prcpsq  = prcpsq  + var(i,j,10)
            mslpsq  = mslpsq  + var(i,j,11)
            z500sq  = z500sq  + var(i,j,12)
15       continue

         uvsd    = sqrt(uvsq/float(6*idim*jdim*mem))
         uv850sd = sqrt(uv850sq/float(2*idim*jdim*mem))
         prcpsd  = sqrt(prcpsq/float(idim*jdim*mem))
         mslpsd  = sqrt(mslpsq/float(idim*jdim*mem))
         z500sd  = sqrt(z500sq/float(idim*jdim*mem))

         do 603 i=1,idim,2
         do 603 j=1,jdim,2

c        *** Spread of horizontal wind cpts averaged over 3 levels ***
            varuv(i,j)=var(i,j,1)+var(i,j,2)+var(i,j,3)+
     &                 var(i,j,4)+var(i,j,5)+var(i,j,6)
            write(1600+kkk,666) i,j,sqrt(varuv(i,j)/3.0)

c        *** Spread of kinetic energy averaged over 3 levels ***
            varuvt(i,j)=0.5*( var(i,j,1)+var(i,j,2)+var(i,j,3)+
     &                        var(i,j,4)+var(i,j,5)+var(i,j,6) ) +
     &                3.346*( var(i,j,7)+var(i,j,8)+var(i,j,9) )
            write(1620+kkk,666) i,j,sqrt(varuvt(i,j)/3.0)

c        *** Spread of surface variables ***
            varsfc(i,j)=(var(i,j,1)+var(i,j,2))/(uv850sd**2) +
     &      var(i,j,10)/(prcpsd**2)+var(i,j,12)/(mslpsd**2)
            write(1640+kkk,666) i,j,sqrt(varsfc(i,j))

603      continue

133   continue

666   format(2i5,f8.3)

c 20121011 rlw return ==> stop for ifort
c      return
      stop
      end
