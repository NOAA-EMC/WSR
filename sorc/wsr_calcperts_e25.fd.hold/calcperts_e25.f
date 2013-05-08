      program read_ECMWF_perts

      real,allocatable::ecmn(:,:,:,:)
      real,allocatable::ecmp(:,:,:,:)
      real,allocatable::ecmmean(:,:,:)
      real,allocatable::datas(:,:,:,:)

c mem    = number of independent ECMWF perturbations = 25
c memec  = number of ECMWF members = 50
c memech = half the number of ECMWF members = 25
c Use this code if NCEP ensemble is absent

      read(5,*)idim,jdim,mem,memec,memrf,memech,memrfh,nv,maxtint

      allocate(ecmn(idim,jdim,memech,12))
      allocate(ecmp(idim,jdim,memech,12))
      allocate(ecmmean(idim,jdim,12))
      allocate(datas(idim,jdim,mem,12))

      do 133 kkk=2,maxtint

c     *** read in ECMWF ensemble ***
      read(250+kkk) ecmn
      read(250+kkk) ecmp
      close(250+kkk)

c     *** ensemble mean ***
      do 80 iv=1,12
         do 85 j=1,jdim
         do 85 i=1,idim
            ecmmean(i,j,iv)=0.0
            do 90 nm=1,memech
               ecmmean(i,j,iv)=ecmmean(i,j,iv) +
     &           (ecmn(i,j,nm,iv)+ecmp(i,j,nm,iv))/float(memec)
90          continue
85       continue
80    continue

c     **************************************************
c     *** 25 perturbations about ECMWF ensemble mean ***
c     **************************************************

      do 63 nm=1,memech
         do 64 iv=1,12
         do 65 j=1,jdim
         do 65 i=1,idim
            datas(i,j,nm,iv)=ecmn(i,j,nm,iv)-ecmmean(i,j,iv)
65       continue
64       continue
63    continue

c      do 260 nm=1,mem
c         do 262 j=1,jdim
c         do 262 i=1,idim
c            datauvt(i,j,nm,1)=datas(i,j,nm,1)
c            datauvt(i,j,nm,2)=datas(i,j,nm,2)
c            datauvt(i,j,nm,3)=datas(i,j,nm,3)
c            datauvt(i,j,nm,4)=datas(i,j,nm,4)
c            datauvt(i,j,nm,5)=datas(i,j,nm,5)
c            datauvt(i,j,nm,6)=datas(i,j,nm,6)
c            datauvt(i,j,nm,7)=datas(i,j,nm,7)
c            datauvt(i,j,nm,8)=datas(i,j,nm,8)
c            datauvt(i,j,nm,9)=datas(i,j,nm,9)
c            dataprcp(i,j,nm)=datas(i,j,nm,10)
c            datamslp(i,j,nm)=datas(i,j,nm,11)/100.
c            dataz500(i,j,nm)=datas(i,j,nm,12)
c262      continue
c260   continue

c     ****************************************
c     *** Write out ensemble perturbations ***
c     ****************************************

      write(1500+kkk) datas
      close(1500+kkk)

133   continue

c 20121010 rlw return ==> stop for ifort
c      return
      stop
      end
