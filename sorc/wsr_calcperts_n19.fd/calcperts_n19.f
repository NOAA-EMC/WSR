      program read_NCEP_perts

      real,allocatable::mrfn(:,:,:,:)
      real,allocatable::mrfp(:,:,:,:)
      real,allocatable::mrfmean(:,:,:)
      real,allocatable::datas(:,:,:,:)


c mem    = number of independent NCEP perturbations = 19
c memrf  = number of NCEP members = 20
c memrfh = half the number of NCEP members = 10
c Use this code if ECMWF ensemble is absent

      read(5,*)idim,jdim,mem,memec,memrf,memech,memrfh,nv,maxtint

      allocate(mrfn(idim,jdim,memrfh,12))
      allocate(mrfp(idim,jdim,memrfh,12))
      allocate(mrfmean(idim,jdim,12))
      allocate(datas(idim,jdim,mem,12))

      write(6,*)maxtint
      do 133 kkk=2,maxtint

c     *** read in MRF ensemble ***
      read(200+kkk) mrfn
      read(200+kkk) mrfp
      close(200+kkk)

c     *** ensemble mean ***
      do 80 iv=1,12
         do 85 j=1,jdim
         do 85 i=1,idim
            mrfmean(i,j,iv)=0.0
            do 90 nm=1,memrfh
               mrfmean(i,j,iv)=mrfmean(i,j,iv) +
     &           (mrfn(i,j,nm,iv)+mrfp(i,j,nm,iv))/float(memrf)
90          continue
85       continue
80    continue

c     *************************************************
c     *** 19 perturbations about NCEP ensemble mean ***
c     *************************************************

      do 63 nm=1,memrfh
         do 64 iv=1,12
         do 65 j=1,jdim
         do 65 i=1,idim
            datas(i,j,nm,iv)=mrfn(i,j,nm,iv)-mrfmean(i,j,iv)
            if (nm.lt.memrfh)
     &      datas(i,j,nm+memrfh,iv)=mrfp(i,j,nm,iv)-mrfmean(i,j,iv)
65       continue
64       continue
63    continue

c     ****************************************
c     *** Write out ensemble perturbations ***
c     ****************************************
      write(1500+kkk)datas
      close(1500+kkk)

133   continue

c 20121010 rlw return ==> stop for ifort
c      return
      stop
      end
