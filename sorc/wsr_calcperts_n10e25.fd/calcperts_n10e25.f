      program read_combined_perts

c      include 'params.f'

      real,allocatable::mrfn(:,:,:,:)
      real,allocatable::mrfp(:,:,:,:)
      real,allocatable::ecmn(:,:,:,:)
      real,allocatable::ecmp(:,:,:,:)
      real,allocatable::mrfmean(:,:,:)
      real,allocatable::ecmmean(:,:,:)
      real,allocatable::commean(:,:,:)
      real,allocatable::datas(:,:,:,:)

c      real mrfn(idim,jdim,memrfh,12), ecmn(idim,jdim,memech,12)
c      real mrfp(idim,jdim,memrfh,12), ecmp(idim,jdim,memech,12)
c      real mrfmean(idim,jdim,12),ecmmean(idim,jdim,12)
c      real commean(idim,jdim,12)
c      real datas(idim,jdim,mem,12)

c mem   = number of independent perturbations = 35
c 10 negative NCEP perturbations, 25 negative ECMWF perturbations
c Perturbations taken about mean of all 50+20 ensemble members
c memrfh=10 for memrf=20-member NCEP ensemble
c memech=25 for memec=50-member ECMWF ensemble

      read(5,*)idim,jdim,mem,memec,memrf,memech,memrfh,nv,maxtint

      allocate(mrfn(idim,jdim,memrfh,12))
      allocate(mrfp(idim,jdim,memrfh,12))
      allocate(ecmn(idim,jdim,memech,12))
      allocate(ecmp(idim,jdim,memech,12))
      allocate(mrfmean(idim,jdim,12))
      allocate(ecmmean(idim,jdim,12))
      allocate(commean(idim,jdim,12))
      allocate(datas(idim,jdim,mem,12))

      do 133 kkk=2,maxtint

c        *** read in MRF ensemble ***
         read(200+kkk) mrfn
         read(200+kkk) mrfp
         close(200+kkk)

      do 150 imem=1,10
      do 150 iv=1,12
         write(26,*) mrfn(3,3,imem,iv)
         write(27,*) mrfp(3,3,imem,iv)
150   continue
         
c        *** read in ECMWF ensemble ***
         read(250+kkk) ecmn
         read(250+kkk) ecmp
         close(250+kkk)

      do 151 iimem=1,25
      do 151 iiv=1,12
         write(28,*) ecmn(3,3,iimem,iiv)
         write(29,*) ecmp(3,3,iimem,iiv)
151   continue

c     *** ensemble mean ***
      do 80 iv=1,12
         do 85 j=1,jdim
         do 85 i=1,idim
            mrfmean(i,j,iv)=0.0
            ecmmean(i,j,iv)=0.0
            commean(i,j,iv)=0.0
            do 90 nm=1,memrfh
               mrfmean(i,j,iv)=mrfmean(i,j,iv) +
     &           (mrfn(i,j,nm,iv)+mrfp(i,j,nm,iv))/float(memrf)
90          continue
            do 91 nm=1,memech
               ecmmean(i,j,iv)=ecmmean(i,j,iv) +
     &           (ecmn(i,j,nm,iv)+ecmp(i,j,nm,iv))/float(memec)
91          continue
            commean(i,j,iv)=mrfmean(i,j,iv)*float(memrf)/float(2*mem) +
     &                      ecmmean(i,j,iv)*float(memec)/float(2*mem)
85       continue
80    continue

c     **************************************************
c     *** perturbations about combined ensemble mean ***
c     **************************************************

      do 63 nm=1,memech
         do 64 iv=1,12
         do 65 j=1,jdim
         do 65 i=1,idim
            datas(i,j,nm,iv)=ecmn(i,j,nm,iv)-commean(i,j,iv)
65       continue
64       continue
63    continue

      do 60 nm=1,memrfh
         na=nm+memech
         do 61 iv=1,12
         do 62 j=1,jdim
         do 62 i=1,idim
            datas(i,j,na,iv)=mrfn(i,j,nm,iv)-commean(i,j,iv)
62       continue
61       continue
60    continue

c     ****************************************
c     *** Write out ensemble perturbations ***
c     ****************************************

      write(1500+kkk) datas
      close(1500+kkk)

133   continue

      return
      end
