      program read_combined_perts

c$$$ program documentation block
c  program: read_combined_perts
c     prgmmr: Yucheng Song   org: emc/ncep        date: 2005-12-18
c  
c  abstract: This program read ensemble products from 
c     ncep, ecmwf and cmc ensemble forecast and output
c     combined perturbations
c
c  program history log: 
c  2004-12-14  song - add more members to the calculation of ensemble 
c                     perturbation     
c  2005-12-19  song - improved the documentation block 
c  2006-12-11  song - implement more ensemble members - major overhual 
c  2008-07-03  song - Add CMC ensemble members  
c     input argument list:
c  
c     output argument list:
c
c Attributes:
c   language: f90
c   machine: ibm RS/6000 sp
c
c$$$    
      real,allocatable::mrf(:,:,:,:)
      real,allocatable::ecm(:,:,:,:)
      real,allocatable::cmc(:,:,:,:)
      real,allocatable::mrfmean(:,:,:)
      real,allocatable::ecmmean(:,:,:)
      real,allocatable::cmcmean(:,:,:)
      real,allocatable::commean(:,:,:)
      real,allocatable::datas(:,:,:,:)


c mem   = number of independent perturbations 
c memrf=84 
c memec=51 
c memcm=42

      read(5,*)idim,jdim,mem,memrf,memec,memcm,
     &         nvar,mintint,maxtint

      allocate(mrf(idim,jdim,memrf,nvar))
      allocate(ecm(idim,jdim,memec,nvar))
      allocate(cmc(idim,jdim,memcm,nvar))
      allocate(mrfmean(idim,jdim,nvar))
      allocate(ecmmean(idim,jdim,nvar))
      allocate(cmcmean(idim,jdim,nvar))
      allocate(commean(idim,jdim,nvar))
      allocate(datas(idim,jdim,mem,nvar))

      do 133 kkk=mintint,maxtint

c        *** read in MRF ensemble ***
         if (memrf.ne.0) then
         read(200+kkk) mrf
         close(200+kkk)
         endif

      do 150 imem=1,10
      do 150 iv=1,12
         write(26,*) mrf(3,3,imem,iv)
150   continue
         
c        *** read in ECMWF ensemble ***
         if(memec.ne.0) then
         read(250+kkk) ecm
         close(250+kkk)
         endif

      do 151 iimem=1,25
      do 151 iiv=1,12
         write(27,*) ecm(3,3,iimem,iiv)
151   continue

c        *** read in CMC ensemble ***
         if(memcm.ne.0) then
         read(300+kkk) cmc 
         close(300+kkk)
         endif

      do 152 iimem=1,25
      do 152 iiv=1,12
         write(28,*) cmc(3,3,iimem,iiv)
152   continue


c     *** ensemble mean ***
      do 80 iv=1,nvar
         do 85 j=1,jdim
         do 85 i=1,idim
            mrfmean(i,j,iv)=0.0
            ecmmean(i,j,iv)=0.0
            cmcmean(i,j,iv)=0.0
            commean(i,j,iv)=0.0
            if(memrf.ne.0)then
            do 90 nm=1,memrf
               mrfmean(i,j,iv)=mrfmean(i,j,iv) +
     &           mrf(i,j,nm,iv)/float(memrf)
90          continue
            endif

            if(memec.ne.0)then
            do 91 nm=1,memec
               ecmmean(i,j,iv)=ecmmean(i,j,iv) +
     &           ecm(i,j,nm,iv)/float(memec)
91          continue
            endif
         
            if(memcm.ne.0)then
            do 92 nm=1,memcm
               cmcmean(i,j,iv)=cmcmean(i,j,iv) +
     &           cmc(i,j,nm,iv)/float(memcm)
92          continue
            endif

            commean(i,j,iv)=mrfmean(i,j,iv)*float(memrf)/float(mem) +
     &                      ecmmean(i,j,iv)*float(memec)/float(mem) +
     &                      cmcmean(i,j,iv)*float(memcm)/float(mem)
85       continue
80    continue

c     **************************************************
c     *** perturbations about combined ensemble mean ***
c     **************************************************

      if(memrf.ne.0)then
      do 63 nm=1,memrf
         do 64 iv=1,nvar
         do 65 j=1,jdim
         do 65 i=1,idim
            datas(i,j,nm,iv)=mrf(i,j,nm,iv)-mrfmean(i,j,iv)
65       continue
64       continue
63    continue
      endif

      if(memec.ne.0)then
      do 60 nm=1,memec
         na=nm+memrf-1
         do 61 iv=1,nvar
         do 62 j=1,jdim
         do 62 i=1,idim
            datas(i,j,na,iv)=ecm(i,j,nm,iv)-ecmmean(i,j,iv)
62       continue
61       continue
60    continue
      endif
 
      if(memcm.ne.0)then
      do 57 nm=1,memcm
         na=nm+memrf+memec-1
         do 58 iv=1,nvar
         do 59 j=1,jdim
         do 59 i=1,idim
            datas(i,j,na,iv)=cmc(i,j,nm,iv)-cmcmean(i,j,iv)
59       continue
58       continue
57    continue
      endif

c     ****************************************
c     *** Write out ensemble perturbations ***
c     ****************************************

      write(1500+kkk) datas
      close(1500+kkk)

133   continue

c 20121010 rlw return==>stop for ifort
c      return
      stop
      end
