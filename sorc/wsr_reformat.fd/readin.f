c **********************************************************************
c **********************************************************************
c ************* READ IN NCEP, ECMWF AND CANADIAN ENSEMBLES *************
c ***********************Updated by Yucheng Song 2006.12.08  ***********
c **********************************************************************
c **********************************************************************
c 3456789012345678901234567890123456789012345678901234567890123456789012
c Document
c mem1 is the number of ensembles for each cycle
c mem0 is the total number

      program readin
      USE MACHINE, ONLY: kind_io4
c     real(kind=kind_io4),allocatable::field(:,:) 
c     real(kind=kind_io4),allocatable::field(:,:) 
      real,allocatable::field(:,:) 
      real,allocatable::vble(:,:,:,:) 

c *******************************
c *** Read in ensemble parameters 
c *******************************
      read(5,*)iens,ltime,mem1,mem0,nvv,idim,jdim
      allocate(field(idim,jdim))
      allocate(vble(idim,jdim,mem0,nvv))
c Calculating ensemble perturbations at targeting and verification times
      print*, 'READING ENSEMBLE DATA'

      mem2=2*mem1
      mem3=3*mem1
      mem4=4*mem1

      do 10 nm=1,mem0

         if (iens.eq.1) then
            if (nm.le.mem1) then
               ifile=10000+(nm-1)
            else if (nm.le.mem2) then
               ifile=20000+(nm-1)-mem1
            else if (nm.le.mem3) then
               ifile=30000+(nm-1)-mem2
            else if (nm.le.mem4) then
               ifile=40000+(nm-1)-mem3
            endif
         endif

         if (iens.eq.2) then
            if (nm.le.mem1) then
               ifile=10000+(nm-1)
            endif
         endif

         if (iens.eq.3) then
            if (nm.le.mem1) then
               ifile=10000+(nm-1)
            else if (nm.le.mem2) then
               ifile=30000+(nm-1)-mem1
            endif
         endif

         do 20 ivar=1,nvv
            open(ifile,form='formatted')
            if (ivar.eq.10)
     &    print*, '(Example):Reading from file ',ifile,' ens. member',nm
c           No APCP at f00 
            if (ivar.eq.10.and.nm.le.mem1.and.ltime.eq.0) then
            do 21 j=1,jdim
            do 21 i=1,idim
               vble(i,j,nm,ivar)=0.0
21          continue
            else
            read(ifile,*) idummy,jdummy
            do 30 j=1,jdim
            do 30 i=1,idim
               read(ifile,*) field(i,j)
               vble(i,j,nm,ivar)=field(i,j)
30          continue
            endif
c           print*, "vble(35,60,2,1)=",vble(35,60,2,1)
            
            close(ifile)
            ifile=ifile+500
20       continue

10    continue


      open(112,file='vble.dat',form='unformatted')
      write(112) vble 
      close(112)

c     return
      stop
      end
