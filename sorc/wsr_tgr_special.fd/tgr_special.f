      
      program tgr_special

      integer ilon,ilat
      integer nlon,nlat
     
      real,allocatable::rdat1(:,:)

      read(5,*)nlon,nlat

      allocate(rdat1(nlon,nlat))

      open (unit=111,access='sequential')

      open (unit=121,access='direct',
     &      form='unformatted', status='new',recl=nlon*nlat*4)

      do ilat=1,nlat
        do ilon=1,nlon
          read (111,31) i1, i2, rdat1(ilon,ilat)
c        if (ilon .eq. 100)
c         print '(a6,i3,a7,i3,a7,f15.8)','ilon= ',ilon,' ilat= ',ilat
c     &          ,' rdat1= ',rdat1(ilon,ilat)
        enddo
      enddo

31    format(2i5,f12.3)

      write (121,rec=1) rdat1

      close (111)
      close (121)

      stop
      end
