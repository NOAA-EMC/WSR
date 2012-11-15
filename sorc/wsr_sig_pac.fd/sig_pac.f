      program tgr

      real,allocatable::rdat1(:,:)
c     real rdat1(nlon,nlat)
      integer nlon,nlat
      integer ilon, ilat
      integer i1,i2,j1,j2

      read(5,*)rlon1,rlon2,rlat1,rlat2

      i1=nint(rlon1/2.5)+1
      i2=nint(rlon2/2.5)+1
      j1=nint((90.-rlat1)/2.5)+1
      j2=nint((90.-rlat2)/2.5)+1

      nlon=(i2-i1)/2+1
      nlat=(j2-j1)/2+1
      print *,rlon1,rlon2,rlat1,rlat2,i1,i2,j1,j2,nlon,nlat

      allocate(rdat1(nlon,nlat))

      open (unit=211,access='sequential')

      open (unit=251,access='direct',
     &      form='unformatted', status='new',recl=nlon*nlat*4)

      do ilon=1,nlon
        do ilat=1,nlat
          read (211,31) i1, i2, rdat1(ilon,ilat)
c        if (ilon .eq. 100)
c         print '(a6,i3,a7,i3,a7,f15.8)','ilon= ',ilon,' ilat= ',ilat
c     &          ,' rdat1= ',rdat1(ilon,ilat)
        enddo
      enddo

31    format(2i5,f8.3)

      write (251,rec=1) rdat1

      close (211)
      close (251)

      stop
      end
