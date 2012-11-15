!
!
!  Description: Set valued for ECWMF application 
!
!  Author: Yucheng Song EMC/NCEP                            
!
!  2008.07.18

program dtsset 
  use grib_api
! use machine
  implicit none
  integer(kind = 4)    :: centre, date
  integer(kind = 4)    :: infile,outfile
  integer(kind = 4)    :: igrib, iret
  integer(kind = 4)    :: mem      
  integer(kind = 4)    :: nlon      
  integer(kind = 4)    :: nlat     
  integer(kind = 4)    :: ilon,ilat,i1,i2     
  integer(kind = 4)    :: obsdate,leadtime,opttime 
  integer(kind = 4)    :: lon1,lon2,lat1,lat2,reso
  integer(kind = 4)    :: vrlatu,vrlonl,vrlatl,vrlonu 
  real(kind = 4)       :: rlon1,rlon2,rlat1,rlat2,rreso
  real(kind = 4)       :: rvrlatu,rvrlonl,rvrlatl,rvrlonu 
  real(kind = 4), dimension(:), allocatable :: values

      read(5,*)nlon,nlat,rlon1,rlon2,rlat1,rlat2,rreso,obsdate,rvrlatu,rvrlonl,&
       rvrlatl,rvrlonu,leadtime,opttime,mem
       if (rlon1.gt.180) then
        rlon1=rlon1 - 360.0
       endif
       if (rlon2.gt.180) then
        rlon2=rlon2 - 360.0
       endif
       lon1=rlon1*1000
       lon2=rlon2*1000
       lat1=rlat1*1000
       lat2=rlat2*1000
       reso=rreso*1000*2
       if (rvrlonl.gt.180) then
       rvrlonl=rvrlonl-360.0
       endif 
       if (rvrlonu.gt.180) then
       rvrlonu=rvrlonu-360.0
       endif 

       vrlatu=rvrlatu*100
       vrlonl=rvrlonl*100
       vrlatl=rvrlatl*100
       vrlonu=rvrlonu*100

      print*, nlon, nlat, lon1,lon2, mem
      open (unit=121,FORM='FORMATTED')
      allocate(values(nlon*nlat), stat=iret)

      do ilat=1,nlat
        do ilon=1,nlon
        read (121,31)i1, i2, values((ilat-1)*nlon+ilon)
        enddo
      enddo

31    format(2i5,f12.3)

  centre = 07
  call current_date(date)
  print*, date
! call grib_open_file(infile,'2008012800_354.grib','r')

! call grib_new_from_file(infile,igrib)
  call grib_new_from_template(igrib, 'SAC_NCEP')
  print*, igrib

  call grib_open_file(outfile, &
       'out.grib1','w')

  call grib_set(igrib,'dataDate',obsdate/100)
  call grib_set(igrib,'dataTime',mod(obsdate,100)*100)
! call grib_set(igrib,'date',obsdate)
  call grib_set(igrib,'centre',centre)
  call grib_set(igrib,'values', values)

  call grib_set(igrib,'editionNumber',1)
! ECMWF local table 2 version 128
  call grib_set(igrib,'gribTablesVersionNo',128)
  call grib_set(igrib,'identificationOfOriginatingGeneratingCentre',7)
  call grib_set(igrib,'generatingProcessIdentifier',96)
  call grib_set(igrib,'gridDefinition',255) ! undefined GRID, user defined

  call grib_set(igrib,'section1Flags',128)
!  According to Table 128 or NCEP table, 126 = WMIXEsfc 
  call grib_set(igrib,'indicatorOfParameter',126) 

  call grib_set(igrib,'indicatorOfTypeOfLevel',1) 
  call grib_set(igrib,'level',0)

! No need if called grib_set(igrib,'date',date)
! call grib_set(igrib,'yearOfCentury',8)
! call grib_set(igrib,'month',1)
! call grib_set(igrib,'day',28)
! call grib_set(igrib,'hour',0)
! call grib_set(igrib,'minute',0)

  call grib_set(igrib,'localDefinitionNumber',21)

    ! 9 = TOST (mars/class.table)  
  call  grib_set(igrib,'marsClass',9)

    ! 64 = Signal variance (mars/type.table)  */
  call grib_set(igrib,'marsType',64)

    ! 1110 = Sensitive area prediction (mars/stream.table)  */
  call grib_set(igrib,'marsStream',1110)

  call grib_set(igrib,'numberOfForecastsInEnsemble',mem)
! 0 lat-lon box region, 1 circular region
  call grib_set(igrib,'shapeOfVerificationArea',1)
! Verification Region definition
  call grib_set(igrib,'accuracyMultipliedByFactor',100)
  call grib_set(igrib,'numberOfVerticalCoordinateValues',0)
  call grib_set(igrib,'northWestLatitudeOfVerficationArea',vrlatu)
  call grib_set(igrib,'northWestLongitudeOfVerficationArea',vrlonl)
  call grib_set(igrib,'southEastLatitudeOfVerficationArea',vrlatl)
  call grib_set(igrib,'southEastLongitudeOfVerficationArea',vrlonu)

! Set Target time values
  call grib_set(igrib,'optimisationTime',opttime)
  call grib_set(igrib,'forecastLeadTime',leadtime)


!    0 = Latitude/Longitude Grid (grib1/6.table)  
   call  grib_set(igrib,'dataRepresentationType',0)

!    Set Search domain

   call  grib_set(igrib,'numberOfPointsAlongAParallel',nlon)
   call  grib_set(igrib,'numberOfPointsAlongAMeridian',nlat)
   call  grib_set(igrib,'latitudeOfFirstGridPoint',lat2)
   call  grib_set(igrib,'latitudeOfLastGridPoint',lat1)
   call  grib_set(igrib,'longitudeOfFirstGridPoint',lon1)
   call  grib_set(igrib,'longitudeOfLastGridPoint',lon2)
   call  grib_set(igrib,'iDirectionIncrement',reso)
   call  grib_set(igrib,'jDirectionIncrement',reso)
!  Refer http://www.nco.ncep.noaa.gov/pmb/docs/on388/table7.html 
!  You have to convert the binary to decimal, e.g. 10000000 = 128
!  
   call  grib_set(igrib,'resolutionAndComponentFlags',128)
   call  grib_set(igrib,'scanningMode',0)
  
!  call  grib_set_long(igrib,"complexPacking",0)
   
  deallocate(values)

! check if it is correct in the actual GRIB message
  call check_settings(igrib)
  !     write modified message to a file
  call grib_write(igrib,outfile)

  call grib_release(igrib)

  call grib_close_file(infile)

  call grib_close_file(outfile)

contains

!======================================
subroutine current_date(date)
integer, intent(out) :: date

integer              :: val_date(8)
call date_and_time ( values = val_date)

date = val_date(1)* 10000 + val_date(2)*100 + val_date(3) 
end subroutine current_date
!======================================
subroutine check_settings(gribid)
  use grib_api
  implicit none
  integer, intent(in) :: gribid
  
  integer(kind = 4)    :: int_value
  character(len = 10)  :: string_value

  !     get centre as a integer
  call grib_get(gribid,'centre',int_value)
  write(*,*) "get centre as a integer - centre = ",int_value
  
  !     get centre as a string
  call grib_get(gribid,'centre',string_value)
  write(*,*) "get centre as a string  - centre = ",string_value
  
  !     get date as a string
  call grib_get(gribid,'dataDate',string_value)
  write(*,*) "get date as a string    - date = ",string_value
  
end subroutine check_settings
end program dtsset
