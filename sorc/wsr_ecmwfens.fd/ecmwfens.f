      program ecmwfens
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: ECMWFENS
C   PRGMMR: WOBUS            ORG: NP20        DATE: 2001-01-17
C
C ABSTRACT: This program converts GRIB ensemble header
c   extensions from ECMWF's format to NCEP's format.  It
c   also calculates ensemble mean and spread fields.
C
C PROGRAM HISTORY LOG:
C   97-01-17  MARCHOK     original program
c   99-09-29  MARCHOK     converted to run on Cray5
c   00-03-17  Wobus       IBM version
C   01-01-16  WOBUS       added DOCBLOCK, removed nonstandard
c                         output
C
C USAGE:
C   INPUT FILES:
c     unit 5   - Namelist NAMIN parameters:  
c                     resflag=2 for 2.5x2.5
c                     maxmem = number of members
c     unit 11  - input GRIB file for one forecast length
c     unit 21  - GRIB index file corresponding to unit 11
C
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
C     unit 6   - standard output
C     unit 51  - GRIB output for rh700
C     unit 52  - GRIB output for z1000
C     unit 53  - GRIB output for z500
C     unit 54  - GRIB output for t500
C     unit 55  - GRIB output for u500
C     unit 56  - GRIB output for v500
C     unit 57  - GRIB output for t850
C     unit 58  - GRIB output for u850
C     unit 59  - GRIB output for v850
C     unit 60  - GRIB output for t200
C     unit 61  - GRIB output for u200
C     unit 62  - GRIB output for v200
C     unit 63  - GRIB output for mslp
C     unit 64  - GRIB output for prcp
C     unit 71  - GRIB output for prcp mean
C     unit 72  - GRIB output for mslp mean
C     unit 73  - GRIB output for t850 mean
C     unit 74  - GRIB output for z500 mean
C     unit 75  - GRIB output for z1000 mean
C     unit 76  - GRIB output for rh700 mean
C     unit 81  - GRIB output for prcp spread
C     unit 82  - GRIB output for mslp spread
C     unit 83  - GRIB output for t850 spread
C     unit 84  - GRIB output for z500 spread
C     unit 85  - GRIB output for z1000 spread
C     unit 86  - GRIB output for rh700 spread
C
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)
C     UNIQUE:    - 
c       INCLUDED - getstats, adjpds, adjext, output, output_stats,
c                  grange, getgbece, ecmext,
c                  grib_close, grib_open, grib_open_wa, grib_open_r
C     LIBRARY:
C       W3LIB    - gbyte,fi632,fi633,w3fi63,w3tagb,w3tage 
c       BACIO    - baopen,baopenwa,baopenr,baclose,baread
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C REMARKS: 
c     Error messages from W3LIB routines will be printed but
c     will not halt execution
C
C ATTRIBUTES:
C   LANGUAGE: Fortran 90
C   MACHINE:  IBM SP
C
C$$$
c
c
c     *** 9/29/99: Due to cray-3 emergency, this program is being
c         implemented on Cray-5 and is being modified to be more
c         efficient.  It doesn't worry about what order the 
c         members are in, it just copies them over.  It still 
c         changes the height level values for the 500 mb height
c         and for mslp for GrADS purposes.  It will leave the 
c         precip fields as they are.
c
c     *** 3/17/00 IBM version
c
c     lugb         logical unit of the unblocked grib data file
c     lugi         logical unit of the unblocked grib index file
c
c     kres         1 - input file contains only high res control.
c                      This lets the program know that you're reading
c                      a "USD" file.
c                  2 - input file contains low res control and
c                      perturbations.  This lets the program know 
c                      you're reading a "USE" file, which has 1 LRC
c                      record for each parm, plus 50 perturbations
c                      for each parm.
c                 (Important to know since high res control
c                  does NOT have a PDS extension)
c
c     memberct    Keeps track of how many members have been read in
c                 for each parameter.  In this way, we can modify the
c                 height level for each successive member (in our 
c                 kludgy system) to be 1,2,3,....,50,51.  And only
c                 index this as "2" since we are only doing this 
c                 kludgy method to 2 variables, z500 and mslp.
c
ccl  level labeling of members is commented out 
ccs  additional statistical output is commented out
c
c
      parameter(lugi=21,lugb=11,jf=512*256,nlat=73,nlon=144)
      parameter(numec=52)
      character*1 contflag,gotz500pds,gotmslppds,gotprcppds,gott850pds
      character*1 gotz1000pds,gotrh700pds
      integer   jpds(200),jgds(200),kpds(200),kgds(200)
      integer   jens(200),kens(200)
      integer   kholdz500pds(200),kholdmslppds(200)
      integer   kholdt850pds(200),kholdprcppds(200)
      integer   kholdz1000pds(200),kholdrh700pds(200)
      integer   memberct(2)
      integer   z500ct,z1000ct,rh700ct,t850ct,prcpct,mslpct
      logical   lb(jf)
      real      f(jf)
      real      z500vals(numec,nlat*nlon),z1000vals(numec,nlat*nlon)
      real      t850vals(numec,nlat*nlon),rh700vals(numec,nlat*nlon)
      real      prcpvals(numec,nlat*nlon),mslpvals(numec,nlat*nlon)
      real      z500mean(nlat*nlon),z1000mean(nlat*nlon)
      real      t850mean(nlat*nlon),rh700mean(nlat*nlon)
      real      prcpmean(nlat*nlon),mslpmean(nlat*nlon)
      real      z500spr(nlat*nlon),z1000spr(nlat*nlon)
      real      t850spr(nlat*nlon),rh700spr(nlat*nlon)
      real      prcpspr(nlat*nlon),mslpspr(nlat*nlon)
      namelist/namin/kres,kmaxmem
c
      call w3tagb('ECMWFENS',2001,0017,0088,'NP20')
c
      read (5,namin,end=1000)
 1000 continue
c
      gotz500pds = 'n'
      gotz1000pds = 'n'
      gotmslppds = 'n'
      gotprcppds = 'n'
      gott850pds = 'n'
      gotrh700pds = 'n'
c
      print *,' '
      print *,'------------------------------------------------------'
      print *,'at beginning of ecmwfens.f, kres= ',kres
     &       ,' kmaxmem= ',kmaxmem

      prcpct = 0
      z500ct  = 0
      z1000ct  = 0
      mslpct = 0
      t850ct = 0
      rh700ct = 0
 
c     maxloop = number of perturbations + the LRC record.  Remember, 
c     the HRC record is in a file by itself, and is handled by the 
c     case of kres=1.

      maxloop = kmaxmem + 1
      jpds = -1
      jgds = -1
      jpds(23) = 0
      memberct = 0

c      jpds(23) = 0
      j = 0
      iret = 0

      do while (iret.eq.0)
 
c       Use my modified version of getgbens in this program to 
c       read the recs.  This version of getgbens reads different
c       bytes from the ECMWF PDS extension than are read from the 
c       NCEP PDS extension. *** NOTE: THE VERSION OF GETGBENS 
c       THAT IS CALLED HAS BEEN MODIFIED FROM THE W3LIB VERSION.
c  Modified getgbens has been renamed getgbece

        print *,'rlwdebug before first call getgbece',lugb,lugi
        print *,'rlwdebug getgbece args',jf,j,jpds(1),jgds(1)
        print *,'rlwdebug getgbece args',jens(1)
        print *,'rlwdebug getgbece args',kf,k,kpds(1),kgds(1)
        print *,'rlwdebug getgbece args',kens(1)
        print *,'rlwdebug getgbece args',lb(1),f(1),iret
        print *,'rlwdebug getgbece args',ktype,kfnum,ktot

        call getgbece(lugb,lugi,jf,j,jpds,jgds,jens,
     &                          kf,k,kpds,kgds,kens,lb,f,iret,
     &                          ktype,kfnum,ktot)
        print *,'rlwdebug after first call getgbece',lugb,lugi
c
        j=k
        if (iret.eq.0) then
          print *,' '
          print *,'immediately after call to getgb, j= ',j
     &           ,' k= ',k,' iret= ',iret
          print *,' '
          call grange(kf,lb,f,dmin,dmax)
          print '(4i5,4i3,2x,a1,i3,3i5,2x,i7,2g12.4)'
     &            ,k,(kpds(i),i=5,11),'f',kpds(14),ktype,kfnum
     &            ,ktot,kf,dmin,dmax
        else
          print *,'!!! getgb IRET= ',iret,'   j= ',j
     &           ,' .... Continuing to next loop iteration ....'
          goto 600
        endif
 
        call adjpds (kpds,contflag,lugout,memberct)

        if (contflag.eq.'n') goto 600

      call grib_open_wa (lugout,ireto)
      if (ireto.gt.0) then
        print *,'ireto,lu from grib_open_wa in ecmwfens = ',ireto,lugout
      endif
 
        call adjext (kens,ktype,kfnum,kres)
 
c        print *,' '
c        write(*,71) (kpds(mm),mm=1,5) 
c        write(*,72) (kpds(mm),mm=6,10) 
c        write(*,73) (kpds(mm),mm=11,15) 
c        write(*,74) (kpds(mm),mm=16,20) 
c        write(*,75) (kpds(mm),mm=21,25) 
c        write(*,76) (kgds(mm),mm=1,5) 
c        write(*,77) (kgds(mm),mm=6,10) 
c        write(*,78) (kgds(mm),mm=11,15) 
c        write(*,79) (kgds(mm),mm=16,20) 
c        write(*,80) (kgds(mm),mm=21,22) 
c        write(*,81) f(1),f(2500),f(5000),f(7500),f(10512)

        kpds(23)=2

        if (kpds(5) .eq. 61) then
          if (gotprcppds .eq. 'n') then
            gotprcppds = 'y'
            do i = 1,25
              kholdprcppds(i) = kpds(i)
            enddo
          endif
          prcpct = prcpct + 1
          do ip = 1,kf
            f(ip) = f(ip) * 1000.0
            prcpvals(prcpct,ip) = prcpvals(prcpct,ip) + f(ip)
          enddo
        else if (kpds(5) .eq. 7) then
         if (kpds(7) .eq. 1000) then
          if (gotz1000pds .eq. 'n') then
            gotz1000pds = 'y'
            do i = 1,25
              kholdz1000pds(i) = kpds(i)
            enddo 
          endif
          z1000ct = z1000ct + 1
          do ip = 1,kf
            z1000vals(z1000ct,ip) = z1000vals(z1000ct,ip) + f(ip)
          enddo
         else
          if (gotz500pds .eq. 'n') then
            gotz500pds = 'y'
            do i = 1,25
              kholdz500pds(i) = kpds(i)
            enddo 
          endif
          z500ct = z500ct + 1
          do ip = 1,kf
            z500vals(z500ct,ip) = z500vals(z500ct,ip) + f(ip)
          enddo
         endif
        else if (kpds(5) .eq. 2) then
          if (gotmslppds .eq. 'n') then
            gotmslppds = 'y'
            do i = 1,25
              kholdmslppds(i) = kpds(i)
            enddo 
          endif
          mslpct = mslpct + 1
          do ip = 1,kf
            mslpvals(mslpct,ip) = mslpvals(mslpct,ip) + f(ip)
          enddo
        else if (kpds(5) .eq. 11) then
          if (gott850pds .eq. 'n') then
            gott850pds = 'y'
            do i = 1,25
              kholdt850pds(i) = kpds(i)
            enddo
          endif
          t850ct = t850ct + 1
          do ip = 1,kf
            t850vals(t850ct,ip) = t850vals(t850ct,ip) + f(ip)
          enddo
        else if (kpds(5) .eq. 52) then
          if (gotrh700pds .eq. 'n') then
            gotrh700pds = 'y'
            do i = 1,25
              kholdrh700pds(i) = kpds(i)
            enddo
          endif
          rh700ct = rh700ct + 1
          do ip = 1,kf
            rh700vals(rh700ct,ip) = rh700vals(rh700ct,ip) + f(ip)
          enddo
        endif

        call output (lugout,kf,kpds,kgds,lb,f,kens)

ccs  additional statistics are commented out
ccs   if ((lugout.ne.53).and.(lugout.ne.63)) then
      call grib_close (lugout,ireto)
      if (ireto.gt.0) then
        print *,'ireto,lu from grib_close in ecmwfens = ',ireto,lugout
      endif
ccs   endif

 
 600    continue

      enddo
      call grib_close (lugb,ireto)
      if (ireto.gt.0) then
        print *,'ireto,lu from grib_close in ecmwfens = ',ireto,lugb
      endif
      call grib_close (lugi,ireto)
      if (ireto.gt.0) then
        print *,'ireto,lu from grib_close in ecmwfens = ',ireto,lugi
      endif

      call getstats (numec,prcpct,prcpvals,z500ct,z500vals
     &             ,z1000ct,z1000vals,rh700ct,rh700vals
     &             ,mslpct,mslpvals,t850ct,t850vals,nlat,nlon
     &             ,prcpmean,z500mean,t850mean,mslpmean,z1000mean
     &             ,rh700mean
     &             ,prcpspr,z500spr,t850spr,mslpspr,z1000spr
     &             ,rh700spr)

      call output_stats ('z500','mean',nlat*nlon,kholdz500pds,kgds
     &                  ,lb,z500mean)
      call output_stats ('mslp','mean',nlat*nlon,kholdmslppds,kgds
     &                  ,lb,mslpmean)
      call output_stats ('t850','mean',nlat*nlon,kholdt850pds,kgds
     &                  ,lb,t850mean)
      call output_stats ('prcp','mean',nlat*nlon,kholdprcppds,kgds
     &                  ,lb,prcpmean)
      call output_stats ('z1k ','mean',nlat*nlon,kholdz1000pds,kgds
     &                  ,lb,z1000mean)
      call output_stats ('r700','mean',nlat*nlon,kholdrh700pds,kgds
     &                  ,lb,rh700mean)
      call output_stats ('z500','spr ',nlat*nlon,kholdz500pds,kgds
     &                  ,lb,z500spr)
      call output_stats ('mslp','spr ',nlat*nlon,kholdmslppds,kgds
     &                  ,lb,mslpspr)
      call output_stats ('t850','spr ',nlat*nlon,kholdt850pds,kgds
     &                  ,lb,t850spr)
      call output_stats ('prcp','spr ',nlat*nlon,kholdprcppds,kgds
     &                  ,lb,prcpspr)
      call output_stats ('z1k ','spr ',nlat*nlon,kholdz1000pds,kgds
     &                  ,lb,z1000spr)
      call output_stats ('r700','spr ',nlat*nlon,kholdrh700pds,kgds
     &                  ,lb,rh700spr)
 
c     Now also write the mean out to the regular perturbation file with
c     a level value of "53", because my GrADS scripts are set up to read 
c     the mean as being level 53.  Only do this for z500 and mslp for
c     now, since those are the only 2 that I'm currently displaying 
c     with GrADS.

ccs  additional statistics are commented out
ccs   kholdz500pds(7) = 53
ccs   lugout          = 53

c     call grib_open_wa (lugout,ireto)
c     if (ireto.gt.0) then
c       print *,'ireto,lu from grib_open_wa in ecmwfens = ',
c    &     ireto,lugout
c     endif

ccs   call output (lugout,nlat*nlon,kholdz500pds,kgds,lb,z500mean,kens)

ccs   call grib_close (lugout,ireto)
ccs   if (ireto.gt.0) then
ccs     print *,'ireto,lu from grib_close in ecmwfens = ',ireto,lugout
ccs   endif

 
ccs   kholdmslppds(6) = 100
ccs   kholdmslppds(7) = 53
ccs   lugout          = 63

c     call grib_open_wa (lugout,ireto)
c     if (ireto.gt.0) then
c       print *,'ireto,lu from grib_open_wa in ecmwfens = ',
c    &     ireto,lugout
c     endif

ccs   call output (lugout,nlat*nlon,kholdmslppds,kgds,lb,mslpmean,kens)

ccs   call grib_close (lugout,ireto)
ccs   if (ireto.gt.0) then
ccs     print *,'ireto,lu from grib_close in ecmwfens = ',ireto,lugout
ccs   endif

 
 71   format('p1=  ',i7,' p2=  ',i7,' p3=  ',i7,' p4=  ',i7,' p5=  ',i7)
 72   format('p6=  ',i7,' p7=  ',i7,' p8=  ',i7,' p9=  ',i7,' p10= ',i7)
 73   format('p11= ',i7,' p12= ',i7,' p13= ',i7,' p14= ',i7,' p15= ',i7)
 74   format('p16= ',i7,' p17= ',i7,' p18= ',i7,' p19= ',i7,' p20= ',i7)
 75   format('p21= ',i7,' p22= ',i7,' p23= ',i7,' p24= ',i7,' p25= ',i7)
 76   format('g1=  ',i7,' g2=  ',i7,' g3=  ',i7,' g4=  ',i7,' g5=  ',i7)
 77   format('g6=  ',i7,' g7=  ',i7,' g8=  ',i7,' g9=  ',i7,' g10= ',i7)
 78   format('g11= ',i7,' g12= ',i7,' g13= ',i7,' g14= ',i7,' g15= ',i7)
 79   format('g16= ',i7,' g17= ',i7,' g18= ',i7,' g19= ',i7,' g20= ',i7)
 80   format('g21= ',i7,' g22= ',i7)
 81   format('f1= ',g12.4,' f2500= ',g12.4,' f5000= ',g12.4
     &      ,' f7500= ',g12.4,' f10512= ',g12.4)
c
 700  continue
      call w3tage('ECMWFENS')
      stop
      end
c
c----------------------------------------------------------------------c
c----------------------------------------------------------------------c
c
      subroutine getstats (numec,prcpct,prcpvals,z500ct,z500vals
     &                ,z1000ct,z1000vals,rh700ct,rh700vals
     &                ,mslpct,mslpvals,t850ct,t850vals,nlat,nlon
     &                ,prcpmean,z500mean,t850mean,mslpmean,z1000mean
     &                ,rh700mean
     &                ,prcpspr,z500spr,t850spr,mslpspr,z1000spr
     &                ,rh700spr)
c$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    getstats
C   PRGMMR: WOBUS            ORG: NP20        DATE: 2001-01-16
C
C ABSTRACT: START ABSTRACT HERE AND INDENT TO COLUMN 5 ON THE
C   FOLLOWING LINES.  PLEASE PROVIDE A BRIEF DESCRIPTION OF
C   WHAT THE SUBPROGRAM DOES.
C
C PROGRAM HISTORY LOG:
C   97-01-17  MARCHOK     original program
C   01-01-16  WOBUS       added DOCBLOCK
C
C USAGE:    CALL PROGRAM-NAME(INARG1, INARG2, WRKARG, OUTARG1, ... )
C   INPUT ARGUMENT LIST:
C     INARG1   - GENERIC DESCRIPTION, INCLUDING CONTENT, UNITS,
C     INARG2   - TYPE.  EXPLAIN FUNCTION IF CONTROL VARIABLE.
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     WRKARG   - GENERIC DESCRIPTION, ETC., AS ABOVE.
C     OUTARG1  - EXPLAIN COMPLETELY IF ERROR RETURN
C     ERRFLAG  - EVEN IF MANY LINES ARE NEEDED
C
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)
C
C   OUTPUT FILES:  (DELETE IF NO OUTPUT FILES IN SUBPROGRAM)
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS
C   MACHINE:  IBM SP
C
C$$$
c
      real      z500vals(numec,nlat*nlon),z1000vals(numec,nlat*nlon)
      real      t850vals(numec,nlat*nlon),rh700vals(numec,nlat*nlon)
      real      prcpvals(numec,nlat*nlon),mslpvals(numec,nlat*nlon)
      real      z500mean(nlat*nlon),z1000mean(nlat*nlon)
      real      t850mean(nlat*nlon),rh700mean(nlat*nlon)
      real      prcpmean(nlat*nlon),mslpmean(nlat*nlon)
      real      z500spr(nlat*nlon),z1000spr(nlat*nlon)
      real      t850spr(nlat*nlon),rh700spr(nlat*nlon)
      real      prcpspr(nlat*nlon),mslpspr(nlat*nlon)
      integer   prcpct,z500ct,mslpct,t850ct,rh700ct,z1000ct
c
      npts = nlon * nlat
      z500mean  = 0.0
      z1000mean  = 0.0
      t850mean = 0.0
      rh700mean = 0.0
      prcpmean = 0.0
      mslpmean = 0.0

c     -----------------------------------------------------
c        PRECIPITATION MEAN & SPREAD
c     -----------------------------------------------------
 
      if (prcpct .gt. 0) then

c       sum up all precip values from all members at all points

        do imem = 1,prcpct
          do n = 1,npts
            prcpmean(n) = prcpmean(n) + prcpvals(imem,n)
          enddo
        enddo

c       calculate mean precip

        do n = 1,npts
          prcpmean(n) = prcpmean(n) / float(prcpct)
        enddo

c       calculate standard deviation of precip

        do n = 1,npts
          varnce = 0.0
          do imem = 1,prcpct
            xdiff    = prcpvals(imem,n) - prcpmean(n)
            xdiffsqr = xdiff * xdiff
            varnce   = varnce + xdiffsqr
          enddo
          prcpspr(n) = sqrt(varnce/imem)
        enddo

      else

        do n = 1,npts
          prcpmean(n) = -99.0
          prcpspr(n)  = -99.0
        enddo

      endif
          
c     -----------------------------------------------------
c        MSLP MEAN & SPREAD
c     -----------------------------------------------------

      if (mslpct .gt. 0) then

c       sum up all mslp values from all members at all points

        do imem = 1,mslpct
          do n = 1,npts
            mslpmean(n) = mslpmean(n) + mslpvals(imem,n)
          enddo
        enddo

c       calculate mean mslp

        do n = 1,npts
          mslpmean(n) = mslpmean(n) / float(mslpct)
        enddo

c       calculate standard deviation of mslp

        do n = 1,npts
          varnce = 0.0
          do imem = 1,mslpct
            xdiff    = mslpvals(imem,n) - mslpmean(n)
            xdiffsqr = xdiff * xdiff
            varnce   = varnce + xdiffsqr
          enddo
          mslpspr(n) = sqrt(varnce/imem)
        enddo

      else

        do n = 1,npts
          mslpmean(n) = -99.0
          mslpspr(n)  = -99.0
        enddo

      endif

c     -----------------------------------------------------
c        t850 MEAN & SPREAD
c     -----------------------------------------------------

      if (t850ct .gt. 0) then

c       sum up all t850 values from all members at all points

        do imem = 1,t850ct
          do n = 1,npts
            t850mean(n) = t850mean(n) + t850vals(imem,n)
          enddo
        enddo

c       calculate mean t850

        do n = 1,npts
          t850mean(n) = t850mean(n) / float(t850ct)
        enddo

c       calculate standard deviation of t850

        do n = 1,npts
          varnce = 0.0
          do imem = 1,t850ct
            xdiff    = t850vals(imem,n) - t850mean(n)
            xdiffsqr = xdiff * xdiff
            varnce   = varnce + xdiffsqr
          enddo
          t850spr(n) = sqrt(varnce/imem)
        enddo

      else

        do n = 1,npts
          t850mean(n) = -99.0
          t850spr(n)  = -99.0
        enddo

      endif

c     -----------------------------------------------------
c        rh700 MEAN & SPREAD
c     -----------------------------------------------------

      if (rh700ct .gt. 0) then

c       sum up all rh700 values from all members at all points

        do imem = 1,rh700ct
          do n = 1,npts
            rh700mean(n) = rh700mean(n) + rh700vals(imem,n)
          enddo
        enddo

c       calculate mean rh700

        do n = 1,npts
          rh700mean(n) = rh700mean(n) / float(rh700ct)
        enddo

c       calculate standard deviation of rh700

        do n = 1,npts
          varnce = 0.0
          do imem = 1,rh700ct
            xdiff    = rh700vals(imem,n) - rh700mean(n)
            xdiffsqr = xdiff * xdiff
            varnce   = varnce + xdiffsqr
          enddo
          rh700spr(n) = sqrt(varnce/imem)
        enddo

      else

        do n = 1,npts
          rh700mean(n) = -99.0
          rh700spr(n)  = -99.0
        enddo

      endif

c     -----------------------------------------------------
c        z500 MEAN & SPREAD
c     -----------------------------------------------------
          
      if (z500ct .gt. 0) then

c       sum up all z500 values from all members at all points

        do imem = 1,z500ct
          do n = 1,npts
            z500mean(n) = z500mean(n) + z500vals(imem,n)
          enddo
        enddo

c       calculate mean z500

        do n = 1,npts
          z500mean(n) = z500mean(n) / float(z500ct)
        enddo

c       calculate standard deviation of z500

        do n = 1,npts
          varnce = 0.0
          do imem = 1,z500ct
            xdiff    = z500vals(imem,n) - z500mean(n)
            xdiffsqr = xdiff * xdiff
            varnce   = varnce + xdiffsqr
          enddo
          z500spr(n) = sqrt(varnce/imem)
        enddo

      else

        do n = 1,npts
          z500mean(n) = -99.0
          z500spr(n)  = -99.0
        enddo

      endif
c
c     -----------------------------------------------------
c        z1000 MEAN & SPREAD
c     -----------------------------------------------------
          
      if (z1000ct .gt. 0) then

c       sum up all z1000 values from all members at all points

        do imem = 1,z1000ct
          do n = 1,npts
            z1000mean(n) = z1000mean(n) + z1000vals(imem,n)
          enddo
        enddo

c       calculate mean z1000

        do n = 1,npts
          z1000mean(n) = z1000mean(n) / float(z1000ct)
        enddo

c       calculate standard deviation of z1000

        do n = 1,npts
          varnce = 0.0
          do imem = 1,z1000ct
            xdiff    = z1000vals(imem,n) - z1000mean(n)
            xdiffsqr = xdiff * xdiff
            varnce   = varnce + xdiffsqr
          enddo
          z1000spr(n) = sqrt(varnce/imem)
        enddo

      else

        do n = 1,npts
          z1000mean(n) = -99.0
          z1000spr(n)  = -99.0
        enddo

      endif
c
      return
      end
c
c----------------------------------------------------------------------c
c----------------------------------------------------------------------c
c
      subroutine adjpds (kpds,contflag,lugout,memberct)
c$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    adjpds
C   PRGMMR: WOBUS            ORG: NP20        DATE: 2001-01-16
C
C ABSTRACT: START ABSTRACT HERE AND INDENT TO COLUMN 5 ON THE
C   FOLLOWING LINES.  PLEASE PROVIDE A BRIEF DESCRIPTION OF
C   WHAT THE SUBPROGRAM DOES.
C
C PROGRAM HISTORY LOG:
C   97-01-17  MARCHOK     original program
C   01-01-16  WOBUS       added DOCBLOCK
C
C USAGE:    CALL PROGRAM-NAME(INARG1, INARG2, WRKARG, OUTARG1, ... )
C   INPUT ARGUMENT LIST:
C     INARG1   - GENERIC DESCRIPTION, INCLUDING CONTENT, UNITS,
C     INARG2   - TYPE.  EXPLAIN FUNCTION IF CONTROL VARIABLE.
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     WRKARG   - GENERIC DESCRIPTION, ETC., AS ABOVE.
C     OUTARG1  - EXPLAIN COMPLETELY IF ERROR RETURN
C     ERRFLAG  - EVEN IF MANY LINES ARE NEEDED
C
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)
C
C   OUTPUT FILES:  (DELETE IF NO OUTPUT FILES IN SUBPROGRAM)
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS
C   MACHINE:  IBM SP
C
C$$$
c
c     ****************************************************
c     ADJUST GRIB PARAMETER FROM THE ECMWF TABLE VALUES
c     TO NCEP TABLE VALUES AS FOLLOWS:
c
c       Parameter        ECMWF Grib Parm    NCEP Grib Parm
c     -------------      ---------------    --------------
c     u-comp (std lev)        131                33
c     v-comp (std lev)        132                34
c     gp height               156                 7
c     relative humidity       157                52
c     temperature             130                11
c     mslp                    151                 2
c     total precip            228                61
c
c     - Others, which currently (1/97) are in the ECMWF
c       data sets, but are not needed for this output data
c       set (they're needed by the Ocean Modeling Branch
c       and are processed in a different program):
c
c     u-comp (10m)            165                33
c     v-comp (10m)            166                34
c
c     -------------------------------------------------
c
c     ALSO, get the  output GRIB file number, which is 
c     based on the parameter number and vertical level.
c
c     contflag is needed because ECMWF sends a couple of 
c     additional records in their package which our Ocean
c     Modeling Branch uses, but which we do not archive,
c     so we don't want to output these to our GRIB files.
c
c     ****************************************************
c
      character*1 contflag
      integer     kpds(25),memberct(2)
c
      if (kpds(5).ne.131 .and. kpds(5).ne.132 .and.
     &    kpds(5).ne.156 .and. kpds(5).ne.157 .and.
     &    kpds(5).ne.130 .and. kpds(5).ne.151 .and.
     &    kpds(5).ne.228) then
        contflag='n'
        goto 900
      endif
c
c      if (kpds(5).eq.131 .or. kpds(5).eq.165) kpds(5) = 33
c      if (kpds(5).eq.132 .or. kpds(5).eq.166) kpds(5) = 34
c
      if (kpds(5).eq.130) then
        kpds(5) = 11
        if (kpds(7).eq.200) lugout = 60
        if (kpds(7).eq.500) lugout = 54
        if (kpds(7).eq.850) lugout = 57
      else if (kpds(5).eq.131) then
        kpds(5) = 33
        if (kpds(7).eq.200) lugout = 61
        if (kpds(7).eq.500) lugout = 55
        if (kpds(7).eq.850) lugout = 58
      else if (kpds(5).eq.132) then
        kpds(5) = 34
        if (kpds(7).eq.200) lugout = 62
        if (kpds(7).eq.500) lugout = 56
        if (kpds(7).eq.850) lugout = 59
 
c     This next bit of code is the kludgy way of adjusting
c     the height level to be 1-51 so that GrADS can plot
c     all the individual members for 500 mb height.
ccl  level labeling is commented out

      else if (kpds(5).eq.156) then
        kpds(5) = 7
        if (kpds(7).eq.1000) lugout = 52
        if (kpds(7).eq.500) then
          lugout = 53
ccl       memberct(1) = memberct(1) + 1
ccl       kpds(7) = memberct(1)
        endif
      else if (kpds(5).eq.157) then
        kpds(5) = 52
        lugout  = 51

c     This next bit of code is the kludgy way of adjusting
c     the height level to be 1-51 so that GrADS can plot
c     all the individual members for mslp.
ccl  level labeling is commented out

      else if (kpds(5).eq.151) then
        lugout  = 63
        kpds(5) = 2
c       kpds(6) = 100
ccl     memberct(2) = memberct(2) + 1
ccl     kpds(7) = memberct(2)

c     Now make adjustments for the precip GRIB PDS parms,
c     which ECMWF did not code correctly for accumulations.

      else if (kpds(5).eq.228) then
        kpds(5) = 61
        lugout  = 64
        kpds(13) = 1
        if (kpds(14).eq.0) then
          kpds(14) = 0
          kpds(15) = 0
        else
          kpds(14) = kpds(14) - 12
          kpds(15) = kpds(14) + 12
        endif
        kpds(16) = 4
        kpds(22) = 1
      endif
c
      kpds(19) = 2
c
      contflag='y'
c
 900  continue
      return
      end
c
c----------------------------------------------------------------------c
c----------------------------------------------------------------------c
c
      subroutine adjext (kens,ktype,kfnum,kres)
c$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    adjext
C   PRGMMR: WOBUS            ORG: NP20        DATE: 2001-01-16
C
C ABSTRACT: START ABSTRACT HERE AND INDENT TO COLUMN 5 ON THE
C   FOLLOWING LINES.  PLEASE PROVIDE A BRIEF DESCRIPTION OF
C   WHAT THE SUBPROGRAM DOES.
C
C PROGRAM HISTORY LOG:
C   97-01-17  MARCHOK     original program
C   01-01-16  WOBUS       added DOCBLOCK
C
C USAGE:    CALL PROGRAM-NAME(INARG1, INARG2, WRKARG, OUTARG1, ... )
C   INPUT ARGUMENT LIST:
C     INARG1   - GENERIC DESCRIPTION, INCLUDING CONTENT, UNITS,
C     INARG2   - TYPE.  EXPLAIN FUNCTION IF CONTROL VARIABLE.
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     WRKARG   - GENERIC DESCRIPTION, ETC., AS ABOVE.
C     OUTARG1  - EXPLAIN COMPLETELY IF ERROR RETURN
C     ERRFLAG  - EVEN IF MANY LINES ARE NEEDED
C
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)
C
C   OUTPUT FILES:  (DELETE IF NO OUTPUT FILES IN SUBPROGRAM)
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS
C   MACHINE:  IBM SP
C
C$$$
c
c     This subroutine takes the ECMWF ensemble PDS header
c     extension parameters and creates the corresponding 
c     NCEP header extension.
c
c     INPUT
c     -----
c     ktype       ECMWF flag; 10 = Control, 11 = Perturbed Fcst
c     kfnum       0 = CONTROL FORECAST, 1-nn = Perturbed Fcst, 
c                  odd number is positive pert,
C                  even number is for negative pert.
c     kres        1 - input file contains only high res control
c                 2 - input file contains low res control and
c                     perturbations 
c                 (Important to know since high res control 
c                  does NOT have a PDS extension)
c
c     OUTPUT
c     ------
c     kens        NCEP ensemble PDS extension (Bytes 41-45)
c
c
c
      integer   kens(5)
c
      kens=0
      kens(1)=1
c
      if (kres.eq.1) then
 
c       If kres=1 (this is information that is passed into this program
c       via a namelist), then you know that you are reading a file that
c       contains only HRC records, so give the current record an 
c       NCEP ensemble extension to indicate such.
 
        kens(2) = 1
        kens(3) = 1

      else

c       If kres=2 and the ktype=10, then you know that you're reading
c       a LRC record from one of the "USE" files.  As such, give it an
c       NCEP LRC designation.  If ktype=11, then you're reading one of
c       the perturbation records.

        if (ktype.eq.10) then
          kens(2) = 1
          kens(3) = 2
        else
          if (mod(kfnum,2).gt.0) then
            kens(2) = 3
          else
            kens(2) = 2
          end if
        end if

      end if
c
c     CONSECUTIVELY NUMBERED ECMWF FORECASTS MAKE UP A NEGATIVELY
c     AND POSITIVELY PERTURBED PAIR.  THIS NEXT BIT OF CODE
c     ASSOCIATES AN ID NUMBER TO A MEMBER FROM EACH PAIR.
c
      if (kres.eq.2 .and. ktype.eq.11) then
        kens(3) = (kfnum + 1) / 2
      endif
c
c     SET NMCEXT ARRAY MEMBERS 4 AND 5 EQUAL TO 1 AND 255.
c
 400  continue
c
      kens(4) = 1
      kens(5) = 255
c
      return 
      end    
c
c----------------------------------------------------------------------c
c----------------------------------------------------------------------c
c
      subroutine output (lugout,kf,kpds,kgds,ld,data,kens)
c$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    output
C   PRGMMR: WOBUS            ORG: NP20        DATE: 2001-01-16
C
C ABSTRACT: START ABSTRACT HERE AND INDENT TO COLUMN 5 ON THE
C   FOLLOWING LINES.  PLEASE PROVIDE A BRIEF DESCRIPTION OF
C   WHAT THE SUBPROGRAM DOES.
C
C PROGRAM HISTORY LOG:
C   97-01-17  MARCHOK     original program
C   01-01-16  WOBUS       added DOCBLOCK
C
C USAGE:    CALL PROGRAM-NAME(INARG1, INARG2, WRKARG, OUTARG1, ... )
C   INPUT ARGUMENT LIST:
C     INARG1   - GENERIC DESCRIPTION, INCLUDING CONTENT, UNITS,
C     INARG2   - TYPE.  EXPLAIN FUNCTION IF CONTROL VARIABLE.
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     WRKARG   - GENERIC DESCRIPTION, ETC., AS ABOVE.
C     OUTARG1  - EXPLAIN COMPLETELY IF ERROR RETURN
C     ERRFLAG  - EVEN IF MANY LINES ARE NEEDED
C
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)
C
C   OUTPUT FILES:  (DELETE IF NO OUTPUT FILES IN SUBPROGRAM)
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS
C   MACHINE:  IBM SP
C
C$$$
c
      integer   kpds(200),kgds(200),kens(200)
      logical   ld(kf)
      real      data(kf)
c
c     ****************************
c      WRITE GRIB FILE
c     ****************************
c
      print *,'at beginning of output, lugout= ',lugout,' kf= ',kf
      print *,' '
      call putgbe (lugout,kf,kpds,kgds,kens,ld,data,iret)
 
      if (iret.eq.0) then
        print *,' '
        print *,'IRET = 0 after call to putgbe'
        print *,' '
      else
        print *,' '
        print *,'!!! ERROR: IRET NE 0 AFTER CALL TO PUTGBE !!!'
        print *,' '
      endif
c
      return 
      end
c
c----------------------------------------------------------------------c
c----------------------------------------------------------------------c
c
      subroutine output_stats (cparm,ctype,kf,kpds,kgds,ld,data)
c$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    output_stats
C   PRGMMR: WOBUS            ORG: NP20        DATE: 2001-01-16
C
C ABSTRACT: START ABSTRACT HERE AND INDENT TO COLUMN 5 ON THE
C   FOLLOWING LINES.  PLEASE PROVIDE A BRIEF DESCRIPTION OF
C   WHAT THE SUBPROGRAM DOES.
C
C PROGRAM HISTORY LOG:
C   97-01-17  MARCHOK     original program
C   01-01-16  WOBUS       added DOCBLOCK
C
C USAGE:    CALL PROGRAM-NAME(INARG1, INARG2, WRKARG, OUTARG1, ... )
C   INPUT ARGUMENT LIST:
C     INARG1   - GENERIC DESCRIPTION, INCLUDING CONTENT, UNITS,
C     INARG2   - TYPE.  EXPLAIN FUNCTION IF CONTROL VARIABLE.
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     WRKARG   - GENERIC DESCRIPTION, ETC., AS ABOVE.
C     OUTARG1  - EXPLAIN COMPLETELY IF ERROR RETURN
C     ERRFLAG  - EVEN IF MANY LINES ARE NEEDED
C
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)
C
C   OUTPUT FILES:  (DELETE IF NO OUTPUT FILES IN SUBPROGRAM)
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS
C   MACHINE:  IBM SP
C
C$$$
c
      integer   kpds(200),kgds(200),kens(200)
      logical   ld(kf)
      real      data(kf)
      character cparm*4,ctype*4

      ld = .TRUE.

      if (cparm .eq. 'prcp') then
        kpds(5) = 61
        kpds(6) = 1
        kpds(7) = 0

C        THIS NEXT STUFF IS ALREADY DONE IN ADJPDS, 
C        SO COMMENT IT OUT HERE....
C        if (kpds(14).eq.0) then
C          kpds(14) = 0
C          kpds(15) = 0
C        else
C          kpds(14) = kpds(14) - 12
C          kpds(15) = kpds(14) + 12
C        endif

        kpds(16) = 4
        kpds(22) = 1
        if (ctype .eq. 'mean') lugout = 71
        if (ctype .eq. 'spr ') lugout = 81
      else if (cparm .eq. 'mslp') then
        kpds(5)  = 2
        kpds(6)  = 102
        kpds(7)  = 0
        if (ctype .eq. 'mean') then
          kpds(22) = 0
          lugout = 72
        else if (ctype .eq. 'spr ') then 
          kpds(22) = 1
          lugout = 82
        endif
      else if (cparm .eq. 't850') then
        kpds(5)  = 11
        kpds(6)  = 100
        kpds(7)  = 850
        kpds(22) = 1
        if (ctype .eq. 'mean') lugout = 73
        if (ctype .eq. 'spr ') lugout = 83
      else if (cparm .eq. 'r700') then
        kpds(5)  = 52
        kpds(6)  = 100
        kpds(7)  = 700
        kpds(22) = 1
        if (ctype .eq. 'mean') lugout = 76
        if (ctype .eq. 'spr ') lugout = 86
      else if (cparm .eq. 'z500') then
        kpds(5)  = 7
        kpds(6)  = 100
        kpds(7)  = 500
        if (ctype .eq. 'mean') then
          kpds(22) = 0
          lugout = 74
        else if (ctype .eq. 'spr ') then
          kpds(22) = 1
          lugout = 84
        endif
      else if (cparm .eq. 'z1k ') then
        kpds(5)  = 7
        kpds(6)  = 100
        kpds(7)  = 1000
        if (ctype .eq. 'mean') then
          kpds(22) = 0
          lugout = 75
        else if (ctype .eq. 'spr ') then
          kpds(22) = 1
          lugout = 85
        endif
      endif
      kens(1)=1
      kens(2)=5
      kens(3)=0
      kens(4)=0
      kens(5)=255
      if (ctype .eq. 'mean') then
        kens(4)=1
      else if (ctype .eq. 'spr ') then
        kens(4)=11
      endif

c     ****************************
c      WRITE GRIB FILE
c     ****************************

      print *,'In output_stats, lugout= ',lugout,' kf= ',kf
      print *,'In output_stats, cparm= ',cparm,' ctype= ',ctype
      print *,' '

      print *,' '
      write(*,71) (kpds(mm),mm=1,5)
      write(*,72) (kpds(mm),mm=6,10)
      write(*,73) (kpds(mm),mm=11,15)
      write(*,74) (kpds(mm),mm=16,20)
      write(*,75) (kpds(mm),mm=21,25)
      write(*,76) (kgds(mm),mm=1,5)
      write(*,77) (kgds(mm),mm=6,10)
      write(*,78) (kgds(mm),mm=11,15)
      write(*,79) (kgds(mm),mm=16,20)
      write(*,80) (kgds(mm),mm=21,22)
      write(*,81) data(1),data(2500),data(5000),data(7500),data(10512)
      print *,' '

 71   format('p1=  ',i7,' p2=  ',i7,' p3=  ',i7,' p4=  ',i7,' p5=  ',i7)
 72   format('p6=  ',i7,' p7=  ',i7,' p8=  ',i7,' p9=  ',i7,' p10= ',i7)
 73   format('p11= ',i7,' p12= ',i7,' p13= ',i7,' p14= ',i7,' p15= ',i7)
 74   format('p16= ',i7,' p17= ',i7,' p18= ',i7,' p19= ',i7,' p20= ',i7)
 75   format('p21= ',i7,' p22= ',i7,' p23= ',i7,' p24= ',i7,' p25= ',i7)
 76   format('g1=  ',i7,' g2=  ',i7,' g3=  ',i7,' g4=  ',i7,' g5=  ',i7)
 77   format('g6=  ',i7,' g7=  ',i7,' g8=  ',i7,' g9=  ',i7,' g10= ',i7)
 78   format('g11= ',i7,' g12= ',i7,' g13= ',i7,' g14= ',i7,' g15= ',i7)
 79   format('g16= ',i7,' g17= ',i7,' g18= ',i7,' g19= ',i7,' g20= ',i7)
 80   format('g21= ',i7,' g22= ',i7)
 81   format('f1= ',g12.4,' f2500= ',g12.4,' f5000= ',g12.4
     &      ,' f7500= ',g12.4,' f10512= ',g12.4)

      call grib_open_wa (lugout,ireto)
      if (ireto.gt.0) then
        print *,'ireto,lu from grib_open_wa in output_stats = ',
     ,     ireto,lugout
      endif

      call putgbe (lugout,kf,kpds,kgds,kens,ld,data,iret)

      call grib_close (lugout,ireto)
      if (ireto.gt.0) then
        print *,'ireto,lu from grib_close in output_stats = ',
     ,     ireto,lugout
      endif

      if (iret.eq.0) then
        print *,' '
        print *,'IRET = 0 after call to putgbe in sub output_stats'
        print *,' '
      else
        print *,' '
        print *,'!!! ERROR: IRET NE 0 AFTER PUTGBE IN OUTPUT_STATS!!!'
        print *,' '
      endif
c
      return
      end
c
c----------------------------------------------------------------------c
c----------------------------------------------------------------------c
c
      subroutine grange(n,ld,d,dmin,dmax)
c$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    GRANGE
C   PRGMMR: WOBUS            ORG: NP20        DATE: 2001-01-16
C
C ABSTRACT: Calculate the maximum and minimum values in an array
C
C PROGRAM HISTORY LOG:
C   97-01-17  MARCHOK     original program
C   01-01-16  WOBUS       added DOCBLOCK
C
C USAGE:    CALL PROGRAM-NAME(INARG1, INARG2, WRKARG, OUTARG1, ... )
C   INPUT ARGUMENT LIST:
c     n        - dimension of the array
c     ld       - logical array (bit map)
c     d        - array
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
c     dmin     - minimum value in array d
c     dmin     - maximum value in array d
C
C ATTRIBUTES:
C   MACHINE:  IBM SP
C
C$$$
      logical ld
      dimension ld(n),d(n)
c
      dmin=1.e30
      dmax=-1.e30
c
      do i=1,n
        if(ld(i)) then
          dmin=min(dmin,d(i))
          dmax=max(dmax,d(i))
        endif
      enddo
c
      return
      end
c
c----------------------------------------------------------------------c
c----------------------------------------------------------------------c
c
      SUBROUTINE GETGBECE(LUGB,LUGI,JF,J,JPDS,JGDS,JENS,
     &                              KF,K,KPDS,KGDS,KENS,LB,F,IRET,
     &                              ktype,kfnum,ktot)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: GETGBECE       FINDS AND UNPACKS A GRIB MESSAGE
C   PRGMMR: WOBUS            ORG: NP20        DATE: 2001-01-16
C
C ABSTRACT: FIND AND UNPACK A GRIB MESSAGE, ECMWF ENSEMBLE VERSION.
C   READ AN ASSOCIATED GRIB INDEX FILE (UNLESS IT ALREADY WAS READ).
C   FIND IN THE INDEX FILE A REFERENCE TO THE GRIB MESSAGE REQUESTED.
C   THE GRIB MESSAGE REQUEST SPECIFIES THE NUMBER OF MESSAGES TO SKIP
C   AND THE UNPACKED PDS AND GDS PARAMETERS.  (A REQUESTED PARAMETER
C   OF -1 MEANS TO ALLOW ANY VALUE OF THIS PARAMETER TO BE FOUND.)
C   IF THE REQUESTED GRIB MESSAGE IS FOUND, THEN IT IS READ FROM THE
C   GRIB FILE AND UNPACKED.  ITS MESSAGE NUMBER IS RETURNED ALONG WITH
C   THE UNPACKED PDS AND GDS PARAMETERS, THE UNPACKED BITMAP (IF ANY),
C   AND THE UNPACKED DATA.  IF THE GRIB MESSAGE IS NOT FOUND, THEN THE
C   RETURN CODE WILL BE NONZERO.
C
C PROGRAM HISTORY LOG:
C   94-04-01  IREDELL
C   97-01-17  MARCHOK - modified version for ECMWF ensemble GRIB ext.
C   01-01-16  WOBUS   - renamed and updated DOCBLOCK
C
C USAGE:    CALL GETGBECE(LUGB,LUGI,JF,J,JPDS,JGDS,JENS,
C    &                              KF,K,KPDS,KGDS,KENS,LB,F,IRET)
C   INPUT ARGUMENTS:
C     LUGB         LOGICAL UNIT OF THE UNBLOCKED GRIB DATA FILE
C     LUGI         LOGICAL UNIT OF THE UNBLOCKED GRIB INDEX FILE
C     JF           INTEGER MAXIMUM NUMBER OF DATA POINTS TO UNPACK
C     J            INTEGER NUMBER OF MESSAGES TO SKIP
C                  (=0 TO SEARCH FROM BEGINNING)
C                  (<0 TO REOPEN INDEX FILE AND SEARCH FROM BEGINNING)
C     JPDS         INTEGER (25) PDS PARAMETERS FOR WHICH TO SEARCH
C                  (=-1 FOR WILDCARD)
C     JGDS         INTEGER (22) GDS PARAMETERS FOR WHICH TO SEARCH
C                  (ONLY SEARCHED IF JPDS(3)=255)
C                  (=-1 FOR WILDCARD)
C     JENS         INTEGER (5) ENSEMBLE PDS PARMS FOR WHICH TO SEARCH
C                  (ONLY SEARCHED IF JPDS(23)=3)
C                  (=-1 FOR WILDCARD)
C   OUTPUT ARGUMENTS:
C     KF           INTEGER NUMBER OF DATA POINTS UNPACKED
C     K            INTEGER MESSAGE NUMBER UNPACKED
C                  (CAN BE SAME AS J IN CALLING PROGRAM
C                  IN ORDER TO FACILITATE MULTIPLE SEARCHES)
C     KPDS         INTEGER (25) UNPACKED PDS PARAMETERS
C     KGDS         INTEGER (22) UNPACKED GDS PARAMETERS
c
c
C     KENS         INTEGER (5) UNPACKED ENSEMBLE PDS PARMS
c
c     ***********  CODE ADDED FOR ECMWF ORIGINAL ENSEMBLE FILES  ****
c
c     ktype        10 = ECMWF control forecast
c                  11 = ECMWF perturbed forecast
c     kfnum        Ensemble Forecast Number;
c                  Control Forecast is number 0,
c                  perturbed forecast are 1-nn, where
c                  positive perturbation is an odd number,
c                  negative perturbation is an even number.
c     ktot         Total number of forecast in ensemble.
c                  This number includes the control forecast.
c
C     LB           LOGICAL (KF) UNPACKED BITMAP IF PRESENT
C     F            REAL (KF) UNPACKED DATA
C     IRET         INTEGER RETURN CODE
C                    0      ALL OK
C                    96     ERROR READING INDEX FILE
C                    97     ERROR READING GRIB FILE
C                    98     NUMBER OF DATA POINTS GREATER THAN JF
C                    99     REQUEST NOT FOUND
C                    OTHER  W3FI63 GRIB UNPACKER RETURN CODE
C
C SUBPROGRAMS CALLED:
C   BAopenr        open for BYTE-ADDRESSABLE READ, read-only
C   BAopen         open for BYTE-ADDRESSABLE READ
C   BAclose        close for BYTE-ADDRESSABLE READ
C   BAREAD         BYTE-ADDRESSABLE READ
C   GBYTE          UNPACK BYTES
C   FI632          UNPACK PDS
C   FI633          UNPACK GDS
C   PDSEUP         UNPACK PDS EXTENSION
C   W3FI63         UNPACK GRIB
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
c IMPORTANT NOTE: THIS GETGBENS SUBROUTINE HAS BEEN MODIFIED!!!  
C                 IT IS *NOT* THE SAME GETGBENS AS IS FOUND IN  
C                 /NWPROD/W3LIB.  MODIFICATIONS WERE MADE TO IT
C                 TO BE ABLE TO READ THE ECMWF PDS EXTENSION  
c  Modified getgbens has been renamed getgbece
c
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C   LANGUAGE: ibm FORTRAN
C
C$$$
      INTEGER JPDS(25),JGDS(22),KPDS(25),KGDS(22)
      PARAMETER(LPDS=23,LGDS=22)
      INTEGER JENS(5),KENS(5)
      LOGICAL LB(JF)
      REAL F(JF)
      PARAMETER(MBUF=8192*128)
      CHARACTER CBUF(MBUF)
      SAVE LUX,NSKP,NLEN,NNUM,CBUF
      DATA LUX/0/
      CHARACTER CHEAD(2)*81
      CHARACTER CPDS(80)*1,CGDS(42)*1
C     INTEGER KPTR(16)
      INTEGER KPTR(20)
      INTEGER IPDSP(LPDS),JPDSP(LPDS),IGDSP(LGDS),JGDSP(LGDS)
      INTEGER IENSP(5),JENSP(5)
      CHARACTER GRIB(200+17*JF/8)*1
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  READ INDEX FILE
      print *,'rlwdebug in begin getgbece',lugb,lugi
        print *,'rlwdebug getgbece args',jf,j,jpds(1),jgds(1)
        print *,'rlwdebug getgbece args',jens(1)
        print *,'rlwdebug getgbece args',kf,k,kpds(1),kgds(1)
        print *,'rlwdebug getgbece args',kens(1)
        print *,'rlwdebug getgbece args',lb(1),f(1),iret
        print *,'rlwdebug getgbece args',ktype,kfnum,ktot
      IF(J.LT.0.OR.LUGI.NE.LUX) THEN
        call grib_open_r (lugb,ireto)
        if (ireto.gt.0) then
          print *,'ireto,lu from grib_open_r in getgbece = ',ireto,lugb
        endif
        call grib_open_r (lugi,ireto)
        if (ireto.gt.0) then
          print *,'ireto,lu from grib_open_r in getgbece = ',ireto,lugi
        endif
        print *,' in getgbece:  units b,i: ',lugb,lugi
c       REWIND LUGI
c       READ(LUGI,IOSTAT=IOS) CHEAD
c             CALL BAREAD(LUGB,LSKIP,LGRIB,LREAD,GRIB)
        ios=-1
        nskp=0
        lgrib=81
        CALL BAREAD(LUGI,NSKP,LGRIB,LREAD,CHEAD(1))
         if ( lgrib.eq.lread ) ios=0
c        print *,'ios after chead read = ',ios
         print *,'chead 42-47 = ',CHEAD(1)(42:47)
         print *,'chead 38-43 = ',CHEAD(1)(38:43)
         print *,' nskp, lgrib, lread = ',nskp,lgrib,lread
        nskp=lread
        CALL BAREAD(LUGI,NSKP,LGRIB,LREAD,CHEAD(2))
         print *,' nskp, lgrib, lread = ',nskp,lgrib,lread
        IF(IOS.EQ.0.AND.CHEAD(1)(42:47).EQ.'GB1IX1') THEN
c       IF(IOS.EQ.0.AND.CHEAD(1)(38:43).EQ.'GB1IX1') THEN
          LUX=0
         READ(CHEAD(2),'(8X,3I10,2X,A40)',IOSTAT=IOS) NSKP,NLEN,NNUM
           print *,'nlen= ',nlen,' ios= ',ios,' nskp= ',nskp,' nnum= '
     &           ,nnum
      print *,'rlwdebug in getgbece after first nnum=',nnum
          IF(IOS.EQ.0) THEN
            NBUF=NNUM*NLEN
            IF(NBUF.GT.MBUF) THEN
              PRINT *,'GETGB: INCREASE BUFFER FROM ',MBUF,' TO ',NBUF
              NNUM=MBUF/NLEN
              NBUF=NNUM*NLEN
            ENDIF
            CALL BAREAD(LUGI,NSKP,NBUF,LBUF,CBUF)
            IF(LBUF.EQ.NBUF) THEN
c              print *,'************** lux being set equal to lugi'
              LUX=LUGI
              J=MAX(J,0)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      print *,'rlwdebug in getgbece before search for request'
C  SEARCH FOR REQUEST
      KENS=0
      LGRIB=0
      KJ=J
      K=J
      KF=0
      print *,'rlwdebug in getgbece before if',j,lugi,lux
      IF(J.GE.0.AND.LUGI.EQ.LUX) THEN
        print *,'rlwdebug in getgbece in begin if',j,lugi,lux
        LPDSP=0
        DO I=1,LPDS
          IF(JPDS(I).NE.-1) THEN
            LPDSP=LPDSP+1
            IPDSP(LPDSP)=I
            JPDSP(LPDSP)=JPDS(I)
          ENDIF
        ENDDO
        print *,'rlwdebug in getgbece in if after 1 loops'
        LGDSP=0
        IF(JPDS(3).EQ.255) THEN
          DO I=1,LGDS
            IF(JGDS(I).NE.-1) THEN
              LGDSP=LGDSP+1
              IGDSP(LGDSP)=I
              JGDSP(LGDSP)=JGDS(I)
            ENDIF
          ENDDO
        ENDIF
        print *,'rlwdebug in getgbece in if after 2 loops'
        LENSP=0
        IF(JPDS(23).EQ.3 .or. jpds(23).eq.0.or.jpds(23).eq.-1) THEN
          DO I=1,5
            IF(JENS(I).NE.-1) THEN
              LENSP=LENSP+1
              IENSP(LENSP)=I
              JENSP(LENSP)=JENS(I)
            ENDIF
          ENDDO
        else
          print *,'!!! jpds(23) != 0 or 3, jpds(23)= ',jpds(23)
        ENDIF
        IRET=99
        print *,'rlwdebug in getgbece in if after 3 loops'
        DOWHILE(LGRIB.EQ.0.AND.KJ.LT.NNUM)
          print *,'rlwdebug in getgbece in dowhile begin',lgrib,kj,nnum
          KJ=KJ+1
          LT=0
          print *,'rlwdebug in getgbece in dowhile before if',lpdsp
          IF(LPDSP.GT.0) THEN
            print *,'rlwdebug in getgbece in dowhile in if',lpdsp
            CPDS=CBUF((KJ-1)*NLEN+26:(KJ-1)*NLEN+53)
            print *,'rlwdebug in dw in if after set cpds',cpds(1)
            KPTR=0
            print *,'rlwdebug in dw in if after set kptr',kptr(1)
crlw replace gbyte with gbytec to process character array
c           CALL GBYTE(CBUF,KPTR(3),(KJ-1)*NLEN*8+25*8,3*8)
            CALL GBYTEC(CBUF,KPTR(3),(KJ-1)*NLEN*8+25*8,3*8)
            print *,'rlwdebug in dw in if after call gbyte',cbuf(1:10)
            print *,'rlwdebug in dw in if before call fi632',cpds
            print *,'rlwdebug in dw in if before call fi632',kptr
            print *,'rlwdebug in dw in if before call fi632',kpds
            print *,'rlwdebug in dw in if before call fi632',iret
            CALL FI632(CPDS,KPTR,KPDS,IRET)
            print *,'rlwdebug in dw in if after call fi632',cpds(1)
c            print *, 'after fi632, iret=',iret
            print *,'rlwdebug in getgbece in dowhile before loop',lpdsp
            DO I=1,LPDSP
              IP=IPDSP(I)
              LT=LT+ABS(JPDS(IP)-KPDS(IP))
            ENDDO
            print *,'rlwdebug in getgbece in dowhile after loop'
          ENDIF
          print *,'rlwdebug in getgbece in dowhile after 1 loop'
          IF(LT.EQ.0.AND.LGDSP.GT.0) THEN
            CGDS=CBUF((KJ-1)*NLEN+54:(KJ-1)*NLEN+95)
            KPTR=0
            CALL FI633(CGDS,KPTR,KGDS,IRET)
c            print *, 'after fi633, iret=',iret
            DO I=1,LGDSP
              IP=IGDSP(I)
              LT=LT+ABS(JGDS(IP)-KGDS(IP))
            ENDDO
          ENDIF
          print *,'rlwdebug in getgbece in dowhile after 2 loops'
c          print *, 'lt=',lt,'lensp=',lensp
          IF(LT.EQ.0.AND.LENSP.GT.0) THEN
            CPDS(41:80)=CBUF((KJ-1)*NLEN+113:(KJ-1)*NLEN+152)
c           CALL PDSEUP(KENS,KPROB,XPROB,KCLUST,KMEMBR,45,CPDS)
            CALL ecmext(ktype,kfnum,ktot,45,CPDS)
            DO I=1,LENSP
              IP=IENSP(I)
              LT=LT+ABS(JENS(IP)-KENS(IP))
            ENDDO
          ENDIF
          print *,'rlwdebug in getgbece in dowhile after 3 loops'
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      print *,'rlwdebug in getgbece before read and unpack'
C  READ AND UNPACK GRIB DATA
          IF(LT.EQ.0) THEN
c           CALL GBYTE(CBUF,LSKIP,(KJ-1)*NLEN*8,4*8)
            CALL GBYTEC(CBUF,LSKIP,(KJ-1)*NLEN*8,4*8)
c           CALL GBYTE(CBUF,LGRIB,(KJ-1)*NLEN*8+20*8,4*8)
            CALL GBYTEC(CBUF,LGRIB,(KJ-1)*NLEN*8+20*8,4*8)
            CGDS=CBUF((KJ-1)*NLEN+54:(KJ-1)*NLEN+95)
            KPTR=0
            CALL FI633(CGDS,KPTR,KGDS,IRET)
c           print *, 'after FI633, iret=',iret
            IF(KPDS(23).EQ.3 .or. kpds(23).eq.0.or.kpds(23).eq.-1) THEN
              CPDS(41:80)=CBUF((KJ-1)*NLEN+113:(KJ-1)*NLEN+152)
c              CALL PDSEUP(KENS,KPROB,XPROB,KCLUST,KMEMBR,45,CPDS)
              CALL ecmext(ktype,kfnum,ktot,45,CPDS)
            else
              print *,'!!! kpds(23) != 0 or 3, kpds(23)= ',kpds(23)
            ENDIF
            IF(LGRIB.LE.200+17*JF/8.AND.KGDS(2)*KGDS(3).LE.JF) THEN
              CALL BAREAD(LUGB,LSKIP,LGRIB,LREAD,GRIB)
              IF(LREAD.EQ.LGRIB) THEN
                CALL W3FI63(GRIB,KPDS,KGDS,LB,F,KPTR,IRET)
c               print *, 'after W3FI63, iret=',iret
                IF(IRET.EQ.0) THEN
                  K=KJ
                  KF=KPTR(10)
                ENDIF
              ELSE
                IRET=97
              ENDIF
            ELSE
              IRET=98
            ENDIF
          ENDIF
        ENDDO
      ELSE
        IRET=96
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      print *,'rlwdebug in end getgbece',lugb,lugi
      RETURN
      END
c
C----------------------------------------------------------------------c
C----------------------------------------------------------------------c
c
      SUBROUTINE ecmext(ktype,kfnum,ktot,ILAST,MSGA)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    ecmext.f    UNPACKS GRIB PDS EXTENSION 41- FOR ENSEMBLE
C   PRGMMR: WOBUS            ORG: NP20        DATE: 2001-01-16
C
C ABSTRACT: unpacks GRIB pds extension starting on byte 41 for ECMWF
c           ensemble files.  NOTE that this extension format is 
c           completely different from NCEP's extension format, and
c           this subroutine will not work if you try to read NCEP 
c           ensemble files.  This subroutine will unpack bytes 
c           41-52 of the pds header extension.
C
C PROGRAM HISTORY LOG:
c   97-01-17  Tim Marchok (Most of the code, however, is taken from 
c                          the pdseup.f subroutine, originally written
c                          by Mark Iredell and Zoltan Toth).
C   97-01-17  MARCHOK     original program
C   01-01-16  WOBUS       added DOCBLOCK
c   
C
C USAGE:    call ecmext(ktype,kfnum,ktot,ILAST,MSGA)
C   INPUT ARGUMENT LIST:
C     ILAST    - LAST BYTE TO BE UNPACKED (IF GREATER/EQUAL TO FIRST BYT
C                IN ANY OF FOUR SECTIONS BELOW, WHOLE SECTION IS PACKED.
C     MSGA     - FULL PDS SECTION, INCLUDING NEW ENSEMBLE EXTENSION
C
C   OUTPUT ARGUMENT LIST:      
c
c     *********  ECMWF PDS EXTENSION BYTE LIST  ****************
c
c     ludn       Byte 41 (Local Use Definition Number; should = 1)
c     kclass     Byte 42 (1=Operations; 2=Research)
c     ktype      Byte 43 (10=Control Fcst; 11=Perturbed Forecast)
c     kstream    Bytes 44-45 (1035=Ensemble Forecasts)
c     kver       Bytes 46-49 (Version Number/Experiment Identifier;
c                             4 ascii characters, right justified)
c     kfnum      Byte 50 (Ensemble Forecast Number;
c                         Control Forecast is number 0,
c                         perturbed forecast are 1-nn, where
c                         positive perturbation is an odd number,
c                         negative perturbation is an even number.
c     ktot       Byte 51 (Total number of forecasts in ensemble.
c                         This number includeds the control forecast).
c     -----      Byte 52 (Reserved, should be set to 0).
c
C
C REMARKS: USE PDSENS.F FOR PACKING PDS ENSEMBLE EXTENSION.
C
C ATTRIBUTES:
C   LANGUAGE: CF77 FORTRAN
C   MACHINE:  CRAY, WORKSTATIONS
C
C$$$
C
      INTEGER KENS(5),KPROB(2),KCLUST(16),KMEMBR(80)
      integer ludn,kclass,ktype,kstream,kver,kfnum,ktot
      DIMENSION XPROB(2)
      CHARACTER*1 MSGA(100)
      character*1 cver(4)
c
C     CHECKING TOTAL NUMBER OF BYTES IN PDS (IBYTES)
c      print *,' '
c     CALL GBYTE(MSGA, IBYTES, 0,24)
      CALL GBYTEC(MSGA, IBYTES, 0,24)
c      PRINT *,'IBYTES (length of pds) = ',IBYTES
      IF (ILAST.GT.IBYTES) THEN
C       ILAST=IBYTES
        PRINT *,'ERROR - THERE ARE ONLY ',IBYTES, ' BYTES IN THE PDS.'
        GO TO 333
      ENDIF
      IF (ILAST.LT.41) THEN
        PRINT *,'WARNING - SUBROUTINE FOR UNPACKING BYTES 41 AND ABOVE'
        GO TO 333
      ENDIF
C     UNPACKING FIRST SECTION (GENERAL INFORMATION)
c
c     CALL GBYTE(MSGA,ludn,40*8,8)
      CALL GBYTEC(MSGA,ludn,40*8,8)
c      print *,'ludn= ',ludn
c     CALL GBYTE(MSGA,kclass,41*8,8)
      CALL GBYTEC(MSGA,kclass,41*8,8)
c      print *,'kclass= ',kclass
c     CALL GBYTE(MSGA,ktype,42*8,8)
      CALL GBYTEC(MSGA,ktype,42*8,8)
c      print *,'ktype= ',ktype
c     CALL GBYTE(MSGA,kstream,43*8,16)
      CALL GBYTEC(MSGA,kstream,43*8,16)
c      print *,'kstream= ',kstream
c      CALL GBYTE(MSGA,kver,45*8,32)
      do ii=1,4
        cver(ii) = msga(ii+45)
      enddo
c      print '(17a,3x,4a1)','Version Number = ',cver
c     CALL GBYTE(MSGA,kfnum,49*8,8)
      CALL GBYTEC(MSGA,kfnum,49*8,8)
c      print *,'kfnum= ',kfnum
c     CALL GBYTE(MSGA,ktot,50*8,8)
      CALL GBYTEC(MSGA,ktot,50*8,8)
c      print *,'ktot= ',ktot
c     CALL GBYTE(MSGA,junk,51*8,8)
      CALL GBYTEC(MSGA,junk,51*8,8)
c      print *,'Byte 52= ',junk
c
c    &             ,' str=',kstream,' ver=',kver,' mem=',kfnum
c      print '(7(a6,i6))','  lu=',ludn,' cls=',kclass,' typ=',ktype
c     &             ,' str=',kstream,' mem=',kfnum
c     &             ,' tot=',ktot,' b52=',junk
      goto 333
C
 333  CONTINUE
      RETURN
      END
c
c----------------------------------------------------------------------c
c----------------------------------------------------------------------c
c
      subroutine grib_close (lug,iret)
c$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    grib_close
C   PRGMMR: WOBUS            ORG: NP20        DATE: 2001-01-16
C
C ABSTRACT: START ABSTRACT HERE AND INDENT TO COLUMN 5 ON THE
C   FOLLOWING LINES.  PLEASE PROVIDE A BRIEF DESCRIPTION OF
C   WHAT THE SUBPROGRAM DOES.
C
C PROGRAM HISTORY LOG:
C   97-01-17  MARCHOK     original program
C   01-01-16  WOBUS       added DOCBLOCK
C
C USAGE:    CALL PROGRAM-NAME(INARG1, INARG2, WRKARG, OUTARG1, ... )
C   INPUT ARGUMENT LIST:
C     INARG1   - GENERIC DESCRIPTION, INCLUDING CONTENT, UNITS,
C     INARG2   - TYPE.  EXPLAIN FUNCTION IF CONTROL VARIABLE.
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     WRKARG   - GENERIC DESCRIPTION, ETC., AS ABOVE.
C     OUTARG1  - EXPLAIN COMPLETELY IF ERROR RETURN
C     ERRFLAG  - EVEN IF MANY LINES ARE NEEDED
C
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)
C
C   OUTPUT FILES:  (DELETE IF NO OUTPUT FILES IN SUBPROGRAM)
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS
C   MACHINE:  IBM SP
C
C$$$

C     ABSTRACT: This subroutine must be called before any attempt is
C     made to read from the input GRIB files.  The GRIB and index files
C     are opened with a call to baopenr.  This call to baopenr was not
C     needed in the cray version of this program (the files could be
C     opened with a simple Cray assign statement), but the GRIB-reading
C     utilities on the SP do require calls to this subroutine (it has
C     something to do with the GRIB I/O being done in C on the SP, and
C     the C I/O package needs an explicit open statement).
C
C     INPUT:
C     lug      The Fortran unit number for the GRIB file
C     OUTPUT:
C     iret     The return code from this subroutine

      character unitname*10
      character fname*80

      unitname(1:8) = "XLFUNIT_"
      write(unitname(9:10),'(I2)') lug
      call getenv(unitname,fname)
c     print *,' '
c     print *,' in grib_close:  unit: ',lug
c     print *,' in grib_close:  fname: ',fname
      ioret=0
      call baclose (lug,fname,ioret)

c     print *,' ' 
c     print *,'baclose: ioret= ',ioret

      iret=0
      if (ioret /= 0) then
        print *,' '
        print *,'!!! ERROR in grib_close closing grib file'
        print *,'!!! baclose return code = ioret = ',ioret
        iret = 93
        return
      endif

      return
      end
c
c----------------------------------------------------------------------c
c----------------------------------------------------------------------c
c
      subroutine grib_open (lug,iret)
c$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    grib_open
C   PRGMMR: WOBUS            ORG: NP20        DATE: 2001-01-16
C
C ABSTRACT: START ABSTRACT HERE AND INDENT TO COLUMN 5 ON THE
C   FOLLOWING LINES.  PLEASE PROVIDE A BRIEF DESCRIPTION OF
C   WHAT THE SUBPROGRAM DOES.
C
C PROGRAM HISTORY LOG:
C   97-01-17  MARCHOK     original program
C   01-01-16  WOBUS       added DOCBLOCK
C
C USAGE:    CALL PROGRAM-NAME(INARG1, INARG2, WRKARG, OUTARG1, ... )
C   INPUT ARGUMENT LIST:
C     INARG1   - GENERIC DESCRIPTION, INCLUDING CONTENT, UNITS,
C     INARG2   - TYPE.  EXPLAIN FUNCTION IF CONTROL VARIABLE.
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     WRKARG   - GENERIC DESCRIPTION, ETC., AS ABOVE.
C     OUTARG1  - EXPLAIN COMPLETELY IF ERROR RETURN
C     ERRFLAG  - EVEN IF MANY LINES ARE NEEDED
C
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)
C
C   OUTPUT FILES:  (DELETE IF NO OUTPUT FILES IN SUBPROGRAM)
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS
C   MACHINE:  IBM SP
C
C$$$

C     ABSTRACT: This subroutine must be called before any attempt is
C     made to access the GRIB file.  The GRIB file
C     is opened with a call to baopen.  This call to baopen was not
C     needed in the cray version of this program (the files could be
C     opened with a simple Cray assign statement), but the GRIB I/O
C     utilities on the SP do require calls to this subroutine (it has
C     something to do with the GRIB I/O being done in C on the SP, and
C     the C I/O package needs an explicit open statement).
C
C     INPUT:
C     lug      The Fortran unit number for the GRIB file
C     OUTPUT:
C     iret     The return code from this subroutine

      character unitname*10
      character fname*80

      unitname(1:8) = "XLFUNIT_"
      write(unitname(9:10),'(I2)') lug
      call getenv(unitname,fname)
c     print *,' '
c     print *,' in grib_open:  unit: ',lug
c     print *,' in grib_open:  fname: ',fname
      ioret=0
      call baopen (lug,fname,ioret)

c     print *,' ' 
c     print *,'baopen: ioret= ',ioret

      iret=0
      if (ioret /= 0) then
        print *,' '
        print *,'!!! ERROR in grib_open opening grib file'
        print *,'!!! baopen return code = ioret = ',ioret
        iret = 93
        return
      endif

      return
      end
c
c----------------------------------------------------------------------c
c----------------------------------------------------------------------c
c
      subroutine grib_open_wa (lug,iret)
c$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    grib_open_wa
C   PRGMMR: WOBUS            ORG: NP20        DATE: 2001-01-16
C
C ABSTRACT: START ABSTRACT HERE AND INDENT TO COLUMN 5 ON THE
C   FOLLOWING LINES.  PLEASE PROVIDE A BRIEF DESCRIPTION OF
C   WHAT THE SUBPROGRAM DOES.
C
C PROGRAM HISTORY LOG:
C   97-01-17  MARCHOK     original program
C   01-01-16  WOBUS       added DOCBLOCK
C
C USAGE:    CALL PROGRAM-NAME(INARG1, INARG2, WRKARG, OUTARG1, ... )
C   INPUT ARGUMENT LIST:
C     INARG1   - GENERIC DESCRIPTION, INCLUDING CONTENT, UNITS,
C     INARG2   - TYPE.  EXPLAIN FUNCTION IF CONTROL VARIABLE.
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     WRKARG   - GENERIC DESCRIPTION, ETC., AS ABOVE.
C     OUTARG1  - EXPLAIN COMPLETELY IF ERROR RETURN
C     ERRFLAG  - EVEN IF MANY LINES ARE NEEDED
C
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)
C
C   OUTPUT FILES:  (DELETE IF NO OUTPUT FILES IN SUBPROGRAM)
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS
C   MACHINE:  IBM SP
C
C$$$

C     ABSTRACT: This subroutine must be called before any attempt is
C     made to write to from the output GRIB files.  The GRIB file
C     is opened with a call to baopenwa.  This call to baopenwa was not
C     needed in the cray version of this program (the files could be
C     opened with a simple Cray assign statement), but the GRIB-writing
C     utilities on the SP do require calls to this subroutine (it has
C     something to do with the GRIB I/O being done in C on the SP, and
C     the C I/O package needs an explicit open statement).
C
C     INPUT:
C     lug      The Fortran unit number for the GRIB file
C     OUTPUT:
C     iret     The return code from this subroutine

      character unitname*10
      character fname*80

      unitname(1:8) = "XLFUNIT_"
      write(unitname(9:10),'(I2)') lug
      call getenv(unitname,fname)
c     print *,' '
c     print *,' in grib_open_wa:  unit: ',lug
c     print *,' in grib_open_wa:  fname: ',fname
      ioret=0
      call baopenwa (lug,fname,ioret)

c     print *,' ' 
c     print *,'baopenwa: ioret= ',ioret

      iret=0
      if (ioret /= 0) then
        print *,' '
        print *,'!!! ERROR in grib_open_wa opening grib file'
        print *,'!!! baopenwa return code = ioret = ',ioret
        iret = 93
        return
      endif

      return
      end
c
c----------------------------------------------------------------------c
c----------------------------------------------------------------------c
c
      subroutine grib_open_r (lug,iret)
c$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    grib_open_r
C   PRGMMR: WOBUS            ORG: NP20        DATE: 2001-01-16
C
C ABSTRACT: This subroutine must be called before any attempt is
C   made to read from the input GRIB files.  The GRIB and index files
C   are opened with a call to baopenr.  This call to baopenr was not
C   needed in the cray version of this program (the files could be
C   opened with a simple Cray assign statement), but the GRIB-reading
C   utilities on the SP do require calls to this subroutine (it has
C   something to do with the GRIB I/O being done in C on the SP, and
C   the C I/O package needs an explicit open statement).
C
C PROGRAM HISTORY LOG:
C   97-01-17  MARCHOK     original program
C   01-01-16  WOBUS       added DOCBLOCK
C
C USAGE:    CALL PROGRAM-NAME(INARG1, INARG2, WRKARG, OUTARG1, ... )
C   INPUT ARGUMENT LIST:
C     lug      The Fortran unit number for the GRIB file
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     iret     The return code from this subroutine
C
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)
C     lug      The Fortran unit number for the GRIB file
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS
C   MACHINE:  IBM SP
C
C$$$

C
C     INPUT:
C     OUTPUT:

      character unitname*10
      character fname*80

      unitname(1:8) = "XLFUNIT_"
      write(unitname(9:10),'(I2)') lug
      call getenv(unitname,fname)
c     print *,' '
c     print *,' in grib_open_r:  unit: ',lug
c     print *,' in grib_open_r:  fname: ',fname
      ioret=0
      call baopenr (lug,fname,ioret)

c     print *,' ' 
c     print *,'baopenr: ioret= ',ioret

      iret=0
      if (ioret /= 0) then
        print *,' '
        print *,'!!! ERROR in sub grib_open_r opening grib file'
        print *,'!!! baopenr return code = ioret = ',ioret
        iret = 93
        return
      endif

      return
      end
