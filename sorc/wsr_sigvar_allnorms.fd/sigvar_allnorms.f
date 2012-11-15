c **********************************************************************
c **********************************************************************
c ****** ETKF code to provide geographical distribution of signal ******
c ******* variance for any given deployment in file 'track.tp' ********* 
c **********************************************************************
c **********************************************************************
c ***** (c) S.J.Majumdar, C.H.Bishop, B.J.Etherton, December 1999. *****
c *************** Code developed at Penn State University **************
c **********************************************************************
c **********************************************************************


c 3456789012345678901234567890123456789012345678901234567890123456789012

      program sigvar_allnorms

      real uvsd,uv850sd,prcpsd,mslpsd,z500sd

      double precision,allocatable::xvvec(:,:)
      double precision,allocatable::cmat(:,:)
      double precision,allocatable::ceval(:)
      double precision,allocatable::ytvec(:,:)
      double precision,allocatable::yvvec(:,:)
      double precision,allocatable::hsty(:,:)
      double precision,allocatable::hstyt(:,:)
      double precision,allocatable::bsig(:,:)
      double precision,allocatable::betasig(:)
      double precision,allocatable::bsig2(:,:)
      double precision,allocatable::work3(:,:)

      read(5,*)idim,jdim,npts12,mem,nd,nd9,rlon1,rlon2,rlat1,rlat2,
     &nflights,georad,ne9,nv,ltcode,ivnorm,ltstep

      allocate(xvvec(npts12,mem))
      allocate(cmat(mem,mem))
      allocate(ceval(mem))
      allocate(ytvec(ne9,mem))
      allocate(yvvec(npts12,mem))
      allocate(hsty(nd9,mem))
      allocate(hstyt(nd9,mem))
      allocate(bsig(mem,mem))
      allocate(betasig(mem))
      allocate(bsig2(mem,mem))
      allocate(work3(mem,mem))


      i1=nint(rlon1/2.5)+1
      i2=nint(rlon2/2.5)+1
      if (idim11.gt.idim12) idim12=idim12+idim
      j1=nint((90.-rlat1)/2.5)+1
      j2=nint((90.-rlat2)/2.5)+1

      knum1=ltcode+8200
      knum2=ltcode+8300
      knum3=ltcode+8400

      read(knum1) ytvec
      read(knum2) ceval
      read(knum3) cmat

      knum0=1234
      open(knum0,form='formatted')
      read(knum0,*) uvsd
      read(knum0,*) uv850sd
      read(knum0,*) prcpsd
      read(knum0,*) mslpsd
      read(knum0,*) z500sd
      close(knum0)

      do 300 kkk=0,ltstep
      call read_tv(kkk,i1,i2,j1,j2,xvvec,idim,jdim,mem,npts12)
      call calc_y(xvvec,yvvec,cmat,npts12,mem)
      call flights(mem,ytvec,hsty,nd,ne9,nd9,idim,jdim,nv)
      CALL DTRANS(nd9,MEM,hsty,hstyt)
      CALL DMRRRR(MEM,nd9,hstyt,MEM,nd9,MEM,hsty,nd9,MEM,MEM,WORK3,MEM)
      CALL DEVCSF(MEM,WORK3,MEM,BETASIG,BSIG,MEM)

      do 1231 nm1=1,mem
      if (BETASIG(nm1).le.1.0e-12)BETASIG(nm1)=1.0e-15
1231  continue

      do 123 nm1=1,mem
      do 123 nm2=1,mem
      BSIG2(nm1,nm2)=BSIG(nm1,nm2)*sqrt(BETASIG(nm2)/(1.+BETASIG(nm2)))
123   continue

      print*,  uv850sd, uvsd,mslpsd,z500sd 

      call calc_sigvar(kkk,i1,i2,j1,j2,yvvec,bsig2,
     &                 uvsd,uv850sd,prcpsd,mslpsd,z500sd,
     &                 npts12,mem,idim,jdim,ivnorm)
300   continue

      STOP
      END

************************************************************************
************************************************************************


************************************************************************
*     -----------------------------------------------------------------
*     Noting that keeping a bunch of extra variables around, the time
*     has come to make matrix transposes a calculates, rather than a
*     stored quantity.
*     -----------------------------------------------------------------
************************************************************************
      SUBROUTINE DTRANS(M,N,A,B)
************************************************************************
 
      double precision A(m,n)
      double precision B(n,m)
      do 1111 i=1,n
      do 1111 j=1,m
1111  B(i,j)=A(j,i)
 
      return
      end




************************************************************************
*     -----------------------------------------------------------------
*     Prescribed flight tracks -- set matrix HY
*     -----------------------------------------------------------------
      subroutine flights(mm,ytvec,hsty,nd,ne9,nd9,idim,jdim,nv)
*     -----------------------------------------------------------------
************************************************************************

      integer mm
      real*4 obslon(nd),obslat(nd)
      integer lat1(nd),lat2(nd),lon1(nd),lon2(nd)
      double precision ytvec(ne9,mm),hmat(nd,4),hsty(nd9,mm)
      double precision oberr(idim,jdim,nv)

*     --------------------------------------------------------
*     First generate the observation error covariance matrix
*     --------------------------------------------------------
         do 125 i=1,idim
         do 125 j=1,jdim
            oberr(i,j,1)=2.4**2
            oberr(i,j,2)=2.4**2
            oberr(i,j,3)=2.8**2
            oberr(i,j,4)=2.8**2
            oberr(i,j,5)=2.95**2
            oberr(i,j,6)=2.95**2
            oberr(i,j,7)=0.8**2
            oberr(i,j,8)=0.8**2
            oberr(i,j,9)=1.2**2
125      continue

*     ----------------------------------------------------------
*     Create the translation matrix from gridpoint space to
*     observation space known as H.
*     ----------------------------------------------------------
      open(7,form='formatted',status='unknown')
      pi=acos(-1.0)
      read(7,*) ndd
      do 20 iobs=1,ndd
         read(7,*) obslon(iobs), obslat(iobs)
C Y Song added to adjust for positive obslon
         if (obslon(iobs).lt.0) then
         obslon(iobs)=360.+obslon(iobs)
         endif
         xlo1=int(obslon(iobs)*144./360.)*2.5
         xlo2=xlo1+2.5
         yla1=int(obslat(iobs)*72./180.)*2.5
         yla2=yla1+2.5
         zquot=(xlo2-xlo1)*(yla2-yla1)
c         print*, 'obslon(iobs), obslat(iobs), xlo1,xlo2,yla1,yla2'
c         print*,  obslon(iobs), obslat(iobs), xlo1,xlo2,yla1,yla2
         hmat(iobs,1)= (obslon(iobs)-xlo2)*(obslat(iobs)-yla2)/zquot
         hmat(iobs,2)=-(obslon(iobs)-xlo2)*(obslat(iobs)-yla1)/zquot
         hmat(iobs,3)=-(obslon(iobs)-xlo1)*(obslat(iobs)-yla2)/zquot
         hmat(iobs,4)= (obslon(iobs)-xlo1)*(obslat(iobs)-yla1)/zquot
c         print*, 'h1=',hmat(iobs,1),'  h2=',hmat(iobs,2)
c         print*, 'h3=',hmat(iobs,3),'  h4=',hmat(iobs,4)
         lon1(iobs)=nint(xlo1/2.5)+1
         lon2(iobs)=nint(xlo2/2.5)+1
         lat1(iobs)=37-nint(yla1/2.5)
         lat2(iobs)=37-nint(yla2/2.5)
c         print*, 'lon1=',lon1(iobs),'  lon2=',lon2(iobs)
c         print*, 'lat1=',lat1(iobs),'  lat2=',lat2(iobs)
20    continue
      close(7)

*     --------------------------------------------------------
*     Generate the nice small matrix H*Y, using our brains!
*     --------------------------------------------------------
      do 33 nm=1,mm
      do 33 iobs=1,ndd
      do 33 iv=1,nv
         mm1=9*(lat1(iobs)-1)*idim + 9*(lon1(iobs)-1) + iv
         mm2=9*(lat2(iobs)-1)*idim + 9*(lon1(iobs)-1) + iv
         mm3=9*(lat1(iobs)-1)*idim + 9*(lon2(iobs)-1) + iv
         mm4=9*(lat2(iobs)-1)*idim + 9*(lon2(iobs)-1) + iv
         io=9*(iobs-1) + iv
      hsty(io,nm)=hmat(iobs,1)*ytvec(mm1,nm)+hmat(iobs,2)*ytvec(mm2,nm)+
     &            hmat(iobs,3)*ytvec(mm3,nm)+hmat(iobs,4)*ytvec(mm4,nm)
      hsty(io,nm)=hsty(io,nm)/sqrt(oberr(1,1,iv))
33    continue

      return
      end


************************************************************************
*     -----------------------------------------------------------------
*     Read in ensemble perturbations at later times t_V
*     -----------------------------------------------------------------
      subroutine read_tv(kkk,i1,i2,j1,j2,xvvec,idim,jdim,mem,npts12)
*     -----------------------------------------------------------------
************************************************************************

      dimension datav(idim,jdim,mem,12)

      double precision xvvec(npts12,mem)

      open(1100+kkk,form='unformatted')
      read(1100+kkk) datav
      close(1100+kkk)

1234  sm=sqrt(float(mem))

      do 35 nm=1,mem
         ie1=0
         do 211 j=j1,j2,2
         do 211 i=i1,i2,2
            do 221 iv=1,12
               ie1=ie1+1
               xvvec(ie1,nm)=datav(i,j,nm,iv)/sm
221         continue
211      continue
35    continue

      return
      end


************************************************************************
*     -----------------------------------------------------------------
*     Using the C's, evaluate transformed ensemble Y
*     -----------------------------------------------------------------
      subroutine calc_y(xvvec,yvvec,cmat,npts12,mem)
*     -----------------------------------------------------------------
************************************************************************

      double precision xvvec(npts12,mem),yvvec(npts12,mem)
      double precision cmat(mem,mem)

*     -------------------------------------------------------------
*     Store Y matrices for unadapted observational network
*     Outer product of Y is first guess prediction error cov matrix
*     -------------------------------------------------------------
      CALL DMRRRR
     &(npts12,mem,xvvec,npts12,mem,mem,cmat,mem,npts12,mem,yvvec,npts12)

      return
      end




************************************************************************
*     -----------------------------------------------------------------
*     Calculate Reduction of Error Variance
*     -----------------------------------------------------------------
      subroutine calc_sigvar(kkk,i1,i2,j1,j2,yvvec,bsig2,
     &                 uvsd,uv850sd,prcpsd,mslpsd,z500sd,
     &                 npts12,mem,idim,jdim,ivnorm)
*     -----------------------------------------------------------------
************************************************************************

      double precision yvvec(npts12,mem),bsig2(mem,mem)
      double precision svv(npts12,mem),impv(idim,jdim)

      real uvsd,uv850sd,prcpsd,mslpsd,z500sd

*     -----------------------------------------------------------------
*     Reduction of prediction error variance
*     -----------------------------------------------------------------
      CALL DMRRRR(npts12,mem,yvvec,npts12,mem,mem,bsig2,mem,
     &            npts12,mem,svv,npts12)
*     -----------------------------------------------------------------
      print*, 'Signal Variance calculated for t_T+',(kkk*12)

      do 500 i=1,idim
      do 500 j=1,jdim
         impv(i,j)=0.0
500   continue


c *** (u,v,T) signal variance ***
      ipt=1
      do 271 j=j1,j2,2
      do 281 i=i1,i2,2
         impv(i,j)=0.0
         iu850=(ipt-1)*12 + 1
         iv850=(ipt-1)*12 + 2
         iu500=(ipt-1)*12 + 3
         iv500=(ipt-1)*12 + 4
         iu200=(ipt-1)*12 + 5
         iv200=(ipt-1)*12 + 6
         it850=(ipt-1)*12 + 7
         it500=(ipt-1)*12 + 8
         it200=(ipt-1)*12 + 9
         iprcp=(ipt-1)*12 + 10
         imslp=(ipt-1)*12 + 11
         iz500=(ipt-1)*12 + 12

         do 35 k=1,mem

            if (ivnorm.eq.1) then
            impv(i,j)=impv(i,j) + svv(iu850,k)**2+svv(iv850,k)**2
     &                          + svv(iu500,k)**2+svv(iv500,k)**2
     &                          + svv(iu200,k)**2+svv(iv200,k)**2
            endif

            if (ivnorm.eq.2) then
            impv(i,j)=impv(i,j) + 0.5*(svv(iu850,k)**2+svv(iv850,k)**2
     &                                +svv(iu500,k)**2+svv(iv500,k)**2
     &                                +svv(iu200,k)**2+svv(iv200,k)**2)
     &                        + 3.346*(svv(it850,k)**2+svv(it500,k)**2
     &                                +svv(it200,k)**2)
            endif

            if (ivnorm.eq.3) then
            impv(i,j)=impv(i,j) + (svv(iu850,k)**2+svv(iv850,k)**2)
     &                          /uv850sd**2 + (svv(iprcp,k)/prcpsd)**2
     &                          + (svv(imslp,k)/mslpsd)**2
            endif

35       continue

         ipt=ipt+1
281   continue
271   continue

      open(1200+kkk,form='formatted')
      do 501 i=i1,i2,2
      do 501 j=j1,j2,2
         write(1200+kkk,666)i,j,impv(i,j)/3.0
501   continue
      close(1200+kkk)

666   format(2i5,f8.3)



      return
      end
