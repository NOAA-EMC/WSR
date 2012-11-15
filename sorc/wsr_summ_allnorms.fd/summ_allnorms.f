c **********************************************************************
c **********************************************************************
c *** ETKF targeting code to provide summary maps of signal variance ***
c ***** (c) S.J.Majumdar, C.H.Bishop, B.J.Etherton, December 1999. *****
c *************** Code developed at Penn State University **************
c **********************************************************************
c **********************************************************************


c 3456789012345678901234567890123456789012345678901234567890123456789012
      program summ_allnorms
      include 'mpif.h' 
      allocatable sistr(:)
      allocatable sigtop(:)
      double precision,allocatable::xvvest(:,:)
      double precision,allocatable::cmat(:,:)
      double precision,allocatable::ceval(:)
      double precision,allocatable::yvvest(:,:)
      double precision,allocatable::ytvec(:,:)
      double precision,allocatable::hy(:,:)
      double precision,allocatable::hyt(:,:)
      double precision,allocatable::obs(:,:,:)
      double precision,allocatable::bsig(:,:)
      double precision,allocatable::betasig(:)
      double precision,allocatable::bsig2(:,:)
      double precision,allocatable::svpvr(:,:)
      double precision,allocatable::sig(:,:)
      double precision,allocatable::sigall(:,:)
      double precision,allocatable::work3(:,:)
      double precision :: time_begin, time_end

      double precision :: fev_routine
      real :: uvsd,uv850sd,prcpsd,mslpsd,z500sd
      real :: sigmaxi,sigmaxj

      integer :: ierr, myrank, nprocs
      integer, parameter :: MASTER=0

      call MPI_INIT(ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr)

      print*, 'myrank=', myrank

c eg. read in  144 37 37 177 80 300 80 0 135.0 35.0

      if (myrank .eq. MASTER) then
      read(5,*)idim,jdim,jstr,mem,rlon1,rlon2,rlat1,rlat2,
     &vrlon,vrlat,radvr,nd,nd9,nd12,georad,ne1,n2,nvr,nvr12,nv,
     &ne9,ltcode,casecode,ivnorm
      close(5)

      allocate(sistr(jstr))
      allocate(sigtop(jstr))
      allocate(ytvec(ne9,mem))
      allocate(ceval(mem))
      allocate(cmat(mem,mem))
      allocate(xvvest(ne1,mem))

      allocate(yvvest(nvr12,mem))
      allocate(hy(nd9,mem))
      allocate(hyt(mem,nd9))
      allocate(obs(idim,jdim,9))
      allocate(bsig(mem,mem))
      allocate(betasig(mem))
      allocate(bsig2(mem,mem))
      allocate(svpvr(nvr12,mem))
      allocate(sig(idim,jdim))
      allocate(sigall(idim,jdim))
      allocate(work3(mem,mem))

      idim11=nint(rlon1/2.5)+1
      idim12=nint(rlon2/2.5)+1
      if (idim11.gt.idim12) idim12=idim12+idim 
      jdim11=nint((90.-rlat1)/2.5)+1
      jdim12=nint((90.-rlat2)/2.5)+1

      ivr=nint(vrlon/2.5)+1
      jvr=nint((90.-vrlat)/2.5)+1

      open(16,form='formatted')
      write(16,*) ivr, jvr, vrlon, vrlat
      close(16)
 
      knum1=ltcode+8200
      knum2=ltcode+8300
      knum3=ltcode+8400
      knum5=casecode+8600
      read(knum1) ytvec
      read(knum2) ceval
      read(knum3) cmat
      read(knum5) xvvest
      close(knum1)
      close(knum2)
      close(knum3)
      close(knum5)

      knum0=1234
      open(knum0,form='formatted')
      read(knum0,*) uvsd
      read(knum0,*) uv850sd
      read(knum0,*) prcpsd
      read(knum0,*) mslpsd
      read(knum0,*) z500sd
      close(knum0)

      pi=acos(-1.0)
      dpsieq=1./36.
      do j=1,jstr
         jeq=37-j
         sistr(j)=asin(jeq*dpsieq)
         sigtop(j)=(pi/2.)-sistr(j)
      end do

      call calcvrci(ivr,jvr,sistr,sigtop,cmat,ivm,xvvest,yvvest,
     &jstr,ne1,mem,nvr12,idim,jdim,radvr,georad)
      call calc_fev_routine(ivm,yvvest,fev_routine,
     &                      uvsd,uv850sd,prcpsd,mslpsd,z500sd,
     &                      nvr12,mem,ivnorm)

      do 125 i=1,idim
      do 125 j=1,jdim
         obs(i,j,1)=2.4**2
         obs(i,j,2)=2.8**2
         obs(i,j,3)=2.95**2
         obs(i,j,4)=2.4**2
         obs(i,j,5)=2.8**2
         obs(i,j,6)=2.95**2
         obs(i,j,7)=0.8**2
         obs(i,j,8)=0.8**2
         obs(i,j,9)=1.2**2
125   continue
       

      deallocate(sistr)
      deallocate(sigtop)
      deallocate(ceval)
      deallocate(cmat)
      deallocate(xvvest)

      endif
      
      CALL  MPI_BCAST(idim,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,ierr)
      CALL  MPI_BCAST(jdim,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,ierr)
      CALL  MPI_BCAST(idim11,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,ierr)
      CALL  MPI_BCAST(idim12,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,ierr)
      CALL  MPI_BCAST(jdim11,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,ierr)
      CALL  MPI_BCAST(jdim12,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,ierr)
      CALL  MPI_BCAST(ne9,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,ierr)
      CALL  MPI_BCAST(nd9,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,ierr)
      CALL  MPI_BCAST(mem,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,ierr)
      CALL  MPI_BCAST(nd,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,ierr)
      CALL  MPI_BCAST(ivnorm,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,ierr)
      CALL  MPI_BCAST(ivm,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,ierr)
      CALL  MPI_BCAST(nvr12,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,ierr)
      CALL  MPI_BCAST(uvsd,1,MPI_REAL,MASTER,MPI_COMM_WORLD,ierr)
      CALL  MPI_BCAST(uv850sd,1,MPI_REAL,MASTER,MPI_COMM_WORLD,ierr)
      CALL  MPI_BCAST(prcpsd,1,MPI_REAL,MASTER,MPI_COMM_WORLD,ierr)
      CALL  MPI_BCAST(mslpsd,1,MPI_REAL,MASTER,MPI_COMM_WORLD,ierr)
      CALL  MPI_BCAST(z500sd,1,MPI_REAL,MASTER,MPI_COMM_WORLD,ierr)
      CALL  MPI_BCAST(fev_routine,1,MPI_DOUBlE_PRECISION,MASTER,
     &                MPI_COMM_WORLD,ierr)

      CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)

      if (myrank .ne. MASTER) then
      allocate(obs(idim,jdim,9))
      allocate(ytvec(ne9,mem))
      allocate(yvvest(nvr12,mem))

      allocate(hy(nd9,mem))
      allocate(hyt(mem,nd9))
      allocate(bsig(mem,mem))
      allocate(betasig(mem))
      allocate(bsig2(mem,mem))
      allocate(svpvr(nvr12,mem))
      allocate(sig(idim,jdim))
      allocate(sigall(idim,jdim))
      allocate(work3(mem,mem))
      endif


      CALL  MPI_BCAST(obs,idim*jdim*9,MPI_DOUBlE_PRECISION,MASTER,
     &                MPI_COMM_WORLD,ierr)
      CALL  MPI_BCAST(ytvec,ne9*mem,MPI_DOUBlE_PRECISION,MASTER,
     &                MPI_COMM_WORLD,ierr)
      CALL  MPI_BCAST(yvvest,nvr12*mem,MPI_DOUBlE_PRECISION,MASTER,
     &                MPI_COMM_WORLD,ierr)

      CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)

*     --------------------------------------------------------
*     START INTRODUCING DIFFERENT SETS OF OBSERVATIONS
*     --------------------------------------------------------
      sig=0.0
      sigall=0.0

      time_begin =  MPI_Wtime()

c     do 10101 jr2=jdim11+myrank*2,jdim12,2*nprocs
c     do 10102 ir2=idim11,idim12,2

      do 10101 ir2=idim11+myrank*2,idim12,2*nprocs
      print*, "ir2=",ir2, " myrank=",myrank
      do 10102 jr2=jdim11,jdim12,2

      HY=0.0
      call squareTR2(ir2,jr2,obs,hy,ytvec,idim,jdim,ne9,nd9,mem,nd)

*     -------------------------------------------------------------
*     Time to calculate the MEM eigenvalues [BETA_sigma] and
*     eigenvectors [B_sigma] of Hstar*P*Hstar(T)
*     -------------------------------------------------------------
      CALL DTRANS(nd9,MEM,HY,HYT)

      CALL DMRRRR(MEM,nd9,HYT,MEM,nd9,MEM,HY,nd9,MEM,MEM,WORK3,MEM)

      CALL DEVCSF(MEM,WORK3,MEM,BETASIG,BSIG,MEM)

      do 1231 nm1=1,mem
       if (BETASIG(nm1).le.1.0e-12) BETASIG(nm1)=1.0e-15
1231  continue

      do 123 nm1=1,mem
      do 123 nm2=1,mem
       BSIG2(nm1,nm2)=
     &    BSIG(nm1,nm2)*sqrt(BETASIG(nm2)/(1.+BETASIG(nm2)))
123   continue

*     ----------------------------------------------------------------
*     Calculate signal variance as function of target region
*     ----------------------------------------------------------------
      CALL DMRRRR(nvr12,MEM,yvvest,nvr12,MEM,MEM,BSIG2,MEM,
     &            nvr12,MEM,svpvr,nvr12)

*     ----------------------------------------------------------------
*     HERE IS WHERE WE DEFINE THE NORM !!
*     ----------------------------------------------------------------
      sig(ir2,jr2)=0.0
      do 61 ivcount=1,ivm
      do 61 k=1,mem
         iu850=(ivcount-1)*12 + 1
         iv850=(ivcount-1)*12 + 2
         iu500=(ivcount-1)*12 + 3
         iv500=(ivcount-1)*12 + 4
         iu200=(ivcount-1)*12 + 5
         iv200=(ivcount-1)*12 + 6
         it850=(ivcount-1)*12 + 7
         it500=(ivcount-1)*12 + 8
         it200=(ivcount-1)*12 + 9
         iprcp=(ivcount-1)*12 + 10
         imslp=(ivcount-1)*12 + 11
         iz500=(ivcount-1)*12 + 12

* ****** (u,v) verifying norm ******
         if (ivnorm.eq.1) then
            sig(ir2,jr2)=sig(ir2,jr2) +
     &         svpvr(iu850,k)**2 + svpvr(iv850,k)**2 +
     &         svpvr(iu500,k)**2 + svpvr(iv500,k)**2 +
     &         svpvr(iu200,k)**2 + svpvr(iv200,k)**2
         endif

* ****** Total energy verifying norm ******
         if (ivnorm.eq.2) then
            sig(ir2,jr2)=sig(ir2,jr2) +
     &         0.5*( svpvr(iu850,k)**2 + svpvr(iv850,k)**2 +
     &               svpvr(iu500,k)**2 + svpvr(iv500,k)**2 +
     &               svpvr(iu200,k)**2 + svpvr(iv200,k)**2 ) +
     &       3.346*( svpvr(it850,k)**2 + svpvr(it500,k)**2 +
     &               svpvr(it200,k)**2 )
         endif

* ****** (u850,v850,precip,mslp) verifying norm ******
         if (ivnorm.eq.3) then
            sig(ir2,jr2)=sig(ir2,jr2) +
     &         (svpvr(iu850,k)**2 + svpvr(iv850,k)**2)/uv850sd**2 +
     &         (svpvr(iprcp,k)/prcpsd)**2 + (svpvr(imslp,k)/mslpsd)**2
         endif

61    continue

      if (ivnorm.eq.1) then
         sig(ir2,jr2)=sig(ir2,jr2)/(mem*ivm*3.0)
      endif

      if (ivnorm.eq.2) then
         sig(ir2,jr2)=sig(ir2,jr2)/(mem*ivm*3.0)
      endif

      if (ivnorm.eq.3) then
         sig(ir2,jr2)=sig(ir2,jr2)/(mem*ivm)
      endif


10102 continue
10101 continue

      time_end =  MPI_Wtime()
      print*,time_end-time_begin,' seconds', 'rank=', myrank
      
C     CALL MPI_GATHER(
      CALL MPI_REDUCE(sig,sigall,idim*jdim,MPI_DOUBLE_PRECISION, 
     &                MPI_SUM,MASTER,MPI_COMM_WORLD,ierr)

      if (myrank .eq. MASTER) then

      do 10104 jr2=jdim11,jdim12,2
      do 10105 ir2=idim11,idim12,2

      write(6,99) 'Signal variance(',ir2,',',jr2,')=',sigall(ir2,jr2),
     &    '   fcst.err.var of routine network=',fev_routine
        if (sigall(ir2,jr2).gt.sigmax) then
         sigmax=sigall(ir2,jr2)
         imx=ir2
         jmx=jr2
        endif

10105 continue
10104 continue

C     sigmaxi=((imx-1)*2.5)-360.
      sigmaxi=((imx-1)*2.5)
      sigmaxj=90.-((jmx-1)*2.5)
      
      open(301,form='formatted')
      write(301,*) 9
      write(301,667) sigmaxi,sigmaxj
      write(301,667) (sigmaxi+5.0),(sigmaxj+5.0)
      write(301,667) (sigmaxi+5.0),(sigmaxj)
      write(301,667) (sigmaxi+5.0),(sigmaxj-5.0)
      write(301,667) (sigmaxi-5.0),(sigmaxj+5.0)
      write(301,667) (sigmaxi-5.0),(sigmaxj)
      write(301,667) (sigmaxi-5.0),(sigmaxj-5.0)
      write(301,667) (sigmaxi),(sigmaxj+5.0)
      write(301,667) (sigmaxi),(sigmaxj-5.0)
      close(301)
667   format (F9.3,F9.3)

      open(10,form='formatted')
       do 601 j=jdim11,jdim12,2
       do 601 i=idim11,idim12,2
         write(10,666)i,j,sigall(i,j)
601   continue
      close(10)

99    format (A16,I3,A1,I2,A2,F7.2,A35,F7.2)
666   format(2i5,f12.3)

      endif

      call MPI_FINALIZE(ierr)

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
*     Square target region -- set matrix HY -- NORMAL GRID
*     -----------------------------------------------------------------
      subroutine squareTR2(ir2,jr2,obs,hy,ytvec,idim,jdim,ne9,nd9,mem,
     &                     nd)
*     -----------------------------------------------------------------
************************************************************************

      double precision ytvec(ne9,mem),hy(nd9,mem),obs(idim,jdim,9)
      integer itra(200),jtra(200)

      iwi=(nint(sqrt(nd+0.0))-1)/2

      do 1302 nm=1,mem
      ie1=0
      itcount=0
      do 1102 j=1,jdim
      do 1002 i=1,idim
         mmm=9*((j-1)*idim) + 9*(i-1)
         if(abs(j-jr2).le.(iwi+.1).and.abs(i-ir2).le.(iwi+.1)) then
            itcount=itcount+1
            itra(itcount)=i
            jtra(itcount)=j
            ie1=ie1+1
            hy(ie1,nm)=ytvec(mmm+1,nm)/sqrt(obs(i,j,1))
            ie1=ie1+1
            hy(ie1,nm)=ytvec(mmm+2,nm)/sqrt(obs(i,j,2))
            ie1=ie1+1
            hy(ie1,nm)=ytvec(mmm+3,nm)/sqrt(obs(i,j,3))
            ie1=ie1+1
            hy(ie1,nm)=ytvec(mmm+4,nm)/sqrt(obs(i,j,4))
            ie1=ie1+1
            hy(ie1,nm)=ytvec(mmm+5,nm)/sqrt(obs(i,j,5))
            ie1=ie1+1
            hy(ie1,nm)=ytvec(mmm+6,nm)/sqrt(obs(i,j,6))
            ie1=ie1+1
            hy(ie1,nm)=ytvec(mmm+7,nm)/sqrt(obs(i,j,7))
            ie1=ie1+1
            hy(ie1,nm)=ytvec(mmm+8,nm)/sqrt(obs(i,j,8))
            ie1=ie1+1
            hy(ie1,nm)=ytvec(mmm+9,nm)/sqrt(obs(i,j,9))
         endif
1002  continue
1102  continue
1302  continue

34    format (I4,I4,F10.4)

      return
      end






************************************************************************
*     -----------------------------------------------------------------
*     Circular verification region, calculate Y matrix in VR
*     -----------------------------------------------------------------
      subroutine calcvrci(ivr,jvr,sistr,sigtop,cmat,ivm,xvvest,yvvest,
     &jstr,ne1,mem,nvr12,idim,jdim,radvr,georad)
*     -----------------------------------------------------------------
************************************************************************

      dimension sistr(jstr),sigtop(jstr)
      integer ivra(500),jvra(500)
      double precision xvvest(ne1,mem),cmat(mem,mem),yvvest(nvr12,mem)

      pi=acos(-1.0)

c     compute coordinates of the verification area
c     ============================================
      radjvr=float(jvr-1)*pi/72.
      radivr=float(ivr-1)*2.0*pi/144.
      zvr=cos(radjvr)
      xvr=sin(radjvr)*cos(radivr)
      yvr=sin(radjvr)*sin(radivr)

c     ================================================================
c     define distance of grid point from center of verification region
c     ================================================================
      ie2=0
c      print*, 'j, sistr(j)*180./pi, sigtop(j)*180./pi'
      do 70 j=1,jstr
c      print *,j
c         print*, j, sistr(j)*180./pi, sigtop(j)*180./pi
         radj=sigtop(j)
         z=cos(radj)
         hr=sin(radj)
            do 60 i=1,idim
            radi=float(i-1)*2.0*pi/144.
            x=hr*cos(radi)
            y=hr*sin(radi)
            costh=(xvr*x+yvr*y+zvr*z)
            th=acos(costh)
            dist=georad*th
            if (dist.le.radvr) then
               ie2=ie2+1
               ivra(ie2)=i
               jvra(ie2)=j
c               print*, ie2,float(i-1)*2.5,sistr(j)*180./pi
            end if
60       continue
70    continue
      ivm=ie2
c      print*, 'ivmemo=',ivm

      do 61 ivcount=1,ivm
      mmm=12*((jvra(ivcount)-1)*idim) + 12*(ivra(ivcount)-1)
      do 61 k=1,mem
      do 61 ivar=1,12
         num=mmm+ivar
         iv=(ivcount-1)*12+ivar
         yvvest(iv,k)=0.0
         do 62 nm2=1,mem 
            yvvest(iv,k)=yvvest(iv,k)+xvvest(num,nm2)*cmat(nm2,k)
62       continue
61    continue

      return
      end


************************************************************************
*     -----------------------------------------------------------------
*     Calculate forecast error variance for routine obs. network
*     -----------------------------------------------------------------
      subroutine calc_fev_routine(ivm,yvvest,fev_routine,
     &                            uvsd,uv850sd,prcpsd,mslpsd,z500sd,
     &                            nvr12,mem,ivnorm)
*     -----------------------------------------------------------------
************************************************************************

      integer ivm
      double precision yvvest(nvr12,mem),fev_routine

      real uvsd,uv850sd,prcpsd,mslpsd,z500sd

      fev_routine=0.0
      do 161 ivcount=1,ivm
      do 161 k=1,mem
         iu850=(ivcount-1)*12 + 1
         iv850=(ivcount-1)*12 + 2
         iu500=(ivcount-1)*12 + 3
         iv500=(ivcount-1)*12 + 4
         iu200=(ivcount-1)*12 + 5
         iv200=(ivcount-1)*12 + 6
         it850=(ivcount-1)*12 + 7
         it500=(ivcount-1)*12 + 8
         it200=(ivcount-1)*12 + 9
         iprcp=(ivcount-1)*12 + 10
         imslp=(ivcount-1)*12 + 11
         iz500=(ivcount-1)*12 + 12

* ****** (u,v) verifying norm ******
         if (ivnorm.eq.1) then
            fev_routine = fev_routine +
     &                    yvvest(iu850,k)**2 + yvvest(iv850,k)**2 +
     &                    yvvest(iu500,k)**2 + yvvest(iv500,k)**2 +
     &                    yvvest(iu200,k)**2 + yvvest(iv200,k)**2
         endif

* ****** Total energy verifying norm ******
         if (ivnorm.eq.2) then
            fev_routine = fev_routine +
     &         0.5*( yvvest(iu850,k)**2 + yvvest(iv850,k)**2 +
     &               yvvest(iu500,k)**2 + yvvest(iv500,k)**2 +
     &               yvvest(iu200,k)**2 + yvvest(iv200,k)**2 ) +
     &       3.346*( yvvest(it850,k)**2 + yvvest(it500,k)**2 +
     &               yvvest(it200,k)**2 )
         endif

* ****** (u850,v850,precip,mslp) verifying norm ******
         if (ivnorm.eq.3) then
            fev_routine = fev_routine +
     &        (yvvest(iu850,k)**2 + yvvest(iv850,k)**2)/uv850sd**2 +
     &        (yvvest(iprcp,k)/prcpsd)**2 + (yvvest(imslp,k)/mslpsd)**2
         endif

161   continue

      if (ivnorm.eq.1) then
         fev_routine = fev_routine/(mem*ivm*3.0)
      endif

      if (ivnorm.eq.2) then
         fev_routine = fev_routine/(mem*ivm*3.0)
      endif

      if (ivnorm.eq.3) then
         fev_routine = fev_routine/(mem*ivm)
      endif

      return
      end
