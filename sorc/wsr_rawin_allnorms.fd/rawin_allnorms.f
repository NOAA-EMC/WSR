c **********************************************************************
c **********************************************************************
c ******* ETKF code to calculate total signal variance in the VR *******
c ********** for any stations  **********
c **********************************************************************
c *************** Yucheng Song *********************************************
c *************** Code developed at NCEP **************
c **********************************************************************
c **********************************************************************


c 3456789012345678901234567890123456789012345678901234567890123456789012

      program rawin_allnorms
 
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
      double precision,allocatable::work3(:,:)
      double precision sig(100),fev_routine
      double precision sigold(100)
      real*4 , allocatable::obslon(:),obslat(:)
      integer, allocatable:: lat1(:),lat2(:),lon1(:),lon2(:)
      integer, allocatable:: number(:),id(:)
      character*34, allocatable:: station(:)
      character*1, allocatable:: mark(:)
      double precision,allocatable::hmat(:,:)

      integer eid(100),ira(100)
      character*2 tfl
      character*8 hires 
      real uvsd,uv850sd,prcpsd,mslpsd,z500sd

      read(5,*)idim,jdim,jstr,mem,vrlon,vrlat,radvr,nd,nd9,
     &nd12,nrawin,georad,ne1,n2,nvr,nvr12,nv,ne9,ltcode,
     &casecode,ivnorm 
    
      allocate(obslon(nrawin))
      allocate(obslat(nrawin))
      allocate(lat1(nrawin))
      allocate(lat2(nrawin))
      allocate(lon1(nrawin))
      allocate(lon2(nrawin))
      allocate(number(nrawin))
      allocate(id(nrawin))
      allocate(station(nrawin))
      allocate(mark(nrawin))
      allocate(hmat(nd,4))

      allocate(sistr(jstr))
      allocate(sigtop(jstr))
      allocate(xvvest(ne1,mem))
      allocate(cmat(mem,mem))
      allocate(ceval(mem))
      allocate(yvvest(nvr12,mem))
      allocate(ytvec(ne9,mem))
      allocate(hy(nd9,mem))
      allocate(hyt(mem,nd9))
      allocate(obs(idim,jdim,12))
      allocate(bsig(mem,mem))
      allocate(betasig(mem))
      allocate(bsig2(mem,mem))
      allocate(svpvr(nvr12,mem))
      allocate(work3(mem,mem))

      ivr=nint(vrlon/2.5)+1
      jvr=nint((90.-vrlat)/2.5)+1
 
      knum1=ltcode+8200
      knum2=ltcode+8300
      knum3=ltcode+8400
      knum5=casecode+8600

      read(knum1) ytvec
      read(knum2) ceval
      read(knum3) cmat
      read(knum5) xvvest

      pi=acos(-1.0)
      dpsieq=1./36.
      do j=1,jstr
         jeq=37-j
         sistr(j)=asin(jeq*dpsieq)
         sigtop(j)=(pi/2.)-sistr(j)
      end do

      knum0=1234
      open(knum0,form='formatted')
      read(knum0,*) uvsd
      read(knum0,*) uv850sd
      read(knum0,*) prcpsd
      read(knum0,*) mslpsd
      read(knum0,*) z500sd
      close(knum0)

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

      sigmax=0.0
      sigmin=1.0e5

*     --------------------------------------------------------
*     START INTRODUCING DIFFERENT SETS OF OBSERVATIONS
*     --------------------------------------------------------

         read(10,*)
      do 20 iobs=1,nrawin
         read(10,100)number(iobs),id(iobs),station(iobs),hires,lat,lon
         obslon(iobs)=float(lon)/100.0
         obslat(iobs)=float(lat)/100.0
         if(obslon(iobs).lt.0) then
         obslon(iobs)=360.+obslon(iobs)
         endif
         print*, obslon(iobs),obslat(iobs)
         xlo1=int(obslon(iobs)*144./360.)*2.5
         xlo2=xlo1+2.5
         yla1=int(obslat(iobs)*72./180.)*2.5
         yla2=yla1+2.5
         zquot=(xlo2-xlo1)*(yla2-yla1)
         hmat(iobs,1)= (obslon(iobs)-xlo2)*(obslat(iobs)-yla2)/zquot
         hmat(iobs,2)=-(obslon(iobs)-xlo2)*(obslat(iobs)-yla1)/zquot
         hmat(iobs,3)=-(obslon(iobs)-xlo1)*(obslat(iobs)-yla2)/zquot
         hmat(iobs,4)= (obslon(iobs)-xlo1)*(obslat(iobs)-yla1)/zquot
         lon1(iobs)=nint(xlo1/2.5)+1
         lon2(iobs)=nint(xlo2/2.5)+1
         lat1(iobs)=37-nint(yla1/2.5)
         lat2(iobs)=37-nint(yla2/2.5)
20    continue
      close(10)
  100   format(I2,2X,I5,1x,A34,2x,A8,2x,I4,2x,I5)

c     do 10102 id = 1, itask
c        sigvar(id)=0.0

c        obslon(id)=
c        obslat(id)=

      do 10101 iobs=1,nrawin

         ndd=1
         hy=0.0
         hyt=0.0
         betasig=0.0
         bsig=0.0

C     call rawin2(iobs,obs,hy,ytvec,nd,ne9,nd9,mem,idim,
c    &jdim,nv)

*     --------------------------------------------------------
*     Generate the nice small matrix H*Y
*     --------------------------------------------------------
      do 33 nm=1,mem
c     do 33 iobs=1,ndd
      do 33 iv=1,nv
         mm1=9*(lat1(iobs)-1)*idim + 9*(lon1(iobs)-1) + iv
         mm2=9*(lat2(iobs)-1)*idim + 9*(lon1(iobs)-1) + iv
         mm3=9*(lat1(iobs)-1)*idim + 9*(lon2(iobs)-1) + iv
         mm4=9*(lat2(iobs)-1)*idim + 9*(lon2(iobs)-1) + iv
c        io=9*(iobs-1) + iv
         io=iv
      hy(io,nm)=hmat(iobs,1)*ytvec(mm1,nm)+hmat(iobs,2)*ytvec(mm2,nm)+
     &          hmat(iobs,3)*ytvec(mm3,nm)+hmat(iobs,4)*ytvec(mm4,nm)
      hy(io,nm)=hy(io,nm)/sqrt(obs(1,1,iv))
33    continue


*     -------------------------------------------------------------
*     Time to calculate the MEM eigenvalues [BETA_sigma] and
*     eigenvectors [B_sigma] of Hstar*P*Hstar(T)
*     -------------------------------------------------------------
      CALL DTRANS(nd9,MEM,hy,hyt)
      CALL DMRRRR(MEM,nd9,hyt,MEM,nd9,MEM,hy,nd9,MEM,MEM,WORK3,MEM)
      CALL DEVCSF(MEM,WORK3,MEM,BETASIG,BSIG,MEM)

      do 1231 nm1=1,mem
      if (BETASIG(nm1).le.1.0e-12)BETASIG(nm1)=1.0e-15
1231  continue

      do 123 nm1=1,mem
      do 123 nm2=1,mem
       BSIG2(nm1,nm2)=
     &    BSIG(nm1,nm2)*sqrt(BETASIG(nm2)/(1.+BETASIG(nm2)))
123   continue

*     ----------------------------------------------------------------
*     Calculate signal variance variance as function of target region
*     ----------------------------------------------------------------
      CALL DMRRRR(nvr12,MEM,yvvest,nvr12,MEM,MEM,BSIG2,MEM,
     &            nvr12,MEM,svpvr,nvr12)
*     ----------------------------------------------------------------
*     HERE IS WHERE WE DEFINE THE NORM !!
*     ----------------------------------------------------------------
      sig(iobs)=0.0
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
            sig(iobs)=sig(iobs) +
     &         svpvr(iu850,k)**2 + svpvr(iv850,k)**2 +
     &         svpvr(iu500,k)**2 + svpvr(iv500,k)**2 +
     &         svpvr(iu200,k)**2 + svpvr(iv200,k)**2
         endif

* ****** Total energy verifying norm ******
         if (ivnorm.eq.2) then
            sig(iobs)=sig(iobs) +
     &         0.5*( svpvr(iu850,k)**2 + svpvr(iv850,k)**2 +
     &               svpvr(iu500,k)**2 + svpvr(iv500,k)**2 +
     &               svpvr(iu200,k)**2 + svpvr(iv200,k)**2 ) +
     &       3.346*( svpvr(it850,k)**2 + svpvr(it500,k)**2 +
     &               svpvr(it200,k)**2 )
         endif

* ****** (u850,v850,precip,mslp) verifying norm ******
         if (ivnorm.eq.3) then
            sig(iobs)=sig(iobs) +
     &         (svpvr(iu850,k)**2 + svpvr(iv850,k)**2)/uv850sd**2 +
     &         (svpvr(iprcp,k)/prcpsd)**2 + (svpvr(imslp,k)/mslpsd)**2
         endif

61    continue

      if (ivnorm.eq.1) then
         sig(iobs)=sig(iobs)/(mem*ivm*3.0)
      endif

      if (ivnorm.eq.2) then
         sig(iobs)=sig(iobs)/(mem*ivm*3.0)
      endif

      if (ivnorm.eq.3) then
         sig(iobs)=sig(iobs)/(mem*ivm)
      endif

      write(6,99) 'Signal Variance(',iobs,')=',sig(iobs),
     &    '   fcst.err.var of routine network=',fev_routine
      
      if (sig(iobs).gt.sigmax) then
         sigmax=sig(iobs)
      endif

      if (sig(iobs).lt.sigmin.and.sig(iobs).gt.0.1) then
         sigmin=sig(iobs)
      endif
          
10101 continue


      open(1020,form='formatted')
      do 602 iobs=1,nrawin
      write(1020,666)iobs,sig(iobs)
602   continue
      close(1020)

      call bubble_sort(sig,sigold,eid,ira,nrawin)

499   write(1021,*) 5* int(sigmax/5.0) + 5
      write(1022,*) 5* int(sigmin/5.0)
   
      mark=' '
      write(1023,*)20
      write(1023,*)sigmax
      write(1023,*)sigmin
      write(1050,*)'THE FOLLOWING STATIONS ARE SELECTED:' 
      do 55 i=1,20
      tfl='00'
      if (eid(i).ge.10) then
         write(tfl(1:2),'(i2)')eid(i)
      else
         write(tfl(2:2),'(i1)')eid(i)
      endif
      write(6,*) tfl, i
      write(1023,*)obslon(eid(i)), obslat(eid(i)),sigold(eid(i))
      write(1050,110)i, id(eid(i)),station(eid(i)) 
      mark(eid(i))='X'
55    continue

C       do iobs=1,20
C       if (mark(ibos).eq.'X') then
C       write(1050,110)number(iobs),id(iobs),station(iobs),mark(iobs)
C       endif
C       enddo
C 110   format(I2,2X,I5,1x,A34,2x,A1)

  110   format(2X,I2,2X,I5,1X,A34)


10102 continue

99    format (A16,I2,A2,F7.2,A35,F7.2)
666   format(i5,f8.3)

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
*     Prescribed -- set matrix HY
*     -----------------------------------------------------------------
      subroutine rawin2(iobs,obs,hy,ytvec,
     &nd,ne9,nd9,mem,idim,jdim,nv)
*     -----------------------------------------------------------------
************************************************************************

      real*4 obslon(nd),obslat(nd)
      integer lat1(nd),lat2(nd),lon1(nd),lon2(nd)
      double precision ytvec(ne9,mem),hmat(nd,4),hy(nd9,mem)
      double precision obs(idim,jdim,nv)

*     ----------------------------------------------------------
*     Create the translation matrix from gridpoint space to
*     observation space known as H.
*     ----------------------------------------------------------

      pi=acos(-1.0)


      do 20 iobs=1,ndd
         xlo1=int(obslon(iobs)*144./360.)*2.5
         xlo2=xlo1+2.5
         yla1=int(obslat(iobs)*72./180.)*2.5
         yla2=yla1+2.5
         zquot=(xlo2-xlo1)*(yla2-yla1)
         hmat(iobs,1)= (obslon(iobs)-xlo2)*(obslat(iobs)-yla2)/zquot
         hmat(iobs,2)=-(obslon(iobs)-xlo2)*(obslat(iobs)-yla1)/zquot
         hmat(iobs,3)=-(obslon(iobs)-xlo1)*(obslat(iobs)-yla2)/zquot
         hmat(iobs,4)= (obslon(iobs)-xlo1)*(obslat(iobs)-yla1)/zquot
         lon1(iobs)=nint(xlo1/2.5)+1
         lon2(iobs)=nint(xlo2/2.5)+1
         lat1(iobs)=37-nint(yla1/2.5)
         lat2(iobs)=37-nint(yla2/2.5)
20    continue

*     --------------------------------------------------------
*     Generate the nice small matrix H*Y, using our brains!
*     --------------------------------------------------------
      do 33 nm=1,mem
      do 33 iobs=1,ndd
      do 33 iv=1,nv
         mm1=9*(lat1(iobs)-1)*idim + 9*(lon1(iobs)-1) + iv
         mm2=9*(lat2(iobs)-1)*idim + 9*(lon1(iobs)-1) + iv
         mm3=9*(lat1(iobs)-1)*idim + 9*(lon2(iobs)-1) + iv
         mm4=9*(lat2(iobs)-1)*idim + 9*(lon2(iobs)-1) + iv
         io=9*(iobs-1) + iv
      hy(io,nm)=hmat(iobs,1)*ytvec(mm1,nm)+hmat(iobs,2)*ytvec(mm2,nm)+
     &          hmat(iobs,3)*ytvec(mm3,nm)+hmat(iobs,4)*ytvec(mm4,nm)
      hy(io,nm)=hy(io,nm)/sqrt(obs(1,1,iv))
33    continue

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


c **********************************************************************
      subroutine bubble_sort(signal,sigold,iid,ira,nrawin)
c **********************************************************************

      integer iid(500), iid2(500), ira(500), nrawin
      double precision signal(500), sigold(500), sigve2(500), maxsig

      do 10 k=1,nrawin
         iid(k)=k
         iid2(k)=iid(k)
         sigve2(k)=signal(k)
         sigold(k)=signal(k)
10    continue

      do 18 k=1,nrawin
         maxsig=0.0
         do 19 kk=k,nrawin
            if (signal(kk).gt.maxsig) then
               maxsig=signal(kk)
               kkk=kk
            endif
19       continue
c        print *,kkk
         do 20 kk=k+1,kkk
            signal(kk)=sigve2(kk-1)
            iid(kk)=iid2(kk-1)
20       continue
         signal(k)=maxsig
         iid(k)=iid2(kkk)
         do 21 kk=1,nrawin
            sigve2(kk)=signal(kk)
            iid2(kk)=iid(kk)
21       continue
18    continue
      close(17)

      do 30 ir=1,nd
         ira(iid(ir))=ir
30    continue
      return
      end
