c **********************************************************************
c **********************************************************************
c *** ETKF targeting code to provide summary maps of signal variance ***
c ***** (c) S.J.Majumdar, C.H.Bishop, B.J.Etherton, December 1999. *****
c *************** Code developed at Penn State University **************
c **********************************************************************
c ********* Thanks to Bob Kohler of HRD for eigenvalue solvers *********
c **********************************************************************
c **********************************************************************


c 3456789012345678901234567890123456789012345678901234567890123456789012

      double precision,allocatable::xtvec(:,:)
      double precision,allocatable::xtvest(:,:)
      double precision,allocatable::cmat(:,:)
      double precision,allocatable::ceval(:)
      double precision,allocatable::ytvec(:,:)
      double precision,allocatable::obs(:,:,:)
      double precision,allocatable::hy(:,:)
      double precision,allocatable::hyt(:,:)
      double precision,allocatable::work3(:,:)
      double precision,allocatable::cmat2(:,:)
      double precision,allocatable::cmat3(:,:)

      read(5,*)idim,jdim,jstr,mem,nv,ne9,ltcode,nd_sondes,   
     &nd_satobs200,nd_satobs500,nd_satobs850,nvtot,ndtot

      allocate(xtvec(ne9,mem))
      allocate(xtvest(ne9,mem))
      allocate(cmat(mem,mem))
      allocate(ceval(mem))
      allocate(ytvec(ne9,mem))
      allocate(obs(idim,jdim,12))
      allocate(hy(nvtot,mem))
      allocate(hyt(mem,nvtot))
      allocate(work3(mem,mem))
      allocate(cmat2(mem,mem))
      allocate(cmat3(mem,mem))

      call read_perts(xtvec,idim,jdim,mem,ne9,nv)

*     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
*     New function call figures out array hy = [R^-.5][H][Zr] from
*     xtvec (Zr).  Then the ceval eigenvalues and cmat eigenvectors
*     from [Zr^T][H^T][R^-1][H][Zr] are calculated.  Finally, the
*     transformation matrix cmat2 is calculated, then rescaled 
*     to form the rescaled transformation matrix cmat3 (T).  Then,
*     ytvec (Zn) is formed my multiplying xtvec (Zr) by cmat3 (T).
*     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
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
         obs(i,j,10)=7.0**2
         obs(i,j,11)=5.0**2
         obs(i,j,12)=5.0**2
125   continue

      do 111 n=1,nvtot
      do 111 nm=1,mem
         hy(n,nm)=0.0
111   continue
 
      nt=nvtot

      call calc_rhzr(xtvec,hy,obs,ndtot,ne9,mem,nvtot,idim,jdim,
     &nd_sondes,nd_satobs200,nd_satobs500,nd_satobs850,nv)
      CALL DTRANS(nvtot,mem,hy,hyt)
      CALL DMRRRR(MEM,NT,HYT,MEM,NT,MEM,HY,NT,MEM,MEM,WORK3,MEM)
      CALL DEVCSF(MEM,WORK3,MEM,CEVAL,CMAT,MEM)

      do 999 ncnt1=1,mem
      do 999 ncnt2=1,mem
  	 cmat2(ncnt1,ncnt2)=cmat(ncnt1,ncnt2)/sqrt(1.+ceval(ncnt2))
         cmat3(ncnt1,ncnt2)=4.182*cmat2(ncnt1,ncnt2)
999   continue
 
      CALL DMRRRR(NE9,MEM,XTVEC,NE9,MEM,MEM,CMAT3,MEM,NE9,MEM,YTVEC,NE9)

      knum1=8200+ltcode
      knum2=8300+ltcode
      knum3=8400+ltcode

      write(knum1) ytvec
      write(knum2) ceval
      write(knum3) cmat3

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
*     Read in ensemble perturbations, interpolate, write matrices
*     -----------------------------------------------------------------
      subroutine read_perts(xtvec,idim,jdim,mem,ne9,nv)
*     -----------------------------------------------------------------
************************************************************************
      
      dimension datat(idim,jdim,mem,9)
      double precision xtvec(ne9,mem)

      open(93,form='unformatted')
      read(93) datat
      close(93)

1234  sqrtmem=sqrt(float(mem))

      do 35 nm=1,mem
         ie1=0
         do 211 j=1,jdim
         do 211 i=1,idim
            do 221 iv=1,nv
               ie1=ie1+1
               xtvec(ie1,nm)=datat(i,j,nm,iv)/sqrtmem
221         continue
211      continue
35     continue

      return
      end

************************************************************************
*     ------------------------------------------------------------------
*     Calculate new Z matrix for the routine component        
*     ------------------------------------------------------------------
      subroutine calc_rhzr(xtvec,hy,obs,ndtot,ne9,mem,nvtot,idim,jdim,
     &nd_sondes,nd_satobs200,nd_satobs500,nd_satobs850,nv)
*     ------------------------------------------------------------------ 
************************************************************************

      real*4 obslon(ndtot),obslat(ndtot)
      integer lat1(ndtot),lat2(ndtot),lon1(ndtot),lon2(ndtot)
      double precision xtvec(ne9,mem),hmat(ndtot,4),hy(nvtot,mem)
      double precision obs(idim,jdim,12)
      integer k 

*     ----------------------------------------------------------
*     Create the translation matrix from gridpoint space to
*     observation space known as H.
*     ----------------------------------------------------------

      pi=acos(-1.0)
      do 20 iobs=1,nd_sondes
         read(7,*) obslon(iobs), obslat(iobs)
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
            if (lon2(iobs).gt.144) then
               lon2(iobs)=1
            endif
         lat1(iobs)=37-nint(yla1/2.5)
         lat2(iobs)=37-nint(yla2/2.5)
20    continue
      close(7)

      do 21 j=1,nd_satobs200
         k=j+nd_sondes
         read(91,*) obslon(k), obslat(k)
         xlo1=int(obslon(k)*144./360.)*2.5
         xlo2=xlo1+2.5
         yla1=int(obslat(k)*72./180.)*2.5
         yla2=yla1+2.5
         zquot=(xlo2-xlo1)*(yla2-yla1)
         hmat(k,1)= (obslon(k)-xlo2)*(obslat(k)-yla2)/zquot
         hmat(k,2)=-(obslon(k)-xlo2)*(obslat(k)-yla1)/zquot
         hmat(k,3)=-(obslon(k)-xlo1)*(obslat(k)-yla2)/zquot
         hmat(k,4)= (obslon(k)-xlo1)*(obslat(k)-yla1)/zquot
         lon1(k)=nint(xlo1/2.5)+1
         lon2(k)=nint(xlo2/2.5)+1
            if (lon2(k).gt.144) then
               lon2(k)=1
            endif
         lat1(k)=37-nint(yla1/2.5)
         lat2(k)=37-nint(yla2/2.5)
21     continue
       close(91) 

      do 22 j=1,nd_satobs500
         k=j+nd_sondes+nd_satobs200
         read(92,*) obslon(k), obslat(k)
         xlo1=int(obslon(k)*144./360.)*2.5
         xlo2=xlo1+2.5
         yla1=int(obslat(k)*72./180.)*2.5
         yla2=yla1+2.5
         zquot=(xlo2-xlo1)*(yla2-yla1)
         hmat(k,1)= (obslon(k)-xlo2)*(obslat(k)-yla2)/zquot
         hmat(k,2)=-(obslon(k)-xlo2)*(obslat(k)-yla1)/zquot
         hmat(k,3)=-(obslon(k)-xlo1)*(obslat(k)-yla2)/zquot
         hmat(k,4)= (obslon(k)-xlo1)*(obslat(k)-yla1)/zquot
         lon1(k)=nint(xlo1/2.5)+1
         lon2(k)=nint(xlo2/2.5)+1
            if (lon2(k).gt.144) then
               lon2(k)=1
            endif
         lat1(k)=37-nint(yla1/2.5)
         lat2(k)=37-nint(yla2/2.5)
22     continue
       close(92)

      do 23 j=1,nd_satobs850
         k=j+nd_sondes+nd_satobs200+nd_satobs500
         read(90,*) obslon(k), obslat(k)
         xlo1=int(obslon(k)*144./360.)*2.5
         xlo2=xlo1+2.5
         yla1=int(obslat(k)*72./180.)*2.5
         yla2=yla1+2.5
         zquot=(xlo2-xlo1)*(yla2-yla1)
         hmat(k,1)= (obslon(k)-xlo2)*(obslat(k)-yla2)/zquot
         hmat(k,2)=-(obslon(k)-xlo2)*(obslat(k)-yla1)/zquot
         hmat(k,3)=-(obslon(k)-xlo1)*(obslat(k)-yla2)/zquot
         hmat(k,4)= (obslon(k)-xlo1)*(obslat(k)-yla1)/zquot
         lon1(k)=nint(xlo1/2.5)+1
         lon2(k)=nint(xlo2/2.5)+1
            if (lon2(k).gt.144) then
               lon2(k)=1
            endif
         lat1(k)=37-nint(yla1/2.5)
         lat2(k)=37-nint(yla2/2.5)
23     continue
       close(90)

*     --------------------------------------------------------
*     Generate the nice small matrix H*Y, using our brains!
*     --------------------------------------------------------
      do 33 nm=1,mem
      do 33 iobs=1,nd_sondes
      do 33 iv=1,nv
         mm1=9*(lat1(iobs)-1)*idim + 9*(lon1(iobs)-1) + iv
         mm2=9*(lat2(iobs)-1)*idim + 9*(lon1(iobs)-1) + iv
         mm3=9*(lat1(iobs)-1)*idim + 9*(lon2(iobs)-1) + iv
         mm4=9*(lat2(iobs)-1)*idim + 9*(lon2(iobs)-1) + iv
         io=9*(iobs-1) + iv
      hy(io,nm)=hmat(iobs,1)*xtvec(mm1,nm)+hmat(iobs,2)*xtvec(mm2,nm)+
     &          hmat(iobs,3)*xtvec(mm3,nm)+hmat(iobs,4)*xtvec(mm4,nm)
      hy(io,nm)=hy(io,nm)/sqrt(obs(1,1,iv))
33    continue

      do 34 nm=1,mem
      do 34 j=1,nd_satobs200
         k=j+nd_sondes
         mm1=9*(lat1(k)-1)*idim + 9*(lon1(k)-1) + 9
         mm2=9*(lat2(k)-1)*idim + 9*(lon1(k)-1) + 9
         mm3=9*(lat1(k)-1)*idim + 9*(lon2(k)-1) + 9
         mm4=9*(lat2(k)-1)*idim + 9*(lon2(k)-1) + 9
         io=j+nd_sondes*9
      hy(io,nm)=hmat(k,1)*xtvec(mm1,nm)+hmat(k,2)*xtvec(mm2,nm)+
     &          hmat(k,3)*xtvec(mm3,nm)+hmat(k,4)*xtvec(mm4,nm)
      hy(io,nm)=hy(io,nm)/sqrt(obs(1,1,12))
34    continue

      do 35 nm=1,mem
      do 35 j=1,nd_satobs500
         k=j+nd_sondes+nd_satobs200
         mm1=9*(lat1(k)-1)*idim + 9*(lon1(k)-1) + 8
         mm2=9*(lat2(k)-1)*idim + 9*(lon1(k)-1) + 8
         mm3=9*(lat1(k)-1)*idim + 9*(lon2(k)-1) + 8
         mm4=9*(lat2(k)-1)*idim + 9*(lon2(k)-1) + 8 
         io=j+nd_satobs200+nd_sondes*9
      hy(io,nm)=hmat(k,1)*xtvec(mm1,nm)+hmat(k,2)*xtvec(mm2,nm)+
     &          hmat(k,3)*xtvec(mm3,nm)+hmat(k,4)*xtvec(mm4,nm)
      hy(io,nm)=hy(io,nm)/sqrt(obs(1,1,11))
35    continue

      do 36 nm=1,mem
      do 36 j=1,nd_satobs850
         k=j+nd_sondes+nd_satobs200+nd_satobs500
         mm1=9*(lat1(k)-1)*idim + 9*(lon1(k)-1) + 7
         mm2=9*(lat2(k)-1)*idim + 9*(lon1(k)-1) + 7
         mm3=9*(lat1(k)-1)*idim + 9*(lon2(k)-1) + 7
         mm4=9*(lat2(k)-1)*idim + 9*(lon2(k)-1) + 7
         io=j+nd_satobs200+nd_satobs500+nd_sondes*9
      hy(io,nm)=hmat(k,1)*xtvec(mm1,nm)+hmat(k,2)*xtvec(mm2,nm)+
     &          hmat(k,3)*xtvec(mm3,nm)+hmat(k,4)*xtvec(mm4,nm)
      hy(io,nm)=hy(io,nm)/sqrt(obs(1,1,10))
36    continue

      return
      end
