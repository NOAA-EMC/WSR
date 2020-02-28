      
      program circlevr

      open (105,form='formatted')
      read (105,*) vrlons,vrlats,rad
c      vrlons=282.5
c      vrlats=35.0
c      rad=750.0
      call circle(vrlons,vrlats,rad)
      end
 
      subroutine circle(vrlons,vrlats,rad)
      implicit double precision(a-h,o-q,w-z)
      parameter(georad=6367.)
      parameter(idim=100)
      dimension thc(0:idim),xc(0:idim),yc(0:idim)
      dimension psip(0:idim)
      dimension totlon(0:idim),totlat(0:idim)
      real rlon1,rlon2,rlat1,rlat2
      integer vrind
      real*4 vrx1,vrx2,vry1,vry2

c      open (8,file='circle.out',form='unformatted')
c      open (10,file='circle.in',form='unformatted')

      vrind=1

      if (vrind .eq. 1) then

      pi=acos(-1.0)
      vrlat=90.-vrlats
      vrlat=vrlat*(pi/180.0)
c xp and zp are the coordinates of the pole relative to a coordinate
c system with z-axis going through center of disk. The x-axis of
c this system goes through the pole.
      zp=cos(vrlat)
      xp=sin(vrlat)
      ac=rad/georad
      zc=cos(ac)
c      write(6,*) 'vrlat,zp.xp,ac,pi,zc'
c      write(6,*) vrlat,zp,xp,ac,pi,zc
c       write(6,*) 'thc(i),xc(i),psip(i),totlon(i),totlat(i)'

      do i=0,idim
       thc(i)=i*((2.*pi)/idim) 
       xc(i)=sin(ac)*cos(thc(i))
       yc(i)=sin(ac)*sin(thc(i))
       psip(i)=acos((xc(i)*xp+zc*zp))
c Find component of vectors defining circle which is parallel to pole vector.
c Magnitude of component is cos(psip(i)). 
c Direction of pole vector is (xp,zp). Hence desired component is
c (pcx,pcz) where,
       pcx=cos(psip(i))*xp
       pcz=cos(psip(i))*zp
c Subtract these components from vectors defining circle to 
c find component of vectors defining circle which lie on equatorial plane
       ecz=zc-pcz
       ecx=xc(i)-pcx
       ecy=yc(i)
c find magnitude of these vectors
       rmag=sqrt((ecx**2+ecy**2+ecz**2))
c normalise magnitude of the vectors
       eczn=ecz/rmag
       ecxn=ecx/rmag
       ecyn=ecy/rmag
c sign of relative longitude is opposite of sin(thc)
       if(sin(thc(i)).gt.0.0)then
          sign=-1
       else
	sign=1
       end if
c Deduce the relative longitude of the vectors defining the circle
c relative to a unit vector
c which lies on the equatorial plane and is parallel to 
c the equatorial projection of the vector defining the center
c of the verification region. This vector is
c given by (xe,ze)=(-cos(vrlat),sin(vrlat))=(-zp,xp), hence the
c cosine of the angle between this vector and the equatorial projection
c of the vectors defining the circle is given by
       tst2=(eczn*xp-ecxn*zp)
c   Yucheng Song added two lines to prevent the NaNQ
       if(tst2.gt.1.0)tst2=1.0
       if(tst2.lt.-1.0)tst2=-1.0
       rellon=acos(tst2)
       rellon=rellon*(180.0/pi)
       totlon(i)=vrlons+sign*rellon
       totlon(i)=totlon(i)
       totlat(i)=90.-psip(i)*(180.0/pi)
c      write(6,*) thc(i),xc(i),psip(i),totlon(i),totlat(i)
c      if (totlon(i) .ge. 180.0) totlon(i)=totlon(i)-360.0
c     write(6,*) totlon(i),totlat(i)
c RLW 20130507 modify format to match CCS output
      write(6,'(F12.7,F12.8)') totlon(i),totlat(i)
c       write(63,*) vrlons,rellon,sign,totlon(i),totlat(i)
      end do
      npoint=idim+1
c  call setusv('LW',4000)
c      call curve(totlon,totlat,npoint)
c  call setusv('LW',1000)

      endif

      if (vrind .eq. 2) then
         rlon1=vrx1
         rlon2=vrx2
         rlat1=vry1
         rlat2=vry2  
c         call quad(rlon1,rlon2,rlat1,rlat2)
      endif

      return
      end
