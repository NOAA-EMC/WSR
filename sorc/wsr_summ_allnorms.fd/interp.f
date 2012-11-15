      subroutine interp(datas,dataw,sistr,sigtop,jstr,idim,jdim)

      dimension sistr(jstr),sigtop(jstr)
      common/quadcb/ y0,y1,y2,x0,x1,x2

      dimension datas(idim,jdim),sinm(jdim)
      dimension dataw(idim,jstr)

c      dimension datai(idim,jstr,3),sinmi(jstr,3)
      dimension datai(300,150,3),sinmi(150,3)
      common/itstcb/ datai,sinmi

c      open(unit=60,file='intout.d',form='formatted')

      pi=acos(-1.0)
      dpsieq=1./36.
      dsieqd=dpsieq*(180./pi)
      dsinm=pi/72.
      hdsi=dsinm/2.0
      dsinmd=dsinm*(180./pi)
c      write(60,*) 'idim,jdim,pi,jstr,dsieqd,dsinmd'
c      write(60,*)  idim,jdim,pi,jstr,dsieqd,dsinmd

      do j=1,jdim
      sinm(j)=(pi/2.)-(j-1)*dsinm
      end do

      do j=1,jstr
         jeq=37-j
         sistr(j)=asin(jeq*dpsieq)
         sigtop(j)=(pi/2.)-sistr(j)
         sid=sistr(j)*180./pi
         sigd=sigtop(j)*180./pi
c         write(60,*) j,jeq,sistr(j),sid,sigtop(j),sigd
      end do

      do i=1,idim
         dataw(i,1)=datas(i,1)/jdim
         dataw(i,jstr)=datas(i,jstr)/jdim
      end do
      do js=2,jstr-1
         do j=2,jdim-1

            if((sinm(j)-hdsi).lt.sistr(js))then
	       x0=sinm(j-1)
	       x1=sinm(j)
	       x2=sinm(j+1)
               sinmi(js,1)=x0
	       sinmi(js,2)=x1
	       sinmi(js,3)=x2
               do i=1,idim
	          y0=datas(i,j-1)
	          y1=datas(i,j)
	          y2=datas(i,j+1)
	          datai(i,js,1)=y0
	          datai(i,js,2)=y1
	          datai(i,js,3)=y2
	          dataw(i,js)=fchb(sistr(js))
               end do
               go to 10
            end if

         end do
c end do loop over j
10       continue
      end do
c end do over js
      return 
      end

      function fchb(si)

      common/quadcb/ y0,y1,y2,x0,x1,x2

      si0=y0*(si-x1)*(si-x2)/((x0-x1)*(x0-x2))
      si1=y1*(si-x2)*(si-x0)/((x1-x0)*(x1-x2))
      si2=y2*(si-x0)*(si-x1)/((x2-x0)*(x2-x1))

      fchb=si0+si1+si2

      return 
      end
