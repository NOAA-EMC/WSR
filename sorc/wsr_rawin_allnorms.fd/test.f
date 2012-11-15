

        Program test
        character*34 station 
        character*8 hires 
        read(10,*)
        do i = 1,37
        read(10,100)number, id,station,hires,lat,lon
        write(12,100)number,id,station,hires,lat,lon
c       write(06,110)number,id,station
        write(06,*)float(lon)/100.0,float(lat)/100.0
        enddo
  100   format(I2,2X,I5,1x,A34,2x,A8,2x,I4,2x,I5)
  110   format(I2,2X,I5,1x,A34,2x,'X')


        stop
        end
