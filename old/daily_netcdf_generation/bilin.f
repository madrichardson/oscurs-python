        function bilin(f,lat,lon,latval,lonval,ny,nx)
        dimension f(181,91)
      real*4 latval,lonval,lat(91),lon(181)
      real*8  y(91),x(181)

      xval=lonval
      yval=latval

      do 1 i=1,nx
1      x(i)=lon(i)
      do 2 j=1,ny
2      y(j)=lat(j)
        if(x(1).gt.x(nx).or.y(1).gt.y(ny))go to 900
        if(x(1).gt.xval.or.x(nx).lt.xval)go to 901
        if(y(1).gt.yval.or.y(ny).lt.yval)go to 901
      nx1=nx-1
      do 10 k=1,nx1
10      if(x(k).le.xval.and.x(k+1).ge.xval)go to 30
30      i=k
      ny1=ny-1
      do 50 k=1,ny1
50      if(y(k).le.yval.and.y(k+1).ge.yval)go to 60
60      j=k

          if(f(i,j).lt.900.or.f(i,j).gt.1100.or.
     1        f(i+1,j).lt.900.or.f(i+1,j).gt.1100.or.
     2        f(i,j+1).lt.900.or.f(i,j+1).gt.1100.or.
     3        f(i+1,j+1).lt.900.or.f(i+1,j+1).gt.1100)
     4        go to 902
      xdiv=(xval-x(i))/(x(i+1)-x(i))
      ydiv=(yval-y(j))/(y(j+1)-y(j))

c	write(6,*)i,j,xval,yval,f(i,j),f(i+1,j),f(i+1,j+1),f(i,j+1),
c     1  x(i),x(i+1),y(j),y(j+1)

      bilin=(1-xdiv)*(1-ydiv)*f(i,j)+
     1       xdiv*(1-ydiv)*f(i+1,j)+
     2       xdiv*ydiv*f(i+1,j+1)+
     3       (1-xdiv)*ydiv*f(i,j+1)

c	write(6,*)bilin
      return


900   write(6,*)' Error, program only works for increasing x- and y- '
      bilin=-9999
      return
901   write(6,*)' Error, target point out of range, xval= ',xval,
     1  ',yval= ',yval
      bilin=-9999
      return
c902	write(6,*)' Error, bad pressure value '
902      bilin=-9999
      return
        end

