
      PROGRAM pres_interp
      INCLUDE '/usr/include/netcdf.inc'
      REAL*4 pmslin(360,180),pmsl(180,92),x(180),y(92),
     1 olon(180,92),olat(180,92),lat(91),lon(181),floatval(1)

      INTEGER iday,time_seriesid,P_MSLid,xid,yid,time_seriesdim,
     1  xdim,ydim,time_index
      INTEGER*4 rcode,dims(3),start(10),count(10)

      PARAMETER (nrec=  124)   !CHANGE THIS TO GENERALIZE
      LOGICAL iopn,iexst
      REAL*8      time_series(nrec)
      CHARACTER*80 infile,outfile
      INTEGER DayNum
      EXTERNAL DayNum
      lunpos=7
C  Open file of lat/lon grids to use
       open(unit=lunpos,file='lola92180.txt',status='old',
     1   access='sequential',err=901)
       do 102 j=1,180
       do 102 i=1,92
       read(lunpos,*)olon(j,i),olat(j,i)
102    olon(j,i)=(-1*olon(j,i))+360

       infile(1:22)='this_month_pressure.nc'
       write(6,*)'computing average field for ',infile(1:22)

C  create output file
       iappend=0
       ndays=0
       idaymin=1

       outfile(1:20)='this_month_oscurs.nc'
        inquire(file=outfile(1:20),opened=iopn,exist=iexst)
        if(iopn.or.iexst)then
        write(6,'('' File exists, overwriting file'')')
        endif
 
20      RCODE=nf_create(outfile(1:20),nf_clobber,ncidout)
      if(RCODE.ne.0)stop
* define dimensions
      rcode = nf_def_dim(ncidout, 'time_series',nf_unlimited,
     1  time_seriesdim)
      rcode = nf_def_dim(ncidout, 'y', 92,ydim)
      rcode = nf_def_dim(ncidout, 'x', 180,xdim)
* define variables
      dims(1) = time_seriesdim
      rcode =nf_def_var(ncidout,'time_series',NF_DOUBLE,1,dims,
     1    time_seriesid)
      dims(1) = ydim
      rcode = nf_def_var(ncidout, 'y', NF_REAL, 1, dims,yid)
      dims(1) = xdim
      rcode = nf_def_var(ncidout, 'x', NF_REAL, 1, dims,xid)
      dims(2) = ydim
      dims(1) = xdim
      rcode = nf_def_var(ncidout, 'lat', NF_REAL, 2, dims,latid)
      dims(2) = ydim
      dims(1) = xdim
      rcode = nf_def_var(ncidout, 'lon', NF_REAL, 2, dims,lonid)
      dims(3) = time_seriesdim
      dims(2) = ydim
      dims(1) = xdim
      rcode = nf_def_var(ncidout, 'pmsl', NF_REAL,3,dims,P_MSLid)
* assign attributes
      rcode = nf_put_att_text(ncidout, time_seriesid, 'long_name',
     1  14,'reference time')
      rcode = nf_put_att_text(ncidout,time_seriesid,'units',
     1    20,'hours since 1948-1-1')
      rcode = nf_put_att_text(ncidout,yid,'long_name',7,'y-index')
      rcode = nf_put_att_text(ncidout,xid,'long_name', 7,'x-index')
      rcode = nf_put_att_text(ncidout,latid,'long_name',8,'latitude')
      rcode = nf_put_att_text(ncidout,latid,'units',13,'degrees_north')
      rcode = nf_put_att_text(ncidout, lonid,'long_name', 9,'longitude')
      rcode = nf_put_att_text(ncidout, lonid, 'units',12,'degrees_east')
      rcode = nf_put_att_text(ncidout,P_MSLid, 'long_name',23,
     1  'Pressure reduced to MSL')
      rcode = nf_put_att_text(ncidout, P_MSLid,'units', 2,'Mb')
      floatval(1) = -9999
      rcode = nf_put_att_real(ncidout, P_MSLid, '_FillValue', NF_REAL,
     1 1,floatval)

      rcode = nf_put_att_text(ncidout, NF_GLOBAL, 'title',37,
     1  'Interpolated from FNMOC 6-hr pressure')
      rcode =nf_put_att_text(ncidout,NF_GLOBAL,'Conventions',6,'COARDS')
* leave define mode
      rcode = nf_enddef(ncidout)
      rcode = nf_close(ncidout )
       rcode = nf_open(outfile(1:20),NF_WRITE,ncidout)
       rcode =nf_inq_varid(ncidout,'lat',latid)
       rcode =nf_inq_varid(ncidout,'lon',lonid)
       rcode =nf_inq_varid(ncidout,'y',yid)
       rcode =nf_inq_varid(ncidout,'x',xid)
       rcode =nf_inq_varid(ncidout,'pmsl',P_MSLid)
       rcode =nf_inq_varid(ncidout,'time_series',time_seriesid)
  
       start(1)=1
       count(1)=92
       do 35 i=1,92
35       y(i)=i
      rcode=nf_put_vara_real(ncidout,yid,start,count,y)
       start(1)=1
       count(1)=180
       do 36 i=1,180
36       x(i)=i
      rcode=nf_put_vara_real(ncidout,xid,start,count,x)
       start(1)=1
       start(2)=1
       count(1)=180
       count(2)=92
      rcode=nf_put_vara_real(ncidout,latid,start,count,olat)
       start(1)=1
       start(2)=1
       count(1)=180
       count(2)=92
      rcode=nf_put_vara_real(ncidout,lonid,start,count,olon)
      

        rcode=nf_open(infile(1:22),0,NCID)
       write(6,*)'opening ',infile(1:22),' rcode=',rcode
        rcode=nf_inq_dimid(ncid,'time_series',intimedimid)
        rcode=nf_inq_dimlen(ncid,intimedimid,nrecs)
c     nrecs now contains number of records in file
       write(6,*)'Number of records in input file: ',nrecs
C
C    statements to fill lat                            
C
       rcode =nf_inq_varid(ncid,'lat',inlatid)
       start(1)=81
       count(1)=91
       rcode=nf_get_vara_real(ncid,inlatid,start,count,lat)
C
C    statements to fill lon                            
C
       rcode =nf_inq_varid(ncid,'lon',inlonid)
       start(1)=101
       count(1)=181
       rcode=nf_get_vara_real(ncid,inlonid,start,count,lon)
      do 71 j=1,181
71       if(lon(j).lt.0)lon(j)=lon(j)+360

C
C    statements to fill time_series                    
C
       rcode =nf_inq_varid(ncid,'time_series',intimeid)
       call check_err(rcode)
       start(1)=1
       count(1)=nrecs
       rcode=nf_get_vara_double(ncid,intimeid,start,count,time_series)
       print *, time_series
       time_index=0
       do 450 iday=1,nrecs,4
       floatval(1) = -9999

C
C    statements to fill Pmsl                          
C
       rcode =nf_inq_varid(ncid,'P_msl',inpmslid)
        start(1) = 101
        start(2) = 81
        start(3) = iday
        count(1) = 181
        count(2) = 91
        count(3) = 1
        rcode=nf_get_vara_real(ncid,inpmslid,start,count,pmslin)
       do 433 jj=1,92
       do 433 ii=1,180
c	  iii=181-ii
      iii=ii
C  Don't use this time step in the mean if any values are missing or bad

C  Interpolate to 92x180 grid
433     pmsl(iii,jj)=bilin(pmslin,lat,lon,olat(ii,jj),olon(ii,jj),
     1        91,181)

           time_index=time_index+1
            START(1) = 1
            START(2) = 1
            START(3) = time_index
            COUNT(1) = 180
            COUNT(2) = 92
            COUNT(3) = 1
        rcode=nf_put_vara_real(ncidout,P_MSLid,start,count,pmsl)
       call check_err(rcode)
        rcode=nf_put_var1_double(ncidout,time_seriesid,time_index,
     1    time_series(iday))
       call check_err(rcode)
450       continue  

      rcode = nf_close(ncid)
      rcode = nf_close(ncidout)
900      write(*,*)'done interpolating'
      return
901   write(6,*)' error opening text file'

      END
      subroutine check_err(iret)
      integer iret
      include '/usr/include/netcdf.inc'
      if (iret .ne. NF_NOERR) then
      print *, nf_strerror(iret)
      stop
      endif
      end

      INTEGER FUNCTION DayNum(iyr,imon,iday)
C
C     PURPOSE: Compute a day number.  One of the more useful
C              applications for this routine is to compute
C              the number of days between two dates.
C
C     INPUT: integer year (iyr), month (imon), and day (iday).
C
C    OUTPUT: integer Day Number (DayNum).  Oct. 15, 1582 will
C            will return a day number of unity.
C
C    COMMENTS: Note the gregorian calendar was adopted in
C              Oct. 15, 1582, and hence this algorithm will
C              not work properly for dates early than
C              10-15-1582.
C
C     CODE DEPENDENCIES:
C      Routine Name                  File
C        LeapYr                    TIMPAK.FOR
C
C      AUTHOR:  Robert D. Stewart
C        DATE:  May 20, 1995
C
      INTEGER cdpy(0:11)
      INTEGER nday,i
      LOGICAL LeapYr
      EXTERNAL LeapYr

C     Cummulative days per year beginning on the first day
C     of the month.
      DATA (cdpy(i),i=0,11)
     @     /0,31,59,90,120,151,181,212,243,273,304,334/

C     The integer 577747 is simply an offset so that
C     Oct 15, 1582 returns a day number of one.
      nday = (iyr-1)*365.25 + cdpy(imon-1) + iday - 577747
      IF (LeapYr(iyr)) THEN
        IF (imon.GT.2) THEN
C         Add one day for the current leap year.
          nday = nday+1
        ENDIF
      ENDIF
      DayNum=nday

      RETURN
      END

      LOGICAL FUNCTION LeapYr(iyr)
C
C     PURPOSE: Check for a leap year.  LeapYr is returned as true
C       if iyr is a leap year, and false otherwise.
C
C     INPUT:
C            iyr: integer year
C
C    OUTPUT:
C            logical true (leap year) or false (not a leap year).
C
C    COMMENTS: Leap years are years that are evenly divisible by
C              4, except years that are evenly divisible by 100
C              must be divisible by 400.
c              NOTE: Gregorian calendar adopted Oct. 15, 1582.
C
C     CODE DEPENDENCIES: N/A
C
C      AUTHOR:  Robert D. Stewart
C        DATE:  May 20, 1995
C
      INTEGER iyr

      IF ((iyr-(iyr/400)*400).EQ.0) THEN
C       Years divisible by 400 are also divisible by 4 and
C       100... they are leap years.
        LeapYr = .TRUE.
      ELSEIF ((iyr-(iyr/100)*100).EQ.0) THEN
        LeapYr = .FALSE.
      ELSEIF ((iyr-(iyr/4)*4).EQ.0) THEN
C       Leap year.
        LeapYr = .TRUE.
      ELSE
        LeapYr = .FALSE.
      ENDIF

      RETURN
      END
C***************************************************************
C***************************************************************
      SUBROUTINE mean_var (mean, sumsq, num, obs)
C***************************************************************
C     Updates the mean and sum of squares of deviations given
C     a new observation.
c
c   on input:
c          mean  - the calculated mean after j observations
c                  real*4, must be initialized to 0.0
c          sumsq - the sum of squares of deviations for the
c                  first j observations. real*4, must be 
c                  initialized to 0.0
c          num   - sum of weights of the first j observations,
c                  Integer*4, must
c                  be initialized to 0
c          obs   - the new observation to use in updating
c                  mean, sumsq, real*4
c
c  on output:
c          mean  - the mean after j+1 observations
c
c          sumsq - the sum of squares of deviations after
c                  j+1 observations
c
c          num   - the number of observations, in this case
c                  j+1
c
c          obs   - unchanged on output
c
c  when all the data have been processed, the variance of the data
c  can be calculated as:
c
c          sumsq/float(num-1)
c
C***************************************************************
      IMPLICIT  NONE      
     
      REAL*4    mean, sumsq, obs
      INTEGER*4 num
     
      REAL*4    temp
     
      temp  = obs - mean
      num   = num + 1
      mean  = mean + (temp/float(num))
      sumsq = sumsq + (temp*(obs-mean))
     
      RETURN
      END

