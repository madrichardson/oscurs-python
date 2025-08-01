	
      PROGRAM pres_interp
      INCLUDE 'netcdf.inc'
	
	REAL*4 pmsl(180,92),x(180),y(92),
     1    olon(180,92),olat(180,92)
	INTEGER np(180,92),idays(12)
	DOUBLE PRECISION pmslin(360,180)
      INTEGER year,month,iday,ihour,time_seriesid,
     2  P_MSLid,xid,yid
	
      PARAMETER (NVARS=23) !NUMBER OF VARIABLES
      PARAMETER (NREC=    14600)   !CHANGE THIS TO GENERALIZE
C     VARIABLE IDS RUN SEQUENTIALLY FROM 1 TO NVARS= 23
      INTEGER*4 RCODE,dims(3),corner(3), edges(3)
      INTEGER*4 RECDIM
      character*1 ans
      CHARACTER*50 long_name(NVARS)
      CHARACTER*50 name(100)
	logical iopn,iexst,LeapYr
C     ****VARIABLES FOR THIS NETCDF FILE****
C
      REAL*8      time_series(NREC),tmp,TAX1(NREC)
      REAL*8      lat(  181)
      REAL*8      lon(  360)
      real*4       floatval(1)
C*************************************
      character*80 infile,outfile
      INTEGER*4 START(10)
      INTEGER*4 COUNT(10)
      INTEGER VDIMS(10) !ALLOW UP TO 10 DIMENSIONS
      CHARACTER*31 DUMMY
      INTEGER time_index,idate_base,idate_new
      INTEGER DayNum
	DATA idays/31,28,31,30,31,30,31,31,30,31,30,31/
      EXTERNAL DayNum

C  Open file of lat/lon grids to use
       open(unit=lunpos,file='lola92180.txt',status='OLD',
     1   access='SEQUENTIAL',err=900)
	do 102 j=1,180
	do 102 i=1,92
	read(lunpos,*)olon(j,i),olat(j,i)
102	olon(j,i)=(-1*olon(j,i))+360

      write(6,'(''Enter year and month (yyyymm? '',$)')
      read(5,'(i4,i2)')iiyear,imonth
	write(6,*)'Year:',year,', Month:', imonth
	infile(1:10)='pmsl_fill_'
	infile(15:17)='.nc'
	write(infile(11:14),'(i4)')iiyear
	write(6,*)'computing average field for ',infile(1:17)

C  File time is 15-jan-1901
c	idate_base_old=DayNum(iiyear,1,1)
	idate_base_old=DayNum(1901,1,15)
C  Beginning of current month
	idate_year=DayNum(iiyear,imonth,1)
C  Base time for new file is Jan 1948
	idate_base=DayNum(1948,1,1) ! days since Daynum t0
C  Hours from 1-1-48 to beg of current month
c	itime_dif=(idate_base_old-idate_base+1)*24
	itime_lower=(idate_year-idate_base)*24

C  create output file
	iappend=0
	ndays=0
	idaymin=1
	
	outfile(1:12)='oscurs_pres_'
	write(outfile(13:18),'(i4,i2.2)')iiyear,imonth
	outfile(19:21)='.nc'
      inquire(file=outfile(1:21),opened=iopn,exist=iexst)
        if(iopn.or.iexst)then
        write(6,'(/,'' File '',a,'' exists, overwrite?'',/,
     2   ''(Y/N)'',$)')outfile(1:21)
        read(5,'(a)')ans
        if(ans.ne.'Y'.and.ans.ne.'y')stop
        write(6,'('' File exists, overwriting file'')')
        endif
 
20      RCODE=nf_create(outfile(1:21),nf_clobber,ncidout)
      if(RCODE.ne.0)stop
* define dimensions
      time_seriesdim = ncddef(ncidout, 'time_series', NCUNLIM, RCODE)
      leveldim = ncddef(ncidout, 'level', 1, RCODE)
      ydim = ncddef(ncidout, 'y', 92, RCODE)
      xdim = ncddef(ncidout, 'x', 180, RCODE)
      ngridsdim = ncddef(ncidout, 'ngrids', 1, RCODE)
      navdim = ncddef(ncidout, 'nav', 1, RCODE)
      nav_lendim = ncddef(ncidout, 'nav_len', 100, RCODE)
* define variables
      dims(1) = time_seriesdim
      time_seriesid =ncvdef(ncidout,'time_series',NCDOUBLE,1,dims,RCODE)
      dims(1) = ydim
      yid = ncvdef (ncidout, 'y', NCFLOAT, 1, dims, RCODE)
      dims(1) = xdim
      xid = ncvdef (ncidout, 'x', NCFLOAT, 1, dims, RCODE)
      dims(2) = ydim
      dims(1) = xdim
      latid = ncvdef (ncidout, 'lat', NCFLOAT, 2, dims, RCODE)
      dims(2) = ydim
      dims(1) = xdim
      lonid = ncvdef (ncidout, 'lon', NCFLOAT, 2, dims, RCODE)
      dims(3) = time_seriesdim
      dims(2) = ydim
      dims(1) = xdim
      P_MSLid = ncvdef (ncidout, 'pmsl', NCFLOAT, 3, dims, RCODE)
* assign attributes
      call ncaptc(ncidout, time_seriesid, 'long_name',NCCHAR,14,
     1  'reference time', RCODE)
      call ncaptc(ncidout,time_seriesid,'units',NCCHAR, 20,
     1   'hours since 1948-1-1', RCODE)
      call ncaptc(ncidout,yid,'long_name',NCCHAR,7,'y-index',RCODE)
      call ncaptc(ncidout,xid,'long_name',NCCHAR,7,'x-index',RCODE)
      call ncaptc(ncidout,latid,'long_name',NCCHAR,8,'latitude',RCODE)
      call ncaptc(ncidout,latid,'units',NCCHAR,13,'degrees_north',RCODE)
      call ncaptc(ncidout, lonid, 'long_name', NCCHAR, 9, 'longitude',
     1 RCODE)
      call ncaptc(ncidout, lonid, 'units', NCCHAR, 12, 'degrees_east',
     1 RCODE)
      call ncaptc(ncidout, P_MSLid, 'long_name',NCCHAR,23,
     1  'Pressure reduced to MSL', RCODE)
      call ncaptc(ncidout, P_MSLid, 'units', NCCHAR, 2, 'Mb', RCODE)
      floatval(1) = -9999
      call ncapt(ncidout,P_MSLid,'_FillValue',NCFLOAT,1,floatval,RCODE)

      call ncaptc(ncidout, NCGLOBAL, 'title',NCCHAR,43,
     1  'Interpolated from FNMOC 6-hr pressure', RCODE)
      call ncaptc(ncidout, NCGLOBAL,'Conventions',NCCHAR,6,'COARDS',
     1RCODE)
* leave define mode
      call ncendf(ncidout, RCODE)
	start(1)=1
	count(1)=92
	do 35 i=1,92
35	y(i)=i
      call ncvpt(ncidout,yid,start,count,y,RCODE)
	start(1)=1
	count(1)=180
	do 36 i=1,180
36	x(i)=i
      call ncvpt(ncidout,xid,start,count,x,RCODE)
	start(1)=1
	start(2)=1
	count(1)=180
	count(2)=92
      call ncvpt(ncidout,latid,start,count,olat,RCODE)
	start(1)=1
	start(2)=1
	count(1)=180
	count(2)=92
      call ncvpt(ncidout,lonid,start,count,olon,RCODE)
      call ncclos (ncidout, RCODE)
      
	ncidout=ncopn(outfile(1:21),ncwrite,RCODE)
	latid=ncvid(ncidout,'lat',RCODE)
	lonid=ncvid(ncidout,'lon',RCODE)
	yid=ncvid(ncidout,'y',RCODE)
	xid=ncvid(ncidout,'x',RCODE)
	P_MSLid=ncvid(ncidout,'pmsl',RCODE)
	time_seriesid=ncvid(ncidout,'time_series',RCODE)
  

51      NCID=NCOPN(infile(1:17),0,RCODE)
	write(6,*)'opening ',infile(1:17),' rcode=',RCODE
c      CALL NCINQ(NCID,NDIMS,NVARS,NGATTS,RECDIM,RCODE)
c      CALL NCDINQ(NCID,RECDIM,DUMMY,NRECS,RCODE)
C     !NRECS! NOW CONTAINS NUM RECORDS FOR THIS FILE
C
C    statements to fill lat                            
C
      ivarid = NCVID(NCID,'LAT',RCODE)
      CALL NCVINQ(NCID,ivarid,DUMMY,NTP,NVDIM,VDIMS,NVS,RCODE)
	start(1)=1
	count(1)=91
      CALL NCVGT(NCID,ivarid,START,COUNT,lat,RCODE)
C
C    statements to fill lon                            
C
      ivarid = NCVID(NCID,'LON',RCODE)
      CALL NCVINQ(NCID,ivarid,DUMMY,NTP,NVDIM,VDIMS,NVS,RCODE)
      LENSTR=1
      DO 70 J=1,NVDIM
      CALL NCDINQ(NCID,VDIMS(J),DUMMY,NDSIZE,RCODE)
      LENSTR=LENSTR*NDSIZE
      START(J)=1
      COUNT(J)=NDSIZE
70    CONTINUE
      CALL NCVGT(NCID,ivarid,START,COUNT,lon,RCODE)
      do 71 j=1,181
71	if(lon(j).lt.0)lon(j)=lon(j)+360

C
C    statements to fill time_series                    
C
c  Note files prior to 2001 have axis of TAX.  Move to Salmonid required
C   reverting to Ferret 4.91 -> needed different axis
      ivarid = NCVID(NCID,'TAX1',RCODE)
      CALL NCVINQ(NCID,ivarid,DUMMY,NTP,NVDIM,VDIMS,NVS,RCODE)
      LENSTR=1
      DO  80 J=1,NVDIM
      CALL NCDINQ(NCID,VDIMS(J),DUMMY,NDSIZE,RCODE)
      LENSTR=LENSTR*NDSIZE
      START(J)=1
      COUNT(J)=NDSIZE
  80  CONTINUE
      CALL NCVGT(NCID,ivarid,START,COUNT,time_series,RCODE)
     
      
	icnt=1
	if(LeapYr(iiyear))idays(2)=29
	idaymin=0
	do 100 i=1,imonth-1
100	idaymin=idaymin+idays(i)
	idaymax=idaymin+idays(imonth)
	idaymin=idaymin+1
	do 450 iday=idaymin,idaymax
	write(6,*)'Day=',iday
	itime_upper=itime_lower+24
      
      floatval(1) = -9999
c      DO 90 i = 1,92
c        DO 90 j = 1,180
c          np(j,i) = 0
c  It works to initialize to -9999 instead of zero because it
C	gets subtracted out duing first call to mean_var
c	  pmsl_mean(j,i)=0
c 90	  continue

           	


	icnt = (iday-1)*4+1
	write(6,*)icnt
c	if(time_series(icnt).lt.itime_lower.or.
c     1            time_series(icnt).gt.itime_upper)go to 450
C
C    statements to fill Pmsl                          
C
	    time_index=icnt
            ivaridp = NCVID(NCID,'PMSL',RCODE)
            CALL NCVINQ(NCID,ivaridp,DUMMY,NTP,NVDIM,VDIMS,NVS,RCODE)
            LENSTR=1
            START(1) = 1
            START(2) = 1
            START(3) = time_index
            COUNT(1) = 181
            COUNT(2) = 91
            COUNT(3) = 1
            CALL NCVGT(NCID,ivaridp,START,COUNT,pmslin,RCODE)
c	do 101 ii=1,180
c	do 101 jj=1,92
c101	pmsl(ii,jj)=floatval(1)

	do 435 jj=1,92
	do 433 ii=1,180
c	  iii=181-ii
	  iii=ii
C  Don't use this time step in the mean if any values are missing or bad

C  Interpolate to 92x180 grid
c	if(ii.eq.1.and.jj.eq.1)write(6,*)lat,lon,olat,olon
c	write(6,*)olat(ii,jj),olon(ii,jj)
         pmsl(iii,jj)=bilin(pmslin,lat,lon,olat(ii,jj),olon(ii,jj),
     1        91,181)
c	write(6,*)pmsl(iii,jj)	

433	    continue	
435	continue
	  
	  


	    time_index=iday-idaymin+1
            START(1) = 1
            START(2) = 1
            START(3) = time_index
            COUNT(1) = 180
            COUNT(2) = 92
            COUNT(3) = 1
	call ncvpt(ncidout,P_MSLid,START,COUNT,pmsl,RCODE)
	 tmp=itime_lower	 
	call ncvpt1(ncidout,time_seriesid,time_index,tmp,
     1      RCODE)     
	itime_lower=itime_upper
C
450	continue	  
      CALL NCCLOS(NCID,RCODE)
      call ncclos(ncidout,RCODE)
C
900      write(*,*)'done interpolating'

      END
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

