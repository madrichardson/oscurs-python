	
      PROGRAM pres_interp
      INCLUDE 'netcdf.inc'
	
	REAL*4 pmsl(180,92)
	
	
      PARAMETER (NVARS=23) !NUMBER OF VARIABLES
      PARAMETER (NREC=    14600)   !CHANGE THIS TO GENERALIZE
      REAL*8    time_series(NREC),tmp,end_time
      INTEGER time_seriesid,P_MSLid,status
      INTEGER*4 START(10),COUNT(10),VDIMS(10),time_index
      logical iopn,iexst
      character*1 ans
      character*80 infile,updfile
      CHARACTER*31 DUMMY

      write(6,'(''Enter year and month (yyyymm? '',$)')
      read(5,'(i4,i2)')iiyear,imonth
	write(6,*)'Year:',iiyear,', Month:', imonth
	updfile(1:12)='oscurs_pres_'
	updfile(19:21)='.nc'
	write(updfile(13:18),'(i4,i2.2)')iiyear,imonth
	write(6,*)'File containing new data to append: ',updfile(1:17)

	infile(1:18)='oscurs_pressure.nc'
	nchin=18
        nchup=21
      inquire(file=infile(1:nchin),opened=iopn,exist=iexst)
        if(iopn.or.iexst)then
        write(6,'(/,'' File '',a,
     2   '' exists, append(A), insert(I), or quit(Q)? '',$)')
     3   infile(1:nchin)
        read(5,'(a)')ans
        if(ans.eq.'Q'.or.ans.eq.'q')stop
	endif
      inquire(file=updfile(1:nchup),exist=iexst)
        if(.not.iexst)then
	  write(6,*)'File to modify not found: ',updfile(1:nchup)
	  stop
	  endif
 
	ncidout=ncopn(infile(1:nchin),ncwrite,status)
	P_MSLid=ncvid(ncidout,'pmsl',status)
	time_seriesid=ncvid(ncidout,'time_series',status)
c  get current number of days in file	
        CALL NCVINQ(ncidout,time_seriesid,DUMMY,NTP,NVDIM,VDIMS,NVS,
     1        status)
        CALL NCDINQ(NCIDOUT,VDIMS(1),DUMMY,NDSIZE,status)
	ndays=NDSIZE
  	if(ans.eq.'A'.or.ans.eq.'a')index=NDSIZE
	if(ans.eq.'I'.or.ans.eq.'i')then
	  write(6,'(/,''Enter starting index for insert: '',$)')
          read(5,'(i5)')index
	  index=index-1
	  endif
	  write(6,*)'Will insert or append starting at index',index+1

  	write(6,*)ndays,' days of data already in file'
	call NCVGT1(ncidout,time_seriesid,NDSIZE,end_time,status)
   
	ncidout=ncopn(infile(1:nchin),ncwrite,status)

51      NCID=ncopn(updfile(1:nchup),NCNOWRIT,status)
	write(6,*)'opening ',updfile(1:nchup),' status=',status
	ivaridp=ncvid(ncid,'pmsl',status)
	ivaridt=ncvid(ncid,'time_series',status)
        CALL NCVINQ(ncid,ivaridt,DUMMY,NTP,NVDIM,VDIMS,NVS,
     1        status)
      CALL NCDINQ(NCID,ivaridt,DUMMY,NVS,status)
      CALL NCDINQ(NCID,VDIMS(1),DUMMY,NRECS,status)
C     !NRECS! NOW CONTAINS NUM RECORDS FOR THIS FILE
C
      START(1)=1
      COUNT(1)=NVS
      CALL NCVGT(NCID,ivaridt,START,COUNT,time_series,status)
	if(time_series(1)-end_time.gt.25)then
	  write(6,'('' Something is wrong with the file times.'',/,
     1    ''Gap between files is greater than 24 hours'',/,
     2    a, '': last time is:'',f13.0,/,
     3    a,'': first time is:'',f13.0,/,
     4    ''Do you want to continue? (Y/N)'',$)')
     5    infile(1:nchin),end_time,updfile(1:nchup),time_series(1)
          read(5,'(a)')ans
          if(ans.ne.'Y'.and.ans.ne.'y')stop
	endif
	do 450 iday=1,NRECS
	write(6,*)'Day=',iday
	     time_index=iday+index
            LENSTR=1
            START(1) = 1
            START(2) = 1
            START(3) = iday
            COUNT(1) = 180
            COUNT(2) = 92
            COUNT(3) = 1
        CALL NCVGT(NCID,ivaridp,START,COUNT,pmsl,status)

            START(3) = time_index
        CALL NCVPT(ncidout,P_MSLid,START,COUNT,pmsl,status)
	 tmp=time_series(iday)	 
	status=NF_PUT_VAR1_DOUBLE(ncidout,time_seriesid,time_index,tmp)     
        CALL NCVPT1(ncidout,time_seriesid,time_index,tmp,status)
C
450	continue	  
      CALL NCCLOS(NCID,status)
      call ncclos(ncidout,status)
C

      END
