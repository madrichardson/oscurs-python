
      PROGRAM pres_append
      INCLUDE '/usr/include/netcdf.inc'

      REAL*4 pmsl(180,92)


      PARAMETER (NVARS=23) !NUMBER OF VARIABLES
      PARAMETER (NREC=    14600)   !CHANGE THIS TO GENERALIZE
      REAL*8    time_series(NREC),tmp,end_time
      INTEGER time_seriesid,P_MSLid,status
      INTEGER*4 START(10),COUNT(10),VDIMS(10),time_index
      logical iopn,iexst
      character*80 infile,updfile


      status=NF_OPEN('oscurs_pressure_month_end.nc',NF_NOWRITE,
     1      ncidtmp)
      status=NF_INQ_VARID(ncidtmp,'time_series',idtmp)
c  get current number of days in file	
        status=NF_INQ_VARDIMID(ncidtmp,idtmp,VDIMS)
        status=NF_INQ_DIMLEN(ncidtmp,VDIMS(1),NDSIZE)
      index=NDSIZE
      status=NF_CLOSE(ncidtmp)
      nchin=18
      infile(1:nchin)='oscurs_pressure.nc'

      nchup=20
      updfile(1:nchup)='this_month_oscurs.nc'
      write(6,*)'Will update file ',infile(1:nchin),
     1  ' with data from file ',updfile(1:nchup)

      inquire(file=infile(1:nchin),opened=iopn,exist=iexst)
        if(.not.iexst)then
        write(6,*)'File to modify not found: ',infile(1:nchin)
        stop
      endif
      inquire(file=updfile(1:nchup),exist=iexst)
        if(.not.iexst)then
        write(6,*)'File to modify not found: ',updfile(1:nchup)
        stop
        endif
 
      status=NF_OPEN(infile(1:nchin),NF_WRITE,ncidout)
      status=NF_INQ_VARID(ncidout,'pmsl',P_MSLid)
      status=NF_INQ_VARID(ncidout,'time_series',time_seriesid)
c  get current number of days in file	
        status=NF_INQ_VARDIMID(ncidout,time_seriesid,VDIMS)
        status=NF_INQ_DIMLEN(ncidout,VDIMS(1),NDSIZE)
      ndays=NDSIZE
        write(6,*)'Will append starting at index',index+1

      write(6,*)ndays,' days of data already in file'
      status=NF_GET_VAR1_DOUBLE(ncidout,time_seriesid,NDSIZE,end_time)

51      status=NF_OPEN(updfile(1:nchup),NF_NOWRITE,NCID)
      write(6,*)'opening ',updfile(1:nchup),' status=',status
        status=NF_INQ_VARID(NCID,'pmsl',ivaridp)
        status=NF_INQ_VARID(NCID,'time_series',ivaridt)
        status=NF_INQ_VARDIMID(NCID,ivaridt,VDIMS)
        status=NF_INQ_DIMLEN(NCID,VDIMS(1),NRECS)
C     !NRECS! NOW CONTAINS NUM RECORDS FOR THIS FILE
C
      write(6,*)NRECS,' records in this_month_oscurs.nc'

        status=NF_GET_VAR_DOUBLE(NCID,ivaridt,time_series)
      if(time_series(1)-end_time.gt.25)then
        write(6,'('' Something is wrong with the file times.'',/,
     1    ''Gap between files is greater than 24 hours'',/,
     2    a, '': last time is:'',f13.0,/,
     3    a,'': first time is:'',f13.0)')
     5    infile(1:nchin),end_time,updfile(1:nchup),time_series(1)
          stop
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
           status=NF_GET_VARA_REAL(NCID,ivaridp,START,COUNT,pmsl)

            START(3) = time_index
      status=NF_PUT_VARA_REAL(ncidout,P_MSLid,START,COUNT,pmsl)
       tmp=time_series(iday) 
      status=NF_PUT_VAR1_DOUBLE(ncidout,time_seriesid,time_index,tmp)
C
450      continue 
      status=NF_CLOSE(NCID)
      status=NF_CLOSE(ncidout)
C

      END
