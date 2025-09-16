c --------------------------------------------------------------------
c  20010141
c  20001221 -- FTP'd from REFM3 (NMFS SEATTLE)
c  20000627 fix 12 years of dslp input ok, not more
c  20000627 fix 12 years of dslp input ok, not more
c  8-20-99 accept input of east longitudes to match ferret map program
c  8-1-99 y2k fix
C  OSCURS 1996-JUNE (CONVERSION TO UNIX FORTRAN)
c         1998- January
C    EXPANSION OF GRID FROM 40X64, 40X104, 66X160, AND NOW 92X180
C  11-12-95
C  12-03-95 RUNNING POLISHING
C  12-16-95 ADD WIND VECTORS ALONG TRAJECTORY FOR 1ST POINT
C---------------------------------------------------------------------
C---------------------------------------------------------------------
      COMMON/GRDCOM/CFACT,M,N,NM,XI(181),YJ(93)
      COMMON/LAT/ALAT2,LS2
      COMMON/LON/ALON2
      COMMON/SIZES/XLEN,YLEN,YYLEN,SIZ1H,SIZ1V,SIZ2H,SIZ2V,
     &SIZ3H,SIZ3V,SIZ4H,SIZ4V
      COMMON/NZMZ/NZ1,NZ2,MZ1,MZ2
      INCLUDE 'netcdf.inc'
      REAL*4 pmslout(92,180)
      INTEGER day1
      COMMON /netcdf/NCID,day1,idate_base,ivaridp
      LOGICAL LeapYr
      CHARACTER*4,A(5),B(2),C(2)
      CHARACTER H*2
      CHARACTER YR1A*4
      CHARACTER MSTA*4
      CHARACTER DSTA*4
      CHARACTER YR2A*4
      CHARACTER YR3A*4
      CHARACTER YR4A*4
      CHARACTER YR5A*4
      CHARACTER YR6A*4
      CHARACTER YR11A*4
      CHARACTER YR7A*4
      CHARACTER YR8A*4
      CHARACTER YR9A*4
      CHARACTER YR10A*4
      CHARACTER YR12A*4
      CHARACTER YR13A*4
      CHARACTER MSTA2*2
      CHARACTER DSTA2*2
      CHARACTER YR2A2*4
      CHARACTER YR3A2*4
      CHARACTER YR4A2*4
      CHARACTER YR5A2*4
      CHARACTER YR6A2*4
      CHARACTER DDIR*9/'~/KATUVS/'/
      CHARACTER HYYMM*6
      CHARACTER YYYYMMDDHH*10
      CHARACTER HDY*2/'dy'/
      CHARACTER HUV*2/'uv'/
c  unix
      CHARACTER GYR*7/'../out/'/
      CHARACTER GDSLP*23/'/disk46/jingraham/dslp/'/
      CHARACTER DSLPWWCTC*9/'dslpwwctc'/
      CHARACTER file29*23
      CHARACTER D63*21
C  DO 12 YEAR RUNS MAX
      CHARACTER GX1*27
      CHARACTER GX2*27
      CHARACTER GX3*27
      CHARACTER GX4*27
      CHARACTER GX5*27
      CHARACTER GX6*27
      CHARACTER GX7*27
      CHARACTER GX8*27
      CHARACTER GX9*27
      CHARACTER GX10*27
      CHARACTER GX11*27
      CHARACTER GX12*27
      CHARACTER GX13*27
      CHARACTER HDATE*8
      CHARACTER H00*2/'00'/
      CHARACTER H12*2/'12'/
      CHARACTER AWFX*2
      CHARACTER AANGF*3
      CHARACTER DOTTXT*4/'.txt'/
      CHARACTER HT92*4/'.t92'/
      CHARACTER HT10*4/'.t10'/
      CHARACTER HM10*4/'.m10'/
      CHARACTER H1YR*4/'.1yr'/
      CHARACTER H2YR*4/'.2yr'/
      CHARACTER H3YR*4/'.3yr'/
      CHARACTER H4YR*4/'.4yr'/
      CHARACTER H5YR*4/'.5yr'/
      CHARACTER H1MO*4/'.1mo'/
      CHARACTER H3MO*4/'.3mo'/
      CHARACTER H4MO*4/'.4mo'/
      CHARACTER H5MO*4/'.5mo'/
      CHARACTER H6MO*4/'.6mo'/
      CHARACTER H7MO*4/'.7mo'/
      CHARACTER H9MO*4/'.9mo'/
      CHARACTER HXMO*4/'.xmo'/
      CHARACTER H30D*4/'.30d'/
      CHARACTER HTST*4/'.tst'/
      CHARACTER HNSU*4/'.nsu'/
      CHARACTER HAM0*4/'.am0'/
      CHARACTER HAM3*4/'.am3'/
      CHARACTER H3D2*4/'.3d2'/
      CHARACTER H3D3*4/'.3d3'/
      CHARACTER HDA3*4/'.da3'/
      CHARACTER HAM5*4/'.am5'/
      CHARACTER HAMX*4/'.amx'/
      CHARACTER HAMY*4/'.amy'/
      CHARACTER HAMZ*4/'.amz'/
      CHARACTER HCM3*4/'.cm3'/
      CHARACTER HREV*4/'.rev'/
      CHARACTER HLA3*4/'.la3'/
      CHARACTER H965*4/'.965'/
      character space*1/' '/
      character yes1*3/'yes'/
      character yes2*3/'YES'/
      character yes3*3/'y  '/
      character yes4*3/'Y  '/
C  unix
      CHARACTER G60*18
      CHARACTER*16 PARFIL, DATFIL
      CHARACTER aokyes*3
      character*4 akyyyy
      character*2 akmm
      character*2 akdd
      character*2 akhh

c  unix name output file string
      character*45 cmd
      character*45 outfile
      character*18 outfile1
      character*14 outfile2
      character*13 outfile3
      character*42 outputfile
      character*8 ahdate
      integer*2 ispladd,isplamm, istmonthmm, istdaydd
      character*1 isplamma1,isplomma1
      character*2 isplamma,isplomma,isplamma2,isplomma2
      character*2 aconvi00/'00'/
      character*1 aconvi0/'0'/
      character*1 aconvi1(9)
      character*2 aconvi2(60)
      character*4 aconvi4(2028)
      integer*4 isplolll
      integer*4 iwfx
      integer*4 istyearyyyy, iangfac, idaysnnn
      character azero*1/'0'/
      character aone*1/'1'/
      character*1 par1
      character*3 ainwfx3
      character*4 adaysnnn
      character*4 ainang4
c>>>>>>>>>>>>>>>>>>>>add for command line input 
      character*3 chrlat
      character*5 chrlatmin
      character*4 chrlon
      character*5 chrlonmin
      character*4 chryr
      character*3 chrmon
      character*2 chrday
      character*3 mons(12)
      character*4 chrddfac
c<<<<<<<<<<<<<<<<<<<add for command line input 

      character*8 chr8date
      character*3 chr3fwfx
      character*4 chr4nnnn
      character*4 chr4angld
      character*4 dotout/'.out'/
      character*1 achN/'N'/
      character*1 achE/'E'/
      character*1 achy/'y'/
      character*1 achw/'w'/
      character*1 acha/'a'/
      character*1 achd/'d'/
c
c
      INTEGER time_index,time_index2
      INTEGER*2 NTRUP
      integer mdi(12)
      dimension idateseq(3,4382)
      data mdi/31,28,31,30,31,30,31,31,30,31,30,31/
      DIMENSION INTARY(9)
      DIMENSION LS(92,180),ALAT(20,44),ALON(20,44)
	DIMENSION DD2044(20,44),LS2044(20,44),DD92180(92,180),LS92180(92,1
     &60),DDTEMP(92,180),DDTEMP2(92,180)
      DIMENSION LSBC(92,180,2),BCI(92,180,2),BCJ(92,180,2),LSDBC(1000,2)
      DIMENSION SLP1(20,44),SLP2(26,48),SLP3(92,180),ISLP3(92,180),
     *LSDBC2(100,2)
      DIMENSION ALAT2(92,180),ALON2(92,180),SLP5X5(14,29,34)
      DIMENSION ALAT3(92,180),ALON3(92,180)
      DIMENSION LS2(92,180),LS1(92,180),LS9(92,180),LS3(92,180)
      DIMENSION OCUW(92,180),OCVW(92,180)
      DIMENSION CSPEED(92,180),DDSPEED(92,180),AKSPEED(92,180)
      DIMENSION WXX(92,180),WYY(92,180),AKSPEED2(92,180)
      DIMENSION CSLP(2560)
      DIMENSION T2(92,180)
      DIMENSION DEPTH(4),NDATA(4),BATH(3,1000,4)
      DIMENSION OCUS(92,180),OCVS(92,180)
      DIMENSION OCUDD(92,180),OCVDD(92,180)
      DIMENSION GU1(92,180),GV1(92,180),GU2(92,180),GV2(92,180)
      DIMENSION IPV(500),JPV(500)
      DIMENSION GIPV(500),GJPV(500)
      DIMENSION ASP(10,100),AKTS(10),AMPS(10)
      DIMENSION CC(72),ANGDEF(30),SPJ(30)
      DIMENSION IAK(200), JAK(200), DDAK(200)
      DIMENSION PLO(2000),PLA(2000),IPEN(2000),P28LO(400),P28LA(400)
      DIMENSION C1(10000),C2(10000),C3(10000),ALATSF(14)
      DIMENSION DUM(92,180)
      DIMENSION ATS(80),JSTN(40),YRXY(2,100)
      DIMENSION WU16(16,400),WV16(16,400),WS16(16,400),WA16(16,400),IYMD
     *(400,3)
      DIMENSION ROUND(11,2)
      DIMENSION DDDUM(180),ICE(92,180,12)
      DIMENSION GLA1C(10,10),GLO1C(10,10),ADISTA(10,10)
      DATA B(1)/', MO'/,B(2)/'NTH='/
      DATA A(1)/'    '/,A(2)/'SLP '/,A(3)/'   Y'/,A(4)/'EAR='/
     &,A(5)/'    '/
      DATA C(1)/', DA'/,C(2)/'Y=  '/
      DATA ALATSF/67.0,16.0,75.0,-1.0,78.0,8.0,
     &78.4,11.0,77.9,7.5,75.0,-4.0,67.0,7.5/
      DATA JSTN/0,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,
     *9,0,1,2,3,4,5,6,7,8,9,0/
     	data mons/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     1 'SEP','OCT','NOV','DEC'/
C
c  I to A conversion data statement
c23456789012345678901234567890123456789012345678901234567890123456789012
      data aconvi1/'1','2','3','4','5','6','7','8','9'/
      data aconvi2/'01','02','03','04','05','06','07','08','09','10','11
     *','12','13','14','15','16','17','18','19','20','21','22','23','24'
     *,'25','26','27','28','29','30','31','32','33','34','35','36','37',
     *'38','39','40','41','42','43','44','45','46','47','48','49','50','
     *51','52','53','54','55','56','57','58','59','60'/
       
      NN=20
      MM=44
C      NM=NN*MM
      NN2=92
      MM2=180
C  RANDOM ANGLE CONSTANT SPEED WALK DIFFUSION
CRANDUM=RANDS(5.0)
           RANDUM=RAND(0)
C
C  ---------------------------------------------------------------------
c
C  ---------------------------------------------------------------------
C  ---------------------------------------------------------------------
C
C          OSCURS"  MODEL  (92X180)
C
C     (OCEAN SURFACE CURRENT SIMULATIONS)
C
c  welcome
c
c
c      WRITE(*,*)' Welcome to OSCURS (Ocean Surface CURrent Simulations)' 
c      WRITE(*,*)' Numerical Model of the North Pacific Ocean-Bering Sea'
c      WRITE(*,*)' Version Unix.98.1 by Jim Ingraham (Oceanographer)'
c      WRITE(*,*)' NOAA-NMFS-Alaska Fisheries Science Center'
c      WRITE(*,*)' Seattle, WA 98115-0070'
c      WRITE(*,*)' (206)526-4241 (jim.ingraham@noaa.gov)  '
c      WRITE(*,*)' -----------------------------------------------------'
c      WRITE(*,*)' To create an Ocean Surface Current trajectory:  '    
c      WRITE(*,*)'    1. Type a +latitude N, +longitude W start point,'
c      WRITE(*,*)'    2. Type a 00Z start date (1946 to 1996), '
c      WRITE(*,*)'    3. Type number of days in the trajectory (<365)'
c      WRITE(*,*)' -----------------------------------------------------'
c      WRITE(*,*)' -----------------------------------------------------'
c      WRITE(*,*)' To skip manual typing in data and go to file ------'
c      WRITE(*,*)' ---set 1st command line variable to integer 1------'
c      WRITE(*,*)' -------set up input file 11 with data-----------'
c      WRITE(*,*)' -----------------------------------------------------'
c  os4 skip
c      CALL get_command(cmd)
c     call get_command_argument(1,par1)
c      write(6,*)cmd
c      write(6,95608)par1
c      nar=iargc()
c      write(6,95607)nar
c      write(9,95607)nar
95607 format(' No. of arguments input on the command line (nar) is ',i2)
c      write(9,95608)par1
95608 format(' a-value of par1=',a1)
C  --------------------------------------------------------------------- 
c  input file of start data (alternative to typing in data)
c      OPEN(8,file='osc2000outfilenames.out',STATUS='unknown')
c
99966 continue
      rewind(4)

c>>>>>>>>>>>>>>>>>>>>add for command line input 
	call getarg(2,chrlat)
	call getarg(3,chrlatmin)
	call getarg(4,chrlon)
	call getarg(5,chrlonmin)
	call getarg(6,chryr)
	call getarg(7,chrmon)
	call getarg(8,chrday)
	call getarg(9,chr4nnnn)
	call getarg(10,chr3fwfx)
	call getarg(11,chr4angld)
	call getarg(12,chrddfac)
	call getarg(13,outfile)

	read(chrlat,'(f3.0)')decdegla
	read(chrlatmin,'(f5.0)')decminla
	read(chrlon,'(f4.0)')decdeglo
	read(chrlonmin,'(f5.0)')decminlo
	read(chryr,'(i4)')iyyyy
	do 95601 imm=1,12
95601	if(chrmon.eq.mons(imm))go to 95602
c	write(6,*)'month not valid'
	stop
95602	read(chrday,'(i2)')idd
        write(chr8date,'(i4,2i2.2)')iyyyy,imm,idd
        read(chr4nnnn,'(i4)')nnnn
	write(chr4nnnn,'(i4.4)')nnnn
        read(chr3fwfx,'(f3.0)')decfwfx
        read(chr4angld,'(f4.0)')decangld
	write(chr3fwfx,'(i3.3)')int(decfwfx*100)
        write(chr4angld,'(i4.4)')int(decangld*100)
	if(decangld.eq.0)chr4angld='0000'
        read(chrddfac,'(f4.0)')DDFAC
c	write(6,*)'ddfac=',DDFAC
c>>>>>>>>>>>>>>>>>>>>add for command line input 


95605 format(1x,f2.0,f5.1,f4.0,f5.1,1x,i4,1x,i2,1x,i2,1x,a8,i5,1x,a4,
     *f4.1,1x,a3,f5.1,1x,a4)
95604 format(1x,f3.0,f5.1,f4.0,f5.1,1x,i4,1x,i2,1x,i2,1x,a8,i5,1x,a4,
     *f4.1,1x,a3,f5.1,1x,a4)
c  stop check if lat or long = zero
      if(decdegla.eq.0.0.or.decdeglo.eq.0.0)go to 99967
            spladd=decdegla
      splamm=decminla
       isplamm=decminla
       ispladd=spladd
        spla=spladd+(splamm/60.0)
      splolll=decdeglo
      splomm=decminlo
       isplomm=decminlo
      isplolll=splolll
        splo=splolll+(splomm/60.0)
      istyearyyyy=iyyyy
         iyr=istyearyyyy
         iyrslp=iyr
      istmonthmm=imm
         imo=istmonthmm
         imstrt=imo
      istdaydd=idd
         idy=istdaydd
         idstrt=idy
      idaysnnn=nnnn
         ndays=idaysnnn
      ahdate=chr8date
      adaysnnn=chr4nnnn
      ainwfx=decfwfx
      ainwfx3=chr3fwfx
        wfx=decfwfx
        iwfx=decfwfx*100.0
      ainang=decangld
      ainang4=chr4angld
      ANGFAC=decangld
c
c  write choices of start data to screen
       continue
       WRITE(6,*) '{"latitude":', spla,','
       WRITE(6,*) '"longitude":', splo,','
       WrITE(6,*) '"year":', istyearyyyy,','
       WRITE(6,*) '"month":', istmonthmm,','
       WRITE(6,*) '"day":', istdaydd,','
       WRITE(6,*) '"ndays":',idaysnnn,','
       WRITE(6,*) '"wind_fac_wfx":',wfx,',' 
       WRITE(6,*) '"wind_fac_ainwfx3":',ainwfx3 ,','
       WRITE(6,*) '"angle_deflector":',ainang,',"track":[' 
c       WRITE(*,*) ' Is this O.K. ?? (exactly {yes} OR {no})'
c
c        write(outputfile,'("output/oscurs_",i4.4,i5.5,"_",i4,i2.2,
c     1      i2.2,"_",i5.5,".csv")')int(spla*100),int(splo*100),
c     1      istyearyyyy,istmonthmm,istdaydd,idaysnnn

       write(outputfile,'("output/",a35)')outfile(1:35)
       OPEN(67,file=outputfile,STATUS='UNKNOWN')
       write(67,74105)
74105 format('year-mm-dd,Lat(N),Lon(E)')
95606 continue
c=====================================================================
c=====================================================================
c=====================================================================
c  inputs received now set up model run
c=====================================================================

c
c  change Web input longitudes East to longitudes West
c  model wants to run with west longitudes
      splo1=splo
c      splo=(360.0-splo)
c
c
C  ---------------------------------------------------------------------
      OPEN(4,file='osc92180.inp',STATUS='OLD')
c      OPEN(9,file='osc2000a.out',STATUS='unknown')
      OPEN(33,file='uvtest.out',STATUS='unknown',ACCESS='SEQUENTIAL') 
      OPEN(12,file='nmbrsglf.da2',STATUS='OLD',ACCESS='SEQUENTIAL')
      OPEN(28,file='p92180xx.dat',STATUS='old',ACCESS='SEQUENTIAL')
      OPEN(39,file='dyncurco.dat',STATUS='OLD',ACCESS='SEQUENTIAL')
      OPEN(50,file='dyncurs2.dat',STATUS='OLD',ACCESS='SEQUENTIAL')
      OPEN(51,file='st90051.cmp',STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(52,file='st90052.cmp',STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(53,file='st90053.cmp',stATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(54,file='st90054.cmp',STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(55,file='st90055.cmp',sTATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(80,file='pij92.dat',STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(81,file='pij92new.dat',STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(10,file='oscur92b.out',STATUS='unknown',ACCESS='SEQUENTIAL')
      OPEN(37,file='1901-2028.txt',STATUS='old',ACCESS='SEQUENTIAL')
C
C  ---------------------------------------------------------------------
C  ---------------------------------------------------------------------
C  SET UP GRID
C  ---------------------------------------------------------------------
C  SET UP FNOC GRID (LAT-LONG,LAND-SEA)
C  READ IN LAT-LONG OF 20X44 GRID (FNWC GRID POINTS)
      IG=2044
      DO 99462 J=1,4
      J2=J*11
      J1=J2-10
      DO 99461 I=1,20
      
      READ(4,99469)IG,IK,JR1,JR2,(ALAT(I,JK),JK=J1,J2)
99461 CONTINUE
99462 CONTINUE
      DO 99463 J=1,4
      J2=J*11
      J1=J2-10
      DO 99464 I=1,20
      READ(4,99469)IG,IK,JR1,JR2,(ALON(I,JK),JK=J1,J2)
99464 CONTINUE
99463 CONTINUE
99469 FORMAT (I5,I3,2I3,11F9.4)
C  SKIP READING 100 LINES OF DD20X44 IN INPUT FILE 5
      DO 99465 ISK=1,100
      READ(4,99469)IG
99465 CONTINUE
C  STOP TEMPORARY
C      IF(1.EQ.1)GO TO 902
C
C  LOOK AT GRID DISTANCES NEAR 170 W (WHERE GRID IS ALONG A MERIDIAN)
C  TEST GRID LENGTH FUNCTION (GRIDLN) VS. LATITUDE
      NBR=0
      SUM1=0.0
      SUM2=0.0
      SUM3=0.0
      SUM4=0.0
      DO 308 I=2,19
      DAA=ALAT(I,22)-ALAT(I+1,22)
      DAA2=ALAT(I-1,22)-ALAT(I+1,22)
      DLAT=ALAT(I,22)-(DAA/2.0)
      DLAT2=ALAT(I,22)
      DAANM=DAA*60.0
      DAANM2=DAA2*60.0
      DAANM2=DAANM2/2.0
      DAAKM=DAANM*1.852
      DAAKM2=DAANM2*1.852
      AKM=(1.0+(SIN(DLAT*3.14159/180.0)))/1.86603
      AKM2=(1.0+(SIN(DLAT2*3.14159/180.0)))/1.86603
      GD60=DAAKM/AKM
      GD602=DAAKM2/AKM2
      NBR=NBR+1
      SUM1=SUM1+DAAKM
      SUM2=SUM2+GD60
      SUM3=SUM3+DAAKM2
      SUM4=SUM4+GD602
  308 CONTINUE
      AV1=SUM1/FLOAT(NBR)
      AV2=SUM2/FLOAT(NBR)
      AV3=SUM3/FLOAT(NBR)
      AV4=SUM4/FLOAT(NBR)
  307 FORMAT(I5,20X,7F11.6)
  299 FORMAT(11F11.6)
      DO 65 N=1,56
      GLA=FLOAT(71-N)
      GG=GDFNOC(GLA)
      GGNM=GG/1.852
   65 CONTINUE
C
C  STOP TEMPORARY
C      IF(1.EQ.1)GO TO 902
C
C  ICE == CONTINUE READING FILE 44 FOR BS ICE DATA
c  ice  bering sea input by month  11, 12, 1, 2, 3, 4, AND 5
C  ZERO OUT ARRAY "ICE"
      DO 22211 I=1,92
      DO 22211 J=1,180
      ICE(I,J,1)=0
      ICE(I,J,2)=0
      ICE(I,J,3)=0
      ICE(I,J,4)=0
      ICE(I,J,5)=0
      ICE(I,J,6)=0
      ICE(I,J,7)=0
      ICE(I,J,8)=0
      ICE(I,J,9)=0
      ICE(I,J,10)=0
      ICE(I,J,11)=0
      ICE(I,J,12)=0
22211 CONTINUE
C
C  TO CONVERT FROM 66X160 TO 92X180 GRIDS USE I+13 AND J+17
      OPEN(44,file='updtbsdd.da5',STATUS='old')
      DO 91245 IDUM=1,72
      READ(44,91246,END=1249)AIMO44
91245 CONTINUE
91246 FORMAT(A5)
      DO 1245 KICE=1,7
      READ(44,1248,END=1249)IMO44
      DO 1246 IICE=1,12
      IIICE=IICE+13
      READ(44,1247,END=1249)(ICE(IIICE,JICE,IMO44),JICE=76,104)
 1246 CONTINUE
 1245 CONTINUE
 1249 CONTINUE
 1248 FORMAT(I3)
 1247 FORMAT(I3,2X,29I2)
 1240 CONTINUE
C  STOP TEMPORARY
C      IF(1.EQ.1)GO TO 902
c
c  read in number file for converting i to a format years(i4)>(a4)
      do 35023 j=1,128
      read(37,35020,end=35029)aconvi4(1900+j)
35023 continue
35020 format(a4)
35029 continue
C
C  -- -------------------------------------------------------------------
C  SET UP MODEL (92X180) GRID AND LAND-SEA TABLES
C  ---------------------------------------------------------  ------------
C  FILL LANDSEA TABLE FOR SLP COMPUTATIONS (ALL=1)
      DO 211 I=1,92
      DO 211 J=1,180
      AKSPEED(I,J)=0.0
      AKSPEED2(I,J)=0.0
      LSBC(I,J,1)=0
      LSBC(I,J,2)=0
  211 LS1(I,J)=1
C
C  READ NEW GRID 92X180 LATITUDES, LONGITUDES FROM INPUT FILE 4
C
c      WRITE(9,68472)
c      WRITE(6,68472)
68472 FORMAT(' START OF READ NEW 92X180 GRID LONGS.   ')
      IG=92180
      DO 68462 J=1,18
      J2=J*10
      J1=J2-9
      DO 68461 I=1,92
      READ(4,68469)IG,IK,JR1,JR2,(ALAT2(I,JK),JK=J1,J2)
68461 CONTINUE
68462 CONTINUE
      DO 68463 J=1,18
      J2=J*10
      J1=J2-9
      DO 68464 I=1,92
      READ(4,68469)IG,IK,JR1,JR2,(ALON2(I,JK),JK=J1,J2)
68464 CONTINUE
68463 CONTINUE
68469 FORMAT (I5,I3,2I3,12F9.4)
c      WRITE(9,68471)
c      WRITE(6,68471)
68471 FORMAT(' END OF READ NEW 92X180 GRID LONGS.   ')
      DO 80141 I=1,92
      DO 80142 J=1,180
      ALAT3(I,J)=ALAT2(I,J)
      ALON3(I,J)=ALON2(I,J)
80142 CONTINUE
80141 CONTINUE
C
CC  LOOK AT GRID DISTANCES NEAR 170 W (WHERE GRID IS ALONG A MERIDIAN)
C  TEST (92X180) GRID LENGTH FUNCTION (GRIDLN) VS. LATITUDE
      NBR=0
      SUM1=0.0
      SUM2=0.0
      SUM3=0.0
      SUM4=0.0
c  os3      DO 56 I=2,91,10
	DO 56 I=2,91,91
	DAA=ALAT2(I,90)-ALAT2(I+1,90)
      DAA2=ALAT2(I-1,90)-ALAT2(I+1,90)
      DLAT=ALAT2(I,90)-(DAA/2.0)
      DLAT2=ALAT2(I,90)
      DAANM=DAA*60.0
      DAANM2=DAA2*60.0
      DAANM2=DAANM2/2.0
      DAAKM=DAANM*1.852
      DAAKM2=DAANM2*1.852
      AKM=(1.0+(SIN(DLAT*3.14159/180.0)))/1.86603
      AKM2=(1.0+(SIN(DLAT2*3.14159/180.0)))/1.86603
      GD60=DAAKM/AKM
      GD602=DAAKM2/AKM2
C
      NBR=NBR+1
      SUM1=SUM1+DAAKM
      SUM2=SUM2+GD60
      SUM3=SUM3+DAAKM2
      SUM4=SUM4+GD602
   56 CONTINUE
      AV1=SUM1/FLOAT(NBR)
      AV2=SUM2/FLOAT(NBR)
      AV3=SUM3/FLOAT(NBR)
      AV4=SUM4/FLOAT(NBR)
c  os3      WRITE(9,61)
   61 FORMAT('1GRID LENGTH (KM) VS. LATITUDE'//)
      GLA=FLOAT(85-N)
      GG=GRIDLN(GLA)
      GGNM=GG/1.852
c  os3      WRITE(9,62)N,GLA,GG,GGNM
   60 CONTINUE
   62 FORMAT(I4,'LATITUDE=',F6.2,'   GRID LENGTH=',F11.6,' KM',5X,
     'F11.6,' NM')
C
C  CREATE NEW GRID LAND-SEA TABLE
C  READS FILE 40X104 ADDED TO ALL 2'S ON 92X180 (CREATED IN OSCRBSNP.FO
      OPEN(13,file='ls292180.dat',STATUS='unknown',ACCESS='SEQUENTIAL')
      DO 140 I=1,92
      DO 141 JR=1,6
      KKJR2=JR*30
      KKJR1=KKJR2-29
65220 format(' testls2 ',5i5)
      READ(13,439,END=99439)IK,KJR1,KJR2,(LS2(I,JRN),JRN=KKJR1,KKJR2)
  141 CONTINUE
  140 CONTINUE
  439 FORMAT(I3,2I4,40I2)
99439 CONTINUE
 1514 FORMAT('1 PRELIMINARY LAND-SEA 2, (92X180)'//)
C  PRINT LAND-SEA
 1519 FORMAT(' ',20I3)
 1516 FORMAT(1H ,60I1,I5)
c      WRITE(9,110)
  110 FORMAT('1LANDSEA 3'//)
      DO 220 I=1,NN2
      DO 221 J=1,MM2
      LS3(I,J)=LS2(I,J)
C  LS2 MODIFIED
C      IF(J.EQ.124.AND.I.LE.17)LS3(I,J)=0
      IF(J.EQ.141.AND.I.LE.30)LS3(I,J)=0
      IF(J.EQ.142.AND.I.LE.30)LS3(I,J)=0
      IF(J.EQ.143.AND.I.LE.30)LS3(I,J)=0
      IF(J.EQ.144.AND.I.LE.30)LS3(I,J)=0
      IF(J.EQ.145.AND.I.LE.31)LS3(I,J)=0
      IF(J.EQ.146.AND.I.LE.31)LS3(I,J)=0
      IF(J.GE.147.AND.I.LE.31)LS3(I,J)=0
  221 CONTINUE
C  CORRECT LAND-SEA TABLE (LS3)
C     LS3(4,85)=0
      LS3(17,102)=0
      LS3(17,108)=0
      LS3(18,101)=0
      LS3(18,102)=0
C      WRITE(9,1509) (LS3(I,JK),JK=1,80),I
  220 CONTINUE
 1509 FORMAT(1H ,80I1,I3)
 1510 FORMAT(1H ,75I1,I3)
C
C  TEMPORARY LAND-SEA TABLE = ALL 1'S
c      WRITE(9,110)
      DO 1506 I=1,92
      DO 1507 J=1,180
      LS1(I,J)=1
      LS9(I,J)=0
 1507 CONTINUE
 1506 CONTINUE
C  PRINT LS3 LONGWAYS ON PAGE
c  os3      WRITE(6,1614)
 1614 FORMAT('1  LAND-SEA 3, (92X180)'//)
C  PRINT LAND-SEA
c  os3      WRITE(6,1619)(NW,NW=1,60,3)
C      DO 1615 I=1,NN2
C      WRITE(6,1616)(LS3(I,J),J=1,60),I
 1615 CONTINUE
 1619 FORMAT(' ',20I3)
 1616 FORMAT(1H ,60I1,I5)
c  os3      WRITE(6,1619)(NW,NW=61,120,3)
C      DO 1635 I=1,NN2
C      WRITE(6,1616)(LS3(I,J),J=61,120),I
 1635 CONTINUE
c  os3      WRITE(6,1619)(NW,NW=121,180,3)
C      DO 1645 I=1,NN2
C      WRITE(6,1616)(LS3(I,J),J=121,180),I
 1645 CONTINUE
C
C  STOP TO PRINT LS3
C      IF(1.EQ.1)GO TO 902
C
C  >>>>>> SET UP PLOT GRAPHICS
C  NEED MAP FOR BOUNDARY CONTROL
	OPEN(16,file='stgridr.dat',STATUS='UNKNOWN',
     &ACCESS='SEQUENTIAL')
	XLN=9.5
      YLN=7.0
c      WRITE(6,9906)
c      WRITE(9,9906)
 9906 FORMAT(' CALL STGRID HERE')
	CALL STGRIDr(NN2,MM2,ALAT2,ALON2,LS1,XLN,YLN)
c	WRITE(6,99906)
c      WRITE(9,99906)
99906 FORMAT(' END STGRID HERE')
C
C  TEST CONVIJ
      XLEN=XLN
      YYLEN=YLN-1.0
      TLON=223.8
      TLAT=33.1
      JTLL=100
      JTLL=91
c  os3      DO 7722 ITLL=1,92,10
	DO 7722 ITLL=1,92,92
	TLAT=ALAT2(ITLL,JTLL)
      TLON=ALON2(ITLL,JTLL)
      XX=TLON
      YY=TLAT
 7722 CONTINUE
C
C  READ IN SLIP BOUNDARY CONDITIONS (LS AND I'S AND J'S)
      OPEN(77,file='slbcij92.da2',STATUS='UNKNOWN')
C
C  ONE PAIR OF (X,Y) POINTS IN EACH GRID SQUARE ALONG LAND BOUNDARY
C  DETERMINES A CONTINUOUS BOUNDARY LINE CONNECTING ALL THE CHOSEN
C   SQUARES.  ALL POINTS ARE ON GRID SQUARE BOUNDARIES
C  LS=1 MEANS KEEP TRAJECTORY BELOW THE BOUNDARY LINE (INCREASING J)
C  LS=2 MEANS KEEP TRAJECTORY ABOVE THE BOUNDARY LINE (DECREASING J)
C  SLIP BOUNDARY MEANS ANY TRAJECTORY POINT BEYOND BOUNDARY IS PUT
C  BACK IN BOUNDS JUST INSIDE THE BOUNDARY LINE BY "DLINE."
C  USES POINT SLOPE FORMULA FOR THE LINE NORMAL TO THE BOUNDARY LINE.
      DLINE1=0.15
      DLINE=DLINE1
C  SAMPLE OF DATA.......
C    LS1 I  J (LAND/SEA)                     I           J
C   1 1 13 159 PLDOT NBG(I,J)=  1  1 BGI=  13.56 BGJ= 159.00 INCH=10.33 5.65
C  ACTUAL FILE 77
C   2 1 13 159 PLDOT NBG(I,J)=  1  1 BGI=  13.56 BGJ= 160.00 INCH=10.33 5.65
C  NEXT PAIR OF POINTS IN ADJACENT BOUNDARY GRID SQUARE
C   3 1 13 158 PLDOT NBG(I,J)=  2  1 BGI=  13.56 BGJ= 159.00 INCH=10.33 5.65
C   4 1 13 158 PLDOT NBG(I,J)=  2  2 BGI=  13.00 BGJ= 158.16 INCH=10.28 5.71
C  NEXT PAIR OF POINTS IN ADJACENT BOUNDARY GRID SQUARE
C   5 1 12 158 PLDOT NBG(I,J)=  2  1 BGI=  13.00 BGJ= 158.16 INCH=10.28 5.71
C   6 1 12 158 PLDOT NBG(I,J)=  2  2 BGI=  12.83 BGJ= 158.00 INCH=10.27 5.73
C   7 1 12 157 PLDOT NBG(I,J)=  2  1 BGI=  12.83 BGJ= 158.00 INCH=10.27 5.73
C   8 1 12 157 PLDOT NBG(I,J)=  2  2 BGI=  12.20 BGJ= 157.00 INCH=10.20 5.79
C   9 1 12 156 PLDOT NBG(I,J)=  2  1 BGI=  12.20 BGJ= 157.00 INCH=10.20 5.79
C  10 1 12 156 PLDOT NBG(I,J)=  2  2 BGI=  12.41 BGJ= 156.00 INCH=10.14 5.77
C23456789012345678901234567890123456789012345678901234567890123456789012
 5000 CONTINUE
      READ(77,5090,END=5091)NREC,LSUD,I,J,NBC,IP1,BCII,BCJJ,XP2IN,YP2IN
 5090 FORMAT(I4,I2,I3,I4,16X,2I3,5X,F7.2,5X,F7.2,6X,2F5.2)
 5095 FORMAT(' SLIP BC=',I4,I2,I3,I4,16X,2I3,5X,F7.2,5X,F7.2,6X,2F5.2)
C  92X180
C  SAVE I,J'S OF COASTLINE GRID SQUARES
C  TO LOCATE SQUARES WITH DIFFERENT DIFFUSION VALUES
      LSDBC(NREC,1)=I
      LSDBC(NREC,2)=J
      LS9(I,J)=9
      LS9(I+1,J)=8
      LS9(I+2,J)=7
      IF((I-2).LT.1)GO TO 5088
      IF((I-1).LT.1)GO TO 5088
      IF((I+3).GT.60)GO TO 5088
      IF((I+2).GT.60)GO TO 5088
      IF((I+1).GT.60)GO TO 5088
      IF(LS9(I+7,J).EQ.0.AND.J.GT.120)LS9(I+7,J)=2
      IF(LS9(I+6,J).EQ.0.AND.J.GT.120)LS9(I+6,J)=3
      IF(LS9(I+5,J).EQ.0.AND.J.GT.120)LS9(I+5,J)=4
      IF(LS9(I+4,J).EQ.0.AND.J.GT.120)LS9(I+4,J)=5
      IF(LS9(I+3,J).EQ.0.AND.J.GT.120)LS9(I+3,J)=6
      IF(LS9(I-2,J).EQ.0)LS9(I-2,J)=7
      IF(LS9(I-1,J).EQ.0)LS9(I-1,J)=8
 5088 CONTINUE
c  pfeg debug
      IF((J-7).LT.1)GO TO 5089
      IF((J-2).LT.1)GO TO 5089
      IF((J-1).LT.1)GO TO 5089
      IF((J+2).GT.180)GO TO 5089
      IF((J+1).GT.180)GO TO 5089
      IF(LS9(I,J-7).EQ.0.AND.J.GT.120)LS9(I,J-7)=2
      IF(LS9(I,J-6).EQ.0.AND.J.GT.120)LS9(I,J-6)=3
      IF(LS9(I,J-5).EQ.0.AND.J.GT.120)LS9(I,J-5)=4
      IF(LS9(I,J-4).EQ.0.AND.J.GT.120)LS9(I,J-4)=5
      IF(LS9(I,J-3).EQ.0.AND.J.GT.120)LS9(I,J-3)=6
      IF(LS9(I,J-2).EQ.0)LS9(I,J-2)=7
      IF(LS9(I,J-1).EQ.0)LS9(I,J-1)=8
      IF(LS9(I,J+1).EQ.0)LS9(I,J+1)=8
      IF(LS9(I,J+2).EQ.0)LS9(I,J+2)=7
 5089 CONTINUE
C
C
      LS3(I,J)=9
      LSBC(I,J,1)=LSUD
      BCI(I,J,1)=BCII
      BCJ(I,J,1)=BCJJ
      DOTX=BCJJ
      DOTY=BCII
      CALL IJ2INCHS(DOTX,DOTY,XP1IN,YP1IN,IERR)
      READ(77,5090,END=5091)NREC,LSUD,I,J,NBC,IP1,BCII,BCJJ,XP2IN,YP2IN
      LS3(I,J)=9
      LSBC(I,J,2)=LSUD
      BCI(I,J,2)=BCII
      BCJ(I,J,2)=BCJJ
      DOTX=BCJJ
      DOTY=BCII
c  pfeg debug
c      WRITE(6,5090)NREC,LSUD,I,J,NBC,IP1,BCII,BCJJ,XP2IN,YP2IN
      GO TO 5000
 5091 CONTINUE
C  ADD SOME ISLAND POINTS INTO DIFFUSION ARRAY
      LSDBC2(1,1)=16+13
      LSDBC2(1,2)=83+17
      LSDBC2(2,1)=17+13
      LSDBC2(2,2)=82+17
      LSDBC2(3,1)=17+13
      LSDBC2(3,2)=81+17
      LSDBC2(4,1)=17+13
      LSDBC2(4,2)=80+17
      LSDBC2(5,1)=18+13
      LSDBC2(5,2)=79+17
      LSDBC2(6,1)=18+13
      LSDBC2(6,2)=78+17
      LSDBC2(7,1)=18+13
      LSDBC2(7,2)=77+17
      LSDBC2(8,1)=18+13
      LSDBC2(8,2)=76+17
      LSDBC2(9,1)=18+13
      LSDBC2(9,2)=75+17
      LSDBC2(10,1)=18+13
      LSDBC2(10,2)=74+17
      LSDBC2(11,1)=18+13
      LSDBC2(11,2)=73+17
      LSDBC2(12,1)=18+13
      LSDBC2(12,2)=72+17
      LSDBC2(13,1)=18+13
      LSDBC2(13,2)=71+17
      LSDBC2(14,1)=17+13
      LSDBC2(14,2)=70+17
      LSDBC2(15,1)=17+13
      LSDBC2(15,2)=69+17
      LSDBC2(16,1)=17+13
      LSDBC2(16,2)=68+17
      LSDBC2(17,1)=16+13
      LSDBC2(17,2)=67+17
      LSDBC2(18,1)=15+13
      LSDBC2(18,2)=66+17
      LSDBC2(19,1)=14+13
      LSDBC2(19,2)=65+17
      LSDBC2(20,1)=13+13
      LSDBC2(20,2)=64+17
      LSDBC2(21,1)=13+13
      LSDBC2(21,2)=63+17
      LSDBC2(22,1)=12+13
      LSDBC2(22,2)=62+17
      LSDBC2(23,1)=13+13
      LSDBC2(23,2)=54+17
      LSDBC2(24,1)=14+13
      LSDBC2(24,2)=53+17
      LSDBC2(25,1)=14+13
      LSDBC2(25,2)=52+17
      LSDBC2(26,1)=14+13
      LSDBC2(26,2)=51+17
      LSDBC2(27,1)=14+13
      LSDBC2(27,2)=50+17
      LSDBC2(28,1)=14+13
      LSDBC2(28,2)=49+17
      LSDBC2(29,1)=14+13
      LSDBC2(29,2)=48+17
      LSDBC2(30,1)=14+13
      LSDBC2(30,2)=47+17
c
      DO 5081 NBC=1,30
c  pfeg debug
      IF(LSDBC2(NBC,1).EQ.0)GO TO 15081
      LI=LSDBC2(NBC,1)
      LJ=LSDBC2(NBC,2)
      LS9(LI-2,LJ)=7
      LS9(LI-1,LJ)=8
      LS9(LI,LJ)=9
      LS9(LI+1,LJ)=8
      LS9(LI+2,LJ)=7
 5081 CONTINUE
c  pfeg debug
15081 CONTINUE
C
C  PASS THROUGH LS9 AGAIN TO COVER BE SURE POINT AT COAST HAS A 9
      NBC=1
 5085 CONTINUE
      IF(LSDBC(NBC,1).EQ.0)GO TO 5082
      LI=LSDBC(NBC,1)
      LJ=LSDBC(NBC,2)
      LS9(LI,LJ)=9
C      IF(LS9(LI-1,LJ).EQ.0)LS9(LI-1,LJ)=8
      NBC=NBC+1
      GO TO 5085
 5082 CONTINUE
C
C  PRINT LS9 LONGWAYS ON PAGE
c      WRITE(9,1714)
 1714 FORMAT('1  LAND-SEA 9, (92X180)'//)
C  PRINT LAND-SEA
C  ---------------------------------------------------------------------
C  INPUT STARTING LOCATIONS
C  ---------------------------------------------------------------------
C  OSCURS95 START POINTS
C  READ INITIAL I,J OF EACH PROGRESSIVE VECTOR PLOT
      KK=1
 1145 FORMAT(2I5,1X,2F7.3,1X,F6.3,F7.3,4X,2F10.6)
 1147 FORMAT(3I5,1X,2F7.3,2X,F5.2,F6.2,5X,2F10.6)
C
C  SKIP TEST
c      IF(1.EQ.1)GO TO 1199
C
C  TEST INPUT POINTS FOR SPECIFIED LAT AND LONG FOR CORRECT IJ
C  LAT-LONG ARE PRIMARY CONTROL
C  NOT GRID I...J...
c  locations already input from input file not old file (28)
      plon=splo
      plat=spla
      XX=PLON
      YY=PLAT
C      CALL CONVIJ (XX,YY,IERR,XLEN,YYLEN,MM2,NN2)
C  COMPUTE NEW I,J FROM LAT,LONG
      CALL CONVIJ (XX,YY,IERR)
c      WRITE(6,13011)splo,spla,XX,YY,IERR
c      WRITE(9,13011)splo,spla,XX,YY,IERR
            
13011 FORMAT(' convij ',2f7.3,'XX=',F10.5,' YY=',F10.5,' IERR=',I2)
C  FILL IN GRID UNITS FROM LAT,LONG IF NOT ORIGINALLY SPECIFIED
      GIPV(KK)=yy
      GJPV(KK)=xx
      IPV(KK)=yy
      JPV(KK)=xx
C  WRITE INPUT VALUES
c     WRITE(9,1177) KK,IPV(KK),JPV(KK),GIPV(KK),GJPV(KK),PLAT,PLON,YY,XX
c     WRITE(6,1177) KK,IPV(KK),JPV(KK),GIPV(KK),GJPV(KK),PLAT,PLON,YY,XX
 1177 FORMAT(' INPUT ',3I5,1X,2F7.3,2X,F5.2,F6.2,5X,2F10.6)
      WRITE(80,1147)KK,IPV(KK),JPV(KK),GIPV(KK),GJPV(KK),PLAT,PLON,YY,XX
      IXX=XX
      IYY=YY
c
c  unix skip since only one point is input
      if(1.eq.1)go to 96080
C  (IYY,IXX) PAIRS MUST BE UNIQE -- TEST THEM!!
C  BECAUSE ONLY ONE STARTING POINT CAN BE STORED AT ONE GRID POINT
      KK3=KK-1
c 8841 CONTINUE
C  LOOK AT ALL (JUST) THE PREVIOUS POINTS (KK-1)
      DO 8843 KK2=1,KK3
      IJFLG=0
      IF(IYY.EQ.IPV(KK2).AND.IXX.EQ.JPV(KK2))IJFLG=1
      IF(IJFLG.EQ.0)GO TO 8843
      IXX1=IXX
      IF(IXX1.GE.12)IXX=IXX-1
      IF(IXX1.LE.11)IXX=IXX+1
      IF(IXX1.GE.150)IXX=IXX-1
C      IF(IXX1.LE.109)IXX=IXX+1
C      JPV(KK2+1)=IXX
      JPV(KK)=IXX
C      WRITE(9,8845)KK2,IYY,IXX,YY,XX,PLAT,PLON,YY,XX
 8845 FORMAT(' CHANGE KK2=',I3,2I5,1X,2F6.3,3X,F5.2,F6.2,5X,2F10.6)
c      GO TO 8841
 8843 CONTINUE
C------------------------------------
c  TEST IF THIS BOUNDARY POINT IS OK (NOT PREVIOUSLY USED)
C  (IYY,IXX) PAIRS MUST BE UNIQE -- TEST THEM!!
C  BECAUSE ONLY ONE STARTING POINT CAN BE STORED AT ONE GRID POINT
      KK3=KK-1
 8851 CONTINUE
96080 CONTINUE
      WRITE(31,1145)IYY,IXX,YY,XX,PLAT,PLON,YY,XX
C
C  LAT-LON TEST
C  SAVE PLOT COORDINATES OF POINTS (INCHES)
      PLON1=PLON
      PLAT1=PLAT
      CALL CONVRT(PLON,PLAT,NDERR)
      P28LO(KK)=PLON
      P28LA(KK)=PLAT
c      WRITE(6,9909)KK,P28LA(KK),P28LO(KK),PLAT1,PLON1,NDERR
c     WRITE(9,9909)KK,P28LA(KK),P28LO(KK),PLAT1,PLON1,NDERR
 9909 FORMAT(' START POINTS (INCHES)  ',I3,4F12.4,I5)
      CLOSE(UNIT=80)
      CLOSE(UNIT=31)
      CLOSE(UNIT=33)
c      WRITE(9,91145) KK
91145 FORMAT(' IEND=',I3)
      IEND=KK
C  INITIALIZE GRID PLOT (UNITS ARE IN GRID VALUES)
C  RETAIN ACTUAL GRID UNITS IN GU AND GV ARRAYS
C  STARTING AT THE UPPER LEFT CORNER OF GRID (AS SEEN IN MAP VIEW)
C  I INCREASES IN THE MINUS Y DIRECTION FROM 1 TO NN2(92)
C  J INCREASES IN THE PLUS X DIRECTION FROM 1 TO MM2(180)
      DO 3105 I=1,NN2
      DO 3106 J=1,MM2
      GU1(I,J)=J
      GV1(I,J)=I
 3106 CONTINUE
 3105 CONTINUE
C
C     IF(1.EQ.1) GO TO 3190
C  THE GJPV(K) & GIPV(K) ARE THE EXACT GRID STARTING VALUES READ IN
C  THE IPV(K) & JPV(K) ARE THE LOCATIONS THEY ARE TO BE STORED AT
C  THIS MAY NOT CORRESPOND TO THE INTEGER VALUES OF GJPV'S
C  FOR MULTIPLE STARTING POINTS NEAR THE SAME GRID INTERSECTION
cu      DO 3189 K=1,KK
      k=kk
c      WRITE(6,1147)K,IPV(K),JPV(K),GU1(ipv(K),jpv(K)),GV1(IPV(K),JPV(K))
c      WRITE(9,1147)K,IPV(K),JPV(K),GU1(ipv(K),JPV(K)),GV1(IPV(K),JPV(K))
      GU1(IPV(K),JPV(K))=GJPV(K)
      GV1(IPV(K),JPV(K))=GIPV(K)
 3189 CONTINUE
 3190 CONTINUE
C  ---------------------------------------------------------------------
C  INPUT RUN CONTROL PARAMETERS
C  ---------------------------------------------------------------------
C  SET CHOICES
C---------------
      NFILE=51
C  SET TUNING FACTORS
      WFAC=1.0
C  DDFAC added as command line input 4/10/02
C      DDFAC=1.00
      AKSTRM=1.0
C  SET PRINT FACTOR (NO PRINT, >>>> NOPR=1)
c  20010114
      NOPR=1
C  SET STORAGE OF FILES (NOT KEEP FILES, NOSTOR=1)
      NOSTOR=1
C  SET STORAGE OF FINAL COMPUTED DAILY FIELDS (DO NOT STORE =1)
C  THIS IS SUCH A LARGE FILE (58000 SECTORS FOR 4 MONTHS)
      NOSTOF=1
C---------------
   80 FORMAT ('1CONTROL CARD PARAMETERS FROM INPUT WFL CARD DECK'//)
   84 FORMAT(I5,67A1)
   85 FORMAT(1X,I5,67A1)
   86 FORMAT(1X,F5.2,67A1)
   87 FORMAT(F5.2,67A1)
 8825 FORMAT(' WARNING >>>>>> OVERRIDE OF INPUT FILE PRESENT')
C------------------
C  DDFAC BYPASS
C      DDFAC=0.0
C------------------
C  OVERRIDE ALASKAN STREAM OVERALL AMPLIFIER
      AKSTRM=20.0
  828 continue
   89 FORMAT(F6.1,67A1)
 9905 FORMAT(' HIT ANY KB KEY TO CONTINUE')
      ANGSAM=SP40
   88 FORMAT(F5.0,67A1)
c
c  ioy's are for the next years sequential run input file
      IOY1=IYRSLP
      
      IOY2=IOY1+1
      IOY3=IOY1+2
      IOY4=IOY1+3
      IOY5=IOY1+4
      IOY6=IOY1+5
      IOY7=IOY1+6
      IOY8=IOY1+7
      IOY9=IOY1+8
      IOY10=IOY1+9
      IOY11=IOY1+10
      IOY12=IOY1+11
      IOY13=IOY1+12
C  WRITE NUMERICAL VALUES INTO DUMMY PARM FILE
      OPEN(24,file='parms92.dm2',STATUS='unknown',ACCESS='SEQUENTIAL')
      WRITE(24,9301)IOY1,IMSTRT,IDSTRT,IOY2,IOY3,ndays,WFX,ANGFAC,IOY4,I
     *OY5,IOY6,IOY7,IOY8,IOY9,IOY10,IOY11,IOY12,IOY13
c      WRITE(6,3301)IOY1,IMSTRT,IDSTRT,IOY2,IOY3,ndays,WFX,ANGFAC,IOY4,IO
c     *Y5,IOY6,IOY7,IOY8,IOY9,IOY10,IOY11,IOY12,IOY13
c      WRITE(9,3301)IOY1,IMSTRT,IDSTRT,IOY2,IOY3,ndays,WFX,ANGFAC,IOY4,IO
c     *Y5,IOY6,IOY7,IOY8,IOY9,IOY10,IOY11,IOY12,IOY13
      CLOSE(UNIT=24)
 9301 FORMAT(5I4,I4,4X,F4.2,3X,F6.2,10I4)
 3301 FORMAT(5I4,I4,4X,F4.2,3X,F6.2,10I4,' numeric years')
C  REREAD IN CHARACTER FORMAT
      OPEN(27,file='parms92.dm2',sTATUS='old',ACCESS='SEQUENTIAL'
     &)
C23456789012345678901234567890123456789012345678901234567890123456789012
      READ(27,9304)YR1A,MSTA,DSTA,YR2A,YR3A,YR4A,YR5A,YR6A,YR7A,YR8A,YR9
     *A,YR10A,YR11A,YR12A,YR13A
c      WRITE(6,8303)YR1A,MSTA,DSTA,YR2A,YR3A,AWFX,AANGF,YR4A,YR5A,YR6A,YR
c     *7A,YR8A,YR9A,YR10A,YR11A,YR12A,YR13A
c      WRITE(9,8303)YR1A,MSTA,DSTA,YR2A,YR3A,AWFX,AANGF,YR4A,YR5A,YR6A,YR
c     *7A,YR8A,YR9A,YR10A,YR11A,YR12A,YR13A
 9302 FORMAT(5A4,8X,A2,5X,A3,3X,10A4)
 8303 FORMAT(5A4,8X,A2,5X,A3,3X,10A4,' Alpha numbers')
 9303 FORMAT(5A4,8X,A2,5X,A3,3X,1A4,' A-S')
 9304 FORMAT(5A4,21X,10A4)
 9344 FORMAT(5A4,8X,A2,5X,A3,3X,3A4,'  (file#29) NEXT RUN IN SEQ=')
c
c  end of file inputs
c-----------------------------------------------------
C
C  TOYS (ACCEPT FILE INPUTS AS FINAL)
C  LEEP YEAR
      IF(ndays.EQ.28.AND.IYRSLP.EQ.2004)ndays=29
      IF(ndays.EQ.28.AND.IYRSLP.EQ.2000)ndays=29
      IF(ndays.EQ.28.AND.IYRSLP.EQ.1996)ndays=29
      IF(ndays.EQ.28.AND.IYRSLP.EQ.1992)ndays=29
      IF(ndays.EQ.28.AND.IYRSLP.EQ.1988)ndays=29
      IF(ndays.EQ.28.AND.IYRSLP.EQ.1984)ndays=29
      IF(ndays.EQ.28.AND.IYRSLP.EQ.1980)ndays=29
      IF(ndays.EQ.28.AND.IYRSLP.EQ.1976)ndays=29
      IF(ndays.EQ.28.AND.IYRSLP.EQ.1972)ndays=29
      IF(ndays.EQ.28.AND.IYRSLP.EQ.1968)ndays=29
      IF(ndays.EQ.28.AND.IYRSLP.EQ.1964)ndays=29
      IF(ndays.EQ.28.AND.IYRSLP.EQ.1960)ndays=29
      IF(ndays.EQ.28.AND.IYRSLP.EQ.1956)ndays=29
      IF(ndays.EQ.28.AND.IYRSLP.EQ.1952)ndays=29
      IF(ndays.EQ.28.AND.IYRSLP.EQ.1948)ndays=29
C  1.20 IS STANDARD COEFICIENT (WFX IS READ FROM FILE PARMS92....)
C  1.20 IS STANDARD COEFICIENT (or WFX IS READ FROM input FILE - os2start.inp)
      WFAC=1.20
      WFAC=WFAC*WFX
c      CLOSE(UNIT=29)
C  WHEEL
      HDATE=YR1A//MSTA//DSTA
      YYYYMMDDHH=YR1A//MSTA//DSTA//H00
C  USE READ IN WFAC DATA FOR FILE NAME (AWFX)
C  pap
      G60=GYR//HDATE//AWFX//H3MO
C  CAN
      HYYMM=YR1A//MSTA
      D63=DDIR//HUV//HYYMM//HDY//H1YR
C  OPEN OUTPUT FILE
c      WRITE(9,9338)G60
 9338 FORMAT(' OPENING TRAJECTORY FILE= ',A30)
55551 format(' ',a45)

       write(UNIT=outfile1,FMT=55550)ispladd,isplamm,isplolll,isplomm
55550 format('OSC',i2,'d',i2.2,'mN',i3,'d',i2.2,'mE')
      outfile=outfile1//achy//ahdate//achw//ainwfx3//acha//ainang4//achd
     *//adaysnnn//dotout

c      write(6,55556)outfile
55556 format(' outfile name is ',a45)

c      write(9,55551)outfile
      write(8,55551)outfile
C-----------------------------------------------------------------
C  CALL DELTAD ONLY TO CALCULATE & STORE NEW GEOSTROPHIC CURRENT FILE
C    (SET NEWDD = 1 TO CREATE NEW FILE)
C  COMPUTE GEOSTROPHIC OCEAN CURRENTS 0/2000 DB
C  AFTER CHANGES HAVE BEEN MADE TO THE EXISTING DYNAMIC HEIGHT FILE DD
C    (SET NEWDD = 1 TO CREATE NEW FILE)
      NEWDD=1
c  pfeg debug
      NEWDD=0
C  DD PAUSE AND STOP
c      IF(NEWDD.EQ.1)WRITE(6,9917)
C      IF(NEWDD.EQ.1)KEY=IXKEY()
      IF (NEWDD.EQ.1) CALL DELTAD(DD92180)
C  PAUSE AND STOP RUN IF NEWDD FIELD IS GENERATED
C      IF(NEWDD.EQ.1)GO TO 902
c  pfeg debug
c      WRITE(6,9907)
c      WRITE(9,9907)
 9907 FORMAT(' READ IN GEOSTROPHIC CURRENTS')
 9917 FORMAT(' PAUSE TO RECOMPUTE GEOSTROPHIC CURRENTS FROM DD FIELD')
C
C  ---------------------------------------------------------------------
C  INPUT PERMANENT GEOSTROPHIC CURRENT FILE (CM/SEC)
C  ---------------------------------------------------------------------
C  READ PERMANENT FILE OF GEOSTROPHIC CURRENTS, U THEN V)
      OPEN(48,file='dd92180s.dat',STATUS='OLD',ACCESS='SEQUENTIAL') 
      DO 38 J=1,18
      J2=J*10
      J1=J2-9
c  pfeg debug
c      write(6,99938)j,j1,j2
99938 format(' test file 49 -U- ',3i3)
      DO 38 I=1,92
      READ(48,436,END=218)IKK,J1K,J2K,(OCUDD(I,JK),JK=J1,J2)
   38 CONTINUE
      DO 39 J=1,18
      J2=J*10
      J1=J2-9
c  pfeg debug
c      write(6,99939)j,j1,j2
99939 format(' test file 49 -V- ',3i3)
      DO 39 I=1,92
      READ(48,436,END=219)IKK,J1K,J2K,(OCVDD(I,JK),JK=J1,J2)
   39 CONTINUE
  219 CONTINUE
c  pfeg debug
  218 CONTINUE
  436 FORMAT (I4,2I3,10F8.3,A2,3I3)
C
C  OCEAN GEOSTROPHIC CURRENT VECTOR SPEED
      DO 954 I=1,NN2
      DO 955 J=1,MM2
      CSPEED(I,J)=SQRT((OCUDD(I,J)*OCUDD(I,J))+(OCVDD(I,J)*OCVDD(I,J)))
      DDSPEED(I,J)=CSPEED(I,J)
  955 CONTINUE
  954 CONTINUE
  956 FORMAT('1ORIGINAL ** GEOSTROPHIC SPEED (CM/SEC)',i5,2I3//)
C  READ IN ALASKAN STREAM AMPLIFICATION FACTOR
      K=1
  601 READ(12,602,END=609)IAK(K),JAK(K),DDAK(K)
      IF(IAK(K).LT.1)GO TO 609
      IF(JAK(K).LT.1)GO TO 609
C  NEW FILE 12 -- NMBRSBLF.DA2 HAS CORRECT GRID POINTS FOR 66X160
C   NO J SHIFT NEEDED, INCLUDES AK STRM EXTENSION AND E KAMCH CURRENT AMP
C  ADD CONVERSION FROM 66X160 TO 92X180 GRID
      IAK(K)=IAK(K)+13
      JAK(K)=JAK(K)+17
      AKSPEED(IAK(K),JAK(K))=DDAK(K)*100.0
      DDAK(K)=DDAK(K)*AKSTRM
      AKSPEED2(IAK(K),JAK(K))=(1.0+DDAK(K))*10.0
      K=K+1
      GO TO 601
  609 CONTINUE
  602 FORMAT(2I4,F8.3)
  604 FORMAT (3I4,F8.3,'=DDAK')
      DO 406 J=1,180
      DO 407 I=1,92
C  DD SAVE ORIGINAL SPEEDS IN DDSPEED ARRAY
      DDSPEED(I,J)=SQRT((OCUDD(I,J)*OCUDD(I,J))+(OCVDD(I,J)*OCVDD(I,J)))
      DDAMP=1.0
      DO 611 KK=1,K
      IF(IAK(KK).EQ.I.AND.JAK(KK).EQ.J)GO TO 651
  611 CONTINUE
      GO TO 3333
  651 DDAMP=1.0+DDAK(KK)
C  MULTIPLY DD CURRENT BY DDAMP FACTOR HERE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  DD TEMP BYPASS DDAMP
C      DDAMP=1.0
C  COMPUTE LOCALLY AMPLIFIED SPEEDS (I.E. AK STREAM AND KAMCHATKA CURR.)
      OCUDD(I,J)=OCUDD(I,J)*DDAMP
      OCVDD(I,J)=OCVDD(I,J)*DDAMP
 3333 CONTINUE
C  MULTIPLY DD CURRENT BY OVER ALL DD FACTOR HERE
      OCUDD(I,J)=OCUDD(I,J)*DDFAC
      OCVDD(I,J)=OCVDD(I,J)*DDFAC
  407 CONTINUE
  406 CONTINUE
c      CALL PRINT2X2(OCVDD,1.0)
C  FINAL OCEAN GEOSTROPHIC CURRENT VECTOR SPEED
      DO 944 I=1,NN2
      DO 945 J=1,MM2
      CSPEED(I,J)=SQRT((OCUDD(I,J)*OCUDD(I,J))+(OCVDD(I,J)*OCVDD(I,J)))
  945 CONTINUE
  944 CONTINUE
C
C-----------------------------------------------------------------
C  DD (SPEED)
C  CREATE PRINT AND PLOT FILE OF SPEEDS TO NEW FILE 46
      OPEN(46,file='dd92180.sp2',STATUS='UNKNOWN')
      OPEN(45,file='dd92180.sp1',STATUS='UNKNOWN')
C  FINAL CSPEED X 1.0
      DO 20453 JR=1,18
      JR2=JR*10
      JR1=JR2-9
       DO 20452 I=1,92
C  PRINT TEST 60 LINES PER PAGE ON HP LJ4
      DO 21452 JPDDT=JR1,JR2
      DDTEMP2(I,JPDDT)=(CSPEED(I,JPDDT))/1.0
      DDTEMP(I,JPDDT)=(DDSPEED(I,JPDDT))/1.0
21452 CONTINUE
20452 CONTINUE
20453 CONTINUE
      I=0
      JR1=141
      JR2=175
C-----------------------------------------------------------------
C  DESCRIBE METHOD OF OBTAINING ANGLE OF DEFLECTION FOR SURFACE WIND
C  DUE TO FRICTION AT SURFACE OF OCEAN (INTO CENTER OF LOW -- TO LEFT)
C  AS A FUNCTION OF WIND SPEED (M/SEC) AND LATITUDE
      GLAT1=67.0
C  DECREASE LATITUDE FROM 67N TO 20N BY 1 DEGREE INCREMENTS
      KK=0
      DO 540 I=1,48
      GLAT=GLAT1-I+1
      GLATR=GLAT/57.2958
      SINLAT=SIN(GLATR)
C  COMPUTE MANGITUDE REDUCTION (H U ROLL METHOD)
      IF(GLAT.GT.45.0) RED=0.75+(0.1*((90.0-GLAT)/45.0))
      IF(GLAT.LE.45.0.AND.GLAT.GE.25.0) RED=0.85
      IF(GLAT.LT.25.0) RED=0.65+(0.2*(GLAT/25.0))
C  COMPUTE DEFLECTION ANGLE AS A FUNCTION OF WIND SPEED
      JJ=0
      DO 541 J=1,21,2
      JJ=JJ+1
      COMP=J-1
      XCOMP=RED*COMP
      YCOMP=RED*COMP
      SPEED=SQRT((XCOMP*XCOMP)+(YCOMP*YCOMP))
      SPJ(JJ)=SPEED
      ANG=(22.5-(0.0175*SPEED*SPEED))*(1.495/(SINLAT+1.0))
      ANGDEF(JJ)=ANG
  541 CONTINUE
      KK=KK+1
  540 CONTINUE
C  DESCRIBE METHOD OF OBTAINING THE ROTATION ANGLE FOR
C  SURFACE OCEAN CURRENTS DUE TO WIND
C  ANGLE TO THE RIGTH OF THE WIND AS A FUNCTION OF WIND SPEED AND LATIT
C  REFERENCE FNWC TECHN. NOTE #9, 1967
C  PAGE 10
C  DEFLECTION ANGLE AS A FUNCTION OF SPEED ONLY IS:
C       ANGLE=34(DEG)-7.5XSQRT V (M/SEC)
C    A CONSTRAINT HERE = THE ANGLE APPROACHES ZERO AT 40 KTS
C  PAGE 11
C  ANGLE AS FN OF LAT ONLY BUT FOR 6 KT (3.088 M/SEC) WIND ONLY IS:
C    GRAP DETERMINED A LINEAR RELATION BETWEEN 30-60 N (MODEL LAT RANGE)
C       ANGLE=(0.2933)(LATITUDE) + 2.4
C  BY MAKING INITIAL ANGLE OF DEFLECTION (34.0 IN ABOVE EQN)
C    A LINEAR FN OF LATITUDE WITH THE ABOVE SLOPE (0.2933)
C     CL=(34.0-((67.0-YLAT)*0.2933))
C    AND HOLDING THE CONSTRAINT THAT THE ANGLE APPROACHES ZERO AT 40KTS
      IF(SP40.LT.40.0)SP40=40.0
      SP40=SP40*0.5148
C    THE NEW COEFICIENT AS A FN OF LAT AT 40 KTS IS:
C      CLV=CL/SQRT(SP40)
C  THEREFORE:
C  THE ANGLE OF DEFLECTION AS A FN OF LAT AND WIND SPEED IS:
C       ASP(I,J)
      YLAT1=67.0
      DO 565 I=1,10
      AI=5.0*(I-1)
      SP=AI*0.5148
      DO 566 J=1,48
      YLAT=YLAT1-((J-1))
      CL=(34.0-((YLAT1-YLAT)*0.2933))
C  CANCEL FUNCTION OF LATITUDE
      CL=34.0
      CLV=CL/SQRT(SP40)
      ASP(I,J)=CL-(CLV*SQRT(SP))
  566 CONTINUE
  565 CONTINUE
      DO 567 I=1,10
      AKTS(I)=(I-1)*5.0
      AMPS(I)=AKTS(I)*0.5148
  567 CONTINUE
c      WRITE(9,573) (AKTS(I),I=1,9)
  573 FORMAT(' WIND SPEED',9F7.0,'   (KTS)')
      DO 568 J=1,48
      YLT=YLAT1-((J-1))
  568 CONTINUE
  575 FORMAT (F6.1,6X,9F7.2)
      SPEED=ISAM
C  10**-8
c     A1=-0.00000001
C  VISCOSITY=0.8TO1.8X(10**-6) (M**2/SEC**2)
      VISC=0.0000013
C  9.81M**2SEC**-2
      GRAV=9.81
      ARG=(A1*SPEED*SPEED*SPEED)/(VISC*GRAV)
      ANGLD1=ANGSAM*EXP(ARG)
  581 CONTINUE
C
C
      NCC=NCC-1
c      IPEN(NCC)=3
C  SET READ OLDCOAST OR NOT ===============
      IOLDCST=1
      IOLDCST=0
      IF(IOLDCST.EQ.1)GO TO 2229
      OPEN(70,file='jiclfnwc.mod',STATUS='OLD' )
      OPEN(76,file='ijslipbc.bs',STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      ISKIP=3
C  DO EVERY OPINT (SET ISKIP=1)
      ISKIP=1
C  DOESN'T WORK, TOO FAR SKIP      ISKIP=4
      IFAST=1
C      IFAST=2
      IFAST=0
      IF (IFAST.EQ.1) CALL FASTCO(IFAST,ISKIP,IC)
      OPEN(72,file='pac92180.out',STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      IF (IFAST.EQ.2) CALL FASTCOB(IFAST,ISKIP,IC)
C  READ LONG-LATS
c      IF(IFAST.EQ.1)WRITE(9,9904)IFAST,ISKIP,IC
 9904 FORMAT(' FAST COASTLINE SAVED, IFAST=',I2,' ISKIP=',I2,' IC=',I5)
      IF(IFAST.EQ.1)GO TO 879
      IF(IFAST.EQ.2)GO TO 901
 2229 CONTINUE
C  READ EXISTING CONVERTED COASTLINE FILE
      OPEN(74,file='cst92180.dat',STATUS='old',ACCESS='SEQUENTIAL')
c      WRITE(9,9901)IFAST,ISKIP
 9901 FORMAT(' READING CONVERTED COASTLINE NOW, IFAST=',I3,' ISKIP=',I3)
      REWIND(74)
      IC=0
    2 READ(74,101,END=3)CLON,CLAT,IP
      IC=IC+1
      C1(IC)=CLON
      C2(IC)=CLAT
      C3(IC)=IP
      GO TO 2
    3 CONTINUE
  101 FORMAT (2F10.4,I3)
c      WRITE(9,9903)IC,C1(IC),C2(IC),C3(IC)
 9903 FORMAT(' END COASTLINE, IC=',I5,3F12.4)
  879 continue
c
c
      YRSTRT=IYRSLP
      STRTMO=IMSTRT
      DYSTRT=IDSTRT
c
c
c  make a list of dates in sequence that will be accessible for 12 yrs

c      integer mdi(12)
c      dimension idateseq(3,4382)
c      data mdi/31,28,31,30,31,30,31,31,30,31,30,31/
      OPEN(81,file='oscurs2dates.tmp',STATUS='unknown',
     *ACCESS='SEQUENTIAL')
      iyc=iyrslp
      imc=imstrt
      idc=idstrt
      idateseq(1,1)=iyc
      idateseq(2,1)=imc
      idateseq(3,1)=idc
c  write first line of file
      i=1
c      write(6,9551)i,idateseq(1,i),idateseq(2,i),idateseq(3,i)
c      write(9,9551)i,idateseq(1,i),idateseq(2,i),idateseq(3,i)
      write(81,9551)i,idateseq(1,i),idateseq(2,i),idateseq(3,i)
c  previous day's values
      iyp=iyc
      imp=imc
      idp=idc
      do 9550 i=2,ndays
c      if(i.eq.ndays)write(6,9552)i,iyp,imp,idp
 9552 format(' previous values ',5i5)
c  leep year test
c      LeapYr(iyp)
      mdi(2)=28
      if(LeapYr(iyp))mdi(2)=29
      mdii=imp
      if(idp.le.mdi(mdii))incflgd=1
     
c  increment day
       if(incflgd.eq.1)idateseq(1,i)=iyp
       if(incflgd.eq.1)idateseq(2,i)=imp
       if(incflgd.eq.1)idateseq(3,i)=idp+1
c  end of month test
        if(idateseq(3,i).gt.mdi(mdii))idateseq(2,i)=idateseq(2,i)+1
c  end of year test
         if(idateseq(2,i).eq.13)idateseq(1,i)=idateseq(1,i)+1
c  reset month to 1 and day to 1
         if(idateseq(3,i).gt.mdi(mdii))idateseq(3,i)=1
         if(idateseq(2,i).eq.13)idateseq(2,i)=1
       iyp=idateseq(1,i)
       imp=idateseq(2,i)
       idp=idateseq(3,i)

c  write file
c      if(i.eq.ndays)write(6,9551)i,idateseq(1,i),idateseq(2,i),idateseq
c     *(3,i)
c      write(9,9551)i,idateseq(1,i),idateseq(2,i),idateseq(3,i)
      write(81,9551)i,idateseq(1,i),idateseq(2,i),idateseq(3,i)
 9551 format(i5,i5,2i3)
 9550 continue
      close(unit=81)
c  TEMP STOP
C      IF(1.EQ.1)GO TO 901
C
C  ---------------------------------------------------------------------
C  ---------------------------------------------------------------------
C     START  DAILY  SEQUENCE  LOOP
C  ---------------------------------------------------------------------
      M13=0
C  DO ndays PASSES THROUGH LOOP
 9342 FORMAT(' DSLP FILES= ',3A28)
c
C  ---------------------------------------------------------------------
	call setup_input
c      write(6,9528)day1,idate_base
 9528 format(' after input setup ',' day1= ',i10,' id_b=',i10)
C
C  ---------------------------------------------------------------------
C___________START ndays LOOP__________
C
      NDREAD=0
      IFLR3=0
      NFIL=81
      NFIL1=81
      IPR12=0
C  TEMP LIMIT ndays
C      ndays=13
C  SLP 5X5 LTM COUNT DAYS ACCEPTED
      NNDYM=0
      NDPM=28
      time_index2=0
      DO  9500 IDY9=1,ndays
      if(idy9.le.2)nostor=0
      if(idy9.gt.2)nostor=1
C
c  20010114
      IF (IDY9.GT.3) NOPR=1
c      WRITE(6,6002) IDY9
c      WRITE(9,6002) IDY9
 6002 FORMAT(' START PROCESSING DAY NUMBER ',I4)
C  ---------------------------------------------------------------------
C  ---------------------------------------------------------------------
C  ---------------------------------------------------------------------
C  ---------------------------------------------------------------------
C  INPUT SEA LEVEL PRESSURE DATA FIELD
C  ---------------------------------------------------------------------
      ncrd=0
c patch in year,month,day from input file variables above
      if(idy9.eq.1)iyear=iyrslp
      if(idy9.eq.1)month=imstrt
      if(idy9.eq.1)iday=idstrt

c      write(6,*)iyear, month, iday
C  call get_pressure for each day that pressure is needed for
      	if(idy9.eq.1)iflagt=0
      	if(idy9.gt.1)iflagt=1
c  increment day count by 1 after first day
        if(idy9.eq.1)time_index2=0
    	call get_pressure(iyear,month,iday,iflagt,time_index2,pmslout)
c        write(6,9520)iyear,month,iday,iflagt,time_index2,idy9
 9520 format(' ',i4,2i3,' iflagt=',i1,' time_index2=',i11, ' idy9=',i5)

c      if(idy9.le.2) write(6,*)((pmslout(j,i),j=1,5),i=1,5)
c      if(idy9.le.2)write(9,9544)(kktr,kktr=1,180,6)
      do 9511 itr=1,92
      do 9512 jtr=1,180
      slp3(itr,jtr)=pmslout(itr,181-jtr)
      islp3(itr,jtr)=pmslout(itr,181-jtr)-1000.0

 9512 continue
c      if(idy9.le.2)write(9,9544)itr,(islp3(itr,ktr),ktr=1,180,6),itr
 9544 format(' slp3 ',64i3)
 9511 continue
c      if(idy9.le.2)write(9,9544)(kktr,kktr=1,180,6)
      ncrd=ncrd+1
      k=idateseq(1,idy9)
      imo=idateseq(2,idy9)
      idy=idateseq(3,idy9)
      IMOA=IMO
      IF(IDY9.EQ.1)IMO1=imo
      IF(IDY9.EQ.1)IYR1=k
      IF(IDY9.EQ.1)IDY1=idy
      AKYR=K
      ILASTDH=0
C---------------------------------------------------------------------
C 
      AKYR=K
      KYR=K
      DAY=IDY
      AIMO=IMO
      BIMO=IMO
c  setup file29 daily name changes
c  stop after 5 days
      if(idy9.gt.1)go to 35010
      kyyyy=k
      kmm=imo
      kdd=idy
      khh=0
c      write(6,35001)kyyyy,kmm,kdd,khh
c      write(9,35001)kyyyy,kmm,kdd,khh
35001 format(' ',i4,3i2,' kyyyy kmm kdd khh')
35002 format(' ',a4,3a2,' akyyyy akmm akdd akhh')
      akyyyy=aconvi4(kyyyy)
      akmm=aconvi2(kmm)
      akdd=aconvi2(kdd)
      akhh=h00
c      write(6,35002)akyyyy,akmm,akdd,akhh
c      write(9,35002)akyyyy,akmm,akdd,akhh
      YYYYMMDDHH=akyyyy//akmm//akdd//h00
      file29=DSLPWWCTC//YYYYMMDDHH//DOTTXT
c      write(6,35005)file29
c      write(9,35005)file29
35005 format(' file29 = ',a23)
35010 continue
c-----------------------------------------------------------------------
C
C  START PROCESSING DSLP FIELD HERE
      IF(WFAC.LE.0.0) GO TO 6001


  107 FORMAT(I3,13F6.1)
C---------------------------------------------------------------------
C  20X44 DSLP FIELD HAS BEEN ACCEPTED
C  EXPAND 20X44 DSLPS (SLP1) TO 26X48 GRID (SLP2) FOR USE IN SPLN2648
C---------------------------------------------------------------------
C
C----------------------------------------------------------------------
C  MINIMIZE AND MAXIMIZE INDEXES WITHIN GRID FOR cpu time SAVINGS
C     IF(1.EQ.1) GO TO 9169
C  THE GJPV(K) & GIPV(K) ARE THE EXACT GRID STARTING VALUES READ IN
C  THE IPV(K) & JPV(K) ARE THE LOCATIONS THEY ARE TO BE STORED AT
C  THIS MAY NOT CORRESPOND TO THE INTEGER VALUES OF GJPV'S
C  FOR MULTIPLE STARTING POINTS NEAR THE SAME GRID INTERSECTION
C  THIS DEFINES THE WORKING AREA OF THE GRID, REST=NOT USED IN CALCS
C  IMIN-IMAX (VERTICAL)
      IMIN=1000
      IMAX=0
      JMIN=1000
      JMAX=0
      DO 9150 MMX=1,IEND
      IF(GU1(IPV(MMX),JPV(MMX)).LT.JMIN)JMIN=GU1(IPV(MMX),JPV(MMX))
      IF(GU1(IPV(MMX),JPV(MMX)).GT.JMAX)JMAX=GU1(IPV(MMX),JPV(MMX))
      IF(GV1(IPV(MMX),JPV(MMX)).LT.IMIN)IMIN=GV1(IPV(MMX),JPV(MMX))
      IF(GV1(IPV(MMX),JPV(MMX)).GT.IMAX)IMAX=GV1(IPV(MMX),JPV(MMX))
 9150 CONTINUE
C  y2k speedup (MADD=4; 4 IS THE LOWER LIMIT WITHOUT CHANGEING TRAJECTORIE)
	MMADD=4
      MMADD=12
      MMADD2=MADD+2
      IMIN=IMIN-MMADD
      IMAX=IMAX+MMADD
      JMIN=JMIN-MMADD
      JMAX=JMAX+MMADD
      IMIN2=IMIN-MMADD2
      IMAX2=IMAX+MMADD2
      JMIN2=JMIN-MMADD2
      JMAX2=JMAX+MMADD2
C?????????
C  LIMIT CALCULATIONS
C  WE WANT VALUES IN THE 92X180 GRID AT THESE POINTS
      IF(IMIN.LT.1)IMIN=1
      IF(IMAX.GT.92)IMAX=92
      IF(JMIN.LT.1)JMIN=1
      IF(JMAX.GT.180)JMAX=180
      IF(IMIN2.LT.1)IMIN2=1
      IF(IMAX2.GT.92)IMAX2=92
      IF(JMIN2.LT.1)JMIN2=1
      IF(JMAX2.GT.180)JMAX2=180
c      WRITE(9,9151)IDY9,IMIN,IMAX,JMIN,JMAX
 9151 FORMAT(I5,' MIN AND MAX LIMITS FOR SPLINE CALC=',2I4,3X,2I4)
      IDX1=IMIN
      IDX2=IMAX
      JDX1=JMIN
      JDX2=JMAX
      IDX12=IMIN2
      IDX22=IMAX2
      JDX12=JMIN2
      JDX22=JMAX2
 9169 CONTINUE
C  -------------------------------------------------------------------
      IF(IDY9.LE.nflds)IDX2=NN2-1
      IF(IDY9.LE.nflds)JDX1=2
      IF(IDY9.LE.nflds)JDX2=MM2-1
      IF(IDY9.LE.nflds)IDX12=2
      IF(IDY9.LE.nflds)IDX22=NN2-1
      IF(IDY9.LE.nflds)JDX12=2
      IF(IDY9.LE.nflds)JDX22=MM2-1
c       write(6,667)nfile,kyr,imo,idy,idx1,idx2,jdx1,jdx2
c       write(9,667)nfile,kyr,imo,idy,idx1,idx2,jdx1,jdx2
  667 format(' print  i,j indexes ',8i4)
68139 FORMAT(' NEW GRID 92X180 I=',I2,' J=',I3,2F9.3,' LA=',F6.3,I5)
68490 FORMAT(' OFF GRID  I=',I2,' J=',I3,2F9.3,' LA=',F11.3,I5)
C  WIND TEST DSLP FIELDS
  665 format(' slp3(92x180) ',4i4)
      if(idy9.le.2)call print2X2(slp3,0.1)
  664 FORMAT('  SLP3(92X180) BY 8 S',5X,'FROM J=',I3,2X,'TO J=',I3)
  663 FORMAT(I3,8F8.2,' slp3 test')
C  ---------------------------------------------------------------------
C  WIND
C  ---------------------------------------------------------------------
C  COMPUTE WIND VECTORS FROM SLP FIELD
      CALL WIND(SLP3,LS1,ALAT2,WXX,WYY,idx12,idx22,jdx12,jdx22)
      IF (NOPR.EQ.1.and.idy9.gt.2) GO TO 847
C  PRESSURE CHANGE PER GRID LENGTH MAGNITUDE
      DO 27 I=IDX1,IDX2
      DO 28 J=JDX1,JDX2
      CSPEED(I,J)=0.0
      IF((I+1).GE.NN2) GO TO 28
      IF((J+1).GE.MM2) GO TO 28
      TX=SLP3(I,J+1)-SLP3(I,J)
      TY=SLP3(I+1,J)-SLP3(I,J)
      CSPEED(I,J)=SQRT((TX*TX)+(TY*TY))
   28 CONTINUE
   27 CONTINUE
c      IF(IDY9.EQ.2)WRITE(9,29) KYR,IMO,IDY
   29 FORMAT(' 1ATMOS PRESS CHANGE X 10 (MAGNITUDE/GRID) (MB)',3I3//)
      IF(IDY9.EQ.2) CALL PRINT2X2(CSPEED,10.0)
      DO 844 I=IDX1,IDX2
      DO 845 J=JDX1,JDX2
      CSPEED(I,J)=SQRT((WXX(I,J)*WXX(I,J))+(WYY(I,J)*WYY(I,J)))
  845 CONTINUE
  844 CONTINUE
C
c  write out wind speed and components
      IF (NOPR.EQ.1.AND.IDY9.gt.2) GO TO 847
c      WRITE(9,846) KYR,IMO,IDY,SP40,wfac,fwx,ANGSAM,ANGDEX
  846 FORMAT(' WIND SPEED (M/SEC)',3I4,' sp40=',5f6.2//)
      CALL PRINT2X2(CSPEED,1.0)
c      WRITE(9,850) KYR,IMO,IDY,SP40,wfac,fwx,ANGSAM,ANGDEX
  850 FORMAT(' U-WIND COMPONENT (M/SEC)',3I4,' sp40=',5f6.2//)
      CALL PRINT2X2(WXX,1.0)
c      WRITE(9,851) KYR,IMO,IDY,SP40,wfac,fwx,ANGSAM,ANGDEX
  851 FORMAT(' V-WIND COMPONENT (M/SEC)',3I4,' sp40=',5f6.2//)
      CALL PRINT2X2(WYY,1.0)
  847 CONTINUE
C  ---------------------------------------------------------------------
C  COMPUTE SURFACE OCEAN CURRENT VECTOR FIELD DUE TO WIND
C  ---------------------------------------------------------------------
C  SEE FUNCTION "WINDC" (SFC CURR DUE TO WIND)
C  SET ANGDEX TO ANGFAC WHICH IS READ IN FROM PARMS92.INP FILE
C  ANGLE OF DEFLECTION MODIFICATION
      DO 1844 I=IDX1,IDX2
      DO 1845 J=JDX1,JDX2
      ANGDEX=ANGFAC
      OCUW(I,J)=WINDC(WXX(I,J),WYY(I,J),1,ALAT2(I,J),SP40,ANGSAM,ANGDEX)
      ANGDEX=ANGFAC
      OCVW(I,J)=WINDC(WYY(I,J),WXX(I,J),2,ALAT2(I,J),SP40,ANGSAM,ANGDEX) 
      CSPEED(I,J)=SQRT(OCUW(I,J)*OCUW(I,J)+OCVW(I,J)*OCVW(I,J))
      II3=GV1(IPV(1),JPV(1))
      JJ3=GU1(IPV(1),JPV(1))
      IF(I.EQ.II3.AND.J.EQ.JJ3)ANGDEFL=ANGDEX
      IF(I.EQ.II3.AND.J.EQ.JJ3)CALL IJTOLL(GV1(I,J),GU1(I,J),GLA1,GLO1)
 1845 CONTINUE
 1844 CONTINUE
 1848 FORMAT(' 1SPEED OF SFC-OCN-CURRENT DUE TO WIND (CM/SEC)',3I4//)
c      IF(IDY9.EQ.1)WRITE(9,1848) KYR,IMO,IDY
      IF(IDY9.EQ.1) CALL PRINT2X2(CSPEED,1.0)
c      IF(IDY9.EQ.2) WRITE(9,1848) KYR,IMO,IDY
      IF(IDY9.EQ.2) CALL PRINT2X2(CSPEED,1.0)
 1849 CONTINUE
 6001 CONTINUE
c  y2k 7/20/99
C  ---------------------------------------------------------------
C  TUNING AND SUMMATION OF CURRENT COMPONENTS
C  ---------------------------------------------------------------
C  MULTIPLY SFC CURRENT DUE TO WIND BY FACTOR HERE
C  ADD WIND CURRENT TO GEOSTROPHIC CURRENT = TOTAL CURRENT
      DO 410 J=1,180
      DO 411 I=1,92
      ICEYES=0
      IOCUS=0
      OCUW(I,J)=OCUW(I,J)*WFAC
      OCVW(I,J)=OCVW(I,J)*WFAC
      OCUS(I,J)=OCUW(I,J)+OCUDD(I,J)
      OCVS(I,J)=OCVW(I,J)+OCVDD(I,J)
C   KEEP VALUES LESS THAN 125.00 ABS (FOR EXTRANEOUS VALUES OVER LAND)
      IF(OCUS(I,J).GT.125.0)IOCUS=IOCUS+1
      IF(OCUS(I,J).LT.-125.0)IOCUS=IOCUS+1
      IF(OCVS(I,J).GT.125.0)IOCUS=IOCUS+1
      IF(OCVS(I,J).LT.-125.0)IOCUS=IOCUS+1
      IF(IOCUS.EQ.0)GO TO 497
C  WIND
  497 CONTINUE
  411 CONTINUE
  410 CONTINUE
C  WRITE FILE OF TOTAL CURRENTS
      IF(NOSTOR.EQ.1) GO TO 227
      DO 94 J=90,138
      DO 94 I=4,5
      I2=I*8
      I1=I2-7
      WRITE(55,113)(OCUS(IK,J),IK=I1,I2),j,i1,i2,KYR,IMO,IDY
  94  CONTINUE
  224 DO 95 J=1,180
      DO 95 I=1,5
      I2=I*8
      I1=I2-7
      WRITE(55,114)(OCVS(IK,J),IK=I1,I2),j,i1,i2,KYR,IMO,IDY
  95  CONTINUE
  225 CONTINUE
  113 FORMAT(8F8.2,i4,i5,3I3,I5,' U')
  114 FORMAT(8F8.2,i4,i5,3I3,I3,' V')
  227 CONTINUE
C
c  write a file foro contouring first day's slp, wind, and wind currents
      IF(NOPR.EQ.1.AND.IDY9.gt.2) GO TO 2151
      OPEN(29,file=file29,STATUS='unknown',ACCESS='SEQUENTIAL')
      do 853 j=3,177,2
      do 854 i=3,89,2
c  scale wind arrow to 20 m/sec = 0.25 inches
      wscale=0.25/20.0
      wxi=wxx(i,j)*wscale
      wyi=wyy(i,j)*wscale
      call angld(wxx(i,j),wyy(i,j),wanglr)
      wlngth=sqrt((wxi*wxi)+(wyi*wyi))
c  scale wind current arrow to 40 cm/sec = 0.25 inches
      wcscale=0.25/40.0
      wcxi=ocuw(i,j)*wcscale
      wcyi=ocvw(i,j)*wcscale
      call angld(wcxi,wcyi,wcanglr)
      wclngth=sqrt((wcxi*wcxi)+(wcyi*wcyi))
c  scale total current arrow to 40 cm/sec = 0.25 inches
      tcscale=wcscale
      tcxi=ocus(i,j)*tcscale
      tcyi=ocvs(i,j)*tcscale
      call angld(tcxi,tcyi,tcanglr)
      tclngth=sqrt((tcxi*tcxi)+(tcyi*tcyi))
      write(29,855)alon2(i,j),alat2(i,j),slp3(i,j),wxx(i,j),wyy(i,j),wan
     &glr,wlngth,ocuw(i,j),ocvw(i,j),wcanglr,wclngth,ocus(i,j),ocvs(i,j)
     &,tcanglr,tclngth,kyr,imo,idy,i,j
  854 CONTINUE
  853 CONTINUE
  855 format(' ',f6.2,f7.2,f8.2,12f7.2,i5,2i3,i3,i4)
      close(unit=29)
c      write(9,856)
  856 format(' just wrote file of dslp, wind, and wind current for later 
     *contouring')
      IF(NOPR.EQ.1.AND.IDY9.gt.2) GO TO 2151
      DO 2144 I=1,NN2
      DO 2145 J=1,MM2
      CSPEED(I,J)=SQRT(OCUS(I,J)*OCUS(I,J)+OCVS(I,J)*OCVS(I,J))
 2145 CONTINUE
 2144 CONTINUE
c      IF(IDY9.eq.1)WRITE(9,2148) KYR,IMO,IDY
c      IF(IDY9.eq.2)WRITE(9,2148) KYR,IMO,IDY
 2148 FORMAT(' 1FINAL CURRENT SPEED OF OCUS-OCVS (CM/SEC)',6I3)
      IF(IDY9.EQ.1) CALL PRINT2X2(CSPEED,1.0)
      IF(IDY9.EQ.2) CALL PRINT2X2(CSPEED,1.0)
c      WRITE(9,2146) KYR,IMO,IDY
 2146 FORMAT(' U-TOTAL SURFACE OCEAN CURRENT (CM/SEC)',3I4)
c      WRITE(9,2149) WFAC,TFAC,DDFAC
 2149 FORMAT(10X,'WIND FACTOR =',F5.2,5X,'THERMAL FACTOR =',F5.2,5X,'GEO
     'STROPHIC FACTOR =',F5.2/)
c      WRITE(9,92146) KYR,IMO,IDY
92146 FORMAT(' V-TOTAL SURFACE OCEAN CURRENT (CM/SEC)',3I4)
      CALL PRINT2X2(OCUS,1.0)
c      WRITE(9,2147) KYR,IMO,IDY
 2147 FORMAT(' V--TOTAL SURFACE OCEAN CURRENT (CM/SEC)',6I4)
c      WRITE(9,296)KYR,IMO,IDY,WFAC,TFAC,DDFAC
      CALL PRINT2X2(OCVS,1.0)
  296 FORMAT('/TOTAL SFCOCN CURRENTS (40X160) STORED FOR',3I4,5X,'WFAC='
     ',F4.2,5X,'TFAC=',F4.2,5X,'DDFAC=',F4.2)
C  ---------------------------------------------------------------
C         TRANSPORT (PROGRESSIVE) VECTORS
 2151 CONTINUE
C  ---------------------------------------------------------------
C
C  START 9100 START-POINT LOOPS
C  GRID LOCATIONS FOR THE TRAJECTORY (PT. 1 AND 2) ARE STORED AT I,J
      DO 9100 K3=1,IEND
      I=IPV(K3)
      J=JPV(K3)
C  SKIP SOME K3'S
CC      IF(K3.EQ.2)GO TO 9100
C  HANDLE OFF MAP POINTS
      TX1=GU1(I,J)
      TY1=GV1(I,J)
      TX2=GU2(I,J)
      TY2=GV2(I,J)
      II=TY1
      JJ=TX1
C  I,J ARE THE LOCATIONS OF THE STORAGE, II,JJ ARE THE ACTUAL GRID VALS.
 8340 FORMAT(' POINT 1 OFF MAP, K3=',I2,' I=',I2,' J=',I3,' II=',I2,' JJ
     *=',I3,F10.4)
 8341 FORMAT(' POINT 2 OFF MAP, K3=',I2,' I=',I2,' J=',I3,' II=',I2,' JJ
     *=',I3,F10.4)
c      IF(GV1(I,J).GE.90.99)WRITE(9,8340)K3,I,J,II,JJ,GV1(I,J)
c      IF(GV1(I,J).LT.2.0)WRITE(9,8340)K3,I,J,II,JJ,GV1(I,J)
c      IF(GU1(I,J).GE.177.99)WRITE(9,8340)K3,I,J,II,JJ,GU1(I,J)
c      IF(GU1(I,J).LT.2.0)WRITE(9,8340)K3,I,J,II,JJ,GU1(I,J)
      IF(GV1(I,J).GE.90.99)GV1(I,J)=90.99
      IF(GV1(I,J).LT.2.0)GV1(I,J)=2.0
      IF(GU1(I,J).GE.177.99)GU1(I,J)=177.99
      IF(GU1(I,J).LT.2.0)GU1(I,J)=2.0
C  ICE, LOOK FOR ICE
      JGU1=GU1(I,J)
      IGV1=GV1(I,J)
      JGU2=GU2(I,J)
      IGV2=GV2(I,J)
      CALL IJTOLL(GV1(I,J),GU1(I,J),GLA1,GLO1)
      CALL SPLINE(GU1(I,J),GV1(I,J),IOK,UU,OCUS)
      CALL SPLINE(GU1(I,J),GV1(I,J),IOK,VV,OCVS)
C  WINDS ALONG DRIFTING TRACK
C  COMPUTE AND PRINT EACH VELOCITY COMPONENT EACH DAY
      CALL SPLINE(GU1(I,J),GV1(I,J),IOK,DDUU,OCUDD)
      CALL SPLINE(GU1(I,J),GV1(I,J),IOK,DDVV,OCVDD)
      CALL SPLINE(GU1(I,J),GV1(I,J),IOK,WUSJO,WXX)
      CALL SPLINE(GU1(I,J),GV1(I,J),IOK,WVSJO,WYY)
      CALL SPLINE(GU1(I,J),GV1(I,J),IOK,WCUU,OCUW)
      CALL SPLINE(GU1(I,J),GV1(I,J),IOK,WCVV,OCVW)
 9296 FORMAT(' ',4I3,' WF',F4.2,' DD',F4.2,' DD',2F6.2,' WSJO',2F6.2,' U
     *V',5F6.2,F7.2,3I3)
98113 FORMAT(' PAPA=',8F8.3)
C  FILE - WINDS ALONG TRACK
      IF(K3.EQ.1)WRITE(62,99296)IDY9,KYR,IMO,IDY,GLA1,GLO1
     *,WUSJO,WVSJO,WSSJO,WATSJO,UU,VV,UVSJO,ATSJO,WFAC,DDFAC,DDUU,DDVV,K
     *3,I,J,TY1,TX1
99296 FORMAT(' ',I4,i5,2I2,F6.2,'N',F7.2,'W',3F6.2,F6.1,' UV=',3F6.2,F6.
     *1,' F=',2F5.2,' DD=',2F6.2,I3,' IJ=',2I3,2F7.2)
C
C
C  COMPUTE NEW I,J GRID COORDINATES OF END POINT OF NET 24 HR. MOTION
C  APPROXIMATE GRID LENGTH IS 90 KM
C  (CM/SEC)(1KM/100000CM)(3600SEC/HR)(24HRS)(1GRDLEN/90KM)=====
C  ===== 0.009600(GRDLEN)/(CM/SEC)
C     TYPICAL FACTOR=0.009600
C  THUS, IT TAKES A CURRENT SPEED OF 104 CM/SEC
C  TO GO ONE GRID POINT OF 90 KM IN A DAY
C  A MORE TYPICAL SPEED OF 15 CM/SEC YIELDS 0.144 GRID LENGTH
C  THEREFORE ABOUT 1 % OF A GRID POINT PER CM/SEC
C  COMPUTE FACTOR AT APPROPRIATE GRID LENGTH AT LATITUDE OF VECTOR
C  GRID LENGTH VARIES WITH LATITUDE -- 97.6KM AT 66N TO 76.5KM AT 30N
      IGV=GV1(I,J)
      JGU=GU1(I,J)
C
C  ADD SALMON SWIMMING SPEED IN CONSTANT DIRECTION
      GLON=ALON2(IGV,JGU)
C
C  USE SOCKIR FOR RANDOM WALK DIFFUSION
      SKCMPS=0.0
12100 CONTINUE
C  ZERO OUT DIRECTION (SKDEGT) FOR RANDOM DIFFUSION OR SWIMMING
C  DIFFUSION SKIP
      SKCMPS=0.0
      SKDEGT=0.0
      UU1=UU
      VV1=VV
      GLA=ALAT2(IGV,JGU)
      GLN=GRIDLN(GLA)
C  TEST FOR DIV BY ZERO
C  DISTANCE MOVED ON CHART (POINT #2)
      FACTOR=0.864000/GLN
      GU2(I,J)=GU1(I,J)+(UU*FACTOR)
      GV2(I,J)=GV1(I,J)-(VV*FACTOR)
63198 FORMAT(' ','GU2=',2F6.2,' UU1=',2F6.2,' UU=',2F6.2)
C  TEST GU1 AND GU2 CONTENTS
C  SJO
      TX1=GU1(I,J)
      TY1=GV1(I,J)
      TX2=GU2(I,J)
      TY2=GV2(I,J)
C  WIND ONLY -- FREEZE TRAJECTORY
      IWCWIND=0
      IF(IWCWIND.EQ.1)GO TO 9500
 3198 FORMAT(2I4,'GU1=',2F6.2,' GU2=',2F6.2,' UU1=',2F6.2,' UU=',2F6.2,'
     * K3=',I3)
C  TOYS BS-ALEUTIANS LIMIT SOME SQUARES IN OVERSHOOT OF BOUNDARY SQUARE
C
      II6=GV1(I,J)
      JJ6=GU1(I,J)
      II7=GV2(I,J)
      JJ7=GU2(I,J)
      IF(II6.LE.(11+13).OR.II6.GE.(16+13))GO TO 5074
      IF(JJ6.LE.(83+17).OR.JJ6.GE.(88+17))GO TO 5074
 5075 FORMAT(' BS-ALU-BC  ',4F8.3)
      KIK=0
      IF(KIK.EQ.0)GO TO 5076
c      IF(II7.EQ.(14+13).AND.JJ7.EQ.(84+17))WRITE(9,5075)TX1,TY1,TX2,TY2
      IF(II7.EQ.(14+13).AND.JJ7.EQ.(84+17))GV2(I,J)=15.0
c      IF(II7.EQ.(14+13).AND.JJ7.EQ.(84+17))WRITE(9,5075)TX1,TY1,TX2,GV2(
c     *I,J)
 5076 CONTINUE
 5074 CONTINUE
 8872 CONTINUE
C  HANDLE OFF MAP POINTS
      II3=GV1(IPV(K3),JPV(K3))
      JJ3=GU1(IPV(K3),JPV(K3))
      U1=OCUS(II3,JJ3)
      V1=OCVS(II3,JJ3)
C  FLAG LOW VELOCITIES
      SPLOW=SQRT((U1*U1)+(V1*V1))
      TX1=GU1(I,J)
      TY1=GV1(I,J)
      TX2=GU2(I,J)
      TY2=GV2(I,J)
      II=TY2
      JJ=TX2
c      IF(GV2(I,J).GE.90.99)WRITE(9,8341)K3,I,J,II,JJ,GV2(I,J)
c      IF(GV2(I,J).LT.3.0)WRITE(9,8341)K3,I,J,II,JJ,GV2(I,J)
c      IF(GU2(I,J).GE.177.99)WRITE(9,8341)K3,I,J,II,JJ,GU2(I,J)
c      IF(GU2(I,J).LT.3.0)WRITE(9,8341)K3,I,J,II,JJ,GU2(I,J)
      IF(GV2(I,J).GE.90.99)GV2(I,J)=90.99
      IF(GV2(I,J).LT.3.00)GV2(I,J)=3.00
      IF(GU2(I,J).GE.177.99)GU2(I,J)=177.99
      IF(GU2(I,J).LT.3.00)GU2(I,J)=3.00
C  SKIP IF IN THE SW CORNER
      IF(GU2(I,J).LE.3.00.AND.GV2(I,J).LE.3.00)GO TO 5080
C
C------------------------------------
C  TEST FOR SLIP BOUNDARY ENCOUNTER AND ADJUST POSITION OF (GU2,GV2)
C
      N2CLOSE=0
 5070 TX1=GU1(I,J)
      TY1=GV1(I,J)
      TX2=GU2(I,J)
      TY2=GV2(I,J)
      DLINE=DLINE1
C  GRID COORDINATES OF SECOND POINT ARE TX2,TY2
      I2CLOSE=0
      II=TY2
      JJ=TX2
      JJXN=GU2(I,J)
      IIYN=GV2(I,J)
c      IF(II.LE.2)WRITE(9,8974)I,J,TX1,TY1,TX2,TY2,II,JJ,K3
c     IF(JJ.LE.3)WRITE(9,8974)I,J,TX1,TY1,TX2,TY2,II,JJ,K3
 8974 FORMAT(' NEAR BNDARY  ',2I4,4F8.3,' II=',I2,' JJ=',I3,'K3=',I3)
C  TEST IF THIS (II,JJ) IS ONE OF THE BOUNDARY SQUARES
      IF(II.LE.1.OR.II.GE.91)GO TO 5080
      IF(JJ.LE.1.OR.JJ.GE.179)GO TO 5080
C  SKIP BC CHECK IF THIS II,JJ IS NOT ONE OF THE BOUNDARY SQUARES
      ICHKBC=0
      IF(LSBC(II,JJ,1).LT.1.OR.LSBC(II,JJ,2).LT.1)ICHKBC=1
95080 FORMAT(' BOUNDARY FILE CHKOR ???',4I5/)
      IF(ICHKBC.EQ.1)GO TO 5080
      BX1=BCJ(II,JJ,1)
      BY1=BCI(II,JJ,1)
      BX2=BCJ(II,JJ,2)
      BY2=BCI(II,JJ,2)
c      IF(II.LE.2)WRITE(9,8973)I,J,TX1,TY1,TX2,TY2,II,JJ,BX1,BY1,BX2,BY2
c      IF(JJ.LE.3)WRITE(9,8973)I,J,TX1,TY1,TX2,TY2,II,JJ,BX1,BY1,BX2,BY2
 8973 FORMAT(' BNDRY SQ ','I=',I2,I4,4F7.2,' II=',I2,I4,4F7.2)
C  DERIVE EQUATION FOR STRAIGHT LINE BOUNDARY BET. POINTS
C  LIMIT SLOPE
      DX=BX2-BX1
      ADX=ABS(DX)
c      IF(ADX.LT.0.001)WRITE(9,8990)bx2,bx1,ADX
 8990 FORMAT(2f10.5,' SMALL X-DIFFERENCE =',F15.12)
      IF(ADX.LT.0.001)ADX=0.001
      IF(DX.LT.0.0)ADX=-ADX
      SLOPE=(BY2-BY1)/ADX
c      IF(SLOPE.LT..001.AND.SLOPE.GT.-0.001)WRITE(9,58990)by2,by1,SLOPE
58990 FORMAT(2f10.5,' SMALL Y-DIFFERENCE =',F15.12)
      IF(SLOPE.LT.0.001.AND.SLOPE.GE.0.0)SLOPE=0.001
      IF(SLOPE.GT.-0.001.AND.SLOPE.LT.0.0)SLOPE=-0.001
      BINTCP=BY1-((SLOPE)*BX1)
C  Y=MX+B   (AT TX2) GIVES VERTICAL YVAL
      YVAL=(SLOPE*TX2)+BINTCP
C  TY2 MUST STAY BELOW LINE (YVAL AT TX2) FOR LSBC(II,JJ,2)=1
C  TY2 MUST STAY ABOVE LINE (YVAL AT TX2) FOR LSBC(II,JJ,2)=2
C   BUT WE REALLY WANT THE PERPENDICULAR DISTANCE
C  SLOPEN IS THE SLOPE OF THE LINE NORMAL TO THE COAST BC
      SLOPEN=-1.0/SLOPE
C  POINT SLOPE EQUATION FOR THIS LINE IS:
C  Y-Y1=M(X-X1) WHERE M=-1/SLOPE
C  RESET POINT CONSIDERING LONG-SHORE COMPONENT
C  THUS, RESET POINT ALONG LINE NORMAL TO BOUNDARY LINE
C  INTERSECTION OF LINE NORMAL TO SHORE THROUGH POINT (TX2,TY2)
C  WITH BOUNDARY LINE DETERMINES THE NEW POINT + OR - THE DLINE
C  SUBSTITUTING YN=M(XN)+B INTO YN-TY2=(-1/M)(XN-TX2)
C  GIVES THE X VALUE OF THE NEW POINT
C  M(XN)+B-TY2=(-1/M)(XN-TX2)
C  XN=(TY2-B+(TX2\M))/(M+(1/M))
      XN=(TY2-BINTCP+(TX2/SLOPE))/(SLOPE+(1.0/SLOPE))
C  SUBSTITUTING XN BACK INTO THE BOUNDARY LINE EQN GIVES YN
      YN=(SLOPE*XN)+BINTCP
      IF(N2CLOSE.EQ.0)XN1=XN
      IF(N2CLOSE.EQ.0)YN1=YN
C  THE (NORMAL) DISTANCE TO THE BOUNDARY LINE IS THEREFORE
      DSX=TX2-XN
      DSY=TY2-YN
      DS=SQRT((DSX*DSX)+(DSY*DSY))
      IF(N2CLOSE.EQ.0)DS1=DS
      DSXA=TX2-XN1
      DSYA=TY2-YN1
      DSA=SQRT((DSXA*DSXA)+(DSYA*DSYA))
c      IF(II.LE.2)WRITE(9,8975)I,J,TX1,TY1,TX2,TY2,II,JJ,YVAL
c      IF(JJ.LE.3)WRITE(9,8975)I,J,TX1,TY1,TX2,TY2,II,JJ,YVAL
 8975 FORMAT(' YVAL=',I3,I4,4F7.2,I3,I4,' YVAL=',F9.4)
C  APPROACH THE BOUNDARY LINE WITHIN DLINE GRID UNITS
C      DLINE=0.05/SQRT(2.0)
C      DLINE=0.10
C      DLINE=0.20
C  SEE LINE 1810 TO SET DLINE
C      DLINE=0.00
C  IF (TX2,TY2) IS WITHIN DS OF THE LINE, THEN MOVE TO DS FROM THE LINE
      IF(DS.LT.DLINE)GO TO 55075
C  IS (TX2,TY2) STILL ON THE CORRECT SIDE OF THE BC LINE, THEN SKIP MOV
      KERRBYND=0
      IF(LSBC(II,JJ,2).EQ.1.AND.TY2.GT.(YVAL))KERRBYND=1
      IF(LSBC(II,JJ,2).EQ.2.AND.TY2.LT.(YVAL))KERRBYND=1
      IF(KERRBYND.EQ.1)GO TO 5080
55075 CONTINUE
C  BOUNDARY CORRECTION IMMINENT
C
C  DERIVE IT FROM 2-POINT EQN
C  1. (Y-BY1)/(X-BX1)=(BY1-BY2)/(BX1-BX2)
C  1A.   SLOPE=(BY1-BY2)/(BX1-BX2)
C  1B.   Y=((SLOPE)(X-BX1)) + BY1
C
C  2. YN=(-1/SLOPE)(XN-X3) + Y3, WHERE X3=TX2 Y3=TY2
C
C  INTERSECT 1B AT Y=YN,X=XN WITH 2 AND SOLVE FOR VALUE OF XN
C
C     XN=((TX2/SLOPE)+(SLOPE*BX1)+TY2-BY1)/(SLOPE+(1/SLOPE))
CCCCC      XN=((TX2/SLOPE)+(SLOPE*BX1)+TY2-BY1)/(SLOPE+(1/SLOPE))
C  SUB XN IN 1B
C     YN=((SLOPE)*(XN-BX1))+BY1
CCCCC      YN=((SLOPE)*(XN-BX1))+BY1
CCCCC      XN1=XN
CCCCC      YN1=YN
c
C  KEEP NEWLINE DLINE GRID DISTANCES FROM THE BOUNDARY
      ANG1=ATAN(SLOPE)
      ANG1N=ATAN(SLOPEN)
      IF(SLOPE.GT.0.0.AND.LSBC(II,JJ,2).EQ.1)DLINE=-DLINE
C  TEST DLINE
      IF(LSBC(II,JJ,2).EQ.2)DLINE=-DLINE
      DLINEY=DLINE*(SIN(ANG1N))
      DLINEX=DLINE*(COS(ANG1N))
      XN=XN+DLINEX
      YN=YN+DLINEY
      DSX2=XN-XN1
      DSY2=YN-YN1
      DS2=SQRT((DSX2*DSX2)+(DSY2*DSY2))
      IF(DS2.LT.(ABS(DLINE)-0.02))I2CLOSE=I2CLOSE+1
      IF(DS2.LT.(ABS(DLINE)-0.02))N2CLOSE=N2CLOSE+1
c      IF(DS2.LT.(ABS(DLINE)-0.02))WRITE(9,55991)IDY9,I2CLOSE,N2CLOSE,DS,
c     *DS2,TX2,TY2,XN,YN
55991 FORMAT(' STILL TO CLOSE--',I4,I2,I3,' DS=',2F10.8,'TXY=',2F7.2,' X
     *N=',2F7.2)
C
CC  THUS, NEW LOCATION FOR POINT 2 WITHIN THE BOUNDARY IS
      GU2(I,J)=XN
      GV2(I,J)=YN
C  HANDLE OFF MAP POINTS
c      IF(II.LE.2)WRITE(9,3198)I,J,GU1(I,J),GV1(I,J),GU2(I,J),GV2(I,J),U1
c     *,V1,UU,VV,K3
c      IF(JJ.LE.3)WRITE(9,3198)I,J,GU1(I,J),GV1(I,J),GU2(I,J),GV2(I,J),U1
c     *,V1,UU,VV,K3
c      IF(GV2(I,J).GE.90.99)WRITE(9,8341)K3,I,J,II,JJ,GV2(I,J)
c      IF(GV2(I,J).LT.3.0)WRITE(9,8341)K3,I,J,II,JJ,GV2(I,J)
c      IF(GU2(I,J).GE.177.99)WRITE(9,8341)K3,I,J,II,JJ,GU2(I,J)
c      IF(GU2(I,J).LT.3.0)WRITE(9,8341)K3,I,J,II,JJ,GU2(I,J)
      IF(GV2(I,J).GE.90.99)GV2(I,J)=90.99
      IF(GV2(I,J).LT.3.00)GV2(I,J)=3.00
      IF(GU2(I,J).GE.177.99)GU2(I,J)=177.99
      IF(GU2(I,J).LT.3.00)GU2(I,J)=3.00
C
C
C  TEST FOR RELOCATION INTO A NEW GRID SQUARE
C  AND RETEST THE BOUNDARIES THERE
      JJXN=GU2(I,J)
      IIYN=GV2(I,J)
c      IF(II.LE.2)WRITE(9,8976)I,J,TX1,TY1,TX2,TY2,II,JJ,YVAL,XN,YN,IIYN,
c     *JJXN
c      IF(JJ.LE.3)WRITE(9,8976)I,J,TX1,TY1,TX2,TY2,II,JJ,YVAL,XN,YN,IIYN,
c     *JJXN
      IF(JJ.LT.104.OR.JJ.GT.110)GO TO 5072
      IF(II.LT.6.OR.II.GT.10)GO TO 5072
 8976 FORMAT(' ',I3,I4,F7.2,F6.2,F7.2,F6.2,I3,I4,' YB=',F6.3,' XN=',F6.2
     *,' YN=',F5.2,I3,I4)
 5072 CONTINUE
      IF(IIYN.EQ.3.OR.II.EQ.3)GO TO 5080
      IF(JJYN.EQ.3.OR.JJ.EQ.3)GO TO 5080
      IF(IIYN.NE.II)GO TO 5070
      IF(JJXN.NE.JJ)GO TO 5070
c      IF(I2CLOSE.GT.0)WRITE(9,55992)IDY9,I2CLOSE,N2CLOSE,DS,DS2,TX2,TY2,
c     *XN,YN,II,JJ,IIYN,JJXN
55992 FORMAT(' CORRECT AGAIN--',I4,I1,I3,' DS=',2F10.8,'TXY=',2F7.2,' XN
     *=',2F7.2,' II=',I3,I4,' IIYN=',I3,I4)
      IF(N2CLOSE.GT.10)GO TO 902
      IF(I2CLOSE.GT.0)GO TO 5070
 5080 CONTINUE
C---------------------------------END OF BOUNDARY SLIP CORRECTION
C  HANDLE OFF MAP POINTS
c      IF(II.LE.2)WRITE(9,3198)I,J,GU1(I,J),GV1(I,J),GU2(I,J),GV2(I,J),U1
c     *,V1,UU,VV,K3
c      IF(JJ.LE.3)WRITE(9,3198)I,J,GU1(I,J),GV1(I,J),GU2(I,J),GV2(I,J),U1
c     *,V1,UU,VV,K3
c      IF(GV2(I,J).GE.90.99)WRITE(9,8341)K3,I,J,II,JJ,GV2(I,J)
c      IF(GV2(I,J).LT.3.0)WRITE(9,8341)K3,I,J,II,JJ,GV2(I,J)
c      IF(GU2(I,J).GE.177.99)WRITE(9,8341)K3,I,J,II,JJ,GU2(I,J)
c      IF(GU2(I,J).LT.3.0)WRITE(9,8341)K3,I,J,II,JJ,GU2(I,J)
      IF(GV2(I,J).GE.90.99)GV2(I,J)=90.99
      IF(GV2(I,J).LT.3.00)GV2(I,J)=3.00
      IF(GU2(I,J).GE.177.99)GU2(I,J)=177.99
      IF(GU2(I,J).LT.3.00)GU2(I,J)=3.00
C
C>>>>>>>>
C  LAT-LON
      CALL IJTOLL(GV1(I,J),GU1(I,J),GLA1,GLO1)
      CALL IJTOLL(GV2(I,J),GU2(I,J),GLA2,GLO2)
C  BETTER LONG AND LAT
      JGU1=GU1(I,J)
      IGV1=GV1(I,J)
  100 FORMAT(8F8.3,3I3,A3)
c
c--------------------------------------------------------------
C  STORE PROGRESSIVE VECTOR FILES (60)
      IF(IDY9.GT.999)IDY8=IDY9-999
      IF(IDY9.LE.999)IDY8=IDY9
      ihr=0
c  add header to file for readers
c      if(idy9.eq.1)write(6,74105)
c      if(idy9.eq.1)write(67,74105)
c74105 format(1x,'Nlat-1 Elon-1  Nlat-2 Elon-2   day# tr year mm dd hh wf
c     *x wfx2 angl deltd')
c  change back to East longitudes for std output file (.out)
c  write output every 30 days to screen
      do 75104 ip30d=1,ndays,30
c      if(idy9.eq.1.and.ip30d.eq.1)WRITE(6,74104)gla1,glo1,gla2,glo2,IDY9
c     *,K3,KYR,IMO,IDY,ihr,wfx,WFAC,angFAC,DDFAC,ndays
c      if(idy9.eq.ip30d)WRITE(6,74104)gla1,glo1,gla2,glo2,IDY9,K3,KYR,
c     *IMO,IDY,ihr,wfx,WFAC,angFAC,DDFAC,ndays
c      if(idy9.eq.ip30d)WRITE(9,74104)gla1,glo1,gla2,glo2,IDY9,K3,KYR,
c     *IMO,IDY,ihr,wfx,WFAC,angFAC,DDFAC,ndays
75104 continue
c  change back to East longitudes for std output
      glo1e=360.0-glo1
      glo2e=360.0-glo2
c      WRITE(6,74104)gla1,glo1e,IDY9,K3,KYR,IMO,IDY,ihr
      WRITE(6,74106)KYR,IMO,IDY,gla1,glo1e
      WRITE(67,74108)KYR,IMO,IDY,gla1,glo1e
c      WRITE(67,74104)gla1,glo1e,gla2,glo2e,IDY9,K3,KYR,IMO,IDY,ihr,wfx,
c     * WFAC,ANGFAC,DDFAC
c      WRITE(9,74104)gla1,glo1e,gla2,glo2e,IDY9,K3,KYR,IMO,IDY,ihr,wfx,
c     * WFAC,ANGFAC,DDFAC
74104 FORMAT(f7.3,f8.3,f7.3,f8.3,i5,i3,i5,3I3,2F5.2,F6.2,F5.2,i6)
c74106 FORMAT('["',i4,'-',i2.2,'-',i2.2,' 00:00:00",',f7.3,',',f8.3,'],')
74106 FORMAT('["',i4,'-',i2.2,'-',i2.2,'",',f7.3,',',f8.3,'],')
74108 FORMAT(i4,'-',i2.2,'-',i2.2,',',f7.3,',',f8.3)
c--------------------------------------------------------------
C  CREATE FILE OF WIND VECTORS AT NIKE SHOE TRAJECTORY POINTS
      JW=GU1(I,J)
      IW=GV1(I,J)
      IF(IDY8.EQ.1)JST=JW
      IF(IDY8.EQ.1)IST=IW
      JTRK=JW
      ITRK=IW
C  WRITE FILE 62 (ITRK=INTEGER I ALONG TRACK....)
 4105 FORMAT(2I3,3I2,2I3,8F7.3)
99908 CONTINUE
      GU1(I,J)=GU2(I,J)
      GV1(I,J)=GV2(I,J)
 9100 CONTINUE
 9500 CONTINUE 
C  ---------------------------------------------------------------------
C     END OF DAILY DO LOOP REPEATED FOR ndays
C  ---------------------------------------------------------------------
       WRITE(67,74108)KYR,IMO,IDY,gla1,glo1e
      WRITE(6,74107)KYR,IMO,IDY,gla2,glo2e
74107 FORMAT('["',i4,'-',i2.2,'-',i2.2,' 23:59:59",',f7.3,',',
     * f8.3,']]}')
  909 CONTINUE
c      WRITE(9,9919)ITYP,IMO,IMEND,IYRSLP
 9919 FORMAT(' I COME FROM 909, ITYP=',I5,' IMO=',I2,' IMEND=',I9,' IYRS
     *LP=',I5)
      IF(NOSTOR.LT.1)CLOSE(UNIT=52)
      IF(NOSTOR.LT.1)CLOSE(UNIT=53)
      IF(NOSTOF.LT.1)CLOSE(UNIT=54)
      IF(NOSTOR.LT.1)CLOSE(UNIT=55)
  800 CONTINUE
C
c      WRITE(9,30)IDY,IYRSLP
   30 FORMAT(' YEARS NOT RIGHT.    IDY=',I4,', LAST=',I4)
  901 CONTINUE
      CLOSE(UNIT=67)
      CLOSE(UNIT=62)
c      WRITE(9,9939)IYRSLP,IMO,IMEND,ndays
 9939 FORMAT(' END OSCURS MODEL, YEAR=',I4,' ST MO=',I4,' END MO=',
     &I2,' ndays=',I4,' !! HIT KB KEY !!')
  902 CONTINUE
C
      CLOSE(UNIT=4)
      CLOSE(UNIT=9)
      CLOSE(UNIT=10)
      CLOSE(UNIT=16)
      CLOSE(UNIT=13)
      CLOSE(UNIT=12)
      CLOSE(UNIT=27)
      CLOSE(UNIT=28)
      CLOSE(UNIT=38)
      CLOSE(UNIT=39)
      CLOSE(UNIT=43)
      CLOSE(UNIT=44)
      CLOSE(UNIT=45)
      CLOSE(UNIT=46)
      CLOSE(UNIT=48)
      CLOSE(UNIT=50)
      CLOSE(UNIT=51)
      CLOSE(UNIT=52)
      CLOSE(UNIT=53)
      CLOSE(UNIT=54)
      CLOSE(UNIT=55)
      CLOSE(UNIT=62)
      CLOSE(UNIT=63)
      CLOSE(UNIT=70)
      CLOSE(UNIT=71)
      CLOSE(UNIT=72)
      CLOSE(UNIT=73)
      CLOSE(UNIT=74)
      CLOSE(UNIT=77)
      CLOSE(UNIT=81)
      CLOSE(UNIT=82)
      CLOSE(UNIT=83)
      CLOSE(UNIT=84)
      CLOSE(UNIT=85)
      CLOSE(UNIT=86)
      CLOSE(UNIT=87)
      CLOSE(UNIT=88)
      CLOSE(UNIT=89)
      CLOSE(UNIT=90)
      CLOSE(UNIT=91)
      CLOSE(UNIT=92)
      CLOSE(UNIT=93)
c  close netCDF file
C  ---------------------------------------------------------------------
C  ---------------------------------------------------------------------
c  restart for next line of input file
      ITYP=0
C  Comment out next line for LAS use
c      go to 99966
99967 continue
C  put following line at end of program to close NetCDF file
      IERCODE=NF_CLOSE(NCID)
C      ierr = ieee_flags('clear', 'exception', 'all', out )
      CLOSE(UNIT=11)
      CLOSE(UNIT=8)
C  ---------------------------------------------------------------------
C  ---------------------------------------------------------------------
C     END  OF  MAIN  PROGRAM
C  ---------------------------------------------------------------------
C  ---------------------------------------------------------------------
      END
C__________________________________________
	subroutine setup_input

C subroutine to open netCDF pressure file and set up for oscurs extract
	
      PARAMETER (NREC=    20000)   !CHANGE THIS FOR MORE RECS
      INTEGER*4 IERCODE,dims(3),corner(3), edges(3),START(10),
     1     COUNT(10),RECDIM
      INTEGER time_index,idate_base,idate_new,DayNum,VDIMS(10),
     1  day1,NCID
      LOGICAL LeapYr
      CHARACTER infile*22
      REAL*8 time_series(NREC)
      REAL*4 pmsl(180,92),x(180),y(92),lon(180,92),lat(180,92)
      EXTERNAL DayNum
	COMMON /netcdf/NCID,day1,idate_base,ivaridp

	infile(1:18)='oscurs_pressure.nc'


C  open input file
      iercode=NF_OPEN(infile(1:18),0,NCID)
c	write(6,*)'opening ',infile(1:18),' IERCODE=',IERCODE
      iercode=NF_INQ_UNLIMDIM(NCID,RECDIM)
      iercode=NF_INQ_DIMLEN(NCID,RECDIM,NRECS)
c        write(6,*)NRECS,' records in file'
C     !NRECS now contains number of records in this file
C
C    statements to fill lat                            
C
      IERCODE = NF_INQ_VARID(NCID,'lat',ivarid )
	start(1)=1
	count(1)=180
      start(2)=1
      count(2)=92
      IERCODE = NF_GET_VARA_REAL(NCID,ivarid,START,COUNT,lat)
C
C    statements to fill lon                            
C
      IERCODE = NF_INQ_VARID(NCID,'lon',ivarid)
      START(1)=1
      COUNT(1)=180
      start(2)=1
      count(2)=92
      IERCODE = NF_GET_VARA_REAL(NCID,ivarid,START,COUNT,lon)

C    statements to fill time_series                    
C
      IERCODE = NF_INQ_VARID(NCID,'time_series',ivarid)
      IERCODE = NF_INQ_VARDIMID(NCID,ivarid,VDIMS)
      IERCODE = NF_INQ_DIMLEN(NCID,VDIMS(1),NDSIZE)
      START(1)=1
      COUNT(1)=NDSIZE
      IERCODE = NF_GET_VARA_DOUBLE(NCID,ivarid,START,COUNT,time_series)
     
c days from Jan 1948 to beg of file
	day1=time_series(1)/24

C  Input file time origin is Jan 1948
	idate_base=DayNum(1948,1,1)
c      write(6,9528)time_series(1),day1,idate_base
 9528 format(' input setup ',' t_s',f20.2,' day1= ',i10,' id_b=',i10)
c get pressure ID

       IERCODE = NF_INQ_VARID(NCID,'pmsl',ivaridp)
C
	return
	end


C__________________________________________
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

C__________________________________________
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
C__________________________________________
      subroutine get_pressure(iyear,month,iday,iflagt,time_index2,
     1   pmslout)

C  extract pressure for 0000Z for day iyear,month,iday

      INTEGER*4 IERCODE,START(10),COUNT(10)
      REAL*4 pmsl(180,92),pmslout(92,180)
      INTEGER time_index,day1,idate_base,time_index2,DayNum
      EXTERNAL DayNum

	COMMON /netcdf/NCID,day1,idate_base,ivaridp

c	write(6,9527)iyear, month, iday, idate_base, day1,iflagt
 9527 format(' subr ',i5,2i3,' idate_b=',i10,' day1=',i10,
     1   ' iflagt=',i1)
      idate_year=0
      itime=0
      time_index=0
C  Requested time
	if(iflagt.eq.0)idate_year=DayNum(iyear,month,iday)
C  Days from 1-1-48 to requested time
	 if(iflagt.eq.0)itime=(idate_year-idate_base)
c days from beg of file to requested time
	 if(iflagt.eq.0)time_index=itime-day1+1
c
       if(iflagt.eq.0)time_index2=time_index
       if(iflagt.eq.1)time_index2=time_index2+1
       if(iflagt.eq.1)time_index=time_index2
c      write(6,9529)idate_year,itime,time_index,time_index2,iflagt
 9529 format(' subr ','idate_y=',i11,' itime=',i11,' itime_index=',i11,
     &' time_index2=',2i11)
C
C    statements to fill Pmsl                          
C
            START(1) = 1
            START(2) = 1
	    start(3) = time_index   
            COUNT(1) = 180
            COUNT(2) = 92
            COUNT(3) = 1
        IERCODE = NF_GET_VARA_REAL(NCID,ivaridp,START,COUNT,pmsl)
	do 101 ii=1,180
	do 101 jj=1,92
	iii=181-ii
101	pmslout(jj,iii)=pmsl(ii,jj)

	return
	end

C__________________________________________
	SUBROUTINE RDNFIL(KFIL,IMO,IYR,IDY,ITYP,C)
C__________________________________________
	DIMENSION LS(20,44),A(20,44)
	DIMENSION C(20,44)
	ITYP=0
  900 CONTINUE
      DO 100 J=1,44
	JJ=45-J
	READ(KFIL,2000,END=800)IY,MM,NDY,IHR,(A(II,JJ),II=1,10)
	READ(KFIL,2000,END=800)IY,MM,NDY,IHR,(A(II,JJ),II=11,20)
  100 CONTINUE
 1100 FORMAT(1X,3I2,I2,6X,10F6.1)
 2000 FORMAT(1X,i4,2I2,I2,6X,10F6.1)
c      IF(NDY.EQ.1.and.j.eq.1)WRITE(9,1110) IY,MM,NDY,IHR
 1110 FORMAT (10X,4i5,' FNWC (20X44) dslp FILE READ')
C  FLAG OUT NON 00Z TIME FIELDS
	IF(IHR.NE.0) GO TO 900
C
	IYE=IY
	MME=MM
	NDYE=NDY
	DO 210 I=1,20
	DO 210 J=1,44
	II=21-I
	C(II,J)=A(I,J)
  210 CONTINUE
	IMO=MM
	IYR=IY
	IDY=NDY
C  IF THIS IS THE LAST DAY OF THE YEAR JUMP TO FLAG AND KFILE CHANGE
C  STOP AT DAY 12-31
	IF(IMO.EQ.12.AND.IDY.EQ.31)GO TO 400
	GO TO 700
  800 CONTINUE
C  800 WRITE(6,10)IYE,MME,NDYE
c	WRITE(9,10)kfil,IY,MM,NDY
c        WRITE(6,10)kfil,IY,MM,NDY
   10 FORMAT(' END OF FILE #',i3,'  ERROR-WENT BEYOND 12-31, RED=',3I5)
      ITYP=888
      	RETURN
  400 CONTINUE
C  SET ITYP=2 ONLY AFTER READING 12-31
	ITYP=2
c	WRITE(6,991)KFIL,IY,MM,NDY,IHR
c	WRITE(9,991)KFIL,IY,MM,NDY,IHR
  991 FORMAT(' CHANGE FILES TO KFILE=',I3,' FROM=',4I5)
	RETURN
c  600 ITYP=888
  700 RETURN
	END
C__________________________________________
	SUBROUTINE READER(IMO,IYR,IDY,ITYP,C)
C__________________________________________
	DIMENSION LS(20,44),A(20,44)
	DIMENSION C(20,44)
	NN=20
	MM=44
	ITYP=0
  900 CONTINUE
	DO 100 J=1,44
	JJ=45-J
	READ(2,1100,END=800)IY,MM,NDY,IHR,(A(II,JJ),II=1,10)
	READ(2,1100,END=800)IY,MM,NDY,IHR,(A(II,JJ),II=11,20)
  100 CONTINUE
 1100 FORMAT(1X,3I2,I2,6X,10F6.1)
C      IF(NDY.EQ.1)WRITE(9,1110) IY,MM,NDY,IHR
c	WRITE(9,1110) IY,MM,NDY,IHR
 1110 FORMAT (10X,4I3,' FNWC (20X44) FILE READ')
C  FLAG OUT NON 00Z TIME FIELDS
	IF(IHR.NE.0) GO TO 900
	IYE=IY
	MME=MM
	NDYE=NDY
	DO 210 I=1,20
	DO 210 J=1,44
	II=21-I
	C(II,J)=A(I,J)
  210 CONTINUE
	IMO=MM
	IYR=IY
	IDY=NDY
	GO TO 700
  800 CONTINUE
C      KEY=IXKEY()
C  800 WRITE(6,10)IYE,MME,NDYE
c	WRITE(9,10)IY,MM,NDY
C      WRITE(6,10)IY,MM,NDY
   10 FORMAT(" END OF FILE.  LAST RECORD FOUND FOR",3I5)
	ITYP=2
	RETURN
c  600 ITYP=888
  700 RETURN
	END
C__________________________________________
	SUBROUTINE READER2(IMO,IYR,IDY,ITYP,C)
C__________________________________________
	DIMENSION LS(20,44),A(20,44)
	DIMENSION C(20,44)
C
	IMO1=IMO
	IYR1=IYR
	IDY1=IDY
	NN=20
	MM=44
	ITYP=0
  900 CONTINUE
	DO 100 J=1,44
	JJ=45-J
	READ(92,1100,END=800)IY,MM,NDY,IHR,(A(II,JJ),II=1,10)
	READ(92,1100,END=800)IY,MM,NDY,IHR,(A(II,JJ),II=11,20)
  100 CONTINUE
 1100 FORMAT(1X,3I2,I2,6X,10F6.1)
c	IF(NDY.EQ.1)WRITE(9,1110) IY,MM,NDY,IHR
 1110 FORMAT (10X,4I3,' FNWC (20X44) FILE READ')
C  FLAG OUT NON 00Z TIME FIELDS
	IF(IHR.NE.0) GO TO 900
	IYE=IY
	MME=MM
	NDYE=NDY
	DO 210 I=1,20
	DO 210 J=1,44
	II=21-I
	C(II,J)=A(I,J)
  210 CONTINUE
	IMO=MM
	IYR=IY
	IDY=NDY
	GO TO 700
  800 CONTINUE
C      KEY=IXKEY()
C  800 WRITE(6,10)IYE,MME,NDYE
c	WRITE(9,10)IYE,MME,NDYE
   10 FORMAT(" END OF FILE.  LAST RECORD FOUND FOR",3I5)
	ITYP=92
	IYR=IYR1
	IMO=IMO1
	IDY=IDY1
	RETURN
c  600 ITYP=888
  700 RETURN
	END
C__________________________________________
      SUBROUTINE BESSEL (XJ,XI,IOK,VAL,DATA)
C__________________________________________
      DIMENSION A(4),AIJ(4)
	DIMENSION DATA(92,180)
      IF(IOK.NE.99) GO TO 300
	DO 310 I=1,92
c	WRITE(9,305) (DATA(I,J),J=1,50,2)
  310 CONTINUE
  305 FORMAT(25F5.1)
  300 CONTINUE
      IOK=0
C  LIMIT CALCULATIONS WITHIN 1 INTERIOR POINT OF LEFT AND BOTTOM
C  LIMIT CALCULATIONS WITNIN 2 INTERIOR PTS OF RIGHT AND TOP
C  DO 4 POINT BELLEL INTERPOLATION FOR VALUES BETWEEN GRID POINTS
      IA=XI
      JA=XJ
C  TEST IF INDEX WITHIN RANGE
      IF (IA.LT.2.OR.JA.LT.2)   GO TO 998
      IF (IA.GT.18.OR.JA.GT.42)  GO TO 998
      XINC=XJ-FLOAT(JA)
      YINC=XI-FLOAT(IA)
      DO 105 K=1,4
      JJ=JA+(K-2)
      DO 106 L=1,4
      II=IA+(L-2)
      AIJ(L)=DATA(II,JJ)
c	IF(XJ.LT.1.9)WRITE(9,450)XI,XJ,L,II,JJ,AIJ(L)
  106 CONTINUE
C  VERTICAL INTERPOLATION
      A(K)=AIJ(2)+YINC*((AIJ(3)-AIJ(2))+((YINC-1.0)/4.0)*((AIJ(4)-AIJ(3)
     1)+(AIJ(1)-AIJ(2))))
c	IF(XJ.LT.1.9)WRITE(9,451)XI,XJ,K,A(K)
  105 CONTINUE
C  HORIZONTAL INTERPOLATION
      VAL=A(2)+XINC*((A(3)-A(2))+((XINC-1.0)/4.0)*((A(4)-A(3))+
     1(A(1)-A(2))))
c	IF (XJ.LT.1.9)WRITE(9,452)XI,XJ,VAL
  450 FORMAT(2F7.3,3I4,F7.3)
  451 FORMAT(2F7.3,I4,F7.3)
  452 FORMAT(3F7.3)
      GO TO 999
  998 IOK=1
      VAL=999999
  999 RETURN
      END
C__________________________________________
C      SUBROUTINE CONVIJRR (XX,YY,IERR,XLEN,YYLEN,M,N)
	SUBROUTINE CONVIJO (XX,YY,IERR)
C__________________________________________
C
	COMMON/GRDCOM/CFACT,M,N,NM,XI(181),YJ(93)
	COMMON/SIZES/XLEN,YLEN,YYLEN,SIZ1H,SIZ1V,SIZ2H,SIZ2V,
     &SIZ3H,SIZ3V,SIZ4H,SIZ4V
	COMMON/LAT/ALAT2,LS2
	COMMON/LON/ALON2
	DIMENSION ALAT2(92,180),ALON2(92,180),LS2(92,180)
	DIMENSION SLON(92,180)
	IF (XX.LT.0.0) XX=-XX
C
C  GIVEN A RANDOM POINT ON THE GLOBE (LONGITUDE,LATITUDE) DEGREE-DECIMAL
C  AND A GRID MESH OF DEFINED (LONG,LAT) POINTS
C  FIND THE (I,J) COORDINATES OF THE RANDOM POINT (XX,YY)
C  RELATIVE TO THE GIVEN GRID MESH
C  REJECT POINTS OUTSIDE OF THE GRID
C  THIS IS A GENERALIZED SUBPROGRAM TO FIT ANY NEARLY SQUARE
C  EQUIDISTANT GRID IN GLOBE (SPHERICAL) SPACE
C  BUT
C  GRID LINES MAY HAVE A CONSTANT SLOPE OR MAY VARY WITHIN GRID
C  IN LONGITUDE-LATITUDE SPACE
C  FOR THE GIVEN (NXM) GRID ----- N=ROW#, M=COL# -----
C  THIS RESULTS IN THE ABILITY TO MAP SQUARE OR ODD SHAPED GRIDS
C  INTO A SQUARE OR RECTANGULAR PLOT TO MATCH THE PRINTED PAGE
C  XLEN IS THE DESIRED LENGTH OF THE X-AXIS IN INCHES
C  YLEN IS THE DESIRED LENGTH OF THE Y-AXIS IN INCHES
	N1=N-1
	M1=M-1
	DXX=XLEN/M1
	DYY=YYLEN/N1
C  REJECT MOST POINTS OUT OF GRID EXTREMES
	IERR=0
C  FIND MAX AND MIN LAT AND LONG OF GRID
	AMAXLA=0.0
	AMAXLO=0.0
	AMINLA=1000.0
	AMINLO=1000.0
	DO 1201 I=1,N
	DO 1202 J=1,M
	IF (ALAT2(I,J).GT.AMAXLA) AMAXLA=ALAT2(I,J)
	IF (ALON2(I,J).GT.AMAXLO) AMAXLO=ALON2(I,J)
	IF (ALAT2(I,J).LT.AMINLA) AMINLA=ALAT2(I,J)
	IF (ALON2(I,J).LT.AMINLO) AMINLO=ALON2(I,J)
 1202 CONTINUE
 1201 CONTINUE
C  REJECT POINT IF OUT OF GRID MAX AND MIN LAT OR LONG
	IF (YY.GT.AMAXLA) IERR=1
	IF (YY.LT.AMINLA) IERR=1
	IF (XX.GT.AMAXLO) IERR=1
	IF (XX.LT.AMINLO) IERR=1
C  JUMP TO ERROR IF POINT IS OUTSIDE OF SQUARE OF BOUNDARY EXTREMES
C  AND SET BOTH COORDINATES TO MAXIMUM PLOT INCHES BEFORE RETURN
 1240 IF (IERR.EQ.1.AND.XX.GT.XLEN) XX=XLEN
	IF (IERR.EQ.1.AND.YY.GT.YYLEN) YY=YYLEN
	IF (IERR.EQ.1)  GO TO 9999
C  IN CASE THE GRID IS TILTED (ALL OR IN PART)
C  LOOK AROUND GRID BOUNDARY LINES TO REJECT EXTERIOR POINTS
C  COMPUTE EQUATION OF LINE BETWEEN 2 POINTS
C  IF XX IS BETWEEN THE POINTS TEST IF YY INSIDE OR OUTSIDE OF GRID
C  COVER THE CASES FOR EACH SIDE OF GRID SLOPING + OR - (LAT=HORIZ)
C  LOOK AT TOP AND BOTTOM LINES OF GRID
	I=1
 8002 DO 8001 J=1,M1
	X1=ALON2(I,J)
	X2=ALON2(I,J+1)
	Y1=ALAT2(I,J)
	Y2=ALAT2(I,J+1)
	IF (XX.GT.X1.OR.XX.LT.X2) GO TO 8001
	IF (X2.EQ.X1) GO TO 8001
	Y0=Y1+((Y2-Y1)*(XX-X1)/(X2-X1))
	IF (YY.GT.Y0.AND.I.EQ.1) IERR=1
	IF (YY.GT.Y0.AND.I.EQ.1) GO TO 1240
	IF (YY.LT.Y0.AND.I.EQ.N) IERR=1
	IF (YY.LT.Y0.AND.I.EQ.N) GO TO 1240
 8001 CONTINUE
	IF (I.EQ.N) GO TO 8025
	I=N
	GO TO 8002
 8025 CONTINUE
C  LOOK AT LEFT AND RIGHT BOUNDARY LINES
	J=1
 8007 DO 8006 I=1,N1
	X1=ALON2(I,J)
	X2=ALON2(I+1,J)
	Y1=ALAT2(I,J)
	Y2=ALAT2(I+1,J)
	IF (X2.EQ.X1) GO TO 8006
	IF ((X2-X1).LT.0.0) GO TO 8013
	IF (XX.LT.X1.OR.XX.GT.X2) GO TO 8006
	GO TO 8014
 8013 IF (XX.GT.X1.OR.XX.LT.X2) GO TO 8006
 8014 Y0=Y1+((Y2-Y1)*(XX-X1)/(X2-X1))
	IF (J.EQ.1.AND.(X2-X1).LT.0.0) GO TO 8011
	IF (J.EQ.M.AND.(X2-X1).GT.0.0) GO TO 8011
	IF (YY.GT.Y0) IERR=1
	IF (YY.GT.Y0) GO TO 1240
	GO TO 8006
 8011 CONTINUE
	IF (YY.LT.Y0) IERR=1
	IF (YY.LT.Y0) GO TO 1240
 8006 CONTINUE
	IF (J.EQ.M)GO TO 8035
	J=M
	GO TO 8007
 8035 CONTINUE
C  NOW, WE DEFINITELY KNOW (XX,YY) IS INSIDE THE GRID OR ON BOUNDARY
C  LOOK THROUGH ENTIRE GRID FOR CLOSEST GRID POINT TO POINT(XX,YY)
C  SEARCH USING LEAST SQUARE DISTANCE IN LINEAR 2D DEGREE SPACE
	DMIN=1000.0
	PI=3.141592
	XX=-XX
	DO 1207 I=1,N
	DO 1208 J=1,M
	SLON(I,J)=-ALON2(I,J)
	X2=SLON(I,J)
	Y2=ALAT2(I,J)
	DIST=SQRT(((X2-XX)*(X2-XX))+((Y2-YY)*(Y2-YY)))
	IF (DIST.LT.DMIN) NLAT=I
	IF (DIST.LT.DMIN) MLON=J
	IF (DIST.LT.DMIN) DMIN=DIST
 1208 CONTINUE
 1207 CONTINUE
C  NLAT,MLON ARE THE I,J COORDINATES FOR THE NEAREST GRID POINT
C  SET UP LAT AND LONG OF 4 SURROUNDING GRID POINTS
C       P2
C       .
C       .
C       .
C  P3...P0...P1
C       .
C       .
C       .
C       P4
C  COMPUTATIONS ARE NOW DONE IN THIS 4-QUADRANT SYSTEM
C  ON BOUNDARY AVOID POINTS OUTSIDE OF GRID (SET THEM = CLOSEST GRID PT)
	C=1000.0
	P0X=SLON(NLAT,MLON)
	P0Y=ALAT2(NLAT,MLON)
	P1X=P0X
	P2X=P0X
	P3X=P0X
	P4X=P0X
	P1Y=P0Y
	P2Y=P0Y
	P3Y=P0Y
	P4Y=P0Y
	IF (MLON.LT.M) P1X=SLON(NLAT,MLON+1)
	IF (MLON.LT.M) P1Y=ALAT2(NLAT,MLON+1)
	IF (NLAT.GT.1) P2X=SLON(NLAT-1,MLON)
	IF (NLAT.GT.1) P2Y=ALAT2(NLAT-1,MLON)
	IF (MLON.GT.1) P3X=SLON(NLAT,MLON-1)
	IF (MLON.GT.1) P3Y=ALAT2(NLAT,MLON-1)
	IF (NLAT.LT.N) P4X=SLON(NLAT+1,MLON)
	IF (NLAT.LT.N) P4Y=ALAT2(NLAT+1,MLON)
C  SPHERICAL GEOMETRY OF THE GLOBE
C    DICTATES THAT CONVERGING LONGITUDES MUST BE
C      MULTIPLIED BY COS(LAT) TO PRESERVE EQUIVALENT COORDINATE SCALES
	XC=COS(ALAT2(NLAT,MLON)*PI/180.0)
	DX0=(XX-P0X)*XC
	DX1=(P1X-P0X)*XC
	DX2=(P2X-P0X)*XC
	DX3=(P3X-P0X)*XC
	DX4=(P4X-P0X)*XC
	DY0=YY-P0Y
	DY1=P1Y-P0Y
	DY2=P2Y-P0Y
	DY3=P3Y-P0Y
	DY4=P4Y-P0Y
	S0=DX0*DY0
	S1=DX1*DY1
	S2=DX2*DY2
	S3=DX3*DY3
	S4=DX4*DY4
C  COMPUTE ANGLE (AP) BETWEEN LINE (XX,YY)-(MLON,NLAT) AND THE +X AXIS
C  AND AVOID INFINITE TANGENTS
	AP=0.0
	AE=0.0
	AN=0.0
	AW=0.0
	AS=0.0
	IF (ABS(DX0).LE.(1.0/C)) AP=ATAN(SIGN(C,S0))
	IF (ABS(DX0).GT.(1.0/C)) AP=ATAN((DY0)/DX0)
	CALL RADCOR(DX0,DY0,ADD)
	AP=AP+ADD
C  COMPUTE THE ANGLE EACH OF THE 4 COORDINATES MAKES WITH THE +X AXIS
C  AE IS THE ANGLE THE EAST LINE (P1-P0) MAKES WITH THE +X AXIS
	IF (ABS(DX1).LE.(1.0/C)) AE=ATAN(SIGN(C,S1))
	IF (ABS(DX1).GT.(1.0/C)) AE=ATAN((DY1)/DX1)
	CALL RADCOR(DX1,DY1,ADD)
	AE=AE+ADD
C  AN IS THE ANGLE THE NORTH LINE (P2-P0) MAKES WITH THE +X AXIS
	IF (ABS(DX2).LE.(1.0/C)) AN=ATAN(SIGN(C,S2))
	IF (ABS(DX2).GT.(1.0/C)) AN=ATAN((DY2)/DX2)
	CALL RADCOR(DX2,DY2,ADD)
	AN=AN+ADD
C  AW IS THE ANGLE THE WEST LINE (P3-P0) MAKES WITH THE +X AXIS
	IF (ABS(DX3).LE.(1.0/C)) AW=ATAN(SIGN(C,S3))
	IF (ABS(DX3).GT.(1.0/C)) AW=ATAN((DY3)/DX3)
	CALL RADCOR(DX3,DY3,ADD)
	AW=AW+ADD
C  AS IS THE ANGLE THE SOUTH LINE (P4-P0) MAKES WITH THE +X AXIS
	IF (ABS(DX4).LE.(1.0/C)) AS=ATAN(SIGN(C,S4))
	IF (ABS(DX4).GT.(1.0/C)) AS=ATAN((DY4)/DX4)
	CALL RADCOR(DX4,DY4,ADD)
	AS=AS+ADD
C  COMPUTE LENGTH TO POINT(XX,YY)
	D0=SQRT((DX0*DX0)+(DY0*DY0))
	D1=SQRT((DX1*DX1)+(DY1*DY1))
	D2=SQRT((DX2*DX2)+(DY2*DY2))
	D3=SQRT((DX3*DX3)+(DY3*DY3))
	D4=SQRT((DX4*DX4)+(DY4*DY4))
C  COMPUTE (ANGLE) THE ANGLE FROM +X AXIS OF ROTATED GRID
C  AE IS THE BASE LINE USED FOR COMPUTING + ANGLES BECAUSE
C  IT CORRESPONDS TO THE +X AXIS OF THE ROTATED GRIDS
C  WARNING:  THE TRIGONOMETRY ASSUMES RIGHT ANGLES WITHIN THE GRID--
C             BUT TESTS INDICATE THIS ROUTINE PERFORMES WELL
C            FOR RELATIVELY SMALL DEVIATIONS (RT. ANGLE +- 10%)
	IF (MLON.EQ.M.AND.AW.LT.PI) AE=AW+PI
	IF (AP.LT.AE) ANGLE=(2.0*PI)+AP-AE
	IF (AP.GE.AE) ANGLE=AP-AE
C  COMPUTE FRACTIONAL I,J'S AWAY FROM GRID POINT
	IF (D0.EQ.0.0) XFRACT=0.0
	IF (D0.EQ.0.0) YFRACT=0.0
	IF (D0.EQ.0.0) GO TO 4198
	DD=D1
	IF (D1.EQ.0.0) DD=D3
	IF (DD.EQ.0.0)  XFRACT=0.0
	IF (DD.EQ.0.0)  GO TO 603
	XFRACT=(D0/DD)*COS(ANGLE)
  603 DD=D2
	IF (D2.EQ.0.0) DD=D4
	IF (DD.EQ.0.0)  YFRACT=0.0
	IF (DD.EQ.0.0)  GO TO 4198
	YFRACT=-(D0/DD)*SIN(ANGLE)
 4198 XXX=XFRACT + FLOAT(MLON)
 4199 YYY=YFRACT + FLOAT(NLAT)
C  TEST 6-1-93
	IF (XXX.LT.1.0) IERR=1
	IF (YYY.LT.1.0) IERR=1
	IF (XXX.GT.M) IERR=1
	IF (YYY.GT.N) IERR=1
C  TEST 6-1-93
	IF (XXX.LT.1.0) XXX=1.0
	IF (YYY.LT.1.0) YYY=1.0
	IF (XXX.GT.M) XXX=M
	IF (YYY.GT.N) YYY=N
C  CONVERT FROM I,J GRID VALUES TO I,J PLOT INCHES
C     XX=XLEN*((XXX-1.0)/M1)
C     YY=YYLEN-(YYLEN*((YYY-1.0)/N1))
	XX=XXX
	YY=YYY
 9999 CONTINUE
	RETURN
	END
C__________________________________________
      SUBROUTINE SMTH(N,ALPHA,FL,FL2)
C__________________________________________
	DIMENSION TDEP(92,180),TML(92,180),FL(92,180),XF(92,180),
     1TCX(92,180),TCY(92,180),XTR2(92,180),XTR(92,180),TS(92,180),
     2SLP(92,180),WCX(92,180),WCY(92,180),LS2(92,180)
	DIMENSION FL2(92,180)
      BETA=1.0-ALPHA
	DO 2 I=1,92
	DO 2 J=1,180
    2 XF(I,J)=FL(I,J)
      DO 40 M=1,N
 10   DO 20 I=2,91
	DO 20 J=2,179
      XY=FL(I,J)
      A=FL(I,J+1)
      B=FL(I+1,J)
      C=FL(I,J-1)
      D=FL(I-1,J)
      IF (A.EQ.0.OR.B.EQ.0)     GO TO 20
      IF (C.EQ.0.OR.D.EQ.0)     GO TO 20
      XF(I,J) = (ALPHA*XY) + (BETA*((A+B+C+D)/4.0))
 20   CONTINUE
C     DO 30 I=2,39
C     DO 30 J=2,49
	DO 30 I=2,91
	DO 30 J=2,179
	FL(I,J)=XF(I,J)
 30   CONTINUE
 40   CONTINUE
      RETURN
      END
C__________________________________________
	SUBROUTINE SPLINE (X,Y,IOK,VAL,DATA)
C__________________________________________
	DIMENSION A(4),AIJ(4)
	DIMENSION DATA(92,180)
	IOK=0
C  DO 4 PT NATURAL SPLINE INTERPOLATION FOR VALUES BETWEEN GRID PTS
	IA=Y
	JA=X
C  LIMIT CALCULATIONS WITHIN 1 INTERIOR POINT OF LEFT AND BOTTOM
C  LIMIT CALCULATIONS WITNIN 2 INTERIOR PTS OF RIGHT AND TOP
	IF (IA.LT.2.OR.JA.LT.2)   GO TO 998
C     IF (IA.GT.18.OR.JA.GT.42)  GO TO 998
C      IF (IA.GT.38.OR.JA.GT.102)  GO TO 998
	IF (IA.GT.90.OR.JA.GT.188)  GO TO 998
	DX=1.0
	X1=JA
	Y1=IA
	XP1=X1+1.0
	YP1=Y1+1.0
	DO 105 K=1,4
	JJ=JA+(K-2)
	DO 106 L=1,4
	II=IA+(L-2)
	AIJ(L)=DATA(II,JJ)
C     IF(XJ.LT.1.9)WRITE(9,450)XI,XJ,L,II,JJ,AIJ(L)
  106 CONTINUE
C  VERTICAL INTERPOLATION
	GPP=(AIJ(3)-(2.0*AIJ(2))+AIJ(1))/(DX*DX)
	GPPP1=(AIJ(4)-(2.0*AIJ(3))+AIJ(2))/(DX*DX)
	G1=((GPP/6.0)*((((YP1-Y)**(3.0))/DX)-(DX*(YP1-Y))))
	G2=((GPPP1/6.0)*((((Y-Y1)**(3.0))/DX)-(DX*(Y-Y1))))
	G3=(AIJ(2)*((YP1-Y)/DX))
	G4=(AIJ(3)*((Y-Y1)/DX))
	G=G1+G2+G3+G4
C     IF(G.LT.20.0)WRITE (66,9412) N,M,K,NK,GPP,GPPP1,G1,G2,G3,G4,G
	A(K)=G
  105 CONTINUE
C  HORIZONTAL INTERPOLATION
	GPP=(A(3)-(2.0*A(2))+A(1))/(DX*DX)
	GPPP1=(A(4)-(2.0*A(3))+A(2))/(DX*DX)
	G1=((GPP/6.0)*((((XP1-X)**(3.0))/DX)-(DX*(XP1-X))))
	G2=((GPPP1/6.0)*((((X-X1)**(3.0))/DX)-(DX*(X-X1))))
	G3=(A(2)*((XP1-X)/DX))
	G4=(A(3)*((X-X1)/DX))
	G=G1+G2+G3+G4
	VAL=G
  450 FORMAT(2F7.3,3I4,F7.3)
  451 FORMAT(2F7.3,I4,F7.3)
  452 FORMAT(3F7.3)
	GO TO 999
  998 IOK=1
	VAL=999999
  999 RETURN
	END
C__________________________________________
	SUBROUTINE SPLN2044(X,Y,IOK,VAL,DATA)
C__________________________________________
	DIMENSION A(4),AIJ(4)
	DIMENSION DATA(20,44)
	IOK=0
C  DO 4 PT NATURAL SPLINE INTERPOLATION FOR VALUES BETWEEN GRID PTS
	IA=Y
	JA=X
C  LIMIT CALCULATIONS WITHIN 1 INTERIOR POINT OF LEFT AND BOTTOM
C  LIMIT CALCULATIONS WITNIN 2 INTERIOR PTS OF RIGHT AND TOP
	IF (IA.LT.2.OR.JA.LT.2)   GO TO 998
	IF (IA.GT.18.OR.JA.GT.42)  GO TO 998
C      IF (IA.GT.38.OR.JA.GT.102)  GO TO 998
	DX=1.0
	X1=JA
	Y1=IA
	XP1=X1+1.0
	YP1=Y1+1.0
	DO 105 K=1,4
	JJ=JA+(K-2)
	DO 106 L=1,4
	II=IA+(L-2)
	AIJ(L)=DATA(II,JJ)
C     IF(XJ.LT.1.9)WRITE(9,450)XI,XJ,L,II,JJ,AIJ(L)
  106 CONTINUE
C  VERTICAL INTERPOLATION
	GPP=(AIJ(3)-(2.0*AIJ(2))+AIJ(1))/(DX*DX)
	GPPP1=(AIJ(4)-(2.0*AIJ(3))+AIJ(2))/(DX*DX)
	G1=((GPP/6.0)*((((YP1-Y)**(3.0))/DX)-(DX*(YP1-Y))))
	G2=((GPPP1/6.0)*((((Y-Y1)**(3.0))/DX)-(DX*(Y-Y1))))
	G3=(AIJ(2)*((YP1-Y)/DX))
	G4=(AIJ(3)*((Y-Y1)/DX))
	G=G1+G2+G3+G4
C     IF(G.LT.20.0)WRITE (66,9412) N,M,K,NK,GPP,GPPP1,G1,G2,G3,G4,G
	A(K)=G
  105 CONTINUE
C  HORIZONTAL INTERPOLATION
	GPP=(A(3)-(2.0*A(2))+A(1))/(DX*DX)
	GPPP1=(A(4)-(2.0*A(3))+A(2))/(DX*DX)
	G1=((GPP/6.0)*((((XP1-X)**(3.0))/DX)-(DX*(XP1-X))))
	G2=((GPPP1/6.0)*((((X-X1)**(3.0))/DX)-(DX*(X-X1))))
	G3=(A(2)*((XP1-X)/DX))
	G4=(A(3)*((X-X1)/DX))
	G=G1+G2+G3+G4
	VAL=G
  450 FORMAT(2F7.3,3I4,F7.3)
  451 FORMAT(2F7.3,I4,F7.3)
  452 FORMAT(3F7.3)
	GO TO 999
  998 IOK=1
	VAL=999999
  999 RETURN
	END
C__________________________________________
	SUBROUTINE SPLN2648(X,Y,IOK,VAL,DATA)
C__________________________________________
	DIMENSION A(4),AIJ(4)
	DIMENSION DATA(26,48)
	IOK=0
C  DO 4 PT NATURAL SPLINE INTERPOLATION FOR VALUES BETWEEN GRID PTS
	IA=Y
	JA=X
C  LIMIT CALCULATIONS WITHIN 1 INTERIOR POINT OF LEFT AND BOTTOM
C  LIMIT CALCULATIONS WITNIN 2 INTERIOR PTS OF RIGHT AND TOP
	IF (IA.LT.2.OR.JA.LT.2)   GO TO 998
	IF (IA.GT.24.OR.JA.GT.46)  GO TO 998
	DX=1.0
	X1=JA
	Y1=IA
	XP1=X1+1.0
	YP1=Y1+1.0
	DO 105 K=1,4
	JJ=JA+(K-2)
	DO 106 L=1,4
	II=IA+(L-2)
	AIJ(L)=DATA(II,JJ)
C     IF(XJ.LT.1.9)WRITE(9,450)XI,XJ,L,II,JJ,AIJ(L)
  106 CONTINUE
C  VERTICAL INTERPOLATION
	GPP=(AIJ(3)-(2.0*AIJ(2))+AIJ(1))/(DX*DX)
	GPPP1=(AIJ(4)-(2.0*AIJ(3))+AIJ(2))/(DX*DX)
	G1=((GPP/6.0)*((((YP1-Y)**(3.0))/DX)-(DX*(YP1-Y))))
	G2=((GPPP1/6.0)*((((Y-Y1)**(3.0))/DX)-(DX*(Y-Y1))))
	G3=(AIJ(2)*((YP1-Y)/DX))
	G4=(AIJ(3)*((Y-Y1)/DX))
	G=G1+G2+G3+G4
C     IF(G.LT.20.0)WRITE (66,9412) N,M,K,NK,GPP,GPPP1,G1,G2,G3,G4,G
	A(K)=G
  105 CONTINUE
C  HORIZONTAL INTERPOLATION
	GPP=(A(3)-(2.0*A(2))+A(1))/(DX*DX)
	GPPP1=(A(4)-(2.0*A(3))+A(2))/(DX*DX)
	G1=((GPP/6.0)*((((XP1-X)**(3.0))/DX)-(DX*(XP1-X))))
	G2=((GPPP1/6.0)*((((X-X1)**(3.0))/DX)-(DX*(X-X1))))
	G3=(A(2)*((XP1-X)/DX))
	G4=(A(3)*((X-X1)/DX))
	G=G1+G2+G3+G4
	VAL=G
  450 FORMAT(2F7.3,3I4,F7.3)
  451 FORMAT(2F7.3,I4,F7.3)
  452 FORMAT(3F7.3)
	GO TO 999
  998 IOK=1
	VAL=999999
  999 RETURN
	END
C__________________________________________
	SUBROUTINE OUTPT(OUT,NFILE,KYR,MO,KDY,H)
C__________________________________________
	DIMENSION OUT(92,180)
	CHARACTER*4,H(2)
	DO 60 J=1,18
	J2=J*10
	J1=J2-9
	DO 60 I=1,92
	WRITE (NFILE,100)I,J1,J2,(OUT(I,JK),JK=J1,J2),H(1),KYR,MO,KDY
   60 CONTINUE
  100 FORMAT (I4,2I3,10F8.3,A2,3I3)
	RETURN
	END
C__________________________________________
	SUBROUTINE CANUVS(OUT,NFILE,KYR,MO,KDY,JJH)
C__________________________________________
C  MAKE DISK FILE OF PART OF THE 66X180 GRID TOTAL U AND V'S
	DIMENSION OUT(92,180),H(2)
	DO 60 J=55,180
	DO 60 I=1,5
	I2=I*8
	I1=I2-7
	WRITE (NFILE,100)  (OUT(IK,J),IK=I1,I2),KYR,MO,KDY,JJH
   60 CONTINUE
  100 FORMAT (8F8.2,3I3,I2)
	RETURN
	END
C__________________________________________
	SUBROUTINE KATUVS(OUT,NFILE,KYR,MO,KDY,JJH)
C__________________________________________
C  MAKE DISK FILE OF PART OF THE 66X160 GRID TOTAL U AND V'S
	DIMENSION OUT(92,180),H(2)
	MFILE=32
C      DO 60 J=67,130
C      DO 60 I=1,5
C      I2=I*8
C  START AT I=3 THROUTH I=40
C      I2=I2+2
C      I1=I2-7
C      IF(I2.EQ.42)I2=40
C      WRITE (NFILE,100)  (OUT(IK,J),IK=I1,I2),KYR,MO,KDY,I1,I2,J,JJH
C      IF(J.EQ.80.AND.I2.EQ.24)WRITE(MFILE,100)(OUT(IK,J),IK=I1,I2),KYR,
C     *O,KDY,I1,I2,J,JJH
C   60 CONTINUE
C  100 FORMAT (8F7.2,6I3,I2)
C
C  WRITE HEADER CARD FOR U+V FILE
	IF(JJH.EQ.1)WRITE (NFILE,200) KYR,MO,KDY,JJH
  200 FORMAT (' DAILY OSCURS U-VELOCIEIES 38BY64 FIELD',3I3,I2)
  202 FORMAT (' DAILY OSCURS V-VELOCITIES 38BY64 FIELD',3I3,I2)
	DO 70 I=3,40
	DO 70 J=1,4
C  START AT J=67 THROUTH J=130 (J=HORIZONTAL IN MAP VIEW)
	J2=66+(J*16)
	J1=J2-15
	WRITE (NFILE,201)  (OUT(I,JK),JK=J1,J2)
C      IF(J.EQ.80.AND.I2.EQ.24)WRITE(MFILE,100)(OUT(IK,J),IK=I1,I2),KYR,
C     *O,KDY,I1,I2,J,JJH
   70 CONTINUE
  201 FORMAT (' ',16F5.1)
	RETURN
	END
C__________________________________________
	SUBROUTINE WIND(SLP5,LS4,BLAT,WX,WY,idx12s,idx22s,jdx12s,jdx22s)

C__________________________________________
	DIMENSION SLP5(92,180),BLAT(92,180),WX(92,180),WY(92,180)
	DIMENSION LS4(92,180)
 100  FORMAT(1X,18F8.3)
	ALPHA=0.75
	BETA=1.0
      OMEGAV=0.80
C  COMPUTE SURFACE GEOSTROPHIC WIND IN  M/SEC
C  WIND(M./SEC) = (DP/DL)/(RHO)(CORIOLIS)
C  1 BAR = 10(6TH) DYNES/CM**2
C  WEIGHT IN KG = MG = (KG)(9.8M/SEC**2)
C 1 MBAR = 10.1971 KG/M**2 = 100.0 KG M-1 SEC-2
C  CW = (100)/(90000)(1.22)(10-4TH) = 9.11
C  1XGRIDLENGTH IS APPROX. 90000 M
C  TYPICAL WIND SPEED COMPUTATION IS FOR 1MB GRADIENT PER GRID LENGEH
C  W(M/S)=(1 MB)(100 KG M-1 SEC-2 MB-1)/(90000 M)(1.22 KG M-3)(0.00014
C                                    58 S-1)(SIN LAT)
C     (1)(100)/(90000)(1.22)(0.0001458)(.707)  =  8.84 M/S
C          INCLUDES REDUCTION AND ROTATION OF WIND
      PI=3.1415962
	THETA=-15.0*PI/180.0
	RR=0.7
	A1=RR*COS(THETA)
	B2=A1
	B1=RR*SIN(THETA)
	A2=-B1
c      DO 20 I=2,91
c      DO 20 J=2,179
	DO 20 I=idx12s,idx22s
	DO 20 J=jdx12s,jdx22s
	IF (J.le.2.OR.J.ge.179) GO TO 21
	IF (I.le.2.OR.I.ge.91) GO TO 21
C  COMPUTE PURE GEOSTROPHIC WIND (WITH NO FRICTION)
      GLAT=BLAT(I,J)
      GG=GRIDLN(GLAT)
      XR=SIN(BLAT(I,J)/57.296)
C  NEAR EQUATOR LIMIT = 15 N (SIN(15)=.26)
	IF(XR.LT.0.25)XR=0.25
	CRLS=0.0001458*XR
C  KM TO METERS
      GG=GG*1000.0
      CW=100.0/(GG*1.22*CRLS)
C     IF(LS4(I,J).EQ.0.OR.LS4(I+1,J).EQ.0.OR.LS4(I,J+1).EQ.0)  GO TO 21
C     IF (LS4(I,J).EQ.0) GO TO 21
C  USE 5-POINT GRADIENT IN INTERIOR OF GRID
C  USE 3-POINT GRADIENT ONE GRID POINT FROM BOUNDARY
      GRADX=SLP5(I,J+1)-SLP5(I,J-1)
      GRADX=GRADX/2.0
	IF (J.le.3.OR.J.ge.178) GO TO 41
	IF (I.le.3.OR.I.ge.90) GO TO 41
      GRADX=SLP5(I,J-2)-(8.0*SLP5(I,J-1))+(8.0*SLP5(I,J+1))-SLP5(I,J+2)
      GRADX=GRADX/12.0
   41 CONTINUE
c  4207 zeroth element accessed 20001227
c	IF (J.lt.1.OR.J.gt.180) GO TO 41
c	IF (I.lt.2.OR.I.gt.92) GO TO 42
      GRADY=SLP5(I-1,J)-SLP5(I+1,J)
      GRADY=GRADY/2.0
	IF (J.le.3.OR.J.ge.178) GO TO 42
	IF (I.le.3.OR.I.ge.90) GO TO 42
      GRADY=SLP5(I-2,J)-(8.0*SLP5(I-1,J))+(8.0*SLP5(I+1,J))-SLP5(I+2,J)
      GRADY=-GRADY/12.0
   42 CONTINUE
      if(i.le.1.or.i.ge.92)grady=0.0
      if(j.le.1.or.j.ge.180)gradx=0.0
      WU=-GRADY*CW
      WV=GRADX*CW
      GLATR=GLAT/57.2958
      SINLAT=SIN(GLATR)
C  COMPUTE MANGITUDE REDUCTION (H U ROLL METHOD)
      IF(GLAT.GT.45.0) RED=0.75+(0.1*((90.0-GLAT)/45.0))
      IF(GLAT.LE.45.0.AND.GLAT.GE.25.0) RED=0.85
      IF(GLAT.LT.25.0) RED=0.65+(0.2*(GLAT/25.0))
C  COMPUTE DEFLECTION ANGLE AS A FUNCTION OF WIND SPEED
C  USE REDUCED COMPONENTS IN FIGURING THE ANGLE OF DEFLECTION
      XCOMP=RED*WU
      YCOMP=RED*WV
      SPEED=SQRT((XCOMP*XCOMP)+(YCOMP*YCOMP))
      ANG=(22.5-(0.0175*SPEED*SPEED))*(1.495/(SINLAT+1.0))
      THETA=-ANG*PI/180.0
      A1=RED*COS(THETA)
      B2=A1
      B1=RED*SIN(THETA)
      A2=-B1
      WX(I,J)=WU*A1 + WV*B1
      WY(I,J)=WV*B2 + WU*A2
      GO TO 20
 21    WX(I,J)=0.0
       WY(I,J)=0.0
 20   CONTINUE
	DO 22 M=1,92
	WX(M,179)=WX(M,178)
	WY(M,179)=WY(M,178)
	WX(M,180)=WX(M,178)
	WY(M,180)=WY(M,178)
      WX(M,2)=WX(M,3)
      WY(M,2)=WY(M,3)
      WX(M,1)=WX(M,3)
      WY(M,1)=WY(M,3)
 22   CONTINUE
	DO 24 N=1,180
	WX(91,N)=WX(90,N)
	WY(91,N)=WY(90,N)
	WX(92,N)=WX(90,N)
	WY(92,N)=WY(90,N)
      WX(2,N)=WX(3,N)
      WY(2,N)=WY(3,N)
      WX(1,N)=WX(3,N)
      WY(1,N)=WY(3,N)
 24   CONTINUE
  395 CONTINUE
      RETURN
      END
C__________________________________________
	FUNCTION WINDC(W1,W2,M,YLAT,SP1,ANGSAM,ANGDEFL)
	DIMENSION S(5),B(5)
C  ANGLD1 -- WEBBER 1983
	DATA S/-0.6,0.6,0.3,0.3,0.0/
C  FOUND 19.0 IN POSITION 4 (4/96)   DATA B/25.0,19.0,22.0,19.0,31.0/
	DATA B/25.0,19.0,22.0,22.0,31.0/
C  ANGLD1 -- WEBBER 1983 WITH MAX=25.0 DEGREES FOR >10 M/SEC WIND
C      DATA S/-0.6,0.6,0.0,0.0,0.0/
C      DATA B/25.0,19.0,25.0,25.0,25.0/
C  ANGLD1 -- AVERAGE SAMUELS AND WEBBER TOGETHER
C      DATA S/0.0,-0.9,-0.4,0.1,0.0/
C      DATA B/22.5,27.0,22.0,13.0,15.5/
C__________________________________________
C
C     CURRENT (CM/SEC) = 4.8X(SQUARE ROOT WIND SPEED M/SEC)
C
C  FACTOR IS BETWEEN 4 AND 5 IF MASS TRANSPORT BY WAVES IS INCLUDED
	XK=4.8
	SPDSQD=W1**2 + W2**2
	IF(SPDSQD-5.0E-04)10,10,20
 10   WINDC=0.0
	RETURN
 20   SPEED=SQRT(SPDSQD)
	SQROOT=SQRT(SPEED)
C     SP40=SP1*0.5148
	SP40=SP1
C  TEST LARGER ANGLE OF DEFLECTION TO RIGHT
C     IF(YLAT.LT.60.0.AND.YLAT.GT.59.9) WRITE(9,199) SP40,YLAT,M
  199 FORMAT(' SP40=',2F8.4,I5)
	CL=(34.0-((67.0-YLAT)*0.2933))
C  CANCEL FUNCTION OF LATITUDE
	CL=34.0
	CLV=CL/SQRT(SP40)
	ANGLD1=CL-(CLV*SQRT(SPEED))
	ADWHIT=ANGLD1
C
C  ANGLD1
C  TRY SAMUELS, 1982 ANGLE OF DEFLECTION
C  10**-8
C  CANCEL
c	IF(1.EQ.1) GO TO 200
c	A1=-0.00000001
C  VISCOSITY=0.8TO1.8X(10**-6) (M**2/SEC**2)
c	VISC=0.0000013
C  9.81M**2SEC**-2
c	GRAV=9.81
c	ARG=(A1*SPEED*SPEED*SPEED)/(VISC*GRAV)
C     ANGLD1=25.0*EXP(ARG)
c	ANGLD1=ANGSAM*EXP(ARG)
  200 CONTINUE
C
C  ADD CONSTANT INCREASE IN ANGLE OF DEFLECTION
C      ANGLD1=ANGLD1+0.0
C
C  MAKE ANGLE OF DEFLECTION CONSTANT
C      ANGLD1=25.0
C
C  ANGLD1=WEBBER
C  5 UNITS OF Y=MX+B
	IW=1
	IF(SPEED.LT.5.0) IW=1
	IF(SPEED.GE.5.0.AND.SPEED.LT.10.0) IW=2
	IF(SPEED.GE.10.0.AND.SPEED.LT.20.0) IW=3
	IF(SPEED.GE.20.0.AND.SPEED.LT.30.0) IW=4
	IF(SPEED.GE.30) IW=5
   99 FORMAT (' WEBBER TEST --',3F10.5)
	ANGLD1=(S(IW)*SPEED)+B(IW)
C
C  ANGLD1 COMMENTED OUT HERE TO ACCEPT FILE INPUT (ANGDEFL)
C  MODIFY HERE FOR OTHER ADDITIONS OR SUBTRACTIONS
C  ANGFAC
C
C  ACCEPT ANGLE CORRECTION FROM INPUT FILE (ANGDEFL=AMT; WEB-AMT)
c	IF(ANGDEFL.LE.0.0)ANGLD1=ANGLD1+ANGDEFL
c	IF(ANGDEFL.GT.0.0)ANGLD1=ANGDEFL
        ANGLD1=ANGLD1+ANGDEFL
c
C  NOW COMPUTE CURRENT SPEED FOR THAT ANGLE OF DEFLECTION
	B1=COS(ANGLD1/57.296)
	B2=SIN(ANGLD1/57.296)
	W10=XK*(W1/SQROOT)
	W20=XK*(W2/SQROOT)
	IF(M.EQ.2) W20=-W20
	WINDC=W10*B1 + W20*B2
   88 FORMAT (1H ,9F12.5)
	ANGDEFL=ANGLD1
	RETURN
	END
C__________________________________________
	SUBROUTINE OCEANC(TML,TCX,TCY,HH)
C__________________________________________
      COMMON/LAT/ALAT2,LS2
	DIMENSION TDEP(92,180),TML(92,180),FL(92,180),XF(92,180),
     1TCX(92,180),TCY(92,180),XTR2(92,180),XTR(92,180),TS(92,180),
     2SLP(92,180),WCX(92,180),WCY(92,180)
	DIMENSION ALAT2(92,180),LS2(92,180)
	CHARACTER*4,HH
CS	GAMMA=1.025
	T=0.0
      DO 70 I=1,91
      DO 70 J=1,179
C  GRIDSIZE VARIES WITH LATITUDE 83-96 KM (SEE GRIDLN FUNCTION)
      XLEN=GRIDLN(ALAT2(I,J))
C  CHANGE KM TO M
      XLEN=XLEN*1000.0
      XOR=SIN(ALAT2(I,J)/57.296)
C  NEAR EQUATOR LIMIT = 15 N (SIN(15)=.26)
	IF(XOR.LT.0.25)XOR=0.25
	COR=0.0001458*XOR
	NP1=LS2(I,J)
	NP2=LS2(I,J+1)
	NP3=LS2(I+1,J)
C  ZERO VALUE IF ANY OF THE LAND-SEA GRID POINTS ARE ZERO
C      IF (NP1.LT.2) GO TO 65
      DDI=TML(I,J+1)-TML(I,J)
      DDJ=TML(I+1,J)-TML(I,J)
 19   IF(HH.EQ.'DYCC') GO TO 20
      GO TO 10
c 65   TCX(I,J)=0.0
c	TCY(I,J)=0.0
c	GO TO 70
   10 CONTINUE
  550 FORMAT (1H ,2I5,7F15.7)
      TCX(I,J)=THERM(DDI,DDJ,XLEN,COR,TML(I,J),T)
      TCY(I,J)=T
C     WRITE (6,550) I,J,TML(I,J),DDI,DDJ,COR,XLEN,TCX(I,J),TCY(I,J)
      GO TO 70
 20   TCX(I,J)=DYCUR(DDI,DDJ,XLEN,COR,T)
      TCY(I,J)=T
C  PUT LIMITS ON VALUES SO NO *********** APPEAR IN FILE
	IF(TCX(I,J).GT.999.0)TCX(I,J)=999.0
	IF(TCY(I,J).GT.999.0)TCY(I,J)=999.0
	IF(TCX(I,J).LT.-999.0)TCX(I,J)=-999.0
	IF(TCY(I,J).LT.-999.0)TCY(I,J)=-999.0
 70   CONTINUE
	DO 75 M=1,92
	TCX(M,180)=TCX(M,179)+(TCX(M,178)-TCX(M,179))
	TCY(M,180)=TCY(M,179)+(TCY(M,178)-TCY(M,179))
C  PUT LIMITS ON VALUES SO NO *********** APPEAR IN FILE
	IF(TCX(M,180).GT.999.0)TCX(M,180)=999.0
	IF(TCY(M,180).GT.999.0)TCY(M,180)=999.0
	IF(TCX(M,180).LT.-999.0)TCX(M,180)=-999.0
	IF(TCY(M,180).LT.-999.0)TCY(M,180)=-999.0
 75   CONTINUE
	DO 77 M=1,180
	TCX(92,M)=TCX(91,M)+(TCX(90,m)-TCX(91,m))
	TCY(92,M)=TCY(91,M)+(TCY(90,m)-TCY(91,m))
C  PUT LIMITS ON VALUES SO NO *********** APPEAR IN FILE
	IF(TCX(92,M).GT.999.0)TCX(92,M)=999.0
	IF(TCY(92,M).GT.999.0)TCY(92,M)=999.0
	IF(TCX(92,M).LT.-999.0)TCX(92,M)=-999.0
	IF(TCY(92,M).LT.-999.0)TCY(92,M)=-999.0
 77   CONTINUE
      RETURN
      END
C__________________________________________
      FUNCTION THERM(DDI,DDJ,XLN,COR,TML,T)
C__________________________________________
C
C  THWIND = DELTAT(G)(DZ)/(F)(TBAR)
C
C  TYPICAL THERMAL WIND CALCULATION
C  C1 = G(DZ)=(9.8M/SEC**2)(200M)=1961.2 M**2/SEC**2
C  C2 = (0.0001458)(10+273)(SIN 45)(82000) = 2392.45 M SEC-1 DEGK
C  ONE DEGREE GRADIENT GIVES TYPICAL VALUE OF CURRENT = 0.8197 M/SEC
C      TIMES 0.0544 = 4.46 CM/SEC
      ALPHA=100.0
      C1=1961.2
      C2=TML*COR*XLN
      T=DDI*C1/C2
      THERM=DDJ*C1/C2
	T=T*ALPHA
	THERM=THERM*ALPHA
C  BEST FIT CORRELATION
C  CONVERTS TO DYNAMIC HEIGHT CALCULATION WITH SALINITY CONSTANT
      T=T*0.0544
      THERM=THERM*0.0544
      RETURN
      END
C__________________________________________
      FUNCTION DYCUR(DDI,DDJ,XLN,COR,T)
C__________________________________________
C TYPICAL CALCULATION
C     10(0.1M**2SEC**-2) DIVIDED BY (0.0001458SEC**-1)(SIN45)(164000M)
C     = 0.0873 M/SEC
C  ALPHA CONVERTS TO 8.73 CM/SEC
      ALPHA=100.0
      C2=XLN*COR
      T=+ALPHA*10.0*DDI/C2
      DYCUR=+ALPHA*10.0*DDJ/C2
      RETURN
      END
C__________________________________________
	FUNCTION GRIDLN(GLAT)
C__________________________________________
C  GRID LENGTH AS A FUNCTION OF GEOGRAPHIC LATITUDE FOR 92X180 GRID
	AKM=(1.0+(SIN(GLAT*3.14159/180.0)))/1.86603
	GRIDLN=AKM*95.23
	RETURN
	END
C__________________________________________
      FUNCTION GDFNOC(GLAT)
C__________________________________________
C  GRID LENGTH AS A FUNCTION OF GEOGRAPHIC LATITUDE FOR FNOC 20X44 GRID
      AKM=(1.0+(SIN(GLAT*3.14159/180.0)))/1.86603
      GDFNOC=AKM*380.8
      RETURN
      END
C__________________________________________
	SUBROUTINE DELTAD(TML)
C__________________________________________ 
        COMMON/LAT/ALAT2,LS2
	COMMON/LON/ALON2
	DIMENSION TDEP(92,180),TML(92,180),FL(92,180),XF(92,180),
     1TCX(92,180),TCY(92,180),XTR2(92,180),XTR(92,180),TS(92,180),
     2SLP(92,180),WCX(92,180),WCY(92,180),IGD(92,180)
	DIMENSION ALAT2(92,180),ALON2(92,180),LS2(92,180)
	CHARACTER*4,H,HUDD(2),HVDD(2)
      character*4 adum
      	DATA HUDD(1)/'UDD '/,HUDD(2)/'  '/
	DATA HVDD(1)/'VDD '/,HVDD(2)/'  '/
C*****INPUT THE DYNAMIC TOPOGRAPHY*****
C  LTM
	KYR=99
	MO=99
	OPEN(UNIT=43,FILE='dd92180.dat',STATUS='old')
C  READ AND PRINT NEW 92X180 DD (DYNAMID DEPTH) FILE 43
C  GEOSTROPHIC CURRENT DD FIELD
c	WRITE(6,78481)
c	WRITE(9,78481)
78481 FORMAT(' START READ FILE43 NEW 92X180 DELTA DD.S  ',I5)
	DO 12462 J=1,18
	J2=J*10
	J1=J2-9
C      WRITE (6,68479)IG,IK,J1,J2,(TML(I,JK),JK=J1,J2)
	DO 12463 I=1,92
	READ(43,68479,end=99460)IG,IK,JR1,JR2,(TML(I,JK),JK=J1,J2)
c      WRITE (6,68479)IG,IK,J1,J2,(TML(I,JK),JK=J1,J2)
      if(I.eq.1)WRITE (6,68479)I,J1,J2
      if(I.le.5)WRITE (6,68479)IG,IK,Jr1,Jr2,(TML(I,JK),JK=J1,J2)
      if(I.gt.90)WRITE (6,68479)IG,IK,Jr1,Jr2,(TML(I,JK),JK=J1,J2)
      if(I.eq.1)WRITE (9,68479)I,J1,J2
      if(I.le.5)WRITE (9,68479)IG,IK,Jr1,Jr2,(TML(I,JK),JK=J1,J2)
      if(I.gt.90)WRITE (9,68479)IG,IK,Jr1,Jr2,(TML(I,JK),JK=J1,J2)

12463 CONTINUE
12462 CONTINUE
99460 continue
68479 FORMAT (I6,I3,2I3,10F6.3)
c	WRITE(6,68481)
c	WRITE(9,68481)
68481 FORMAT(' END OF READ NEW 92X180 DELTA DD.S  ',I5)

	CALL OCEANC(TML,TCX,TCY,'DYCC')
C  PRINT GEOSTROPHIC CURRENTS
c	WRITE(9,300)
  300 FORMAT(' 1 U--COMPONENT OF GEOSTROPHIC SURFACE CURRENT (CM/SEC)')
c	WRITE(9,303)
  303 FORMAT (10X,'LONG--TERM MEAN'/)
	CALL PRINT2(TCX,1.0)
c	WRITE(9,301)
  301 FORMAT(' 1V  COMPONENT OF GEOSTROPHIC SURFACE CURRENT (CM/SEC)'/)
	CALL PRINT2(TCY,1.0)
      OPEN(38,file='dd92180s.dat',sTATUS='UNknown',ACCESS='SEQUENTIAL') 
	CALL OUTPT(TCX,38,KYR,MO,KDY,HUDD)
	CALL OUTPT(TCY,38,KYR,MO,KDY,HVDD)
	CLOSE(UNIT=38)
99999 CONTINUE
	RETURN
	END
C__________________________________________
	SUBROUTINE PRINT0(P,F)
C__________________________________________
	DIMENSION P(20,44),IP(20,44),I1(20)
C      WRITE(9,302)
	DO 1 I=1,20
	I1(I)=I
    1 CONTINUE
c	WRITE(9,302)F
  302 FORMAT(/' PRINT0 SUBROUTINE 20X44',F5.1,'=F')
	DO 200 I=1,20
	DO 201 JB=1,44
C      J=181-JB
	IP(I,JB)=(P(I,JB)-1000.0)*F
  201 CONTINUE
  200 CONTINUE
c	WRITE(9,301)J2,(I1((21-K)),K=1,10)
	DO 305 J=1,40
	J2=J
c	WRITE(9,301)J2,(IP((21-K),J2),K=1,10),J2
  305 CONTINUE
c	WRITE(9,301)J2,(I1((21-K)),K=1,10)
c	WRITE(9,301)J2,(I1((21-K)),K=11,20)
	DO 306 J=1,40
	J2=J
c	WRITE(9,301)J2,(IP((21-K),J2),K=11,20),J2
  306 CONTINUE
c	WRITE(9,301)J2,(I1((21-K)),K=11,20)
  301 FORMAT (' J=',I3,1X,10I6,I5)
	RETURN
	END
C__________________________________________
	SUBROUTINE PRINT1(P,F)
C__________________________________________
	DIMENSION P(26,48),IP(26,48),I1(48)
C      WRITE(9,302)
	DO 1 I=1,26
	I1(I)=I
    1 CONTINUE
c	WRITE(9,302)F
  302 FORMAT(/' PRINT1 SUBROUTINE 26X48',F5.1,'=F')
	DO 200 I=1,26
	DO 201 JB=1,48
C      J=181-JB
	IP(I,JB)=(P(I,JB)-1000.0)*F
  201 CONTINUE
  200 CONTINUE
c	WRITE(9,301)J2,(I1((27-K)),K=1,13)
	DO 305 J=1,48
	J2=J
c	WRITE(9,301)J2,(IP((27-K),J2),K=1,13),J2
  305 CONTINUE
c	WRITE(9,301)J2,(I1((27-K)),K=1,13)
c	WRITE(9,301)J2,(I1((27-K)),K=14,26)
	DO 306 J=1,48
	J2=J
c	WRITE(9,301)J2,(IP((27-K),J2),K=14,26),J2
  306 CONTINUE
c	WRITE(9,301)J2,(I1((27-K)),K=14,26)
  301 FORMAT (' J=',I3,1X,13I5,I5)
	RETURN
	END
C__________________________________________
	SUBROUTINE PRINT2(P,F)
C__________________________________________
	DIMENSION P(92,180),IP(92,180),I1(92)
C      WRITE(9,302)
	DO 1 I=1,92
	I1(I)=I
    1 CONTINUE
      j0=0
c	WRITE(9,302)F
  302 FORMAT(/' PRINT2 SUBROUTINE 92X180',F5.1,'=F')
	DO 200 I=1,92
	DO 201 JB=1,180
C      J=181-JB
	IP(I,JB)=P(I,JB)*F
  201 CONTINUE
  200 CONTINUE
c	WRITE(9,301)J0,(I1((93-K)),K=1,23)
	DO 305 J=1,180
	J2=J
	J3=j2
c	WRITE(9,301)J3,(IP((93-K),J3),K=1,23),J3
  305 CONTINUE
c	WRITE(9,301)J3,(I1((93-K)),K=1,23)
c	WRITE(9,301)J3,(I1((93-K)),K=23,46)
	DO 306 J=1,180
	J2=J
	J3=J2
c	WRITE(9,301)J3,(IP((93-K),J3),K=24,46),J3
  306 CONTINUE
c	WRITE(9,301)J3,(I1((93-K)),K=24,46)
c	WRITE(9,301)J3,(I1((93-K)),K=47,69)
	DO 307 J=1,180
	J2=J
	J3=J2
c	WRITE(9,301)J3,(IP((93-K),J3),K=47,69),J3
  307 CONTINUE
c	WRITE(9,301)J3,(I1((93-K)),K=47,69)
c	WRITE(9,301)J3,(I1((93-K)),K=70,92)
	DO 308 J=1,180
	J2=J
	J3=J2
c	WRITE(9,301)J3,(IP((93-K),J3),K=70,92),J3
  308 CONTINUE
c	WRITE(9,301)J0,(I1((93-K)),K=70,92)
  301 FORMAT (' J=',I3,1X,23I3,I4)
	RETURN
	END
C__________________________________________
C__________________________________________
	SUBROUTINE PRINT2X2(P,F)
C__________________________________________
	DIMENSION P(92,180),IP(92,180),I1(92)
C      WRITE(9,302)
	DO 1 I=1,92
	I1(I)=I
    1 CONTINUE
      j0=0
c	WRITE(9,301)J3,(IP((93-K),J3),K=1,47,2)
  305 CONTINUE
c	WRITE(9,301)J3,(I1((93-K)),K=1,47,2)
c	WRITE(9,301)J3,(I1((93-K)),K=49,92,2)
	DO 307 J=1,180,2
	J2=J
	J3=J2
c	WRITE(9,301)J3,(IP((93-K),J3),K=49,92,2)
  307 CONTINUE
c	WRITE(9,301)J0,(I1((93-K)),K=49,92,2)
  301 FORMAT (' J=',I3,1X,23I3,I4)
	RETURN
	END
C__________________________________________
	SUBROUTINE ANGLD(X,Y,AAA)
C__________________________________________
C  GIVEN THE X- AND Y- COMPONENT OF LENGTH OR VELOCITY
C  FIND ANGLE(RADIANS)----THAT IS + COUNTERCLOCKWISE FROM THE X-AXIS
	IQ1=0
	IQ2=0
	IQ3=0
	IQ4=0
	IF (X.GE.0.0.AND.Y.GE.0.0) IQ1=1
	IF (X.LT.0.0.AND.Y.GE.0.0) IQ2=1
	IF (X.LT.0.0.AND.Y.LT.0.0) IQ3=1
	IF (X.GE.0.0.AND.Y.LT.0.0) IQ4=1
	PI=3.141596
	RAD=PI/180.0
	IF (X.GE.0.0.AND.X.LT.0.000001) X=0.000001
	IF (X.LT.0.0.AND.X.GT.-0.000001) X=-0.000001
	AA=Y/X
	IF (IQ1.EQ.1) ANG=ATAN(AA)
	IF (IQ1.EQ.1) GO TO 50
	IF (IQ2.EQ.1) ANG=PI + ATAN(AA)
	IF (IQ2.EQ.1) GO TO 50
	IF (IQ3.EQ.1) ANG=PI + ATAN(AA)
	IF (IQ3.EQ.1) GO TO 50
	IF (IQ4.EQ.1) ANG=(2.0*PI) + ATAN(AA)
   50 CONTINUE
	AAA=ANG/RAD
  101 FORMAT(4I4,5F11.5)
	RETURN
	END
C__________________________________________
	SUBROUTINE ANGLDT(X,Y,AAA,AAAT)
C__________________________________________
C  GIVEN THE X- AND Y- COMPONENT OF LENGTH OR VELOCITY
C  FIND ANGLE(RADIANS)----THAT IS + COUNTERCLOCKWISE FROM THE X-AXIS
C  CONVERT TO DEGREES ...TRUE..
	IQ1=0
	IQ2=0
	IQ3=0
	IQ4=0
	IF (X.GE.0.0.AND.Y.GE.0.0) IQ1=1
	IF (X.LT.0.0.AND.Y.GE.0.0) IQ2=1
	IF (X.LT.0.0.AND.Y.LT.0.0) IQ3=1
	IF (X.GE.0.0.AND.Y.LT.0.0) IQ4=1
	PI=3.141596
	RAD=PI/180.0
	IF (X.GE.0.0.AND.X.LT.0.000001) X=0.000001
	IF (X.LT.0.0.AND.X.GT.-0.000001) X=-0.000001
	AA=Y/X
	IF (IQ1.EQ.1) ANG=ATAN(AA)
	IF (IQ1.EQ.1) GO TO 50
	IF (IQ2.EQ.1) ANG=PI + ATAN(AA)
	IF (IQ2.EQ.1) GO TO 50
	IF (IQ3.EQ.1) ANG=PI + ATAN(AA)
	IF (IQ3.EQ.1) GO TO 50
	IF (IQ4.EQ.1) ANG=(2.0*PI) + ATAN(AA)
   50 CONTINUE
	AAA=ANG/RAD
C  NOW FIND DEGREES TRUE (COURSE HEADING REL TO NORTH)
	IF (IQ1.EQ.1) ANG=(PI/2.0)-ATAN(AA)
	IF (IQ1.EQ.1) GO TO 60
	IF (IQ2.EQ.1) ANG=(3.0*PI/2.0)-ATAN(AA)
	IF (IQ2.EQ.1) GO TO 60
	IF (IQ3.EQ.1) ANG=PI + ((PI/2.0)-ATAN(AA))
	IF (IQ3.EQ.1) GO TO 60
	IF (IQ4.EQ.1) ANG=(PI/2.0) - ATAN(AA)
   60 CONTINUE
	AAAT=ANG/RAD
  101 FORMAT(4I2,4F8.2,' AAAX=',F9.2,' AAAT=',F9.2)
	RETURN
	END
C__________________________________________
	SUBROUTINE IJTOLL(Y,X,Y2,X2)
C__________________________________________
	COMMON/LAT/ALAT2,LS2
	COMMON/LON/ALON2
	DIMENSION ALAT2(92,180),ALON2(92,180),LS2(92,180)
c  CONVERT (I,J) TO LAT-LONG
C  OR FIND LAT AND LONG OF RANDOM GRID POINTS (Y,X)
C     WITHIN SLOPING GRID SQUARE   A----B
C                                  C----C
	IX=X
	DX=X-IX
	IY=Y
	DY=Y-IY
C  CORNER A = (IY,IX) INTEGERS
C  GO DOWN AC TO (YB,XB)
	YB=ALAT2(IY,IX)+(DY*(ALAT2(IY+1,IX)-ALAT2(IY,IX)))
	XB=ALON2(IY,IX)+(DY*(ALON2(IY+1,IX)-ALON2(IY,IX)))
C  COMPUTE WEIGHTED AVERAGE SLOPES FOR AB AND CD PER UNIT GRID DISTANCE
	SLA=((1.0-DY)*(ALAT2(IY,IX+1)-ALAT2(IY,IX))) +
     '    ((    DY)*(ALAT2(IY+1,IX+1)-ALAT2(IY+1,IX)))
	SLO=((1.0-DY)*(ALON2(IY,IX+1)-ALON2(IY,IX))) +
     '    ((    DY)*(ALON2(IY+1,IX+1)-ALON2(IY+1,IX)))
C  FIND LAT-LONG OF X,Y
	Y2=YB+(DX*SLA)
	X2=XB+(DX*SLO)
C      WRITE(9,5011)ALAT2(IY,IX),ALAT2(IY,IX+1),ALAT2(IY+1,IX),ALAT2(IY+
C     ',IX+1),YB,SLA,Y,DY,Y2
 5011 FORMAT(' IJTOLL LA ',9F8.3)
C      WRITE(9,5012)ALON2(IY,IX),ALON2(IY,IX+1),ALON2(IY+1,IX),ALON2(IY+
C     ',IX+1),XB,SLO,X,DX,X2
 5012 FORMAT(' IJTOLL LO ',9F8.3)
	RETURN
	END
C*****************************************************
C*                                                   *
C*          SUBROUTINE IJ2INCHS(XX,YY,XXIN,YYIN,IERR)*
C*  CONVERTS XX,YY FROM GRID (I,J) UNITS             *
C*  TO PLOT INCHES                                   *
C*  IF (XX,YY) OUT OF GRID AREA, IERR=1              *
C*                                                   *
C*****************************************************
	SUBROUTINE IJ2INCHS(XX,YY,XXIN,YYIN,IERR)
	COMMON/GRDCOM/CFACT,M,N,NM,XI(181),YJ(93)
	COMMON/SIZES/XLEN,YLEN,YYLEN,SIZ1H,SIZ1V,SIZ2H,SIZ2V,
     &SIZ3H,SIZ3V,SIZ4H,SIZ4V
	COMMON/LATLON/ALAT(92,180),ALON(92,180)
C
C  XLEN IS THE DESIRED LENGTH OF THE X-AXIS IN INCHES
C  YLEN IS THE DESIRED LENGTH OF THE Y-AXIS IN INCHES
C
C
	N1=N-1
	M1=M-1
	AN1=N1
	AM1=M1
	DXX=XLEN/AM1
	DYY=YYLEN/AN1
C  DYY IS THE NUMBER OF INCHES PER GRID LENGTH
C  DXX IS THE NUMBER OF INCHES PER GRID LENGTH
C
C  YYY IS IN GRID UNITS
C  XX IS IN GRID UNITS
	IF (XX.LT.1.0) XX=1.0
	IF (YYY.LT.1.0) YYY=1.0
	IF (XX.GT.M) XX=M
	IF (YYY.GT.N) YYY=N
C
C  CONVERT FROM I,J GRID VALUES TO I,J PLOT INCHES
	XXIN=DXX*(XX-1.0)
	YYIN=YYLEN-(DYY*(YY-1.0))
C
C      YY=YYLEN-(YYLEN*((YYY-1.0)/AN1))
C      XX=XLEN*((XXX-1.0)/AM1)
C
C  TEST WRITE
C      WRITE(9,1999)XXIN,DXX,XX,XLEN,YYIN,DYY,YY,YLEN,N,M
 1999 FORMAT(' IJ2INCHS XX=',8F7.3,2I4)
	RETURN
	END
C  ---------------------------------------------------------------------
	SUBROUTINE RADCOR1(X,Y,ADD)
C
C  ---------------------------------------------------------------------
C  COMPUTES THE CORRECTION IN RADIANS AS A FUNCTION OF QUADRANT
C  "ADD" IS THE NUMBER WHICH MUST BE ADDEDED TO ARCTANGENT
C        COMPUTED ANGLES TO GIVE THE TOTAL ANGLE COUNTERCLOCKWISE
C        FROM THE +X AXIS
C
C
      IF (X.GE.0.0.AND.Y.GE.0.0) IQ=1
      IF (X.LT.0.0.AND.Y.GE.0.0) IQ=2
      IF (X.LE.0.0.AND.Y.LT.0.0) IQ=3
      IF (X.GT.0.0.AND.Y.LT.0.0) IQ=4
      PI=3.141592
      IF (IQ.EQ.1) ADD=0.0
      IF (IQ.EQ.2.OR.IQ.EQ.3) ADD=PI
      IF (IQ.EQ.4) ADD=2.0*PI
      RETURN
      END
C  --------------------------------------------------------------------
C
C      SUBROUTINE SOCKY (SOCKEYE SWIMS 090 T, 1KT)
C
C  -------------------------------------------------------------------
	SUBROUTINE SOCKI1(GL,AUU,AVV)
C  GRID ANGLE (GRDANG=MID LONGITUDE-PRESENT LONGITUDE)
C  THE ONLY NORTH-SOUTH MERIDIAN IS 170 DEGREES WEST
	GRDANG=170.0-GL
C  DEFINE SWIMMING SPEED OF SOCKEYE TOWARD FRAZIER RIVER
C  1KT=51 CM/SEC
	SOCKUU=51.0
	SOCKUU=26.7
C  --------------------
C  COMPUTE SPEED RELATIVE TO GRID X,Y COMPONENTS
	AUU=AUU+(SOCKUU*COS(GRDANG*3.14159/180.0))
	AVV=AVV+((SOCKUU*SIN(GRDANG*3.14159/180.0)))
C  DEFINE COMPAS SWIMMING ANGLE OF SOCKEYE TOWARD FRAZIER RIVER
	SOCKAN=95
	SOCKAN=90
C  --------------------
C  COMPUTE ANGLE + OF 90 INTO GRID X,Y COMPONENTS
	SOCKAN=SOCKAN-90
	SOCKVV=SOCKUU*(SIN(SOCKAN*3.14159/180.0))
	AUU=AUU+(SOCKVV*SIN(GRDANG*3.14159/180.0))
	AVV=AVV+((SOCKVV*COS(GRDANG*3.14159/180.0)))
	RETURN
	END
	SUBROUTINE SOCKI(SKCMPS,SKDEGT,GL,AUU,AVV)
	GRDANG=170.0-GL
C  DEFINE SWIMMING SPEED OF SOCKEYE TOWARD FRAZIER RIVER
C  1KT=51 CM/SEC
	SOCKSP=51.0
	SOCKSP=26.7
	IF(SKCMPS.NE.0.0)SOCKSP=SKCMPS
C  --------------------
C  DEFINE COMPAS SWIMMING ANGLE OF SOCKEYE TOWARD FRAZIER RIVER
C  IN DEGREES TRUE
	SOCKAN=95
C     SOCKAN=90
	SOCKAN=105.0
C     SOCKAN=120.0
	IF(SKDEGT.NE.0.0)SOCKAN=SKDEGT
C  --------------------
C  COMPUTE SPEED RELATIVE TO GRID X,Y COMPONENTS
	SOCKAN=SOCKAN-90
	AUU=AUU+SOCKSP*(COS((GRDANG-SOCKAN)*3.14159/180.0))
	AVV=AVV+SOCKSP*(SIN((GRDANG-SOCKAN)*3.14159/180.0))
	RETURN
	END
	SUBROUTINE AVECTR(XI,YJ,U,V)
	DATA VFAC/10.0/
C       CALL PLOT(XI,YJ,3)
	IF(U.EQ.0. .AND. V.EQ.0.)GO TO 200
	X=XI+U/VFAC
	Y=YJ+V/VFAC
C       CALL PLOT(X,Y,2)
	Z=.09/SQRT(U*U+V*V)
	SI=V*Z
	CO=U*Z
	XX=SI*.3
	YY=CO*.3
	CO=X-CO
	SI=Y-SI
C       CALL PLOT(CO-XX,SI+YY,2)
C       CALL PLOT(CO+XX,SI-YY,2)
C       CALL PLOT(X,Y,2)
 200  CONTINUE
	RETURN
	END
C**************************************************************
C*                                                            *
C*                    GRIDS PACKAGE                           *
C*                                                            *
C*    SUBROUTINES ARE ORDERED AS FOLLOWS                      *
C*                                                           *
C*                  SUBROUTINE INDEX                         *
C*                                                           *
C*  NAME           DESCRIPTION                     SEQUENCE  *
C*                                                           *
C* BATHY    PLOTS BATHYMETRY AND LEGEND               6700   *
C*                                                           *
C* CLCALC   COMPUTES DEFAULT CONTOUR LIMITS          58300   *
C*                                                           *
C* COAST    DRAWS COASTLINE                          21530   *
C*                                                           *
C* CONTOR   CONTOURS DATA                            60100   *
C*                                                           *
C* CONVRT   SCALES LAT-LONG DATA TO GRID INCHES      22000   *
C*                                                           *
C* DATFIX   SETS UP DATA STRING                      99900   *
C*                                                           *
C* GRIDER   DRAWS BACKGROUND GRID                   105800   *
C*                                                           *
C* MZERO    (FUNCTION) CHECKS FOR LAND              109000   *
C*                                                           *
C* NUMMER   NUMBERS ROWS AND COLUMNS                110200   *
C*                                                           *
C* PLTXY1   CHECKS IF PEN SHOULD CHANGE             114000   *
C* PLTXY2   (ENTRY) CHANGES PEN, WRITES NUMBERS     114800   *
C*          FOR CONTOR                                       *
C*                                                           *
C* POINTR   DRAWS (+) AT EACH GRID                  116600   *
C*                                                           *
C* SETIM    DRAWS SECTION PLOTS                     122780   *
C*                                                           *
C* SHIFT    (FUNCTION) SHIFTS BITS                  129300   *
C*                                                           *
C* STGRID   SETS UP COMMON BLOCKS                   131500   *
C*                                                           *
C* VECTOR   DRAWS VECTORS                           135300   *
C*                                                           *
C* XAXIS    DRAWS X-AXIS AND LABELS IT              13500   *
C*                                                           *
C* XMIT     ZEROES OUT CONTOR WORKING ARRAY         143000   *
C*                                                           *
C* YAXIS    DRAWS Y-AXIS AND LABELS IT              144300   *
C*                                                           *
C*************************************************************
C ********************************************************************
C *
      SUBROUTINE ANAGRD (NDATA,DATA,DATAT)
	COMMON/LSEA/LS(92,180)
	COMMON/LATLON/ALAT(92,180),ALON(92,180)
	COMMON/GRDCOM/CFACT,M,N,NM,XI(181),YJ(93)
	DIMENSION GUESS(10560),R(4),D(4),DATAT(92,180),
     &A(10560),DATA(3,NDATA)
C
C     SUBROUTINE TO ANALYZE RANDOM DATA VALUES INTO GRIDDED DATA.
C     USING FNWC METHODS
C     NDATA - NUMBER OF DATA VALUES SENT TO THE SUBROUTINE TO BE ANALYZE
C     DATA - AN ARRAY OF DATA VALUES TO BE ANALYZED.
C         SEE THE DOCUMENTATION FOR SPECIFICS OF INDICES.
C     DATAT - AN ARRAY OF GRIDDED DATA VALUES SENT BACK TO THE PROGRAM.
C
C  ZERO THE GUESS AND ITS WEIGHTING ARRAY A.
C
      DO 10 I=1,NM
      A(I)=0.
   10 GUESS(I)=0.
C
C  START THE ANALYSIS ON EACH DATA POINT
C
      MDATA=0
      DO 200 II=1,NDATA
      YK=DATA(1,II)
      XK=DATA(2,II)
      CALL CONVRT(XK,YK,IERR)
C
C  REJECT DATA IF IERR EQ 1  (INDICATES OFF GRID BOUNDARIES)
C
      IF(IERR.EQ.1)GO TO 200
      MDATA=MDATA+1
C   SELECTS THE Y-COMPONENT OF THE GRID FROM THE CONVERTED
C   Y POSITION,YK, OF THE DATA VALUE.
      YPP=100.
      DO 50 I=1,N
      IT=N-I+1
      DLTAY=YJ(IT)-YK
      IF(DLTAY.LT.0.)GO TO 50
      YP=DLTAY*DLTAY
      IF(YP.GT.YPP)GO TO 50
      YPP=YP
      IY=I
  50  CONTINUE
C   SELECTS THE X-COMPONENT OF THE GRID FROM THE CONVERTED
C   X POSITION,XK, OF THE DATA VALUE.
      XPP=100.
      DO 60 J=1,M
      DLTAX=XI(J)-XK
      IF(DLTAX.GT.0.)GO TO 60
      XP=DLTAX*DLTAX
      IF(XP.GT.XPP)GO TO 60
      XPP=XP
      IX=J
  60  CONTINUE
      ADATA=DATA(3,II)
      YINC=SQRT(YPP)
      XINC=SQRT(XPP)
      XINC1=1.-XINC
      YINC1=1.-YINC
C
C     COMPUTE WEIGHTS
C
      XS=XINC*XINC
      XZ=XINC1*XINC1
      YS=YINC*YINC
      YZ=YINC1*YINC1
      RDIFF=0.
      R(1)=XS+YS
      R(2)=XZ+YS
      R(3)=XS+YZ
      R(4)=XZ+YZ
      DO 121 K=1,4
      RX=1.-R(K)*R(K)
      R(K)=AMAX1(RX,0.)
      RDIFF=RDIFF+R(K)
  121 CONTINUE
      DO 122 K=1,4
      R(K)=R(K)/RDIFF
      D(K)=R(K)*ADATA
  122 CONTINUE
      IJ=(IY-1)*M+IX
      IF(IY.EQ.N) IJ=IJ-M
      IF(IX.EQ.M) IJ=IJ-1
C
C  COMPUTE W AND WD
C
      IF(IY.EQ.0) GO TO 124
      GUESS(IJ)=GUESS(IJ)+D(1)
      IF(IX.EQ.0) GO TO 123
      A(IJ)=A(IJ)+R(1)
  123 L=IJ+1
      GUESS(L)=GUESS(L)+D(2)
      A(L)=A(L)+R(2)
  124 L=IJ+M
      GUESS(L)=GUESS(L)+D(3)
      A(L)=A(L)+R(3)
      L=IJ+M+1
      GUESS(L)=GUESS(L)+D(4)
      A(L)=A(L)+R(4)
C
  200 CONTINUE
 2002 FORMAT(I5)
C
C  BE SURE NO DATA VALUES ARE STORED IN ZERO LAND-SEA SLOTS,
C  TRANSFER DATA INTO THE GRIDDED ARRAY FROM THE STRING ARRAY GUESS.
C
      DO 300 I=1,N
      DO 300 J=1,M
      IJ=(I-1)*M+J
      IF(LS(I,J).EQ.0)GO TO 300
      IF(A(IJ).EQ.0.)GO TO 300
      DATAT(I,J)=GUESS(IJ)/A(IJ)
  300 CONTINUE
      IF(MDATA.EQ.0)MDATA=1
      NDATA=MDATA
      RETURN
      END
C************************************************
C*              SUBROUTINE COAST                *
C*  C=ARRAY OF COAsTLINE LAT,LON,IPEN            *
C*  IC= NuMBER OF ELEMENTS IN C                 *
C************************************************
      SUBROUTINE COAST(IFCNV,IFPLT,IC,C1,C2,C3)
	DIMENSION C1(10000),C2(10000),C3(10000)
     1,C1NEW(5000),C2NEW(5000),I3NEW(5000)
      IF(IFCNV.NE.1)GO TO 150
      KI=0
      IOLD=0
      C1OLD=C1(1)
      C2OLD=C2(1)
      DO 130 J=1,IC
c	IF(J.EQ.500)WRITE(6,991)J,IC
c	IF(J.EQ.1000)WRITE(6,991)J,IC
c	IF(J.EQ.1500)WRITE(6,991)J,IC
c	IF(J.EQ.2000)WRITE(6,991)J,IC
c	IF(J.EQ.2500)WRITE(6,991)J,IC
c	IF(J.EQ.3000)WRITE(6,991)J,IC
  991 FORMAT(' ',I5,' POINTS OF',I5,' POINTS ARE CONVERTED')
	CALL CONVRT(C1(J),C2(J),IERR)
      IF(IERR.NE.1)GO TO 120
      IOLD=IERR
      GO TO 130
  120 CN1=C1(J)
      CN2=C2(J)
      DN1=C1OLD-CN1
      DN2=C2OLD-CN2
      DSTNCE=SQRT(DN1**2+DN2**2)
      IF(DSTNCE.LE.0.02)GO TO 130
      KI=KI+1
      II=C3(J)
      IF(IOLD.EQ.1)II=3
      C1NEW(KI)=C1(J)
      C2NEW(KI)=C2(J)
      I3NEW(KI)=II
      C1OLD=CN1
      C2OLD=CN2
      IOLD=IERR
  130 CONTINUE
      DO 140 K=1,IC
      IF(K.GT.KI)GO TO 145
      C1(K)=C1NEW(K)
      C2(K)=C2NEW(K)
      C3(K)=I3NEW(K)
      GO TO 140
  145 C1(K)=0.
      C2(K)=0.
      C3(K)=3
  140 CONTINUE
      IC=KI
  150 IF(IFPLT.NE.1)GO TO 200
      DO 160 I=2,IC
	IPEN=C3(I)
  160 CONTINUE
  200 RETURN
      END
C************************************************
C*          SUBROUTINE  CLCALC                  *
C*  CALCULATES DEFAULT CONTOUR LIMITS           *
C*     CALLED BY CONTOR, CALLS SCLE             *
C************************************************
      SUBROUTINE CLCALC(AMIN,AMAX,ICL)
	COMMON/CNTCOM/IPTMAX,IPTMIN,IPT,CNTMIN,CNTMAX,
     *CL,CLMOD1,CLMOD2,IP2,IPLT,IFCALC
C      CALL SCLE(15.,AMAX,AMIN,FIRST,DELTA)
      CNTMIN=FIRST
      CL=DELTA
      ICL=INT(CL)
      CNTMAX=AMAX
      CLMOD1=4.*CL
      CLMOD2=2.*CL
      RETURN
      END
C************************************************
C*         LOGICAL FUNCTION MZERO               *
C*           CHECKS FOR LAND                    *
C*    CALLED BY VECTOR AND CONTOR               *
C************************************************
      LOGICAL FUNCTION MZERO(A)
C
C      IF(A.LT.960.)WRITE(9,99)A
   99 FORMAT(' MZERO ACCESSED FOR ####= ',F12.6)
C
      MZERO=.FALSE.
      IF(A.EQ.-9999.)MZERO=.TRUE.
      RETURN

      END

C************************************************
C*            SUBROUTINE DATFIX                 *
C*  CONVERTS N BY M ARRAY (AB) INTO STRING (A)  *
C*   SETS LAND VALUES EQUAL TO -9999., AND      *
C* INTERPOLATES ACROSS SINGLE-POINT ISLANDS     *
C************************************************
      SUBROUTINE DATFIX(AB,A)
	COMMON/GRDCOM/CFACT,M,N,NM,XI(181),YJ(93)
	DIMENSION AB(92,180),IJS(8)
	DIMENSION A(10560)
	COMMON/LSEA/LS(92,180)
	LOGICAL MZERO
      DO 1 IXX=1,N
      I=(N+1)-IXX
C
C SET LAND VALUES = -9999.
C
      DO 2 J=1,M
C>>>>>>
C  CONTOR DOESN'T LIKE GRID VALUES = CONTOUR VALUE (IE .0000000...)
	IAB=AB(I,J)
	RIAB=IAB
	EE=RIAB-AB(I,J)
c	IF(ABS(EE).LT.0.01.AND.IAB.GT.800)WRITE(9,9976)J,I,IJ,AB(I,J)
 9976 FORMAT(' INTEGER I=',I2,' J=',I2,' IJ=',I4,' AB=',F15.10)
	IF(ABS(EE).LT.0.01)AB(I,J)=AB(I,J)+0.01
C  0.01 IS SMALL ENOUGH NOT TO CHANGE CONTOUR PATTERN SIGNIFICANTLY)
C<<<<<<
	IF(LS(I,J).EQ.0)AB(I,J)=-9999.
      IJ=(IXX-1)*M+J
   2  A(IJ)=AB(I,J)
   1  CONTINUE
C
C  IF SINGLE-POINT ISLAND,INTERPOLATE
C
      NM=N*M
      DO 200 IJ=1,NM
	IF(.NOT.MZERO(A(IJ)))GO TO 200
	IJS(1)=IJ+1
      IJS(2)=IJ-1
      IJS(3)=IJ+M
      IJS(4)=IJ-M
      IJS(5)=(IJ+M)+1
      IJS(6)=(IJ+M)-1
      IJS(7)=(IJ-M)+1
      IJS(8)=(IJ-M)-1
      SUM1=0.
      SUM=0.
      K=0
  10  K=K+1
C
C  CHECK IF IJ TOO NEAR EDGE OF GRID
C
      IF(IJS(K).LE.M)GO TO 9
      I2=MOD(M,IJS(K))
	IF(IJS(K).GE.1.AND.IJS(K).LE.NM.AND.I2.GT.1)
     &GO TO 11
   9  SUM1=SUM1+1
      IF(K.LT.8)GO TO 10
      GO TO 12
  11  R=A(IJS(K))
      IF(MZERO(R))GO TO 200
      SUM=SUM+R
      IF(K.LT.8)GO TO 10
  12  IF(SUM1.GE.4)GO TO 200
      A(IJ)=SUM/(8.0-SUM1)
 200  CONTINUE
	RETURN
      END
C*****************************************
C*        SUBROUTINE STGRD2              *
C*                                       *
C* SETS UP COMMON BLOCKS LIKE STGRID     *
C* FROM X&Y COORDINATE DATA              *
C*****************************************
      SUBROUTINE STGRD2(NN,MM,X,Y,ILS,XL,YL)
	DIMENSION X(50),Y(50),ILS(92,180)
	COMMON/GRDCOM/CFACT,M,N,NM,XI(181),YJ(93)
	COMMON/SIZES/XLEN,YLEN,YYLEN,SIZ1H,SIZ1V,SIZ2H,SIZ2V,
     &SIZ3H,SIZ3V,SIZ4H,SIZ4V
	COMMON/LSEA/LS(92,180)
      COMMON/SSYM/ISYM(5)
	M=MM
      N=NN
      NM=N*M
      XLEN=XL
      YLEN=YL
      YYLEN=YLEN-1.0
      SIZ1H=0.198
      SIZ1V=0.307
      SIZ2H=0.181
      SIZ2V=0.290
      SIZ3H=0.120
      SIZ3V=0.185
      SIZ4H=0.110
      SIZ4V=0.168
      DO 10 I=1,M
   10 XI(I)=X(I)
      DO 20 J=1,N
   20 YJ(J)=Y(J)
      DO 30 I=1,N
      DO 30 J=1,M
   30 LS(I,J)=ILS(I,J)
      ISYM(1)=40
      ISYM(2)=44
      ISYM(3)=41
      ISYM(4)=42
      ISYM(5)=32
      RETURN
      END
C***********************************************
C*        SUBROUTINE STGRIDr (read fast file)   *
C*                                             *
C* SETS UP COMMON BLOCKS TO BE USED BY ROUTINES*
C* NN,MM ARE NUMBER ROWS, NUMBER COLS IN GRID  *
C* ALA,ALO ARE LON,LAT ARRAYS OF GRID POINTS   *
C* ILS IS THE LAND-SEA ARRAY                   *
C* XL,YL ARE THE WIDTH AND HEIGHT OF THE PLOT  *
C***********************************************
	SUBROUTINE STGRIDr(NN,MM,ALA,ALO,ILS,XL,YL)
	COMMON/GRDCOM/CFACT,M,N,NM,XI(181),YJ(93)
	COMMON/SIZES/XLEN,YLEN,YYLEN,SIZ1H,SIZ1V,SIZ2H,SIZ2V,
     &SIZ3H,SIZ3V,SIZ4H,SIZ4V
	COMMON/LSEA/LS(92,180)
	COMMON/LATLON/ALAT(92,180),ALON(92,180)
	COMMON/SSYM/ISYM(5)
	COMMON/NZMZ/NZ1,NZ2,MZ1,MZ2
	DIMENSION ALA(92,180),ALO(92,180),ILS(92,180)
	DIMENSION SLAT(92,180),SLON(92,180)
	irdonly=1
	M=MM
	N=NN
	NM=N*M
	XLEN=XL
	YLEN=YL
	YYLEN=YLEN-1.0
	NZ1=1
	NZ2=92
	MZ1=1
	MZ2=180
	NZMZ=NZ2*MZ2
	SIZ1H=0.198
	SIZ1V=0.307
	SIZ2H=0.181
	SIZ2V=0.290
	SIZ3H=0.120
	SIZ3V=0.185
	SIZ4H=0.110
	SIZ4V=0.168
	DO 10 I=1,N
	DO 10 J=1,M
	ALAT(I,J)=ALA(I,J)
	ALON(I,J)=ALO(I,J)
	LS(I,J)=ILS(I,J)
   10 CONTINUE
	DO 60 I=1,N
   60 CONTINUE
	ISYM(1)=40
	ISYM(2)=44
	ISYM(3)=41
	ISYM(4)=42
	ISYM(5)=32
C-------------------
	J=1
	J=80
	DO 20 I=1,N
C	if(irdonly.eq.1)Read(16,9888,end=20)IM1d,YJ(i)
	if(irdonly.eq.1)go to 20
	IM1=I
	SLON(I,J)=ALON(I,J)
	SLAT(I,J)=ALAT(I,J)
	CALL CONVRTO(SLON(I,J),SLAT(I,J),IE)
	YJ(IM1)=SLAT(I,J)
	XI(J)=SLON(I,J)
c      IF(I.EQ.90)WRITE(6,888)IM1,YJ(IM1),I,J,XI(J),ALAT(I,J),ALON(I,J)
	WRITE(10,9888)IM1,YJ(IM1),i,J,XI(J),ALAT(I,J),ALON(I,J)
   20 CONTINUE
C--------------------
	I=1
	I=33
	I=46
	DO 21 J=1,M
C	if(irdonly.eq.1)Read(16,9888,end=21)IM1d,YJd,id,Jd,XI(J)
	if(irdonly.eq.1)go to 21
	IM1=I
	SLON(I,J)=ALON(I,J)
	SLAT(I,J)=ALAT(I,J)
	CALL CONVRTO(SLON(I,J),SLAT(I,J),IE)
	YJ(IM1)=SLAT(I,J)
	XI(J)=SLON(I,J)
c	IF(J.EQ.60)WRITE(6,888)IM1,YJ(IM1),I,J,XI(J),ALAT(I,J),ALON(I,J)
c	IF(J.EQ.120)WRITE(6,888)IM1,YJ(IM1),I,J,XI(J),ALAT(I,J),ALON(I,J)
c	IF(J.EQ.180)WRITE(6,888)IM1,YJ(IM1),I,J,XI(J),ALAT(I,J),ALON(I,J)
c	WRITE(9,888)IM1,YJ(IM1),i,J,XI(J),ALAT(I,J),ALON(I,J)
	WRITE(10,9888)IM1,YJ(IM1),i,J,XI(J),ALAT(I,J),ALON(I,J)
 9888 FORMAT(I4,F9.3,2I4,3F9.3)
   21 CONTINUE
  888 FORMAT(' IN STGRID ===== ',I4,F9.3,2I4,3F9.3)
  844 CONTINUE
	RETURN
	END
C***********************************************
C*        SUBROUTINE STGRID                    *
C*                                             *
C* SETS UP COMMON BLOCKS TO BE USED BY ROUTINES*
C* NN,MM ARE NUMBER ROWS, NUMBER COLS IN GRID  *
C* ALA,ALO ARE LON,LAT ARRAYS OF GRID POINTS   *
C* ILS IS THE LAND-SEA ARRAY                   *
C* XL,YL ARE THE WIDTH AND HEIGHT OF THE PLOT  *
C***********************************************
	SUBROUTINE STGRID(NN,MM,ALA,ALO,ILS,XL,YL)
	COMMON/GRDCOM/CFACT,M,N,NM,XI(181),YJ(93)
	COMMON/SIZES/XLEN,YLEN,YYLEN,SIZ1H,SIZ1V,SIZ2H,SIZ2V,
     &SIZ3H,SIZ3V,SIZ4H,SIZ4V
	COMMON/LSEA/LS(92,180)
	COMMON/LATLON/ALAT(92,180),ALON(92,180)
	COMMON/SSYM/ISYM(5)
	COMMON/NZMZ/NZ1,NZ2,MZ1,MZ2
	DIMENSION ALA(92,180),ALO(92,180),ILS(92,180)
	DIMENSION SLAT(92,180),SLON(92,180)
	M=MM
	N=NN
	NM=N*M
	XLEN=XL
	YLEN=YL
	YYLEN=YLEN-1.0
	NZ1=1
	NZ2=92
	MZ1=1
	MZ2=180
	NZMZ=NZ2*MZ2
	SIZ1H=0.198
	SIZ1V=0.307
	SIZ2H=0.181
	SIZ2V=0.290
	SIZ3H=0.120
	SIZ3V=0.185
	SIZ4H=0.110
	SIZ4V=0.168
	DO 10 I=1,N
	DO 10 J=1,M
	ALAT(I,J)=ALA(I,J)
	ALON(I,J)=ALO(I,J)
	LS(I,J)=ILS(I,J)
   10 CONTINUE
	DO 60 I=1,N
   60 CONTINUE
	ISYM(1)=40
	ISYM(2)=44
	ISYM(3)=41
	ISYM(4)=42
	ISYM(5)=32
C-------------------
	J=1
	J=80
	DO 20 I=1,N
	IM1=I
	SLON(I,J)=ALON(I,J)
	SLAT(I,J)=ALAT(I,J)
	CALL CONVRTO(SLON(I,J),SLAT(I,J),IE)
	YJ(IM1)=SLAT(I,J)
	XI(J)=SLON(I,J)
c	IF(I.EQ.30)WRITE(6,888)IM1,YJ(IM1),I,J,XI(J),ALAT(I,J),ALON(I,J)
c	IF(I.EQ.60)WRITE(6,888)IM1,YJ(IM1),I,J,XI(J),ALAT(I,J),ALON(I,J)
c	IF(I.EQ.90)WRITE(6,888)IM1,YJ(IM1),I,J,XI(J),ALAT(I,J),ALON(I,J)
c	WRITE(9,888)IM1,YJ(IM1),J,XI(J),ALAT(I,J),ALON(I,J)
   20 CONTINUE
C--------------------
	I=1
	I=33
	I=46
	DO 21 J=1,M
	IM1=I
	SLON(I,J)=ALON(I,J)
	SLAT(I,J)=ALAT(I,J)
	CALL CONVRTO(SLON(I,J),SLAT(I,J),IE)
	YJ(IM1)=SLAT(I,J)
	XI(J)=SLON(I,J)
c	IF(J.EQ.60)WRITE(6,888)IM1,YJ(IM1),I,J,XI(J),ALAT(I,J),ALON(I,J)
c	IF(J.EQ.120)WRITE(6,888)IM1,YJ(IM1),I,J,XI(J),ALAT(I,J),ALON(I,J)
c	IF(J.EQ.180)WRITE(6,888)IM1,YJ(IM1),I,J,XI(J),ALAT(I,J),ALON(I,J)
c	WRITE(9,888)IM1,YJ(IM1),J,XI(J),ALAT(I,J),ALON(I,J)
   21 CONTINUE
  888 FORMAT(' IN STGRID ===== ',I4,F9.3,2I4,3F9.3)
  844 CONTINUE
	RETURN
	END
C***********************************************
C*        SUBROUTINE STGRIDT (TEST) 4-27-95                    *
C*                                             *
C* SETS UP COMMON BLOCKS TO BE USED BY ROUTINES*
C* NN,MM ARE NUMBER ROWS, NUMBER COLS IN GRID  *
C* ALA,ALO ARE LON,LAT ARRAYS OF GRID POINTS   *
C* ILS IS THE LAND-SEA ARRAY                   *
C* XL,YL ARE THE WIDTH AND HEIGHT OF THE PLOT  *
C***********************************************
	SUBROUTINE STGRIDT(NN,MM,ALA,ALO,ILS,XL,YL)
	COMMON/GRDCOM/CFACT,M,N,NM,XI(181),YJ(93)
	COMMON/SIZES/XLEN,YLEN,YYLEN,SIZ1H,SIZ1V,SIZ2H,SIZ2V,
     &SIZ3H,SIZ3V,SIZ4H,SIZ4V
	COMMON/LSEA/LS(92,180)
	COMMON/LATLON/ALAT(92,180),ALON(92,180)
	COMMON/SSYM/ISYM(5)
	COMMON/NZMZ/NZ1,NZ2,MZ1,MZ2
	DIMENSION ALA(92,180),ALO(92,180),ILS(92,180)
	DIMENSION SLAT(92,180),SLON(92,180)
	M=MM
	N=NN
	NM=N*M
	XLEN=XL
	YLEN=YL
	YYLEN=YLEN-1.0
	NZ1=1
	NZ2=92
	MZ1=1
	MZ2=180
	SIZ1H=0.198
	SIZ1V=0.307
	SIZ2H=0.181
	SIZ2V=0.290
	SIZ3H=0.120
	SIZ3V=0.185
	SIZ4H=0.110
	SIZ4V=0.168
	DO 10 I=1,N
	DO 10 J=1,M
	ALAT(I,J)=ALA(I,J)
	ALON(I,J)=ALO(I,J)
	LS(I,J)=ILS(I,J)
   10 CONTINUE
	DO 60 I=1,N
   60 CONTINUE
	ISYM(1)=40
	ISYM(2)=44
	ISYM(3)=41
	ISYM(4)=42
	ISYM(5)=32
C-------------------
	J=1
	J=80
	DO 20 I=1,N
	IM1=I
	SLON(I,J)=ALON(I,J)
	SLAT(I,J)=ALAT(I,J)
	CALL CONVRT(SLON(I,J),SLAT(I,J),IE)
	YJ(IM1)=SLAT(I,J)
	XI(J)=SLON(I,J)
c	IF(I.EQ.10)WRITE(9,888)IM1,YJ(IM1),J,XI(J),ALAT(I,J),ALON(I,J)
c	IF(I.EQ.50)WRITE(9,888)IM1,YJ(IM1),J,XI(J),ALAT(I,J),ALON(I,J)
	SLON(I,J)=ALON(I,J)
	SLAT(I,J)=ALAT(I,J)
	CALL CONVRTO(SLON(I,J),SLAT(I,J),IE)
	YJ(IM1)=SLAT(I,J)
	XI(J)=SLON(I,J)
   20 CONTINUE
C--------------------
	I=1
	I=33
	DO 21 J=1,M
	IM1=I
	SLON(I,J)=ALON(I,J)
	SLAT(I,J)=ALAT(I,J)
	CALL CONVRTO(SLON(I,J),SLAT(I,J),IE)
	YJ(IM1)=SLAT(I,J)
	XI(J)=SLON(I,J)
   21 CONTINUE
  888 FORMAT(' IN STGRIDT ===== ',I4,F9.3,I4,3F9.3)
  844 CONTINUE
	RETURN
	END
C************************************************
C*         SUBROUTINE FASTCOAST
C*              ORIGINAL
C************************************************
	SUBROUTINE FASTCO(IFAST,ISKIP,NREC)
	NREC=0
	NIE=0
	NSKIP=0
      OPEN(71,file='cst92180.dat',STATUS='UNKNOWN',
     &ACCESS='SEQUENTIAL')
    1 READ(70,107,END=49) XC,YC,IPEN
	NSKIP=NSKIP+1
	IF (IPEN.EQ.3.OR.NSKIP.EQ.ISKIP) GO TO 2
	GO TO 1
    2 CONTINUE
	NSKIP=0
  107 FORMAT (2F10.4,I10)
	CALL CONVRT(XC,YC,IE)
	IF (IE.GT.0) NIE=NIE+1
	IF (NIE.GT.0) IPEN=3
	IF (IE.EQ.0) NIE=0
	IF (NIE.GT.2) GO TO 1
	IF (IE.GT.0) IPEN=3
	IF (NREC.EQ.0) IPEN=3
	NREC=NREC+1
	WRITE (71,108) XC,YC,IPEN
  108 FORMAT (2F10.4,I3)
	GO TO 1
   49 CLOSE(UNIT=71)
c	WRITE(6,110) NREC
c	WRITE(9,110) NREC
	RETURN
  110 FORMAT (1H0,'NUMBER OF FASTCOAST RECORDS =',I10)
	END
C************************************************
C*         SUBROUTINE FASTCOAST-B
C*           WITH PACIFIC RIM SLIP BOUNDARY CALC. SET UP
C************************************************
	SUBROUTINE FASTCOB(IFAST,ISKIP,NREC)
	DIMENSION NBG(92,180),BGI(92,180,9),BGJ(92,180,9)
C  IN BGI ARRAY THE I VALUES ARE STORED AND IN THE BGJ ARRAY THE J
C  VALUES ARE STORED FOR THE (I.YY, J.XX) BOUNDARY POINTS AT GRID LINE
C  INTERSECTIONS AT THE CORRESPONDING (I,J) GRID POINTS
C  2. PLOT LINES BETWEEN 2 POINTS IN EACH SQUARE
C      SUBROUTINE IJ2INCHS(XX,YY,XXIN,YYIN,IERR)
C
C       CALL NEWPEN(10)
	XP1=2.0
	YP1=2.0
	XP2=179.0
	YP2=91.0
	CALL IJ2INCHS(XP1,YP1,XP1IN,YP1IN,IERR)
	CALL IJ2INCHS(XP2,YP1,XP2IN,YP2IN,IERR)
	CALL IJ2INCHS(XP2,YP2,XP3IN,YP3IN,IERR)
	CALL IJ2INCHS(XP1,YP2,XP4IN,YP4IN,IERR)
	DO 812 IZR=1,92
	DO 813 JZR=1,160
	NBG(IZR,JZR)=0
	DO 814 KZR=1,9
	BGI(IZR,JZR,KZR)=0.0
	BGJ(IZR,JZR,KZR)=0.0
  814 CONTINUE
  813 CONTINUE
  812 CONTINUE
C
C  READ LONG-LAT, PEN
C  OF POINT BY POINT COASTLINE FILE (C:\COASTLNS\JICLFNWC.MOD)
	IREC=0
	NREC=0
	IERRLAST=0
	NIE=0
	NSKIP=0
    1 READ(70,107,END=49) XC,YC,IPEN,MREC
	IREC=IREC+1
	NSKIP=NSKIP+1
	IF (IPEN.EQ.3.OR.NSKIP.EQ.ISKIP) GO TO 2
	GO TO 1
    2 CONTINUE
	NSKIP=0
  107 FORMAT (2F10.4,2I10)
  118 FORMAT(' COAST950-1050 ',2I5,2F7.2,' IPEN=',I1,' FILE REC=',I5)
	XCGD1=XC
	YCGD1=YC
	XCGD=XC
	YCGD=YC
C  CONVERT LON-LAT TO GRID I-J
	CALL CONVIJ (XCGD,YCGD,IERR)
  119 FORMAT(' CONVIJ IERR >0 ',2I5,2F7.2,' IPEN=',I1,' FILE REC=',I5,'
     *IERR=',I1)
	IF(IERR.EQ.1)IERRLAST=1
	IF(IERR.EQ.1)GO TO 1
C
C  CONVERT LON-LAT TO PLOT INCHES X-Y
	CALL CONVRT(XC,YC,IE)
  117 FORMAT(' CONVRT IE=0',2I5,2F7.2,' IPEN=',I1,' FILE REC=',I5,'
     * IE=',I1)
	IF(IE.EQ.1)IERRLAST=1
	IF (IE.GT.0) NIE=NIE+1
	IF (NIE.GT.0) IPEN=3
	IF (IE.EQ.0) NIE=0
	IF (NIE.GT.2) GO TO 1
	IF (IE.GT.0) IPEN=3
C
C  FLAG OUT OFF MAP POINTS
	IF(IE.GT.0)IERRLAST=1
	IF(IE.GT.0)GO TO 1
	IF (NREC.EQ.0) IPEN=3
C
	NREC=NREC+1
C  WRITE FASTCOAST FILE 71
C      WRITE (71,108) XC,YC,IPEN
C      IF(IPEN.EQ.3)WRITE (6,108)IREC,NREC,XC,YC,IPEN,XCGD,YCGD,XLEN,YYL
C     *N,IERR
  108 FORMAT (2I5,2F7.2,' PLOT IPEN=',I1,4F7.2,' IERR=',I2)
C  FIND ALL GRID LINE INTERCEPTS GOING POINT TO POINT
C   WITH ST. LINE ALONG COAST
C
	IF(IPEN.EQ.3.OR.IERRLAST.EQ.1)X1=XCGD
	IF(IPEN.EQ.3.OR.IERRLAST.EQ.1)Y1=YCGD
	X2=XCGD
	Y2=YCGD
	IF(IPEN.EQ.3.OR.IERRLAST.EQ.1)GO TO 8999
	I1=Y1
	J1=X1
	I2=Y2
	J2=X2
C  INITIAL POINT IS AT (I1,J1) IN (92,180) ARRAY
C--------------
C  SKIP IF BOTH POINTS ARE IN THE SAME GRID
	IF(I1.EQ.I2.AND.J1.EQ.J2)GO TO 8999
C  DERIVE EQUATION FOR STRAIGHT LINT BET. POINTS
C  LIMIT SLOPE
	DX=X2-X1
	ADX=ABS(DX)
	IF(ADX.LT.0.001)ADX=0.001
	IF(DX.LT.0.0)ADX=-ADX
	SLOPE=(Y2-Y1)/ADX
	BINTCP=Y1-((SLOPE)*X1)
	IF(J1.EQ.J2) GO TO 800
C
C--------------------------------------------
C  FIND J INTERCEPT WHILE CROSSING THE LINE I=I1
	ABJ=ABS(J2-J1)
c	IF(ABJ.GT.1.0)WRITE(9,890)NREC,X1,Y1,X2,Y2
  890 FORMAT(' WARNING----J DIFFERENCE GT 1 BET. COAST POINTS',I5,4F8.4)
C      IF(ABJ.GT.1.0)IERRLAST=1
	IF(ABJ.GT.1.0)GO TO 8999
	IF(X2.GT.X1)XINT=J2
	IF(X2.LE.X1)XINT=J1
	YVAL=(SLOPE*XINT)+BINTCP
C  Y CANNOT BE OUT OF THIS INITIAL (I1,J1) GRID SQUARE
	IYVAL=YVAL
c	IF(IYVAL.NE.I1)WRITE(9,350)I1,XINT,YVAL
  350 FORMAT(' OUT OF I1 GRID   I1=',I3,' XINT=',F10.5,'  YVAL=',F10.5)
	IF(IYVAL.NE.I1)GO TO 8999
C  SAVE VALUE AT X-INTERCEPT
	J=XINT
	IF(J.GE.2)J2STOR=J-1
C
C  TEST WRITE
  318 FORMAT(' I1=',I5,' J=',I5,' NBG(I1,J,2)=',I15,'   OK TO BGJ ')
C
C  FIX NBG(...)
C*********
	NBG(I1,J)=NBG(I1,J)+1
	LSP1=NBG(I1,J)
c	IF(LSP1.GE.5)WRITE(9,315)I1,J,LSP1
  315 FORMAT(2I5,' LSP1=',I15,'   OK TO BGJ IN J-INTERCEPT DATA')
c	IF(YVAL.LT.1.0)WRITE(6,340)I1,J,LSP1,YVAL
  340 FORMAT(' WARNING==YVAL=0.0 AT  ',2I4,F7.2)
	IF(YVAL.LT.1.0)GO TO 8999
	BGI(I1,J,LSP1)=YVAL
	BGJ(I1,J,LSP1)=XINT
	NBG(I1,J2STOR)=NBG(I1,J2STOR)+1
	LSP2=NBG(I1,J2STOR)
C
C  TEST WRITE
c	IF(LSP2.GE.5)WRITE(9,317)I1,J2STOR,LSP2
  317 FORMAT(2I5,' LSP2=',I5,'   OK TO BGJ IN J-INTERCEPT DATA')
C
	BGI(I1,J2STOR,LSP2)=YVAL
	BGJ(I1,J2STOR,LSP2)=XINT
C
C  WRITE 72 J-INTERCEPT ONLY
	WRITE(72,89)IREC,NREC,XCGD1,YCGD1,X1,Y1,X2,Y2,I1,J,YVAL,XINT,I1,J2
     *STOR
	GO TO 8999
C
  800 CONTINUE
C  FIND I INTERCEPT WHILE CROSSING THE LINE J=J1
C
C  X=(Y-B)/M
	ABI=ABS(I2-I1)
c	IF(ABI.GT.1.0)WRITE(9,891)NREC,X1,Y1,X2,Y2
	IF(ABI.GT.1.0)GO TO 8999
  891 FORMAT(' WARNING----I DIFFERENCE GT 1 BET. COAST POINTS',I5,4F8.4)
	IF(Y2.GT.Y1)YINT=I2
	IF(Y2.LE.Y1)YINT=I1
	XVAL=(YINT-BINTCP)/SLOPE
C  Y CANNOT BE OUT OF THIS (I1,J1) GRID SQUARE
	IXVAL=XVAL
c	IF(IXVAL.NE.J1)WRITE(9,351)J1,YINT,XVAL
  351 FORMAT(' OUT OF J1 GRID   J1=',I3,' YINT=',F10.5,'  XVAL=',F10.5)
	IF(IXVAL.NE.J1)GO TO 8999
C
c	IF(XVAL.LT.1.0)WRITE(6,341)I,J1,NBG(I,J1),XVAL
  341 FORMAT(' WARNING==XVAL=0.0 AT  ',2I4,F7.2)
C
	IF(YVAL.LT.1.0)GO TO 8999
C  SAVE VALUE AT Y-INTERCEPT AT Y=YINT
	I=YINT
	IF(I.GE.2)I2STOR=I-1
	NBG(I,J1)=NBG(I,J1)+1
	LSP1=NBG(I,J1)
C
C  TEST WRITE
c	IF(LSP1.GE.5)WRITE(9,327)I,J1,LSP1
  327 FORMAT(2I5,' LSP1=',I5,'   OK TO BGI IN I-INTERCEPT DATA')
C
	BGI(I,J1,LSP1)=YINT
	BGJ(I,J1,LSP1)=XVAL
 8980 NBG(I2STOR,J1)=NBG(I2STOR,J1)+1
	LSP2=NBG(I2STOR,J1)
C
C  TEST WRITE
c	IF(LSP2.GE.5)WRITE(9,328)I2STOR,J1,LSP2
  328 FORMAT(2I5,' LSP2=',I5,'   OK TO BGI IN I-INTERCEPT DATA')
C
	BGI(I2STOR,J1,LSP2)=YINT
	BGJ(I2STOR,J1,LSP2)=XVAL
C
C  WRITE 72 I-INTERCEPT ONLY
	WRITE(72,89)IREC,NREC,XCGD1,YCGD1,X1,Y1,X2,Y2,I,J1,YINT,XVAL,I2STO
     *R,J1
   89 FORMAT(2I5,2F7.2,F7.2,F6.2,F7.2,F6.2,I3,I4,2F7.2,I3,I4)
C  FOR LAST GOOD POINT IERRLAST=0
	IERRLAST=0
 8999 CONTINUE
	X1=X2
	Y1=Y2
	IF(IE.EQ.0)IERRLAST=0
	IF(IERR.EQ.0)IERRLAST=0
C  GO TO NEXT POINT PAIR
C
	GO TO 1
   49 CONTINUE
C
C  PLOT LINES BETWEEN 2 POINTS IN EACH SQUARE
	XP1=2.0
	YP1=2.0
	XP2=179.0
	YP2=91.0
	CALL IJ2INCHS(XP1,YP1,XP1IN,YP1IN,IERR)
	CALL IJ2INCHS(XP2,YP1,XP2IN,YP2IN,IERR)
	CALL IJ2INCHS(XP2,YP2,XP3IN,YP3IN,IERR)
	CALL IJ2INCHS(XP1,YP2,XP4IN,YP4IN,IERR)
C
C  DRAW LINE WITHIN EACH SQUARE WITH 2 VALUES
C  FROM UPPER LEFT TO LOWER RIGHT ?? ARRAY 1 TO 2
C  SKIP
	IF(1.EQ.1)GO TO 403
	DO 400 I=1,91
	DO 401 J=1,179
	IF(NBG(I,J).LE.0)GO TO 401
	YP1=BGJ(I,J,1)
	XP1=J
	CALL IJ2INCHS(XP1,YP1,XP1IN,YP1IN,IERR)
	YP2=I+1
	XP2=BGI(I,J,2)
	CALL IJ2INCHS(XP2,YP2,XP2IN,YP2IN,IERR)
c	WRITE(9,301)I,J,NBG(I,J),NBG(I,J),BGI(I,J,1),BGI(I,J,2),
c     *BGJ(I,J,1),BGJ(I,J,2)
  301 FORMAT(2I4,' NBG=',2I3,' BGI=',2F7.2,' BGJ=',2F7.2)
  401 CONTINUE
  400 CONTINUE
  403 CONTINUE
C
	CLOSE(UNIT=72)
C  TEST PRINT EACH SQUARE AND RESOLVE ONE POINT PER BOUNDARY
C  WRITE FILE FROM RIGHT TO LEFT (INCREASING LONGITUDES)
C  FOR SORTING ONE PAIR PER GRID SQUARE CONTINUOUS ACROSS MAP
C  AS SLIP BOUNDARY
	DO 500 JL=1,179
	J=160-JL
C  BERING SEA PATCH (BETWEEN J=80 AND J=88)
	IF(J.LT.80)GO TO 500
	IF(J.GT.87)GO TO 500
	DO 501 I=1,91
C  TEST FOR OFFSCALE
c	IF(NBG(I,J).GT.9)WRITE(9,330)I,J,NBG(I,J)
  330 FORMAT(' DOT-NBG1 GT 9   ',2I5,I15)
	IF(NBG(I,J).GT.9)NBG(I,J)=0
	IF(NBG(I,J).LE.0)GO TO 501
	DO 510 IP1=1,NBG(I,J)
	DOTX=BGJ(I,J,IP1)
	DOTY=BGI(I,J,IP1)
	CALL IJ2INCHS(DOTX,DOTY,XP2IN,YP2IN,IERR)
	WRITE(76,59)I,J,NBG(I,J),IP1,BGI(I,J,IP1),BGJ(I,J,IP1),XP2IN,YP2IN
   59 FORMAT(2I4,' PLDOT NBG(I,J)=',2I3,' BGI=',F7.2,' BGJ=',F7.2,' INCH
     *=',2F5.2)
  510 CONTINUE
  501 CONTINUE
  500 CONTINUE
	CLOSE(UNIT=76)
C
C
C  EXPAND GRID TO VIEW COASTLINE AND LIMITING LINE IN EACH COASTAL GRID
C  SQUARE
C  ===== NOT YET =====
C
c	WRITE(6,110) NREC
	RETURN
  110 FORMAT (1H0,'NUMBER OF FASTCOAST RECORDS =',I10)
	END
C******************************************************
C*
C*            SUBROUTINE CSET
C*
C******************************************************
	SUBROUTINE CSET(NFILE,IC,C1,C2,C3)
	DIMENSION C1(10000),C2(10000),C3(10000)
	N1=1
	N2=0
    1 READ(NFILE,100,END=999)C1(N1),C2(N1),C3(N1)
  100 FORMAT(2F10.4,I3)
	N1=N1+1
	N2=N2+1
	GO TO 1
  999 CONTINUE
	IC=N2
	RETURN
	END

	SUBROUTINE PRINT3(P,F)
	DIMENSION P(92,180),IP(92,180),I1(92)
c	WRITE(6,302)F
	DO 1 I=1,92
      I1(I)=I
    1 CONTINUE
c	WRITE(6,301) (I1(K),K=1,92)
c      WRITE(6,302)
  302 FORMAT(/,F5.1,'=F')
	DO 200 I=1,66
	DO 201 JB=1,160
	J=161-JB
      IP(I,J)=P(I,JB)*F
  201 CONTINUE
  200 CONTINUE
	DO 300 J=1,160
c	WRITE(6,301) (IP(K,J),K=1,66),J
  300 CONTINUE
  301 FORMAT (1X,66I3,I6)
      RETURN
      END
C*****************************************************
C*                                                   *
C*          SUBROUTINE CONVRTO(XX,YY,IERR)            *
C*  CONVERTS XX,YY FROM LON,LAT TO GRID (I,J) UNITS  *
C*  THEN CONVERTS FROM GRID UNITS TO PLOT INCHES     *
C*  IF (XX,YY) OUT OF GRID AREA, IERR=1              *
C*                 "OLD"                                  *
C*****************************************************
	SUBROUTINE CONVRTO(XX,YY,IERR)
	COMMON/GRDCOM/CFACT,M,N,NM,XI(181),YJ(93)
	COMMON/SIZES/XLEN,YLEN,YYLEN,SIZ1H,SIZ1V,SIZ2H,SIZ2V,
     &SIZ3H,SIZ3V,SIZ4H,SIZ4V
	COMMON/LATLON/ALAT(92,180),ALON(92,180)
	DIMENSION SLON(92,180)
	IF (XX.LT.0.0) XX=-XX
C
C
C  GIVEN A RANDOM POINT ON THE GLOBE (LONGITUDE,LATITUDE) DEGREE-DECIMAL
C  AND A GRID MESH OF DEFINED (LONG,LAT) POINTS
C  FIND THE (I,J) COORDINATES OF THE RANDOM POINT (XX,YY)
C  RELATIVE TO THE GIVEN GRID MESH
C  REJECT POINTS OUTSIDE OF THE GRID
C  THIS IS A GENERALIZED SUBPROGRAM TO FIT ANY NEARLY SQUARE
C  EQUIDISTANT GRID IN GLOBE (SPHERICAL) SPACE
C  BUT
C  GRID LINES MAY HAVE A CONSTANT SLOPE OR MAY VARY WITHIN GRID
C  IN LONGITUDE-LATITUDE SPACE
C  FOR THE GIVEN (NXM) GRID ----- N=ROW#, M=COL# -----
C  THIS RESULTS IN THE ABILITY TO MAP SQUARE OR ODD SHAPED GRIDS
C  INTO A SQUARE OR RECTANGULAR PLOT TO MATCH THE PRINTED PAGE
C  XLEN IS THE DESIRED LENGTH OF THE X-AXIS IN INCHES
C  YLEN IS THE DESIRED LENGTH OF THE Y-AXIS IN INCHES
C
C
C
C
	N1=N-1
	M1=M-1
	AN1=N1
	AM1=M1
	DXX=XLEN/AM1
	DYY=YYLEN/AN1
C
C  REJECT MOST POINTS OUT OF GRID EXTREMES
	IERR=0
C
C  FIND MAX AND MIN LAT AND LONG OF GRID
	AMAXLA=0.0
	AMAXLO=0.0
	AMINLA=1000.0
	AMINLO=1000.0
	DO 1201 I=1,N
	DO 1202 J=1,M
	IF (ALAT(I,J).GT.AMAXLA) AMAXLA=ALAT(I,J)
	IF (ALON(I,J).GT.AMAXLO) AMAXLO=ALON(I,J)
	IF (ALAT(I,J).LT.AMINLA) AMINLA=ALAT(I,J)
	IF (ALON(I,J).LT.AMINLO) AMINLO=ALON(I,J)
 1202 CONTINUE
 1201 CONTINUE
C
C
C  REJECT POINT IF OUT OF GRID MAX AND MIN LAT OR LONG
	IF (YY.GT.AMAXLA) IERR=1
	IF (YY.LT.AMINLA) IERR=1
	IF (XX.GT.AMAXLO) IERR=1
	IF (XX.LT.AMINLO) IERR=1
C  JUMP TO ERROR IF POINT IS OUTSIDE OF SQUARE OF BOUNDARY EXTREMES
C  AND SET BOTH COORDINATES TO MAXIMUM PLOT INCHES BEFORE RETURN
 1240 IF (IERR.EQ.1.AND.XX.GT.XLEN) XX=XLEN
	IF (IERR.EQ.1.AND.YY.GT.YYLEN) YY=YYLEN
	IF (IERR.EQ.1)  GO TO 9999
C
C
C  IN CASE THE GRID IS TILTED (ALL OR IN PART)
C  LOOK AROUND GRID BOUNDARY LINES TO REJECT EXTERIOR POINTS
C  COMPUTE EQUATION OF LINE BETWEEN 2 POINTS
C  IF XX IS BETWEEN THE POINTS TEST IF YY INSIDE OR OUTSIDE OF GRID
C  COVER THE CASES FOR EACH SIDE OF GRID SLOPING + OR - (LAT=HORIZ)
C
C  LOOK AT TOP AND BOTTOM LINES OF GRID
	I=1
 8002 DO 8001 J=1,M1
	X1=ALON(I,J)
	X2=ALON(I,J+1)
	Y1=ALAT(I,J)
	Y2=ALAT(I,J+1)
	IF (XX.GT.X1.OR.XX.LT.X2) GO TO 8001
	IF (X2.EQ.X1) GO TO 8001
	Y0=Y1+((Y2-Y1)*(XX-X1)/(X2-X1))
	IF (YY.GT.Y0.AND.I.EQ.1) IERR=1
	IF (YY.GT.Y0.AND.I.EQ.1) GO TO 1240
	IF (YY.LT.Y0.AND.I.EQ.N) IERR=1
	IF (YY.LT.Y0.AND.I.EQ.N) GO TO 1240
 8001 CONTINUE
	IF (I.EQ.N) GO TO 8025
	I=N
	GO TO 8002
 8025 CONTINUE
C
C  LOOK AT LEFT AND RIGHT BOUNDARY LINES
	J=1
 8007 DO 8006 I=1,N1
	X1=ALON(I,J)
	X2=ALON(I+1,J)
	Y1=ALAT(I,J)
	Y2=ALAT(I+1,J)
	IF (X2.EQ.X1) GO TO 8006
	IF ((X2-X1).LT.0.0) GO TO 8013
	IF (XX.LT.X1.OR.XX.GT.X2) GO TO 8006
	GO TO 8014
 8013 IF (XX.GT.X1.OR.XX.LT.X2) GO TO 8006
 8014 Y0=Y1+((Y2-Y1)*(XX-X1)/(X2-X1))
	IF (J.EQ.1.AND.(X2-X1).LT.0.0) GO TO 8011
	IF (J.EQ.M.AND.(X2-X1).GT.0.0) GO TO 8011
	IF (YY.GT.Y0) IERR=1
	IF (YY.GT.Y0) GO TO 1240
	GO TO 8006
 8011 CONTINUE
	IF (YY.LT.Y0) IERR=1
	IF (YY.LT.Y0) GO TO 1240
 8006 CONTINUE
	IF (J.EQ.M)GO TO 8035
	J=M
	GO TO 8007
 8035 CONTINUE
C
C  NOW, WE DEFINITELY KNOW (XX,YY) IS INSIDE THE GRID OR ON BOUNDARY
C
C  LOOK THROUGH ENTIRE GRID FOR CLOSEST GRID POINT TO POINT(XX,YY)
C  SEARCH USING LEAST SQUARE DISTANCE IN LINEAR 2D DEGREE SPACE
	DMIN=1000.0
	PI=3.141592
	XX=-XX
	DO 1207 I=1,N
	DO 1208 J=1,M
	SLON(I,J)=-ALON(I,J)
	X2=SLON(I,J)
	Y2=ALAT(I,J)
	DIST=SQRT(((X2-XX)*(X2-XX))+((Y2-YY)*(Y2-YY)))
	IF (DIST.LT.DMIN) NLAT=I
	IF (DIST.LT.DMIN) MLON=J
	IF (DIST.LT.DMIN) DMIN=DIST
 1208 CONTINUE
 1207 CONTINUE
C
C  NLAT,MLON ARE THE I,J COORDINATES FOR THE NEAREST GRID POINT
C
C  SET UP LAT AND LONG OF 4 SURROUNDING GRID POINTS
C
C
C       P2
C       .
C       .
C       .
C  P3...P0...P1
C       .
C       .
C       .
C       P4
C
C
C
C  COMPUTATIONS ARE NOW DONE IN THIS 4-QUADRANT SYSTEM
C  ON BOUNDARY AVOID POINTS OUTSIDE OF GRID (SET THEM = CLOSEST GRID PT)
	C=1000.0
	P0X=SLON(NLAT,MLON)
	P0Y=ALAT(NLAT,MLON)
	P1X=P0X
	P2X=P0X
	P3X=P0X
	P4X=P0X
	P1Y=P0Y
	P2Y=P0Y
	P3Y=P0Y
	P4Y=P0Y
	IF (MLON.LT.M) P1X=SLON(NLAT,MLON+1)
	IF (MLON.LT.M) P1Y=ALAT(NLAT,MLON+1)
	IF (NLAT.GT.1) P2X=SLON(NLAT-1,MLON)
	IF (NLAT.GT.1) P2Y=ALAT(NLAT-1,MLON)
	IF (MLON.GT.1) P3X=SLON(NLAT,MLON-1)
	IF (MLON.GT.1) P3Y=ALAT(NLAT,MLON-1)
	IF (NLAT.LT.N) P4X=SLON(NLAT+1,MLON)
	IF (NLAT.LT.N) P4Y=ALAT(NLAT+1,MLON)
C  SPHERICAL GEOMETRY OF THE GLOBE
C    DICTATES THAT CONVERGING LONGITUDES MUST BE
C      MULTIPLIED BY COS(LAT) TO PRESERVE EQUIVALENT COORDINATE SCALES
	XC=COS(ALAT(NLAT,MLON)*PI/180.0)
	DX0=(XX-P0X)*XC
	DX1=(P1X-P0X)*XC
	DX2=(P2X-P0X)*XC
	DX3=(P3X-P0X)*XC
	DX4=(P4X-P0X)*XC
	DY0=YY-P0Y
	DY1=P1Y-P0Y
	DY2=P2Y-P0Y
	DY3=P3Y-P0Y
	DY4=P4Y-P0Y
	S0=DX0*DY0
	S1=DX1*DY1
	S2=DX2*DY2
	S3=DX3*DY3
	S4=DX4*DY4
C
C  COMPUTE ANGLE (AP) BETWEEN LINE (XX,YY)-(MLON,NLAT) AND THE +X AXIS
C  AND AVOID INFINITE TANGENTS
	AP=0.0
	AE=0.0
	AN=0.0
	AW=0.0
	AS=0.0
	IF (ABS(DX0).LE.(1.0/C)) AP=ATAN(SIGN(C,S0))
	IF (ABS(DX0).GT.(1.0/C)) AP=ATAN((DY0)/DX0)
	CALL RADCOR(DX0,DY0,ADD)
	AP=AP+ADD
C
C  COMPUTE THE ANGLE EACH OF THE 4 COORDINATES MAKES WITH THE +X AXIS
C  AE IS THE ANGLE THE EAST LINE (P1-P0) MAKES WITH THE +X AXIS
	IF (ABS(DX1).LE.(1.0/C)) AE=ATAN(SIGN(C,S1))
	IF (ABS(DX1).GT.(1.0/C)) AE=ATAN((DY1)/DX1)
	CALL RADCOR(DX1,DY1,ADD)
	AE=AE+ADD
C  AN IS THE ANGLE THE NORTH LINE (P2-P0) MAKES WITH THE +X AXIS
	IF (ABS(DX2).LE.(1.0/C)) AN=ATAN(SIGN(C,S2))
	IF (ABS(DX2).GT.(1.0/C)) AN=ATAN((DY2)/DX2)
	CALL RADCOR(DX2,DY2,ADD)
	AN=AN+ADD
C  AW IS THE ANGLE THE WEST LINE (P3-P0) MAKES WITH THE +X AXIS
	IF (ABS(DX3).LE.(1.0/C)) AW=ATAN(SIGN(C,S3))
	IF (ABS(DX3).GT.(1.0/C)) AW=ATAN((DY3)/DX3)
	CALL RADCOR(DX3,DY3,ADD)
	AW=AW+ADD
C  AS IS THE ANGLE THE SOUTH LINE (P4-P0) MAKES WITH THE +X AXIS
	IF (ABS(DX4).LE.(1.0/C)) AS=ATAN(SIGN(C,S4))
	IF (ABS(DX4).GT.(1.0/C)) AS=ATAN((DY4)/DX4)
	CALL RADCOR(DX4,DY4,ADD)
	AS=AS+ADD
C
C  COMPUTE LENGTH TO POINT(XX,YY)
	D0=SQRT((DX0*DX0)+(DY0*DY0))
	D1=SQRT((DX1*DX1)+(DY1*DY1))
	D2=SQRT((DX2*DX2)+(DY2*DY2))
	D3=SQRT((DX3*DX3)+(DY3*DY3))
	D4=SQRT((DX4*DX4)+(DY4*DY4))
C
C  COMPUTE (ANGLE) THE ANGLE FROM +X AXIS OF ROTATED GRID
C  AE IS THE BASE LINE USED FOR COMPUTING + ANGLES BECAUSE
C  IT CORRESPONDS TO THE +X AXIS OF THE ROTATED GRIDS
C  WARNING:  THE TRIGONOMETRY ASSUMES RIGHT ANGLES WITHIN THE GRID--
C             BUT TESTS INDICATE THIS ROUTINE PERFORMES WELL
C            FOR RELATIVELY SMALL DEVIATIONS (RT. ANGLE +- 10%)
	IF (MLON.EQ.M.AND.AW.LT.PI) AE=AW+PI
	IF (AP.LT.AE) ANGLE=(2.0*PI)+AP-AE
	IF (AP.GE.AE) ANGLE=AP-AE
C
C  COMPUTE FRACTIONAL I,J'S AWAY FROM GRID POINT
	IF (D0.EQ.0.0) XFRACT=0.0
	IF (D0.EQ.0.0) YFRACT=0.0
	IF (D0.EQ.0.0) GO TO 4198
	DD=D1
	IF (D1.EQ.0.0) DD=D3
	IF (DD.EQ.0.0)  XFRACT=0.0
	IF (DD.EQ.0.0)  GO TO 603
	XFRACT=(D0/DD)*COS(ANGLE)
  603 DD=D2
	IF (D2.EQ.0.0) DD=D4
	IF (DD.EQ.0.0)  YFRACT=0.0
	IF (DD.EQ.0.0)  GO TO 4198
	YFRACT=-(D0/DD)*SIN(ANGLE)
 4198 XXX=XFRACT + FLOAT(MLON)
 4199 YYY=YFRACT + FLOAT(NLAT)
C  6-1-93 TEST
	IF (XXX.LT.1.0) IERR=1
	IF (YYY.LT.1.0) IERR=1
	IF (XXX.GT.M) IERR=1
	IF (YYY.GT.N) IERR=1
C  6-1-93 TEST
	IF (XXX.LT.1.0) XXX=1.0
	IF (YYY.LT.1.0) YYY=1.0
	IF (XXX.GT.M) XXX=M
	IF (YYY.GT.N) YYY=N
C
C  CONVERT FROM I,J GRID VALUES TO I,J PLOT INCHES
	XX=DXX*(XXX-1.0)
C  YYY IS IN GRID UNITS
C  DYY IS THE NUMBER OF INCHES PER GRID LENGTH

	YY=YYLEN-(DYY*(YYY-1.0))
C
 1999 FORMAT(' IN CONVRTO XX=',4F7.3,' YY=',4F7.3,2I4)
 9999 CONTINUE
	RETURN
C
C
C
	END
C___________________________________________________________
	SUBROUTINE RADCOR(X,Y,ADD)
C
C  COMPUTES THE CORRECTION IN RADIANS AS A FUNCTION OF QUADRANT
C  "ADD" IS THE NUMBER WHICH MUST BE ADDEDED TO ARCTANGENT
C        COMPUTED ANGLES TO GIVE THE TOTAL ANGLE COUNTERCLOCKWISE
C        FROM THE +X AXIS
C
C  BE CAREFUL THAT X==ZERO IS +NUMBER
	IF(X.EQ.0.0)X=1.0E-15
C
	IF (X.GE.0.0.AND.Y.GE.0.0) IQ=1
	IF (X.LT.0.0.AND.Y.GE.0.0) IQ=2
	IF (X.LE.0.0.AND.Y.LT.0.0) IQ=3
	IF (X.GT.0.0.AND.Y.LT.0.0) IQ=4
	PI=3.141592
	IF (IQ.EQ.1) ADD=0.0
	IF (IQ.EQ.2.OR.IQ.EQ.3) ADD=PI
	IF (IQ.EQ.4) ADD=2.0*PI
	RETURN
	END
	SUBROUTINE SOCKIR(SKCMPS,SKDEGT,GL,AUU,AVV)
C  -----------------------------------------------------
C  SOCKIR=SPEED DEFINED, DIRECTION=RANDOM
C----------------------
	GRDANG=170.0-GL
C  DEFINE SWIMMING SPEED OF SOCKEYE TOWARD FRAZIER RIVER
C  1KT=51 CM/SEC
	SOCKSP=51.0
	SOCKSP=26.7
	IF(SKCMPS.NE.0.0)SOCKSP=SKCMPS
C  --------------------
C  DEFINE COMPAS SWIMMING ANGLE OF SOCKEYE TOWARD FRAZIER RIVER
C  IN DEGREES TRUE
	SOCKAN=95
C     SOCKAN=90
	SOCKAN=105.0
C     SOCKAN=120.0
        SOCKAN=360.0*RAND(0)
	IF(SKDEGT.NE.0.0)SOCKAN=SKDEGT
C  --------------------
C  COMPUTE SPEED RELATIVE TO GRID X,Y COMPONENTS
	SOCKAN=SOCKAN-90.0
	AUU=AUU+SOCKSP*(COS((GRDANG-SOCKAN)*3.14159/180.0))
	AVV=AVV+SOCKSP*(SIN((GRDANG-SOCKAN)*3.14159/180.0))
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++++++++++++++++++++++++++
C  PATCH TO CHANGE CONVRT TO FUNCTION
C++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE LTLNIJ20(XLAT,XLON,XI,XJ)
	DATA RED/31.205/,POLEI/31.0/,POLEJ/31.0/
	DATA DEGRAD/0.01745329/
C-----------------------------------------------------------------
C  PROGRAM
C  LAT LON TO FNWC 63X63 GRID UNITS
C-----------------------------------------------------------------
	COSLAT=COS(XLAT*DEGRAD)
	SINLAT=SIN(XLAT*DEGRAD)
	COSLON=COS((350.-XLON)*DEGRAD)
	SINLON=SIN((350.-XLON)*DEGRAD)
	XI=POLEI+RED*(COSLAT*COSLON/(1.+SINLAT))
	XJ=POLEJ+RED*(COSLAT*SINLON/(1.+SINLAT))
C  CONVERT FROM 63X63 (I=7TO26 UP, J=9TO52 LEFT) TO 20X44 (I=1TO20 DN,
C     J=1,44 RIGHT)
	XI=27.0-XI
	XJ=53.0-XJ
	RETURN
	END
	SUBROUTINE LTLNIJ66(YI2044,XJ2044,YI66160,XJ66160)
C-----------------------------------------------------------------
C  PROGRAM
C  CONVERT FROM FNOC 63X63 GRID UNITS TO OSCURS 66X160 GRID UNITS
C-----------------------------------------------------------------
C
C  INFO ON FRACTIONAL VALUES OF 66X160 GRID WITHIN 20X44 GRID
C  MAKE NEW GRID, EXPAND 1/4 FNWC TO WITHIN 1.25 OF BOUNDARY ALL AROUND
C  GRID: X=2.25 TO 42.25 (160); Y=2.25 TO 18.50 (66)(OF THE 20X44 GRID)
C      YI=2.25+(FLOAT(I-1)*0.25)
C      XJ=2.25+(FLOAT(J-1)*0.25)
C
C  OLD      YI66160=(-YI2044+25.0)/0.25
C  OLD      XJ66160=(-XJ2044+51.0)/0.25
C-----------------------------------------------------------------
C  PROGRAM
C  CONVERT FROM FNOC 20X44 GRID UNITS TO OSCURS 66X160 GRID UNITS
C-----------------------------------------------------------------
	YI66160=1.00+((YI2044-2.25)*4.00)
	XJ66160=1.00+((XJ2044-2.25)*4.00)
	RETURN
	END
C*****************************************************
C*                                                   *
C*          SUBROUTINE CONVRT(XX,YY,IERR)            *
C*  CONVERTS XX,YY FROM LON,LAT TO GRID (I,J) UNITS  *
C*  THEN CONVERTS FROM GRID UNITS TO PLOT INCHES     *
C*  IF (XX,YY) OUT OF GRID AREA, IERR=1              *
C*          NEW (7-7-94)                             *
C*****************************************************
	SUBROUTINE CONVRT (XX,YY,IERR)
	COMMON/GRDCOM/CFACT,M,N,NM,XI(181),YJ(93)
	COMMON/SIZES/XLEN,YLEN,YYLEN,SIZ1H,SIZ1V,SIZ2H,SIZ2V,
     &SIZ3H,SIZ3V,SIZ4H,SIZ4V
	COMMON/LATLON/ALAT(92,180),ALON(92,180)
	COMMON/NZMZ/NZ1,NZ2,MZ1,MZ2
	DIMENSION SLON(92,180)
	IF (XX.LT.0.0) XX=-XX
C
C
C  GIVEN A RANDOM POINT ON THE GLOBE (LONGITUDE,LATITUDE) DEGREE-DECIMAL
C  FIND THE (I,J) COORDINATES OF THE RANDOM POINT (XX,YY)
C  RELATIVE TO THE FNOC 63X63 GRID MESH
C  REJECT POINTS OUTSIDE OF THE GRID
C  FOR THE GIVEN (NXM) GRID ----- N=ROW#, M=COL# -----
C  XLEN IS THE DESIRED LENGTH OF THE X-AXIS IN INCHES
C  YLEN IS THE DESIRED LENGTH OF THE Y-AXIS IN INCHES
C
C
	N1=N-1
	M1=M-1
	AN1=N1
	AM1=M1
	DXX=XLEN/AM1
	DYY=YYLEN/AN1
C
C  REJECT MOST POINTS OUT OF GRID EXTREMES
	IERR=0
	XX1=XX
	YY1=YY
C
C  FIND MAX AND MIN LAT AND LONG OF GRID
C  LOOK AROUND BOUNDARIES ONLY
	AMAXLA=0.0
	AMAXLO=0.0
	AMINLA=1000.0
	AMINLO=1000.0
	DO 1201 I=1,N
	J=1
	IF (ALAT(I,J).GT.AMAXLA) AMAXLA=ALAT(I,J)
	IF (ALON(I,J).GT.AMAXLO) AMAXLO=ALON(I,J)
	IF (ALAT(I,J).LT.AMINLA) AMINLA=ALAT(I,J)
	IF (ALON(I,J).LT.AMINLO) AMINLO=ALON(I,J)
	J=M
	IF (ALAT(I,J).GT.AMAXLA) AMAXLA=ALAT(I,J)
	IF (ALON(I,J).GT.AMAXLO) AMAXLO=ALON(I,J)
	IF (ALAT(I,J).LT.AMINLA) AMINLA=ALAT(I,J)
	IF (ALON(I,J).LT.AMINLO) AMINLO=ALON(I,J)
 1201 CONTINUE
	DO 1202 J=1,M
	I=1
	IF (ALAT(I,J).GT.AMAXLA) AMAXLA=ALAT(I,J)
	IF (ALON(I,J).GT.AMAXLO) AMAXLO=ALON(I,J)
	IF (ALAT(I,J).LT.AMINLA) AMINLA=ALAT(I,J)
	IF (ALON(I,J).LT.AMINLO) AMINLO=ALON(I,J)
	I=N
	IF (ALAT(I,J).GT.AMAXLA) AMAXLA=ALAT(I,J)
	IF (ALON(I,J).GT.AMAXLO) AMAXLO=ALON(I,J)
	IF (ALAT(I,J).LT.AMINLA) AMINLA=ALAT(I,J)
	IF (ALON(I,J).LT.AMINLO) AMINLO=ALON(I,J)
 1202 CONTINUE
C
C
C  REJECT POINT IF OUT OF GRID MAX AND MIN LAT OR LONG
	IF (YY.GT.AMAXLA) IERR=1
	IF (YY.LT.AMINLA) IERR=1
	IF (XX.GT.AMAXLO) IERR=1
	IF (XX.LT.AMINLO) IERR=1
C  JUMP TO ERROR IF POINT IS OUTSIDE OF SQUARE OF BOUNDARY EXTREMES
C  AND SET BOTH COORDINATES TO MAXIMUM PLOT INCHES BEFORE RETURN
c	IF (IERR.EQ.1)WRITE(9,993)IERR,XX,YY
  993 FORMAT(' MAX-MIN IERR=',I2,'  XX=',F10.4,'  YY=',F10.4)
 1240 IF (IERR.EQ.1.AND.XX.GT.XLEN) XX=XLEN
	IF (IERR.EQ.1.AND.YY.GT.YYLEN) YY=YYLEN
c	IF (IERR.EQ.1)WRITE(9,994)IERR,XX1,XX,YY1,YY
  994 FORMAT(' CONVRT IERR=',I2,'  XX=',2F12.4,'  YY=',2F12.4)
	IF (IERR.EQ.1)  GO TO 9999
C
	ZLAT=YY
	ZLON=XX
	CALL LTLNIJ20(ZLAT,ZLON,FI20,FJ20)
	CALL LTLNIJ66(FI20,FJ20,ZI66,ZJ66)
C  ADD CONVERSION FROM 66X160 TO 92X180 GRID
	ZI66=ZI66+13
	ZJ66=ZJ66+17
C  REJECT OUT OF GRID
	ZNZ1=NZ1
	ZNZ2=NZ2
	ZMZ1=MZ1
	ZMZ2=MZ2
	IF(ZI66.LT.ZNZ1)IERR=1
	IF(ZI66.GT.ZNZ2)IERR=1
	IF(ZJ66.LT.ZMZ1)IERR=1
	IF(ZJ66.GT.ZMZ2)IERR=1
c	IF (IERR.EQ.1)WRITE(9,99994)IERR,ZI66,ZNZ1,ZNZ2,ZJ66,ZMZ1,ZMZ2
99994 FORMAT(' CONVRT IERR=',I2,'  ZI=',3F9.4,'  ZJ=',3F9.4)
	IF (IERR.EQ.1)  GO TO 9999
C  OSCURS FULL OR SUBSET GRID UNITS (XXX,YYY)
	XXX=ZJ66-ZMZ1+1.0
	YYY=ZI66-ZNZ1+1.0
C
C  CONVERT FROM I,J GRID VALUES TO I,J PLOT INCHES
	XX=DXX*(XXX-1.0)
C  DYY IS THE NUMBER OF INCHES PER GRID LENGTH
	YY=YYLEN-(DYY*(YYY-1.0))
C
C
 1999 FORMAT(' IN CONVRT XX=',4F7.3,' YY=',4F7.3,I3,I4,I2)
 9999 CONTINUE
	RETURN
C
C
C
	END
C*****************************************************
C*                                                   *
C*          SUBROUTINE CONVIJ(XX,YY,IERR)            *
C*  CONVERTS XX,YY FROM LON,LAT TO GRID (I,J) UNITS  *
C*  IF (XX,YY) OUT OF GRID AREA, IERR=1              *
C*          NEW (7-7-94)                             *
C*                                                   *
C*****************************************************
	SUBROUTINE CONVIJ (XX,YY,IERR)
	COMMON/GRDCOM/CFACT,M,N,NM,XI(181),YJ(93)
	COMMON/SIZES/XLEN,YLEN,YYLEN,SIZ1H,SIZ1V,SIZ2H,SIZ2V,
     &SIZ3H,SIZ3V,SIZ4H,SIZ4V
	COMMON/LATLON/ALAT(92,180),ALON(92,180)
	COMMON/NZMZ/NZ1,NZ2,MZ1,MZ2
	DIMENSION SLON(92,180)
	IF (XX.LT.0.0) XX=-XX
C
C
C  GIVEN A RANDOM POINT ON THE GLOBE (LONGITUDE,LATITUDE) DEGREE-DECIMAL
C  FIND THE (I,J) COORDINATES OF THE RANDOM POINT (XX,YY)
C  RELATIVE TO THE FNOC 63X63 GRID MESH
C  REJECT POINTS OUTSIDE OF THE GRID
C  FOR THE GIVEN (NXM) GRID ----- N=ROW#, M=COL# -----
C  XLEN IS THE DESIRED LENGTH OF THE X-AXIS IN INCHES
C  YLEN IS THE DESIRED LENGTH OF THE Y-AXIS IN INCHES
C
C
	N1=N-1
	M1=M-1
	AN1=N1
	AM1=M1
	DXX=XLEN/AM1
	DYY=YYLEN/AN1
C
C  REJECT MOST POINTS OUT OF GRID EXTREMES
	IERR=0
C
C  FIND MAX AND MIN LAT AND LONG OF GRID
C  LOOK AROUND BOUNDARIES ONLY
	AMAXLA=0.0
	AMAXLO=0.0
	AMINLA=1000.0
	AMINLO=1000.0
	DO 1201 I=1,N
	J=1
	IF (ALAT(I,J).GT.AMAXLA) AMAXLA=ALAT(I,J)
	IF (ALON(I,J).GT.AMAXLO) AMAXLO=ALON(I,J)
	IF (ALAT(I,J).LT.AMINLA) AMINLA=ALAT(I,J)
	IF (ALON(I,J).LT.AMINLO) AMINLO=ALON(I,J)
	J=M
	IF (ALAT(I,J).GT.AMAXLA) AMAXLA=ALAT(I,J)
	IF (ALON(I,J).GT.AMAXLO) AMAXLO=ALON(I,J)
	IF (ALAT(I,J).LT.AMINLA) AMINLA=ALAT(I,J)
	IF (ALON(I,J).LT.AMINLO) AMINLO=ALON(I,J)
 1201 CONTINUE
	DO 1202 J=1,M
	I=1
	IF (ALAT(I,J).GT.AMAXLA) AMAXLA=ALAT(I,J)
	IF (ALON(I,J).GT.AMAXLO) AMAXLO=ALON(I,J)
	IF (ALAT(I,J).LT.AMINLA) AMINLA=ALAT(I,J)
	IF (ALON(I,J).LT.AMINLO) AMINLO=ALON(I,J)
	I=N
	IF (ALAT(I,J).GT.AMAXLA) AMAXLA=ALAT(I,J)
	IF (ALON(I,J).GT.AMAXLO) AMAXLO=ALON(I,J)
	IF (ALAT(I,J).LT.AMINLA) AMINLA=ALAT(I,J)
	IF (ALON(I,J).LT.AMINLO) AMINLO=ALON(I,J)
 1202 CONTINUE
C
C
C  REJECT POINT IF OUT OF GRID MAX AND MIN LAT OR LONG
	IF (YY.GT.AMAXLA) IERR=1
	IF (YY.LT.AMINLA) IERR=1
	IF (XX.GT.AMAXLO) IERR=1
	IF (XX.LT.AMINLO) IERR=1
C  JUMP TO ERROR IF POINT IS OUTSIDE OF SQUARE OF BOUNDARY EXTREMES
C  AND SET BOTH COORDINATES TO MAXIMUM PLOT INCHES BEFORE RETURN
c	IF (IERR.EQ.1)WRITE(6,993)IERR,XX,YY
c	IF (IERR.EQ.1)WRITE(6,99993)AMAXLA,AMINLA,AMAXLO,AMINLO
c	IF (IERR.EQ.1)WRITE(9,993)IERR,XX,YY
c	IF (IERR.EQ.1)WRITE(9,99993)AMAXLA,AMINLA,AMAXLO,AMINLO
  993 FORMAT(' MAX-MIN IERR=',I2,'  XX=',F10.4,'  YY=',F10.4)
99993 FORMAT('    IERR     ',4F10.4)
 1240 IF (IERR.EQ.1.AND.XX.GT.XLEN) XX=XLEN
	IF (IERR.EQ.1.AND.YY.GT.YYLEN) YY=YYLEN
C      IF (IERR.EQ.1)WRITE(6,994)IERR,XX,YY
  994 FORMAT(' BOUNDARY IERR=',I2,'  XX=',F10.4,'  YY=',F10.4)
	IF (IERR.EQ.1)  GO TO 9999
C
	ZLAT=YY
	ZLON=XX
	CALL LTLNIJ20(ZLAT,ZLON,FI20,FJ20)
	CALL LTLNIJ66(FI20,FJ20,ZI66,ZJ66)
C  ADD CONVERSION FROM 66X160 TO 92X180 GRID
	ZI66=ZI66+13
	ZJ66=ZJ66+17
C  REJECT OUT OF GRID
	ZNZ1=NZ1
	ZNZ2=NZ2
	ZMZ1=MZ1
	ZMZ2=MZ2
	IF(ZI66.LT.ZNZ1)IERR=1
	IF(ZI66.GT.ZNZ2)IERR=1
	IF(ZJ66.LT.ZMZ1)IERR=1
	IF(ZJ66.GT.ZMZ2)IERR=1
	IF (IERR.EQ.1)  GO TO 9999
C  OSCURS FULL OR SUBSET GRID UNITS (XXX,YYY)
	XXX=ZJ66-ZMZ1+1.0
	YYY=ZI66-ZNZ1+1.0
C
c	WRITE(9,1999)XX,DXX,XXX,XLEN,YY,DYY,YYY,YLEN,N,M
 1999 FORMAT(' IN CONVIJ XX=',4F7.3,' YY=',4F7.3,2I4)
C
	XX=XXX
	YY=YYY
 9999 CONTINUE
	RETURN
	END
	SUBROUTINE GRCIRC(X1,Y1,X2,Y2,C1,D1,VLA,VLO)                      
C-------------------------------------------------------------------    
C                                                                       
C                                                                       
C  COMPUTES PARAMETERS FOR THE GREAT CIRCLE BETWEEN ANY INPUT POINTS    
C  (X1,Y1) AND (X2,Y2)  --  USE LONGITUDE-LATITUDE DEGREES-DECIMAL      
C                                                                       
C                                                                       
C  C1=INITIAL COURSE HEADING OF THE GREAT CIRCLE AT (X1,Y1)             
C     RELATIVE TO THE NORTH POLE (+ TO THE RIGHT, - TO THE LEFT)        
C  D1=GREAT CIRCLE DISTANCE(DEGREES) BETWEEN POINTS (X1,Y1) AND (X2,Y2) 
C  VLA=VERTEX(HIGHEST LATITUDE) ON THE GREAT CIRCLE                     
C  VLO=DIFFERENCE IN LONGITUDE BETWEEN (X1,Y1) AND THE VERTEX           
C      (+ TO THE RIGHT, - TO THE LEFT)                                  
C                                                                       
C                                                                       
C-------------------------------------------------------------------    
	PI=3.1415927                                                   
	RAD=PI/180.0                                                  
	X12=(X1-X2)                                                  
	AX12=ABS(X12)                                               
	Y12=Y2-Y1                                                  
	AY12=ABS(Y12)                                             
	YAV=(Y1+Y2)/2.0                                          
	IF (X12.EQ.0) TANS=300000.0                             
	IF(X12.EQ.0.AND.Y12.EQ.0)GO TO 299
	IF (X12.EQ.0) GO TO 91                                      
	TANS=Y12/(X12*COS(YAV*RAD))                                
   91 CONTINUE                                                    
	ATANS=ABS(TANS)                                          
C                                                                       
C  SKIP COMPUTATIONS FOR NEAR VERTICAL OR HORIZONTAL GREAT CIRCLES      
	IF (ATANS.LT.0.05) GO TO 98                               
	IF (ATANS.GT.10.0) GO TO 99                              
C                                                               
	TAN2=TAN(Y2*RAD)                                       
	IF (TAN2.LT.0.000001) TANPHI=30000.0                  
	IF (TAN2.LT.0.000001) GO TO 5                        
	TANPHI=COS(X12*RAD)/TAN2                            
    5 PHI=ATAN(TANPHI)                                     
	IF (PHI.LT.0.000001) TAN3=30000.0                 
	IF (PHI.LT.0.000001) GO TO 6                     
	TAN3=TAN(X12*RAD)                               
    6 IF (TAN3.LT.0.000001.AND.TAN3.GT.-0.000001) COTC1=30000.0         
	IF (TAN3.LT.0.000001.AND.TAN3.GT.-0.000001) GO TO 7             
	IF (PHI.LT.0.000001) COTC1=30000.0                             
	IF (PHI.LT.0.000001) GO TO 7                                  
	YP=(Y1*RAD)+PHI                                              
	COTC1=(COS(YP)/SIN(PHI))/TAN3                               
	IF (COTC1.EQ.0.0) CCOTC1=30000.0                           
	IF (COTC1.EQ.0.0) GO TO 7                                 
	CCOTC1=1.0/COTC1                                         
    7 CONTINUE                                                  
C  COMPUTE C1                                                  
	C1=ATAN(CCOTC1)                                       
      IF (YP.GT.(PI/2.0)) C1=((PI/2.0) + ((PI/2.0)-ABS(C1)))*SIGN(1.0,X1
     '2)                                                                
	COTD1=COS(C1)*TAN(YP)                                
	IF (COTD1.EQ.0.0) CCOTD1=30000.0                    
	IF (COTD1.EQ.0.0) GO TO 88                         
	CCOTD1=1.0/COTD1                                  
   88 CONTINUE                                           
C  COMPUTE D1                                                           
	D1=ATAN(CCOTD1)/RAD                             
	COSVLA=SIN(C1)*COS(Y1*RAD)                     
C  COMPUTE VLA                                                          
C      VERTEX IS THE POINT OF HIGHEST LATITUDE                          
	VLA=ACOS(COSVLA)/RAD                          
	IF (VLA.GT.90.0) VLA=90.0-(VLA-90.0)         
	COTVLO=TAN(C1)*SIN(Y1*RAD)                  
	IF (COTVLO.EQ.0.0) CCTVLO=30000.0          
	IF (COTVLO.EQ.0.0) GO TO 89               
	CCTVLO=1.0/COTVLO                        
   89 CONTINUE                                                          
C  COMPUTE VLO                                                          
	VLO=ATAN(CCTVLO)/RAD                    
	C1=C1/RAD                              
  101 FORMAT (9F14.4)                                                   
	RETURN                                                         
C                                                                       
C  SETTINGS FOR NEAR VERTICAL GREAT CIRCLES                             
   99 CONTINUE                                                          
	C1=0.0                                                        
	IF (Y12.LT.0.0) C1=180.0*SIGN(1.0,X12)                       
	VLA=90.0                                                    
	VLO=90.0*SIGN(1.0,X12)                                     
	D1=AY12                                                   
	RETURN                                                   
C                                                               
C  SETTINGS FOR NEAR HORIZONTAL GREAT CIRCLES                           
   98 CONTINUE                                                          
	C1=90.0*SIGN(1.0,X12)                                  
	VLA=Y2                                                
	VLO=X12                                              
	D1=AX12*COS(Y2*RAD)                                 
	RETURN                                             
C
C  SPECIAL CASE FOR TWO POINTS ARE THE SAME
  299 CONTINUE
	C1=0.0
	VLA=Y2
	VLO=X12
	D1=0.000000000
	RETURN
	END
