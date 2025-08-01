#---Makefile for stock synthesis--------------------------------------
#-----PFEL 03-11-2001 (Jim Ingraham) for oscurs2 directory
#-----PFEL 12-21-2000 (Jim Ingraham) for oscurs directory
#---FC is name of Fortran Compiler
#FC = g77
#FC = f77
#FC = fort77 (default Macro on upwell)
FC = g77 
# ---FCFLAGS are flags to be set for the compile
# 	Note the -c flag is hard wired in

# ---Following should be uncommented for standalone operation 
#FCFLAGS =-C -r8 -dalign -Bstatic -cg89 -g 

FCFLAGS = -g -ffixed-line-length-132 -I/usr/local/include 

#FCFLAGS =-C -g 

#FCFLAGS =-C -r8 -dalign -Bstatic -cg92 -O4 
#FCFLAGS =-C -r8 -dalign -Bstatic -cg92 -g
#FCFLAGS =-C -r8 -dalign -Bstatic -cg89 -O3 
#FCFLAGS =-C -r8 -dalign -Bstatic -cg89 -g

# ---Following should be uncommented for library shared operation 
#FCFLAGS =-C -cg89 -g 

# ---Following flags increase the default sizes of dimensions sortof
#  	-Nc40 -Nd40 -Nl30 -Nn2000 -Ns800 -Nx400
#FCFLAGS = -C -u -v -xld -g -sb -f
#  -u -v -xld

# Libraries needed in the load process go here.
# There may be none, or perhaps Fortran or C Libraries
# This is very system specific

LIBS = -L/usr/local/lib -lnetcdf -lz -lm

#
# from Lynn DeWitt
#######f77 -o call_get_pres call_get_pres.f get_pressure.f setup_input.f -L/usr/local/lib -lnetcdf
#----------------------------------------------------------------------
# The user probably doesn't want to change anything below here

TARGET = os2


OBJ = os2.o 
 
CODE= os2.for

#          Set up suffixes for created implicit rules
.SUFFIXES:
.SUFFIXES:  .for .c .o  $(SUFFIXES)

#     Create relocatable load file


#          Rule for .for to .o
.for.o:
	   $(FC) $(FCFLAGS) -c $*.for

all:$(TARGET)

$(TARGET):$(OBJ) 
	   $(FC) $(FCFLAGS) -o $(TARGET) $(OBJ) $(LIBS)

clean:
	rm $(OBJ)

##########size:$(TARGET2)
##########$(TARGET2):$(OBJ2) 
##########	   $(FC) $(FCFLAGS) -o $(TARGET2) $(OBJ2) $(LIBS)



#FFLAGS = -g -C -c -ffixed-line-length-12

#.KEEP_STATE

#arowmk1.97: arowmk1.97.o  vllxy.o  f77.
#	g77 -ffixed-line-length-132 -o arowmk1.97 arowmk1.97.o  vllxy.o  f77.o  

#arowmk1.97.o: arowmk1.97.
#	g77 $(FFLAGS) arowmk1.97.

#vllxy.o: vllxy.
#	g77 $(FFLAGS) vllxy.

#f77.o: f77.
#	g77 $(FFLAGS) f77.

