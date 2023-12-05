include ../opt/option.txt

OBJDIR = objs

.SUFFIXES : .f90

TARGET = ../extractAreaFunction

SRC  =  variables.f90\
	readParam.f90\
	readSTL.f90\
	writeVTK.f90\
	writeVTK3.f90\
	planeCut.f90\
	iniPlane.f90\
	calcCOG.f90\
	rePlane.f90\
	bspline.f90\
	calcArea.f90\
	writeArea.f90\
	main.f90

OBJ  =  $(patsubst %.f90, $(OBJDIR)/%.o, $(SRC))


all: $(OBJ)  $(TARGET) 

$(TARGET): $(OBJ)
	$(FC) $(OPT) -o $@  $(OBJ)

$(OBJ): $(OBJDIR)/%.o: %.f90
	$(FC) $(OPT) -c $<  -o $@ 

clean:
	rm -f $(OBJDIR)/*.o *.mod 

