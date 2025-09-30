FC = gfortran
FFLAGS = -O2 -Wall

TARGET = mpm2d

SRC = init_module.f90 \
      buildgrid.f90 \
      p_dist.f90 \
      spfunc.f90 \
      p2n.f90 \
      renewal.f90 \
      boundary.f90 \
      n2p.f90 \
      double_map.f90 \
      p_renew.f90 \
      gnu_out.f90 \
      write_vtk.f90 \
      main.f90 

OBJ = $(SRC:.f90=.o)

$(TARGET): $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ)

%.o: %.f90
	$(FC) $(FFLAGS) -c $<

clean:
	rm -f *.o *.mod $(TARGET)
