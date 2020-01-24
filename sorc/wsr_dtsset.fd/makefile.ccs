SHELL	=/bin/sh
INCMOD	= /global/save/wx20ys/tobs/grib_api_dir/include
INCM	=
SRCS	= dtsset.f90
SRCM	=
OBJS	=         
MODS	=
FC	= xlf90
FFLAGS	=-qnosave  -qmaxmem=-1
LDFLAGS	=   
LIBS    =  -L/global/save/wx20ys/tobs/grib_api_dir/lib -lgrib_api_f90 -lgrib_api
CMD	=wsr_dtsset
all:	$(CMD)
$(CMD):	$(OBJS) $(SRCS)
	$(FC) -I $(INCMOD) $(FFLAGS) $(LDFLAGS) $(OBJS) $(SRCS) $(LIBS) -o $(CMD)
clean:
	-rm -f $(OBJS) $(MODS)
clobber:        clean
	-rm -f $(CMD)
void:   clobber
	-rm -f $(SRCS) $(SRCM) $(INCM) makefile
