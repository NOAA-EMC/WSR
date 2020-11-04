SHELL=	/bin/sh
#
CMD = wsr_sigvar_allnorms
#
FC =    ncepxlf -qmaxmem=-1
SRCS=	sigvar_allnorms.f subroutines.f interp.f eigen.f
OBJS=	sigvar_allnorms.o subroutines.o interp.o eigen.o
LIBS=/usr/lib/libblas.a

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:	$(CMD)

$(CMD):	$(OBJS)
	$(FC) -o $(@) $(OBJS) $(LIBS)

# Make the profiled version of the command and call it a.out.prof
#
clean:
	-rm -f $(OBJS)

clobber:        clean
	-rm -f $(CMD)

void:   clobber
	-rm -f $(SRCS) makefile
