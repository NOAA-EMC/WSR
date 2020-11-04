SHELL=	/bin/sh
#
CMD = wsr_summ_allnorms
#
#FC =    ncepxlf -C 
FC =    mpxlf_r
SRCS=	summ_allnorms.f subroutines.f eigen.f
OBJS=	summ_allnorms.o subroutines.o eigen.o
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
