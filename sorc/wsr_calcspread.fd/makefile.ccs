SHELL=	/bin/sh
#
CMD =   wsr_calcspread
#
FC =    xlf -O3 -qnostrict 
#FC =    xlf 
SRCS=	calcspread.f
OBJS=	calcspread.o
LIBS=   

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

