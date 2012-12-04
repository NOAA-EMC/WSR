LIBS    = -L/nwprod/lib -lw3_8 -lsp_8 -lip_8 -lbacio_8
# OPTS= -qnosave -qarch=$(arch) -qrealsize=$(bytes) -qintsize=$(bytes) \
#  -bnoquiet -qfixed -qsmp=noauto -lessl
OPTS= -qnosave -qarch=$(arch) -qrealsize=8 -qintsize=8 \
  -bnoquiet -qfixed -qsmp=noauto -lessl
wsr_ecmwfens:	ecmwfens.f 
	ncepxlf $(OPTS) $(LIBS) -o wsr_ecmwfens ecmwfens.f
