bytes=8

LIBS=	 /nwprod/w3lib90/w3lib_$(bytes) \
	 /nwprod/w3lib90/splib_$(bytes) \
         /nwprod/w3lib90/iplib_$(bytes) \
         /nwprod/w3lib90/bacio_$(bytes)
# OPTS= -qnosave -qarch=$(arch) -qrealsize=$(bytes) -qintsize=$(bytes) \
#  -bnoquiet -qfixed -qsmp=noauto -lessl
OPTS= -qnosave -qarch=auto -qrealsize=$(bytes) -qintsize=$(bytes) \
  -bnoquiet -qfixed -qsmp=noauto -lessl
wsr_ecmwfens:	ecmwfens.f $(LIBS)
	xlf_r $(OPTS) -o wsr_ecmwfens ecmwfens.f $(LIBS)
