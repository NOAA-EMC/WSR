# envir-p1.h
export job=${job:-$LSB_JOBNAME} #Can't use $job in filenames!
export jobid=${jobid:-$job.$LSB_JOBID}

# export RUN_ENVIR=${RUN_ENVIR:-nco}
export RUN_ENVIR=${RUN_ENVIR:-dev}
#export envir=%ENVIR%
#export SENDDBN=${SENDDBN:-%SENDDBN:YES%}
export SENDDBN=${SENDDBN:-%SENDDBN:NO%}
#export SENDDBN_NTC=${SENDDBN_NTC:-%SENDDBN_NTC:YES%}
export SENDDBN_NTC=${SENDDBN_NTC:-%SENDDBN_NTC:NO%}

module load prod_envir prod_util

case $envir in
  prod)
    export jlogfile=${jlogfile:-${COMROOT}/logs/jlogfiles/jlogfile.${jobid}}
    export DATAROOT=${DATAROOT:-/tmpnwprd1}
    if [ "$SENDDBN" == "YES" ]; then
       export DBNROOT=/iodprod/dbnet_siphon  # previously set in .bash_profile
    else
       export DBNROOT=${UTILROOT}/fakedbn
    fi
    ;;
  eval)
    export envir=para
    export jlogfile=${jlogfile:-${COMROOT}/logs/${envir}/jlogfile}
    export DATAROOT=${DATAROOT:-/tmpnwprd2}
    if [ "$SENDDBN" == "YES" ]; then
       export DBNROOT=${UTILROOT}/para_dbn
       SENDDBN_NTC=NO
    else
       export DBNROOT=${UTILROOT}/fakedbn
    fi
    ;;
  para|test)
    export jlogfile=${jlogfile:-${COMROOT}/logs/${envir}/jlogfile}
    export DATAROOT=${DATAROOT:-/tmpnwprd2}
    export DBNROOT=${UTILROOT}/fakedbn
    ;;
  dev)
    export jlogfile=${jlogfile:-$localprefixptmp/com/logs/${envir}/jlogfile}
    export DATAROOT=${DATAROOT:-$localprefixptmp/tmpnwprd}
    export DBNROOT=${UTILROOT}/fakedbn
    ;;
  *)
    echo ecflow_client --abort="ENVIR must be prod, para, eval, or test [envir.h]"
    exit
    ;;
esac
echo in envir-p1.h
echo envir=$envir
echo DATAROOT=$DATAROOT

export NWROOT=/nw${envir}
export NWROOT=$localprefixsave/nw$envir
export COMROOT=/com
export COMROOT=$localprefixptmp/com
export PCOMROOT=/pcom/${envir}
#export SENDECF=${SENDECF:-YES}
export SENDECF=${SENDECF:-NO}
export SENDCOM=${SENDCOM:-YES}
#export KEEPDATA=${KEEPDATA:-%KEEPDATA:NO%}
export KEEPDATA=${KEEPDATA:-$KEEPDATA:YES}
export GESROOT=$localprefixptmp/nwges

  if (( 0 == 1 )); then
if [ -n "%PARATEST:%" ]; then export PARATEST=${PARATEST:-%PARATEST:%}; fi
if [ -n "%PDY:%" ]; then export PDY=${PDY:-%PDY:%}; fi
if [ -n "%COMPATH:%" ]; then export COMPATH=${COMPATH:-%COMPATH:%}; fi
if [ -n "%MAILTO:%" ]; then export MAILTO=${MAILTO:-%MAILTO:%}; fi
if [ -n "%DBNLOG:%" ]; then export DBNLOG=${DBNLOG:-%DBNLOG:%}; fi
  fi
