# envir-p3.h
export job=${job:-$LSB_JOBNAME} #Can't use $job in filenames!
export jobid=${jobid:-$job.$LSB_JOBID}
export RUN_ENVIR=${RUN_ENVIR:-nco}

  if (( 0 == 1 )); then
export envir=%ENVIR%
export SENDDBN=${SENDDBN:-%SENDDBN:YES%}
export SENDDBN_NTC=${SENDDBN_NTC:-%SENDDBN_NTC:YES%}
  else
export RUN_ENVIR=emc
  fi

  if (( 0 == 1 )); then
module load prod_envir/1.0.3 prod_util/1.1.3 ecflow/4.7.1
  else
module load prod_envir/1.0.3 prod_util/1.1.3
  fi

echo in head_p3.h envir=$envir
case $envir in
  prod)
    export jlogfile=${jlogfile:-${COMROOT}/logs/jlogfiles/jlogfile.${jobid}}
    export DATAROOT=${DATAROOT:-/gpfs/dell1/nco/ops/tmpnwprd}
    export COMROOT=%COM%
    export COMOUT_ROOT=$COMROOT
    export PCOMROOT=%COM%/pcom/prod

    if [ "$SENDDBN" == "YES" ]; then
       export DBNROOT=/iodprod/dbnet_siphon  # previously set in .bash_profile
    else
       export DBNROOT=${UTILROOT}/fakedbn
    fi
    ;;
  eval)
    export envir=para
    export jlogfile=${jlogfile:-${COMROOT}/logs/${envir}/jlogfile}
    export DATAROOT=${DATAROOT:-/gpfs/dell1/nco/ops/tmpnwprd}
    if [ "$SENDDBN" == "YES" ]; then
       export DBNROOT=${UTILROOT}/para_dbn
       SENDDBN_NTC=NO
    else
       export DBNROOT=${UTILROOT}/fakedbn
    fi
    ;;
  para|test)
    export jlogfile=${jlogfile:-${COMROOT}/logs/${envir}/jlogfile}
    export DATAROOT=${DATAROOT:-/gpfs/dell1/nco/ops/tmpnwprd}
    export DBNROOT=${UTILROOT}/fakedbn
    ;;
  dev)
    export jlogfile=$localprefixptmp/logs/${envir}/jlogfile
    mkdir -p $localprefixptmp/logs/${envir}
    touch $jlogfile
    export COMROOT=$localprefixptmp/com
    export DATAROOT=$localprefixptmp/tmpnwprd
    export DBNROOT=${UTILROOT}/fakedbn
    export GESROOT=$localprefixptmp/nwges
    ;;
  *)
    ecflow_client --abort="ENVIR must be prod, para, eval, or test [envir.h]"
    exit
    ;;
esac

export NWROOT=/gpfs/dell1/nco/ops/nw${envir}
export SENDECF=${SENDECF:-YES}
export SENDCOM=${SENDCOM:-YES}
export KEEPDATA=${KEEPDATA:-%KEEPDATA:NO%}
export ECF_PORT=%ECF_PORT%

  if (( 0 == 1 )); then
if [ -n "%PDY:%" ]; then export PDY=${PDY:-%PDY:%}; fi
if [ -n "%COMPATH:%" ]; then export COMPATH=${COMPATH:-%COMPATH:%}; fi
if [ -n "%MAILTO:%" ]; then export MAILTO=${MAILTO:-%MAILTO:%}; fi
if [ -n "%DBNLOG:%" ]; then export DBNLOG=${DBNLOG:-%DBNLOG:%}; fi
  fi
