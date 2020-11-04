set -xe  # print commands as they are executed and enable signal trapping

  if (( 0 == 1 )); then
export ECF_NAME=%ECF_NAME%
export ECF_HOST=%ECF_LOGHOST%
export ECF_PORT=%ECF_PORT%
export ECF_PASS=%ECF_PASS%
export ECF_TRYNO=%ECF_TRYNO%
export ECF_RID=$LSB_JOBID
  fi

# Tell ecFlow we have started
# POST_OUT variable enables LSF to communicate with ecFlow
if [ -d /opt/modules ]; then
    # WCOSS TO4 (Cray XC40)
    . /opt/modules/default/init/sh
      if (( 0 == 1 )); then
    module load ecflow
    POST_OUT=/gpfs/hps/tmpfs/ecflow/ecflow_post_in.$LSB_BATCH_JID
      fi
elif [ -d /usrx/local/Modules ]; then
    # WCOSS Phase 1 & 2 (IBM iDataPlex)
    . /usrx/local/Modules/default/init/sh
      if (( 0 == 1 )); then
    module load ecflow
    POST_OUT=/var/lsf/ecflow_post_in.$LSB_BATCH_JID
      fi
else
    # WCOSS Phase 3 (Dell PowerEdge)
    . /usrx/local/prod/lmod/lmod/init/sh
    echo in head.h MODULEPATH=$MODULEPATH
    export MODULEPATH=/usrx/local/prod/lmod/lmod/modulefiles/Core:/usrx/local/prod/modulefiles/core_third:/usrx/local/prod/modulefiles/defs:/gpfs/dell1/nco/ops/nwprod/modulefiles/core_prod:/usrx/local/dev/modulefiles
      if (( 0 == 1 )); then
    module load ips/18.0.1.163 ecflow/4.7.1
    POST_OUT=/var/lsf/ecflow_post_in.$LSB_BATCH_JID
      else
    module load ips/18.0.1.163
      fi
fi

  if (( 0 == 1 )); then
ecflow_client --init=${ECF_RID}

cat > $POST_OUT <<ENDFILE
ECF_NAME=${ECF_NAME}
ECF_HOST=${ECF_HOST}
ECF_PORT=${ECF_PORT}
ECF_PASS=${ECF_PASS}
ECF_TRYNO=${ECF_TRYNO}
ECF_RID=${ECF_RID}
ENDFILE
  fi

  if (( 0 == 1 )); then
# Define error handler
ERROR() {
  set +ex
  if [ "$1" -eq 0 ]; then
     msg="Killed by signal (likely via bkill)"
  else
     msg="Killed by signal $1"
  fi
  ecflow_client --abort="$msg"
  echo $msg
  echo "Trap Caught" >>$POST_OUT
  trap $1; exit $1
}
# Trap all error and exit signals
trap 'ERROR $?' ERR EXIT
  fi

