#!/usr/bin/env bash

export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}

#export NTASKS=${GEFS_NTASKS}
export total_tasks=${GEFS_NTASKS}
export OMP_NUM_THREADS=${GEFS_TPP:-1}
export taskspernode=${GEFS_PPN}

# Calculate the number of tasks based on the task geometry
#(( NTASKS=$(echo $LSB_PJL_TASK_GEOMETRY | grep -Po "\d+" | sort -n | tail -1) + 1 ))
#export NTASKS

export gefsmpexec="mpiexec -n $total_tasks"
export gefsmpexec_mpmd="mpiexec -n $total_tasks cfp mpmd_cmdfile"
export wavempexec="mpiexec -n"
export wave_mpmd="cfp"

#export APRUNC="$gefsmpexec"
#export APRUN_RECENT="$gefsmpexec"
export APRUN_CHGRES="mpiexec -n 1"
#export aprun_gec00="mpirun -n 1"
export APRUN_CALCINC="mpiexec -n 1"

#export HOMEdata=/lfs/h2/emc/ens/noscrub/Xianwu.Xue/wsr/HOMEdata_WSR
export HOMEdata=/lfs/h1/ops

#export COMPATH=$HOMEdata/canned/com/gefs:$HOMEdata/canned/com/naefs:${WORKDIR}/$envir/com/${NET} #$COMROOT/$RUN
#export DCOMROOT=${HOMEdata}/canned/dcom

export COMPATH=${WORKDIR}/$envir/com/${NET}


. $GEFS_ROCOTO/parm/setbase
. $GEFS_ROCOTO/parm/gefs_config
#. $GEFS_ROCOTO/parm/gefs_dev.parm

export COMPATH=${WORKDIR}/$envir/com/${NET}
