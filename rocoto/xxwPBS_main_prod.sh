#!/bin/sh
#PBS -N wsr_main_20201120
##PBS -o /lfs/h1/emc/ptmp/Xianwu.Xue/o/wsr_port2wcoss2/com/output/dev/20201120/wsr_main_00.%J
#PBS -j oe
#PBS -l select=1:ncpus=48
##PBS -R span[ptile=16]
#PBS -q workq
#PBS -l walltime=0:45:00
##PBS -L /bin/sh
#PBS -A GEN-T2O

set -x
module purge


export envir='dev'
export RUN_ENVIR='dev'
export WHERE_AM_I='acorn'
export EXPID='port2wcoss2_new2_cominout'
export GEFS_ROCOTO="/lfs/h1/emc/ens/noscrub/Xianwu.Xue/wsr/${EXPID}/rocoto"
export WORKDIR="/lfs/h1/emc/ptmp/Xianwu.Xue/o/${EXPID}"
export PDY='20201120'
export cyc='00'
export SOURCEDIR="/lfs/h1/emc/ens/noscrub/Xianwu.Xue/wsr/${EXPID}"
export job=wsr_main_${EXPID}_${PDY}${cyc}

#set -x
ulimit -s unlimited
ulimit -a

# module_ver.h
. $SOURCEDIR/versions/wsr_acorn.ver

module purge
source /apps/prod/lmodules/startLmod
module load envvar/$envvar_ver
module load intel/$intel_ver PrgEnv-intel
module load intel/$intel_ver/cray-mpich/$mpich_ver
module load cray-pals/1.0.8

module load prod_util/$prod_util_ver
module load prod_envir/$prod_envir_ver

module list

KEEPDATA=YES

export RUN_ENVIR=${RUN_ENVIR:-dev}
export envir=${envir:-dev}
#export NET=${NET:-wsr}
#export RUN=${RUN:-wsr}
export SENDDBN=${SENDDBN:-NO}
export SENDDBN_NTC=${SENDDBN_NTC:-NO}

#export PARAFLAG=${PARAFLAG:-YES}
export SENDECF=${SENDECF:-NO}
export SENDCOM=${SENDCOM:-YES}
export KEEPDATA=${KEEPDATA:-NO}

# Enviorenmenat variables related to Development work place and output files
#
export basesource=$SOURCEDIR
export baseoutput=$WORKDIR

export HOMEwsr=$basesource
export COMROOT=$baseoutput/com
#export GESROOT=$baseoutput/nwges
export DATAROOT=$baseoutput/tmp

# Export List
#export MP_SHARED_MEMORY=yes
export MP_TASK_AFFINITY=core
export MP_EUIDEVICE=sn_all
export MP_EUILIB=us

#${GEFS_ROCOTO}/bin/acorn/wsr_main.sh
$SOURCEDIR/jobs/JWSR_MAIN
~                                                                                                                                                              

