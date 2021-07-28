#!/bin/sh
#PBS -N wsr_prep_prod_20201120
##PBS -o /lfs/h1/emc/ptmp/Xianwu.Xue/o/wsr_port2wcoss2/com/output/dev/20201120/wsr_prep_00.%J
#PBS -j oe
#PBS -l place=vscatter,select=1:ncpus=64
##PBS -R span[ptile=16]
##PBS -R 'affinity[core(1)]'
#PBS -q workq
#PBS -l walltime=3:20:00
##PBS -L /bin/sh
#PBS -A GEN-T2O

set -x
#module purge

date

export envir='dev'
export RUN_ENVIR='dev'
export WHERE_AM_I='wcoss2'
export EXPID='port2wcoss2_new2_cominout'
export GEFS_ROCOTO="/lfs/h1/emc/ens/noscrub/Xianwu.Xue/wsr/${EXPID}/rocoto"
export WORKDIR="/lfs/h1/emc/ptmp/Xianwu.Xue/o/${EXPID}"
export PDY='20201120'
export cyc='00'
export SOURCEDIR="/lfs/h1/emc/ens/noscrub/Xianwu.Xue/wsr/${EXPID}"
export job=wsr_prep_${EXPID}_${PDY}${cyc}


#set -x
ulimit -s unlimited
ulimit -a

# module_ver.h
. $SOURCEDIR/versions/wsr_wcoss2.ver

module purge
module load envvar/$envvar_ver
module load intel/$intel_ver PrgEnv-intel

module load craype/$craype_ver
module load cray-mpich/$cray_mpich_ver
module load cray-pals/$cray_pals_ver

module load prod_util/$prod_util_ver
module load prod_envir/$prod_envir_ver

module load grib_util/$grib_util_ver

module load cce/11.0.2
module load gcc/10.2.0
module load libjpeg/9c

#module load lsf/$lsf_ver

module load cfp/$cfp_ver
export USE_CFP=YES
module list

export KEEPDATA=YES

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

export COMINgens=/lfs/h1/emc/ens/noscrub/Xianwu.Xue/wsr/HOMEdata_WSR/com/gefs/prod
#export COMPATH=/lfs/h1/emc/ens/noscrub/Xianwu.Xue/wsr/HOMEdata_WSR/com/naefs/prod
export COMINcmce=/lfs/h1/emc/ens/noscrub/Xianwu.Xue/wsr/HOMEdata_WSR/com/naefs/prod
export DCOMROOT=/lfs/h1/emc/ens/noscrub/Xianwu.Xue/wsr/HOMEdata_WSR/dcom

# Export List
export MP_EUIDEVICE=sn_all
export MP_EUILIB=us
export MP_TASK_AFFINITY=core

export MP_PGMMODEL=mpmd
export MP_CSS_INTERRUPT=yes

export OMP_NUM_THREADS=1

#${GEFS_ROCOTO}/bin/wcoss2/wsr_prep.sh
$SOURCEDIR/jobs/JWSR_PREP
