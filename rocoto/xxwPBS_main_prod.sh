#PBS -N wsr_main_prod_2021021500
#PBS -j oe
#PBS -S /bin/bash
#PBS -q dev
#PBS -A GEFS-DEV
#PBS -l walltime=0:45:00
#PBS -l select=1:ncpus=48:mem=10GB
#PBS -l debug=true

set -x
#module purge


export envir='dev'
export RUN_ENVIR='dev'
export WHERE_AM_I='wcoss2'
export EXPID='wsr_wcoss2_final'
export SOURCEDIR="/lfs/h2/emc/ens/noscrub/$LOGNAME/wsr/${EXPID}"
export GEFS_ROCOTO="${SOURCEDIR}/rocoto"
export WORKDIR="/lfs/h2/emc/ptmp/$LOGNAME/o/${EXPID}"

export PDY='20210215' #'20201120' #'20210215'
export cyc='00'
export job=wsr_main_${EXPID}_${PDY}${cyc}

#set -x
ulimit -s unlimited
ulimit -a

# module_ver.h
. $SOURCEDIR/versions/run.ver

module purge
module load envvar/$envvar_ver
module load intel/$intel_ver
module load PrgEnv-intel/$PrgEnv_intel_ver

module load craype/$craype_ver
module load cray-mpich/$cray_mpich_ver
module load cray-pals/$cray_pals_ver

module load prod_util/$prod_util_ver
module load prod_envir/$prod_envir_ver

module list

export KEEPDATA=YES

export RUN_ENVIR=${RUN_ENVIR:-dev}
export envir=${envir:-dev}
export NET=${NET:-wsr}
export RUN=${RUN:-wsr}

#export ver=${ver:-v3.3}

export SENDDBN=${SENDDBN:-NO}
export SENDDBN_NTC=${SENDDBN_NTC:-NO}

#export PARAFLAG=${PARAFLAG:-YES}
export SENDECF=${SENDECF:-NO}
export SENDCOM=${SENDCOM:-YES}
export KEEPDATA=${KEEPDATA:-NO}

# Enviorenmenat variables related to Development work place and output files
#

export HOMEwsr=$SOURCEDIR
#export COMROOT=$WORKDIR/${envir}/com
#export COMIN=$baseoutput/$envir/com/${NET}/${ver}/${RUN}.${PDY}/prep
#export COMOUT=$baseoutput/$envir/com/${NET}/${ver}/${RUN}.${PDY}/main
export DATAROOT=$WORKDIR/tmp

export COMPATH=${WORKDIR}/$envir/com/${NET}

# Export List
#export MP_SHARED_MEMORY=yes
export MP_TASK_AFFINITY=core
export MP_EUIDEVICE=sn_all
export MP_EUILIB=us

export envir=prod
${HOMEwsr:?}/jobs/JWSR_MAIN
~                                                                                                                                                              

