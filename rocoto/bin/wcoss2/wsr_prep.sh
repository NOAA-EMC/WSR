#! /usr/bin/env bash

set -x
ulimit -s unlimited
ulimit -a

# module_ver.h
. $SOURCEDIR/versions/run.ver

# Load modules
module list
module purge

module load envvar/$envvar_ver
module load intel/$intel_ver
module load PrgEnv-intel/$PrgEnv_intel_ver
module load craype/$craype_ver
module load cray-mpich/$cray_mpich_ver
module load cray-pals/$cray_pals_ver
module load libjpeg/$libjpeg_ver
module load grib_util/$grib_util_ver
module load cfp/$cfp_ver
export USE_CFP=YES

module list

# For Development
. $GEFS_ROCOTO/bin/wcoss2/common.sh

# Export List
set -x

export MP_EUIDEVICE=sn_all
export MP_EUILIB=us
export MP_TASK_AFFINITY=core

export MP_PGMMODEL=mpmd
export MP_CSS_INTERRUPT=yes

export envir=prod

# CALL executable job script here
$SOURCEDIR/jobs/JWSR_PREP

