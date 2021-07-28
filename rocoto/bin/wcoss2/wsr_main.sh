#!/bin/sh

set -x
ulimit -s unlimited
ulimit -a

# module_ver.h
. $SOURCEDIR/versions/wsr_acorn.ver

# Load modules
module list
module purge

module load envvar/$envvar_ver
module load intel/$intel_ver PrgEnv-intel
module load cray-mpich/$cray_mpich_ver
module load cray-pals/$cray_pals_ver

module load prod_util/$prod_util_ver
module load prod_envir/$prod_envir_ver

#module load cfp/$cfp_ver
#export USE_CFP=YES

module list

# For Development
. $GEFS_ROCOTO/bin/acorn/common.sh

# Export List

# CALL executable job script here
export MP_SHARED_MEMORY=yes
export MP_TASK_AFFINITY=core
export MP_EUIDEVICE=sn_all
export MP_EUILIB=us

$SOURCEDIR/jobs/JWSR_MAIN

################################################################################
# TASKSCRIPT DOCUMENTATION BLOCK
#
# PURPOSE:  Executes the job that creates the Winter Storm Recon Files.

