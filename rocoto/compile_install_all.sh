#!/bin/bash
set -eu #x

sWS=$(pwd)
echo ${sWS}

while getopts c:a:r:m:f:b:e: option
do
  case "${option}"
  in
    c) CompileCode=${OPTARG};;
    a) CleanAll=${OPTARG};;
    r) RunRocoto=${OPTARG};;
    m) machine=${OPTARG};;
    f) userConfigFile=${OPTARG};;
    b) AddCrontabToMyCrontab=${OPTARG};;
    e) RunEnvir=${OPTARG};;
  esac
done

CompileCode=${CompileCode:-no}
CleanAll=${CleanAll:-no}
RunRocoto=${RunRocoto:-no}
machine=${machine:-nomachine}
userConfigFile=${userConfigFile:-user_full.conf}
AddCrontabToMyCrontab=${AddCrontabToMyCrontab:-no}
DeleteCrontabFromMyCrontab=${DeleteCrontabFromMyCrontab:-no}
RunEnvir=${RunEnvir:-emc}

if [ ${machine} = "nomachine" ]; then
  if [ -d /scratch1/NCEPDEV ]; then
    machine=hera
  elif [[ -d /apps/prod ]]; then # WCOSS2
    machine=wcoss2
  else
    echo "This is not supported by this script!"
    exit 55
  fi
fi

echo $CompileCode
echo $CleanAll
echo $RunRocoto
echo $machine
echo $userConfigFile
echo $RunEnvir

if [ ${CompileCode} = "yes" ]; then
  cd ${sWS}/../sorc

  ## Build the code and install
  if [[ ${machine} == "hera" ]]; then
    ./build_all.sh -m hera
  elif [[ ${machine} == "wcoss2" ]]; then
    ./build_all.sh -m wcoss2
  else
    echo "unsupported machine!"
    exit -1
  fi

fi


# for cleanning
if [ ${CleanAll} = "yes" ]; then
  echo "Cleaning ..."

  cd ${sWS}

  rm -rf gefs.xml
  rm -rf gefs*.db
  rm -rf cron_rocoto
  rm -rf tasks

  cd ${sWS}/../sorc

  rm -rf logs

  cd ${sWS}/../sorc
  rm -rf ../exec

fi # for CleanAll

# for rocoto
if [ $RunRocoto = "yes" ]; then
  cd $sWS
  if [ $machine = "hera" ]; then
    module load intel/18.0.5.274
    module load rocoto/1.3.3
    module load contrib
    module load anaconda/latest

  elif [ $machine = "wcoss2" ]; then

    module purge
    module load envvar/1.0

    module load PrgEnv-intel/8.3.3
    module load craype/2.7.17
    module load intel/19.1.3.304
    module load cray-mpich/8.1.9

    module load python/3.8.6

    module use /apps/ops/test/nco/modulefiles/
    module load core/rocoto/1.3.5

  fi
  ./py/run_pyGEFS.py -r yes -f ${userConfigFile}
  echo "Generated xml and/or ent and updated bin file!"
fi # For RunRocoto

#echo "crontab"
# For Crontab
if [ ${AddCrontabToMyCrontab} = "yes" ]; then
  cd ${sWS}
  sFile=${HOME}/cron/mycrontab
  if [ -f ${sFile} ]; then
    echo "Adding crontab to ${sFile}!"
  else
    mkdir ${HOME}/cron
    touch ${sFile}
  fi

  py/add_crontab.py
  crontab ${sFile}
  echo "Added crontab to ${sFile}!"

fi
