#!/bin/bash
set -eu #x

sWS=$(pwd)
echo $sWS

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

if [ $machine = "nomachine" ]; then
    if [[ -L /usrx && "$( readlink /usrx 2> /dev/null )" =~ dell ]] ; then # We are on NOAA Mars or Venus
        machine=wcoss_dell_p3
	elif [[ -d /apps/prod ]]; then
		machine=acorn
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

if [ $CompileCode = "yes" ]; then
    cd $sWS/../sorc

    ## Build the code and install
    if [[ $machine == "wcoss_dell_p3" ]]; then
    	./build_all.sh -m dell
    elif [[ $machine == "acorn" ]]; then
        ./build_all.sh -m acorn
    else
        ./build_all.sh -m acorn
    fi

fi


# for cleanning
if [ $CleanAll = "yes" ]; then
    echo "Cleaning ..."
    
    rm -rf gefs.xml
    rm -rf cron_rocoto
    rm -rf tasks

    cd $sWS/../sorc

    rm -rf logs

    #for dir in global_ensadd.fd  global_enspqpf.fd  gefs_ensstat.fd  global_ensppf.fd ; do
    #    if [ -f $dir ]; then
    #        cd $dir
    #        make clean
    #        cd ..
    #    fi
    #done

    cd ${sWS}/../sorc
    rm -rf ../exec

fi # for CleanAll

# for rocoto
if [ $RunRocoto = "yes" ]; then
    cd $sWS
    if [ $machine = "wcoss_dell_p3" ]; then
        . /usrx/local/prod/lmod/lmod/init/sh
        module use /gpfs/dell3/usrx/local/dev/emc_rocoto/modulefiles
        module load lsf/10.1
        module load ruby/2.5.1
        module load rocoto/complete
        module load python/3.6.3

    elif [ $machine = "acorn" ]; then
        module purge
        module load envvar/1.0

        module load intel/19.1.3.304 PrgEnv-intel
        module load python/3.8.6

        module list

    fi
    #./py/run_to_get_all.py  $userConfigFile
    ./py/run_pyGEFS.py -r yes -f $userConfigFile
    echo "Generated xml and/or ent and updated bin file!"
fi # For RunRocoto

#echo "crontab"
# For Crontab
if [ $AddCrontabToMyCrontab = "yes" ]; then
    cd $sWS
    if [ $machine = "acorn" ]; then
        if [ -f $HOME/cron/mycrontab ]; then
            echo "Adding crontab to $HOME/cron/mycrontab!" 
        else 
            mkdir $HOME/cron
            touch $HOME/cron/mycrontab
        fi
    
        py/add_crontab.py
        crontab $HOME/cron/mycrontab
        echo "Added crontab to $HOME/cron/mycrontab!"

    elif [ $machine = "wcoss_dell_p3" ]; then
        py/add_crontab.py
        echo "Added crontab to $HOME/cron/mycrontab!"
    fi
fi
