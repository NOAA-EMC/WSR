#!/bin/bash
# 
# build wsr executables for dell
#
set -eux

hostname

while getopts m: option
do
    case "${option}"
    in
        m) machine=${OPTARG};;
    esac
done

machine=${machine:-acorn} #dell, acorn

if [ $machine = "dell" ]; then
  module purge
  source /usrx/local/prod/lmod/lmod/init/bash
  module load ips/19.0.5.281

  module list

  export FCMP=ifort
elif [ $machine = "acorn" ]; then
  module purge
  source /apps/prod/lmodules/startLmod
  module load envvar/1.0

  module load intel/19.1.3.304 cpe-intel

  module list
  
  export FCMP=ftn
fi


dirpwd=`pwd`
execlist="calcperts calcspread circlevr flights_allnorms rawin_allnorms reformat"
execlist="$execlist sig_pac sigvar_allnorms summ_allnorms tcoeffuvt tgr_special xvvest_allnorms"
if [ $machine = "dell" ]; then
	execlist="$execlist dtsset"
fi

echo execlist=$execlist
command=
if (( $# > 0 )); then
  command=$1
  echo command=$command will be issued for each make
fi
echo
ns=0
nf=0
failstring=
for exec in $execlist
do
  echo "`date`   $exec   begin"
  sorcdir=wsr_${exec}.fd
  cd $sorcdir
  rc=$?
  if (( rc == 0 )); then
    make -f makefile.$machine $command
    rc=$?
    echo in $sorcdir
    if (( rc == 0 )); then
      echo make -f makefile.dell succeeded
      make clean -f makefile.dell
      (( ns = ns + 1 ))
    else
      echo make -f makefile.$machine FAILED rc=$?
      (( nf = nf + 1 ))
      failstring="$failstring $exec"
    fi
  else
    echo cd $sorcdir FAILED rc=$rc
  fi
  cd $dirpwd
  echo "`date`   $exec   end"
  echo
done
echo make $command succeeded for $ns codes
echo make $command failed for $nf codes $failstring
echo

exit

