#!/bin/bash
set -eux

hostname

build_dir=`pwd`
logs_dir=$build_dir/logs

if [ ! -d $logs_dir  ]; then
  echo "Creating logs folder"
  mkdir $logs_dir
fi

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  echo "Creating ../exec folder"
  mkdir ../exec
fi

module purge
source /usrx/local/prod/lmod/lmod/init/bash
module load ips/19.0.5.281

module list

# Build libs
echo "Building libs..."
cd $build_dir/../libs
./build.sh > $logs_dir/build_libs.log 2>&1
cd $build_dir

# Build sorcs
echo "Building programs in sorc/ ..."
./build.sh > $logs_dir/build_sorc.log 2>&1

# Install programs in sorc
echo "Installing programs in sorc/ ..."
./install.sh > $logs_dir/install_sorc.log 2>&1


echo;echo " .... Build system finished .... ";echo;

exit 0
