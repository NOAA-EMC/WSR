#!/bin/bash
#
# build the ECMWF library for WSR
#

set -eux

#libdir=`dirname $0`
#if [[ $libdir = '.' ]]; then
libdir=`pwd`
#fi

if [ -d $libdir/grib_api-1.9.16-working ]; then
  echo "Deleting $libdir/grib_api-1.9.16-working"
  rm -rf $libdir/grib_api-1.9.16-working
fi
if [ -d $libdir/ecmwf_grib_api-1.9.16 ]; then
  echo "Deleting $libdir/ecmwf_grib_api-1.9.16"
  rm -rf $libdir/ecmwf_grib_api-1.9.16
fi

cp -pr $libdir/grib_api-1.9.16 $libdir/grib_api-1.9.16-working
cd $libdir/grib_api-1.9.16-working
rc=$?
if (( rc == 0 )); then
  module load ips/19.0.5.281
  module load jasper/1.900.1
  ecmwfdir=$libdir/ecmwf_grib_api-1.9.16
  ./configure \
    --prefix=$ecmwfdir \
    --with-jasper=/usrx/local/prod/packages/gnu/4.8.5/jasper/1.900.1
  rc=$?
  if (( rc != 0 )); then
    echo configure FAILED rc=$rc
  else
    echo to continue:
    echo cd grib_api-1.9.16-working
    echo "----------------------------- make ..."
    make
    echo "----------------------------- make check ..."
    make check
    echo "----------------------------- make install..."
    make install
  fi
else
  echo cd $libdir/grib_api-1.9.16-working FAILED rc=$rc
  echo $rc
fi

echo;echo " .... Build grib_api-1.9.16 finished .... "
echo;

exit 0

