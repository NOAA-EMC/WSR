#!/bin/bash
#
# build the ECMWF library for WSR
#
libdir=`dirname $0`
if [[ $libdir = '.' ]]; then
  libdir=`pwd`
fi
cp -pr $libdir/grib_api-1.9.16 $libdir/grib_api-1.9.16-working
cd $libdir/grib_api-1.9.16-working
rc=$?
if (( rc == 0 )); then
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
    echo make
    echo make check
    echo make install
  fi
else
  echo cd $libdir/grib_api-1.9.16-working FAILED rc=$rc
fi

