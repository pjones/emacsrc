#!/bin/sh

if [ $# -ne 1 ]; then
    echo "  Usage: build-postgresql.sh PREFIX"
    echo "Example: build-postgresql.sh /usr/local"
    exit
fi

export CC=/usr/bin/gcc
export CXX=/usr/bin/g++

pg_version=`basename $PWD`
pg_options=''
prefix=$1
shift

if [ -d $prefix/openssl ]; then
    echo "====> Using OpenSSL from $prefix/openssl"
    pg_options="$pg_options --with-includes=$prefix/openssl/include --with-libraries=$prefix/openssl/lib"
fi

./configure --disable-nls --disable-shared --enable-thread-safety \
    --without-tcl --without-tk --without-perl --without-python \
    --without-java --with-openssl --with-zlib \
    --prefix=${prefix}/${pg_version} $pg_options || exit 1

gmake || exit 1
echo "====> you are ready to run gmake install"
