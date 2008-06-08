#! /bin/sh

if [ $# -ne 1 ]; then
    echo "  Usage: build-apache.sh PREFIX"
    echo "Example: build-apache.sh /usr/local"
    return
fi

export CC=/usr/bin/gcc
export CXX=/usr/bin/g++

apache_version=`basename $PWD | sed 's/httpd/apache/'`
apache_options=''
prefix=$1
shift

echo
echo "====> Configuring Apache to install in ${prefix}/${apache_version}"
echo

if [ -d $prefix/openssl ]; then
    echo "====> Using OpenSSL from $prefix/openssl"
    apache_options="$apache_options --with-ssl=$prefix/openssl"
fi
	
./configure --prefix=${prefix}/${apache_version} \
    --enable-mods-shared=all \
    --disable-status \
    --disable-info \
    --enable-ssl \
    --enable-proxy \
    $apache_options \
    $* \
    && gmake \
    && echo "====> you are ready to run gmake install"
