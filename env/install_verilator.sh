#!/bin/sh

set -x

apt-get install -y git make autoconf g++ flex libfl-dev bison python3
git clone http://git.veripool.org/git/verilator
unset VERILATOR_ROOT
cd verilator
git pull
git checkout v4.214
autoconf
./configure --prefix /usr/local
make -j$(nproc)
make install
cd ..