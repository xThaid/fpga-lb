#!/bin/sh

set -x

apt-get install -y libtool automake libusb-1.0-0-dev texinfo libusb-dev libyaml-dev pkg-config
git clone https://github.com/SpinalHDL/openocd_riscv
cd openocd_riscv
./bootstrap
./configure --prefix=/opt/openocd_vex --enable-ftdi --enable-dummy
make -j $(nproc)
make install

echo 'export PATH=$PATH:/opt/openocd_vex/bin' >> /etc/bash.bashrc
