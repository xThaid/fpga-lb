#!/bin/sh

set -x

VERSION=riscv64-unknown-elf-toolchain-10.2.0-2020.12.8-x86_64-linux-ubuntu14

curl -O https://static.dev.sifive.com/dev-tools/freedom-tools/v2020.12/$VERSION.tar.gz
tar -xzvf $VERSION.tar.gz
mv $VERSION /opt/riscv

echo 'export PATH=$PATH:/opt/riscv/bin' >> /etc/bash.bashrc
