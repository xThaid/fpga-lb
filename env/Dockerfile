FROM ubuntu:20.04

ARG USER_ID
ARG GROUP_ID

RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive \
    apt-get -y install \
        ca-certificates \
        curl \
        git \
        locales \
        libglib2.0-0 \
        make

RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen && \
    locale-gen

ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

# Quartus Lite
COPY install_quartus.sh /tmp/quartus/
RUN cd /tmp/quartus && ./install_quartus.sh
RUN rm -rf /tmp/quartus /opt/quartus/uninstall

# Verilator
COPY install_verilator.sh /tmp/verilator/
RUN cd /tmp/verilator && ./install_verilator.sh
RUN rm -rf /tmp/verilator

# SBT (required for building VexRISCV)
COPY install_sbt.sh /tmp/sbt/
RUN cd /tmp/sbt && ./install_sbt.sh
RUN rm -rf /tmp/sbt

# Risc-V toolchain
COPY install_riscv_toolchain.sh /tmp/riscv_toolchain/
RUN cd /tmp/riscv_toolchain && ./install_riscv_toolchain.sh
RUN rm -rf /tmp/riscv_toolchain

RUN addgroup --gid $GROUP_ID user
RUN adduser --disabled-password --gecos '' --uid $USER_ID --gid $GROUP_ID user

# Hardcaml
COPY install_hardcaml.sh /tmp/hardcaml/
RUN cd /tmp/hardcaml && ./install_hardcaml.sh
RUN rm -rf /tmp/hardcaml

# OpenOCD for VexRiscv
COPY install_openocd_vex.sh /tmp/openocd_vex/
RUN cd /tmp/openocd_vex && ./install_openocd_vex.sh
RUN rm -rf /tmp/openocd_vex

USER user

ENTRYPOINT bash

