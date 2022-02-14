#!/bin/sh

set -x

OCAML_VERSION=4.13.1
OPAM_VERSION=2.0.5

apt-get install -y unzip build-essential gcc curl coreutils libgmp-dev pkg-config libffi-dev

curl -L -o /usr/bin/opam "https://github.com/ocaml/opam/releases/download/$OPAM_VERSION/opam-$OPAM_VERSION-$(uname -m)-$(uname -s)" && \
    chmod 755 /usr/bin/opam


su user -c "opam init -y --comp $OCAML_VERSION --disable-sandboxing"

su user -c "opam repo add janestreet-bleeding https://ocaml.janestreet.com/opam-repository"
su user -c "opam install -y utop hardcaml hardcaml_waveterm ppx_deriving_hardcaml dune hardcaml_step_testbench hardcaml_circuits macaddr hardcaml_verilator tuntap lwt"

su user -c "echo 'test -r /home/user/.opam/opam-init/init.sh && . /home/user/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true' >> ~/.bashrc"
