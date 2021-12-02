#!/bin/sh

set -x

apt-get install -y software-properties-common
add-apt-repository -y ppa:openjdk-r/ppa
apt-get update
apt-get install openjdk-8-jdk -y
update-alternatives --config java
update-alternatives --config javac

echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list
echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add
apt-get update
apt-get install sbt
