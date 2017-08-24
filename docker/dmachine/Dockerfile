FROM ubuntu:16.04

RUN apt-get update && apt-get -yq install \
    wget \
 && wget http://master.dl.sourceforge.net/project/d-apt/files/d-apt.list \
    -O /etc/apt/sources.list.d/d-apt.list \
 && wget -qO - https://dlang.org/d-keyring.gpg | apt-key add - \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys EBCF975E5BA24D5E \
 && apt-get update && apt-get -yq install \
    dmd-bin \
 && apt-get autoremove -y \
 && apt-get autoclean \
 && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
