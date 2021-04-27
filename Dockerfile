FROM ubuntu:20.04
LABEL MAINTAINER "chris@chrishayward.xyz"

ENV CC=gcc-10
ENV NATIVE_FULL_AOT=1
ENV DEBIAN_FRONTEND=noninteractive

WORKDIR /usr/src
RUN apt update -y \
    && install -y git \
    && git clone https://github.com/djcb/mu.git \
    && git clone https://git.savannah.gnu.org/git/emacs.git

RUN apt install -y \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg-agent \
    software-properties-common

RUN add-apt-repository ppa:ubuntu-toolchain-r/ppa \
    && apt update -y \
    && apt install -y \
    gcc-10 \
    libgccjit0 \
    libgccjit-10-dev

RUN apt install -y \
    libjansson4 \
    libjansson-dev

RUN apt install -y \
    isync
    libxapian-dev \
    libgmime-3.0-dev

RUN sed -i 's/# deb-src/deb-src' /etc/apt/sources.list \
    && apt update -y \
    && apt build-dep -y emacs

WORKDIR /usr/src/emacs
RUN ./autogen.sh \
    && ./configure --with-native-compilation --with-mailutils
    && make -j 2
    && make install

WORKDIR /usr/src/mu
RUN ./autogen.sh \
    && ./configure
    && make
    && make install

WORKDIR /usr/src/app
ENTRYPOINT [ "emacs" ]