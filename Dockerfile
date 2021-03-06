ARG JDK_VERSION=8

FROM openjdk:${JDK_VERSION}

ARG SBT_VERSION=1.2.8

# install tools for LMS with WASM
RUN apt-get update \
  && apt-get install -y --no-install-recommends curl \
  && curl -L -o sbt-$SBT_VERSION.deb https://dl.bintray.com/sbt/debian/sbt-$SBT_VERSION.deb \
  && dpkg -i sbt-$SBT_VERSION.deb  \
  && rm sbt-$SBT_VERSION.deb \
  && curl -sL https://deb.nodesource.com/setup_12.x | bash - \
  && apt-get update \
  && apt-get install -y --no-install-recommends \
       ca-certificates \
       sbt \
       nodejs \
       git \
       gcc \
       build-essential \
       cmake \
  && rm -rf /var/lib/apt/lists/*

RUN npm install --global --productive printf
ENV NODE_PATH /usr/lib/node_modules

RUN git clone --recursive https://github.com/WebAssembly/wabt \
  && cd wabt \
  && git checkout tags/1.0.21 \
  && git submodule update --init \
  && make gcc-release-no-tests \
  && mv out/gcc/Release/no-tests/wat2wasm /bin/ \
  && cd .. \
  && rm -rf

# build lms from source
RUN git clone https://github.com/TiarkRompf/lms-clean \
  && cd lms-clean \
  && git checkout d584b717d951ff4dd2346a3e2e84d232c9afb375 \
  && sbt publishLocal \
  && cd .. \
  && rm -rf lms-clean

# add lms sources to the image
COPY . /opt/lms-wasm/

WORKDIR /opt/lms-wasm

# compile it
RUN sbt compile

# default command
CMD /bin/bash
