ARG JDK_VERSION=11

FROM openjdk:${JDK_VERSION} as builder

ARG SBT_VERSION=1.2.8

WORKDIR /opt

# install tools for LMS with WASM
RUN apt-get update \
  && apt-get install -y --no-install-recommends curl \
  && curl -L -o sbt-$SBT_VERSION.deb https://dl.bintray.com/sbt/debian/sbt-$SBT_VERSION.deb \
  && dpkg -i sbt-$SBT_VERSION.deb  \
  && rm sbt-$SBT_VERSION.deb \
  && apt-get update \
  && apt-get install -y --no-install-recommends \
       ca-certificates \
       build-essential \
       git \
       make \
       cmake \
       llvm-3.9-dev \
       libclang-3.9-dev \
       clang-3.9 \
       sbt \
  && rm -rf /var/lib/apt/lists/* \
  && curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | bash -s -- -y

ENV cc=clang-3.9
ENV LLVM_CONFIG_PATH=/usr/bin/llvm-config-3.9

# build wasmtime from source
RUN git clone --recurse-submodules https://github.com/CraneStation/wasmtime.git \
  && cd wasmtime \
  && ~/.cargo/bin/cargo build --release

# build lms from source
RUN git clone https://github.com/TiarkRompf/lms-clean \
  && cd lms-clean \
  && sbt publishLocal

# build final image with small footprint
FROM openjdk:${JDK_VERSION}

ARG SBT_VERSION=1.2.8

# install tools for LMS with WASM
RUN apt-get update \
  && apt-get install -y --no-install-recommends curl \
  && curl -L -o sbt-$SBT_VERSION.deb https://dl.bintray.com/sbt/debian/sbt-$SBT_VERSION.deb \
  && dpkg -i sbt-$SBT_VERSION.deb  \
  && rm sbt-$SBT_VERSION.deb \
  && apt-get update \
  && apt-get install -y --no-install-recommends \
       ca-certificates \
       sbt \
  && rm -rf /var/lib/apt/lists/*

COPY --from=builder /opt/wasmtime/target/release/ /opt/wasmtime/target/release/
COPY --from=builder /root/.ivy2/local/ /root/.ivy2/local/

ENV PATH /opt/wasmtime/target/release:$PATH

# add lms sources to the image
COPY . /opt/lms-wasm/

WORKDIR /opt/lms-wasm
VOLUME  /opt/lms-wasm

# compile it
RUN sbt compile

# default command
CMD /bin/bash