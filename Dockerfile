FROM rocker/shiny:4.0.5

WORKDIR /build

ENV DEBIAN_FRONTEND=noninteractive

# https://hosting.analythium.io/best-practices-for-r-with-docker/
# https://www.r-bloggers.com/2021/06/running-shiny-server-in-docker/

# Install system requirements for index.R as needed

RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    g++ \
    git \
    nano \
    wget \
    zlib1g-dev \
    fftw3-dev \
    libgit2-dev \
    libssl-dev \
    libssh2-1-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libomp-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

ENV _R_SHLIB_STRIP_=true

RUN install2.r --error --skipinstalled \
    shiny \
    git2r \
    data.table \
    plotrix \
    geosphere \
    DT \
    shinyFiles \
    shinydashboard \
    lubridate \
    wkb \
    aws.s3 \
    shinybusy \
    shinythemes
    
ADD https://cmake.org/files/v3.22/cmake-3.22.2-linux-x86_64.sh /cmake-3.22.2-linux-x86_64.sh


RUN mkdir /opt/cmake \
 && sh /cmake-3.22.2-linux-x86_64.sh --prefix=/opt/cmake --skip-license \
 && ln -s /opt/cmake/bin/cmake /usr/local/bin/cmake \
 && ln -s /opt/cmake/bin/cmake /usr/bin/cmake \
 && cmake --version


RUN git clone --recursive https://github.com/microsoft/LightGBM \
 && cd LightGBM \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make -j4

## Luna

RUN cd /build \
 && git clone https://github.com/remnrem/luna-base.git \
 && cd luna-base \
 && make -j 2 LGBM=1 LGBM_PATH=/build/LightGBM/

## LunaR

RUN cd /build \
 && cp /build/LightGBM/lib_lightgbm.so /usr/local/lib/ \
 && git clone https://github.com/remnrem/luna.git \
 && echo 'PKG_LIBS=include/libluna.a -L$(FFTW)/lib/ -L${LGBM_PATH} -lfftw3 -l_lightgbm' >> luna/src/Makevars \
 && LGBM=1 LGBM_PATH=/build/LightGBM/ R CMD INSTALL luna
 
# copy ui.R and server.R over and set up shiny
RUN mkdir /root/moon
COPY ui.R /root/moon
COPY server.R /root/moon
COPY pops /root/moon/pops

<<<<<<< HEAD
COPY ui.R server.R /srv/shiny-server/
COPY pops /srv/shiny-server/pops

USER shiny
=======
COPY Rprofile.site /usr/local/lib/R/etc/
>>>>>>> a09d924e3fff02905867a8fe4e5f9fbf70b5598a

EXPOSE 3838

CMD ["R", "-q", "-e", "shiny::runApp('/root/moon')"]
