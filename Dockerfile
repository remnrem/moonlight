
FROM rocker/r-ver:4.2.1

ENV DEBIAN_FRONTEND=noninteractive

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
    java-common \
    libasound2 \
    libxtst6 \
    cmake \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN mkdir /Programme
RUN mkdir /root/moon
COPY ui.R /root/moon
COPY server.R /root/moon
COPY pops /root/moon/pops
COPY data /root/moon/data

ENV _R_SHLIB_STRIP_=true

RUN cd /Programme \
 && git clone --recursive https://github.com/microsoft/LightGBM \
 && cd LightGBM \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make -j4

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

# Luna
RUN cd /Programme \
 && git clone https://github.com/remnrem/luna-base.git \
 && cd luna-base \ 
 && make -j 2 LGBM=1 LGBM_PATH=/Programme/LightGBM/

## LunaR
RUN cd /Programme \
 && cp /Programme/LightGBM/lib_lightgbm.so /usr/local/lib/ \
 && cp /Programme/LightGBM/lib_lightgbm.so /usr/lib/ \
 && git clone https://github.com/remnrem/luna.git \
 && echo 'PKG_LIBS=include/libluna.a -L${LGBM_PATH} -lfftw3 -l_lightgbm' >> luna/src/Makevars \
 && LGBM=1 LGBM_PATH=/Programme/LightGBM/ R CMD INSTALL luna

COPY Rprofile.site /usr/local/lib/R/etc/

EXPOSE 3838

CMD ["R", "-q", "-e", "shiny::runApp('/root/moon')"]
