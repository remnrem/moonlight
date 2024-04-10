FROM rocker/r-ver:4.2.1 AS builder

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
    libcurl4-openssl-dev \
    libxml2-dev \
    libomp-dev \
    libxt6 \
    software-properties-common \
    lsb-release \
    gpg-agent \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Add Kitware's APT repository for up-to-date CMake versions
RUN wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc 2>/dev/null | gpg --dearmor - | tee /usr/share/keyrings/kitware-archive-keyring.gpg >/dev/null \
    && echo "deb [signed-by=/usr/share/keyrings/kitware-archive-keyring.gpg] https://apt.kitware.com/ubuntu/ $(lsb_release -cs) main" | tee /etc/apt/sources.list.d/kitware.list >/dev/null \
    && apt-get update

# Install CMake (specifying a version or the latest available)
RUN apt-get install -y cmake

WORKDIR /Programme

ENV _R_SHLIB_STRIP_=true

RUN git clone --recursive https://github.com/microsoft/LightGBM \
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
    shinyjs \
    shinythemes

COPY Rprofile.site /usr/local/lib/R/etc/

# Luna
RUN git clone https://github.com/remnrem/luna-base.git \
 && cd luna-base \ 
 && make -j 2 LGBM=1 LGBM_PATH=/Programme/LightGBM/ \
 && ln -s /Programme/luna-base/luna /usr/local/bin/luna \
 && ln -s /Programme/luna-base/destrat /usr/local/bin/destrat \
 && ln -s /Programme/luna-base/behead /usr/local/bin/behead \
 && ln -s /Programme/luna-base/fixrows /usr/local/bin/fixrows

## LunaR
RUN cp LightGBM/lib_lightgbm.so /usr/local/lib/ \
 && cp LightGBM/lib_lightgbm.so /usr/lib/ \
 && git clone https://github.com/remnrem/luna.git \
 && echo 'PKG_LIBS=include/libluna.a -L${LGBM_PATH} -lfftw3 -l_lightgbm' >> luna/src/Makevars \
 && LGBM=1 LGBM_PATH=/Programme/LightGBM/ R CMD INSTALL luna

#------------------------------- Multi-stage build (keeps the image size down)-------------------------------------------
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
    cmake \
    libxt6 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN mkdir /data
RUN mkdir /root/moon
COPY ui.R server.R /root/moon/
COPY pops /root/moon/pops
COPY data /root/moon/data
COPY models /root/moon/models

ENV _R_SHLIB_STRIP_=true
COPY --from=builder /Programme/luna-base/luna /usr/local/bin/luna
COPY --from=builder /Programme/luna-base/destrat /usr/local/bin/destrat
COPY --from=builder /Programme/luna-base/behead /usr/local/bin/behead
COPY --from=builder /Programme/luna-base/fixrows /usr/local/bin/fixrows
COPY --from=builder /Programme/LightGBM/lib_lightgbm.so /usr/local/lib
COPY --from=builder /Programme/LightGBM/lib_lightgbm.so /usr/lib
COPY --from=builder /usr/local/lib/R /usr/local/lib/R

ENV MOONLIGHT_SERVER_MODE=0
EXPOSE 3838
CMD ["R", "-q", "-e", "shiny::runApp('/root/moon')"]
