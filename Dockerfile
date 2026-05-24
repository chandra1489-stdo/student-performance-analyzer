FROM rocker/r-ver:4.4.2

ENV RENV_CONFIG_SANDBOX_ENABLED=false \
    APP_DATA_DIR=/var/data \
    PORT=10000

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    libicu-dev \
    libgit2-dev \
    libssl-dev \
    libxml2-dev \
    libpng-dev \
    libjpeg-dev \
    r-cran-shiny \
    r-cran-shinydashboard \
    r-cran-png \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . /app

RUN chmod +x /app/render-start.sh

EXPOSE 10000

CMD ["/app/render-start.sh"]
