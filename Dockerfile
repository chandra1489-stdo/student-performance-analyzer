FROM rocker/shiny:4.4.2

ENV RENV_CONFIG_SANDBOX_ENABLED=false \
    APP_DATA_DIR=/var/data \
    PORT=10000

RUN R -e "install.packages(c('shinydashboard', 'png'), repos = 'https://cloud.r-project.org')"

WORKDIR /app
COPY . /app

RUN chmod +x /app/render-start.sh

EXPOSE 10000

CMD ["/app/render-start.sh"]
