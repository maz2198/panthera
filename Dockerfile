FROM rocker/shiny-verse:latest

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl14-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    wget

RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O \           
    "version.txt" && VERSION=$(cat version.txt) && \
    wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n  ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    . /etc/environment && \
    R -e "install.packages(c('shiny','spData','spdep','DT','data.table','RColorBrewer','stringr', \
  'leaflet','dplyr','readr','ggplot2','ggdark','countrycode','magrittr'),repos='$MRAN')" 

RUN chmod -R 755 /srv/shiny-server/

EXPOSE 3838

COPY shiny-server.sh /usr/bin/shiny-server.sh
COPY ./pantheraShinyApp/app.R /srv/shiny-server/

EXPOSE 3838

COPY shiny-server.sh usr/bin/shiny-server.sh

CMD ["usr/bin/shiny-server.sh"]

