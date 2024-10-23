# Verwende ein R-basiertes Image mit Shiny Server
FROM rocker/shiny

# Installiere die RSQLite-Bibliothek für den Zugriff auf SQLite
RUN R -e "install.packages('RSQLite', repos='http://cran.rstudio.com/')"

# Kopiere die Shiny-App in das Verzeichnis für den Shiny-Server
COPY ./app /srv/shiny-server/

# Setze die Zugriffsrechte für den Shiny-Server
RUN chown -R shiny:shiny /srv/shiny-server

# Exponiere den Shiny-Port (standardmäßig 3838)
EXPOSE 3838

# Startbefehl für den Shiny-Server
CMD ["/usr/bin/shiny-server"]
