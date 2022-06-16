FROM rocker/shiny:4.2.0
RUN install2.r rsconnect shiny bslib sf leaflet data.table tidyverse plotly shinyWidgets
WORKDIR /home/shinyusr
COPY app.R app.R 
COPY deploy.R deploy.R
CMD Rscript deploy.R
