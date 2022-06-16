FROM rocker/shiny:4.2.0
RUN install2.r rsconnect
WORKDIR /home/shinyusr
COPY app.R app.R 
COPY deploy.R deploy.R
CMD Rscript deploy.R
