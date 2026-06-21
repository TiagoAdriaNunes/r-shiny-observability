FROM rocker/r-ver:4.6.0

RUN install2.r --error shiny bslib otel otelsdk DBI RPostgres

COPY app.R /app/app.R

EXPOSE 3838

CMD ["Rscript", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]
