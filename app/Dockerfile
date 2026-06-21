FROM rocker/r-ver:4.5.3

RUN install2.r --error renv

WORKDIR /app

# Copy renv files first - layer is cached until lock changes
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

RUN Rscript -e "renv::restore(prompt = FALSE)"

COPY app.R app.R

EXPOSE 3838

CMD ["Rscript", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]
