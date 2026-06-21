library(shiny)
library(bslib)
library(httr2)
library(later)

# ---- Shared metrics state ----
.m <- new.env(parent = emptyenv())
.m$sessions_total  <- 0L
.m$sessions_active <- 0L
.m$requests_total  <- 0L
.m$errors_total    <- 0L

# ---- OTLP helpers ----
unix_nano <- function() format(round(as.numeric(Sys.time()) * 1e9), scientific = FALSE)

make_gauge <- function(name, desc, value) {
  list(
    name        = name,
    description = desc,
    gauge       = list(dataPoints = list(list(
      asInt        = as.integer(value),
      timeUnixNano = unix_nano()
    )))
  )
}

make_counter <- function(name, desc, value) {
  list(
    name        = name,
    description = desc,
    sum         = list(
      dataPoints             = list(list(
        asInt        = as.integer(value),
        timeUnixNano = unix_nano()
      )),
      aggregationTemporality = 2L,  # CUMULATIVE
      isMonotonic            = TRUE
    )
  )
}

# ---- OTLP push (every 10s via later event loop) ----
otel_endpoint <- paste0(
  Sys.getenv("OTEL_EXPORTER_OTLP_ENDPOINT", "http://otel-collector:4318"),
  "/v1/metrics"
)

push_metrics <- function() {
  payload <- list(
    resourceMetrics = list(list(
      resource    = list(attributes = list(
        list(key = "service.name",    value = list(stringValue = "r-shiny")),
        list(key = "service.version", value = list(stringValue = "0.1.0"))
      )),
      scopeMetrics = list(list(
        scope   = list(name = "shiny.instrumentation"),
        metrics = list(
          make_gauge(   "shiny_sessions_active", "Active Shiny sessions",      .m$sessions_active),
          make_counter( "shiny_sessions_total",  "Total Shiny sessions",        .m$sessions_total),
          make_counter( "shiny_requests_total",  "Total reactive computations", .m$requests_total),
          make_counter( "shiny_errors_total",    "Total errors triggered",      .m$errors_total)
        )
      ))
    ))
  )

  tryCatch(
    request(otel_endpoint) |>
      req_body_json(payload) |>
      req_timeout(5) |>
      req_perform(),
    error = function(e) message("[otel] push failed: ", conditionMessage(e))
  )

  later(push_metrics, delay = 10)
}

push_metrics()

# ---- UI ----
ui <- page_sidebar(
  title = "R Shiny Observability",
  theme = bs_theme(version = 5, preset = "darkly"),
  sidebar = sidebar(
    sliderInput("n", "Sample size", 100, 10000, 1000, step = 100),
    selectInput("dist", "Distribution", c(
      "Normal"      = "norm",
      "Uniform"     = "unif",
      "Exponential" = "exp"
    )),
    actionButton("error_btn", "Trigger Error", class = "btn-danger w-100 mt-2"),
    hr(),
    p(class = "text-muted small", "OTel Collector: :4317 / :4318"),
    p(class = "text-muted small", "Prometheus:     :9091"),
    p(class = "text-muted small", "Grafana:        :3000")
  ),
  layout_column_wrap(
    width = 1 / 4,
    fill  = FALSE,
    value_box("Sessions Total",  textOutput("vb_sessions_total"),  theme = "primary"),
    value_box("Active Sessions", textOutput("vb_sessions_active"), theme = "success"),
    value_box("Reactive Calls",  textOutput("vb_requests"),        theme = "info"),
    value_box("Errors",          textOutput("vb_errors"),          theme = "danger")
  ),
  card(
    full_screen = TRUE,
    card_header("Distribution Plot"),
    plotOutput("plot")
  )
)

# ---- Server ----
server <- function(input, output, session) {
  .m$sessions_total  <- .m$sessions_total  + 1L
  .m$sessions_active <- .m$sessions_active + 1L
  onSessionEnded(function() .m$sessions_active <- .m$sessions_active - 1L)

  tick <- reactiveTimer(2000)

  data_r <- reactive({
    .m$requests_total <- .m$requests_total + 1L
    switch(input$dist,
      norm = rnorm(input$n),
      unif = runif(input$n),
      exp  = rexp(input$n)
    )
  })

  observeEvent(input$error_btn, {
    .m$errors_total <- .m$errors_total + 1L
    showNotification("Error triggered and counted!", type = "error")
  })

  output$plot <- renderPlot({
    hist(data_r(),
      main   = paste(input$dist, "distribution"),
      col    = "#375a7f",
      border = "white",
      xlab   = "value"
    )
  })

  output$vb_sessions_total  <- renderText({ tick(); .m$sessions_total  })
  output$vb_sessions_active <- renderText({ tick(); .m$sessions_active })
  output$vb_requests        <- renderText({ tick(); .m$requests_total  })
  output$vb_errors          <- renderText({ tick(); .m$errors_total    })
}

shinyApp(ui, server)
