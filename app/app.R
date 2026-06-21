library(shiny)
library(bslib)
library(otelsdk)
library(DBI)
library(RPostgres)

message("OTel tracing enabled: ", otel::is_tracing_enabled())

.db <- tryCatch(
  DBI::dbConnect(RPostgres::Postgres(),
    host = Sys.getenv("POSTGRES_HOST", "postgres"),
    dbname = "shiny_events", user = "shiny", password = "shiny"
  ),
  error = function(e) { message("[db] connect failed: ", e$message); NULL }
)

if (!is.null(.db)) {
  DBI::dbExecute(.db, "CREATE TABLE IF NOT EXISTS events (
    id SERIAL PRIMARY KEY,
    event_type TEXT,
    session_id TEXT,
    created_at TIMESTAMPTZ DEFAULT now()
  )")
}

log_event <- function(event_type, session_id) {
  if (is.null(.db)) return(invisible(NULL))
  tryCatch(
    DBI::dbExecute(.db, "INSERT INTO events (event_type, session_id) VALUES ($1, $2)",
      list(event_type, session_id)),
    error = function(e) message("[db] insert failed: ", e$message)
  )
}

# ---- Shared metrics state ----
.m <- new.env(parent = emptyenv())
.m$sessions_total  <- 0L
.m$sessions_active <- 0L
.m$requests_total  <- 0L
.m$errors_total    <- 0L

# ---- OTel metrics instruments ----
.meter <- otel::get_meter("shiny.instrumentation")
.instr <- list(
  sessions_total  = .meter$counter("shiny_sessions_total",
                      description = "Total Shiny sessions"),
  sessions_active = .meter$up_down_counter("shiny_sessions_active",
                      description = "Active Shiny sessions"),
  requests_total  = .meter$counter("shiny_requests_total",
                      description = "Total reactive computations"),
  errors_total    = .meter$counter("shiny_errors_total",
                      description = "Total errors triggered")
)

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
  otel::counter_add(.instr$sessions_total, 1L)
  otel::up_down_counter_add(.instr$sessions_active, 1L)
  log_event("session_start", session$token)
  onSessionEnded(function() {
    .m$sessions_active <- .m$sessions_active - 1L
    otel::up_down_counter_add(.instr$sessions_active, -1L)
  })

  tick <- reactiveTimer(2000)

  data_r <- reactive({
    .m$requests_total <- .m$requests_total + 1L
    otel::counter_add(.instr$requests_total, 1L)
    switch(input$dist,
      norm = rnorm(input$n),
      unif = runif(input$n),
      exp  = rexp(input$n)
    )
  })

  observeEvent(input$error_btn, {
    .m$errors_total <- .m$errors_total + 1L
    otel::counter_add(.instr$errors_total, 1L)
    log_event("error_triggered", session$token)
    showNotification("Error triggered and counted!", type = "error")
  })

  observeEvent(input$dist, {
    log_event("distribution_changed", session$token)
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
