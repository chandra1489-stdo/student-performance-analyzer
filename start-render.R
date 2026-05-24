port <- as.integer(Sys.getenv("PORT", unset = "10000"))
if (is.na(port)) {
  port <- 10000L
}

options(
  shiny.host = "0.0.0.0",
  shiny.port = port
)

shiny::runApp(
  appDir = "/app",
  host = "0.0.0.0",
  port = port
)
