port <- as.integer(Sys.getenv("PORT", unset = "10000"))
if (is.na(port)) {
  port <- 10000L
}

required_packages <- c("shiny", "shinydashboard", "png")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
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
