library(rsconnect)

token <- Sys.getenv("SHINYAPPS_TOKEN")
secret <- Sys.getenv("SHINYAPPS_SECRET")

rsconnect::setAccountInfo(
  name = "jules-dev",
  token = token,
  secret = secret
)

rsconnect::deployApp(
  appDir = "./",
  appName = "Marion-Stokes-Tapes-Dashboard"
)