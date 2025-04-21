
## Note we must run superClean.R locally first, to ensure the Datasets/dt_final.rds file is saved.
source("superClean.R")

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
  appName = "Marion-Stokes-Tapes-Dashboard",
)