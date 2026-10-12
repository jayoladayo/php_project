# Publish StatDesign Studio on shinyapps.io from your own computer.
#
# 1. Open this project in RStudio (or set the working directory to this folder).
# 2. On shinyapps.io go to Account > Tokens, press "Show", then "Show secret",
#    and copy the three values into the lines below. Keep them private: do not
#    share this file once it has your token in it.
# 3. Press Source. The first deploy takes a few minutes while the server
#    installs the packages.

ACCOUNT <- "your-account-name"
TOKEN   <- "paste-token-here"
SECRET  <- "paste-secret-here"
APP     <- "statdesign-studio"

if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")

# Packages the app uses; install any that are missing so rsconnect can record them.
pkgs <- c("shiny", "bslib", "ggplot2", "jsonlite", "readxl", "writexl", "rhandsontable", "car", "lmtest",
          "nortest", "multcomp", "coin", "officer", "flextable", "gridExtra", "png", "base64enc", "htmltools",
          "curl", "promises", "later", "commonmark")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)

rsconnect::setAccountInfo(name = ACCOUNT, token = TOKEN, secret = SECRET)
rsconnect::deployApp(
  appDir = ".", appName = APP, account = ACCOUNT, launch.browser = TRUE, forceUpdate = TRUE,
  appFiles = c("app.R", list.files("R", full.names = TRUE), list.files("www", full.names = TRUE),
               intersect(c(".Renviron", ".Renviron.txt", "Renviron", "Renviron.txt", ".env"), list.files(".", all.files = TRUE))))  # the built-in AI key, if you set one
