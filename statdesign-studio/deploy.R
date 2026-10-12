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

# Packages the app uses. rsconnect refuses to deploy unless these AND everything they
# depend on (for example broom, doBy, gdtools, ragg, rbibutils, SparseM) are installed
# on this computer, so install whatever is missing first.
pkgs <- c("shiny", "bslib", "ggplot2", "jsonlite", "readxl", "writexl", "rhandsontable", "car", "lmtest",
          "nortest", "multcomp", "emmeans", "coin", "officer", "flextable", "gridExtra", "png", "base64enc",
          "htmltools", "curl", "promises", "later", "commonmark")
repo <- getOption("repos"); if (is.null(repo) || identical(unname(repo["CRAN"]), "@CRAN@")) options(repos = c(CRAN = "https://cloud.r-project.org"))
# Follow the requirements of both the installed versions (what rsconnect checks) and the
# current CRAN versions (what install.packages() will fetch), until nothing new turns up.
dep_of <- function(p, db) unlist(tools::package_dependencies(p, db = db, which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE), use.names = FALSE)
cran <- tryCatch(utils::available.packages(), error = function(e) NULL)
need <- pkgs
repeat {
  more <- unique(c(need, dep_of(need, utils::installed.packages()), if (!is.null(cran) && nrow(cran)) dep_of(need, cran)))
  if (length(more) == length(need)) break
  need <- more
}
have <- rownames(utils::installed.packages())
missing <- setdiff(need, c(have, rownames(utils::installed.packages(priority = "base"))))
if (length(missing)) {
  message("Installing packages the app needs: ", paste(missing, collapse = ", "))
  install.packages(missing)
}
still <- setdiff(need, rownames(utils::installed.packages()))
if (length(still)) stop("These packages could not be installed: ", paste(still, collapse = ", "),
                        ". Install them with install.packages(), then run this script again.", call. = FALSE)

rsconnect::setAccountInfo(name = ACCOUNT, token = TOKEN, secret = SECRET)
rsconnect::deployApp(
  appDir = ".", appName = APP, account = ACCOUNT, launch.browser = TRUE, forceUpdate = TRUE,
  appFiles = c("app.R", list.files("R", full.names = TRUE), list.files("www", full.names = TRUE),
               intersect(c(".Renviron", ".Renviron.txt", "Renviron", "Renviron.txt", ".env"), list.files(".", all.files = TRUE))))  # the built-in AI key, if you set one
