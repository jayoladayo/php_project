# Rebuild data/class_cache.rds: the problem type, test type and post-hoc of every course problem.
# Run from the app folder after changing the engine or the problems:  Rscript tools/build_class_cache.R
suppressMessages({ library(shiny); library(bslib); library(ggplot2) })
invisible(Sys.setlocale("LC_ALL", "C.UTF-8"))
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, local = TRUE)
.ps_class_state$loaded <- TRUE                     # compute from scratch
rm(list = ls(.ps_class_cache), envir = .ps_class_cache)
t0 <- Sys.time(); for (p in PROBLEMS) ps_classify(p)
cat(sprintf("Classified %d problems in %.1f s; cache %s\n", length(PROBLEMS), as.numeric(difftime(Sys.time(), t0, units = "secs")), if (isTRUE(ps_class_cache_save())) "written to data/class_cache.rds" else "NOT written"))
