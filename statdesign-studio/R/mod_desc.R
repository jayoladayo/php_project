# Descriptive statistics: numerical and graphical summaries, as in the class script.

# Mode as in DescTools::Mode: the most frequent value(s); none when every value is unique.
mode_txt <- function(x) {
  x <- x[is.finite(x)]; if (!length(x)) return("—")
  tb <- table(x); mx <- max(tb)
  if (mx == 1) return("None (all unique)")
  m <- names(tb)[tb == mx]
  if (length(m) <= 3) paste(m, collapse = ", ") else sprintf("%d values tie", length(m))
}
# Skewness and excess kurtosis, DescTools default (type 3).
skew3 <- function(x) { n <- length(x); m <- mean(x); s <- sqrt(sum((x - m)^2) / n); g1 <- sum((x - m)^3) / n / s^3; g1 * ((n - 1) / n)^1.5 }
kurt3 <- function(x) { n <- length(x); m <- mean(x); m2 <- sum((x - m)^2) / n; g2 <- sum((x - m)^4) / n / m2^2 - 3; (g2 + 3) * (1 - 1 / n)^2 - 3 }

desc_stats <- function(v, pct = 90) {
  n_all <- length(v); x <- v[is.finite(v)]; n <- length(x)
  if (!n) return(NULL)
  q <- stats::quantile(x, c(.25, .75, pct / 100), names = FALSE)
  sdv <- if (n > 1) stats::sd(x) else NA
  sw <- sw_test(x)
  list(n = n, missing = n_all - n, mean = mean(x), median = stats::median(x), mode = mode_txt(x),
       min = min(x), max = max(x), range = max(x) - min(x), q1 = q[1], q3 = q[2], iqr = q[2] - q[1], pct = q[3],
       var = if (n > 1) stats::var(x) else NA, sd = sdv, se = sdv / sqrt(n), cv = 100 * sdv / mean(x),
       skew = if (n > 2 && isTRUE(sdv > 0)) skew3(x) else NA, kurt = if (n > 3 && isTRUE(sdv > 0)) kurt3(x) else NA,
       sw = sw$p, outliers = sum(x < q[1] - 1.5 * (q[2] - q[1]) | x > q[2] + 1.5 * (q[2] - q[1])))
}

DESC_ROWS <- function(pct) list(
  c("n", "n"), c("missing", "Missing"), c("mean", "Mean"), c("median", "Median"), c("mode", "Mode"),
  c("min", "Min"), c("max", "Max"), c("range", "Range"), c("q1", "Q1 (25th percentile)"), c("q3", "Q3 (75th percentile)"),
  c("iqr", "IQR"), c("pct", sprintf("%sth percentile", format(pct))), c("var", "Variance"), c("sd", "Std. deviation"),
  c("se", "Std. error"), c("cv", "CV"), c("skew", "Skewness"), c("kurt", "Kurtosis (excess)"), c("sw", "Shapiro-Wilk p"),
  c("outliers", "Outliers (1.5 × IQR rule)"))

fmt_stat <- function(key, v) {
  if (is.character(v)) return(v)
  if (is.null(v) || is.na(v)) return("—")
  switch(key, n = , missing = , outliers = format(v), cv = sprintf("%.2f%%", v), sw = fmt_p(v, 3),
         skew = , kurt = sprintf("%.3f", v), formatC(v, format = "f", digits = 3))
}

desc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Descriptive statistics", uiOutput(ns("subtitle"), inline = TRUE),
              div(class = "hrow", pin_button(ns),
                  downloadButton(ns("dl"), label = tagList(icon("download", 14), "Export table"), icon = NULL, class = "btn-k bp bsm"))),
    kcard(div(class = "grid", style = "grid-template-columns:minmax(0,1.2fr) minmax(0,1.6fr) minmax(0,1.2fr) minmax(0,.8fr);align-items:end",
              selectInput(ns("ds"), "Dataset", NULL),
              selectizeInput(ns("vars"), "Variables", NULL, multiple = TRUE, options = list(plugins = list("remove_button"), placeholder = "Choose numeric columns")),
              selectizeInput(ns("grp"), "Group by (optional)", NULL),
              numericInput(ns("pct"), "Extra percentile", 90, min = 1, max = 99, step = 1))),
    uiOutput(ns("body"), class = "rcol"))
}

desc_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    last_active <- NULL
    observe({
      dss <- store$datasets
      if (!length(dss)) { updateSelectInput(session, "ds", choices = character(0)); return() }
      ch <- setNames(names(dss), vapply(dss, `[[`, "", "name"))
      sel <- isolate(input$ds); act <- store$active
      if (!identical(act, last_active) && !is.null(act) && act %in% ch) sel <- act
      last_active <<- act
      if (is.null(sel) || !(sel %in% ch)) sel <- act %||% (if (length(ch)) ch[length(ch)] else NULL)
      updateSelectInput(session, "ds", choices = ch, selected = sel)
    })
    ds <- reactive({ id <- input$ds; if (is.null(id) || !nzchar(id)) NULL else store$datasets[[id]] })
    dat <- reactive({ d <- ds(); req(d); drop_empty_rows(d$data) })
    observeEvent(list(input$ds, ds()$types), {
      d <- ds(); req(d); df <- dat(); ty <- d$types
      nc <- names(df)[vapply(names(df), function(n) identical(ty[[n]], "number"), TRUE)]
      gc <- names(df)[vapply(names(df), function(n) { k <- length(level_order(df[[n]])); k >= 2 && k <= 30 && k < sum(!is.na(df[[n]])) }, TRUE)]
      gc <- c(setdiff(gc, nc), intersect(gc, nc))
      keep <- intersect(isolate(input$vars) %||% character(0), nc); if (!length(keep)) keep <- nc[seq_len(min(1, length(nc)))]
      updateSelectizeInput(session, "vars", choices = nc, selected = keep)
      g <- isolate(input$grp); if (is.null(g) || !(g %in% gc)) g <- ""
      updateSelectizeInput(session, "grp", choices = c("None" = "", setNames(gc, gc)), selected = g)
    })
    output$subtitle <- renderUI({
      d <- ds(); if (is.null(d)) return(span("Add a dataset in Data Studio, or open a class example."))
      span(sprintf("Dataset: %s · %d rows", d$name, nrow(dat())))
    })
    num <- function(col) suppressWarnings(as.numeric(dat()[[col]]))
    vars <- reactive(intersect(input$vars %||% character(0), names(dat())))
    pct <- reactive({ p <- input$pct; if (is.null(p) || is.na(p) || p <= 0 || p >= 100) 90 else p })
    grp <- reactive({ g <- input$grp %||% ""; if (nzchar(g) && g %in% names(dat())) g else "" })
    focus <- reactive({ v <- vars(); f <- input$focus; if (!is.null(f) && f %in% v) f else v[1] })
    stats_all <- reactive({ v <- vars(); req(length(v)); lapply(setNames(v, v), function(c) desc_stats(num(c), pct())) })
    gfac <- reactive({ g <- grp(); req(nzchar(g)); x <- as.character(dat()[[g]]); factor(x, levels = level_order(x)) })

    output$body <- renderUI({
      if (is.null(ds())) return(kcard(div(class = "empty", icon("bar", 28), h3(class = "ct", "No data yet"),
                                          p(class = "small", "Upload a file or open a class example, then come back."),
                                          tags$button(class = "btn-k bp", onclick = nav_js("data"), icon("database", 15), "Open Data Studio"))))
      v <- vars()
      if (!length(v)) return(note(kind = "w", ic = "alert", "Choose at least one numeric variable. If a column is missing, set its type to Number in Data Studio."))
      st <- stats_all(); g <- grp()
      tagList(
        div(class = if (length(v) == 1) "split" else "col", style = if (length(v) == 1) "grid-template-columns:minmax(0,1fr) minmax(0,2fr)",
            kcard(h2(class = "ct", if (length(v) == 1) v else "Summary"),
                  div(class = "tscroll", style = "margin-top:8px",
                      tags$table(class = "kt", tags$thead(tags$tr(tags$th("Statistic"), lapply(v, function(x) tags$th(class = "n", x)))),
                                 tags$tbody(lapply(DESC_ROWS(pct()), function(r) tags$tr(tags$td(r[2]), lapply(v, function(x) tags$td(class = "n", fmt_stat(r[1], st[[x]][[r[1]]]))))))))),
            div(class = "col",
                if (length(v) > 1) div(class = "hrow", span(class = "small", "Charts for"), div(style = "width:220px", selectInput(ns("focus"), NULL, v, isolate(focus())))),
                div(class = "grid g2",
                    kcard(div(class = "hrow", h2(class = "ct", "Histogram with normal curve"), dl_link("hist")), plotOutput(ns("pl_hist"), height = "260px")),
                    kcard(div(class = "hrow", h2(class = "ct", if (nzchar(g)) sprintf("Box plot by %s", g) else "Box plot"), dl_link("box")), plotOutput(ns("pl_box"), height = "260px"))),
                div(class = "grid g2",
                    kcard(div(class = "hrow", h2(class = "ct", "Normal QQ plot"), dl_link("qq")), plotOutput(ns("pl_qq"), height = "240px")),
                    kcard(h2(class = "ct", "Stem-and-leaf"), tags$pre(class = "mono", style = "margin-top:10px;font-size:12.5px;max-height:240px;overflow:auto;background:var(--surface2);border-radius:8px;padding:10px;color:var(--text)",
                                                                       textOutput(ns("stem"), inline = TRUE)))),
                if (nzchar(g)) kcard(h2(class = "ct", sprintf("%s by %s", focus(), g)), div(class = "tscroll", style = "margin-top:10px", uiOutput(ns("bygrp")))))),
        if (length(v) >= 2) div(class = "grid g2",
            kcard(div(class = "col", style = "gap:10px",
                      div(class = "hrow", h2(class = "ct", "Scatter and correlation"), dl_link("sc")),
                      div(class = "grid g2", selectInput(ns("cx"), "X", v, isolate(input$cx) %||% v[1]), selectInput(ns("cy"), "Y", v, isolate(input$cy) %||% v[2])),
                      plotOutput(ns("pl_sc"), height = "260px"), uiOutput(ns("cor")))),
            kcard(div(class = "hrow", h2(class = "ct", "Correlation heat map (Pearson)"), dl_link("heat")), plotOutput(ns("pl_heat"), height = "360px")))
        else note("Choose two or more variables to see the scatter plot, covariance and the Pearson, Spearman and Kendall correlations."))
    })
    dl_link <- function(id) div(class = "ml", downloadLink(ns(paste0("dl_", id)), title = "Download PNG", class = "ib", style = "width:30px;height:30px", icon("download", 14)))
    reg <- function(id, fun, h = 280) {
      output[[paste0("pl_", id)]] <- renderPlot(fun(), res = 96, bg = "transparent")
      output[[paste0("dl_", id)]] <- downloadHandler(filename = function() paste0("desc_", id, ".png"),
        content = function(file) ggplot2::ggsave(file, fun() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA)), width = 7, height = h / 96 * 1.1, dpi = 200, bg = "white"))
    }
    reg("hist", function() { req(focus()); plot_hist(num(focus()), focus()) })
    reg("box", function() { y <- num(focus()); req(length(y)); if (nzchar(grp())) { g <- gfac(); req(length(g) == length(y)); plot_box_by(y, g, focus()) } else plot_box_by(y, factor(rep(focus(), length(y))), focus()) })
    reg("qq", function() { req(focus()); plot_qq_vec(num(focus())) })
    reg("sc", function() { req(input$cx, input$cy); plot_scatter(num(input$cx), num(input$cy), input$cx, input$cy) })
    reg("heat", function() { v <- vars(); req(length(v) >= 2); m <- stats::cor(sapply(v, num), use = "pairwise.complete.obs"); plot_corr_heat(m) }, 360)
    output$stem <- renderText({ x <- num(focus()); x <- x[is.finite(x)]; req(length(x) > 1); paste(utils::capture.output(graphics::stem(x)), collapse = "\n") })
    output$bygrp <- renderUI({
      y <- num(focus()); g <- gfac(); lv <- levels(g); cols <- sample_cols(length(lv))
      tags$table(class = "kt", tags$thead(tags$tr(tags$th(grp()), lapply(c("n", "Mean", "Median", "SD", "Min", "Max", "Q1", "Q3"), function(h) tags$th(class = "n", h)))),
                 tags$tbody(lapply(seq_along(lv), function(i) { x <- y[g %in% lv[i] & is.finite(y)]
                   q <- if (length(x)) stats::quantile(x, c(.25, .75), names = FALSE) else c(NA, NA)
                   tags$tr(tags$td(span(style = sprintf("display:inline-block;width:10px;height:10px;border-radius:3px;background:%s;margin-right:8px", cols[i])), lv[i]),
                           tags$td(class = "n", length(x)), tags$td(class = "n", fmt_num(mean(x))), tags$td(class = "n", fmt_num(stats::median(x))),
                           tags$td(class = "n", fmt_num(if (length(x) > 1) stats::sd(x) else NA)), tags$td(class = "n", fmt_num(min(x))), tags$td(class = "n", fmt_num(max(x))),
                           tags$td(class = "n", fmt_num(q[1])), tags$td(class = "n", fmt_num(q[2]))) })))
    })
    output$cor <- renderUI({
      req(input$cx, input$cy); x <- num(input$cx); y <- num(input$cy); ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]
      req(length(x) >= 3)
      ct <- function(m) quiet(stats::cor.test(x, y, method = m, exact = FALSE))
      rows <- list(c("Pearson (parametric)", "pearson"), c("Spearman (non-parametric)", "spearman"), c("Kendall (non-parametric)", "kendall"))
      tagList(
        div(class = "kv-row", span("Covariance"), span(fmt_num(stats::cov(x, y)))),
        tags$table(class = "kt", style = "margin-top:6px", tags$thead(tags$tr(tags$th("Correlation"), tags$th(class = "n", "Coefficient"), tags$th(class = "n", "p-value"))),
                   tags$tbody(lapply(rows, function(r) { t <- ct(r[2])
                     tags$tr(tags$td(r[1]), tags$td(class = "n", fmt_num(stats::cor(x, y, method = r[2]))), tags$td(class = "n", if (is.null(t)) "—" else fmt_p(t$p.value, 4))) }))),
        p(class = "small", style = "margin:8px 0 0", sprintf("n = %d complete pairs. The p-value tests whether the correlation is zero.", length(x))))
    })
    summary_tab <- function() {
      st <- stats_all(); rows <- DESC_ROWS(pct())
      tab <- data.frame(Statistic = vapply(rows, `[`, "", 2), check.names = FALSE)
      for (v in names(st)) tab[[v]] <- vapply(rows, function(r) fmt_stat(r[1], st[[v]][[r[1]]]), "")
      tab
    }
    pin_observers(input, session, store, items = reactive({
      req(ds(), length(vars())); f <- focus(); g <- grp()
      list(summary = list(label = "Summary table", default = TRUE, build = function() mk_pin("table", "Descriptive statistics", "sigma", df = summary_tab())),
           hist = list(label = sprintf("Histogram of %s", f), default = TRUE, build = function() mk_pin("plot", sprintf("Histogram of %s", f), "bar", plot = plot_hist(num(f), f), h = 3)),
           box = list(label = if (nzchar(g)) sprintf("Box plot of %s by %s", f, g) else sprintf("Box plot of %s", f), default = nzchar(g), build = function() {
             y <- num(f); gg <- if (nzchar(g)) gfac() else factor(rep(f, length(y)))
             mk_pin("plot", if (nzchar(g)) sprintf("%s by %s", f, g) else sprintf("Box plot of %s", f), "bar", plot = plot_box_by(y, gg, f), h = 3.2) }))
    }), src = reactive({ d <- ds()
      list(key = "desc", label = sprintf("Descriptive \u00b7 %s", paste(vars(), collapse = ", ")), title = "Descriptive statistics",
           subtitle = sprintf("Dataset %s \u00b7 %d rows", d$name, nrow(dat())), dataset = d$name, notes = d$notes, code = "") }))

    output$dl <- downloadHandler(filename = function() sprintf("descriptive_%s.xlsx", format(Sys.Date())),
      content = function(file) {
        st <- stats_all(); rows <- DESC_ROWS(pct())
        tab <- data.frame(Statistic = vapply(rows, `[`, "", 2), check.names = FALSE)
        for (v in names(st)) tab[[v]] <- vapply(rows, function(r) fmt_stat(r[1], st[[v]][[r[1]]]), "")
        out <- list(Summary = tab)
        if (nzchar(grp())) { y <- num(focus()); g <- gfac()
          out$ByGroup <- do.call(rbind, lapply(levels(g), function(l) { x <- y[g %in% l & is.finite(y)]
            data.frame(group = l, n = length(x), mean = mean(x), median = stats::median(x), sd = if (length(x) > 1) stats::sd(x) else NA, min = min(x), max = max(x)) })) }
        writexl::write_xlsx(out, file)
      })

    list(ai = function(full) {
      d <- ds(); if (is.null(d)) return(NULL)
      v <- vars()
      list(label = if (length(v)) sprintf("descriptive statistics of %s", paste(v, collapse = ", ")) else sprintf("dataset %s", d$name), kind = "data",
           text = if (full) paste0(if (length(v)) paste0("Descriptive statistics table shown (variables: ", paste(v, collapse = ", "),
                                                        if (nzchar(grp())) paste0("; grouped plots by ", grp()) else "", "):\n",
                                                        tryCatch(tbl_text(summary_tab()), error = function(e) "(not available)"), "\n\n") else "",
                                   ctx_dataset(d)))
    })
  })
}
