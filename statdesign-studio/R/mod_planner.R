# Design Planner: randomise a CRD, RCBD, Latin or Graeco-Latin layout before collecting data.

PLAN_DESIGNS <- c("CRD" = "CRD", "RCBD" = "RCBD", "Latin" = "LSD", "Graeco-Latin" = "GLSD")
PLAN_NAMES <- c(CRD = "completely randomised design", RCBD = "randomised complete block design", LSD = "Latin square", GLSD = "Graeco-Latin square")
nz <- function(x, d) if (is.null(x) || !nzchar(trimws(x))) d else trimws(x)
PLAN_COLOURS <- c("#008540", "#003E51", "#AA8A00", "#7A4EAB", "#A42A1F", "#4B8BBE", "#D9822B", "#00573F", "#6B6F7B")
GREEK <- c("α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι")

# Multiplication in GF(2^k), used to build orthogonal squares of order 4 and 8.
gf2_mult <- function(a, b, poly, k) {
  r <- 0L
  while (b > 0) {
    if (bitwAnd(b, 1L)) r <- bitwXor(r, a)
    b <- bitwShiftR(b, 1L); a <- bitwShiftL(a, 1L)
    if (bitwAnd(a, bitwShiftL(1L, k))) a <- bitwXor(a, poly)
  }
  r
}

# Two orthogonal Latin squares of order p, as 0-based integer matrices.
orthogonal_pair <- function(p) {
  i <- matrix(0:(p - 1), p, p); j <- t(i)
  if (p %in% c(3, 5, 7)) return(list((i + j) %% p, (2 * i + j) %% p))
  if (p %in% c(4, 8)) {
    k <- log2(p); poly <- if (p == 4) 7L else 11L
    m1 <- matrix(bitwXor(as.integer(i), as.integer(j)), p, p)
    m2 <- matrix(mapply(function(a, b) bitwXor(gf2_mult(2L, a, poly, k), b), as.integer(i), as.integer(j)), p, p)
    return(list(m1, m2))
  }
  if (p == 9) {
    s <- orthogonal_pair(3)
    kron <- function(a, b) { out <- matrix(0, 9, 9); for (r in 0:2) for (c in 0:2) out[r * 3 + 1:3, c * 3 + 1:3] <- a[r + 1, c + 1] * 3 + b; out }
    return(list(kron(s[[1]], s[[1]]), kron(s[[2]], s[[2]])))
  }
  NULL
}

# Randomised plan: one row per plot, with grid position and labels.
plan_layout <- function(design, trt, seed, reps = 3, nblocks = 3, greek = NULL, rows = NULL, cols = NULL) {
  set.seed(seed)
  t <- length(trt)
  if (design == "CRD") {
    a <- sample(rep(seq_len(t), reps)); n <- length(a); nc <- t
    return(data.frame(plot = seq_len(n), r = (seq_len(n) - 1) %/% nc + 1, c = (seq_len(n) - 1) %% nc + 1, trt = a, greek = NA))
  }
  if (design == "RCBD") {
    a <- unlist(lapply(seq_len(nblocks), function(b) sample(t)))
    return(data.frame(plot = seq_along(a), r = rep(seq_len(nblocks), each = t), c = rep(seq_len(t), nblocks), trt = a, greek = NA))
  }
  p <- t
  sq <- if (design == "GLSD") orthogonal_pair(p) else list(outer(0:(p - 1), 0:(p - 1), function(a, b) (a + b) %% p))
  if (is.null(sq)) return(NULL)
  ro <- sample(p); co <- sample(p); lt <- sample(p); gk <- sample(p)
  L <- sq[[1]][ro, co, drop = FALSE]
  G <- if (design == "GLSD") sq[[2]][ro, co, drop = FALSE] else NULL
  d <- expand.grid(c = seq_len(p), r = seq_len(p))[, c("r", "c")]
  d$plot <- seq_len(nrow(d))
  d$trt <- lt[L[cbind(d$r, d$c)] + 1]
  d$greek <- if (!is.null(G)) gk[G[cbind(d$r, d$c)] + 1] else NA
  d[, c("plot", "r", "c", "trt", "greek")]
}

plan_valid <- function(design, d, t) {
  if (is.null(d)) return(FALSE)
  if (design == "CRD") return(length(unique(table(d$trt))) == 1)
  if (design == "RCBD") return(all(tapply(d$trt, d$r, function(x) length(unique(x)) == t)))
  ok <- all(tapply(d$trt, d$r, function(x) length(unique(x)) == t)) && all(tapply(d$trt, d$c, function(x) length(unique(x)) == t))
  if (design == "GLSD") ok <- ok && all(tapply(d$greek, d$r, function(x) length(unique(x)) == t)) &&
    all(tapply(d$greek, d$c, function(x) length(unique(x)) == t)) && !anyDuplicated(paste(d$trt, d$greek))
  ok
}

plan_error_df <- function(design, t, reps, nblocks) switch(design,
  CRD = t * (reps - 1), RCBD = (t - 1) * (nblocks - 1), LSD = (t - 1) * (t - 2), GLSD = (t - 1) * (t - 3))

# Power to detect a difference delta between two treatment means, the others at the grand mean.
plan_power <- function(design, t, n_per, dfe, delta, sigma, alpha = 0.05) {
  if (!is.finite(delta) || !is.finite(sigma) || sigma <= 0 || dfe < 1) return(NA)
  ncp <- n_per * delta^2 / (2 * sigma^2)
  1 - stats::pf(stats::qf(1 - alpha, t - 1, dfe), t - 1, dfe, ncp)
}

planner_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Design Planner", "Plan and randomise your experiment before you collect any data."),
    div(class = "split plan",
        div(class = "col",
            kcard(div(class = "col", style = "gap:14px",
                      h2(class = "ct", "Plan"),
                      div(tags$label(class = "control-label", "Design"), seg_input(ns("design"), PLAN_DESIGNS, "GLSD")),
                      div(class = "grid g2", style = "gap:12px",
                          textInput(ns("trt_name"), "Treatment factor", "Variety"),
                          textInput(ns("resp_name"), "Response to measure", "Yield")),
                      selectizeInput(ns("trt"), "Treatments", choices = c("A", "B", "C", "D"), selected = c("A", "B", "C", "D"),
                                     multiple = TRUE, width = "100%", options = list(create = TRUE, plugins = list("remove_button"), placeholder = "Type a name and press Enter")),
                      uiOutput(ns("extra")),
                      div(tags$label(class = "control-label", "Random seed"),
                          div(class = "seed-row",
                              numericInput(ns("seed"), NULL, as.integer(format(Sys.Date(), "%Y%m%d")), width = "100%"),
                              tags$button(class = "ib action-button", id = ns("new_seed"), type = "button", title = "New random seed", icon("shuffle", 16)))),
                      btn(ns("go"), "Randomise layout", "shuffle", "bp", style = "width:100%"))),
            kcard(div(class = "col", style = "gap:10px",
                      h2(class = "ct", "Power and sample size"),
                      uiOutput(ns("power"))))),
        div(class = "col",
            kcard(div(class = "hrow", style = "flex-wrap:wrap;gap:10px",
                      h2(class = "ct", "Field layout"), uiOutput(ns("valid"), inline = TRUE),
                      div(class = "ml hrow",
                          downloadButton(ns("dl_map"), label = tagList(icon("download", 14), "Field map (PDF)"), icon = NULL, class = "btn-k bs bsm"),
                          downloadButton(ns("dl_sheet"), label = tagList(icon("sheet", 14), "Data sheet (Excel)"), icon = NULL, class = "btn-k bp bsm"))),
                  div(class = "plan-wrap", uiOutput(ns("grid")), uiOutput(ns("legend")))),
            kcard(div(class = "hrow", style = "flex-wrap:wrap", h2(class = "ct", "Data collection sheet preview"),
                      span(class = "small ml", "Fill in the response column, then upload the sheet in Data Studio")),
                  div(class = "tscroll", style = "margin-top:10px", uiOutput(ns("sheet"))))))
  )
}

planner_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    design <- reactive(input$design %||% "GLSD")

    output$extra <- renderUI({
      d <- design()
      lev <- function(id, label, choices) selectizeInput(ns(id), label, choices = choices, selected = choices, multiple = TRUE, width = "100%",
                                                         options = list(create = TRUE, plugins = list("remove_button"), placeholder = "Leave empty to number them"))
      switch(d,
        CRD = div(class = "grid g2", style = "gap:12px", numericInput(ns("reps"), "Replicates per treatment", 4, min = 2, max = 50, step = 1)),
        RCBD = div(class = "grid g2", style = "gap:12px",
                   textInput(ns("blk_name"), "Block factor", "Farm"),
                   numericInput(ns("nblocks"), "Number of blocks", 4, min = 2, max = 30, step = 1)),
        tagList(
          if (d == "GLSD") tagList(
            textInput(ns("gk_name"), "Greek factor", "Fertilizer"),
            selectizeInput(ns("gk"), "Greek factor levels", choices = c("Organic", "Chemical", "Mixed", "Untreated"), selected = c("Organic", "Chemical", "Mixed", "Untreated"),
                           multiple = TRUE, width = "100%", options = list(create = TRUE, plugins = list("remove_button"), placeholder = "One level per treatment"))),
          div(class = "grid g2", style = "gap:12px",
              textInput(ns("row_name"), "Rows", if (d == "GLSD") "Soil quality" else "Row"),
              textInput(ns("col_name"), "Columns", if (d == "GLSD") "Irrigation" else "Column")),
          div(class = "grid g2", style = "gap:12px",
              lev("row_lv", "Row labels", if (d == "GLSD") c("High", "Medium-high", "Medium-low", "Low") else character(0)),
              lev("col_lv", "Column labels", if (d == "GLSD") c("Very low", "Low", "Medium", "High") else character(0)))))
    })

    observeEvent(input$new_seed, updateNumericInput(session, "seed", value = sample.int(99999999, 1)))

    cfg <- reactive({
      d <- design(); trt <- input$trt %||% character(0)
      list(design = d, trt = trt, t = length(trt), seed = input$seed %||% 1,
           reps = max(2, as.integer(input$reps %||% 4)), nblocks = max(2, as.integer(input$nblocks %||% 4)),
           trt_name = nz(input$trt_name, "Treatment"), resp = nz(input$resp_name, "Response"),
           blk_name = nz(input$blk_name, "Block"),
           gk_name = nz(input$gk_name, "Greek"), gk = input$gk %||% character(0),
           row_name = nz(input$row_name, "Row"), col_name = nz(input$col_name, "Column"),
           row_lv = input$row_lv %||% character(0), col_lv = input$col_lv %||% character(0))
    })

    problem <- reactive({
      c <- cfg()
      if (c$t < 2) return("Add at least two treatments.")
      if (anyDuplicated(c$trt)) return("Treatment names must be different.")
      if (c$design %in% c("LSD", "GLSD") && c$t > 9) return("Squares larger than 9 × 9 are not supported.")
      if (c$design == "GLSD" && c$t == 6) return("A 6 × 6 Graeco-Latin square does not exist. Use 4, 5, 7, 8 or 9 treatments.")
      if (c$design == "GLSD" && c$t == 2) return("A Graeco-Latin square needs at least 3 treatments.")
      if (c$design == "GLSD" && length(c$gk) != c$t)
        return(sprintf("The Greek factor needs exactly %d levels, one for each treatment (you have %d).", c$t, length(c$gk)))
      NULL
    })

    plan <- reactiveVal(NULL)
    make_plan <- function() {
      c <- cfg()
      if (!is.null(problem())) { plan(NULL); return() }
      d <- plan_layout(c$design, c$trt, c$seed, c$reps, c$nblocks)
      if (is.null(d)) { plan(NULL); return() }
      plan(list(cfg = c, d = d))
    }
    observeEvent(input$go, make_plan())
    # Keep the layout in step with the plan, but only re-randomise on design or size changes and the button.
    observeEvent(list(design(), length(input$trt), input$reps, input$nblocks, length(input$gk)), make_plan(), ignoreNULL = FALSE)
    observeEvent(list(input$trt, input$trt_name, input$resp_name, input$blk_name, input$gk_name, input$gk,
                      input$row_name, input$col_name, input$row_lv, input$col_lv), {
      p <- plan()
      if (!is.null(p) && is.null(problem()) && length(cfg()$trt) == p$cfg$t) { seed <- p$cfg$seed; p$cfg <- cfg(); p$cfg$seed <- seed; plan(p) }
      else make_plan()
    }, ignoreInit = TRUE)

    # Labels for rows and columns of the grid.
    axis_labels <- function(c, d) {
      p <- c$t
      switch(c$design,
        CRD = list(rows = paste("Row", seq_len(max(d$r))), cols = paste("Position", seq_len(p)), rname = "Row", cname = "Position"),
        RCBD = list(rows = paste(c$blk_name, seq_len(c$nblocks)), cols = paste("Plot", seq_len(p)), rname = c$blk_name, cname = "Plot in block"),
        list(rows = if (length(c$row_lv) == p) c$row_lv else paste(c$row_name, seq_len(p)),
             cols = if (length(c$col_lv) == p) c$col_lv else paste(c$col_name, seq_len(p)),
             rname = c$row_name, cname = c$col_name))
    }

    sheet_df <- reactive({
      p <- plan(); req(p); c <- p$cfg; d <- p$d; ax <- axis_labels(c, d)
      out <- data.frame(Plot = sprintf("%02d", d$plot), check.names = FALSE)
      if (c$design == "RCBD") out[[c$blk_name]] <- ax$rows[d$r]
      if (c$design %in% c("LSD", "GLSD")) { out[[c$row_name]] <- ax$rows[d$r]; out[[c$col_name]] <- ax$cols[d$c] }
      out[[c$trt_name]] <- c$trt[d$trt]
      if (c$design == "GLSD") out[[c$gk_name]] <- c$gk[d$greek]
      out[[c$resp]] <- NA_real_
      names(out) <- make.unique(names(out), sep = " ")
      out
    })

    output$valid <- renderUI({
      pr <- problem(); if (!is.null(pr)) return(pill("Not ready", "warn", "alert"))
      p <- plan(); req(p)
      nm <- PLAN_NAMES[[p$cfg$design]]
      if (plan_valid(p$cfg$design, p$d, p$cfg$t)) pill(sprintf("Valid %s", nm), "pass", "check") else pill("Check the layout", "fail", "alert")
    })

    tile_col <- function(i) PLAN_COLOURS[(i - 1) %% length(PLAN_COLOURS) + 1]

    output$grid <- renderUI({
      pr <- problem(); if (!is.null(pr)) return(note(kind = "w", ic = "alert", pr))
      p <- plan(); req(p); c <- p$cfg; d <- p$d; ax <- axis_labels(c, d)
      nc <- max(d$c)
      tile <- function(k) {
        x <- d[k, ]; g <- if (!is.na(x$greek)) x$greek else NULL
        sub <- sprintf("Plot %02d%s", x$plot, if (!is.null(g)) paste0(" · ", c$gk[g]) else "")
        div(class = "ptile", style = sprintf("background:%s", tile_col(x$trt)), title = sprintf("%s: %s", c$trt_name, c$trt[x$trt]),
            div(class = "pl", LETTERS[x$trt], if (!is.null(g)) span(class = "gk", GREEK[g])), div(class = "ps", sub))
      }
      tags$table(class = "pgrid",
        tags$thead(tags$tr(tags$th(""), lapply(seq_len(nc), function(j) tags$th(ax$cols[j])))),
        tags$tbody(lapply(seq_len(max(d$r)), function(i) tags$tr(
          tags$th(scope = "row", ax$rows[i]),
          lapply(seq_len(nc), function(j) { k <- which(d$r == i & d$c == j); tags$td(if (length(k)) tile(k)) })))))
    })

    output$legend <- renderUI({
      p <- plan(); req(p, is.null(problem())); c <- p$cfg
      lab <- function(x) if (nchar(x) <= 2) paste(c$trt_name, x) else x
      div(class = "plegend",
          div(class = "lt", "Legend"),
          lapply(seq_len(c$t), function(i) div(class = "li", span(class = "sw", style = sprintf("background:%s", tile_col(i))),
                                               span(tags$b(LETTERS[i]), " ", lab(c$trt[i])))),
          if (c$design == "GLSD") tagList(div(class = "lsep"),
            lapply(seq_len(c$t), function(i) div(class = "li", span(class = "gsym", GREEK[i]), span(c$gk[i])))))
    })

    output$sheet <- renderUI({
      req(is.null(problem())); s <- sheet_df(); resp <- ncol(s)
      n <- nrow(s); show <- head(s, 6)
      tagList(
        tags$table(class = "kt",
          tags$thead(tags$tr(lapply(seq_along(show), function(j) tags$th(class = if (j == resp) "resp", names(show)[j])))),
          tags$tbody(lapply(seq_len(nrow(show)), function(i) tags$tr(lapply(seq_along(show), function(j)
            tags$td(class = if (j == resp) "resp", if (j == resp) "" else as.character(show[[j]][i]))))))),
        if (n > 6) p(class = "small", style = "margin:8px 0 0", sprintf("Showing 6 of %d plots. The Excel file has them all.", n)))
    })

    output$power <- renderUI({
      c <- cfg()
      if (c$t < 2) return(p(class = "small", style = "margin:0", "Add treatments to see the plan size."))
      dfe <- plan_error_df(c$design, c$t, c$reps, c$nblocks)
      n <- switch(c$design, CRD = c$t * c$reps, RCBD = c$t * c$nblocks, c$t^2)
      size <- switch(c$design,
        CRD = sprintf("%d treatments × %d replicates: %d plots, %d error df.", c$t, c$reps, n, dfe),
        RCBD = sprintf("%d treatments × %d blocks: %d plots, %d error df.", c$t, c$nblocks, n, dfe),
        sprintf("%d × %d square: %d plots, %d error df.", c$t, c$t, n, dfe))
      warn <- if (c$design == "GLSD" && c$t == 3) note(kind = "w", ic = "alert", "A 3 × 3 Graeco-Latin square leaves 0 error df and cannot be analysed. Use 4 × 4 or larger.")
        else if (c$design == "LSD" && c$t == 3) note(kind = "w", ic = "alert", "A 3 × 3 Latin square leaves only 2 error df, so the F test has very little power. Use 4 × 4 or larger if you can.")
        else if (dfe < 6) note(kind = "w", ic = "alert", sprintf("Only %d error df. Tests with fewer than about 6 error df have little power; add replicates or blocks.", dfe))
        else note(ic = "info", sprintf("%d error df is enough for the F test to work well.", dfe))
      tagList(p(class = "small", style = "margin:0", size), warn,
              div(class = "grid g2", style = "gap:12px;margin-top:4px",
                  numericInput(ns("pw_delta"), "Difference worth detecting", NA, min = 0),
                  numericInput(ns("pw_sd"), "Expected SD of the response", NA, min = 0)),
              uiOutput(ns("power_out")))
    })

    output$power_out <- renderUI({
      c <- cfg(); delta <- input$pw_delta; sd <- input$pw_sd
      if (is.null(delta) || is.null(sd) || is.na(delta) || is.na(sd) || sd <= 0 || delta <= 0)
        return(p(class = "small", style = "margin:0", "Enter a difference and an SD (from a pilot study or past data) to estimate the power at α = 0.05."))
      dfe <- plan_error_df(c$design, c$t, c$reps, c$nblocks)
      n_per <- switch(c$design, CRD = c$reps, RCBD = c$nblocks, c$t)
      pw <- plan_power(c$design, c$t, n_per, dfe, delta, sd)
      if (is.na(pw)) return(note(kind = "w", ic = "alert", "There are no error df, so power cannot be computed."))
      need <- NULL
      if (c$design %in% c("CRD", "RCBD") && pw < 0.8) {
        for (r in (n_per + 1):60) {
          pr <- plan_power(c$design, c$t, r, plan_error_df(c$design, c$t, r, r), delta, sd)
          if (!is.na(pr) && pr >= 0.8) { need <- r; break }
        }
      }
      what <- if (c$design == "CRD") "replicates per treatment" else "blocks"
      tagList(
        div(class = "kv-row", span("Power to detect a difference of ", format(delta), " between two treatments"),
            tags$b(style = sprintf("color:%s", if (pw >= 0.8) "var(--green)" else "var(--warn)"), sprintf("%.0f%%", 100 * pw))),
        if (!is.null(need)) p(class = "small", style = "margin:6px 0 0", sprintf("About %d %s would give 80%% power.", need, what))
        else if (pw < 0.8 && c$design %in% c("LSD", "GLSD")) p(class = "small", style = "margin:6px 0 0", "To gain power with a square, use more treatments per side or repeat the square.")
      )
    })

    map_plot <- function() {
      p <- plan(); req(p); c <- p$cfg; d <- p$d; ax <- axis_labels(c, d)
      d$lab <- paste0(LETTERS[d$trt], ifelse(is.na(d$greek), "", GREEK[pmax(1, d$greek)]))
      d$sub <- sprintf("Plot %02d", d$plot)
      d$fill <- factor(c$trt[d$trt], levels = c$trt)
      ggplot2::ggplot(d, ggplot2::aes(c, r)) +
        ggplot2::geom_tile(ggplot2::aes(fill = fill), colour = "white", linewidth = 2) +
        ggplot2::geom_text(ggplot2::aes(label = lab), colour = "white", fontface = "bold", size = 7, vjust = -0.1) +
        ggplot2::geom_text(ggplot2::aes(label = sub), colour = "white", size = 3.2, vjust = 2.2) +
        ggplot2::scale_fill_manual(values = setNames(tile_col(seq_len(c$t)), c$trt), name = c$trt_name) +
        ggplot2::scale_x_continuous(breaks = seq_along(ax$cols), labels = ax$cols, position = "top", expand = c(0, 0)) +
        ggplot2::scale_y_reverse(breaks = seq_along(ax$rows), labels = ax$rows, expand = c(0, 0)) +
        ggplot2::labs(x = ax$cname, y = ax$rname, title = sprintf("Field map: %s", PLAN_NAMES[[c$design]]),
                      subtitle = paste0(sprintf("Random seed %s", format(c$seed)),
                                        if (c$design == "GLSD") paste0("   ", c$gk_name, ": ", paste(GREEK[seq_len(c$t)], c$gk, collapse = ", ")) else "")) +
        ggplot2::coord_equal() + ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "bottom", plot.title = ggplot2::element_text(face = "bold"))
    }

    output$dl_map <- downloadHandler(
      filename = function() sprintf("field_map_%s.pdf", tolower(design())),
      content = function(file) { p <- plan(); n <- max(p$d$c); m <- max(p$d$r)
        ggplot2::ggsave(file, map_plot(), width = max(6, 1.5 * n + 2.5), height = max(5, 1.3 * m + 2.5), device = grDevices::cairo_pdf) })
    output$dl_sheet <- downloadHandler(
      filename = function() sprintf("data_sheet_%s.xlsx", tolower(design())),
      content = function(file) {
        p <- plan(); c <- p$cfg
        info <- data.frame(Item = c("Design", "Treatments", "Random seed", "Plots", "Error df", "Created"),
                           Value = c(PLAN_NAMES[[c$design]], paste(c$trt, collapse = ", "), format(c$seed), nrow(p$d),
                                     plan_error_df(c$design, c$t, c$reps, c$nblocks), format(Sys.Date())))
        writexl::write_xlsx(list(`Data sheet` = sheet_df(), Plan = info), file)
      })

    list(ai = function(full) {
      c <- cfg(); pr <- problem()
      list(label = sprintf("your %s plan", PLAN_NAMES[[c$design]]), kind = "general",
           text = if (full) paste0(sprintf("Design Planner: %s with %d treatments (%s), seed %s.\n", PLAN_NAMES[[c$design]], c$t, paste(c$trt, collapse = ", "), format(c$seed)),
                                   if (!is.null(pr)) paste0("Problem shown: ", paste(unlist(pr), collapse = " "), "\n")
                                   else paste0(sprintf("Error df: %s.\n", tryCatch(plan_error_df(c$design, c$t, c$reps, c$nblocks), error = function(e) "?")),
                                               "Randomised data sheet:\n", tryCatch(df_text(sheet_df(), 80), error = function(e) "(not available)"))))
    })
  })
}
