# One wizard for one, two and k sample comparisons: data, variables, settings, results.

CMP <- list(
  one = list(key = "one", title = "One sample", short = "one-sample", icon = "target", example = "crd",
             how = list(p("Compare one sample with a target value, for example “is the mean yield different from 50 kg?”"),
                        tags$ul(class = "small", style = "padding-left:18px",
                                tags$li(tags$b("t-test: "), "compares the mean; needs roughly normal data or n ≥ 30."),
                                tags$li(tags$b("z-test: "), "the same, when the population standard deviation σ is known."),
                                tags$li(tags$b("Wilcoxon signed-rank: "), "compares the median; needs a symmetric shape, not normality."),
                                tags$li(tags$b("Sign test: "), "compares the median with no shape assumption at all.")))),
  two = list(key = "two", title = "Two samples", short = "two-sample", icon = "columns", example = "crd",
             how = list(p("Compare two groups. Choose independent when the groups contain different units, and paired when the same units are measured twice."),
                        tags$ul(class = "small", style = "padding-left:18px",
                                tags$li(tags$b("Pooled t-test: "), "independent groups with equal variances (checked with the F test and Levene)."),
                                tags$li(tags$b("Welch t-test: "), "independent groups with unequal variances."),
                                tags$li(tags$b("Mann-Whitney U: "), "independent groups without normality."),
                                tags$li(tags$b("Paired t-test and Wilcoxon signed-rank: "), "for paired data, using the differences.")))),
  k = list(key = "k", title = "k samples", short = "k-sample", icon = "layers", example = "crd",
           how = list(p("Compare three or more independent groups without blocking."),
                      tags$ul(class = "small", style = "padding-left:18px",
                              tags$li(tags$b("One-way ANOVA: "), "normal errors and equal variances (Levene, Bartlett). Post-hoc: Tukey HSD."),
                              tags$li(tags$b("Welch ANOVA: "), "normal errors, unequal variances. Post-hoc: Games-Howell."),
                              tags$li(tags$b("Kruskal-Wallis: "), "no normality needed. Post-hoc: Dunn with Holm adjustment."))))
)
CMP_STEPS <- c("Data", "Map variables", "Context & settings", "Results")

cmp_role <- function(ns, id, label, desc, ic, required = TRUE, multiple = FALSE) {
  kcard(div(class = "role", ico_box(ic, if (required) "ig" else "ip"),
            div(class = "rb",
                div(div(class = "hrow", style = "gap:8px", span(style = "font-size:14.5px;font-weight:600;color:var(--ink)", label),
                        pill(if (required) "Required" else "Optional", if (required) "inf" else "neu")),
                    div(class = "small", desc)),
                selectizeInput(ns(id), NULL, choices = NULL, multiple = multiple, width = "100%",
                               options = list(placeholder = if (multiple) "Choose columns" else "Choose a column", plugins = if (multiple) list("remove_button"))))))
}

cmp_layout_card <- function(ns, k) {
  kcard(div(class = "col", style = "gap:10px",
            h3(class = "ct", "How is your data laid out?"),
            div(class = "optcards", radioButtons(ns("layout"), NULL, selected = "stacked",
              choiceNames = list(span(span(class = "oc-t", "One response column and a group column"), span(class = "oc-s", "Stacked: e.g. Yield and Fertilizer")),
                                 span(span(class = "oc-t", "One column per sample"), span(class = "oc-s", if (k == "two") "Unstacked: e.g. columns A and B" else "Unstacked: e.g. columns x1, x2, x3"))),
              choiceValues = list("stacked", "wide")))))
}

cmp_ui <- function(id, kind) {
  ns <- NS(id); K <- CMP[[kind]]
  step2 <- switch(kind,
    one = tagList(cmp_role(ns, "resp", "Measurement", "The numeric column to test.", "bar"),
                  cmp_role(ns, "sub_col", "Use only some rows", "Optional: keep rows where a group column has one value.", "filter", FALSE),
                  conditionalPanel(sprintf("input['%s'] != ''", ns("sub_col")), kcard(selectInput(ns("sub_lev"), "Keep rows where it equals", NULL)))),
    two = tagList(kcard(div(class = "col", style = "gap:10px", h3(class = "ct", "How are the two samples related?"),
                            seg_input(ns("paired"), c("Independent groups" = "ind", "Paired (same units twice)" = "paired"), "ind"),
                            p(class = "small", style = "margin:0", "Paired means each value in one sample belongs with one value in the other: the same plot, person or batch measured twice."))),
                  cmp_layout_card(ns, "two"),
                  conditionalPanel(sprintf("input['%s'] == 'stacked'", ns("layout")),
                                   div(class = "col", style = "gap:14px",
                                       cmp_role(ns, "resp", "Response", "The numeric measurement.", "bar"),
                                       cmp_role(ns, "grp", "Group", "The column that says which sample each row belongs to.", "tag"),
                                       kcard(selectizeInput(ns("lev2"), "Which two groups?", NULL, multiple = TRUE, width = "100%", options = list(maxItems = 2))),
                                       conditionalPanel(sprintf("input['%s'] == 'paired'", ns("paired")),
                                                        cmp_role(ns, "pair", "Pair ID", "Optional: the column that links the two values of a pair, e.g. Block or Subject.", "link", FALSE)))),
                  conditionalPanel(sprintf("input['%s'] == 'wide'", ns("layout")),
                                   div(class = "col", style = "gap:14px",
                                       cmp_role(ns, "c1", "First sample", "Numeric column for sample 1.", "bar"),
                                       cmp_role(ns, "c2", "Second sample", "Numeric column for sample 2.", "bar")))),
    k = tagList(cmp_layout_card(ns, "k"),
                conditionalPanel(sprintf("input['%s'] == 'stacked'", ns("layout")),
                                 div(class = "col", style = "gap:14px",
                                     cmp_role(ns, "resp", "Response", "The numeric measurement.", "bar"),
                                     cmp_role(ns, "grp", "Group", "The column that says which group each row belongs to.", "tag"))),
                conditionalPanel(sprintf("input['%s'] == 'wide'", ns("layout")),
                                 cmp_role(ns, "cols", "Sample columns", "Two or more numeric columns, one per group.", "bar", TRUE, TRUE))))
  tagList(
    page_head(K$title, uiOutput(ns("subtitle"), inline = TRUE), btn(ns("how"), sprintf("How %s tests work", K$short), "book", size = "bsm")),
    uiOutput(ns("stepper")),
    tabsetPanel(id = ns("steps"), type = "hidden",
      tabPanelBody("1", div(class = "split",
        kcard(div(class = "col", style = "gap:16px",
                  div(h2(class = "ct", "Choose the data for this analysis"),
                      p(class = "small", style = "margin:0", "Pick a dataset you added in Data Studio, or start from the class example.")),
                  uiOutput(ns("ds_pick")))),
        div(class = "col",
            kcard(div(class = "col", style = "gap:12px", h3(class = "ct", "Need data?"),
                      p(class = "small", style = "margin:0", "Upload Excel or CSV, type it in, or paste it from a spreadsheet in Data Studio."),
                      tags$button(class = "btn-k bs", onclick = nav_js("data"), icon("database", 15), "Open Data Studio"),
                      btn(ns("load_example"), "Use the class yield example", "flask"))),
            div(class = "hrow", style = "justify-content:flex-end", btn(ns("to2"), "Continue", "arrowr", "bp"))))),
      tabPanelBody("2", div(class = "split wide",
        div(class = "col", style = "gap:14px",
            div(h2(class = "ct", "Tell us where the samples are"),
                p(class = "small", style = "margin:0", "Numeric columns are listed for measurements; any column can be a group.")),
            step2),
        div(class = "col", uiOutput(ns("check")),
            div(class = "hrow", btn(ns("back1"), "Back", "arrowl"), div(class = "ml", btn(ns("to3"), "Continue", "arrowr", "bp")))))),
      tabPanelBody("3", cmp_step3_ui(ns, kind)),
      tabPanelBody("4", div(class = "col", uiOutput(ns("results"), class = "rcol")))))
}

cmp_step3_ui <- function(ns, kind) {
  div(class = "split wide",
      div(class = "col",
          kcard(div(class = "col", style = "gap:14px", h2(class = "ct", "Context"),
                    textInput(ns("title"), "Title", ""),
                    div(class = "grid", style = "grid-template-columns:minmax(0,2fr) minmax(0,1fr)",
                        textInput(ns("resp_name"), "Response name", ""), textInput(ns("unit"), "Unit", "", placeholder = "e.g. kg")),
                    if (kind == "two") div(class = "grid g2", textInput(ns("name1"), "Name of sample 1", ""), textInput(ns("name2"), "Name of sample 2", "")))),
          kcard(div(class = "col", style = "gap:10px", h2(class = "ct", "Hypotheses"),
                    p(class = "small", style = "margin:0", "Written from your settings."), uiOutput(ns("hyp"))))),
      div(class = "col",
          kcard(div(class = "col", style = "gap:16px", h2(class = "ct", "Settings"),
                    div(div(class = "klab", "Significance level (α)"),
                        seg_input(ns("alpha"), c("0.01" = "0.01", "0.05" = "0.05", "0.10" = "0.10", "Custom" = "custom"), "0.05"),
                        conditionalPanel(sprintf("input['%s'] == 'custom'", ns("alpha")),
                                         div(style = "margin-top:8px;width:160px", numericInput(ns("alpha_custom"), NULL, 0.05, min = 0.001, max = 0.5, step = 0.005)))),
                    selectInput(ns("conf"), "Confidence level for intervals", c("90%" = "0.90", "95%" = "0.95", "99%" = "0.99"), "0.95"),
                    if (kind != "k") tagList(
                      div(div(class = "klab", "Alternative hypothesis (H1)"),
                          seg_input(ns("alt"), c("Two-sided ≠" = "two.sided", "Less than <" = "less", "Greater than >" = "greater"), "two.sided")),
                      numericInput(ns("mu"), if (kind == "one") "Target value (μ0)" else "Hypothesised difference (μ1 − μ2)", 0),
                      checkboxInput(ns("known_sigma"), "The population standard deviation σ is known (adds the z-test)", FALSE),
                      conditionalPanel(sprintf("input['%s']", ns("known_sigma")),
                                       div(class = "grid g2", numericInput(ns("sigma"), if (kind == "one") "σ" else "σ of sample 1", NA, min = 0),
                                           if (kind == "two") numericInput(ns("sigma2"), "σ of sample 2", NA, min = 0)))),
                    if (kind %in% c("two", "k")) uiOutput(ns("var_ui")),
                    div(class = "divider"),
                    radioButtons(ns("method"), "Which test to report",
                                 choiceNames = list(tagList(tags$b("Automatic (recommended)"), div(class = "small", "Use the rank-based test if normality fails")),
                                                    "Always the parametric test", "Always the rank-based test"),
                                 choiceValues = list("auto", "param", "nonpar"), selected = "auto"))),
          btn(ns("run"), "Run analysis", "play", "bp", "blg")))
}

cmp_server <- function(id, kind, store, goto) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns; K <- CMP[[kind]]
    step <- reactiveVal(1); maxstep <- reactiveVal(1); ds_id <- reactiveVal(NULL); res <- reactiveVal(NULL)
    auto <- reactiveValues()
    go <- function(i) { step(i); maxstep(max(maxstep(), i)); updateTabsetPanel(session, "steps", selected = as.character(i)) }
    observeEvent(input$goto_step, { if (input$goto_step <= maxstep()) go(input$goto_step) })
    output$stepper <- renderUI(stepper_ui(ns, CMP_STEPS, step(), maxstep()))
    ds <- reactive({ id <- ds_id(); if (is.null(id)) NULL else store$datasets[[id]] })
    dat <- reactive({ d <- ds(); req(d); drop_empty_rows(d$data) })
    output$subtitle <- renderUI({
      d <- ds(); if (is.null(d)) return(span("Choose a dataset to begin."))
      span(sprintf("Dataset: %s · %d rows", d$name, nrow(drop_empty_rows(d$data))))
    })
    observeEvent(input$how, showModal(modalDialog(title = K$title, easyClose = TRUE, footer = modalButton("Close"), K$how)))

    # ---------- Step 1 ----------
    observe({
      if (is.null(ds_id()) && !is.null(store$active) && store$active %in% names(store$datasets)) ds_id(store$active)
      if (!is.null(ds_id()) && !(ds_id() %in% names(store$datasets))) ds_id(NULL)
    })
    output$ds_pick <- renderUI({
      dss <- store$datasets
      if (!length(dss)) return(note(kind = "w", ic = "alert", "You have no datasets yet. Open Data Studio, or use the class example."))
      div(class = "optcards", radioButtons(ns("ds"), NULL, selected = isolate(ds_id()),
        choiceNames = unname(lapply(dss, function(d) span(span(class = "oc-t", d$name),
                                                   span(class = "oc-s", sprintf("%d rows · %d columns · %s", nrow(drop_empty_rows(d$data)), ncol(d$data), d$source))))),
        choiceValues = names(dss)))
    })
    observeEvent(input$ds, { if (!identical(input$ds, ds_id())) ds_id(input$ds) })
    observeEvent(input$load_example, {
      ex <- example_dataset(K$example); store$datasets[[ex$id]] <- ex; store$active <- ex$id; ds_id(ex$id); go(2)
    })
    observeEvent(input$to2, { if (is.null(ds())) { showNotification("Choose a dataset first.", type = "warning"); return() }; go(2) })

    # ---------- Step 2 ----------
    cols_of <- function(type = c("number", "group")) {
      d <- dat(); ty <- ds()$types
      if (type[1] == "number") names(d)[vapply(names(d), function(n) identical(ty[[n]], "number"), TRUE)]
      else names(d)[vapply(names(d), function(n) { k <- length(level_order(d[[n]])); k >= 2 && k <= 50 && k < sum(!is.na(d[[n]])) }, TRUE)]
    }
    lab_num <- function(cols) setNames(cols, vapply(cols, function(n) sprintf("%s · number, %d values", n, sum(is.finite(suppressWarnings(as.numeric(dat()[[n]]))))), ""))
    lab_grp <- function(cols) setNames(cols, vapply(cols, function(n) sprintf("%s · %d levels", n, length(level_order(dat()[[n]]))), ""))
    observeEvent(ds_id(), {
      req(ds()); nc <- cols_of("number"); gc <- cols_of("group")
      gc_first <- c(setdiff(gc, nc), intersect(gc, nc))
      pick1 <- function(x, avoid = character(0)) { x <- setdiff(x, avoid); if (length(x)) x[1] else "" }
      resp <- pick1(nc)
      grp <- pick1(gc_first, resp)
      if (!is.null(input$resp) || TRUE) updateSelectizeInput(session, "resp", choices = lab_num(nc), selected = resp)
      updateSelectizeInput(session, "grp", choices = lab_grp(gc_first), selected = grp)
      updateSelectizeInput(session, "sub_col", choices = c("None" = "", lab_grp(gc_first)), selected = "")
      updateSelectizeInput(session, "pair", choices = c("None" = "", lab_grp(gc_first)), selected = "")
      updateSelectizeInput(session, "c1", choices = lab_num(nc), selected = pick1(nc))
      updateSelectizeInput(session, "c2", choices = lab_num(nc), selected = pick1(nc, pick1(nc)))
      updateSelectizeInput(session, "cols", choices = lab_num(nc), selected = nc[seq_len(min(length(nc), 4))])
      if (kind != "one") {
        # Stacked by default when there is a group column; otherwise one column per sample.
        lay <- if (nzchar(grp) && length(nc) >= 1) "stacked" else "wide"
        updateRadioButtons(session, "layout", selected = lay)
      }
    })
    observeEvent(list(input$grp, ds_id()), {
      req(ds()); g <- input$grp %||% ""
      lv <- if (nzchar(g) && g %in% names(dat())) level_order(dat()[[g]]) else character(0)
      updateSelectizeInput(session, "lev2", choices = lv, selected = lv[seq_len(min(2, length(lv)))])
    })
    observeEvent(list(input$sub_col, ds_id()), {
      req(ds()); g <- input$sub_col %||% ""
      lv <- if (nzchar(g) && g %in% names(dat())) level_order(dat()[[g]]) else character(0)
      updateSelectInput(session, "sub_lev", choices = lv, selected = lv[1])
    })

    numv <- function(col) suppressWarnings(as.numeric(dat()[[col]]))
    samples <- reactive({
      req(ds()); d <- dat(); err <- character(0); warn <- character(0); out <- list()
      has <- function(x) !is.null(x) && nzchar(x) && x %in% names(d)
      lay <- if (kind == "one") "one" else input$layout %||% "stacked"
      paired <- kind == "two" && identical(input$paired, "paired")
      if (kind == "one") {
        if (!has(input$resp)) err <- c(err, "Choose the measurement column.")
        else {
          x <- numv(input$resp); nm <- input$resp
          if (has(input$sub_col) && !is.null(input$sub_lev)) {
            keep <- as.character(d[[input$sub_col]]) == input$sub_lev; keep[is.na(keep)] <- FALSE
            x <- x[keep]; nm <- sprintf("%s (%s = %s)", input$resp, input$sub_col, input$sub_lev)
          }
          out <- list(data = list(x), names = nm, resp = input$resp)
        }
      } else if (lay == "stacked") {
        if (!has(input$resp)) err <- c(err, "Choose the response column.")
        if (!has(input$grp)) err <- c(err, "Choose the group column.")
        if (has(input$resp) && has(input$grp) && input$resp == input$grp) err <- c(err, "The response and the group must be different columns.")
        if (!length(err)) {
          y <- numv(input$resp); g <- as.character(d[[input$grp]]); lv <- level_order(g)
          if (kind == "two") {
            sel <- input$lev2 %||% character(0)
            if (length(sel) != 2) err <- c(err, sprintf("Choose exactly two groups of %s.", input$grp))
            else {
              sel <- lv[lv %in% sel]
              if (paired) {
                if (has(input$pair)) {
                  id <- as.character(d[[input$pair]])
                  a <- data.frame(id = id[g == sel[1]], v = y[g == sel[1]]); b <- data.frame(id = id[g == sel[2]], v = y[g == sel[2]])
                  if (any(duplicated(a$id)) || any(duplicated(b$id))) err <- c(err, sprintf("Each %s should appear once per group.", input$pair))
                  m <- merge(a, b, by = "id", sort = FALSE)
                  if (nrow(m) < max(nrow(a), nrow(b))) warn <- c(warn, sprintf("%d values have no partner and were left out.", max(nrow(a), nrow(b)) - nrow(m)))
                  out <- list(data = list(m$v.x, m$v.y), pair_ids = m$id)
                } else {
                  a <- y[g == sel[1] & !is.na(g)]; b <- y[g == sel[2] & !is.na(g)]
                  if (length(a) != length(b)) err <- c(err, sprintf("The two groups have %d and %d values, so they cannot be paired by order. Choose a Pair ID column.", length(a), length(b)))
                  else warn <- c(warn, "Pairs are matched by their order in the data. Choose a Pair ID column to match them explicitly.")
                  out <- list(data = list(a, b))
                }
              } else out <- list(data = list(y[g %in% sel[1]], y[g %in% sel[2]]))
              out$names <- sel; out$resp <- input$resp; out$grp <- input$grp
              if (length(lv) > 2) warn <- c(warn, sprintf("%s has %d groups; only %s and %s are compared. Use k samples to compare all of them.", input$grp, length(lv), sel[1], sel[2]))
            }
          } else {
            if (length(lv) < 2) err <- c(err, sprintf("%s has only one group.", input$grp))
            out <- list(y = y, g = factor(g, levels = lv), names = lv, resp = input$resp, grp = input$grp)
            if (length(lv) == 2) warn <- c(warn, "There are only two groups. The one-way ANOVA F test then equals the pooled t-test squared.")
          }
        }
      } else {
        if (kind == "two") {
          if (!has(input$c1) || !has(input$c2)) err <- c(err, "Choose a column for each sample.")
          else if (input$c1 == input$c2) err <- c(err, "Choose two different columns.")
          else {
            out <- list(data = list(numv(input$c1), numv(input$c2)), names = c(input$c1, input$c2), resp = "")
            if (paired) {
              n_inc <- sum(xor(is.finite(out$data[[1]]), is.finite(out$data[[2]])))
              if (n_inc) warn <- c(warn, sprintf("%d row%s with only one value were left out.", n_inc, if (n_inc > 1) "s" else ""))
            }
          }
        } else {
          cs <- intersect(input$cols %||% character(0), names(d))
          if (length(cs) < 2) err <- c(err, "Choose at least two columns.")
          else {
            y <- unlist(lapply(cs, numv)); g <- factor(rep(cs, each = nrow(d)), levels = cs)
            out <- list(y = y, g = g, names = cs, resp = "", grp = "Group")
          }
        }
      }
      if (!length(err)) {
        vecs <- if (kind == "k") split(out$y, out$g) else out$data
        n <- vapply(vecs, function(v) sum(is.finite(v)), 0)
        if (any(n < 2)) err <- c(err, "Every sample needs at least 2 numeric values.")
        else if (any(n < 5)) warn <- c(warn, sprintf("Small samples (smallest n = %d): tests have little power and normality checks are weak.", min(n)))
        out$n <- n
      }
      out$errors <- err; out$warnings <- warn; out$paired <- paired; out$layout <- lay
      out
    })

    output$check <- renderUI({
      sm <- samples(); ok <- !length(sm$errors)
      st <- if (ok && !length(sm$warnings)) pill("Ready", "pass", "check") else if (ok) pill("Ready, with notes", "warn", "alert") else pill("Needs attention", "fail", "x")
      vecs <- if (ok) { if (kind == "k") split(sm$y, sm$g) else setNames(sm$data, sm$names[seq_along(sm$data)]) } else list()
      tagList(
        kcard(div(class = "hrow", h2(class = "ct", "Data check"), div(class = "ml", st)),
              if (ok) div(style = "margin-top:8px",
                          div(class = "kv-row", span("Test"), span(switch(kind, one = "One sample", two = if (sm$paired) "Two paired samples" else "Two independent samples", k = sprintf("%d independent samples", length(vecs))))),
                          if (kind == "one") div(class = "kv-row", span("Sample"), span(sm$names))),
              if (length(sm$errors)) div(class = "col", style = "gap:8px;margin-top:12px", lapply(sm$errors, function(e) note(kind = "e", ic = "x", e))),
              if (length(sm$warnings)) div(class = "col", style = "gap:8px;margin-top:12px", lapply(sm$warnings, function(e) note(kind = "w", ic = "alert", e)))),
        if (ok) kcard(h2(class = "ct", "Samples"),
                      div(class = "tscroll", style = "margin-top:10px",
                          tags$table(class = "kt", tags$thead(tags$tr(tags$th("Sample"), tags$th(class = "n", "n"), tags$th(class = "n", "Mean"), tags$th(class = "n", "SD"), tags$th(class = "n", "Min"), tags$th(class = "n", "Max"))),
                                     tags$tbody(lapply(names(vecs), function(nm) { v <- vecs[[nm]]; v <- v[is.finite(v)]
                                       tags$tr(tags$td(tags$b(nm)), tags$td(class = "n", length(v)), tags$td(class = "n", fmt_g(mean(v))), tags$td(class = "n", fmt_g(stats::sd(v))),
                                               tags$td(class = "n", fmt_g(min(v))), tags$td(class = "n", fmt_g(max(v)))) }))))))
    })
    observeEvent(input$back1, go(1))
    observeEvent(input$to3, { if (length(samples()$errors)) { showNotification("Fix the items in the data check first.", type = "warning"); return() }; go(3) })

    # ---------- Step 3 ----------
    autofill <- function(id, value) {
      cur <- input[[id]] %||% ""
      if (!nzchar(cur) || identical(cur, auto[[id]])) { updateTextInput(session, id, value = value); auto[[id]] <- value }
    }
    observe({
      sm <- samples(); req(!length(sm$errors))
      rn <- if (nzchar(sm$resp %||% "")) sm$resp else "Value"
      autofill("resp_name", rn)
      if (kind == "two") { autofill("name1", sm$names[1]); autofill("name2", sm$names[2]) }
      autofill("title", switch(kind, one = sprintf("Is mean %s different from the target?", rn),
                               two = sprintf("%s vs %s", sm$names[1], sm$names[2]),
                               k = sprintf("%s across %s", rn, if (nzchar(sm$grp %||% "") && sm$grp != "Group") sm$grp else "groups")))
    })
    output$var_ui <- renderUI({
      if (kind == "two" && identical(input$paired, "paired")) return(NULL)
      radioButtons(ns("var_mode"), "Equal variances?",
                   choiceNames = list(tagList(tags$b("Decide from the data (recommended)"), div(class = "small", if (kind == "two") "F test and Levene" else "Levene test")),
                                      "Assume equal variances", "Do not assume equal variances"),
                   choiceValues = list("auto", "equal", "unequal"), selected = isolate(input$var_mode) %||% "auto")
    })
    alpha <- reactive(if (identical(input$alpha, "custom")) input$alpha_custom %||% 0.05 else as.numeric(input$alpha %||% "0.05"))
    settings <- reactive({
      sm <- samples()
      nm <- if (kind == "two") c(input$name1 %||% sm$names[1], input$name2 %||% sm$names[2]) else if (kind == "one") sm$names else sm$names
      nm[!nzchar(nm)] <- sm$names[!nzchar(nm)]
      list(kind = kind, title = input$title, response_name = input$resp_name, unit = input$unit, names = nm,
           group_name = sm$grp, alpha = alpha(), conf = as.numeric(input$conf %||% "0.95"),
           alt = input$alt %||% "two.sided", mu = if (is.null(input$mu) || is.na(input$mu)) 0 else input$mu,
           sigma = if (isTRUE(input$known_sigma)) input$sigma else NULL,
           sigma2 = if (isTRUE(input$known_sigma) && kind == "two") input$sigma2 else NULL,
           var_mode = input$var_mode %||% "auto", method = input$method %||% "auto", paired = isTRUE(sm$paired))
    })
    output$hyp <- renderUI({
      s <- settings(); rn <- s$response_name %||% "the response"
      h <- switch(kind,
        one = c(sprintf("The mean %s equals %s (μ = %s).", rn, format(s$mu), format(s$mu)),
                sprintf("The mean %s is %s %s (μ %s %s).", rn, c(two.sided = "different from", less = "less than", greater = "greater than")[[s$alt]], format(s$mu), alt_sym(s$alt), format(s$mu))),
        two = {
          w <- if (s$paired) "mean difference" else "difference in means"
          c(sprintf("The %s (%s − %s) is %s.", w, s$names[1], s$names[2], format(s$mu)),
            sprintf("The %s (%s − %s) is %s %s (μ1 − μ2 %s %s).", w, s$names[1], s$names[2],
                    c(two.sided = "not", less = "less than", greater = "greater than")[[s$alt]], format(s$mu), alt_sym(s$alt), format(s$mu)))
        },
        k = c(sprintf("All %d groups have the same mean %s.", length(s$names), rn), sprintf("At least one group mean %s is different.", rn)))
      div(class = "hyp", tags$b("H0: "), h[1], br(), tags$b("H1: "), h[2])
    })

    observeEvent(input$run, {
      sm <- samples()
      if (length(sm$errors)) { showNotification("Fix the items in the data check first.", type = "error"); return() }
      a <- alpha(); if (is.na(a) || a <= 0 || a >= 1) { showNotification("Choose a significance level between 0 and 1.", type = "warning"); return() }
      s <- settings()
      if (!is.null(s$sigma) && (is.na(s$sigma) || s$sigma <= 0)) { showNotification("Enter a positive σ, or untick the known σ box.", type = "warning"); return() }
      r <- tryCatch(switch(kind,
        one = run_one(sm$data[[1]], s),
        two = if (s$paired) run_paired(sm$data[[1]], sm$data[[2]], s) else run_two(sm$data[[1]], sm$data[[2]], s),
        k = { g <- sm$g; levels(g) <- s$names; run_k(sm$y, g, s) }), error = function(e) e)
      if (inherits(r, "error")) { showNotification(paste("The analysis could not run:", conditionMessage(r)), type = "error", duration = NULL); return() }
      r$ds_name <- ds()$name
      res(r); go(4)
    })
    observeEvent(input$edit_settings, go(3))

    # ---------- Results ----------
    test_sel <- reactive({ r <- res(); req(r); t <- input$test; if (is.null(t) || !(t %in% names(r$tests))) r$recommend else t })
    est_sub <- function(r, t) {
      u <- r$settings$unit %||% ""; uu <- if (nzchar(u)) paste0(" ", u) else ""
      if (r$kind == "two" && t$family == "param") sprintf("%s − %s%s", fmt_g(mean(r$data[[1]])), fmt_g(mean(r$data[[2]])), uu)
      else if (r$kind == "paired" && t$family == "param") sprintf("mean of %s − %s", r$settings$names[1], r$settings$names[2])
      else if (r$kind == "one") sprintf("target %s%s", format(r$settings$mu), uu) else ""
    }
    headline_s <- function(r) {
      t <- r$tests[[r$recommend]]; s <- r$settings; a <- s$alpha; sig <- !is.na(t$p) && t$p < a
      rn <- s$response_name %||% "the response"; loc <- if (t$family == "param") "mean" else "median"
      altw <- c(two.sided = "differs from", less = "is less than", greater = "is greater than")[[s$alt %||% "two.sided"]]
      title <- switch(r$kind,
        one = if (sig) sprintf("The %s %s %s %s", loc, rn, altw, format(s$mu)) else sprintf("No evidence that the %s %s %s %s", loc, rn, altw, format(s$mu)),
        two = , paired = {
          cmpw <- c(two.sided = "differ in", less = "has lower", greater = "has higher")[[s$alt]]
          if (s$alt == "two.sided") { if (sig) sprintf("%s and %s differ in %s %s", s$names[1], s$names[2], loc, rn) else sprintf("No significant difference in %s between %s and %s", rn, s$names[1], s$names[2]) }
          else if (sig) sprintf("%s %s %s %s than %s", s$names[1], cmpw, loc, rn, s$names[2]) else sprintf("No evidence that %s %s %s %s than %s", s$names[1], cmpw, loc, rn, s$names[2])
        },
        k = if (sig) sprintf("%s differs between the %d groups", rn, length(s$names)) else sprintf("No significant difference in %s between the %d groups", rn, length(s$names)))
      dfx <- if (!is.na(t$df %||% NA) && !is.character(t$df)) sprintf("(%s)", format(round(t$df, 2))) else if (is.character(t$df)) sprintf("(%s)", t$df) else ""
      why_rec <- if (s$method == "param") "You chose to report the parametric test."
        else if (s$method == "nonpar") "You chose to report the rank-based test."
        else if (!r$normal) sprintf("Normality fails (Shapiro-Wilk %s), so the rank-based test is the one to report.", p_txt(min(r$normality$p, na.rm = TRUE)))
        else switch(r$recommend,
          z = "σ is known and the data look normal, so the z-test is the one to report.",
          pooled = "Both samples look normal and the variances look equal, so the pooled t-test is the one to report.",
          welch = "Both samples look normal but the variances differ, so the Welch test is the one to report.",
          anova = "The residuals look normal and the variances look equal, so the one-way ANOVA is the one to report.",
          paired = "The differences look normal, so the paired t-test is the one to report.",
          "The data look normal, so the t-test is the one to report.")
      list(sig = sig, title = title,
           why = sprintf("%s: %s%s = %s, %s, which is %s α = %s, so we %s H0. %s", t$name, t$stat_name, dfx, fmt_g(t$stat), p_txt(t$p),
                         if (sig) "below" else "above", format(a), if (sig) "reject" else "do not reject", why_rec))
    }
    interp_s <- function(r) {
      s <- r$settings; t <- r$tests[[r$recommend]]; a <- s$alpha; sig <- t$p < a; rn <- s$response_name %||% "the response"
      u <- if (nzchar(s$unit %||% "")) paste0(" ", s$unit) else ""
      desc <- r$desc
      d_txt <- paste(sprintf("%s (n = %d, mean %s%s, SD %s)", desc$sample, desc$n, fmt_g(desc$mean), u, fmt_g(desc$sd)), collapse = "; ")
      nrm <- paste(sprintf("%s: Shapiro-Wilk %s", r$normality$sample, vapply(r$normality$p, p_txt, "")), collapse = "; ")
      ci <- if (is.finite(t$lwr %||% NA) || is.finite(t$upr %||% NA)) sprintf(" The %g%% confidence interval for the %s is [%s, %s].", 100 * s$conf, tolower(t$est_name %||% "estimate"),
                                                                          if (is.finite(t$lwr)) fmt_g(t$lwr) else "−∞", if (is.finite(t$upr)) fmt_g(t$upr) else "∞") else ""
      others <- setdiff(names(r$tests), r$recommend)
      agree <- vapply(others, function(k) (r$tests[[k]]$p < a) == sig, TRUE)
      oth <- if (length(others)) { if (all(agree)) " The other tests give the same conclusion." else sprintf(" %s give%s a different conclusion, so interpret with care.",
                                                                    paste(vapply(others[!agree], function(k) r$tests[[k]]$name, ""), collapse = " and "), if (sum(!agree) == 1) "s" else "") } else ""
      sprintf("Data: %s. Normality: %s. %s gives %s = %s, %s, so at α = %s we %s H0. Conclusion: %s.%s%s",
              d_txt, nrm, t$name, t$stat_name, fmt_g(t$stat), p_txt(t$p), format(a), if (sig) "reject" else "do not reject",
              headline_s(r)$title, ci, oth)
    }

    reg <- function(id, fun, h = 300) {
      output[[id]] <- renderPlot({ r <- res(); req(r); fun(r) }, res = 96, bg = "transparent")
      output[[paste0("dl_", id)]] <- downloadHandler(filename = function() paste0(kind, "_", id, ".png"),
        content = function(file) ggplot2::ggsave(file, fun(res()) + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA)),
                                                 width = 7, height = h / 96 * 1.1, dpi = 200, bg = "white"))
    }
    tools <- function(id) div(class = "ml", downloadLink(ns(paste0("dl_", id)), title = "Download PNG", class = "ib", style = "width:30px;height:30px", icon("download", 14)))
    reg("pl_box", plot_samples_box, 300); reg("pl_box2", plot_samples_box, 300)
    reg("pl_paired", plot_paired, 300)
    reg("pl_dist", function(r) { t <- r$tests[[test_sel()]]; req(t$dist != "none")
      if (t$dist %in% c("F", "chisq")) plot_dist(t$dist, t$stat, t$df1, t$df2, r$settings$alpha) else plot_dist_alt(t$dist, t$stat, t$df, NA, r$settings$alpha, r$settings$alt) }, 240)
    reg("pl_ci", function(r) { t <- r$tests[[test_sel()]]; req(is.finite(t$lwr) || is.finite(t$upr)); plot_ci_one(t, r$settings$mu %||% 0, r$settings$conf, r$settings$unit %||% "") }, 200)
    reg("pl_hist", function(r) if (r$kind == "two") plot_hist_groups(r$data, r$settings$names, ylab_s(r$settings))
                               else plot_hist(if (r$kind == "paired") r$diffs else r$data[[1]], if (r$kind == "paired") "Difference" else ylab_s(r$settings)), 260)
    reg("pl_qq", function(r) if (r$kind == "two") plot_qq_groups(r$data, r$settings$names)
                             else plot_qq_vec(if (r$kind == "paired") r$diffs else if (r$kind == "k") stats::residuals(r$fit) else r$data[[1]],
                                              if (r$kind == "k") "Residuals" else "Sample quantiles"), 260)
    reg("pl_means", function(r) {
      m <- K_POSTHOC[[if (test_sel() %in% names(K_POSTHOC)) test_sel() else "anova"]]
      fake <- list(posthoc = list(treatment = list(means = r$means, letters = list(x = r$letters[[m]]))),
                   data = data.frame(y = r$data$y, treatment = r$data$g), settings = list(labels = NULL, response_name = r$settings$response_name, unit = r$settings$unit),
                   names = list(treatment = r$settings$group_name %||% "Group"), map = list(response = r$settings$response_name))
      plot_means(fake, "treatment", "x") }, 320)
    reg("pl_phci", function(r) { m <- K_POSTHOC[[test_sel()]]; tab <- r$posthoc[[m]]; req(!is.null(tab$lwr)); tab$signif <- tab$p < r$settings$alpha
      plot_ci(tab, r$settings$conf, r$settings$unit %||% "") }, 240)

    output$results <- renderUI({
      r <- res(); req(r); s <- r$settings; h <- headline_s(r)
      rec <- r$tests[[r$recommend]]
      sub <- sprintf("%s · %s%s · %s", switch(r$kind, one = "One sample", two = "Two independent samples", paired = "Two paired samples", k = sprintf("%d independent samples", length(s$names))),
                     s$response_name, if (nzchar(s$unit %||% "")) paste0(" (", s$unit, ")") else "",
                     paste(sprintf("%s (n = %d)", r$desc$sample[seq_len(if (r$kind == "paired") 2 else nrow(r$desc))], r$desc$n[seq_len(if (r$kind == "paired") 2 else nrow(r$desc))]), collapse = ", "))
      tests <- r$tests
      ord <- c(r$recommend, setdiff(names(tests), r$recommend))
      seg_names <- vapply(names(tests), function(k) sub(" \\(.*\\)$", "", tests[[k]]$name), "")
      a <- s$alpha
      tagList(
        div(class = "page-head",
            div(class = "grow", h2(class = "ct", style = "font-size:20px", s$title), p(class = "small", style = "margin:2px 0 0", sub)),
            div(class = "hrow", btn(ns("edit_settings"), "Edit settings", "edit", size = "bsm"),
                pin_button(ns),
                downloadButton(ns("dl_tables"), label = tagList(icon("download", 14), "Download tables"), icon = NULL, class = "btn-k bp bsm"))),
        kcard(div(class = "hrow", style = "align-items:flex-start;flex-wrap:nowrap",
                  div(class = paste("ico", if (h$sig) "ig" else "ip"), style = "width:44px;height:44px;border-radius:22px", icon(if (h$sig) "check" else "info", 20, 2.5)),
                  div(style = "flex:1;min-width:0", h2(class = "ct", style = "font-size:19px", h$title), p(class = "sub", style = "margin-top:4px", h$why)),
                  div(class = "ml", pill(sprintf("Recommended: %s", rec$name), "gold")))),
        div(class = "split wide",
            div(class = "col",
                if (!is.null(r$var_tests)) kcard(
                  div(class = "hrow", span(class = "pill neu", "1"), h2(class = "ct", "Are the variances equal?"),
                      div(class = "ml", if (r$equal_var) pill(if (s$var_mode == "equal") "Assumed equal" else "Yes, treat as equal", "pass") else pill(if (s$var_mode == "unequal") "Not assumed equal" else "No, treat as unequal", "warn"))),
                  div(class = "grid g2", style = "margin-top:10px;align-items:start",
                      div(test_tbl_ui(r$var_tests),
                          p(class = "small", style = "margin:10px 0 0",
                            if (s$var_mode != "auto") "You fixed this in the settings."
                            else if (r$equal_var) sprintf("No evidence of unequal variances, so the %s is used for the parametric result.", if (r$kind == "k") "one-way ANOVA" else "pooled t-test")
                            else sprintf("The variances differ, so the %s is used for the parametric result.", if (r$kind == "k") "Welch ANOVA" else "Welch t-test")),
                          if (!is.null(r$resid_var)) div(style = "margin-top:12px", div(class = "klab", "Constant variance of the ANOVA residuals"), test_tbl_ui(r$resid_var),
                                                         p(class = "small", style = "margin:6px 0 0", "Breusch-Pagan checks the spread of the residuals of the one-way model; Levene and Bartlett above compare the spread of the groups."))),
                      plotOutput(ns("pl_box"), height = "240px"))),
                if (is.null(r$var_tests)) kcard(div(class = "hrow", h2(class = "ct", if (r$kind == "paired") "The pairs" else "The sample"), tools(if (r$kind == "paired") "pl_paired" else "pl_box")),
                                                plotOutput(ns(if (r$kind == "paired") "pl_paired" else "pl_box"), height = "280px")),
                div(class = "hrow", seg_input(ns("test"), setNames(ord, ifelse(ord == r$recommend, paste(seg_names[ord], "★"), seg_names[ord])), isolate(if (!is.null(input$test) && input$test %in% ord) input$test else r$recommend)),
                    span(class = "small ml", "★ recommended")),
                uiOutput(ns("test_body"), class = "rcol")),
            div(class = "col",
                kcard(h2(class = "ct", sprintf("All %d tests", length(tests))),
                      tags$table(class = "kt", style = "margin-top:10px",
                                 tags$thead(tags$tr(tags$th("Test"), tags$th(class = "n", "p"), tags$th("Decision"))),
                                 tags$tbody(lapply(ord, function(k) { t <- tests[[k]]
                                   tags$tr(tags$td(t$name, if (k == r$recommend) span(class = "pill gold", style = "margin-left:6px;padding:1px 7px", "★")),
                                           tags$td(class = "n", fmt_p(t$p)), tags$td(if (!is.na(t$p) && t$p < a) pill("Reject H0", "pass") else pill("Do not reject", "neu"))) }))),
                      if (length(unique(vapply(tests, function(t) !is.na(t$p) && t$p < a, TRUE))) > 1)
                        div(style = "margin-top:12px", note(kind = "w", ic = "alert", sprintf("The tests disagree. %s Report the recommended test and mention the others.",
                                                                                             if (min(r$normality$n) < 10) "With small samples, borderline results are common." else "The result is near the boundary.")))),
                kcard(h2(class = "ct", if (r$kind == "k") "Normality" else if (r$kind == "paired") "Normality of the differences" else if (r$kind == "one") "Normality" else "Normality of each sample"),
                      lapply(seq_len(nrow(r$normality)), function(i) { z <- r$normality[i, ]
                        div(class = "kv-row", style = "align-items:center", span(z$sample, br(), span(class = "small", sprintf("Shapiro-Wilk W = %s, %s", fmt_g(z$W, 3), p_txt(z$p)))), status_pill(z$status)) }),
                      p(class = "small", style = "margin:10px 0 0", if (r$normal) "Normal enough for the parametric test." else "Not normal, and the samples are small (n < 30), so rank-based tests are safer.")),
                kcard(h2(class = "ct", "Summary statistics"),
                      div(class = "tscroll", style = "margin-top:10px",
                          tags$table(class = "kt", tags$thead(tags$tr(tags$th(""), tags$th(class = "n", "n"), tags$th(class = "n", "Mean"), tags$th(class = "n", "SD"), tags$th(class = "n", "Median"))),
                                     tags$tbody(lapply(seq_len(nrow(r$desc)), function(i) { z <- r$desc[i, ]
                                       tags$tr(tags$td(tags$b(z$sample)), tags$td(class = "n", z$n), tags$td(class = "n", fmt_g(z$mean)), tags$td(class = "n", fmt_g(z$sd)), tags$td(class = "n", fmt_g(z$median))) }))))),
                kcard(h2(class = "ct", "Settings"),
                      div(class = "kv-row", span("Samples"), span(switch(r$kind, one = "One", two = "Independent", paired = "Paired", k = "Independent"))),
                      if (r$kind != "k") div(class = "kv-row", span("H1"), span(switch(r$kind, one = sprintf("μ %s %s", alt_sym(s$alt), format(s$mu)),
                                                                                       sprintf("μ1 − μ2 %s %s", alt_sym(s$alt), format(s$mu))))),
                      if (!is.null(s$sigma)) div(class = "kv-row", span("Known σ"), span(paste(c(s$sigma, s$sigma2), collapse = ", "))),
                      div(class = "kv-row", span("α / confidence"), span(sprintf("%s / %g%%", format(a), 100 * s$conf))),
                      div(class = "kv-row", span("Test choice"), span(switch(s$method, auto = "Automatic", param = "Parametric", nonpar = "Rank-based")))))),
        div(class = "grid g2",
            kcard(div(class = "hrow", h2(class = "ct", if (r$kind == "k") "Residuals, QQ plot" else if (r$kind == "paired") "Differences, QQ plot" else if (r$kind == "two") "QQ plot of each sample" else "QQ plot"), tools("pl_qq")),
                  plotOutput(ns("pl_qq"), height = "260px")),
            if (r$kind == "k") kcard(div(class = "hrow", h2(class = "ct", "Distribution by group"), tools("pl_box2")), plotOutput(ns("pl_box2"), height = "260px"))
            else kcard(div(class = "hrow", h2(class = "ct", if (r$kind == "paired") "Histogram of the differences" else if (r$kind == "two") "Histogram of each sample" else "Histogram"), tools("pl_hist")),
                       plotOutput(ns("pl_hist"), height = "260px"))),
        kcard(div(class = "hrow", h2(class = "ct", "Interpretation"),
                  tags$button(class = "btn-k bs bsm ml", onclick = sprintf("navigator.clipboard.writeText(document.getElementById('%s').innerText); this.lastChild.textContent=' Copied'", ns("interp")),
                              icon("copy", 14), span(" Copy"))),
              p(id = ns("interp"), style = "margin:10px 0 0;font-size:14px;line-height:23px;color:var(--text)", interp_s(r))),
        kcard(tags$details(
          tags$summary(style = "cursor:pointer", span(class = "hrow", style = "display:inline-flex", icon("code", 16), tags$b(style = "color:var(--ink)", "R code that reproduces these results"))),
          div(class = "hrow", style = "margin-top:12px",
              tags$button(class = "btn-k bs bsm", onclick = sprintf("navigator.clipboard.writeText(document.getElementById('%s').innerText); this.lastChild.textContent=' Copied'", ns("code_pre")), icon("copy", 14), span(" Copy")),
              downloadButton(ns("dl_code"), label = tagList(icon("download", 14), "Download .R"), icon = NULL, class = "btn-k bp bsm")),
          tags$pre(id = ns("code_pre"), class = "mono", style = "margin-top:12px;background:var(--surface2);border:1px solid var(--line);border-radius:10px;padding:16px;max-height:520px;overflow:auto;white-space:pre;color:var(--text)",
                   make_samples_code(r, r$ds_name)),
          if (r$kind %in% c("one", "two", "paired")) p(class = "small", style = "margin:10px 0 0",
            "Note: the class script writes wilcox.test(..., m = 25). R ignores m, so that line tests against 0. Use mu = 25, as in the script above.")))
      )
    })

    output$test_body <- renderUI({
      r <- res(); req(r); k <- test_sel(); t <- r$tests[[k]]; s <- r$settings; a <- s$alpha
      tile <- function(label, value, sub) div(class = "kcard soft", style = "padding:14px 16px", div(class = "small", style = "font-weight:500", label),
                                              div(style = "font-size:22px;font-weight:600;color:var(--ink);line-height:30px", value), div(class = "small", sub))
      has_ci <- is.finite(t$lwr %||% NA) || is.finite(t$upr %||% NA)
      ref <- s$mu %||% 0
      ci_txt <- if (has_ci) sprintf("[%s, %s]", if (is.finite(t$lwr)) fmt_g(t$lwr) else "−∞", if (is.finite(t$upr)) fmt_g(t$upr) else "∞") else "—"
      excl <- has_ci && ((is.finite(t$lwr) && t$lwr > ref) || (is.finite(t$upr) && t$upr < ref))
      dfs <- if (is.character(t$df)) paste("df =", t$df) else if (!is.na(t$df %||% NA)) sprintf("df = %s", format(round(t$df, 2))) else ""
      ph <- if (r$kind == "k") K_POSTHOC[[k]] else NULL
      tagList(
        kcard(div(class = "col", style = "gap:14px",
                  div(class = "hrow", h2(class = "ct", t$name), if (!is.na(t$p) && t$p < a) pill(sprintf("Reject H0 at α = %s", format(a)), "pass") else pill(sprintf("Do not reject H0 at α = %s", format(a)), "neu"),
                      if (k == r$recommend) div(class = "ml", pill("Recommended", "gold"))),
                  div(class = "grid g4",
                      if (!is.null(t$est_name)) tile(t$est_name, fmt_g(t$est), est_sub(r, t)) else tile("Groups", length(s$names), "compared"),
                      tile(sprintf("%s statistic", t$stat_name), fmt_g(t$stat), dfs),
                      tile("p-value", fmt_p(t$p), if (r$kind == "k") "right tail" else ALT_NAMES[[s$alt]]),
                      if (has_ci) tile(sprintf("%g%% CI", 100 * s$conf), ci_txt, if (excl) sprintf("excludes %s", format(ref)) else sprintf("includes %s", format(ref)))
                      else if (!is.null(t$effect)) tile(t$effect$name, fmt_g(t$effect$value), "effect size")
                      else tile("Decision", if (t$p < a) "Reject" else "Keep H0", sprintf("α = %s", format(a)))),
                  p(class = "small", style = "margin:0", t$about, if (!is.null(t$effect) && has_ci) sprintf(" %s = %s.", t$effect$name, fmt_g(t$effect$value)),
                    if (!is.null(t$note)) paste0(" ", t$note)),
                  div(class = "grid g2",
                      if (t$dist != "none") div(div(class = "klab", "Rejection region"), plotOutput(ns("pl_dist"), height = "220px")),
                      if (has_ci) div(div(class = "klab", "Confidence interval"), plotOutput(ns("pl_ci"), height = "180px"),
                                      p(class = "small", style = "margin:4px 0 0", sprintf("The %g%% interval %s %s %s.", 100 * s$conf, ci_txt, if (excl) "excludes" else "includes", format(ref))))))),
        if (!is.null(ph)) {
          tab <- r$posthoc[[ph]]; lt <- r$letters[[ph]]
          kcard(div(class = "col", style = "gap:12px",
                    div(class = "hrow", h2(class = "ct", sprintf("Post-hoc: %s", K_POSTHOC_NAMES[[ph]])),
                        span(class = "small ml", switch(ph, tukey = "Goes with the one-way ANOVA", gh = "Goes with the Welch ANOVA; no equal-variance assumption", dunn = "Goes with Kruskal-Wallis; compares mean ranks"))),
                    div(class = "tscroll", if (ph == "dunn") np_ph_table_ui(tab, a, "Mean rank difference") else ph_table_ui(tab, s$conf, a)),
                    div(class = "grid g2", style = "align-items:start",
                        div(div(class = "hrow", div(class = "klab", "Means and grouping letters"), tools("pl_means")), plotOutput(ns("pl_means"), height = "280px")),
                        if (ph != "dunn") div(div(class = "hrow", div(class = "klab", "Intervals for each difference"), tools("pl_phci")), plotOutput(ns("pl_phci"), height = "280px"))),
                    p(class = "small", style = "margin:0", "Groups that share a letter are not significantly different: ",
                      paste(sprintf("%s %s", names(lt), lt), collapse = ", "), ".")))
        }
      )
    })

    output$dl_code <- downloadHandler(filename = function() sprintf("analysis_%s.R", kind), content = function(file) writeLines(make_samples_code(res(), res()$ds_name), file, useBytes = TRUE))
    output$dl_tables <- downloadHandler(filename = function() sprintf("results_%s_%s.xlsx", kind, format(Sys.Date())),
      content = function(file) writexl::write_xlsx(samples_tables(res()), file))

    pin_observers(input, session, store, items = reactive({
      r <- res(); req(r); s <- r$settings
      tt <- function() do.call(rbind, lapply(r$tests, function(t) data.frame(
        Test = paste0(t$name, if (t$key == r$recommend) " (recommended)" else ""), Statistic = sprintf("%s = %s", t$stat_name, fmt_g(t$stat, 3)),
        df = if (is.null(t$df) || all(is.na(t$df))) "" else if (is.character(t$df)) t$df else format(round(t$df, 2)),
        `p-value` = fmt_p(t$p), Estimate = if (is.finite(t$est %||% NA)) fmt_g(t$est, 3) else "",
        CI = if (is.finite(t$lwr %||% NA) || is.finite(t$upr %||% NA)) sprintf("[%s, %s]", if (is.finite(t$lwr)) fmt_g(t$lwr) else "\u2212\u221e", if (is.finite(t$upr)) fmt_g(t$upr) else "\u221e") else "",
        check.names = FALSE)))
      it <- list(
        tests = list(label = "Test results", default = TRUE, build = function() mk_pin("table", "Test results", "table", df = tt())),
        summary = list(label = "Summary statistics", default = TRUE, build = function() mk_pin("table", "Summary statistics", "sigma", df = fmt_df(r$desc))),
        normality = list(label = "Normality checks", default = FALSE, build = function() mk_pin("table", "Normality checks", "scale", df = fmt_df(r$normality))),
        plot = list(label = if (r$kind == "paired") "Paired plot" else "Box plot", default = TRUE,
                    build = function() mk_pin("plot", if (r$kind == "paired") "Paired plot" else "Box plot", "bar", plot = if (r$kind == "paired") plot_paired(r) else plot_samples_box(r), h = 3.2)))
      if (!is.null(r$var_tests)) it$var <- list(label = "Equal variance tests", default = FALSE, build = function() mk_pin("table", "Equal variance tests", "scale", df = fmt_df(r$var_tests)))
      if (r$kind == "k") {
        m <- K_POSTHOC[[if (r$recommend %in% names(K_POSTHOC)) r$recommend else "anova"]]
        it$posthoc <- list(label = sprintf("%s table", K_POSTHOC_NAMES[[m]]), default = TRUE,
                           build = function() mk_pin("table", sprintf("%s table", K_POSTHOC_NAMES[[m]]), "columns", df = fmt_df(r$posthoc[[m]])))
      }
      it$interp <- list(label = "Interpretation", default = TRUE, build = function() mk_pin("text", "Interpretation", "file", text = interp_s(r)))
      it
    }), src = reactive({
      r <- res(); s <- r$settings; d <- ds()
      kname <- switch(r$kind, one = "One sample", two = "Two samples", paired = "Paired samples", k = "k samples")
      list(key = paste0("cmp_", kind), label = sprintf("%s \u00b7 %s", kname, s$response_name %||% "Response"), title = s$title %||% kname,
           subtitle = sprintf("%s \u00b7 %s \u00b7 \u03b1 = %s", kname, paste(sprintf("%s (n = %d)", r$desc$sample, r$desc$n), collapse = ", "), format(s$alpha)),
           dataset = d$name %||% r$ds_name, notes = d$notes, code = tryCatch(make_samples_code(r, r$ds_name), error = function(e) ""))
    }))

    list(ai = function(full) {
           d <- ds(); if (is.null(d)) return(NULL)
           r <- res()
           kname <- switch(kind, one = "one sample", two = "two sample", k = "k sample")
           if (!is.null(r) && step() == 4)
             return(list(label = sprintf("your %s test results", kname), kind = "samples",
                         text = if (full) paste0(ctx_samples(r, interp_s(r), tryCatch(make_samples_code(r, r$ds_name), error = function(e) "")), "\n\n", ctx_dataset(d))))
           list(label = sprintf("dataset %s and your %s set-up", d$name, kname), kind = "setup",
                text = if (full) paste0(ctx_setup(paste(kname, "test"), step(), list(), NULL), "\n", ctx_dataset(d)))
         },
         use_dataset = function(id) { ds_id(id); go(2) },
         set_paired = function(p) updateRadioButtons(session, "paired", selected = if (isTRUE(p)) "paired" else "ind"),
         set_method = function(m) updateRadioButtons(session, "method", selected = m))
  })
}

samples_tables <- function(r) {
  tt <- do.call(rbind, lapply(r$tests, function(t) data.frame(test = t$name, statistic = t$stat_name, value = t$stat, df = as.character(t$df %||% NA), p = t$p,
                                                              estimate = t$est_name %||% NA, est = t$est, lwr = t$lwr, upr = t$upr, recommended = t$key == r$recommend)))
  out <- list(Tests = tt, Summary = r$desc, Normality = r$normality)
  if (!is.null(r$var_tests)) out$Variances <- r$var_tests
  if (r$kind == "k") { out$Tukey <- r$posthoc$tukey; out$GamesHowell <- r$posthoc$gh; out$Dunn <- r$posthoc$dunn }
  out
}
