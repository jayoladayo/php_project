# Problem Sets: the course exercises grouped by chapter and test type, a custom problem
# editor, and the 7-step hypothesis-testing process taught in the course:
#   1 Problem & data  2 Hypotheses  3 Significance level  4 Information available
#   5 Test  6 Decision  7 Post-hoc   (+ Assumption checks)

PS_STEPS <- c("Problem & data", "Hypotheses", "Significance level", "Information available", "Test", "Decision", "Post-hoc")
PS_STEPS_SHORT <- c("Problem", "Hypotheses", "α level", "Information", "Test", "Decision", "Post-hoc")

PS_GROUP_DESC <- c(
  "One sample" = "Compare one mean, median or variance with a target value.",
  "Two sample" = "Compare two groups: independent samples, or paired (dependent) samples.",
  "K sample" = "Compare three or more independent groups.",
  "Parametric" = "Normal populations: z, t, χ² and F tests (one-way ANOVA).",
  "Non-parametric" = "Normality fails: rank tests (Wilcoxon, Mann-Whitney, Kruskal-Wallis).")

ps_groups <- function(ch) {
  fam <- list(list(t = "Parametric", fam = "param"), list(t = "Non-parametric", fam = "nonpar"))
  if (ch == "ch3") return(lapply(fam, function(f) list(title = f$t, fam = f$fam)))
  list(list(title = "One sample", kinds = c("one", "variance"), subs = fam),
       list(title = "Two sample", kinds = c("two", "paired"),
            subs = list(list(t = "Parametric · independent samples", fam = "param", kinds = "two"), list(t = "Parametric · paired samples", fam = "param", kinds = "paired"),
                        list(t = "Non-parametric · independent samples", fam = "nonpar", kinds = "two"), list(t = "Non-parametric · paired samples", fam = "nonpar", kinds = "paired"))),
       list(title = "K sample", kinds = "k", subs = fam))
}

ps_alt_choices <- function(kind) {
  sym <- switch(kind, one = "μ", variance = "σ²", two = "μ1 − μ2", paired = "μd", "μ")
  v <- if (kind == "variance") "σ0²" else if (kind == "one") "μ0" else "Δ0"
  stats::setNames(c("two.sided", "greater", "less"), sprintf(c("Two-sided: %s ≠ %s", "Right-tailed: %s > %s", "Left-tailed: %s < %s"), sym, v))
}
ps_value_label <- function(kind) switch(kind, one = "Hypothesised mean μ0", variance = "Hypothesised variance σ0²",
                                        two = "Hypothesised difference Δ0 = μ1 − μ2", paired = "Hypothesised mean difference Δ0", "Value")

problems_ui <- function(id) {
  ns <- NS(id)
  tabsetPanel(id = ns("views"), type = "hidden",
    # ---------- Problem list ----------
    tabPanelBody("list",
      page_head("Problem Sets", "Exercises from the course problem sheets with their complete data, solved with the 7-step hypothesis-testing process.",
                btn(ns("new_custom"), "Create custom problem", "plus", "bp", "bsm")),
      uiOutput(ns("resume")),
      kcard(div(class = "hrow", style = "align-items:flex-end",
                div(seg_input(ns("cat"), c("Chapter 2" = "ch2", "Chapter 3" = "ch3", "Custom Problem" = "custom"), "ch2")),
                div(class = "ml ps-search", textInput(ns("q"), NULL, placeholder = "Search by number, topic or test"))),
            uiOutput(ns("cat_blurb"))),
      uiOutput(ns("cards"))),
    # ---------- One problem, step by step ----------
    tabPanelBody("detail",
      uiOutput(ns("detail_head")),
      uiOutput(ns("stepper")),
      tabsetPanel(id = ns("steps"), type = "hidden",
                  tabPanelBody("1", uiOutput(ns("s1"))), tabPanelBody("2", uiOutput(ns("s2"))), tabPanelBody("3", uiOutput(ns("s3"))),
                  tabPanelBody("4", uiOutput(ns("s4"))), tabPanelBody("5", uiOutput(ns("s5"))), tabPanelBody("6", uiOutput(ns("s6"))),
                  tabPanelBody("7", uiOutput(ns("s7"))), tabPanelBody("8", uiOutput(ns("s8"))))),
    # ---------- Custom problem editor ----------
    tabPanelBody("edit",
      page_head("Custom problem", "Write or paste a problem, bring in its data and say what each column is. The 7 steps then work as for the course problems.",
                btn(ns("edit_back"), "Back to problems", "arrowl", "bs", "bsm")),
      div(class = "split wide",
          div(class = "col",
              kcard(div(class = "hrow", div(class = "ps-stepn", "1"), h2(class = "ct", "Problem statement")),
                    div(style = "margin-top:12px", textInput(ns("c_title"), "Title", placeholder = "For example: Tensile strength of four mixing techniques")),
                    div(style = "margin-top:12px", textAreaInput(ns("c_statement"), "Statement and questions", rows = 7, width = "100%",
                                                                 placeholder = "Paste the full problem. Writing α (for example α = 0.01), μ0 = … or σ = … lets the steps fill them in."))),
              kcard(div(class = "hrow", div(class = "ps-stepn", "2"), h2(class = "ct", "Dataset")),
                    div(style = "margin-top:12px", seg_input(ns("c_src"), c("Paste" = "paste", "Upload a file" = "upload", "Class example" = "example"), "paste")),
                    conditionalPanel("input.c_src == 'paste'", ns = ns, div(style = "margin-top:12px",
                      textAreaInput(ns("c_paste"), NULL, rows = 6, width = "100%", placeholder = "Paste from Excel or type values separated by tabs or commas.\nFirst row = column names, for example:\nTechnique\tStrength\n1\t3129\n1\t3000\n2\t3200"),
                      div(class = "hrow", style = "margin-top:8px", checkboxInput(ns("c_header"), "First row holds column names", TRUE), div(class = "ml"), btn(ns("c_use_paste"), "Use pasted data", "check", "bs", "bsm")))),
                    conditionalPanel("input.c_src == 'upload'", ns = ns, div(style = "margin-top:12px",
                      fileInput(ns("c_file"), NULL, accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls"), buttonLabel = "Choose CSV or Excel", placeholder = "First sheet is used"))),
                    conditionalPanel("input.c_src == 'example'", ns = ns, div(class = "hrow", style = "margin-top:12px;align-items:flex-end",
                      div(style = "flex:1;min-width:220px", selectInput(ns("c_ex"), NULL, setNames(names(CLASS_EXAMPLES), vapply(CLASS_EXAMPLES, `[[`, "", "title")), width = "100%")),
                      btn(ns("c_use_ex"), "Load", "check", "bs", "bsm"))),
                    uiOutput(ns("c_wide")),
                    div(style = "margin-top:12px", class = "gridbox", rhandsontable::rHandsontableOutput(ns("c_grid"))),
                    p(class = "small", style = "margin:8px 0 0", "Edit cells directly. Right-click the grid to add or remove rows."))),
          div(class = "col",
              kcard(div(class = "hrow", div(class = "ps-stepn", "3"), h2(class = "ct", "What each column is")),
                    p(class = "small", style = "margin:6px 0 12px", "One sample: a Response only. Two or more groups: a Response and a Group. Paired data: also the Pair ID that links the two measurements."),
                    div(class = "col", style = "gap:12px",
                        selectInput(ns("c_response"), "Response (numeric)", NULL), selectInput(ns("c_treatment"), "Group (sample) column", NULL),
                        selectInput(ns("c_block"), "Pair ID (paired samples only)", NULL),
                        checkboxInput(ns("c_variance"), "The question is about a variance, not a mean (one sample)", FALSE))),
              uiOutput(ns("c_check")),
              btn(ns("c_go"), "Continue to the 7 steps", "arrowr", "bp", "blg"))))
  )
}

problems_server <- function(id, store, show_crumb, open_design = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    sel <- reactiveVal(NULL)
    custom <- reactiveVal(NULL)
    memo <- reactiveValues()
    cdata <- reactiveVal(NULL)
    crev <- reactiveVal(0)
    keyn <- reactiveVal(0)
    # The problem being worked on. Inputs carry the problem key in their ids, so a value typed
    # for one problem can never be read for another while the browser catches up.
    cur <- reactiveValues(id = NULL, key = NULL, meta = NULL, st = NULL, kind = NULL, s = NULL, h0 = NULL, h1 = NULL, hs0 = NULL, hs1 = NULL,
                          test = NULL, test_user = FALSE, ph = NULL, res = NULL, step = 1, seen = 1)
    kid <- function(n) paste0(n, "_", isolate(cur$key))
    kin <- function(n) input[[paste0(n, "_", cur$key)]]

    view <- function(v, crumb = NULL) {
      updateTabsetPanel(session, "views", selected = v)
      show_crumb(crumb %||% c("Design of experiments", "Problem Sets"))
      session$sendCustomMessage("scroll_top", list())
    }
    prob_label <- function(m) if (identical(m$id, "custom")) (if (nzchar(m$title %||% "")) m$title else "Custom problem") else sprintf("Problem %s", m$id)
    meta_of <- function(i) if (identical(i, "custom")) custom() else PROBLEMS[[i]]

    # ---------- List ----------
    output$cat_blurb <- renderUI({
      k <- input$cat %||% "ch2"
      if (k == "custom") return(p(class = "small", style = "margin:12px 0 0", "Write or paste your own problem, enter or upload the data, and work through the same 7 steps."))
      ch <- PS_CHAPTERS[[k]]
      div(style = "margin-top:12px", h3(class = "ct", ch$title), p(class = "small", style = "margin:4px 0 0",
          if (k == "ch2") "Grouped by the number of samples, then by whether the test is parametric or non-parametric. Click a heading to collapse it."
          else "Every problem is a completely randomized design (one-way layout), grouped by whether the test is parametric or non-parametric."))
    })
    card <- function(p, cl, cur_id) {
      tags$a(class = paste("kcard acard ps-card", if (identical(cur_id, p$id)) "on"), href = "#",
             onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", ns("pick"), p$id),
             div(class = "hrow", span(class = "ps-num", paste("Problem", p$id)), if (identical(cur_id, p$id)) div(class = "ml", pill("Selected", "pass", "check"))),
             div(class = "at", p$title),
             div(class = "ad", sprintf("%s · %d observations", PS_KIND_LABEL[[cl$kind]], nrow(p$data))),
             div(class = "meta", span(class = "chip", icon("wand", 12), cl$test)))
    }
    group_box <- function(title, n, desc, body, sub = FALSE) {
      tags$details(class = paste(if (sub) "ps-sub" else "ps-group kcard"), open = NA,
                   tags$summary(span(class = "ps-chev", icon("chev", 14)), span(class = "ps-gt", title), span(class = "pill neu", plural(n, "problem")),
                                if (!is.null(desc)) span(class = "small ps-gd", desc)),
                   div(class = "ps-gbody", body))
    }
    output$cards <- renderUI({
      k <- input$cat %||% "ch2"; cur_id <- sel()
      if (k == "custom") {
        cm <- custom()
        return(div(class = "grid g3",
          tags$a(class = "kcard acard ps-card", href = "#", onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'}); return false;", ns("new_custom")),
                 div(class = "hrow", ico_box("plus", "ig")), div(class = "at", "Create a new problem"),
                 div(class = "ad", "Statement, dataset (paste, upload or a class example) and the column roles, then the 7 steps.")),
          if (!is.null(cm)) tags$a(class = paste("kcard acard ps-card", if (identical(cur_id, "custom")) "on"), href = "#",
                                   onclick = sprintf("Shiny.setInputValue('%s', 'custom', {priority: 'event'}); return false;", ns("pick")),
                                   div(class = "hrow", span(class = "ps-num", "Custom")), div(class = "at", prob_label(cm)),
                                   div(class = "ad", sprintf("%d rows · %d columns", nrow(cm$data), ncol(cm$data))))))
      }
      ps <- ps_by_chapter(k)
      q <- tolower(trimws(input$q %||% ""))
      cls <- lapply(ps, ps_classify)
      hit <- vapply(seq_along(ps), function(i) !nzchar(q) || grepl(q, tolower(paste(ps[[i]]$id, ps[[i]]$title, paste(ps[[i]]$statement, collapse = " "), cls[[i]]$test, PS_KIND_LABEL[[cls[[i]]$kind]])), fixed = TRUE), TRUE)
      if (!any(hit)) return(kcard(div(class = "empty", ico_box("search", "io", 24), h2(class = "ct", "No problems match"), p(class = "sub", "Try another word, or clear the search."))))
      pick <- function(kinds = NULL, fam = NULL) which(hit & vapply(cls, function(c) (is.null(kinds) || c$kind %in% kinds) && (is.null(fam) || c$family == fam), TRUE))
      grid_of <- function(idx) div(class = "grid g3", lapply(idx, function(i) card(ps[[i]], cls[[i]], cur_id)))
      div(class = "col ps-groups", lapply(ps_groups(k), function(g) {
        if (is.null(g$subs)) { idx <- pick(fam = g$fam); if (!length(idx)) return(NULL)
          return(group_box(g$title, length(idx), PS_GROUP_DESC[[g$title]], grid_of(idx))) }
        idx_all <- pick(kinds = g$kinds)
        if (!length(idx_all)) return(if (nzchar(q)) NULL else group_box(g$title, 0, PS_GROUP_DESC[[g$title]],
          p(class = "small", style = "margin:0", paste("No problem in this chapter uses this layout.", if (g$title == "K sample") "The k-sample (one-way ANOVA) problems are in Chapter 3." else ""))))
        group_box(g$title, length(idx_all), PS_GROUP_DESC[[g$title]], lapply(g$subs, function(sb) {
          idx <- pick(kinds = sb$kinds %||% g$kinds, fam = sb$fam); if (!length(idx)) return(NULL)
          group_box(sb$t, length(idx), NULL, grid_of(idx), sub = TRUE) }))
      }))
    })
    output$resume <- renderUI({
      i <- sel(); if (is.null(i)) return(NULL)
      m <- meta_of(i); if (is.null(m)) return(NULL)
      note(kind = "g", ic = "check", div(class = "hrow", style = "gap:12px",
        span(sprintf("You are working on %s%s, at step %d of 7.", prob_label(m), if (identical(i, "custom")) "" else paste0(": ", m$title), min(cur$step, 7))),
        div(class = "ml"), tags$button(class = "btn-k bp bsm", onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("pick"), i), icon("arrowr", 14), "Continue")))
    })

    # ---------- Opening a problem ----------
    save_cur <- function() {
      i <- isolate(cur$id); if (is.null(i)) return()
      memo[[i]] <- isolate(list(s = cur$s, h0 = cur$h0, h1 = cur$h1, hs0 = cur$hs0, hs1 = cur$hs1, test = cur$test, test_user = cur$test_user,
                                ph = cur$ph, res = cur$res, step = cur$step, seen = cur$seen))
    }
    open_problem <- function(i, step = NULL) {
      m <- meta_of(i); if (is.null(m)) return()
      save_cur()
      st <- ps_structure(m$data, m$roles); kind <- ps_kind(st, m)
      if (is.na(kind) || kind == "blocked") { showNotification(ps_kind_problem(kind, st), type = "error", duration = 10); return() }
      sv <- isolate(memo[[i]])
      s <- sv$s %||% ps_default_settings(m, kind)
      info <- ps_info(kind, st, m, s); h <- ps_hypotheses(kind, st, m, s, info$family)
      keyn(isolate(keyn()) + 1)
      cur$id <- i; cur$meta <- m; cur$st <- st; cur$kind <- kind; cur$s <- s
      cur$h0 <- sv$h0 %||% h$h0; cur$h1 <- sv$h1 %||% h$h1; cur$hs0 <- sv$hs0 %||% h$h0; cur$hs1 <- sv$hs1 %||% h$h1
      cur$test <- sv$test %||% info$suggest; cur$test_user <- isTRUE(sv$test_user)
      cur$ph <- sv$ph %||% (if (s$posthoc %in% c("tukey", "lsd", "bonferroni", "scheffe")) s$posthoc else "tukey")
      cur$res <- sv$res; cur$seen <- sv$seen %||% 1
      cur$key <- paste0(gsub("[^A-Za-z0-9]", "", i), "k", isolate(keyn()))
      sel(i)
      go(step %||% sv$step %||% 1, scroll = FALSE)
      view("detail", c("Design of experiments", "Problem Sets", prob_label(m)))
    }
    observeEvent(input$pick, open_problem(input$pick))
    observeEvent(input$back, { save_cur(); view("list") })

    # ---------- Live settings from the step inputs ----------
    observe({
      k <- cur$key; req(k)
      g <- function(n) input[[paste0(n, "_", k)]]
      v <- list(alt = g("alt"), value = g("value"), alpha = g("alpha"), family = g("family"), var_mode = g("var_mode"),
                sigma = g("sigma"), sigma2 = g("sigma2"), test = g("test"), ph = g("ph"), h0 = g("h0"), h1 = g("h1"))
      isolate({
        s <- cur$s
        if (!is.null(v$alt)) s$alt <- v$alt
        if (!is.null(v$value) && is.finite(v$value)) s$value <- v$value
        if (!is.null(v$alpha) && is.finite(v$alpha) && v$alpha > 0 && v$alpha < 1) s$alpha <- v$alpha
        if (!is.null(v$family)) s$family <- v$family
        if (!is.null(v$var_mode)) s$var_mode <- v$var_mode
        if (!is.null(v$sigma)) s["sigma"] <- list(if (is.finite(v$sigma) && v$sigma > 0) v$sigma else NULL)
        if (!is.null(v$sigma2)) s["sigma2"] <- list(if (is.finite(v$sigma2) && v$sigma2 > 0) v$sigma2 else NULL)
        s <- Filter(Negate(is.null), s)
        if (!identical(s, cur$s)) cur$s <- s
        if (!is.null(v$test) && !identical(v$test, cur$test)) { cur$test <- v$test; cur$test_user <- TRUE }
        if (!is.null(v$ph)) cur$ph <- v$ph
        if (!is.null(v$h0)) cur$h0 <- v$h0
        if (!is.null(v$h1)) cur$h1 <- v$h1
      })
    })
    info <- reactive({ req(cur$kind); ps_info(cur$kind, cur$st, cur$meta, cur$s) })
    choices <- reactive({ req(cur$kind); ps_test_choices(cur$kind, cur$s) })
    # Keep the hypotheses' wording in step with the settings until the student edits it.
    observe({
      req(cur$kind); h <- ps_hypotheses(cur$kind, cur$st, cur$meta, cur$s, info()$family)
      isolate({
        if (identical(cur$h0, cur$hs0) && !identical(h$h0, cur$h0)) { cur$h0 <- h$h0; updateTextAreaInput(session, kid("h0"), value = h$h0) }
        if (identical(cur$h1, cur$hs1) && !identical(h$h1, cur$h1)) { cur$h1 <- h$h1; updateTextAreaInput(session, kid("h1"), value = h$h1) }
        cur$hs0 <- h$h0; cur$hs1 <- h$h1
      })
    })
    # The test follows the information step unless the student picked one.
    observe({
      ch <- choices(); sug <- info()$suggest
      isolate({
        t <- if (!cur$test_user || !cur$test %in% names(ch)) sug else cur$test
        if (!cur$test %in% names(ch)) cur$test_user <- FALSE
        cur$test <- t
        updateSelectInput(session, kid("test"), choices = stats::setNames(names(ch), ifelse(names(ch) == sug, paste0(ch, "  ★ suggested"), ch)), selected = t)
      })
    })
    observeEvent(input[[paste0("h_reset_", cur$key)]], {
      h <- ps_hypotheses(cur$kind, cur$st, cur$meta, cur$s, info()$family)
      cur$h0 <- h$h0; cur$h1 <- h$h1; cur$hs0 <- h$h0; cur$hs1 <- h$h1
      updateTextAreaInput(session, kid("h0"), value = h$h0); updateTextAreaInput(session, kid("h1"), value = h$h1)
    }, ignoreInit = TRUE)

    # ---------- Running the test ----------
    stale <- reactive(!is.null(cur$res) && !identical(cur$res$sig, list(cur$test, cur$s)))
    run_test <- function(quiet_ok = FALSE) {
      res <- tryCatch(ps_run_test(cur$kind, cur$test, cur$st, cur$meta, cur$s), error = function(e) e)
      if (inherits(res, "error")) { showNotification(paste("The test could not be run:", conditionMessage(res)), type = "error", duration = 10); return(FALSE) }
      res$sig <- list(cur$test, cur$s); cur$res <- res
      if (!quiet_ok) showNotification(sprintf("%s: %s = %s, %s.", res$t$name, res$t$stat_name, ps_fmt(res$t$stat, 4), ps_pe(res$p)), type = "message")
      TRUE
    }
    go <- function(n, scroll = TRUE) {
      n <- max(1, min(8, as.integer(n)))
      if (n %in% 6:7 && (is.null(isolate(cur$res)) || isolate(stale()))) if (!run_test(quiet_ok = TRUE)) n <- 5
      cur$step <- n; cur$seen <- max(isolate(cur$seen), min(n, 7))
      updateTabsetPanel(session, "steps", selected = as.character(n))
      if (scroll) session$sendCustomMessage("scroll_top", list())
    }
    observeEvent(input$go, go(input$go))
    observeEvent(input[[paste0("run_", cur$key)]], run_test(), ignoreInit = TRUE)

    # ---------- Header and stepper ----------
    output$detail_head <- renderUI({
      m <- cur$meta; req(m)
      page_head(prob_label(m), if (identical(m$id, "custom")) "Your own problem" else m$title,
                btn(ns("back"), "All problems", "arrowl", "bs", "bsm"),
                if (identical(m$id, "custom")) btn(ns("edit_custom"), "Edit problem", "edit", "bs", "bsm"),
                pin_button(ns))
    })
    output$stepper <- renderUI({
      req(cur$key); n <- cur$step; seen <- cur$seen
      steps <- lapply(seq_along(PS_STEPS), function(i) {
        cls <- if (i == n) "cur" else if (i <= seen) "done" else ""
        tags$button(class = paste("st", cls), title = sprintf("Step %d: %s", i, PS_STEPS[i]), onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("go"), i),
                    div(class = "dot", if (i < n && i <= seen) icon("check", 13, 3) else i), span(class = "lbl", PS_STEPS_SHORT[i]))
      })
      parts <- list()
      for (i in seq_along(steps)) { parts[[length(parts) + 1]] <- steps[[i]]; if (i < length(steps)) parts[[length(parts) + 1]] <- div(class = paste("sline", if (i < seen) "done")) }
      div(class = "ps-stepbar",
          div(class = "stepper ps-stepper", parts),
          tags$button(class = paste("ps-asm-tab", if (n == 8) "on"), onclick = sprintf("Shiny.setInputValue('%s', 8, {priority: 'event'})", ns("go")),
                      icon("scale", 16), span("Assumption checks")))
    })
    nav <- function(back = NULL, nxt = NULL, nxt_label = NULL, extra = NULL) {
      div(class = "ps-nav",
          if (!is.null(back)) tags$button(class = "btn-k bs", onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("go"), back), icon("arrowl", 15), sprintf("Back: %s", if (back == 8) "Assumptions" else PS_STEPS[back])),
          extra, div(class = "ml"),
          if (!is.null(nxt)) tags$button(class = "btn-k bp", onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("go"), nxt),
                                         sprintf("Next: %s", nxt_label %||% (if (nxt == 8) "Assumption checks" else PS_STEPS[nxt])), icon("arrowr", 15)))
    }
    step_card <- function(n, sub, ..., footer = NULL) {
      kcard(class = "ps-step",
            div(class = "ps-step-head", div(class = "ps-stepn", if (n == 8) icon("scale", 15) else n),
                div(h2(class = "ct", if (n == 8) "Assumption checks" else sprintf("Step %d · %s", n, PS_STEPS[n])), p(class = "sub", style = "margin-top:2px", sub))),
            div(class = "ps-step-body", ...), footer)
    }
    tbl <- function(df) if (!is.null(df) && nrow(df)) div(class = "tscroll", html_table(df))
    plot_box <- function(id, title, h = 260) div(class = "kcard soft ps-plot", div(class = "small", style = "font-weight:600;margin-bottom:6px", title), plotOutput(ns(id), height = h))

    # ---------- Step 1: problem and data ----------
    output$s1 <- renderUI({
      req(cur$key); m <- isolate(cur$meta)
      step_card(1, "Read the question and the data exactly as given.",
        div(class = "split ps-s1",
            div(class = "col",
                div(class = "ps-statement", lapply(m$statement, function(x) p(x)),
                    if (length(m$parts)) tags$ol(class = "ps-parts", lapply(m$parts, function(x) tags$li(sub("^\\([a-z]\\)\\s*", "", x))), type = if (all(grepl("^\\([a-z]\\)", m$parts))) "a" else "1")),
                if (!is.null(m$note)) note(kind = "w", ic = "alert", m$note),
                div(class = "small", m$source %||% "")),
            div(class = "col",
                div(class = "hrow", h3(class = "ct", "Data"), div(class = "ml hrow",
                    downloadButton(ns("dl_data"), label = tagList(icon("download", 14), "CSV"), icon = NULL, class = "btn-k bs bsm"),
                    if (!identical(m$id, "custom")) btn(ns("edit_this"), "Copy to Custom Problem", "copy", "bs", "bsm"))),
                div(class = "tscroll ps-data", html_table(m$display)),
                p(class = "small", style = "margin:0", sprintf("%d observations, as printed in the problem.", nrow(m$data))))),
        footer = nav(nxt = 2))
    })
    output$dl_data <- downloadHandler(filename = function() sprintf("problem_%s.csv", gsub("[^A-Za-z0-9]+", "_", cur$meta$id)),
                                      content = function(file) utils::write.csv(cur$meta$data, file, row.names = FALSE))

    # ---------- Step 2: hypotheses ----------
    output$s2 <- renderUI({
      req(cur$key); kind <- isolate(cur$kind); s <- isolate(cur$s)
      step_card(2, "State the null hypothesis H0 (no effect, no difference) and the alternative H1 (what the question wants to show). The system suggests both; edit them if needed.",
        if (kind != "k") div(class = "grid g2", style = "gap:14px 20px",
            selectInput(ns(kid("alt")), "Type of alternative", ps_alt_choices(kind), s$alt, width = "100%"),
            numericInput(ns(kid("value")), ps_value_label(kind), s$value, width = "100%")),
        uiOutput(ns("h_sym")),
        div(class = "ps-hyp",
            div(class = "ps-hlab", "H0"), textAreaInput(ns(kid("h0")), NULL, isolate(cur$h0), rows = 2, width = "100%", resize = "vertical"),
            div(class = "ps-hlab alt", "H1"), textAreaInput(ns(kid("h1")), NULL, isolate(cur$h1), rows = 2, width = "100%", resize = "vertical")),
        div(class = "hrow", actionLink(ns(kid("h_reset")), tagList(icon("wand", 14), " Restore the suggested wording"), class = "small", style = "color:var(--green);font-weight:600")),
        footer = nav(1, 3))
    })
    output$h_sym <- renderUI({
      req(cur$kind); h <- ps_hypotheses(cur$kind, cur$st, cur$meta, cur$s, info()$family)
      div(class = "ps-sym", span(class = "chip", h$h0s), span(class = "chip", h$h1s),
          span(class = "small", if (cur$kind == "k") "With three or more groups H1 is always “at least one differs”."
               else switch(cur$s$alt, two.sided = "Two-sided: H1 allows a difference in either direction.",
                           greater = "Right-tailed: the question asks whether the value is greater.", less = "Left-tailed: the question asks whether the value is less.")))
    })

    # ---------- Step 3: significance level ----------
    output$s3 <- renderUI({
      req(cur$key); s <- isolate(cur$s); m <- isolate(cur$meta)
      stated <- ps_stated_alpha(m)
      step_card(3, "Choose α, the probability of a Type I error: rejecting H0 when it is actually true.",
        div(class = "ps-alpha",
            div(numericInput(ns(kid("alpha")), "Significance level α", s$alpha, min = 0.001, max = 0.5, step = 0.005, width = "220px"),
                div(class = "hrow", style = "gap:6px;margin-top:8px", lapply(c(0.01, 0.05, 0.10), function(a)
                  tags$button(class = "btn-k bs bsm", onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", ns("alpha_preset"), a), format(a))))),
            div(class = "col", style = "gap:10px",
                note(kind = if (is.na(stated)) "i" else "g", ic = if (is.na(stated)) "info" else "check",
                     if (!is.na(stated)) sprintf("The question states α = %s, so it has been filled in.", format(stated))
                     else "The question does not state α, so the usual α = 0.05 is filled in. Change it if your instructor uses another level."),
                uiOutput(ns("alpha_note")))),
        footer = nav(2, 4))
    })
    observeEvent(input$alpha_preset, updateNumericInput(session, kid("alpha"), value = input$alpha_preset))
    output$alpha_note <- renderUI({
      a <- cur$s$alpha; req(a)
      div(class = "ps-kv",
          div(class = "kv-row", span("α (Type I error)"), span(format(a))),
          div(class = "kv-row", span("Confidence level for intervals, 1 − α"), span(sprintf("%g%%", 100 * (1 - a)))),
          p(class = "small", style = "margin:8px 0 0", sprintf("If H0 is true, a test at α = %s wrongly rejects it %g%% of the time. A smaller α makes rejecting harder.", format(a), 100 * a)))
    })

    # ---------- Step 4: information available ----------
    output$s4 <- renderUI({
      req(cur$key); kind <- isolate(cur$kind); s <- isolate(cur$s)
      step_card(4, "Collect what is known about the populations and the samples. This decides between a parametric and a non-parametric test, and checks independence first.",
        div(class = "grid g3", style = "gap:14px 20px",
            if (kind != "variance") selectInput(ns(kid("family")), "Type of test", c("Decide from the information" = "auto", "Parametric" = "param", "Non-parametric" = "nonpar"), s$family %||% "auto", width = "100%"),
            if (kind == "one") numericInput(ns(kid("sigma")), "Population σ, if known", s[["sigma"]] %||% NA, min = 0, width = "100%"),
            if (kind == "two") numericInput(ns(kid("sigma")), "Population σ1, if known", s[["sigma"]] %||% NA, min = 0, width = "100%"),
            if (kind == "two") numericInput(ns(kid("sigma2")), "Population σ2, if known", s[["sigma2"]] %||% NA, min = 0, width = "100%"),
            if (kind == "two") selectInput(ns(kid("var_mode")), "Equal variances?", c("Decide with the F test" = "auto", "Assume equal" = "equal", "Assume unequal" = "unequal"), s$var_mode %||% "auto", width = "100%")),
        uiOutput(ns("info_body")),
        footer = nav(3, 5))
    })
    output$info_body <- renderUI({
      inf <- info(); a <- cur$s$alpha; kind <- cur$kind
      ind <- inf$independence; ind_ok <- is.na(ind$p) || ind$p >= a
      tagList(
        div(class = "grid g2 ps-info", style = "margin-top:16px",
            div(class = "col", style = "gap:14px",
                div(h3(class = "ct", "What we know"), div(style = "margin-top:8px", lapply(seq_len(nrow(inf$facts)), function(i) div(class = "kv-row ps-fact", span(inf$facts$What[i]), span(inf$facts$Information[i]))))),
                div(class = "kcard soft", h3(class = "ct", span(class = "hrow", icon("clock", 16), "Independence check (first)")),
                    div(class = "kv-row", span("Durbin-Watson statistic"), span(ps_fmt(ind$stat, 4))), div(class = "kv-row", span("p-value"), span(ps_p(ind$p))),
                    div(style = "margin-top:8px", if (ind_ok) pill("Independence holds", "pass", "check") else pill("Observations look dependent", "fail", "x")),
                    p(class = "small", style = "margin:8px 0 0", if (ind_ok) sprintf("No evidence that consecutive observations are correlated (p ≥ %s). Together with the randomised design, the observations can be treated as independent.", format(a))
                      else "Consecutive residuals are correlated. Check how the data were collected before trusting any test."))),
            div(class = "col", style = "gap:14px",
                div(class = "kcard soft", h3(class = "ct", "Normality"),
                    tbl(transform(inf$normality, W = ps_fmt(W, 4), p = ps_p(p))),
                    p(class = "small", style = "margin:8px 0 0", if (inf$stated) "The problem states the population is normal." else sprintf("Shapiro-Wilk at α = %s: p ≥ α means no evidence against normality.", format(a)))),
                if (!is.null(inf$equal_var)) div(class = "kcard soft", h3(class = "ct", "Equal variances"),
                    tbl(transform(inf$equal_var$table, p = ps_p(p))),
                    p(class = "small", style = "margin:8px 0 0", sprintf("The %s decides: %s.", inf$equal_var$basis, if (inf$equal_var$equal) "the variances can be treated as equal" else "the variances differ"))),
                plot_box("pl_ind", "Residuals in recorded order (independence)", 200))),
        div(class = paste("ps-verdict", if (inf$family == "param") "param" else "nonpar"),
            div(class = "ps-vtag", if (inf$family == "param") "Parametric test" else "Non-parametric test"),
            div(tags$b(inf$family_reason), br(), span(sprintf("Suggested test: %s. %s", ps_test_choices(kind, cur$s)[[inf$suggest]], inf$suggest_reason)))))
    })
    output$pl_ind <- renderPlot({ ps_plot_order(info()$independence$fit) }, bg = "transparent", res = 96)

    # ---------- Step 5: test ----------
    output$s5 <- renderUI({
      req(cur$key); ch <- isolate(choices()); sug <- isolate(info()$suggest)
      step_card(5, "Run the test chosen from the information in step 4. You can choose another test if you have a reason to.",
        div(class = "hrow", style = "align-items:flex-end;gap:14px",
            div(style = "flex:1;min-width:260px", selectInput(ns(kid("test")), "Test", stats::setNames(names(ch), ifelse(names(ch) == sug, paste0(ch, "  ★ suggested"), ch)), isolate(cur$test), width = "100%")),
            actionButton(ns(kid("run")), label = tagList(icon("play", 15), "Run test"), icon = NULL, class = "btn-k bp")),
        uiOutput(ns("test_body")),
        footer = nav(4, 6))
    })
    output$test_body <- renderUI({
      r <- cur$res; inf <- info()
      choice_note <- if (!identical(cur$test, inf$suggest)) note(kind = "w", ic = "edit", sprintf("You chose %s; the information in step 4 suggests %s.", choices()[[cur$test]], choices()[[inf$suggest]]))
      if (is.null(r)) return(tagList(choice_note, div(class = "ps-empty", icon("play", 22), p(class = "sub", "Press Run test to compute the test statistic and p-value."))))
      t <- r$t
      tile <- function(label, value, sub = NULL) div(class = "ps-tile", div(class = "small", label), div(class = "ps-tv", value), if (!is.null(sub)) div(class = "small", sub))
      dfx <- if (is.character(t$df)) t$df else if (!is.na(t$df %||% NA)) ps_fmt(t$df, 4) else "—"
      tagList(choice_note,
        if (stale()) note(kind = "i", ic = "info", "The settings or the test changed after this result was computed. Press Run test to update it."),
        div(class = "grid g4", style = "margin-top:14px",
            tile("Test", t$name), tile(sprintf("Test statistic (%s)", t$stat_name), ps_fmt(t$stat, 5), if (dfx != "—") paste("df =", dfx)),
            tile("P-value", ps_p(t$p), if (r$kind == "k") "right tail" else switch(r$alt, two.sided = "two-sided", greater = "right tail", less = "left tail")),
            tile(if (!is.null(t$est_name)) t$est_name else "α", if (!is.null(t$est_name)) ps_fmt(t$est) else format(r$alpha))),
        div(class = "grid g2", style = "margin-top:14px;align-items:start",
            div(h3(class = "ct", "How the statistic is computed"), div(class = "am-formula", lapply(r$formula, div))),
            div(h3(class = "ct", "Sample summary"), div(style = "margin-top:6px", tbl(ps_desc_table(r))))),
        if (r$kind == "k" && r$test == "anova") div(style = "margin-top:14px", h3(class = "ct", "ANOVA table"), div(style = "margin-top:6px", tbl(ps_anova_table(r)))),
        div(style = "margin-top:14px", h3(class = "ct", "Result"), div(style = "margin-top:6px", tbl(ps_test_table(r)))))
    })

    # ---------- Step 6: decision ----------
    decision <- reactive({ r <- cur$res; req(r); ps_decision(r, cur$h1 %||% "") })
    output$s6 <- renderUI({ req(cur$key); step_card(6, "Decide whether to reject H0 with three methods: the rejection region, the confidence interval and the p-value. With a 1 − α interval they normally lead to the same decision.",
                                                    uiOutput(ns("dec_body")), footer = nav(5, 7)) })
    output$dec_body <- renderUI({
      d <- decision(); r <- cur$res
      method <- function(n, title, ok, txt, plot = NULL) div(class = "kcard ps-method",
        div(class = "hrow", span(class = "ps-mn", n), h3(class = "ct", title), div(class = "ml", if (is.na(ok)) pill("Not available", "neu") else if (ok) pill("Reject H0", "pass") else pill("Fail to reject H0", "neu"))),
        if (!is.null(plot)) plotOutput(ns(plot), height = 210), p(class = "small", style = "margin:8px 0 0", txt))
      tagList(
        div(class = paste("ps-decision", if (d$reject) "rej" else "keep"),
            div(class = "ps-dicon", icon(if (d$reject) "check" else "info", 22, 2.6)),
            div(div(class = "ps-dt", if (d$reject) "Reject H0" else "Fail to reject H0"), p(style = "margin:4px 0 0", d$conclusion))),
        div(class = "grid g3", style = "margin-top:16px;align-items:start",
            method(1, "Rejection region", d$rr$reject, d$rr$text, "pl_rr"),
            method(2, "Confidence interval", if (isTRUE(d$ci$available)) d$ci$reject else NA, d$ci$text, if (isTRUE(d$ci$available)) "pl_ci"),
            method(3, "P-value", d$pv$reject, d$pv$text, "pl_pv")),
        div(class = "ps-hyprecap", span(class = "small", tags$b("H0: "), cur$h0), span(class = "small", tags$b("H1: "), cur$h1)))
    })
    output$pl_rr <- renderPlot({ d <- decision(); ps_plot_rr(d$ref, cur$res$alpha) }, bg = "transparent", res = 96)
    output$pl_ci <- renderPlot({ d <- decision(); r <- cur$res; req(isTRUE(d$ci$available))
      if (r$kind == "k") ps_plot_ci_k(d$ci$table, r$conf) else plot_ci_one(r$t, ref_value(r), r$conf, cur$meta$labels$unit %||% "") }, bg = "transparent", res = 96)
    output$pl_pv <- renderPlot({ d <- decision(); ps_plot_pvalue(d$ref, cur$res$p, cur$res$alpha) }, bg = "transparent", res = 96)

    # ---------- Step 7: post-hoc ----------
    output$s7 <- renderUI({
      req(cur$key); r <- cur$res
      ch <- if (!is.null(r) && r$kind == "k") ps_posthoc_choices(r$test) else character(0)
      step_card(7, "When H0 is rejected for three or more groups, find which pairs of groups differ.",
        if (length(ch) && isTRUE(r$reject)) div(style = "width:300px;max-width:100%", selectInput(ns(kid("ph")), "Method", stats::setNames(names(ch), ch), if (isolate(cur$ph) %in% names(ch)) isolate(cur$ph) else names(ch)[1], width = "100%")),
        uiOutput(ns("ph_body")),
        footer = nav(6, 8))
    })
    posthoc <- reactive({
      r <- cur$res; req(r, r$kind == "k", r$reject)
      ch <- ps_posthoc_choices(r$test); m <- if (cur$ph %in% names(ch)) cur$ph else names(ch)[1]
      c(ps_posthoc(r, m), list(method = m, name = ch[[m]]))
    })
    output$ph_body <- renderUI({
      r <- cur$res; req(r)
      if (r$kind != "k") {
        t <- r$t
        return(div(class = "ps-empty left", icon("info", 20), div(p(class = "sub", style = "margin:0", sprintf("Post-hoc comparisons are only needed for three or more groups. With %s, the test in step 5 already answers the question.",
                                                                                                               switch(r$kind, one = , variance = "one sample", "two samples"))),
                                                                  if (!is.null(t$est_name) && (is.finite(t$lwr %||% NA) || is.finite(t$upr %||% NA)))
                                                                    p(class = "small", style = "margin:6px 0 0", sprintf("%s = %s, %g%% interval [%s, %s].", t$est_name, ps_fmt(t$est), 100 * r$conf, ps_fmt(t$lwr), ps_fmt(t$upr))))))
      }
      if (!r$reject) return(div(class = "ps-empty left", icon("info", 20), p(class = "sub", style = "margin:0", sprintf("H0 was not rejected (%s ≥ α = %s), so there is no evidence that any group differs and no post-hoc analysis is carried out.", ps_pe(r$p), format(r$alpha)))))
      ph <- posthoc(); nsig <- sum(ph$raw$signif)
      tagList(
        p(class = "ltext", sprintf("%s: %d of %d pairs differ at α = %s. Groups that share a letter are not significantly different.", ph$name, nsig, nrow(ph$raw), format(r$alpha))),
        div(class = "grid g2", style = "margin-top:12px;align-items:start",
            div(h3(class = "ct", "Pairwise comparisons"), div(style = "margin-top:6px", tbl(ph$table))),
            div(class = "col", style = "gap:14px", div(h3(class = "ct", "Grouping"), div(style = "margin-top:6px", tbl(ph$groups))),
                if (!is.null(ph$raw$lwr)) plot_box("pl_ph", "Simultaneous intervals for the differences", 240))))
    })
    output$pl_ph <- renderPlot({ ph <- posthoc(); req(!is.null(ph$raw$lwr)); ps_plot_ci_k(ph$raw, cur$res$conf) }, bg = "transparent", res = 96)

    # ---------- Assumption checks ----------
    asm <- reactive({ req(cur$kind); ps_assumptions(cur$kind, cur$st, cur$s) })
    output$s8 <- renderUI({ req(cur$key); step_card(8, "Check normality, independence and constant variance, the assumptions behind the parametric tests.", uiOutput(ns("asm_body")), footer = nav(7, NULL)) })
    output$asm_body <- renderUI({
      a <- asm(); kind <- cur$kind; al <- cur$s$alpha
      status <- function(ok) if (is.na(ok)) pill("Not applicable", "neu") else if (ok) pill("Satisfied", "pass", "check") else pill("Not satisfied", "fail", "x")
      sec <- function(title, ok, ...) div(class = "kcard ps-asm", div(class = "hrow", h3(class = "ct", title), div(class = "ml", status(ok))), ...)
      tagList(
        sec("Normality", a$normal_ok, tbl(a$normality),
            p(class = "small", style = "margin:8px 0", sprintf("H0: the %s come from a normal distribution. p ≥ %s means no evidence against normality.", switch(kind, paired = "differences", k = "residuals", two = "samples", "data"), format(al))),
            div(class = "grid g2", plot_box("pl_qq", "Normal QQ plot"), plot_box("pl_hist", "Histogram"))),
        sec("Independence", a$indep_ok, tbl(a$independence),
            p(class = "small", style = "margin:8px 0", "H0: the observations are independent. The residuals should show no pattern over the recorded order. Randomising the run order protects this assumption."),
            plot_box("pl_ord", "Residuals in recorded order", 220)),
        sec("Constant variance", a$var_ok,
            if (is.null(a$variance)) p(class = "small", style = "margin:8px 0 0", sprintf("Not applicable: %s has no groups whose variances could differ.", if (kind == "paired") "the paired test works on one set of differences, so it" else "a single sample"))
            else tagList(tbl(a$variance), p(class = "small", style = "margin:8px 0", sprintf("H0: all groups have the same variance. p ≥ %s means the spread can be treated as equal.", format(al))),
                         div(class = "grid g2", plot_box("pl_box", "Spread of each group"), plot_box("pl_rvf", "Residuals versus fitted values")))))
    })
    output$pl_qq <- renderPlot({ a <- asm(); tg <- a$targets; if (length(tg) > 1) plot_qq_groups(tg, names(tg)) else plot_qq_vec(tg[[1]], names(tg)[1]) }, bg = "transparent", res = 96)
    output$pl_hist <- renderPlot({ a <- asm(); tg <- a$targets; if (length(tg) > 1) plot_hist_groups(tg, names(tg), cur$st$roles$response) else plot_hist(tg[[1]], names(tg)[1]) }, bg = "transparent", res = 96)
    output$pl_ord <- renderPlot({ ps_plot_order(asm()$ind_fit) }, bg = "transparent", res = 96)
    output$pl_box <- renderPlot({ ps_plot_data(cur$kind, cur$st, cur$meta) }, bg = "transparent", res = 96)
    output$pl_rvf <- renderPlot({ ps_plot_rvf(asm()$ind_fit) }, bg = "transparent", res = 96)

    # ---------- Pin to report ----------
    pin_observers(input, session, store,
      items = reactive({
        r <- cur$res; req(r); d <- ps_decision(r, cur$h1 %||% "")
        it <- list(
          hyp = list(label = "Hypotheses and significance level", default = TRUE, build = function() mk_pin("text", "Hypotheses", "file",
                     text = sprintf("H0: %s\nH1: %s\nSignificance level α = %s.", cur$h0, cur$h1, format(r$alpha)))),
          test = list(label = "Test result", default = TRUE, build = function() mk_pin("table", r$t$name, "table", df = ps_test_table(r))),
          decision = list(label = "Decision (three methods)", default = TRUE, build = function() mk_pin("text", "Decision", "check",
                          text = paste(c(d$conclusion, paste("Rejection region:", d$rr$text), paste("Confidence interval:", d$ci$text), paste("P-value:", d$pv$text)), collapse = "\n"))),
          rr = list(label = "Rejection region plot", default = FALSE, build = function() mk_pin("plot", "Rejection region", "bar", plot = ps_plot_rr(d$ref, r$alpha), h = 3)))
        if (r$kind == "k" && r$test == "anova") it$anova <- list(label = "ANOVA table", default = TRUE, build = function() mk_pin("table", "ANOVA table", "table", df = ps_anova_table(r)))
        if (r$kind == "k" && r$reject) it$posthoc <- list(label = "Post-hoc comparisons", default = TRUE, build = function() { ph <- posthoc(); mk_pin("table", ph$name, "columns", df = ph$table) })
        it
      }),
      src = reactive({
        r <- cur$res; m <- cur$meta
        list(key = paste0("problem_", gsub("[^A-Za-z0-9]", "_", m$id)), label = sprintf("%s · %s", prob_label(m), r$t$name),
             title = if (identical(m$id, "custom")) prob_label(m) else sprintf("Problem %s: %s", m$id, m$title),
             subtitle = sprintf("%s · α = %s", r$t$name, format(r$alpha)), dataset = prob_label(m), notes = c(m$source %||% "", m$note %||% character(0)), code = "")
      }))

    # ---------- Custom problem editor ----------
    role_ids <- c(response = "c_response", treatment = "c_treatment", block = "c_block")
    # "(none)" needs a real value: the dropdown cannot send an empty one, so an emptied role
    # would otherwise keep pointing at a column from the previous dataset.
    NO_ROLE <- ".none"
    set_roles <- function(df, roles = NULL) {
      ch <- c("(none)" = NO_ROLE, names(df))
      if (is.null(roles)) roles <- ps_guess_roles(df)
      for (k in names(role_ids)) {
        v <- roles[[k]] %||% ""
        updateSelectInput(session, role_ids[[k]], choices = ch, selected = if (nzchar(v) && v %in% names(df)) v else NO_ROLE)
      }
    }
    load_data <- function(df, roles = NULL) {
      df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
      df[] <- lapply(df, as.character)
      rownames(df) <- NULL
      cdata(df); crev(isolate(crev()) + 1); set_roles(df, roles)
    }
    open_editor <- function(seed = NULL) {
      if (!is.null(seed)) {
        updateTextInput(session, "c_title", value = seed$title)
        updateTextAreaInput(session, "c_statement", value = seed$statement)
        updateCheckboxInput(session, "c_variance", value = identical(seed$target, "variance"))
        load_data(seed$data, seed$roles)
        session$userData$ps_seed <- seed
      }
      view("edit", c("Design of experiments", "Problem Sets", "Custom problem"))
    }
    observeEvent(input$new_custom, { if (is.null(custom())) session$userData$ps_seed <- NULL; open_editor() })
    observeEvent(input$edit_custom, open_editor())
    observeEvent(input$edit_this, {
      m <- cur$meta; req(m)
      open_editor(list(title = sprintf("Problem %s (edited)", m$id), statement = paste(c(m$statement, m$parts), collapse = "\n"),
                       data = m$data, roles = m$roles, target = m$target, settings = m$settings, labels = m$labels))
      showNotification(sprintf("Problem %s copied into the editor. Change anything, then press Continue to the 7 steps.", m$id), type = "message")
    })
    observeEvent(input$edit_back, view("list"))
    observeEvent(input$c_use_paste, {
      df <- tryCatch(parse_pasted(input$c_paste %||% "", isTRUE(input$c_header)), error = function(e) e)
      if (inherits(df, "error") || is.null(df) || !nrow(df)) { showNotification("Nothing could be read from the pasted text. Put one row per line, with values separated by tabs or commas.", type = "error"); return() }
      load_data(df); showNotification(sprintf("Read %d rows and %d columns.", nrow(df), ncol(df)), type = "message")
    })
    observeEvent(input$c_file, {
      f <- input$c_file; req(f)
      ext <- tolower(tools::file_ext(f$name))
      df <- tryCatch(if (ext %in% c("xlsx", "xls")) read_excel_sheet(f$datapath, readxl::excel_sheets(f$datapath)[1]) else read_text_table(f$datapath)$data, error = function(e) e)
      if (inherits(df, "error") || !nrow(df)) { showNotification(sprintf("The file could not be read: %s", if (inherits(df, "error")) conditionMessage(df) else "it has no rows."), type = "error"); return() }
      load_data(df); showNotification(sprintf("Read %d rows and %d columns from %s.", nrow(df), ncol(df), f$name), type = "message")
    })
    observeEvent(input$c_use_ex, {
      ex <- CLASS_EXAMPLES[[input$c_ex]]; req(ex)
      load_data(ex$data)
      if (!nzchar(input$c_title %||% "")) updateTextInput(session, "c_title", value = ex$title)
      if (!nzchar(input$c_statement %||% "")) updateTextAreaInput(session, "c_statement", value = ex$desc)
    })
    output$c_grid <- rhandsontable::renderRHandsontable({
      crev(); df <- isolate(cdata())
      if (is.null(df)) df <- data.frame(Group = rep(NA_character_, 6), Response = rep(NA_character_, 6), check.names = FALSE)
      rhandsontable::rhandsontable(df, useTypes = FALSE, stretchH = "all", height = min(460, 60 + 30 * max(nrow(df), 6)), rowHeaders = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
    observeEvent(input$c_grid, {
      df <- tryCatch(rhandsontable::hot_to_r(input$c_grid), error = function(e) NULL); req(!is.null(df))
      df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
      df[] <- lapply(df, function(c) { c <- as.character(c); c[!is.na(c) & trimws(c) == ""] <- NA; c })
      old <- cdata()
      if (is.null(old)) { if (any(!is.na(as.matrix(df)))) { cdata(df); set_roles(df) } ; return() }
      if (!identical(names(df), names(old))) return()
      rownames(df) <- NULL
      if (!identical(df, old)) cdata(df)
    })
    # Wide data (one column per group) can be stacked into long format.
    output$c_wide <- renderUI({
      df <- cdata(); if (is.null(df) || ncol(df) < 2) return(NULL)
      isnum <- vapply(df, function(x) { v <- x[!is.na(x)]; length(v) > 0 && all(!is.na(suppressWarnings(as.numeric(v)))) }, TRUE)
      if (sum(isnum) < 2) return(NULL)
      div(class = "note i", style = "margin-top:12px;display:block",
          div(class = "hrow", icon("info", 16), tags$b("Data in wide format?"), span("If each column is a group (as in a textbook table), stack them into one row per observation.")),
          div(class = "hrow", style = "margin-top:8px;align-items:flex-end",
              div(style = "flex:1;min-width:220px", selectizeInput(ns("c_stack"), "Columns to stack", names(df), selected = names(df)[isnum], multiple = TRUE, width = "100%")),
              div(style = "width:150px", textInput(ns("c_stack_name"), "Group column name", "Group")),
              btn(ns("c_stack_go"), "Stack", "layers", "bs", "bsm")),
          p(class = "small", style = "margin:6px 0 0", "Columns you do not stack (for example a Pair ID) are kept and repeated for every stacked value."))
    })
    observeEvent(input$c_stack_go, {
      df <- cdata(); cols <- intersect(input$c_stack, names(df)); req(length(cols) >= 2)
      keep <- setdiff(names(df), cols); gname <- trimws(input$c_stack_name %||% "Group"); if (!nzchar(gname)) gname <- "Group"
      long <- do.call(rbind, lapply(cols, function(cn) { x <- data.frame(df[keep], check.names = FALSE, stringsAsFactors = FALSE); x[[gname]] <- cn; x$Value <- df[[cn]]; x }))
      long <- long[!is.na(long$Value) & trimws(long$Value) != "", , drop = FALSE]
      roles <- list(response = "Value", treatment = gname, block = if (length(keep) == 1) keep else "")
      load_data(long, roles)
      showNotification(sprintf("Stacked %d columns into %d rows.", length(cols), nrow(long)), type = "message")
    })
    # Roles as chosen, ignoring any that do not name a column of the current data.
    croles <- reactive({
      nm <- names(cdata() %||% data.frame())
      lapply(role_ids, function(x) { v <- input[[x]] %||% ""; if (identical(v, NO_ROLE) || !v %in% nm) "" else v })
    })
    cmeta <- reactive({
      df <- cdata(); req(df)
      txt <- input$c_statement %||% ""; seed <- session$userData$ps_seed
      s <- modifyList(list(alpha = 0.05, alt = "two.sided"), seed$settings %||% list())
      det <- ps_detect_settings(txt)
      for (k in names(det)) if (is.null(seed$settings[[k]])) s[[k]] <- det[[k]]
      paras <- strsplit(txt, "\n\\s*\n")[[1]]; paras <- trimws(paras[nzchar(trimws(paras))])
      roles <- croles()
      same <- !is.null(seed) && identical(unname(unlist(seed$roles[c("response", "treatment", "block")])), unname(unlist(roles)))
      list(id = "custom", chapter = 0, num = "Custom", title = trimws(input$c_title %||% ""), statement = if (length(paras)) paras else "No statement was entered.",
           parts = character(0), data = df, display = df, roles = roles, labels = if (same) seed$labels %||% list() else list(), settings = s,
           target = if (isTRUE(input$c_variance)) "variance" else "mean", source = "Your own problem")
    })
    output$c_check <- renderUI({
      df <- cdata()
      if (is.null(df)) return(note(kind = "i", ic = "info", "Bring in a dataset to see the data checks."))
      m <- cmeta(); st <- ps_structure(m$data, m$roles); kind <- ps_kind(st, m)
      wide <- !nzchar(m$roles$treatment) && sum(vapply(df, function(x) { v <- x[!is.na(x)]; length(v) > 0 && all(!is.na(suppressWarnings(as.numeric(v)))) }, TRUE)) >= 2
      kcard(h3(class = "ct", "Data checks"),
            if (is.na(kind) || kind == "blocked") tagList(note(kind = if (identical(kind, "blocked")) "w" else "e", ic = "alert", ps_kind_problem(kind, st)),
                                                          if (identical(kind, "blocked") && is.function(open_design)) div(style = "margin-top:10px", btn(ns("c_rcbd"), "Open on the RCBD page", "rows", "bs", "bsm")))
            else tagList(
              if (wide) note(kind = "i", ic = "info", "Several numeric columns and no Group: if each column is a sample, press Stack (under the data)."),
              div(class = "kv-row", span("Layout"), span(PS_KIND_LABEL[[kind]])),
              div(class = "kv-row", span("Observations"), span(st$n)),
              if (!is.null(st$ntrt)) div(class = "kv-row", span("Groups"), span(sprintf("%d (%s)", st$ntrt, ps_list(levels(st$d$treatment))))),
              if (length(st$warnings)) div(class = "col", style = "gap:8px;margin-top:8px", lapply(st$warnings, function(x) note(kind = "w", ic = "alert", x)))))
    })
    observeEvent(input$c_rcbd, {
      m <- cmeta(); req(is.function(open_design))
      open_design(m$data, if (nzchar(m$title)) m$title else "Custom problem data")
    })
    observeEvent(input$c_go, {
      df <- cdata()
      if (is.null(df) || !nrow(df)) { showNotification("Add a dataset first: paste it, upload a file or load a class example.", type = "error"); return() }
      m <- cmeta(); st <- ps_structure(m$data, m$roles); kind <- ps_kind(st, m)
      if (is.na(kind) || kind == "blocked") { showNotification(ps_kind_problem(kind, st), type = "error", duration = 10); return() }
      custom(m); memo[["custom"]] <- NULL; if (identical(isolate(cur$id), "custom")) cur$id <- NULL
      open_problem("custom", step = 1)
    })

    # ---------- For Ask Professor Riaz ----------
    list(ai = function(full) {
      i <- cur$id
      if (is.null(i)) return(list(label = "the Problem Sets list", kind = "general", text = if (full) "The student is browsing the Problem Sets list (Chapter 2 one- and two-sample problems and Chapter 3 one-way ANOVA problems)."))
      m <- cur$meta; n <- cur$step; r <- isolate(cur$res)
      list(label = sprintf("%s, step %s", prob_label(m), if (n == 8) "assumption checks" else n), kind = "general",
           text = if (full) paste0("Problem Sets page, 7-step hypothesis-testing process. ", prob_label(m), ". The student is on ",
                                   if (n == 8) "the assumption checks" else sprintf("step %d (%s)", n, PS_STEPS[n]), ". Statement: ", paste(c(m$statement, m$parts), collapse = " "),
                                   " Data (first rows): ", paste(utils::capture.output(print(utils::head(m$data, 12), row.names = FALSE)), collapse = "\n"),
                                   " H0: ", isolate(cur$h0), " H1: ", isolate(cur$h1), " alpha = ", format(isolate(cur$s$alpha)), ".",
                                   if (!is.null(r)) { d <- ps_decision(r, isolate(cur$h1) %||% ""); paste0(" Test: ", r$t$name, ", ", r$t$stat_name, " = ", ps_fmt(r$t$stat, 4), ", ", ps_pe(r$p), ". ", d$conclusion) }
                                   else " The test has not been run yet."))
    })
  })
}

# First guess at the roles from column names and contents.
ps_guess_roles <- function(df) {
  ty <- guess_types(df); nm <- names(df)
  num <- nm[ty == "number"]; cat <- nm[ty != "number"]
  pick <- function(cands, pat) { h <- cands[grepl(pat, tolower(cands))]; if (length(h)) h[1] else "" }
  resp <- pick(num, ROLE_RESPONSE$pattern); if (!nzchar(resp) && length(num)) resp <- tail(num, 1)
  pool <- setdiff(nm, resp)
  # "Column 1", "Column 2" are names made up for data pasted without a header row, so they
  # say nothing about the column's role.
  named <- pool[!grepl("^column \\d+$", tolower(pool))]
  blk <- pick(named, "block|pair|inspector|subject|farm|batch|day|girder|rep$")
  trt <- pick(setdiff(named, blk), ROLE_TREAT$pattern)
  if (!nzchar(trt)) { rest <- setdiff(intersect(cat, pool), blk); if (length(rest)) trt <- rest[1] }
  if (nzchar(blk) && !nzchar(trt)) { trt <- blk; blk <- "" }
  list(response = resp, treatment = trt, block = blk)
}

# Settings that can be read from a typed problem statement.
ps_detect_settings <- function(txt) {
  out <- list(); t <- gsub("\\s+", " ", txt)
  num_after <- function(pat) { m <- regmatches(t, regexpr(pat, t, perl = TRUE, ignore.case = TRUE)); if (length(m)) suppressWarnings(as.numeric(sub(".*?(-?[0-9]*\\.?[0-9]+)\\s*$", "\\1", m, perl = TRUE))) else NA }
  a <- num_after("(α|alpha)\\s*=\\s*0?\\.[0-9]+"); if (!is.na(a) && a > 0 && a < 0.5) out$alpha <- a
  mu <- num_after("(μ0?|mu0?)\\s*=\\s*-?[0-9]*\\.?[0-9]+"); if (!is.na(mu)) out$mu0 <- mu
  sg <- num_after("(σ|sigma)\\s*=\\s*[0-9]*\\.?[0-9]+"); if (!is.na(sg) && sg > 0) out$sigma <- sg
  out
}
