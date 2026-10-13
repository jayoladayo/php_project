# Problem Sets: the course exercises, a custom problem editor, an automatic
# analysis recommendation that the student can override, and the Answer Model.
# Flow on every problem: Problem -> Data -> Recommended analysis -> Run analysis.

PS_REQUIREMENTS <- list(
  one_sample = c("One numeric response measured on independent units", "The data are a random sample from a normal population (checked with Shapiro-Wilk); with n ≥ 30 the t-test is robust", "A target value μ0, and σ if it is known"),
  variance1 = c("One numeric response measured on independent units", "The population is normal: the χ² test on a variance is not robust to non-normality", "A hypothesised variance σ0²"),
  two_sample = c("A numeric response and a factor with exactly 2 levels", "The two samples are independent (different units, random run order)", "Each sample comes from a normal population; equal variances for the pooled test (checked with the F test)"),
  paired = c("A numeric response, a treatment with 2 levels and a pair (block) identifier", "Exactly one observation for each treatment in every pair", "The differences are normally distributed; pairs are independent of each other"),
  crd = c("A numeric response and one treatment factor with 2 or more levels", "Units assigned to treatments completely at random; at least 2 observations per level", "Errors independent, normal, with equal variance in every group (checked on the residuals)"),
  random = c("A numeric response and one factor whose levels are a random sample from a large population", "Errors and level effects independent and normal", "Conclusions are about the whole population of levels"),
  rcbd = c("A numeric response, a treatment factor and a block factor", "Every treatment appears in every block (complete blocks), usually once", "No treatment × block interaction; errors independent, normal, equal variance"),
  lsd = c("A numeric response with row, column and treatment variables", "p treatments in a p × p square: one observation per row–column cell and every treatment once in each row and column", "No interactions; errors independent, normal, equal variance"),
  glm = c("A numeric response and one or more factors", "Effects add together (no interactions in this model); the design may be unbalanced", "Errors independent, normal, equal variance"),
  nonpar = c("A numeric (or ordinal) response", "Independent observations; groups of similar shape", "No normality assumption"),
  desc = c("Any numeric response"), diag = c("A fitted model; at least 4 observations"))

ps_alt_choices <- c("Two-sided (≠)" = "two.sided", "Greater than (>)" = "greater", "Less than (<)" = "less")

problems_ui <- function(id) {
  ns <- NS(id)
  cond <- function(kinds, ...) conditionalPanel(sprintf("[%s].indexOf(input.kind) >= 0", paste0("'", kinds, "'", collapse = ",")), ns = ns, ...)
  tabsetPanel(id = ns("views"), type = "hidden",
    # ---------- Problem list ----------
    tabPanelBody("list",
      page_head("Problem Sets", "Exercises from the course problem sheets with their complete data. Pick one to see the recommended analysis and a worked Answer Model, or write your own.",
                btn(ns("new_custom"), "Create custom problem", "plus", "bp", "bsm")),
      uiOutput(ns("resume")),
      kcard(div(class = "hrow", style = "align-items:flex-end",
                div(seg_input(ns("cat"), c("Chapter 2 problems" = "ch2", "Chapter 3 problems" = "ch3", "Custom problem" = "custom"), "ch2")),
                div(class = "ml ps-search", textInput(ns("q"), NULL, placeholder = "Search by number, topic or method"))),
            uiOutput(ns("cat_blurb"))),
      uiOutput(ns("cards"))),
    # ---------- One problem ----------
    tabPanelBody("detail",
      uiOutput(ns("detail_head")),
      uiOutput(ns("flow")),
      kcard(id = ns("sec1"), uiOutput(ns("sec_problem"))),
      kcard(id = ns("sec2"),
            div(class = "hrow", div(class = "ps-stepn", "2"), h2(class = "ct", "Data"),
                div(class = "ml hrow", seg_input(ns("dview"), c("As printed" = "wide", "One row per observation" = "long"), "wide"),
                    downloadButton(ns("dl_data"), label = tagList(icon("download", 14), "CSV"), icon = NULL, class = "btn-k bs bsm"),
                    btn(ns("edit_this"), "Edit this problem", "edit", "bs", "bsm"))),
            uiOutput(ns("data_tbl"))),
      kcard(id = ns("sec3"),
            div(class = "hrow", div(class = "ps-stepn", "3"), h2(class = "ct", "Recommended analysis")),
            uiOutput(ns("sec_rec")),
            div(class = "divider", style = "margin:18px 0 14px"),
            h3(class = "ct", "Analysis to run"),
            p(class = "small", style = "margin:2px 0 12px", "The system's recommendation is selected. You can override it; analyses that do not fit this data are listed separately and explain what would need to change."),
            div(class = "grid g2", style = "gap:14px 20px",
                div(selectInput(ns("kind"), "Analysis", choices = NULL, width = "100%")),
                div(selectInput(ns("transform"), "Transform the response", c("None" = "none", "Natural log, ln(y)" = "log", "Square root, √y" = "sqrt"), width = "100%"))),
            uiOutput(ns("kind_note")),
            div(class = "grid g4 ps-settings", style = "gap:14px 16px;margin-top:14px",
                selectInput(ns("alpha"), "Significance level α", c("0.01" = "0.01", "0.05" = "0.05", "0.10" = "0.1"), "0.05"),
                selectInput(ns("conf"), "Confidence level", c("0.90" = "0.9", "0.95" = "0.95", "0.99" = "0.99"), "0.95"),
                cond(c("one_sample", "variance1", "two_sample", "paired", "nonpar"), selectInput(ns("alt"), "Alternative hypothesis H1", ps_alt_choices)),
                cond(c("one_sample", "nonpar"), numericInput(ns("mu0"), "Hypothesised mean μ0", NA)),
                cond(c("one_sample"), numericInput(ns("sigma"), "Known σ (leave empty if unknown)", NA, min = 0)),
                cond(c("variance1"), numericInput(ns("var0"), "Hypothesised variance σ0²", NA, min = 0)),
                cond(c("two_sample"), numericInput(ns("sigma1"), "Known σ1 (empty if unknown)", NA, min = 0)),
                cond(c("two_sample"), numericInput(ns("sigma2"), "Known σ2 (empty if unknown)", NA, min = 0)),
                cond(c("two_sample"), selectInput(ns("var_mode"), "Variances", c("Decide with the F test" = "auto", "Assume equal (pooled)" = "equal", "Assume unequal (Welch)" = "unequal"))),
                cond(c("crd", "rcbd", "lsd"), selectInput(ns("posthoc"), "Mean comparisons", c("Tukey HSD" = "tukey", "Fisher LSD" = "lsd")))),
            div(class = "hrow", style = "margin-top:18px",
                btn(ns("use_rec"), "Use the recommended analysis", "wand", "bs", "bsm"),
                div(class = "ml"), btn(ns("run"), "Run analysis", "play", "bp"))),
      uiOutput(ns("answer"))),
    # ---------- Custom problem editor ----------
    tabPanelBody("edit",
      page_head("Custom problem", "Write or paste a problem, bring in its data, say what each column is, and the system recommends the analysis.",
                btn(ns("edit_back"), "Back to problems", "arrowl", "bs", "bsm")),
      div(class = "split wide",
          div(class = "col",
              kcard(div(class = "hrow", div(class = "ps-stepn", "1"), h2(class = "ct", "Problem statement")),
                    div(style = "margin-top:12px", textInput(ns("c_title"), "Title", placeholder = "For example: Fertilizer trial on three farms")),
                    div(style = "margin-top:12px", textAreaInput(ns("c_statement"), "Statement and questions", rows = 7, width = "100%",
                                                                 placeholder = "Paste the full problem. Mentioning α (for example α = 0.01), μ0, or that levels were selected at random helps the recommendation."))),
              kcard(div(class = "hrow", div(class = "ps-stepn", "2"), h2(class = "ct", "Dataset")),
                    div(style = "margin-top:12px", seg_input(ns("c_src"), c("Paste" = "paste", "Upload a file" = "upload", "Class example" = "example"), "paste")),
                    conditionalPanel("input.c_src == 'paste'", ns = ns, div(style = "margin-top:12px",
                      textAreaInput(ns("c_paste"), NULL, rows = 6, width = "100%", placeholder = "Paste from Excel or type values separated by tabs or commas.\nFirst row = column names, for example:\nFertilizer\tBlock\tYield\nA\t1\t50\nB\t1\t48"),
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
                    p(class = "small", style = "margin:6px 0 12px", "Leave a role empty if the data do not have it. The recommendation is based on the roles you set."),
                    div(class = "col", style = "gap:12px",
                        selectInput(ns("c_response"), "Response (numeric)", NULL), selectInput(ns("c_treatment"), "Treatment or group", NULL),
                        selectInput(ns("c_block"), "Block or pair", NULL), selectInput(ns("c_row"), "Row (Latin square)", NULL), selectInput(ns("c_column"), "Column (Latin square)", NULL),
                        checkboxInput(ns("c_random"), "The treatment levels were selected at random from a larger population", FALSE),
                        checkboxInput(ns("c_variance"), "The question is about a variance, not a mean (one sample)", FALSE))),
              uiOutput(ns("c_check")),
              btn(ns("c_go"), "Recommend and continue", "arrowr", "bp", "blg"))))
  )
}

problems_server <- function(id, store, show_crumb) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    sel <- reactiveVal(NULL)
    custom <- reactiveVal(NULL)
    memo <- reactiveValues()
    cdata <- reactiveVal(NULL)
    crev <- reactiveVal(0)
    # Inputs update asynchronously. After opening a problem, wait until the browser
    # confirms the new values before saving choices, so one problem's settings never
    # leak into another's.
    ready <- reactiveVal(TRUE); token <- reactiveVal(0)
    observeEvent(input$ready, if (isTRUE(as.numeric(input$ready) == token())) ready(TRUE))

    meta <- reactive({ i <- sel(); req(i); if (identical(i, "custom")) custom() else PROBLEMS[[i]] })
    view <- function(v, crumb = NULL) {
      updateTabsetPanel(session, "views", selected = v)
      show_crumb(crumb %||% c("Design of experiments", "Problem Sets"))
      session$sendCustomMessage("scroll_top", list())
    }
    prob_label <- function(m) if (identical(m$id, "custom")) (if (nzchar(m$title %||% "")) m$title else "Custom problem") else sprintf("Problem %s", m$id)

    # ---------- List ----------
    output$cat_blurb <- renderUI({
      k <- input$cat %||% "ch2"
      if (k == "custom") return(p(class = "small", style = "margin:12px 0 0", "Write or paste your own problem, enter or upload the data, and let the system recommend the analysis. You can edit everything before running it."))
      ch <- PS_CHAPTERS[[k]]
      div(style = "margin-top:12px", h3(class = "ct", ch$title), p(class = "small", style = "margin:4px 0 0", ch$blurb))
    })
    output$cards <- renderUI({
      k <- input$cat %||% "ch2"; cur <- sel()
      if (k == "custom") {
        cm <- custom()
        return(div(class = "grid g3",
          tags$a(class = "kcard acard ps-card", href = "#", onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'}); return false;", ns("new_custom")),
                 div(class = "hrow", ico_box("plus", "ig")), div(class = "at", "Create a new problem"),
                 div(class = "ad", "Statement, dataset (paste, upload or a class example), column roles, then the recommended analysis.")),
          if (!is.null(cm)) tags$a(class = paste("kcard acard ps-card", if (identical(cur, "custom")) "on"), href = "#",
                                   onclick = sprintf("Shiny.setInputValue('%s', 'custom', {priority: 'event'}); return false;", ns("pick")),
                                   div(class = "hrow", span(class = "ps-num", "Custom"), if (identical(cur, "custom")) div(class = "ml", pill("Selected", "pass", "check"))),
                                   div(class = "at", prob_label(cm)), div(class = "ad", sprintf("%d rows · %d columns", nrow(cm$data), ncol(cm$data))),
                                   div(class = "meta", span(class = "chip", "Open"), span(class = "chip", "Edit"))) ))
      }
      ps <- ps_by_chapter(k)
      q <- tolower(trimws(input$q %||% ""))
      if (nzchar(q)) ps <- Filter(function(p) grepl(q, tolower(paste(p$id, p$title, paste(p$statement, collapse = " "), paste(p$tags, collapse = " "), paste(names(p$data), collapse = " "))), fixed = TRUE), ps)
      if (!length(ps)) return(kcard(div(class = "empty", ico_box("search", "io", 24), h2(class = "ct", "No problems match"), p(class = "sub", "Try another word, or clear the search."))))
      div(class = "grid g3", lapply(ps, function(p) {
        st <- ps_structure(p$data, p$roles); rk <- ps_recommend(st, p)$kind
        tags$a(class = paste("kcard acard ps-card", if (identical(cur, p$id)) "on"), href = "#",
               onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", ns("pick"), p$id),
               div(class = "hrow", span(class = "ps-num", paste("Problem", p$id)), if (identical(cur, p$id)) div(class = "ml", pill("Selected", "pass", "check"))),
               div(class = "at", p$title),
               div(class = "ad", sprintf("%d observations · %s", nrow(p$data), paste(names(p$data)[names(p$data) != "Replicate" & names(p$data) != "Observation"], collapse = ", "))),
               div(class = "meta", span(class = "chip", icon("wand", 12), PS_ANALYSES[[rk]]$short), lapply(head(p$tags, 2), function(t) span(class = "chip", t))))
      }))
    })
    output$resume <- renderUI({
      i <- sel(); if (is.null(i)) return(NULL)
      m <- if (identical(i, "custom")) custom() else PROBLEMS[[i]]; if (is.null(m)) return(NULL)
      has_res <- !is.null(memo[[i]]$res)
      note(kind = "g", ic = "check", div(class = "hrow", style = "gap:12px",
        span(sprintf("You are working on %s: %s.%s", prob_label(m), if (identical(i, "custom")) "your own problem" else m$title, if (has_res) " Its Answer Model is ready." else "")),
        div(class = "ml"), tags$button(class = "btn-k bp bsm", onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("pick"), i), icon("arrowr", 14), "Continue")))
    })

    # ---------- Opening a problem ----------
    num_or_na <- function(x) if (is.null(x)) NA else x
    apply_settings <- function(s, kind, choices) {
      updateSelectInput(session, "kind", choices = choices, selected = kind)
      updateSelectInput(session, "alpha", selected = as.character(s$alpha %||% 0.05))
      updateSelectInput(session, "conf", selected = as.character(s$conf %||% 0.95))
      updateSelectInput(session, "alt", selected = s$alt %||% "two.sided")
      updateNumericInput(session, "mu0", value = num_or_na(s$mu0))
      updateNumericInput(session, "sigma", value = if (is.null(s[["sigma2"]])) num_or_na(s[["sigma"]]) else NA)
      updateNumericInput(session, "sigma1", value = if (!is.null(s[["sigma2"]])) num_or_na(s[["sigma"]]) else NA)
      updateNumericInput(session, "sigma2", value = num_or_na(s[["sigma2"]]))
      updateNumericInput(session, "var0", value = num_or_na(s$var0))
      updateSelectInput(session, "var_mode", selected = s$var_mode %||% "auto")
      updateSelectInput(session, "posthoc", selected = s$posthoc %||% "tukey")
      updateSelectInput(session, "transform", selected = s$transform %||% "none")
    }
    kind_choices <- function(st, rec_kind, s) {
      ok <- ps_compatible(st, modifyList(s, list(mu0 = s$mu0 %||% 0, var0 = s$var0 %||% 1)))
      keys <- names(PS_ANALYSES); lab <- vapply(PS_ANALYSES, `[[`, "", "label")
      other <- setdiff(keys[ok], rec_kind); bad <- keys[!ok]
      out <- list()
      if (!is.na(rec_kind)) out[["Recommended for this data"]] <- setNames(rec_kind, paste0(lab[[rec_kind]], "  ★"))
      if (length(other)) out[["Other compatible analyses"]] <- setNames(other, lab[other])
      if (length(bad)) out[["Need a different data structure"]] <- setNames(bad, paste0(lab[bad], " (not compatible)"))
      out
    }
    open_problem <- function(i) {
      m <- if (identical(i, "custom")) custom() else PROBLEMS[[i]]
      if (is.null(m)) return()
      sel(i)
      saved <- isolate(memo[[i]])
      s <- saved$settings %||% m$settings
      st <- ps_structure(m$data, m$roles, s$transform %||% "none")
      rk <- ps_recommend(st, m, m$statement)$kind
      ready(FALSE); token(token() + 1)
      apply_settings(s, saved$kind %||% rk, kind_choices(st, rk, s))
      session$sendCustomMessage("ps_ready", list(id = ns("ready"), token = token()))
      view("detail", c("Design of experiments", "Problem Sets", prob_label(m)))
    }
    observeEvent(input$pick, open_problem(input$pick))
    observeEvent(input$back, view("list"))

    # ---------- Current state ----------
    settings <- reactive({
      n <- function(x) if (is.null(x) || is.na(x)) NULL else x
      two <- identical(input$kind, "two_sample")
      list(alpha = as.numeric(input$alpha %||% 0.05), conf = as.numeric(input$conf %||% 0.95), alt = input$alt %||% "two.sided",
           mu0 = n(input$mu0), sigma = if (two) n(input$sigma1) else n(input$sigma), sigma2 = if (two) n(input$sigma2) else NULL,
           var0 = n(input$var0), var_mode = input$var_mode %||% "auto", posthoc = input$posthoc %||% "tukey", transform = input$transform %||% "none")
    })
    st <- reactive(ps_structure(meta()$data, meta()$roles, input$transform %||% "none"))
    rec <- reactive(ps_recommend(st(), meta(), meta()$statement))
    valid <- reactive({ req(input$kind %in% names(PS_ANALYSES)); ps_validate(input$kind, st(), settings()) })

    # Remember the choice for each problem so coming back restores it.
    observe({
      req(ready()); i <- sel(); req(i, input$kind %in% names(PS_ANALYSES))
      cur <- isolate(memo[[i]]) %||% list()
      cur$kind <- input$kind; cur$settings <- settings()
      memo[[i]] <- cur
    })
    # Keep the selector labels in step with the data (a transform can change compatibility).
    observeEvent(st(), {
      req(isolate(ready()))
      ch <- kind_choices(st(), rec()$kind, isolate(settings()))
      updateSelectInput(session, "kind", choices = ch, selected = isolate(input$kind))
    }, ignoreInit = TRUE)
    observeEvent(input$use_rec, { k <- rec()$kind; if (!is.na(k)) updateSelectInput(session, "kind", selected = k) })

    # ---------- Detail sections ----------
    output$detail_head <- renderUI({
      m <- meta()
      page_head(prob_label(m), if (identical(m$id, "custom")) "Your own problem" else m$title,
                btn(ns("back"), "All problems", "arrowl", "bs", "bsm"),
                if (identical(m$id, "custom")) btn(ns("edit_custom"), "Edit problem", "edit", "bs", "bsm"),
                pin_button(ns))
    })
    output$flow <- renderUI({
      has_res <- !is.null(memo[[sel()]]$res)
      step <- function(n, lab, target, done) tags$a(class = paste("ps-flowstep", if (done) "done"), href = "#",
        onclick = sprintf("document.getElementById('%s').scrollIntoView({behavior:'smooth', block:'start'}); return false;", target),
        span(class = "dot", if (done) icon("check", 13, 3) else n), span(lab))
      div(class = "stepper ps-flow", step(1, "Problem", ns("sec1"), TRUE), div(class = "sline done"), step(2, "Data", ns("sec2"), TRUE), div(class = "sline done"),
          step(3, "Recommended analysis", ns("sec3"), TRUE), div(class = paste("sline", if (has_res) "done")), step(4, "Run analysis", ns("answer"), has_res))
    })
    output$sec_problem <- renderUI({
      m <- meta(); s <- st(); r <- rec()
      req_list <- PS_REQUIREMENTS[[r$kind %||% "desc"]] %||% character(0)
      tagList(
        div(class = "hrow", div(class = "ps-stepn", "1"), h2(class = "ct", "Problem"), div(class = "ml", span(class = "small", m$source %||% ""))),
        div(class = "ps-statement", lapply(m$statement, function(x) p(x)),
            if (length(m$parts)) tags$ol(class = "ps-parts", lapply(m$parts, function(x) tags$li(sub("^\\([a-z]\\)\\s*", "", x))), type = if (all(grepl("^\\([a-z]\\)", m$parts))) "a" else "1")),
        if (!is.null(m$note)) note(kind = "w", ic = "alert", m$note),
        div(class = "grid g2", style = "margin-top:16px",
            div(h3(class = "ct", "Factors and variables"),
                if (length(s$errors)) note(kind = "e", ic = "x", paste(s$errors, collapse = " ")) else div(class = "tscroll", style = "margin-top:8px", html_table(ps_vars_table(s, m), numeric_right = FALSE))),
            div(h3(class = "ct", "Design type"),
                div(style = "margin-top:8px", pill(if (is.na(r$kind)) "Not determined" else ps_design_name(r$kind, s), "inf", "grid")),
                h3(class = "ct", style = "margin-top:14px", "Assumptions and data requirements"),
                tags$ul(class = "ps-ul", lapply(req_list, tags$li)))),
        if (!is.null(m$covers)) div(class = "small", style = "margin-top:12px", tags$b("What the Answer Model covers: "), m$covers))
    })
    output$data_tbl <- renderUI({
      m <- meta()
      d <- if (identical(input$dview, "long")) m$data else m$display
      tagList(div(class = "tscroll ps-data", style = "margin-top:12px", html_table(d)),
              p(class = "small", style = "margin:8px 0 0", sprintf("%d observations. %s", nrow(m$data),
                if (identical(input$dview, "long")) "This is the format the analysis uses: one row per observation, one column per variable." else "Laid out as in the problem sheet.")))
    })
    output$dl_data <- downloadHandler(filename = function() sprintf("problem_%s.csv", gsub("[^A-Za-z0-9]+", "_", meta()$id)),
                                      content = function(file) utils::write.csv(meta()$data, file, row.names = FALSE))
    output$sec_rec <- renderUI({
      r <- rec(); s <- st()
      if (is.na(r$kind)) return(note(kind = "e", ic = "x", div(tags$b("The data cannot be analysed yet. "), paste(r$reasons, collapse = " "))))
      tagList(
        div(class = "ps-recbox", style = "margin-top:12px",
            div(class = "hrow", ico_box("wand", "ig"), div(style = "flex:1;min-width:0", div(class = "small", "The system recommends"), div(class = "ps-rect", PS_ANALYSES[[r$kind]]$label)),
                pill("System recommendation", "pass", "check"))),
        div(class = "grid g2", style = "margin-top:14px",
            div(h3(class = "ct", "Why this analysis"), tags$ul(class = "ps-ul", lapply(r$reasons, tags$li))),
            div(h3(class = "ct", "What it will compute"), tags$ul(class = "ps-ul", lapply(r$computes, tags$li)))),
        if (length(r$notes)) div(class = "col", style = "gap:8px;margin-top:12px", lapply(r$notes, function(x) note(kind = "w", ic = "alert", x))))
    })
    output$kind_note <- renderUI({
      k <- input$kind; req(k %in% names(PS_ANALYSES))
      r <- rec(); v <- valid()
      manual <- !is.na(r$kind) && k != r$kind
      tagList(
        if (manual) note(kind = "w", ic = "edit", div(tags$b("Manually selected: "), sprintf("you chose %s instead of the recommended %s. The Answer Model will be labelled as your choice.", PS_ANALYSES[[k]]$label, PS_ANALYSES[[r$kind]]$label)))
        else if (!is.na(r$kind)) note(kind = "g", ic = "check", div(tags$b("Using the recommended analysis: "), PS_ANALYSES[[k]]$label)),
        if (length(v$errors)) div(class = "col", style = "gap:8px;margin-top:8px", lapply(v$errors, function(x) note(kind = "e", ic = "x", div(tags$b("Cannot run: "), x))),
                                  if (any(grepl("Map |column", v$errors))) p(class = "small", style = "margin:0", "To change which column plays which role, use Edit this problem in the Data section.")),
        if (length(v$warnings)) div(class = "col", style = "gap:8px;margin-top:8px", lapply(v$warnings, function(x) note(kind = "w", ic = "alert", x))))
    })

    # ---------- Run ----------
    observeEvent(input$run, {
      if (!ready()) { showNotification("Still loading this problem; press Run analysis again.", type = "warning"); return() }
      i <- sel(); req(i); k <- input$kind; req(k %in% names(PS_ANALYSES))
      v <- valid()
      if (length(v$errors)) { showNotification(paste("Cannot run:", v$errors[1]), type = "error", duration = 8); return() }
      res <- tryCatch(ps_run(k, st(), settings(), meta()), error = function(e) e)
      if (inherits(res, "error")) { showNotification(paste("The analysis failed:", conditionMessage(res)), type = "error", duration = 10); return() }
      res$rec_kind <- rec()$kind; res$problem <- prob_label(meta()); res$transform <- st()$transform
      cur <- isolate(memo[[i]]) %||% list(); cur$res <- res; cur$res_sig <- list(k, settings()); memo[[i]] <- cur
      session$sendCustomMessage("scroll_to", list(id = ns("answer")))
    })
    cur_res <- reactive({ i <- sel(); req(i); memo[[i]]$res })

    # ---------- Answer Model ----------
    plot_slot <- function(i, h = 300) div(class = "kcard soft ps-plot", div(class = "small", style = "font-weight:600;margin-bottom:6px", textOutput(ns(paste0("pt", i)), inline = TRUE)),
                                          plotOutput(ns(paste0("pl", i)), height = h))
    for (i in 1:5) local({
      j <- i
      output[[paste0("pl", j)]] <- renderPlot({ r <- cur_res(); req(r, length(r$plots) >= j); r$plots[[j]]$p }, bg = "transparent", res = 96)
      output[[paste0("pt", j)]] <- renderText({ r <- cur_res(); req(r, length(r$plots) >= j); r$plots[[j]]$t })
    })
    tbl <- function(df) if (!is.null(df) && nrow(df)) div(class = "tscroll", style = "margin-top:10px", html_table(df))
    output$answer <- renderUI({
      i <- sel(); req(i)
      r <- memo[[i]]$res
      if (is.null(r)) return(kcard(div(class = "hrow", div(class = "ps-stepn muted", "4"), h2(class = "ct", "Run analysis")),
                                   p(class = "sub", style = "margin-top:8px", "Press Run analysis to produce the Answer Model: the full worked solution, step by step.")))
      stale <- !identical(memo[[i]]$res_sig, list(input$kind, settings()))
      manual <- !is.na(r$rec_kind) && r$kind != r$rec_kind
      pk <- vapply(r$plots, function(p) if (grepl("QQ|Residuals versus", p$t)) "asm" else if (grepl("intervals", p$t)) "cmp" else "data", "")
      slots <- function(kind) { idx <- which(pk == kind); if (length(idx)) div(class = "grid g2", style = "margin-top:12px", lapply(idx, plot_slot)) }
      step <- function(n, title, ...) div(class = "am-step", div(class = "am-n", n), div(class = "am-b", h3(class = "ct", title), ...))
      ptxt <- function(x) lapply(x, function(z) p(class = "ltext", z))
      calc_ui <- lapply(r$calc, function(c) div(class = "am-calc", div(class = "am-ct", c$t), div(class = "am-formula", lapply(c$x, div)), tbl(c$df)))
      extra_ui <- lapply(r$extra %||% list(), function(e) div(class = "am-calc", div(class = "am-ct", e$t), if (!is.null(e$x)) div(class = "am-formula", lapply(e$x, div)), tbl(e$df)))
      kcard(class = "am",
        div(class = "hrow", div(class = "ps-stepn", "4"), h2(class = "ct", "Answer Model"),
            div(class = "ml hrow", if (manual) pill(sprintf("Your choice: %s", PS_ANALYSES[[r$kind]]$short), "warn", "edit") else pill(sprintf("Recommended: %s", PS_ANALYSES[[r$kind]]$short), "pass", "check"))),
        if (manual) note(kind = "w", ic = "info", sprintf("This answer uses the analysis you selected (%s). The system recommended %s.", r$label, PS_ANALYSES[[r$rec_kind]]$label)),
        if (stale) note(kind = "i", ic = "info", "You have changed the analysis or its settings since this answer was produced. Press Run analysis to update it."),
        if (length(r$warnings)) div(class = "col", style = "gap:8px;margin-top:8px", lapply(r$warnings, function(x) note(kind = "w", ic = "alert", x))),
        div(class = "am-steps",
            step(1, "Identify the experimental design", ptxt(c(r$design, r$why))),
            step(2, "Define the treatments, blocks and response", tbl(r$vars), slots("data")),
            step(3, "State the hypotheses", div(class = "am-formula", lapply(r$hyp, div))),
            step(4, "Specify the model", div(class = "formula am-model", r$model$eq), ptxt(r$model$notes)),
            step(5, "Work through the calculations", calc_ui, extra_ui),
            step(6, r$table$title, tbl(r$table$df),
                 if (!is.null(r$stats)) p(class = "small", style = "margin:8px 0 0", "df = degrees of freedom. F0 (or t0, Z0, χ0²) is the observed test statistic; the P-value is the probability of a value at least this extreme if H0 were true.")),
            if (!is.null(r$stats)) step(7, "Test statistics, degrees of freedom and P-values", tbl(r$stats),
                                        p(class = "small", style = "margin:8px 0 0", "Critical = the value the statistic must pass for the test to reject H0 at the chosen α.")),
            step(if (is.null(r$stats)) 7 else 8, "Decision about the null hypothesis", div(class = "am-decision", lapply(r$decision, div))),
            step(if (is.null(r$stats)) 8 else 9, "Treatment comparisons", ptxt(r$comp$text), tbl(r$comp$table),
                 if (!is.null(r$comp$groups)) div(style = "margin-top:10px", h3(class = "ct", style = "font-size:13.5px", "Grouping (levels sharing a letter do not differ)"), tbl(r$comp$groups)),
                 slots("cmp")),
            step(if (is.null(r$stats)) 9 else 10, "Check the assumptions", tbl(r$assump$table), ptxt(r$assump$text), slots("asm")),
            step(if (is.null(r$stats)) 10 else 11, "Final statistical conclusion", div(class = "am-concl", r$conclusion)),
            step(if (is.null(r$stats)) 11 else 12, "Plain-language interpretation", p(class = "ltext", r$plain))))
    })

    # ---------- Pin to report ----------
    pin_observers(input, session, store,
      items = reactive({
        r <- cur_res(); req(r)
        it <- list(
          answer = list(label = "Answer Model (full worked solution as text)", default = TRUE, build = function() mk_pin("text", sprintf("%s: Answer Model", r$problem), "file", text = ps_answer_text(r))),
          table = list(label = r$table$title, default = TRUE, build = function() mk_pin("table", r$table$title, "table", df = r$table$df)))
        if (!is.null(r$comp$table)) it$comp <- list(label = "Treatment comparisons", default = TRUE, build = function() mk_pin("table", "Treatment comparisons", "columns", df = r$comp$table))
        if (!is.null(r$assump$table)) it$assump <- list(label = "Assumption checks", default = TRUE, build = function() mk_pin("table", "Assumption checks", "scale", df = r$assump$table))
        for (j in seq_along(r$plots)) local({ pp <- r$plots[[j]]; it[[paste0("plot", j)]] <<- list(label = pp$t, default = j == 1, build = function() mk_pin("plot", pp$t, "bar", plot = pp$p, h = 3.4)) })
        it
      }),
      src = reactive({
        r <- cur_res(); m <- meta()
        list(key = paste0("problem_", gsub("[^A-Za-z0-9]", "_", m$id)), label = sprintf("%s · %s", r$problem, PS_ANALYSES[[r$kind]]$short),
             title = if (identical(m$id, "custom")) prob_label(m) else sprintf("Problem %s: %s", m$id, m$title),
             subtitle = sprintf("%s · n = %d · α = %s", r$label, nrow(st()$d), format(r$settings$alpha)),
             dataset = prob_label(m), notes = c(m$source %||% "", m$note %||% character(0)), code = "")
      }))

    # ---------- Custom problem editor ----------
    role_ids <- c(response = "c_response", treatment = "c_treatment", block = "c_block", row = "c_row", column = "c_column")
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
        updateCheckboxInput(session, "c_random", value = isTRUE(seed$random))
        updateCheckboxInput(session, "c_variance", value = identical(seed$target, "variance"))
        load_data(seed$data, seed$roles)
        session$userData$ps_seed_settings <- seed$settings
      }
      view("edit", c("Design of experiments", "Problem Sets", "Custom problem"))
    }
    observeEvent(input$new_custom, {
      cm <- custom()
      if (!is.null(cm)) open_editor() else { session$userData$ps_seed_settings <- NULL; open_editor() }
    })
    observeEvent(input$edit_custom, open_editor())
    observeEvent(input$edit_this, {
      m <- meta()
      if (identical(m$id, "custom")) return(open_editor())
      open_editor(list(title = sprintf("Problem %s (edited)", m$id), statement = paste(c(m$statement, m$parts), collapse = "\n"),
                       data = m$data, roles = m$roles, random = m$random, target = m$target, settings = m$settings))
      showNotification(sprintf("Problem %s copied into the editor. Change anything, then press Recommend and continue.", m$id), type = "message")
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
      if (is.null(df)) df <- data.frame(Treatment = rep(NA_character_, 6), Response = rep(NA_character_, 6), check.names = FALSE)
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
          p(class = "small", style = "margin:6px 0 0", "Columns you do not stack (for example a Pair or Block ID) are kept and repeated for every stacked value."))
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
      txt <- input$c_statement %||% ""
      seed <- session$userData$ps_seed_settings %||% list()
      s <- modifyList(list(alpha = 0.05, conf = 0.95, alt = "two.sided"), seed)
      det <- ps_detect_settings(txt)
      for (k in names(det)) if (is.null(seed[[k]])) s[[k]] <- det[[k]]
      paras <- strsplit(txt, "\n\\s*\n")[[1]]; paras <- trimws(paras[nzchar(trimws(paras))])
      list(id = "custom", chapter = 0, num = "Custom", title = trimws(input$c_title %||% ""), statement = if (length(paras)) paras else "No statement was entered.",
           parts = character(0), data = df, display = df, roles = croles(), labels = list(), settings = s,
           target = if (isTRUE(input$c_variance)) "variance" else "mean", random = isTRUE(input$c_random), tags = "custom", source = "Your own problem")
    })
    output$c_check <- renderUI({
      df <- cdata()
      if (is.null(df)) return(note(kind = "i", ic = "info", "Bring in a dataset to see the data checks."))
      m <- cmeta(); s <- ps_structure(m$data, m$roles); r <- ps_recommend(s, m, m$statement)
      kcard(h3(class = "ct", "Data checks"),
            if (length(s$errors)) div(class = "col", style = "gap:8px;margin-top:10px", lapply(s$errors, function(x) note(kind = "e", ic = "x", x)))
            else tagList(
                         if (!nzchar(m$roles$treatment) && sum(vapply(df, function(x) { v <- x[!is.na(x)]; length(v) > 0 && all(!is.na(suppressWarnings(as.numeric(v)))) }, TRUE)) >= 2)
                           note(kind = "i", ic = "info", "Several numeric columns and no treatment: if each column is a group, press Stack (under the data) so the groups can be compared."),
                         tags$ul(class = "ps-ul", lapply(ps_layout_text(s, m), tags$li)),
                         div(class = "hrow", style = "margin-top:8px", span(class = "small", "Recommended:"), pill(PS_ANALYSES[[r$kind]]$label, "pass", "wand")),
                         if (length(s$warnings)) div(class = "col", style = "gap:8px;margin-top:8px", lapply(s$warnings, function(x) note(kind = "w", ic = "alert", x)))))
    })
    observeEvent(input$c_go, {
      df <- cdata()
      if (is.null(df) || !nrow(df)) { showNotification("Add a dataset first: paste it, upload a file or load a class example.", type = "error"); return() }
      m <- cmeta(); s <- ps_structure(m$data, m$roles)
      if (length(s$errors)) { showNotification(s$errors[1], type = "error", duration = 8); return() }
      custom(m); memo[["custom"]] <- NULL
      open_problem("custom")
    })

    # ---------- For Ask Professor Riaz ----------
    list(ai = function(full) {
      i <- isolate(sel())
      if (is.null(i)) return(list(label = "the Problem Sets list", kind = "general", text = if (full) "The student is browsing the Problem Sets list (Chapter 2 comparative experiments and Chapter 3 single-factor ANOVA problems)."))
      m <- isolate(meta()); r <- isolate(memo[[i]]$res); rk <- isolate(rec()$kind)
      list(label = prob_label(m), kind = "general",
           text = if (full) paste0("Problem Sets page. ", prob_label(m), ". Statement: ", paste(c(m$statement, m$parts), collapse = " "),
                                   " Data (long format, first rows): ", paste(utils::capture.output(print(utils::head(m$data, 12), row.names = FALSE)), collapse = "\n"),
                                   " Recommended analysis: ", PS_ANALYSES[[rk]]$label %||% "none", ".",
                                   if (!is.null(r)) paste0(" Answer Model produced with ", r$label, ": ", ps_answer_text(r)) else " The analysis has not been run yet."))
    })
  })
}

# Name of the design the structure implies, for the Problem section.
ps_design_name <- function(kind, st) switch(kind,
  one_sample = "Single sample", variance1 = "Single sample (variance)", two_sample = "Two independent samples (CRD with 2 levels)",
  paired = "Paired comparison (RCBD with 2 treatments)", crd = "Completely randomized design (CRD)", random = "Single factor, random effects",
  rcbd = "Randomized complete block design (RCBD)", lsd = sprintf("Latin square (%d × %d)", st$psize %||% 0L, st$psize %||% 0L), glm = "Unbalanced or incomplete layout", "—")

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
  row <- pick(named, "^rows?$|^row[ _.]"); col <- pick(setdiff(named, row), "^col(umn)?s?$|^col(umn)?[ _.]")
  blk <- pick(setdiff(named, c(row, col)), "block|pair|inspector|subject|farm|batch|day|girder|rep$")
  trt <- pick(setdiff(named, c(row, col, blk)), ROLE_TREAT$pattern)
  if (!nzchar(trt)) { rest <- setdiff(intersect(cat, pool), c(row, col, blk)); if (length(rest)) trt <- rest[1] }
  if (nzchar(blk) && !nzchar(trt)) { trt <- blk; blk <- "" }
  list(response = resp, treatment = trt, block = blk, row = row, column = col)
}

# Settings that can be read from a typed problem statement.
ps_detect_settings <- function(txt) {
  out <- list(); t <- gsub("\\s+", " ", txt)
  num_after <- function(pat) { m <- regmatches(t, regexpr(pat, t, perl = TRUE, ignore.case = TRUE)); if (length(m)) suppressWarnings(as.numeric(sub(".*?(-?[0-9]*\\.?[0-9]+)\\s*$", "\\1", m, perl = TRUE))) else NA }
  a <- num_after("(α|alpha)\\s*=\\s*0?\\.[0-9]+"); if (!is.na(a) && a > 0 && a < 0.5) { out$alpha <- a; out$conf <- 1 - a }
  mu <- num_after("(μ0?|mu0?)\\s*=\\s*-?[0-9]*\\.?[0-9]+"); if (!is.na(mu)) out$mu0 <- mu
  sg <- num_after("(σ|sigma)\\s*=\\s*[0-9]*\\.?[0-9]+"); if (!is.na(sg) && sg > 0) out$sigma <- sg
  out
}

# The whole Answer Model as plain text, for the report and the assistant.
ps_answer_text <- function(r) {
  df_txt <- function(df) if (is.null(df) || !nrow(df)) "" else paste(apply(df, 1, function(x) paste(paste(names(df), x, sep = ": "), collapse = "; ")), collapse = "\n")
  paste(c(
    sprintf("Analysis: %s%s", r$label, if (!is.na(r$rec_kind %||% NA) && r$kind != r$rec_kind) sprintf(" (chosen by the student; the system recommended %s)", PS_ANALYSES[[r$rec_kind]]$label) else " (system recommendation)"),
    paste("1. Design:", r$design, r$why),
    paste("2. Variables:", df_txt(r$vars)),
    paste("3. Hypotheses:", paste(r$hyp, collapse = "; ")),
    paste("4. Model:", r$model$eq, paste(r$model$notes, collapse = " ")),
    paste("5. Calculations:", paste(vapply(r$calc, function(c) paste0(c$t, ": ", paste(c$x, collapse = "; ")), ""), collapse = " | ")),
    paste0("6. ", r$table$title, ":\n", df_txt(r$table$df)),
    if (!is.null(r$stats)) paste0("7. Test statistics:\n", df_txt(r$stats)),
    paste("8. Decision:", paste(r$decision, collapse = " ")),
    paste("9. Comparisons:", paste(r$comp$text, collapse = " "), if (!is.null(r$comp$table)) paste0("\n", df_txt(r$comp$table)) else ""),
    paste("10. Assumptions:", paste(r$assump$text, collapse = " ")),
    paste("11. Conclusion:", r$conclusion),
    paste("12. Interpretation:", r$plain)), collapse = "\n\n")
}
