# Problem Sets: the course exercises by chapter, with categories and filters (problem type,
# test type, post-hoc), a custom problem editor, and the hypothesis-testing steps taught in
# the course:
#   One and two samples: 1 Problem & data  2 Hypotheses  3 Significance level
#                        4 Information available  5 Test  6 Decision
#   k samples (Chapter 3), blocked designs (Chapter 4) and factorial designs (Chapter 5): the
#                        parametric process runs to
#                        the end first: the ANOVA, 7 Post-hoc (only if H0 is rejected) and
#                        8 Assumption checks. If an assumption fails, the process starts again
#                        for the non-parametric test (9-14: hypotheses about medians, α,
#                        information, the rank test, its decision and post-hoc).

PS_STEP_NAMES <- c("Problem & data", "Hypotheses", "Significance level", "Information available", "Test", "Decision", "Post-hoc",
                   "Assumption checks", "Non-parametric hypotheses", "Significance level (non-parametric)", "Information available (non-parametric)",
                   "Rank test", "Rank test decision", "Rank test post-hoc")
PS_STEP_SHORT <- c("Problem", "Hypotheses", "α level", "Information", "Test", "Decision", "Post-hoc", "Assumptions",
                   "Hypotheses", "α level", "Information", "Test", "Decision", "Post-hoc")
PS_K_LAST <- 14L
# The non-parametric restart repeats steps 2 to 7 of the process.
ps_step_num <- function(i) c(1:8, 2:7)[i]
# Steps 12 to 14 are named after the design's rank test.
ps_step_name <- function(i, kind = "k") {
  if (i >= 12 && ps_multi(kind)) return(switch(i - 11, ps_np_name(kind), paste(ps_np_short(kind), "decision"), paste(ps_np_short(kind), "post-hoc")))
  PS_STEP_NAMES[i]
}

PS_TYPE_DESC <- c(
  "One sample" = "Compare one mean, median or variance with a target value.",
  "Two sample · independent" = "Two groups made of different units.",
  "Two sample · paired" = "Two measurements on the same units (dependent samples).",
  "CRD" = "Completely randomized design: three or more independent groups on one factor. CRD ANOVA, Welch ANOVA or Kruskal-Wallis.",
  "RCBD" = "Randomized complete block design: every treatment once in each block. Friedman if an assumption fails.",
  "Latin square" = "Two blocking factors (rows and columns), each treatment once in every row and column.",
  "Graeco-Latin square" = "Three blocking or treatment factors laid over a square.",
  "Two-factor factorial (CRD)" = "Two crossed factors in a completely randomized layout (CRD ANOVA): the main effect of each and their interaction. Kruskal-Wallis for each effect if an assumption fails.",
  "Three-factor factorial (CRD)" = "Three crossed factors in a completely randomized layout (CRD ANOVA): three main effects and the AB, AC, BC and ABC interactions.",
  "Four-factor factorial (CRD)" = "Four crossed factors (usually a 2⁴ design) in a completely randomized layout: four main effects and their interactions. With one observation per combination, the three- and four-factor interactions form the error.",
  "Five-factor factorial (CRD)" = "Five crossed factors (a 2⁵ design) in a completely randomized layout. With one observation per combination, the three-factor and higher interactions form the error.",
  "Factorial in blocks (RCBD)" = "A factorial experiment with each replicate run as a block (RCBD ANOVA). Friedman if an assumption fails.",
  "Factorial in a Latin square (LSD)" = "A factorial experiment with row and column blocking (LSD ANOVA). Quade's test if an assumption fails.",
  "Factorial in a Graeco-Latin square (GLSD)" = "A factorial experiment with row, column and Greek-letter blocking (GLSD ANOVA). Quade's test if an assumption fails.",
  "Parametric" = "The parametric test is the answer: its assumptions hold.",
  "Non-parametric" = "A rank test is the answer: normality or another assumption fails.")
PS_TYPE_ORDER <- c("One sample", "Two sample · independent", "Two sample · paired", "CRD", "RCBD", "Latin square", "Graeco-Latin square",
                   "Two-factor factorial (CRD)", "Three-factor factorial (CRD)", "Four-factor factorial (CRD)", "Five-factor factorial (CRD)", "Factorial in blocks (RCBD)", "Factorial in a Latin square (LSD)", "Factorial in a Graeco-Latin square (GLSD)")
PS_PH_ORDER <- c("Tukey HSD", "Fisher LSD", "Bonferroni", "Scheffé", "Dunnett", "Games-Howell", "Dunn", "Nemenyi", "Wilcoxon", "None", "Not applicable")

ps_alt_choices <- function(kind) {
  sym <- switch(kind, one = "μ", variance = "σ²", two = "μ1 − μ2", paired = "μd", "μ")
  v <- if (kind == "variance") "σ0²" else if (kind == "one") "μ0" else "Δ0"
  stats::setNames(c("two.sided", "greater", "less"), sprintf(c("Two-sided: %s ≠ %s", "Right-tailed: %s > %s", "Left-tailed: %s < %s"), sym, v))
}
ps_value_label <- function(kind) switch(kind, one = "Hypothesised mean μ0", variance = "Hypothesised variance σ0²",
                                        two = "Hypothesised difference Δ0 = μ1 − μ2", paired = "Hypothesised mean difference Δ0", "Value")

# The analysis pages (One sample, Two samples, k samples, CRD, RCBD, Latin square, Graeco-Latin square and
# Factorial) run the same step-by-step process as the Problem Sets on the student's own data. Each page says
# which columns it needs and which layouts it accepts.
PS_MODES <- list(
  one = list(title = "One sample", kinds = c("one", "variance"), roles = "response",
             sub = "Compare the mean, the median or the variance of one sample with a target value, in five steps.",
             need = "One sample needs a Response column only."),
  two = list(title = "Two samples", kinds = c("two", "paired"), roles = c("response", "treatment", "block"),
             labels = list(treatment = "Group (the two samples)", block = "Pair ID (only for paired, dependent samples)"),
             sub = "Compare two independent samples, or two dependent (paired) samples, in five steps.",
             need = "Two samples needs a Response and a Group column with exactly two groups. For paired data also choose the Pair ID, with one observation per group in each pair."),
  k = list(title = "k samples", kinds = "k", roles = c("response", "treatment"), labels = list(treatment = "Group (the samples)"),
           sub = "Compare three or more independent samples: ANOVA or Welch ANOVA first, then the Kruskal-Wallis test if an assumption fails.",
           need = "k samples needs a Response and a Group column with three or more groups, and no Block."),
  crd = list(title = "CRD", kinds = "k", roles = c("response", "treatment"), labels = list(treatment = "Treatment"),
             sub = "Completely randomized design: CRD ANOVA (or Welch ANOVA) first, then the Kruskal-Wallis test if an assumption fails.",
             need = "A CRD needs a Response and a Treatment column with three or more treatments, and no Block."),
  rcbd = list(title = "RCBD", kinds = "rcbd", roles = c("response", "treatment", "block"), labels = list(treatment = "Treatment", block = "Block"),
              sub = "Randomized complete block design: RCBD ANOVA first, then the Friedman test if an assumption fails.",
              need = "An RCBD needs a Response, a Treatment and a Block column, with every treatment in every block."),
  lsd = list(title = "Latin Square", kinds = "latin", roles = c("response", "treatment", "row", "column"),
             labels = list(treatment = "Treatment (the Latin letters)", row = "Row", column = "Column"),
             sub = "Latin square design: LSD ANOVA first, then Quade's test if an assumption fails.",
             need = "A Latin square needs a Response, a Treatment, a Row and a Column column (and no Greek letter)."),
  glsd = list(title = "Graeco-Latin Square", kinds = "glsd", roles = c("response", "treatment", "row", "column", "greek"),
              labels = list(treatment = "Treatment (the Latin letters)", row = "Row", column = "Column", greek = "Greek letter (the second blocking factor)"),
              sub = "Graeco-Latin square design: GLSD ANOVA first, then Quade's test if an assumption fails.",
              need = "A Graeco-Latin square needs a Response, a Treatment, a Row, a Column and a Greek letter column."),
  fact = list(title = "Factorial", kinds = c("fact", "factb", "factl", "factg"),
              roles = c("response", "treatment", "factorB", "factorC", "factorD", "factorE", "block", "row", "column", "greek"),
              labels = list(treatment = "Factor A", factorB = "Factor B", factorC = "Factor C (three or more factors)", factorD = "Factor D (four or five factors)", factorE = "Factor E (five factors)", block = "Block (factorial in blocks, optional)",
                            row = "Row (Latin square, optional)", column = "Column (Latin square, optional)", greek = "Greek letter (Graeco-Latin, optional)"),
              sub = "Factorial designs: every factor and every interaction is tested; CRD, RCBD, LSD or GLSD ANOVA first, then the rank test of the layout if an assumption fails.",
              need = "A factorial design needs a Response and at least Factor A and Factor B, with every combination of levels observed more than once (or in blocks)."))

problems_ui <- function(id, mode = NULL) {
  ns <- NS(id); md <- if (!is.null(mode)) PS_MODES[[mode]]
  show_role <- function(k) is.null(md) || k %in% md$roles
  rlab <- function(k, default) md$labels[[k]] %||% default
  tabsetPanel(id = ns("views"), type = "hidden", selected = if (is.null(md)) "list" else "edit",
    # ---------- Problem list ----------
    tabPanelBody("list", if (is.null(md)) tagList(
      page_head("Problem Sets", "Exercises from the course problem sheets with their complete data, solved with the step-by-step hypothesis-testing process.",
                btn(ns("new_custom"), "Create custom problem", "plus", "bp", "bsm")),
      uiOutput(ns("resume")),
      kcard(div(class = "hrow", style = "align-items:flex-end",
                div(seg_input(ns("cat"), c("Chapter 2" = "ch2", "Chapter 3" = "ch3", "Chapter 4" = "ch4", "Chapter 5" = "ch5", "Chapter 6" = "ch6", "Custom Problem" = "custom"), "ch2")),
                div(class = "ml ps-search", textInput(ns("q"), NULL, placeholder = "Search by number, topic or test"))),
            uiOutput(ns("cat_blurb")),
            conditionalPanel("input.cat != 'custom'", ns = ns, div(class = "ps-filters",
              div(class = "ps-filter", selectInput(ns("f_type"), "Problem type", c("All problem types" = ""), width = "100%")),
              div(class = "ps-filter", selectInput(ns("f_family"), "Test type", c("Parametric and non-parametric" = "", "Parametric" = "param", "Non-parametric" = "nonpar"), width = "100%")),
              div(class = "ps-filter", selectInput(ns("f_ph"), "Post-hoc", c("All post-hoc types" = ""), width = "100%")),
              div(class = "ps-filter", selectInput(ns("f_group"), "Group by", c("Problem type" = "type", "Test type" = "family", "Post-hoc" = "posthoc", "No grouping" = "none"), width = "100%"))))),
      uiOutput(ns("cards")))),
    # ---------- One problem, step by step ----------
    tabPanelBody("detail",
      uiOutput(ns("detail_head")),
      uiOutput(ns("stepper")),
      tabsetPanel(id = ns("steps"), type = "hidden",
                  tabPanelBody("1", uiOutput(ns("s1"))), tabPanelBody("2", uiOutput(ns("s2"))), tabPanelBody("3", uiOutput(ns("s3"))),
                  tabPanelBody("4", uiOutput(ns("s4"))), tabPanelBody("5", uiOutput(ns("s5"))), tabPanelBody("6", uiOutput(ns("s6"))),
                  tabPanelBody("7", uiOutput(ns("s7"))), tabPanelBody("8", uiOutput(ns("s8"))), tabPanelBody("9", uiOutput(ns("s9"))),
                  tabPanelBody("10", uiOutput(ns("s10"))), tabPanelBody("11", uiOutput(ns("s11"))), tabPanelBody("12", uiOutput(ns("s12"))),
                  tabPanelBody("13", uiOutput(ns("s13"))), tabPanelBody("14", uiOutput(ns("s14"))))),
    # ---------- Custom problem editor ----------
    tabPanelBody("edit",
      if (is.null(md)) page_head("Custom problem", "Write or paste a problem, bring in its data and say what each column is. The steps then work as for the course problems.",
                btn(ns("edit_back"), "Back to problems", "arrowl", "bs", "bsm"))
      else page_head(md$title, md$sub),
      div(class = "split wide",
          div(class = "col",
              kcard(div(class = "hrow", div(class = "ps-stepn", "1"), h2(class = "ct", if (is.null(md)) "Problem statement" else "Your question (optional)")),
                    div(style = "margin-top:12px", textInput(ns("c_title"), "Title", placeholder = "For example: Tensile strength of four mixing techniques")),
                    div(style = "margin-top:12px", textAreaInput(ns("c_statement"), if (is.null(md)) "Statement and questions" else "What you want to find out", rows = if (is.null(md)) 7 else 4, width = "100%",
                                                                 placeholder = "Paste the full problem. Writing α (for example α = 0.01), μ0 = … or σ = … lets the steps fill them in."))),
              kcard(div(class = "hrow", div(class = "ps-stepn", "2"), h2(class = "ct", "Dataset")),
                    div(style = "margin-top:12px", seg_input(ns("c_src"), c(if (!is.null(md)) c("Data Studio" = "ds"), "Paste" = "paste", "Upload a file" = "upload", "Class example" = "example"), if (is.null(md)) "paste" else "ds")),
                    conditionalPanel("input.c_src == 'ds'", ns = ns, div(class = "hrow", style = "margin-top:12px;align-items:flex-end",
                      div(style = "flex:1;min-width:220px", selectInput(ns("c_ds"), NULL, character(0), width = "100%")),
                      btn(ns("c_use_ds"), "Use this dataset", "check", "bs", "bsm"))),
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
                    p(class = "small", style = "margin:6px 0 12px", if (is.null(md)) "One sample: a Response only. Two or more groups: a Response and a Group (treatment). Paired data or a randomized block design: also the Block or pair ID. A Latin square needs Row and Column (and a Greek letter for a Graeco-Latin square). A factorial design needs Factor A and Factor B (and Factor C). The response is always a number." else md$need),
                    div(class = "col", style = "gap:12px",
                        selectInput(ns("c_response"), "Response (numeric)", NULL),
                        if (show_role("treatment")) selectInput(ns("c_treatment"), rlab("treatment", "Group, treatment or Factor A column"), NULL),
                        if (show_role("factorB") || show_role("factorC")) div(class = "grid g2", style = "gap:12px",
                            if (show_role("factorB")) selectInput(ns("c_factorB"), rlab("factorB", "Factor B (factorial)"), NULL),
                            if (show_role("factorC")) selectInput(ns("c_factorC"), rlab("factorC", "Factor C (three-factor)"), NULL)),
                        if (show_role("factorD") || show_role("factorE")) div(class = "grid g2", style = "gap:12px",
                            if (show_role("factorD")) selectInput(ns("c_factorD"), rlab("factorD", "Factor D (four-factor)"), NULL),
                            if (show_role("factorE")) selectInput(ns("c_factorE"), rlab("factorE", "Factor E (five-factor)"), NULL)),
                        if (show_role("block")) selectInput(ns("c_block"), rlab("block", "Block or pair ID (paired samples, RCBD)"), NULL),
                        if (show_role("row") || show_role("column") || show_role("greek")) div(class = "grid g3", style = "gap:12px",
                            if (show_role("row")) selectInput(ns("c_row"), rlab("row", "Row (Latin square)"), NULL),
                            if (show_role("column")) selectInput(ns("c_column"), rlab("column", "Column (Latin square)"), NULL),
                            if (show_role("greek")) selectInput(ns("c_greek"), rlab("greek", "Greek letter (Graeco-Latin)"), NULL)),
                        if (is.null(md) || length(intersect(md$roles, "treatment"))) uiOutput(ns("c_control_ui")),
                        if (is.null(md) || identical(mode, "one")) checkboxInput(ns("c_variance"), "The question is about a variance, not a mean (one sample)", FALSE))),
              uiOutput(ns("c_check")),
              btn(ns("c_go"), if (is.null(md)) "Continue to the steps" else "Start the 5 steps", "arrowr", "bp", "blg"))))
  )
}

problems_server <- function(id, store, show_crumb, open_design = NULL, mode = NULL, lang = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    md <- if (!is.null(mode)) PS_MODES[[mode]]
    # Charts follow the language of the page: the words on them are translated when it is Arabic.
    renderPlot <- function(expr, ...) {
      e <- substitute(expr); env <- parent.frame()
      shiny::renderPlot({ ar <- is.function(lang) && identical(lang(), "ar"); p <- eval(e, env); if (ar) ar_plot(p) else p }, ...)
    }
    ud <- new.env()   # the seed of the problem in the editor (per page: pages must not share it)
    sel <- reactiveVal(NULL)
    custom <- reactiveVal(NULL)
    memo <- reactiveValues()
    cdata <- reactiveVal(NULL)
    paired_hint <- reactiveVal(FALSE)
    method_hint <- reactiveVal(NULL)
    crev <- reactiveVal(0)
    keyn <- reactiveVal(0)
    # The problem being worked on. Inputs carry the problem key in their ids, so a value typed
    # for one problem can never be read for another while the browser catches up.
    cur <- reactiveValues(id = NULL, key = NULL, meta = NULL, st = NULL, kind = NULL, s = NULL, h0 = NULL, h1 = NULL, hs0 = NULL, hs1 = NULL,
                          h0n = NULL, h1n = NULL, hs0n = NULL, hs1n = NULL, test = NULL, test_user = FALSE, ph = NULL, res = NULL, res_np = NULL, step = 1, seen = 1,
                          fh = NULL, fhn = NULL)   # factorial designs: hypotheses of every effect, by effect code
    kid <- function(n) paste0(n, "_", isolate(cur$key))
    kin <- function(n) input[[paste0(n, "_", cur$key)]]

    view <- function(v, crumb = NULL) {
      updateTabsetPanel(session, "views", selected = v)
      show_crumb(crumb %||% (if (!is.null(mode)) PAGE_CRUMB[[mode]] else c("Design of experiments", "Problem Sets")))
      session$sendCustomMessage("scroll_top", list())
    }
    prob_label <- function(m) if (identical(m$id, "custom")) (if (nzchar(m$title %||% "")) m$title else if (!is.null(md)) md$title else "Custom problem") else sprintf("Problem %s", m$id)
    # Data the student edited in a problem replace the printed values (kept per problem until reset).
    edits <- reactiveValues()
    meta_of <- function(i) {
      if (identical(i, "custom")) return(custom())
      p <- PROBLEMS[[i]]; e <- edits[[i]]
      if (!is.null(e)) { p$data <- e; p$display <- e; p$edited <- TRUE }
      p
    }

    # ---------- List ----------
    output$cat_blurb <- renderUI({
      k <- input$cat %||% "ch2"
      if (k == "custom") return(p(class = "small", style = "margin:12px 0 0", "Write or paste your own problem, enter or upload the data, and work through the same steps."))
      ch <- PS_CHAPTERS[[k]]
      div(style = "margin-top:12px", h3(class = "ct", ch$title), p(class = "small", style = "margin:4px 0 0",
          paste(ch$blurb, "Filter by problem type, test type and post-hoc, and choose how the problems are grouped. Click a heading to collapse it.")))
    })
    # The filter choices list only what the chapter holds.
    observeEvent(input$cat, {
      k <- input$cat %||% "ch2"; if (k == "custom") return()
      cls <- lapply(ps_by_chapter(k), ps_classify)
      ty <- intersect(PS_TYPE_ORDER, vapply(cls, `[[`, "", "type")); ph <- intersect(PS_PH_ORDER, vapply(cls, `[[`, "", "posthoc"))
      updateSelectInput(session, "f_type", choices = c("All problem types" = "", stats::setNames(ty, ty)), selected = "")
      updateSelectInput(session, "f_ph", choices = c("All post-hoc types" = "", stats::setNames(ph, ph)), selected = "")
      updateSelectInput(session, "f_family", selected = "")
    })
    card <- function(p, cl, cur_id) {
      tags$a(class = paste("kcard acard ps-card", if (identical(cur_id, p$id)) "on"), href = "#",
             onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", ns("pick"), p$id),
             div(class = "hrow", span(class = "ps-num", paste("Problem", p$id)), if (identical(cur_id, p$id)) div(class = "ml", pill("Selected", "pass", "check"))),
             div(class = "at", p$title),
             div(class = "ad", sprintf("%s · %d observations", PS_KIND_LABEL[[cl$kind]], sum(!is.na(p$data[[p$roles$response]])))),
             div(class = "meta ps-chips",
                 span(class = "chip", icon("layers", 12), cl$type),
                 span(class = paste("chip", if (cl$family == "nonpar") "np"), icon("wand", 12), if (cl$family == "nonpar") "Non-parametric" else "Parametric"),
                 span(class = "chip", icon("flask", 12), cl$test),
                 if (!cl$posthoc %in% c("Not applicable")) span(class = "chip", icon("columns", 12), if (cl$posthoc == "None") "No post-hoc" else paste("Post-hoc:", cl$posthoc))))
    }
    group_box <- function(title, n, desc, body) {
      tags$details(class = "ps-group kcard", open = NA,
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
                 div(class = "ad", "Statement, dataset (paste, upload or a class example) and the column roles, then the steps.")),
          if (!is.null(cm)) tags$a(class = paste("kcard acard ps-card", if (identical(cur_id, "custom")) "on"), href = "#",
                                   onclick = sprintf("Shiny.setInputValue('%s', 'custom', {priority: 'event'}); return false;", ns("pick")),
                                   div(class = "hrow", span(class = "ps-num", "Custom")), div(class = "at", prob_label(cm)),
                                   div(class = "ad", sprintf("%d rows · %d columns", nrow(cm$data), ncol(cm$data))))))
      }
      ps <- ps_by_chapter(k)
      q <- tolower(trimws(input$q %||% ""))
      cls <- lapply(ps, ps_classify)
      ft <- input$f_type %||% ""; ff <- input$f_family %||% ""; fp <- input$f_ph %||% ""; gb <- input$f_group %||% "type"
      hit <- vapply(seq_along(ps), function(i) {
        c <- cls[[i]]
        (!nzchar(q) || grepl(q, tolower(paste(ps[[i]]$id, ps[[i]]$title, paste(ps[[i]]$statement, collapse = " "), c$test, c$type, c$posthoc)), fixed = TRUE)) &&
          (!nzchar(ft) || c$type == ft) && (!nzchar(ff) || c$family == ff) && (!nzchar(fp) || c$posthoc == fp)
      }, TRUE)
      if (!any(hit)) return(kcard(div(class = "empty", ico_box("search", "io", 24), h2(class = "ct", "No problems match"), p(class = "sub", "Change a filter or the search, or choose All."))))
      grid_of <- function(idx) div(class = "grid g3", lapply(idx, function(i) card(ps[[i]], cls[[i]], cur_id)))
      if (gb == "none") return(div(class = "col ps-groups", grid_of(which(hit))))
      key <- vapply(cls, function(c) switch(gb, type = c$type, family = if (c$family == "nonpar") "Non-parametric" else "Parametric",
                                           posthoc = c$posthoc), "")
      ord <- switch(gb, type = PS_TYPE_ORDER, family = c("Parametric", "Non-parametric"), posthoc = PS_PH_ORDER)
      groups <- intersect(ord, unique(key[hit]))
      div(class = "col ps-groups", lapply(groups, function(g) {
        idx <- which(hit & key == g)
        group_box(switch(gb, posthoc = if (g == "None") "No post-hoc (H0 not rejected)" else if (g == "Not applicable") "No post-hoc (one or two samples)" else paste("Post-hoc:", g), g),
                  length(idx), if (!is.na(PS_TYPE_DESC[g])) unname(PS_TYPE_DESC[g]), grid_of(idx))
      }))
    })
    output$resume <- renderUI({
      i <- sel(); if (is.null(i)) return(NULL)
      m <- meta_of(i); if (is.null(m)) return(NULL)
      note(kind = "g", ic = "check", div(class = "hrow", style = "gap:12px",
        span(sprintf("You are working on %s%s, at %s.", prob_label(m), if (identical(i, "custom")) "" else paste0(": ", m$title), ps_step_name(cur$step, cur$kind))),
        div(class = "ml"), tags$button(class = "btn-k bp bsm", onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("pick"), i), icon("arrowr", 14), "Continue")))
    })

    # ---------- Opening a problem ----------
    save_cur <- function() {
      i <- isolate(cur$id); if (is.null(i)) return()
      memo[[i]] <- isolate(list(s = cur$s, h0 = cur$h0, h1 = cur$h1, hs0 = cur$hs0, hs1 = cur$hs1, h0n = cur$h0n, h1n = cur$h1n, hs0n = cur$hs0n, hs1n = cur$hs1n, fh = cur$fh, fhn = cur$fhn,
                                test = cur$test, test_user = cur$test_user, ph = cur$ph, res = cur$res, res_np = cur$res_np, step = cur$step, seen = cur$seen, visited = cur$visited))
    }
    open_problem <- function(i, step = NULL) {
      m <- meta_of(i); if (is.null(m)) return()
      save_cur()
      st <- ps_structure(m$data, m$roles); kind <- ps_kind(st, m)
      if (is.na(kind)) { showNotification(ps_kind_problem(kind, st), type = "error", duration = 10); return() }
      sv <- isolate(memo[[i]])
      s <- sv$s %||% ps_default_settings(m, kind)
      info <- ps_info(kind, st, m, s); h <- ps_hypotheses(kind, st, m, s, info$family); hn <- ps_hypotheses(kind, st, m, s, "nonpar")
      keyn(isolate(keyn()) + 1)
      cur$id <- i; cur$meta <- m; cur$st <- st; cur$kind <- kind; cur$s <- s
      cur$h0 <- sv$h0 %||% h$h0; cur$h1 <- sv$h1 %||% h$h1; cur$hs0 <- sv$hs0 %||% h$h0; cur$hs1 <- sv$hs1 %||% h$h1
      cur$h0n <- sv$h0n %||% hn$h0; cur$h1n <- sv$h1n %||% hn$h1; cur$hs0n <- sv$hs0n %||% hn$h0; cur$hs1n <- sv$hs1n %||% hn$h1
      cur$test <- sv$test %||% info$suggest; cur$test_user <- isTRUE(sv$test_user)
      cur$ph <- sv$ph %||% s$posthoc
      if (ps_is_fact(kind)) {
        fhx <- function(fam) lapply(ps_fact_hypotheses(st, m, fam), function(e) list(h0 = e$h0, h1 = e$h1))
        cur$fh <- sv$fh %||% fhx("param"); cur$fhn <- sv$fhn %||% fhx("nonpar")
      } else { cur$fh <- NULL; cur$fhn <- NULL }
      cur$res <- sv$res; cur$res_np <- sv$res_np; cur$seen <- sv$seen %||% 1; cur$visited <- as.integer(sv$visited %||% seq_len(sv$seen %||% 1))
      cur$key <- paste0(gsub("[^A-Za-z0-9]", "", i), "k", isolate(keyn()))
      sel(i)
      go(step %||% sv$step %||% 1, scroll = FALSE)
      view("detail", if (!is.null(mode)) PAGE_CRUMB[[mode]] else c("Design of experiments", "Problem Sets", prob_label(m)))
    }
    observeEvent(input$pick, open_problem(input$pick))
    observeEvent(input$back, { save_cur(); view(if (is.null(mode)) "list" else "edit") })

    # ---------- Live settings from the step inputs ----------
    observe({
      k <- cur$key; req(k)
      g <- function(n) input[[paste0(n, "_", k)]]
      v <- list(alt = g("alt"), value = g("value"), alpha = g("alpha"), family = g("family"), var_mode = g("var_mode"), var_test = g("var_test"),
                sigma = g("sigma"), sigma2 = g("sigma2"), test = g("test"), ph = g("ph"), h0 = g("h0"), h1 = g("h1"), h0n = g("h0n"), h1n = g("h1n"))
      # The hypotheses of every effect of a factorial design are read here, outside isolate(), so typing re-runs this.
      fin <- list()
      if (ps_is_fact(isolate(cur$kind))) for (slot in c("fh", "fhn")) for (cd in names(isolate(cur[[slot]]))) for (w in c("h0", "h1"))
        fin[[paste(slot, w, cd, sep = "_")]] <- g(paste(slot, w, cd, sep = "_"))
      isolate({
        s <- cur$s
        if (!is.null(v$alt)) s$alt <- v$alt
        if (!is.null(v$value) && is.finite(v$value)) s$value <- v$value
        if (!is.null(v$alpha) && is.finite(v$alpha) && v$alpha > 0 && v$alpha < 1) s$alpha <- v$alpha
        if (!is.null(v$family)) s$family <- v$family
        if (!is.null(v$var_mode)) s$var_mode <- v$var_mode
        if (!is.null(v$var_test)) s$var_test <- v$var_test
        if (!is.null(v$sigma)) s["sigma"] <- list(if (is.finite(v$sigma) && v$sigma > 0) v$sigma else NULL)
        if (!is.null(v$sigma2)) s["sigma2"] <- list(if (is.finite(v$sigma2) && v$sigma2 > 0) v$sigma2 else NULL)
        s <- Filter(Negate(is.null), s)
        if (!identical(s, cur$s)) cur$s <- s
        if (!is.null(v$test) && !identical(v$test, cur$test)) { cur$test <- v$test; cur$test_user <- TRUE }
        if (!is.null(v$ph)) cur$ph <- v$ph
        for (n in c("h0", "h1", "h0n", "h1n")) if (!is.null(v[[n]])) cur[[n]] <- v[[n]]
        if (ps_is_fact(cur$kind)) for (slot in c("fh", "fhn")) {
          cur_l <- cur[[slot]]; ch <- FALSE
          for (cd in names(cur_l)) for (w in c("h0", "h1")) {
            x <- fin[[paste(slot, w, cd, sep = "_")]]
            if (!is.null(x) && !identical(x, cur_l[[cd]][[w]])) { cur_l[[cd]][[w]] <- x; ch <- TRUE }
          }
          if (ch) cur[[slot]] <- cur_l
        }
      })
    })
    is_multi <- reactive(ps_multi(cur$kind))
    # A rank test chosen in step 5 replaces the ANOVA: the process then runs for that test only.
    rank_first <- reactive(isTRUE(is_multi()) && cur$test %in% PS_NONPAR_TESTS)
    sname <- function(n) ps_step_name(n, isolate(cur$kind))
    pname <- function(kind, test) sub(" \\(F test\\)$", "", ps_param_name(kind, test))
    pname_lc <- function(kind, test) sub("^One-way", "one-way", pname(kind, test))
    info <- reactive({ req(cur$kind); ps_info(cur$kind, cur$st, cur$meta, cur$s) })
    choices <- reactive({ req(cur$kind); ps_test_choices(cur$kind, cur$s) })
    # The assumption checks always belong to the parametric model (a rank test chosen in step 5 is judged against it).
    asm_base <- reactive({ req(cur$kind); ps_assumptions(cur$kind, cur$st, cur$s, info()$suggest) })
    asm_test <- reactive(if (isTRUE(rank_first())) info()$suggest else cur$test)
    asm <- reactive({ req(cur$kind); ps_assumptions(cur$kind, cur$st, cur$s, asm_test()) })
    # Keep the hypotheses' wording in step with the settings until the student edits it.
    observe({
      req(cur$kind); h <- ps_hypotheses(cur$kind, cur$st, cur$meta, cur$s, if (isTRUE(rank_first())) "nonpar" else info()$family); hn <- ps_hypotheses(cur$kind, cur$st, cur$meta, cur$s, "nonpar")
      isolate({
        sync <- function(n, sug_n, val) {
          if (identical(cur[[n]], cur[[sug_n]]) && !identical(val, cur[[n]])) { cur[[n]] <- val; updateTextAreaInput(session, kid(n), value = val) }
          cur[[sug_n]] <- val
        }
        sync("h0", "hs0", h$h0); sync("h1", "hs1", h$h1); sync("h0n", "hs0n", hn$h0); sync("h1n", "hs1n", hn$h1)
      })
    })
    # The test follows the information step unless the student picked one. For the multi-group
    # designs the suggestion is the parametric test the information step names (ANOVA, or Welch ANOVA);
    # a rank test can be chosen instead (a warning says when the ANOVA assumptions do not call for it).
    observe({
      ch <- choices(); sug <- info()$suggest
      isolate({
        t <- if (!cur$test_user || !cur$test %in% names(ch)) sug else cur$test
        if (!cur$test %in% names(ch)) cur$test_user <- FALSE
        cur$test <- t
        updateSelectInput(session, kid("test"), choices = stats::setNames(names(ch), ifelse(names(ch) == sug, paste0(ch, "  ★ suggested"), ch)), selected = t)
      })
    })
    observeEvent(rank_first(), {
      if (!ps_is_fact(cur$kind)) return()
      fam <- if (rank_first()) "nonpar" else "param"
      cur$fh <- lapply(ps_fact_hypotheses(cur$st, cur$meta, fam), function(e) list(h0 = e$h0, h1 = e$h1))
      for (cd in names(cur$fh)) for (w in c("h0", "h1")) updateTextAreaInput(session, kid(paste0("fh_", w, "_", cd)), value = cur$fh[[cd]][[w]])
    }, ignoreInit = TRUE)
    reset_h <- function(sets) for (x in sets) { cur[[x[1]]] <- x[3]; cur[[x[2]]] <- x[3]; updateTextAreaInput(session, kid(x[1]), value = x[3]) }
    observeEvent(input[[paste0("fh_reset_", cur$key)]], {
      cur$fh <- lapply(ps_fact_hypotheses(cur$st, cur$meta, if (rank_first()) "nonpar" else "param"), function(e) list(h0 = e$h0, h1 = e$h1))
      for (cd in names(cur$fh)) for (w in c("h0", "h1")) updateTextAreaInput(session, kid(paste0("fh_", w, "_", cd)), value = cur$fh[[cd]][[w]])
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("fhn_reset_", cur$key)]], {
      cur$fhn <- lapply(ps_fact_hypotheses(cur$st, cur$meta, "nonpar"), function(e) list(h0 = e$h0, h1 = e$h1))
      for (cd in names(cur$fhn)) for (w in c("h0", "h1")) updateTextAreaInput(session, kid(paste0("fhn_", w, "_", cd)), value = cur$fhn[[cd]][[w]])
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("h_reset_", cur$key)]], {
      h <- ps_hypotheses(cur$kind, cur$st, cur$meta, cur$s, if (isTRUE(rank_first())) "nonpar" else if (is_multi()) "param" else info()$family)
      reset_h(list(c("h0", "hs0", h$h0), c("h1", "hs1", h$h1)))
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("hn_reset_", cur$key)]], {
      hn <- ps_hypotheses(cur$kind, cur$st, cur$meta, cur$s, "nonpar")
      reset_h(list(c("h0n", "hs0n", hn$h0), c("h1n", "hs1n", hn$h1)))
    }, ignoreInit = TRUE)

    # ---------- Running the tests ----------
    stale <- reactive(!is.null(cur$res) && !identical(cur$res$sig, list(cur$test, cur$s)))
    stale_np <- reactive(!is.null(cur$res_np) && !identical(cur$res_np$sig, cur$s))
    run_test <- function(quiet_ok = FALSE) {
      res <- tryCatch(ps_run_test(cur$kind, cur$test, cur$st, cur$meta, cur$s), error = function(e) e)
      if (inherits(res, "error")) { showNotification(paste("The test could not be run:", conditionMessage(res)), type = "error", duration = 10); return(FALSE) }
      res$sig <- list(cur$test, cur$s); cur$res <- res
      if (!quiet_ok) showNotification(res$summary_msg %||% sprintf("%s: %s = %s, %s.", res$t$name, res$t$stat_name, ps_fmt(res$t$stat, 4), ps_pe(res$p)), type = "message")
      TRUE
    }
    run_np <- function(quiet_ok = FALSE) {
      res <- tryCatch(ps_run_test(cur$kind, ps_np_test(cur$kind), cur$st, cur$meta, cur$s), error = function(e) e)
      if (inherits(res, "error")) { showNotification(sprintf("The %s could not be run: %s", ps_np_name(cur$kind), conditionMessage(res)), type = "error", duration = 10); return(FALSE) }
      res$sig <- cur$s; cur$res_np <- res
      if (!quiet_ok) showNotification(res$summary_msg %||% sprintf("%s: %s = %s, %s.", res$t$name, res$t$stat_name, ps_fmt(res$t$stat, 4), ps_pe(res$p)), type = "message")
      TRUE
    }

    # ---------- The flow ----------
    # One and two samples: steps 1-6. Multi-group designs: the parametric process to the end
    # first, the ANOVA (5, 6), post-hoc only when H0 is rejected (7) and the assumption checks
    # (8); then, only when an assumption fails, the non-parametric restart (9-14).
    flow_next <- function(n) {
      if (!isTRUE(is_multi())) return(if (n < 6) n + 1L else NULL)
      switch(as.character(n),
        "6" = if (isTRUE(cur$res$reject)) 7L else 8L, "7" = 8L,
        "8" = if (isTRUE(asm()$all_ok) || isTRUE(rank_first())) NULL else 9L,
        "13" = if (isTRUE(cur$res_np$reject)) 14L else NULL, "14" = NULL, n + 1L)
    }
    flow_prev <- function(n) {
      if (n <= 1) return(NULL)
      if (isTRUE(is_multi()) && n == 8) return(if (isTRUE(cur$res$reject)) 7L else 6L)
      n - 1L
    }
    go <- function(n, scroll = TRUE) {
      k <- isTRUE(isolate(is_multi())); n <- max(1L, min(if (k) PS_K_LAST else 6L, as.integer(n)))
      if (n >= 6 && (is.null(isolate(cur$res)) || isolate(stale()))) if (!run_test(quiet_ok = TRUE)) n <- 5L
      if (k && n == 7 && !isTRUE(isolate(cur$res)$reject)) { showNotification("H0 was not rejected, so no post-hoc analysis is needed. Next come the assumption checks.", type = "message"); n <- 8L }
      if (k && n >= 9 && isTRUE(isolate(rank_first()))) { showNotification("The rank test was chosen in step 5 and has already been carried out, so the non-parametric restart is not needed.", type = "message"); n <- 8L }
      if (k && n >= 9 && isTRUE(isolate(asm())$all_ok)) { showNotification("All the assumptions hold, so the non-parametric analysis is not needed.", type = "message"); n <- 8L }
      if (k && n >= 13 && (is.null(isolate(cur$res_np)) || isolate(stale_np()))) if (!run_np(quiet_ok = TRUE)) n <- 12L
      cur$step <- n; cur$seen <- max(isolate(cur$seen), n); cur$visited <- union(as.integer(isolate(cur$visited)), n)
      updateTabsetPanel(session, "steps", selected = as.character(n))
      if (scroll) session$sendCustomMessage("scroll_top", list())
    }
    observeEvent(input$go, go(input$go))
    observeEvent(input[[paste0("run_", cur$key)]], run_test(), ignoreInit = TRUE)
    observeEvent(input[[paste0("run_np_", cur$key)]], run_np(), ignoreInit = TRUE)

    # ---------- Header and stepper ----------
    output$detail_head <- renderUI({
      m <- cur$meta; req(m)
      page_head(prob_label(m), if (!is.null(md)) sprintf("%s · your own data", md$title) else if (identical(m$id, "custom")) "Your own problem" else m$title,
                btn(ns("back"), if (is.null(md)) "All problems" else "Change data or columns", "arrowl", "bs", "bsm"),
                if (identical(m$id, "custom")) btn(ns("edit_custom"), "Edit problem", "edit", "bs", "bsm"),
                btn(ns("share"), "Share analysis", "link", "bs", "bsm"),
                pin_button(ns))
    })
    output$stepper <- renderUI({
      req(cur$key); n <- cur$step; seen <- cur$seen; k <- is_multi(); kind <- cur$kind
      skipped <- function(i) k && ((i == 7 && !is.null(cur$res) && !cur$res$reject) || (i >= 9 && (isTRUE(asm()$all_ok) || isTRUE(rank_first()))) ||
                                   (i == 14 && !is.null(cur$res_np) && !cur$res_np$reject))
      dot <- function(i) {
        cls <- if (i == n) "cur" else if (skipped(i)) "skip" else if (i <= seen) "done" else ""
        tags$button(class = paste("st", cls), title = sprintf("%s%s", ps_step_name(i, kind), if (skipped(i)) " (not needed)" else ""),
                    onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("go"), i),
                    div(class = "dot", if (i != n && i <= seen && !skipped(i)) icon("check", 12, 3) else ps_step_num(i)), span(class = "lbl", PS_STEP_SHORT[i]))
      }
      row <- function(idx) { parts <- list(); for (j in seq_along(idx)) { parts[[length(parts) + 1]] <- dot(idx[j]); if (j < length(idx)) parts[[length(parts) + 1]] <- div(class = paste("sline", if (idx[j] < seen) "done")) }; parts }
      if (!k) return(div(class = "ps-stepbar", div(class = "stepper ps-stepper", row(1:6))))
      # The phase being worked on is shown in full; the other one is folded to its dots.
      np_on <- !isTRUE(asm()$all_ok) && !isTRUE(rank_first()) && !is.null(cur$res); in_np <- n >= 9
      div(class = "ps-stepbar",
          div(class = paste("ps-phase", if (in_np) "fold" else "open"),
              div(class = "ps-phase-lab", if (isTRUE(rank_first())) paste("1 · Rank test:", ps_np_name(kind)) else if (in_np) "1 · Parametric" else paste("1 · Parametric:", sub(" \\(F test\\)$", "", ps_param_name(kind, cur$test)))),
              div(class = "stepper ps-stepper many", title = if (in_np) sprintf("%s: an assumption failed", ps_param_name(kind, cur$test)), row(1:8),
                  if (in_np) span(class = "ps-fold-note", "Done"))),
          div(class = paste("ps-phase np", if (in_np) "open" else "fold", if (!np_on) "off"),
              div(class = "ps-phase-lab", if (in_np) paste("2 · Non-parametric:", ps_np_short(kind)) else "2 · Non-parametric"),
              div(class = "stepper ps-stepper", title = if (!in_np) sprintf("%s: used only if an ANOVA assumption fails", ps_np_name(kind)), row(9:PS_K_LAST),
                  if (!in_np) span(class = "ps-fold-note", if (np_on) "Next" else "If needed"))))
    })
    nav_btn <- function(n, label, primary = FALSE, back = FALSE) tags$button(class = paste("btn-k", if (primary) "bp" else "bs"),
      onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("go"), n), if (back) icon("arrowl", 15), label, if (!back) icon("arrowr", 15))
    for (i in 1:PS_K_LAST) local({
      n <- i
      output[[paste0("nav", n)]] <- renderUI({
        req(cur$key); pv <- flow_prev(n); nx <- flow_next(n)
        end <- is.null(nx) && n >= 6
        # the worked solution is offered only to a student who has been through the steps, not one who jumped to the last dot
        went <- all(2:6 %in% as.integer(cur$visited))
        tagList(
          if (end && !went) div(class = "ps-export lock", div(class = "ps-export-t", icon("check", 14, 2.6), "End of the analysis"),
                                p(class = "small", "The worked solution (PDF) and the R script appear here once you have been through every step, from the hypotheses to the decision.")),
          if (end && went) div(class = "ps-export",
                       div(class = "ps-export-t", icon("check", 14, 2.6), "End of the analysis"),
                       p(class = "small", "Keep your work: the worked solution as a PDF, or an R script you can run to check the numbers yourself."),
                       div(class = "hrow", style = "gap:10px;flex-wrap:wrap",
                           downloadButton(ns(paste0("dl_solution_", n)), label = tagList(icon("download", 15), "Download solution (PDF)"), icon = NULL, class = "btn-k bp"),
                           downloadButton(ns(paste0("dl_script_", n)), label = tagList(icon("code", 15), "R script"), icon = NULL, class = "btn-k bs"))),
          div(class = "ps-nav",
              if (!is.null(pv)) nav_btn(pv, sprintf("Back: %s", ps_step_name(pv, cur$kind)), back = TRUE), div(class = "ml"),
              if (!is.null(nx)) nav_btn(nx, sprintf("Next: %s", ps_step_name(nx, cur$kind)), primary = TRUE)))
      })
    })
    step_card <- function(n, sub, ...) {
      kcard(class = "ps-step",
            div(class = "ps-step-head", div(class = paste("ps-stepn", if (n >= 9) "np"), ps_step_num(n)),
                div(h2(class = "ct", sname(n)), p(class = "sub", sub))),
            div(class = "ps-step-body", ...), uiOutput(ns(paste0("nav", n))))
    }
    tbl <- function(df) if (!is.null(df) && nrow(df)) div(class = "tscroll", html_table(df))
    plot_box <- function(id, title, h = 250) div(class = "kcard soft ps-plot", div(class = "ps-plot-t", title), plotOutput(ns(id), height = h))
    data_block <- function(m, title = "Data") div(class = "col", style = "gap:8px",
      div(class = "hrow", h3(class = "ct", title), span(class = "small ml", sprintf("%d observations, as printed in the problem", sum(!is.na(m$data[[m$roles$response]]))))),
      div(class = "tscroll ps-data", html_table(m$display)))
    final_card <- function(title, text, kind = "rej") div(class = paste("ps-final", kind), div(class = "ps-final-t", icon("flag", 15), title), p(text))

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
            div(class = "col", style = "gap:8px",
                div(class = "hrow", h3(class = "ct", "Data"), div(class = "ml hrow",
                    downloadButton(ns("dl_data"), label = tagList(icon("download", 14), "CSV"), icon = NULL, class = "btn-k bs bsm"),
                    if (!identical(m$id, "custom")) btn(ns("edit_data"), "Edit data", "edit", "bs", "bsm"),
                    if (isTRUE(m$edited)) btn(ns("reset_data"), "Reset data", "arrowl", "bs", "bsm"),
                    if (!identical(m$id, "custom")) btn(ns("edit_this"), "Copy to Custom Problem", "copy", "bs", "bsm"))),
                if (isTRUE(m$edited)) note(kind = "w", ic = "edit", "You are working with edited data: every step uses your values, not the printed ones. Press Reset data to go back to the printed values."),
                div(class = "tscroll ps-data", html_table(m$display)),
                p(class = "small", style = "margin:0", sprintf("%d observations, as printed in the problem%s.", sum(!is.na(m$data[[m$roles$response]])),
                                                            if (any(is.na(m$data[[m$roles$response]]))) " (a cell marked missing is estimated in step 5)" else "")))))
    })
    output$dl_data <- downloadHandler(filename = function() sprintf("problem_%s.csv", gsub("[^A-Za-z0-9]+", "_", cur$meta$id)),
                                      content = function(file) utils::write.csv(cur$meta$data, file, row.names = FALSE))

    # ---------- Step 2: hypotheses ----------
    hyp_inputs <- function(id0, id1, v0, v1) div(class = "ps-hyp",
      div(class = "ps-hlab", "H0"), textAreaInput(ns(kid(id0)), NULL, v0, rows = 2, width = "100%", resize = "vertical"),
      div(class = "ps-hlab alt", "H1"), textAreaInput(ns(kid(id1)), NULL, v1, rows = 2, width = "100%", resize = "vertical"))
    # A factorial design has a pair of hypotheses for every main effect and every interaction.
    fact_hyp_block <- function(slot, fam) {
      hs <- ps_fact_hypotheses(isolate(cur$st), isolate(cur$meta), fam); vals <- isolate(cur[[slot]])
      div(class = "col", style = "gap:12px", lapply(names(hs), function(cd) {
        e <- hs[[cd]]; ef <- isolate(ps_fact_effects(cur$st, cur$meta))[[cd]]; np <- identical(fam, "nonpar")
        div(class = paste("ps-hblock", if (np) "np"),
            div(class = "ps-hhead", span(class = paste("pill", if (ef$main) "inf" else "warn"), if (ef$main) "Main effect" else "Interaction"), span(sprintf("%s (%s)", ef$label, cd))),
            div(class = "ps-sym", span(class = "chip", e$h0s), span(class = "chip", e$h1s)),
            hyp_inputs(paste0(slot, "_h0_", cd), paste0(slot, "_h1_", cd), vals[[cd]]$h0, vals[[cd]]$h1))
      }))
    }
    output$s2 <- renderUI({
      req(cur$key); kind <- isolate(cur$kind); s <- isolate(cur$s); multi <- ps_multi(kind)
      if (ps_is_fact(kind)) return(step_card(2, sprintf("State H0 and H1 for the %s, for each factor (its main effect) and for each interaction, about the population means (medians η if you choose the rank test in step 5). The system suggests them; edit them if needed.", pname(kind, info()$suggest)),
        fact_hyp_block("fh", "param"),
        note(kind = "i", ic = "info", if (isTRUE(isolate(cur$st$pool))) "There is one observation per combination, so there is no pure error: the main effects and the two-factor interactions are tested, and the three-factor and higher interactions are pooled to form the error (step 5)."
                                      else if (isTRUE(isolate(cur$st$unrep))) "There is one observation per combination, so only the main effects are tested; the interaction is checked with Tukey's test for nonadditivity (step 5)."
                                      else "If an interaction is significant, the main effects of the factors in it cannot be read alone: the post-hoc step compares the levels within each level of the other factor (simple effects)."),
        div(class = "hrow", actionLink(ns(kid("fh_reset")), tagList(icon("wand", 14), " Restore the suggested wording"), class = "small ps-link"))))
      step_card(2, if (multi) sprintf("State H0 and H1 for the parametric test (%s), about the population means μ (medians η if you choose the rank test in step 5). The system suggests both; edit them if needed.", pname(kind, info()$suggest))
                   else "State the null hypothesis H0 (no effect, no difference) and the alternative H1 (what the question wants to show). The system suggests both; edit them if needed.",
        if (!multi) div(class = "grid g2", style = "gap:12px 20px",
            selectInput(ns(kid("alt")), "Type of alternative", ps_alt_choices(kind), s$alt, width = "100%"),
            numericInput(ns(kid("value")), ps_value_label(kind), s$value, width = "100%")),
        uiOutput(ns("h_sym")), hyp_inputs("h0", "h1", isolate(cur$h0), isolate(cur$h1)),
        div(class = "hrow", actionLink(ns(kid("h_reset")), tagList(icon("wand", 14), " Restore the suggested wording"), class = "small ps-link")))
    })
    output$h_sym <- renderUI({
      req(cur$kind); multi <- is_multi(); h <- ps_hypotheses(cur$kind, cur$st, cur$meta, cur$s, if (multi && isTRUE(rank_first())) "nonpar" else if (multi) "param" else info()$family)
      div(class = "ps-sym", span(class = "chip", h$h0s), span(class = "chip", h$h1s),
          span(class = "small", if (multi) paste("With three or more treatments H1 is always “at least one differs”.",
                                               if (isTRUE(rank_first())) "A rank test is chosen in step 5, so the hypotheses are about the medians η." else "",
                                               if (cur$kind %in% PS_BLOCK_KINDS) "The blocking factors are nuisance factors: the hypotheses are about the treatments only." else "")
               else paste(if (info()$family == "nonpar") "Non-parametric test, so the hypotheses are about the median η." else "Parametric test, so the hypotheses are about the mean μ.",
                          switch(cur$s$alt, two.sided = "Two-sided alternative.", greater = "Right-tailed: the question asks whether the value is greater.", less = "Left-tailed: the question asks whether the value is less."))))
    })
    output$h_sym_np <- renderUI({
      req(is_multi()); h <- ps_hypotheses(cur$kind, cur$st, cur$meta, cur$s, "nonpar")
      div(class = "ps-sym", span(class = "chip", h$h0s), span(class = "chip", h$h1s),
          span(class = "small", sprintf("%s, so the hypotheses are about the population medians η%s. H1 is “at least one median differs”.",
                                        ps_np_name(cur$kind), if (identical(cur$kind, "rcbd")) " of the treatments within blocks" else "")))
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
                uiOutput(ns("alpha_note")))))
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
      step_card(4, switch(kind,
                          k = "Collect what is known about the groups and the data, and check whether the group variances are equal: this decides between the one-way ANOVA and the Welch ANOVA. The other assumptions are checked on the residuals after the test.",
                          two = , paired = "Collect what is known about the populations and the samples, check independence and normality, and choose between a parametric and a non-parametric test.",
                          one = , variance = "Collect what is known about the population and the sample, check normality, and choose between a parametric and a non-parametric test.",
                          "Collect what is known about the design and the data. No assumption test is run here: the ANOVA comes first, and its assumptions are checked on the residuals after the test."),
        if (kind %in% c("one", "two", "paired")) div(class = "grid g3", style = "gap:12px 20px",
            selectInput(ns(kid("family")), "Type of test", c("Decide from the information" = "auto", "Parametric" = "param", "Non-parametric" = "nonpar"), s$family %||% "auto", width = "100%"),
            if (kind == "one") numericInput(ns(kid("sigma")), "Population σ, if known", s[["sigma"]] %||% NA, min = 0, width = "100%"),
            if (kind == "two") numericInput(ns(kid("sigma")), "Population σ1, if known", s[["sigma"]] %||% NA, min = 0, width = "100%"),
            if (kind == "two") numericInput(ns(kid("sigma2")), "Population σ2, if known", s[["sigma2"]] %||% NA, min = 0, width = "100%"),
            # Equal variances are a question only while σ1 and σ2 are unknown.
            if (kind == "two") conditionalPanel(sprintf("!(input['%s'] > 0 && input['%s'] > 0)", ns(kid("sigma")), ns(kid("sigma2"))),
              selectInput(ns(kid("var_mode")), "Equal variances?", c("Decide with the F test" = "auto", "Assume equal" = "equal", "Assume unequal" = "unequal"), s$var_mode %||% "auto", width = "100%"))),
        if (kind == "k") div(class = "grid g3", style = "gap:12px 20px",
            selectInput(ns(kid("var_test")), "Test for equal group variances", c("Levene (median)" = "levene", "Bartlett" = "bartlett"), s$var_test %||% "levene", width = "100%"),
            selectInput(ns(kid("var_mode")), "Equal variances?", c("Decide with the test" = "auto", "Assume equal (one-way ANOVA)" = "equal", "Assume unequal (Welch ANOVA)" = "unequal"), s$var_mode %||% "auto", width = "100%")),
        uiOutput(ns("info_body")))
    })
    plan_ui <- function(kind, test) div(class = "ps-plan",
      div(class = "ps-plan-t", "How this problem is analysed"),
      div(class = "ps-plan-row",
          span(class = "ps-chipstep", paste("5 ·", pname(kind, test))), icon("arrowr", 14), span(class = "ps-chipstep", "6 · Decision"), icon("arrowr", 14),
          span(class = "ps-chipstep alt", "7 · Post-hoc (only if H0 is rejected)"), icon("arrowr", 14), span(class = "ps-chipstep", "8 · Assumption checks"), icon("arrowr", 14),
          span(class = "ps-chipstep np", sprintf("Non-parametric restart: %s (only if an assumption fails)", ps_np_name(kind)))),
      p(class = "small", style = "margin:8px 0 0", sprintf("If every assumption holds, the %s result is the answer. If any fails, the process starts again for the non-parametric test: hypotheses about medians (η), α, information, the %s, its decision and post-hoc.",
                                                     pname_lc(kind, test), ps_np_name(kind))))
    output$info_body <- renderUI({
      inf <- info(); a <- cur$s$alpha; kind <- cur$kind; m <- cur$meta
      facts <- div(h3(class = "ct", "What we know"), div(style = "margin-top:6px", lapply(seq_len(nrow(inf$facts)), function(i) div(class = "kv-row ps-fact", span(inf$facts$What[i]), span(inf$facts$Information[i])))))
      if (ps_is_fact(kind)) {
        nf <- length(cur$st$ffacs); npair <- choose(nf, 2); ih <- if (npair <= 1) 250 else if (npair <= 3) 300 else 130 * ceiling(npair / 3) + 90
        pl2 <- list(plot_box("pl_fmain", "Main-effects plot", if (nf >= 4) 260 else 300), plot_box("pl_finter", "Interaction plots", ih))
        eff2k <- if (ps_fact_2level(cur$st)) {
          ce <- ps_fact_contrasts(cur$st, m)
          div(class = "col", style = "gap:8px", h3(class = "ct", sprintf("Effect estimates of the 2%s design", c("²", "³", "⁴", "⁵")[min(max(nf - 1, 1), 4)])),
              p(class = "small", style = "margin:0", "Each effect is the average response at the high level minus the average at the low level (for an interaction, the same contrast of the product of the coded levels, −1 and +1). The sum of squares of an effect is N × (effect/2)², and the % contribution is its share of the total of all effects."),
              tbl(data.frame(Effect = sprintf("%s (%s)", ce$label, ce$code), `Effect estimate` = ps_fmt(ce$effect, 5), `Sum of squares` = ps_fmt(ce$ss, 6), `% contribution` = sprintf("%.2f", ce$pct), check.names = FALSE)),
              plot_box("pl_fnorm", "Normal probability plot of the effects", 340),
              p(class = "small", style = "margin:0", if (isTRUE(cur$st$pool)) "With one observation per combination there is no pure error. Effects that fall off the line through the small ones are real; the points near the line behave like noise, which is why the higher-order interactions can be pooled into the error in step 5."
                                                      else "A quick look before the tests: effects far from the line stand out. Step 5 tests every effect against the pure error of the replicates."))
        }
        return(tagList(facts, data_block(m, "The data"),
          div(class = "col", style = "gap:8px", h3(class = "ct", "Plots of the data"),
              p(class = "small", style = "margin:0", "Look at the data before testing: the main-effects plot shows how the mean response changes with each factor, the interaction plots show whether the effect of one factor depends on the level of another (non-parallel lines), and the boxplots show the spread of every treatment combination."),
              if (nf >= 4) tagList(pl2[[1]], pl2[[2]]) else div(class = "grid g2", style = "align-items:start", pl2[[1]], pl2[[2]]),
              plot_box("pl_fbox", "Boxplots by treatment combination", 280)),
          eff2k, plan_ui(kind, info()$suggest)))
      }
      if (kind %in% PS_BLOCK_KINDS) return(tagList(facts, data_block(m, "The data"), plan_ui(kind, info()$suggest)))
      norm_card <- if (!is.null(inf$normality)) div(class = "kcard soft ps-card-s", h3(class = "ct", "Normality"),
        tbl(transform(inf$normality, W = ps_fmt(W, 4), p = ps_p(p))),
        p(class = "small", style = "margin:8px 0 0", if (inf$stated) "The problem states the population is normal." else sprintf("Shapiro-Wilk at α = %s: p ≥ α means no evidence against normality.", format(a))))
      ind <- inf$independence
      ind_card <- if (!is.null(ind)) { ok <- is.na(ind$p) || ind$p >= a
        div(class = "kcard soft ps-card-s", h3(class = "ct", span(class = "hrow", icon("clock", 15), "Independence check (first)")),
            div(class = "kv-row", span("Breusch-Godfrey LM statistic (lag 1)"), span(ps_fmt(ind$stat, 4))), div(class = "kv-row", span("p-value"), span(ps_p(ind$p))),
            div(style = "margin-top:8px", if (ok) pill("Independence holds", "pass", "check") else pill("Observations look dependent", "fail", "x")),
            p(class = "small", style = "margin:8px 0 0", if (ok) sprintf("No evidence of correlation between consecutive observations (p ≥ %s), in the order the data are listed.", format(a))
              else "Consecutive observations are correlated in the order listed. Check how the data were collected before trusting the test.")) }
      ev <- inf$equal_var
      ev_card <- if (!is.null(ev) && kind == "k") { vm <- cur$s$var_mode %||% "auto"
        div(class = "kcard soft ps-card-s", h3(class = "ct", "Equal variances of the groups"),
            tbl(transform(ev$table, p = ps_p(p))),
            div(style = "margin-top:8px", if (ev$equal) pill("Variances can be treated as equal", "pass", "check") else pill("Variances differ", "warn", "alert")),
            p(class = "small", style = "margin:8px 0 0", sprintf("H0: all %d groups have the same variance. The %s decides (p %s %s)%s, so the %s is used.", nlevels(cur$st$d$treatment), ev$basis,
                                                           if (ev$equal) "≥" else "<", format(a), if (vm != "auto") sprintf("; you chose to assume %s variances", if (vm == "equal") "equal" else "unequal") else "",
                                                           pname_lc(kind, inf$suggest)))) }
      else if (!is.null(ev)) div(class = "kcard soft ps-card-s", h3(class = "ct", "Equal variances"),
        tbl(transform(ev$table, p = ps_p(p))),
        p(class = "small", style = "margin:8px 0 0", sprintf("The %s decides: %s.", ev$basis, if (ev$equal) "the variances can be treated as equal" else "the variances differ")))
      else if (kind == "two" && isTRUE(inf$sigma_known)) div(class = "kcard soft ps-card-s", h3(class = "ct", "Equal variances"),
        div(style = "margin-top:6px", pill("Not needed: σ1 and σ2 are known", "neu", "info")),
        p(class = "small", style = "margin:8px 0 0", "The population variances σ1² and σ2² are given, so the two-sample z-test uses them directly. An equal-variance test is only needed when they are unknown and must be estimated from the samples."))
      verdict <- if (kind == "k") tagList(
          div(class = "ps-verdict param", div(class = "ps-vtag", "Parametric test"),
              div(tags$b(inf$family_reason), br(), span(sprintf("Test used: %s. %s", ps_param_name(kind, inf$suggest), inf$suggest_reason)))),
          plan_ui(kind, inf$suggest))
        else div(class = paste("ps-verdict", inf$family),
                 div(class = "ps-vtag", if (inf$family == "param") "Parametric test" else "Non-parametric test"),
                 div(tags$b(inf$family_reason), br(), span(sprintf("Suggested test: %s. %s", ps_test_choices(kind, cur$s)[[inf$suggest]], inf$suggest_reason))))
      tagList(
        div(class = "grid g2 ps-info",
            div(class = "col", style = "gap:14px", facts, if (kind %in% c("two", "paired")) ind_card),
            div(class = "col", style = "gap:14px", norm_card, ev_card)),
        data_block(m, "The data"),
        verdict)
    })

    # ---------- Step 5: test ----------
    output$s5 <- renderUI({
      req(cur$key); kind <- isolate(cur$kind); ch <- isolate(choices()); sug <- isolate(info()$suggest); multi <- ps_multi(kind)
      step_card(5, if (multi) "The test that fits the design is selected for you (★ suggested): the parametric test first, whose assumptions are checked on the residuals in step 8. You can choose another test, including the rank test; a warning appears when it does not suit the data." else "Run the test chosen from the information in step 4. You can choose another test if you have a reason to.",
        div(class = "hrow", style = "align-items:flex-end;gap:14px",
            div(style = "flex:1;min-width:260px", selectInput(ns(kid("test")), "Test", stats::setNames(names(ch), ifelse(names(ch) == sug, paste0(ch, "  ★ suggested"), ch)), isolate(cur$test), width = "100%")),
            actionButton(ns(kid("run")), label = tagList(icon("play", 15), "Run test"), icon = NULL, class = "btn-k bp")),
        uiOutput(ns("test_body")))
    })
    block_note <- function(r) switch(r$kind,
      rcbd = "The blocks are a nuisance factor: their F ratio is shown for information. The test of interest is the treatments.",
      latin = "Rows and columns are blocking factors: their F ratios are shown for information. The test of interest is the treatments (Latin letters).",
      glsd = sprintf("Rows and columns are blocking factors. The Latin letters are the treatments being tested; the Greek letters (%s) have their own F test in the table.", r$term_labels[["greek"]]),
      NULL)
    # ---- factorial designs: helpers shared by the steps ----
    # The effect shown in a step: the student's pick, else `pick` (default the first of `pool`).
    fsel <- function(id, res, pool = res$effects$code, pick = NULL) {
      x <- input[[paste0(id, "_", cur$key)]]
      if (!is.null(x) && x %in% pool) x else (pick %||% pool[1])
    }
    eff_choices <- function(res, codes = res$effects$code) stats::setNames(codes, vapply(codes, function(cd) { e <- res$effects[res$effects$code == cd, ]
      sprintf("%s · %s%s", cd, e$label, if (e$p < res$alpha) "  (significant)" else "") }, ""))
    # The significant effects, interactions of the highest order first.
    sig_effects <- function(res) { sg <- res$effects$code[res$effects$p < res$alpha]; sg[order(-nchar(sg), match(sg, res$effects$code))] }
    fact_conclusions <- function(res, slot) vapply(res$effects$code, function(cd) ps_decision(ps_fact_effect_res(res, cd), cur[[slot]][[cd]]$h1 %||% "")$conclusion, "")
    eff_picker <- function(id, res, codes = res$effects$code, label = "Effect shown below", pick = NULL) {
      ch <- eff_choices(res, codes)
      div(style = "max-width:520px", selectInput(ns(kid(id)), label, ch, selected = pick %||% codes[1], width = "100%"))
    }
    result_fact_ui <- function(r, stale_flag, choice_note = NULL) {
      m <- cur$meta; vc <- if (isTRUE(m$random_blocks)) r$varcomp; anova_t <- identical(r$test, "anova"); eff <- r$effects; sg <- eff$code[eff$p < r$alpha]
      tile <- function(label, value, sub = NULL) div(class = "ps-tile", div(class = "ps-tl", label), div(class = "ps-tv", value), if (!is.null(sub)) div(class = "ps-ts", sub))
      tagList(choice_note,
        if (stale_flag) note(kind = "i", ic = "info", "The settings changed after this result was computed. Press Run test to update it."),
        div(class = "grid g4", style = "margin-top:4px",
            tile("Test", r$t$name), tile("Effects tested", as.character(nrow(eff)), paste(eff$code, collapse = ", ")),
            tile("Significant effects", if (length(sg)) paste(sg, collapse = ", ") else "None", sprintf("at α = %s", format(r$alpha))),
            tile(if (anova_t) "Error df" else "Statistic", if (anova_t) as.character(r$an$dfe) else sprintf("%s per effect", r$stat_name), if (anova_t) sprintf("MS_E = %s", ps_fmt(r$an$mse, 5)) else "χ² distribution")),
        div(class = "col", style = "gap:6px;margin-top:6px", h3(class = "ct", "How the statistics are computed"), div(class = "am-formula", lapply(r$formula, div))),
        if (!is.null(r$anova_df)) div(class = "col", style = "gap:6px", h3(class = "ct", "ANOVA table"), tbl(r$anova_df),
            p(class = "small", style = "margin:0", if (isTRUE(r$pool)) sprintf("There is one observation per combination, so there is no pure error. The error line pools the three-factor and higher interactions (%d df), which are assumed negligible; every main effect and two-factor interaction is tested against it.", r$an$dfe)
                                                  else if (isTRUE(r$unrep)) "There is one observation per combination, so there is no pure error: the error line is the interaction, and the interaction itself is tested below with Tukey's test for nonadditivity."
                                                  else if (identical(r$kind, "factb")) "Each replicate is a block: the block effect is removed from the error. The test of interest is each factor and each interaction."
                                                  else "Every factor and every interaction is tested against the error mean square.")),
        if (!is.null(r$nonadd_df)) div(class = "col", style = "gap:6px", h3(class = "ct", "Test for nonadditivity (Tukey, 1 df)"), tbl(r$nonadd_df),
            p(class = "small", style = "margin:0", "H0: the effects of the two factors add (no interaction). The interaction sum of squares has one degree of freedom and is tested against the remainder.")),
        if (!is.null(vc)) div(class = "kcard soft ps-card-s", h3(class = "ct", "Blocks: variance component and usefulness of blocking"),
            div(class = "kv-row", span("F test for blocks"), span(sprintf("F = %s, %s", ps_fmt(vc$F, 4), ps_pe(vc$p)))),
            div(class = "kv-row", span("σ̂²β = (MS_Blocks − MS_E) / (number of combinations)"), span(sprintf("(%s − %s) / %d = %s", ps_fmt(vc$msb, 6), ps_fmt(vc$mse, 6), vc$ncell, ps_fmt(vc$raw, 6)))),
            div(class = "kv-row", span("Relative efficiency of blocking against a CRD"), span(ps_fmt(vc$re, 4))),
            p(class = "small", style = "margin:8px 0 0", if (vc$raw < 0) "The estimate is negative, so it is set to 0: there is no evidence of variation between blocks."
                                                         else sprintf("Blocking %s: a relative efficiency %s 1 means the blocked design is more precise than a completely randomized one.", if (vc$re > 1.1) "was useful" else "did little", if (vc$re > 1) "above" else "at or below"))),
        div(class = "col", style = "gap:6px", h3(class = "ct", if (anova_t) "Each effect: F test" else sprintf("Each effect: %s", r$t$name)), tbl(ps_test_table(r))),
        if (!is.null(r$rank_tables)) div(class = "col", style = "gap:10px", lapply(names(r$rank_tables), function(cd) div(class = "col", style = "gap:6px",
            h3(class = "ct", sprintf("Ranks of the levels of %s", r$ef[[cd]]$label)), tbl(r$rank_tables[[cd]])))))
    }
    result_ui <- function(r, stale_flag, choice_note = NULL) {
      if (ps_is_fact(r$kind)) return(result_fact_ui(r, stale_flag, choice_note))
      t <- r$t; m <- cur$meta
      tile <- function(label, value, sub = NULL) div(class = "ps-tile", div(class = "ps-tl", label), div(class = "ps-tv", value), if (!is.null(sub)) div(class = "ps-ts", sub))
      dfx <- if (is.character(t$df)) t$df else if (!is.na(t$df %||% NA)) ps_fmt(t$df, 4) else "—"
      fill <- r$fill; vc <- r$varcomp
      tagList(choice_note,
        if (stale_flag) note(kind = "i", ic = "info", "The settings changed after this result was computed. Press Run test to update it."),
        div(class = "grid g4", style = "margin-top:4px",
            tile("Test", t$name), tile(sprintf("Test statistic (%s)", t$stat_name), ps_fmt(t$stat, 5), if (dfx != "—") paste("df =", dfx)),
            tile("P-value", ps_p(t$p), if (ps_multi(r$kind)) "right tail" else switch(r$alt, two.sided = "two-sided", greater = "right tail", less = "left tail")),
            tile(if (!is.null(t$est_name)) t$est_name else "α", if (!is.null(t$est_name)) ps_fmt(t$est) else format(r$alpha),
                 if (identical(t$est_name, "η²") || identical(t$est_name, "Partial η²")) "share of the variation explained by the treatments")),
        if (!is.null(fill)) div(class = "kcard soft ps-card-s",
            h3(class = "ct", if (fill$n == 1) "Estimated missing value" else "Estimated missing values"),
            tbl(stats::setNames(transform(fill$est, Estimate = ps_fmt(Estimate, 6)), c(vapply(names(fill$est)[-ncol(fill$est)], function(k) ucfirst(ps_lab(m, k)), ""), "Estimated value"))),
            p(class = "small", style = "margin:8px 0 0", "The value that makes the error sum of squares smallest is put in the empty cell, and one error degree of freedom is removed for each estimated value.")),
        div(class = "col", style = "gap:6px;margin-top:6px", h3(class = "ct", "How the statistic is computed"), div(class = "am-formula", lapply(r$formula, div))),
        if (!is.null(r$anova_df)) div(class = "col", style = "gap:6px", h3(class = "ct", "ANOVA table"), tbl(r$anova_df),
                                      if (!is.null(block_note(r))) p(class = "small", style = "margin:0", block_note(r))),
        if (!is.null(r$exact)) div(class = "col", style = "gap:6px", h3(class = "ct", "Approximate and exact analysis"), tbl(r$exact),
            p(class = "small", style = "margin:0", "The exact analysis fits the model to the observed data only (general regression significance test), so the treatment sum of squares is adjusted for blocks. The approximate analysis uses the estimated value; its treatment sum of squares is slightly too large.")),
        if (!is.null(vc)) div(class = "kcard soft ps-card-s", h3(class = "ct", "Block variance component (random blocks)"),
            div(class = "kv-row", span("σ̂²β = (MS_Blocks − MS_E) / a"), span(sprintf("(%s − %s) / %d = %s", ps_fmt(vc$msb, 6), ps_fmt(vc$mse, 6), vc$a, ps_fmt(vc$raw, 6)))),
            div(class = "kv-row", span("σ̂² = MS_E (error variance)"), span(ps_fmt(vc$mse, 6))),
            if (vc$raw < 0) p(class = "small", style = "margin:8px 0 0", "The estimate is negative, so it is set to 0: there is no evidence of variation between blocks."),
            p(class = "small", style = "margin:8px 0 0", sprintf("With random blocks, E(MS_Blocks) = σ² + aσ²β, where a = %d treatments, which gives the estimator above.", vc$a))),
        div(class = "col", style = "gap:6px", h3(class = "ct", "Result"), tbl(ps_test_table(r))))
    }
    output$test_body <- renderUI({
      r <- cur$res; inf <- info()
      adv <- ps_test_advice(cur$kind, cur$test, inf$suggest, choices(), if (is_multi()) asm_base())
      choice_note <- if (!is.null(adv)) note(kind = if (adv$level == "w") "w" else "i", ic = if (adv$level == "w") "alert" else "info", adv$text)
      if (is.null(r)) return(tagList(choice_note, div(class = "ps-empty", icon("play", 20), p(class = "sub", "Press Run test to compute the test statistic and p-value."))))
      tagList(result_ui(r, stale(), choice_note), if (!is.null(r[["rank_table"]])) div(class = "col", style = "gap:6px;margin-top:8px", h3(class = "ct", r[["rank_title"]]), tbl(r[["rank_table"]])))
    })

    # ---------- Step 6 and 13: decision ----------
    decision_ui <- function(d, r, h0, h1, ids) {
      method <- function(nm, title, ok, txt, plot = NULL) div(class = "kcard ps-method",
        div(class = "hrow", span(class = "ps-mn", nm), h3(class = "ct", title), div(class = "ml", if (is.na(ok)) pill("Not available", "neu") else if (ok) pill("Reject H0", "pass") else pill("Fail to reject H0", "neu"))),
        if (!is.null(plot)) plotOutput(ns(plot), height = 200), p(class = "small", style = "margin:8px 0 0", txt))
      tagList(
        div(class = paste("ps-decision", if (d$reject) "rej" else "keep"),
            div(class = "ps-dicon", icon(if (d$reject) "check" else "info", 20, 2.6)),
            div(div(class = "ps-dt", if (d$reject) "Reject H0" else "Fail to reject H0"), p(style = "margin:4px 0 0", d$conclusion))),
        div(class = "grid g3", style = "align-items:start",
            method(1, "Rejection region", d$rr$reject, d$rr$text, ids[1]),
            method(2, "Confidence interval", if (isTRUE(d$ci$available)) d$ci$reject else NA, d$ci$text, if (isTRUE(d$ci$available)) ids[2]),
            method(3, "P-value", d$pv$reject, d$pv$text, ids[3])),
        div(class = "ps-hyprecap", span(class = "small", tags$b("H0: "), h0), span(class = "small", tags$b("H1: "), h1)))
    }
    dec_res <- reactive({ r <- cur$res; req(r); if (ps_is_fact(r$kind)) ps_fact_effect_res(r, fsel("eff6", r)) else r })
    decision <- reactive({ r <- cur$res; req(r)
      if (ps_is_fact(r$kind)) { cd <- fsel("eff6", r); return(ps_decision(dec_res(), cur$fh[[cd]]$h1 %||% "")) }
      ps_decision(r, cur$h1 %||% "") })
    output$s6 <- renderUI({ req(cur$key); fct <- ps_is_fact(isolate(cur$kind))
      step_card(6, if (fct) "Decide each effect on its own with three methods: the rejection region, the confidence interval and the p-value. The table lists every effect; choose one below to see its decision, plots and effect plot."
                   else "Decide whether to reject H0 with three methods: the rejection region, the confidence interval and the p-value. With a 1 − α interval they normally lead to the same decision.",
                if (fct) uiOutput(ns("dec_summary")), if (fct) uiOutput(ns("eff_pick6")),
                uiOutput(ns("dec_body"))) })
    output$dec_summary <- renderUI({ r <- cur$res; req(r, ps_is_fact(r$kind))
      div(class = "col", style = "gap:6px", h3(class = "ct", "All the effects"), tbl(ps_test_table(r)),
          p(class = "small", style = "margin:0", sprintf("Significant at α = %s: %s.", format(r$alpha), if (any(r$effects$p < r$alpha)) paste(r$effects$code[r$effects$p < r$alpha], collapse = ", ") else "none"))) })
    output$eff_pick6 <- renderUI({ r <- cur$res; req(r, ps_is_fact(r$kind)); sg <- sig_effects(r); eff_picker("eff6", r, pick = if (length(sg)) sg[1] else NULL) })
    output$dec_body <- renderUI({
      d <- decision(); r <- cur$res; m <- cur$meta
      if (ps_is_fact(r$kind)) { cd <- fsel("eff6", r); er <- dec_res()
        return(tagList(decision_ui(d, er, cur$fh[[cd]]$h0, cur$fh[[cd]]$h1, c("pl_rr", "pl_ci", "pl_pv")),
          div(class = "kcard ps-method", div(class = "hrow", h3(class = "ct", sprintf("Effect plot: %s", r$ef[[cd]]$label))), plotOutput(ns("pl_eff"), height = 260),
              p(class = "small", style = "margin:8px 0 0", if (r$ef[[cd]]$main) "The line joins the mean of each level; the dots are the observations." else "Parallel lines mean no interaction; lines that cross or fan out mean the effect of one factor depends on the level of the other.")),
          if (any(r$effects$p[!r$effects$main] < r$alpha) && r$ef[[cd]]$main) note(kind = "w", ic = "alert", "An interaction is significant, so the main effects of the factors in it cannot be read alone: the post-hoc step compares levels within each level of the other factor (simple effects)."),
          note(kind = "i", ic = "info", if (any(r$effects$p < r$alpha)) "At least one effect is significant, so the next step finds which levels differ (post-hoc). The assumptions are checked after that."
                                         else if (rank_first()) "No effect is significant, so no post-hoc analysis is needed. The next step shows the ANOVA assumption checks, which tell whether the rank test was needed."
                                         else "No effect is significant, so no post-hoc analysis is needed. The next step checks the ANOVA assumptions."))) }
      tagList(decision_ui(d, r, cur$h0, cur$h1, c("pl_rr", "pl_ci", "pl_pv")),
              if (isTRUE(m$scaled_t) && !is.null(r$ph_all)) div(class = "kcard ps-method",
                  div(class = "hrow", h3(class = "ct", "Treatment means against a scaled t distribution")),
                  plotOutput(ns("pl_st"), height = 230),
                  p(class = "small", style = "margin:8px 0 0", "The curve is a t distribution with the error degrees of freedom, scaled by √(MSE/n). Imagine sliding it along the axis: means that fit under the curve together are not different; a mean far outside it differs from the others.")),
              if (is_multi()) note(kind = "i", ic = "info", if (d$reject) "H0 is rejected, so the next step finds which treatments differ (post-hoc). The assumptions are checked after that."
                                                            else if (rank_first()) "H0 is not rejected, so no post-hoc analysis is needed. The next step shows the ANOVA assumption checks, which tell whether the rank test was needed."
                                                            else "H0 is not rejected, so no post-hoc analysis is needed. The next step checks the ANOVA assumptions."))
    })
    output$pl_rr <- renderPlot({ d <- decision(); ps_plot_rr(d$ref, cur$res$alpha) }, bg = "transparent", res = 88)
    output$pl_ci <- renderPlot({ d <- decision(); r <- cur$res; req(isTRUE(d$ci$available))
      if (ps_multi(r$kind)) ps_plot_ci_k(d$ci$table, r$conf) else plot_ci_one(r$t, ref_value(r), r$conf, cur$meta$labels$unit %||% "") }, bg = "transparent", res = 88)
    output$pl_pv <- renderPlot({ d <- decision(); er <- dec_res(); ps_plot_pvalue(d$ref, er$p, er$alpha) }, bg = "transparent", res = 88)
    output$pl_eff <- renderPlot({ r <- cur$res; req(r, ps_is_fact(r$kind)); ps_plot_fact_effect(r, fsel("eff6", r)) }, bg = "transparent", res = 88)
    output$pl_st <- renderPlot({ r <- cur$res; req(r, r$ph_all); ps_plot_scaled_t(r) }, bg = "transparent", res = 88)

    # ---------- Step 7 and 14: post-hoc ----------
    posthoc_ui <- function(ph, r) {
      dun <- identical(ph$method, "dunnett")
      tagList(
        p(class = "ltext", sprintf("%s: %d of %d %s differ at α = %s. %s", ph$name, sum(ph$raw$signif), nrow(ph$raw), if (dun) "treatments" else "pairs", format(r$alpha),
                                   if (dun) sprintf("Each treatment is compared with the control (%s).", r$control) else "Treatments that share a letter are not significantly different.")),
        div(class = "grid g2", style = "align-items:start",
            div(class = "col", style = "gap:6px", h3(class = "ct", if (dun) "Comparisons with the control" else "Pairwise comparisons"), tbl(ph$table)),
            div(class = "col", style = "gap:14px", div(class = "col", style = "gap:6px", h3(class = "ct", if (dun) "Compared with the control" else "Grouping"), tbl(ph$groups)),
                if (!is.null(ph$raw$lwr)) plot_box(if (identical(r, cur$res_np)) "pl_ph_np" else "pl_ph", "Simultaneous intervals for the differences", 230))))
    }
    # One effect's comparisons: the levels of its factor (main effect) or of each factor within the levels of the others (interaction).
    fact_ph_ui <- function(r, cd, method, fam = "param") {
      ph <- ps_fact_posthoc(r, cd, method); e <- r$ef[[cd]]
      nsig <- function(b) sum(b$raw$signif, na.rm = TRUE)
      tagList(
        if (!e$main) note(kind = "w", ic = "alert", sprintf("%s is significant, so its main effects cannot be read alone. Each factor is compared within every level of the other factor (simple effects).", e$label))
        else note(kind = "i", ic = "info", sprintf("The levels of %s are compared two by two; levels that share a letter are not significantly different.", tolower(e$label))),
        lapply(ph$blocks, function(b) {
          pairs <- function(tb, gp) div(class = "grid g2", style = "align-items:start", div(class = "col", style = "gap:6px", h3(class = "ct", "Comparisons"), tbl(tb)), div(class = "col", style = "gap:6px", h3(class = "ct", "Grouping"), tbl(gp)))
          within <- "Within" %in% names(b$table)
          div(class = "kcard soft ps-card-s", h3(class = "ct", b$title),
            p(class = "small", style = "margin:4px 0 8px", sprintf("%d of %d comparisons differ at α = %s.%s", nsig(b), nrow(b$raw), format(r$alpha), if (isTRUE(b$fallback)) " Dunnett's test needs a control, so Tukey HSD is used here." else "")),
            if (!within) pairs(b$table, b$groups)
            else div(class = "col", style = "gap:12px", lapply(unique(b$table$Within), function(w)
              div(class = "ps-simple", div(class = "ps-simple-t", w), pairs(b$table[b$table$Within == w, names(b$table) != "Within", drop = FALSE], b$groups[b$groups$Within == w, names(b$groups) != "Within", drop = FALSE])))))
        }))
    }
    output$s7 <- renderUI({
      req(cur$key); r <- cur$res
      ch <- if (!is.null(r)) ps_posthoc_choices(r$test, r$kind, r$control) else character(0); sg <- ps_default_ph(isolate(cur$s), ch)
      step_card(7, if (!is.null(r) && ps_is_fact(r$kind)) "At least one effect is significant, so find which levels differ: the levels of a factor for a significant main effect, and the simple effects for a significant interaction." else "H0 was rejected, so find which pairs of treatments differ.",
        if (length(ch) && isTRUE(r$reject)) div(class = "hrow", style = "gap:14px;align-items:flex-end;flex-wrap:wrap",
          if (ps_is_fact(r$kind)) { sge <- sig_effects(r); eff_picker("eff7", r, sge, "Significant effect", sge[1]) },
          div(style = "width:340px;max-width:100%", selectInput(ns(kid("ph")), "Method", stats::setNames(names(ch), ifelse(names(ch) == sg, paste0(ch, "  ★ suggested"), ch)),
                                                                                    if (isolate(cur$ph) %in% names(ch)) isolate(cur$ph) else sg, width = "100%"))),
        if (isTRUE(r$reject) && "dunnett" %in% names(ch)) note(kind = "i", ic = "info", sprintf("The problem names a control group (%s), so Dunnett's test, which compares every treatment with the control, is offered and suggested.", r$control)),
        uiOutput(ns("ph_body")))
    })
    posthoc <- reactive({
      r <- cur$res; req(r, ps_multi(r$kind), !ps_is_fact(r$kind), r$reject)
      ch <- ps_posthoc_choices(r$test, r$kind, r$control); m <- if (isTRUE(cur$ph %in% names(ch))) cur$ph else ps_default_ph(cur$s, ch)
      c(ps_posthoc(r, m), list(method = m, name = ch[[m]]))
    })
    output$ph_body <- renderUI({
      r <- cur$res; req(r)
      if (!r$reject) return(div(class = "ps-empty left", icon("info", 18), p(class = "sub", style = "margin:0", if (ps_is_fact(r$kind)) sprintf("No effect is significant at α = %s, so no post-hoc analysis is carried out.", format(r$alpha)) else sprintf("H0 was not rejected (%s ≥ α = %s), so no post-hoc analysis is carried out.", ps_pe(r$p), format(r$alpha)))))
      if (ps_is_fact(r$kind)) { ch <- ps_posthoc_choices(r$test, r$kind, r$control); m <- if (isTRUE(cur$ph %in% names(ch))) cur$ph else ps_default_ph(cur$s, ch)
        return(fact_ph_ui(r, fsel("eff7", r, sig_effects(r)), m)) }
      posthoc_ui(posthoc(), r)
    })
    output$pl_ph <- renderPlot({ ph <- posthoc(); req(!is.null(ph$raw$lwr)); ps_plot_ci_k(ph$raw, cur$res$conf) }, bg = "transparent", res = 88)

    # ---------- Step 8: assumption checks ----------
    np_about <- function(kind) switch(kind, k = "compares the groups through the ranks of all the observations",
                                      rcbd = "ranks the responses within each block, so differences between blocks cannot affect it",
                                      fact = "ranks all the observations together and tests each factor and each interaction on the ranks",
                                      factb = "ranks the treatment combinations within each block and tests each factor and each interaction on the rank sums",
                                      factl = , factg = "removes the blocking factors, ranks the residuals and tests each factor and each interaction on the ranks (Quade's test)",
                                      "removes the blocking factors (rows and columns, and Greek letters) and compares the treatments through the ranks of the residuals (Quade's test)")
    output$s8 <- renderUI({ req(cur$key); step_card(8, "Check the three ANOVA assumptions on the residuals: normality, constant variance (Breusch-Pagan) and independence.", uiOutput(ns("asm_body"))) })
    output$asm_body <- renderUI({
      a <- asm(); al <- cur$s$alpha; r <- cur$res; kind <- cur$kind
      status <- function(ok) if (is.na(ok)) pill(if (isTRUE(a$welch)) "Not required" else "Not available", "neu") else if (ok) pill("Satisfied", "pass", "check") else pill("Not satisfied", "fail", "x")
      sec <- function(title, ok, ...) div(class = "kcard ps-asm", div(class = "hrow", h3(class = "ct", title), div(class = "ml", status(ok))), ...)
      d <- if (!is.null(r) && !ps_is_fact(kind)) ps_decision(r, cur$h1 %||% "")
      concl <- if (ps_is_fact(kind)) paste(fact_conclusions(r, "fh"), collapse = " ") else if (!is.null(d)) d$conclusion else ""
      pn <- ps_param_name(kind, cur$test)
      tagList(
        sec("Normality", a$normal_ok, tbl(a$normality),
            p(class = "small", sprintf("H0: the residuals come from a normal distribution. p ≥ %s means no evidence against normality.", format(al))),
            div(class = "grid g2", plot_box("pl_qq", "Normal QQ plot of the residuals"), plot_box("pl_hist", "Histogram of the residuals"))),
        sec("Constant variance of the residuals", a$var_ok,
            if (isTRUE(a$welch)) note(kind = "i", ic = "info", "Not required: the Welch ANOVA does not assume equal variances, so this check does not apply. The plots are shown for reference.")
            else tagList(tbl(a$variance), p(class = "small", sprintf("H0: the residuals have constant variance (Breusch-Pagan test: the squared residuals regressed on %s). p ≥ %s means the spread can be treated as constant.",
                                                                    if (ps_is_fact(kind)) "the fitted values; with a term for every combination and two replicates, the usual version would reject whatever the data" else "the model terms", format(al)))),
            div(class = "grid g2", plot_box("pl_rvf", "Residuals versus fitted values"), plot_box("pl_box", "Spread of each treatment"))),
        sec("Independence", a$indep_ok, tbl(a$independence),
            p(class = "small", if (ps_is_fact(kind)) "H0: the residuals are independent (no correlation between consecutive observations). The observations are listed replicate by replicate: every combination once, then the next replicate." else "H0: the residuals are independent (no correlation between consecutive observations, in the order the data are listed)."),
            plot_box("pl_ord", "Residuals in the order listed", 210)),
        if (isTRUE(rank_first())) final_card(
            if (a$all_ok) "The assumptions hold: the ANOVA was also valid" else sprintf("%s failed: the rank test was the right choice", paste(a$failed, collapse = " and ")),
            paste(if (a$all_ok) sprintf("The residual checks above are all satisfied, so the %s would have been valid and is usually more powerful than a rank test. The result of the %s (step 6) stands, but its conclusions are about medians.", pn, ps_np_name(kind))
                  else sprintf("The residual checks above show that %s failed, so the %s result cannot be trusted and the %s used in step 5 is the appropriate analysis. The process stops here.", paste(tolower(a$failed), collapse = " and "), pn, ps_np_name(kind)), concl),
            if (isTRUE(r$reject)) "rej" else "keep")
        else if (a$all_ok) final_card("All assumptions hold: the analysis stops here",
                                 paste(sprintf("The %s result stands.", pn), concl), if (isTRUE(r$reject)) "rej" else "keep")
        else div(class = "ps-switch", div(class = "ps-final-t", icon("alert", 15), sprintf("%s failed: start again with the non-parametric test", paste(a$failed, collapse = " and "))),
                 p(sprintf("The %s assumes %s, and the check above says this is not reasonable here, so its result cannot be trusted. The process starts again for the %s, which %s: new hypotheses about the medians, the significance level, the information, the test, the decision and the post-hoc.",
                           pn, paste(tolower(a$failed), collapse = " and "), ps_np_name(kind), np_about(kind)))))
    })
    output$pl_qq <- renderPlot({ a <- asm(); plot_qq_vec(a$targets[[1]], "Residuals") }, bg = "transparent", res = 88)
    output$pl_hist <- renderPlot({ a <- asm(); plot_hist(a$targets[[1]], "Residuals") }, bg = "transparent", res = 88)
    output$pl_ord <- renderPlot({ ps_plot_order(asm()$ind_fit) }, bg = "transparent", res = 88)
    output$pl_fmain <- renderPlot({ req(ps_is_fact(cur$kind)); ps_plot_fact_main(cur$st, cur$meta) }, bg = "transparent", res = 88)
    output$pl_finter <- renderPlot({ req(ps_is_fact(cur$kind)); ps_plot_fact_inter(cur$st, cur$meta) }, bg = "transparent", res = 88)
    output$pl_fnorm <- renderPlot({ req(ps_is_fact(cur$kind), ps_fact_2level(cur$st)); ps_plot_fact_normal(cur$st, cur$meta) }, bg = "transparent", res = 88)
    output$pl_fbox <- renderPlot({ req(ps_is_fact(cur$kind)); ps_plot_data(cur$kind, cur$st, cur$meta) }, bg = "transparent", res = 88)
    output$pl_box <- renderPlot({ ps_plot_data(cur$kind, cur$st, cur$meta) }, bg = "transparent", res = 88)
    output$pl_rvf <- renderPlot({ ps_plot_rvf(asm()$ind_fit) }, bg = "transparent", res = 88)

    # ---------- Steps 9-11: the non-parametric restart ----------
    output$s9 <- renderUI({
      req(cur$key)
      if (ps_is_fact(isolate(cur$kind))) return(step_card(9, "An ANOVA assumption failed, so the process starts again for the non-parametric test. State H0 and H1 about the population medians η, for each factor and each interaction. The system suggests them; edit them if needed.",
        uiOutput(ns("np_why")), fact_hyp_block("fhn", "nonpar"),
        div(class = "hrow", actionLink(ns(kid("fhn_reset")), tagList(icon("wand", 14), " Restore the suggested wording"), class = "small ps-link"))))
      step_card(9, "An ANOVA assumption failed, so the process starts again for the non-parametric test. State H0 and H1 about the population medians η. The system suggests both; edit them if needed.",
        uiOutput(ns("np_why")),
        uiOutput(ns("h_sym_np")), hyp_inputs("h0n", "h1n", isolate(cur$h0n), isolate(cur$h1n)),
        div(class = "hrow", actionLink(ns(kid("hn_reset")), tagList(icon("wand", 14), " Restore the suggested wording"), class = "small ps-link")))
    })
    output$np_why <- renderUI({
      a <- asm(); req(!a$all_ok); kind <- cur$kind
      div(class = "ps-process",
          div(class = "ps-process-t", sprintf("Why the analysis starts again: %s failed", paste(tolower(a$failed), collapse = " and "))),
          p(style = "margin:4px 0 0", sprintf("The %s compares means and needs normal, independent errors with constant variance. The %s %s, so its hypotheses are about medians instead of means.",
                                              ps_param_name(kind, cur$test), ps_np_name(kind), np_about(kind))))
    })
    output$s10 <- renderUI({
      req(cur$key)
      step_card(10, "The non-parametric test is judged at the same significance level as the parametric analysis.", uiOutput(ns("alpha_np")))
    })
    output$alpha_np <- renderUI({
      a <- cur$s$alpha; req(a)
      div(class = "grid g2 ps-alpha",
          div(class = "ps-kv",
              div(class = "kv-row", span("α (Type I error)"), span(format(a))),
              div(class = "kv-row", span(sprintf("The %s rejects H0 when", ps_np_name(cur$kind))), span(sprintf("p-value < %s", format(a))))),
          div(class = "col", style = "gap:10px",
              note(kind = "i", ic = "info", sprintf("α = %s is kept from step 3, so both analyses use the same level.", format(a))),
              div(class = "hrow", actionLink(ns("np_alpha_edit"), tagList(icon("edit", 14), " Change α in step 3"), class = "small ps-link"))))
    })
    observeEvent(input$np_alpha_edit, go(3))
    output$s11 <- renderUI({
      req(cur$key)
      step_card(11, "Collect what is known for the non-parametric test: the design, what the assumption checks showed, and the data.", uiOutput(ns("info_np")))
    })
    output$info_np <- renderUI({
      a <- asm(); req(!a$all_ok); m <- cur$meta; d <- cur$st$d; kind <- cur$kind
      facts <- info()$facts; facts <- facts[!facts$What %in% c("Population standard deviation", "Model"), , drop = FALSE]
      facts$Information[facts$What == "Population distribution"] <- sprintf("Not assumed normal: the ANOVA assumption checks found %s failed.", paste(tolower(a$failed), collapse = " and "))
      why <- switch(kind,
        k = c(sprintf("%d independent samples and an ANOVA assumption fails, so the Kruskal-Wallis test is used.", nlevels(d$treatment)),
              "It needs only independent random samples with distributions of similar shape, and compares the groups through the ranks of all the observations."),
        rcbd = c("A randomized complete block design and an ANOVA assumption fails, so the Friedman test is used.",
                 "It ranks the responses within each block, so block-to-block differences cannot affect the result, and it needs no normality."),
        fact = c("A factorial experiment in a completely randomized layout and an ANOVA assumption fails, so the Kruskal-Wallis test is used for each factor and each interaction.",
                 "All the observations are ranked together; the rank sum of each level (or combination) gives the H statistic of that effect, compared with the χ² distribution."),
        factb = c("A factorial experiment in blocks and an ANOVA assumption fails, so the Friedman test is used for each factor and each interaction.",
                  "The treatment combinations are ranked within each block, so block-to-block differences cannot affect the result, and the rank sums give the statistic of each effect."),
        factl = , factg = c(sprintf("A %s and an ANOVA assumption fails, so Quade's test is used for each factor and each interaction.", tolower(PS_KIND_LABEL[[kind]])),
                            "The row and column (and Greek-letter) effects are removed, the residuals are ranked, and each effect is tested on the ranks with a χ² statistic (permutation p-values are shown as well)."),
        c(sprintf("A %s and an ANOVA assumption fails, so Quade's test is used.", tolower(PS_KIND_LABEL[[kind]])),
          "Quade's test: fit the blocking factors only, rank the residuals, and compare the treatments through these ranks (the Kruskal-Wallis statistic, with the permutation p-value shown as well). Dunn's test with the Holm adjustment, or the Wilcoxon rank-sum test with the Bonferroni adjustment, then finds which treatments differ."))
      tagList(
        div(class = "grid g2 ps-info",
            div(h3(class = "ct", "What we know"), div(style = "margin-top:6px", lapply(seq_len(nrow(facts)), function(i) div(class = "kv-row ps-fact", span(facts$What[i]), span(facts$Information[i]))))),
            div(class = "col", style = "gap:6px", h3(class = "ct", "Assumption checks of the ANOVA (step 8)"), tbl(ps_asm_summary(a)))),
        data_block(m, "The data"),
        div(class = "ps-verdict nonpar", div(class = "ps-vtag", "Non-parametric test"), div(tags$b(why[1]), br(), span(why[2]))))
    })

    # ---------- Steps 12-14: the rank test, its decision and post-hoc ----------
    output$s12 <- renderUI({
      req(cur$key); kind <- isolate(cur$kind)
      step_card(12, sprintf("Compare the treatments with the %s, which %s.", ps_np_name(kind), np_about(kind)),
        uiOutput(ns("np_hrecap")),
        div(class = "hrow", style = "align-items:flex-end;gap:14px",
            div(style = "flex:1;min-width:260px", div(class = "klab", "Test"), div(class = "ps-fixed", icon("lock", 14), ps_np_name(kind))),
            actionButton(ns(kid("run_np")), label = tagList(icon("play", 15), "Run test"), icon = NULL, class = "btn-k bp")),
        uiOutput(ns("np_body")))
    })
    output$np_hrecap <- renderUI({
      if (ps_is_fact(cur$kind)) return(div(class = "ps-hyprecap", lapply(names(cur$fhn), function(cd) span(class = "small", tags$b(sprintf("%s: ", cd)), cur$fhn[[cd]]$h1)), span(class = "small", sprintf("α = %s", format(cur$s$alpha)))))
      div(class = "ps-hyprecap", span(class = "small", tags$b("H0: "), cur$h0n), span(class = "small", tags$b("H1: "), cur$h1n),
          span(class = "small", sprintf("α = %s", format(cur$s$alpha))))
    })
    output$np_body <- renderUI({
      r <- cur$res_np
      if (is.null(r)) return(div(class = "ps-empty", icon("play", 20), p(class = "sub", sprintf("Press Run test to compute the %s statistic.", ps_np_name(cur$kind)))))
      tagList(result_ui(r, stale_np()), if (!is.null(r[["rank_table"]])) div(class = "col", style = "gap:6px", h3(class = "ct", r[["rank_title"]]), tbl(r[["rank_table"]])))
    })
    decres_np <- reactive({ r <- cur$res_np; req(r); if (ps_is_fact(r$kind)) ps_fact_effect_res(r, fsel("eff13", r)) else r })
    decision_np <- reactive({ r <- cur$res_np; req(r)
      if (ps_is_fact(r$kind)) { cd <- fsel("eff13", r); return(ps_decision(decres_np(), cur$fhn[[cd]]$h1 %||% "")) }
      ps_decision(r, cur$h1n %||% "") })
    output$s13 <- renderUI({ req(cur$key); fct <- ps_is_fact(isolate(cur$kind))
      step_card(13, sprintf("Decide whether to reject H0 for the %s%s. The rank test has no confidence interval, so the rejection region and the p-value decide.", ps_np_name(isolate(cur$kind)), if (fct) ", effect by effect" else ""),
                if (fct) uiOutput(ns("dec_np_summary")), if (fct) uiOutput(ns("eff_pick13")), uiOutput(ns("dec_np_body"))) })
    output$dec_np_summary <- renderUI({ r <- cur$res_np; req(r, ps_is_fact(r$kind))
      div(class = "col", style = "gap:6px", h3(class = "ct", "All the effects"), tbl(ps_test_table(r))) })
    output$eff_pick13 <- renderUI({ r <- cur$res_np; req(r, ps_is_fact(r$kind)); sg <- sig_effects(r); eff_picker("eff13", r, pick = if (length(sg)) sg[1] else NULL) })
    np_ph_name <- function(kind) { ch <- ps_posthoc_choices(ps_np_test(kind), kind); ch[[1]] }
    set_aside <- function() sprintf("The %s was set aside because %s failed.", ps_param_name(cur$kind, cur$test), paste(tolower(asm()$failed), collapse = " and "))
    output$dec_np_body <- renderUI({
      d <- decision_np(); r <- cur$res_np; kind <- cur$kind
      if (ps_is_fact(kind)) { cd <- fsel("eff13", r)
        return(tagList(decision_ui(d, decres_np(), cur$fhn[[cd]]$h0, cur$fhn[[cd]]$h1, c("pl_rr_np", "pl_ci_np", "pl_pv_np")),
          div(class = "kcard ps-method", div(class = "hrow", h3(class = "ct", sprintf("Effect plot: %s", r$ef[[cd]]$label))), plotOutput(ns("pl_eff_np"), height = 260)),
          if (!r$reject) final_card(sprintf("Final conclusion (%s)", ps_np_short(kind)), paste(paste(fact_conclusions(r, "fhn"), collapse = " "), set_aside()), "keep")
          else note(kind = "i", ic = "info", sprintf("At least one effect is significant, so the next step finds which levels differ with the %s.", np_ph_name(kind))))) }
      tagList(decision_ui(d, r, cur$h0n, cur$h1n, c("pl_rr_np", "pl_ci_np", "pl_pv_np")),
              if (!d$reject) final_card(sprintf("Final conclusion (%s)", ps_np_short(kind)), paste(d$conclusion, set_aside()), "keep")
              else note(kind = "i", ic = "info", sprintf("H0 is rejected, so the next step finds which treatments differ with the %s.", np_ph_name(kind))))
    })
    output$pl_rr_np <- renderPlot({ d <- decision_np(); ps_plot_rr(d$ref, cur$res_np$alpha) }, bg = "transparent", res = 88)
    output$pl_pv_np <- renderPlot({ d <- decision_np(); er <- decres_np(); ps_plot_pvalue(d$ref, er$p, er$alpha) }, bg = "transparent", res = 88)
    output$pl_eff_np <- renderPlot({ r <- cur$res_np; req(r, ps_is_fact(r$kind)); ps_plot_fact_effect(r, fsel("eff13", r)) }, bg = "transparent", res = 88)
    posthoc_np <- reactive({ r <- cur$res_np; req(r, r$reject); ch <- ps_posthoc_choices(r$test, r$kind); m <- fsel("phn", r, names(ch), names(ch)[1])
      c(ps_posthoc(r, m), list(method = m, name = ch[[m]])) })
    output$s14 <- renderUI({ req(cur$key); kind <- isolate(cur$kind); r <- cur$res_np; fct <- ps_is_fact(kind)
      step_card(14, sprintf("The %s rejected H0, so the %s finds which %s differ.", ps_np_name(kind), np_ph_name(kind), if (fct) "levels" else "pairs of treatments"),
        if (!is.null(r) && isTRUE(r$reject) && (fct || length(ps_posthoc_choices(r$test, r$kind)) > 1)) div(class = "hrow", style = "gap:14px;align-items:flex-end;flex-wrap:wrap",
          if (fct) { sge <- sig_effects(r); eff_picker("eff14", r, sge, "Significant effect", sge[1]) },
          { ch <- ps_posthoc_choices(r$test, r$kind); div(style = "width:380px;max-width:100%", selectInput(ns(kid("phn")), "Method", stats::setNames(names(ch), ifelse(seq_along(ch) == 1, paste0(ch, "  ★ suggested"), ch)), names(ch)[1], width = "100%")) }),
        uiOutput(ns("ph_np_body"))) })
    output$ph_np_body <- renderUI({
      r <- cur$res_np; req(r)
      if (!r$reject) return(div(class = "ps-empty left", icon("info", 18), p(class = "sub", style = "margin:0", sprintf("The %s did not reject H0, so no post-hoc analysis is needed.", r$t$name))))
      if (ps_is_fact(r$kind)) { ch <- ps_posthoc_choices(r$test, r$kind); m <- fsel("phn", r, names(ch), names(ch)[1])
        return(tagList(fact_ph_ui(r, fsel("eff14", r, sig_effects(r)), m, "nonpar"),
                       final_card(sprintf("Final conclusion (%s)", ps_np_short(cur$kind)), paste(paste(fact_conclusions(r, "fhn"), collapse = " "), set_aside()), "keep"))) }
      ph <- posthoc_np(); d <- decision_np()
      tagList(posthoc_ui(ph, r),
              final_card(sprintf("Final conclusion (%s)", ps_np_short(cur$kind)), paste(d$conclusion, sprintf("The %s finds %d of %d pairs different.", ph$name, sum(ph$raw$signif), nrow(ph$raw)), set_aside())))
    })

    # ---------- Pin to report ----------
    pin_observers(input, session, store,
      items = reactive({
        r <- cur$res; req(r)
        if (ps_is_fact(r$kind)) {
          hyp_txt <- paste(vapply(r$effects$code, function(cd) sprintf("%s (%s)\n  H0: %s\n  H1: %s", cd, r$ef[[cd]]$label, cur$fh[[cd]]$h0, cur$fh[[cd]]$h1), ""), collapse = "\n")
          it <- list(
            hyp = list(label = "Hypotheses and significance level", default = TRUE, build = function() mk_pin("text", "Hypotheses", "file", text = sprintf("%s\nSignificance level α = %s.", hyp_txt, format(r$alpha)))),
            test = list(label = "Effects tested", default = TRUE, build = function() mk_pin("table", r$t$name, "table", df = ps_test_table(r))),
            decision = list(label = "Decision for each effect", default = TRUE, build = function() mk_pin("text", "Decision", "check", text = paste(fact_conclusions(r, "fh"), collapse = "\n"))))
          if (!is.null(r$anova_df)) it$anova <- list(label = "ANOVA table", default = TRUE, build = function() mk_pin("table", "ANOVA table", "table", df = r$anova_df))
          if (r$reject) it$posthoc <- list(label = "Post-hoc comparisons (first significant effect)", default = TRUE, build = function() {
            cd <- sig_effects(r)[1]; ch <- ps_posthoc_choices(r$test, r$kind, r$control); ph <- ps_fact_posthoc(r, cd, ps_default_ph(cur$s, ch)); mk_pin("table", sprintf("Post-hoc: %s", r$ef[[cd]]$label), "columns", df = ph$blocks[[1]]$table) })
          it$asm <- list(label = "Assumption checks", default = TRUE, build = function() mk_pin("table", "Assumption checks", "scale", df = ps_asm_summary(asm())))
          rn <- cur$res_np
          if (!is.null(rn)) it$np <- list(label = sprintf("%s and decisions", rn$t$name), default = TRUE, build = function() mk_pin("table", rn$t$name, "shuffle", df = ps_test_table(rn)))
          return(it)
        }
        d <- ps_decision(r, cur$h1 %||% "")
        it <- list(
          hyp = list(label = "Hypotheses and significance level", default = TRUE, build = function() mk_pin("text", "Hypotheses", "file",
                     text = sprintf("H0: %s\nH1: %s\nSignificance level α = %s.", cur$h0, cur$h1, format(r$alpha)))),
          test = list(label = "Test result", default = TRUE, build = function() mk_pin("table", r$t$name, "table", df = ps_test_table(r))),
          decision = list(label = "Decision (three methods)", default = TRUE, build = function() mk_pin("text", "Decision", "check",
                          text = paste(c(d$conclusion, paste("Rejection region:", d$rr$text), paste("Confidence interval:", d$ci$text), paste("P-value:", d$pv$text)), collapse = "\n"))),
          rr = list(label = "Rejection region plot", default = FALSE, build = function() mk_pin("plot", "Rejection region", "bar", plot = ps_plot_rr(d$ref, r$alpha), h = 3)))
        if (!is.null(r$anova_df)) it$anova <- list(label = "ANOVA table", default = TRUE, build = function() mk_pin("table", "ANOVA table", "table", df = r$anova_df))
        if (ps_multi(r$kind) && r$reject) it$posthoc <- list(label = "Post-hoc comparisons", default = TRUE, build = function() { ph <- posthoc(); mk_pin("table", ph$name, "columns", df = ph$table) })
        if (ps_multi(r$kind)) it$asm <- list(label = "Assumption checks", default = TRUE, build = function() mk_pin("table", "Assumption checks", "scale", df = ps_asm_summary(asm())))
        rn <- cur$res_np
        if (!is.null(rn)) { dn <- ps_decision(rn, cur$h1n %||% "")
          it$np <- list(label = sprintf("%s and decision", rn$t$name), default = TRUE, build = function() mk_pin("text", rn$t$name, "shuffle",
                        text = sprintf("H0: %s\nH1: %s\n%s = %s, df = %s, %s.\n%s", cur$h0n, cur$h1n, rn$t$stat_name, ps_fmt(rn$t$stat, 4), format(rn$t$df), ps_pe(rn$p), dn$conclusion)))
          if (rn$reject) it$np_ph <- list(label = "Post-hoc after the rank test", default = TRUE, build = function() { ph <- posthoc_np(); mk_pin("table", ph$name, "columns", df = ph$table) }) }
        it
      }),
      src = reactive({
        r <- cur$res; m <- cur$meta
        list(key = paste0("problem_", gsub("[^A-Za-z0-9]", "_", m$id), if (!is.null(mode)) paste0("_", mode)), label = sprintf("%s · %s", prob_label(m), r$t$name),
             title = if (identical(m$id, "custom")) prob_label(m) else sprintf("Problem %s: %s", m$id, m$title),
             subtitle = sprintf("%s · α = %s", r$t$name, format(r$alpha)), dataset = prob_label(m), notes = c(m$source %||% "", m$note %||% character(0)), code = "")
      }))

    # ---------- Custom problem editor ----------
    role_ids <- c(response = "c_response", treatment = "c_treatment", factorB = "c_factorB", factorC = "c_factorC", factorD = "c_factorD", factorE = "c_factorE", block = "c_block", row = "c_row", column = "c_column", greek = "c_greek")
    # "(none)" needs a real value: the dropdown cannot send an empty one, so an emptied role
    # would otherwise keep pointing at a column from the previous dataset.
    NO_ROLE <- ".none"
    set_roles <- function(df, roles = NULL) {
      ch <- c("(none)" = NO_ROLE, names(df))
      if (is.null(roles)) roles <- if (is.null(mode)) ps_guess_roles(df) else ps_guess_roles_mode(df, mode, isTRUE(paired_hint()))
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
        ud$ps_seed <- seed
        load_data(seed$data, seed$roles)
      }
      view("edit", if (!is.null(mode)) PAGE_CRUMB[[mode]] else c("Design of experiments", "Problem Sets", "Custom problem"))
    }
    observeEvent(input$new_custom, { if (is.null(custom())) ud$ps_seed <- NULL; open_editor() })
    observeEvent(input$edit_custom, open_editor())
    observeEvent(input$edit_this, {
      m <- cur$meta; req(m)
      open_editor(list(title = sprintf("Problem %s (edited)", m$id), statement = paste(c(m$statement, m$parts), collapse = "\n"),
                       data = m$data, roles = m$roles, target = m$target, settings = m$settings, labels = m$labels, control = m$control))
      showNotification(sprintf("Problem %s copied into the editor. Change anything, then press Continue to the steps.", m$id), type = "message")
    })
    observeEvent(input$edit_back, view("list"))

    # ---------- Editable data ----------
    # A grid of the problem's data (one row per observation, as the process reads it). Apply checks that the
    # edited data still form the same layout, then restarts the steps on them; Reset restores the printed data.
    show_ed_msg <- function(txt) output$ed_msg <- renderUI(if (!is.null(txt)) note(kind = "w", ic = "alert", txt))
    observeEvent(input$edit_data, {
      m <- cur$meta; req(m); show_ed_msg(NULL)
      showModal(modalDialog(title = sprintf("Edit the data of Problem %s", m$id), size = "l", easyClose = FALSE,
        p(class = "small", "Change a value, add a row (right-click) or delete one. Keep the columns as they are. Press Apply to run the steps on your data."),
        div(class = "gridbox", rhandsontable::rHandsontableOutput(ns("ed_grid"))), uiOutput(ns("ed_msg")),
        footer = tagList(modalButton("Cancel"), if (isTRUE(m$edited)) actionButton(ns("ed_reset"), "Reset to the printed data", class = "btn-k bs"),
                         actionButton(ns("ed_apply"), "Apply", class = "btn-k bp"))))
    })
    output$ed_grid <- rhandsontable::renderRHandsontable({
      m <- isolate(cur$meta); req(m); df <- as.data.frame(m$data, check.names = FALSE)
      rhandsontable::rhandsontable(df, useTypes = FALSE, stretchH = "all", height = min(420, 60 + 30 * max(nrow(df), 6)), rowHeaders = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
    reopen_with <- function(id) {
      memo[[id]] <- NULL; cur$id <- NULL        # so the old state is not saved back over the new data
      removeModal(); open_problem(id, step = 1)
    }
    observeEvent(input$ed_apply, {
      m <- cur$meta; req(m, input$ed_grid)
      # The raw cell text is read (not hot_to_r), so text typed in a numeric column is caught instead of turning into NA.
      g <- input$ed_grid
      df <- tryCatch({
        rows <- lapply(g$data, function(r) vapply(r, function(v) if (is.null(v) || (length(v) == 1 && is.na(v))) NA_character_ else as.character(v), ""))
        x <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE, check.names = FALSE)
        names(x) <- unlist(g$params$colHeaders); x
      }, error = function(e) NULL)
      if (is.null(df)) return(show_ed_msg("The grid could not be read. Try again."))
      orig <- m$data
      if (!identical(names(df), names(orig))) return(show_ed_msg("The columns changed. Keep the same columns and edit the values."))
      for (n in names(df)) {
        x <- df[[n]]
        if (is.numeric(orig[[n]])) { x <- suppressWarnings(as.numeric(gsub(",", ".", trimws(as.character(x))))); bad <- is.na(x) & !is.na(df[[n]]) & nzchar(trimws(as.character(df[[n]])))
          if (any(bad)) return(show_ed_msg(sprintf("Row %d of %s is not a number.", which(bad)[1], n))) }
        else { x <- as.character(x); x[!is.na(x) & !nzchar(trimws(x))] <- NA }
        df[[n]] <- x
      }
      df <- df[rowSums(!is.na(df)) > 0, , drop = FALSE]; rownames(df) <- NULL
      st2 <- ps_structure(df, m$roles); k2 <- ps_kind(st2, m)
      if (is.na(k2)) return(show_ed_msg(paste("These data cannot be analysed:", ps_kind_problem(k2, st2))))
      if (!identical(k2, cur$kind)) return(show_ed_msg(sprintf("The edit changes the layout (%s instead of %s). Keep the same groups, blocks and factors.", PS_KIND_LABEL[[k2]], PS_KIND_LABEL[[cur$kind]])))
      id <- m$id; edits[[id]] <- df
      reopen_with(id); showNotification("Your data are in use. The steps start again from step 1.", type = "message")
    })
    reset_edits <- function() {
      m <- cur$meta; req(m, isTRUE(m$edited)); id <- m$id; edits[[id]] <- NULL
      reopen_with(id); showNotification("The printed data are back. The steps start again from step 1.", type = "message")
    }
    observeEvent(input$reset_data, reset_edits())
    observeEvent(input$ed_reset, reset_edits())

    # ---------- Worked-solution PDF ----------
    ui_lang <- function() { l <- if (is.function(lang)) isolate(lang()) else NULL; if (identical(l, "ar")) "ar" else "en" }
    sol_make <- function(m, s, test, ph, file) {
      tryCatch(ps_solution_pdf(m, file, s = s, test = test, ph = ph, lang = ui_lang()), error = function(e) {
        showNotification(paste("The solution could not be built:", conditionMessage(e)), type = "error", duration = 10)
        grDevices::cairo_pdf(file); grid::grid.text("The solution could not be built."); grDevices::dev.off() })
    }
    dl_solution_h <- downloadHandler(
      filename = function() sprintf("solution_%s.pdf", gsub("[^A-Za-z0-9]+", "_", cur$meta$id)),
      content = function(file) sol_make(isolate(cur$meta), isolate(cur$s), isolate(cur$test), isolate(cur$ph), file))
    dl_script_h <- downloadHandler(
      filename = function() sprintf("solution_%s.R", gsub("[^A-Za-z0-9]+", "_", if (is.null(mode)) cur$meta$id else paste0(mode, "_", cur$meta$id))),
      content = function(file) {
        hyp <- isolate(list(h0 = cur$h0, h1 = cur$h1, fh = cur$fh))
        code <- tryCatch(ps_solution_script(isolate(cur$meta), isolate(cur$s), isolate(cur$test), isolate(cur$ph), hyp),
                         error = function(e) { showNotification(paste("The script could not be built:", conditionMessage(e)), type = "error", duration = 10); paste("# The script could not be built:", conditionMessage(e)) })
        writeLines(code, file, useBytes = TRUE)
      })
    # the two downloads sit at the end of the analysis; each step that can be the last has its own button id
    for (i in 6:PS_K_LAST) local({ n <- i; output[[paste0("dl_solution_", n)]] <- dl_solution_h; output[[paste0("dl_script_", n)]] <- dl_script_h })
    # ---------- Share the analysis (file or code) ----------
    share_state <- function(name = "", note = "") {
      m <- isolate(cur$meta); req(m)
      sv <- isolate(list(s = cur$s, h0 = cur$h0, h1 = cur$h1, hs0 = cur$hs0, hs1 = cur$hs1, h0n = cur$h0n, h1n = cur$h1n, hs0n = cur$hs0n, hs1n = cur$hs1n,
                         fh = cur$fh, fhn = cur$fhn, test = cur$test, test_user = cur$test_user, ph = cur$ph, step = cur$step, seen = cur$seen))
      meta <- if (identical(m$id, "custom")) m[intersect(c("title", "statement", "parts", "roles", "labels", "settings", "target", "control", "random_blocks", "design"), names(m))]
      sm <- tryCatch(ps_share_summary(m, sv$s, sv$test), error = function(e) NULL)
      list(app = "StatDesign Studio", kind = "analysis", version = 1, saved = format(Sys.time(), "%Y-%m-%d %H:%M"), from = name, note = note,
           page = if (is.null(mode)) "problems" else mode, problem_id = m$id, title = prob_label(m), edited = isTRUE(m$edited),
           meta = meta, data = as.data.frame(m$data, check.names = FALSE), state = sv, summary = sm)
    }
    observeEvent(input$share, {
      req(cur$meta)
      showModal(modalDialog(title = "Share this analysis", size = "m", easyClose = TRUE,
        p(class = "small", "Whoever opens it sees exactly this analysis: your data, settings and wording, the test and post-hoc you chose, and the step you are on. The results are recomputed and checked against yours."),
        div(class = "grid g2", style = "gap:12px", textInput(ns("share_name"), "Your name (optional)", width = "100%"),
            div()), textAreaInput(ns("share_note"), "Note for the lecturer (optional)", rows = 2, width = "100%"),
        div(class = "hrow", style = "gap:10px;flex-wrap:wrap", downloadButton(ns("dl_share"), label = tagList(icon("download", 15), "Download file"), icon = NULL, class = "btn-k bp"),
            tags$button(type = "button", class = "btn-k bs", onclick = sprintf("return sdsCopyShare('%s', this, 'link');", ns("share_code_box")), icon("link", 15), "Copy link"),
            tags$button(type = "button", class = "btn-k bs", onclick = sprintf("return sdsCopyShare('%s', this, 'code');", ns("share_code_box")), icon("copy", 15), "Copy code")),
        p(class = "small", style = "margin:12px 0 4px", "Send the link, the file or the code. The other person opens the link in their browser, or uses Open shared analysis (at the top) with the file or the code."),
        uiOutput(ns("share_code_ui")),
        footer = modalButton("Close")))
    })
    output$share_code_ui <- renderUI({
      code <- tryCatch(share_encode(share_state(input$share_name %||% "", input$share_note %||% "")), error = function(e) conditionMessage(e))
      tags$textarea(id = ns("share_code_box"), class = "form-control", rows = 4, readonly = NA, style = "font-family:var(--mono,monospace);font-size:11px", code)
    })
    output$dl_share <- downloadHandler(
      filename = function() sprintf("analysis_%s.sdsa", gsub("[^A-Za-z0-9]+", "_", if (is.null(mode)) cur$meta$id else paste0(mode, "_", cur$meta$id))),
      content = function(file) writeLines(as.character(jsonlite::toJSON(share_state(isolate(input$share_name) %||% "", isolate(input$share_note) %||% ""), auto_unbox = TRUE, null = "null", na = "null", digits = NA, dataframe = "columns")), file, useBytes = TRUE))
    # Open a shared analysis here: the page and the step are restored, then the numbers are checked against the student's.
    restore_share <- function(x) {
      sv <- x$state; df <- x$data; id <- x$problem_id
      keep <- c("s", "h0", "h1", "hs0", "hs1", "h0n", "h1n", "hs0n", "hs1n", "fh", "fhn", "test", "test_user", "ph", "step", "seen")
      memo_new <- sv[intersect(keep, names(sv))]; memo_new$res <- NULL; memo_new$res_np <- NULL
      memo_new$s <- Filter(Negate(is.null), as.list(memo_new$s))
      if (!is.null(memo_new$fh)) memo_new$fh <- lapply(memo_new$fh, as.list); if (!is.null(memo_new$fhn)) memo_new$fhn <- lapply(memo_new$fhn, as.list)
      step <- as.integer(sv$step %||% 1)
      if (!identical(id, "custom") && !is.null(PROBLEMS[[id]])) {
        orig <- PROBLEMS[[id]]$data
        if (isTRUE(x$edited) && identical(names(df), names(orig))) {
          for (n in names(df)) if (is.numeric(orig[[n]])) df[[n]] <- as.numeric(df[[n]]) else df[[n]] <- as.character(df[[n]])
          edits[[id]] <- df
        } else edits[[id]] <- NULL
        memo[[id]] <- memo_new; cur$id <- NULL; open_problem(id, step = step)
      } else {
        mt <- x$meta; roles <- as.list(mt$roles); for (n in names(df)) if (!identical(n, roles$response)) df[[n]] <- as.character(df[[n]])
        m <- list(id = "custom", chapter = 0, num = "Custom", title = mt$title %||% "", statement = as.character(mt$statement), parts = as.character(mt$parts %||% character(0)),
                  data = df, display = df, roles = roles, labels = as.list(mt$labels), settings = as.list(mt$settings), target = mt$target %||% "mean",
                  source = "Shared analysis", control = mt$control)
        if (!is.null(mt$random_blocks)) m$random_blocks <- isTRUE(mt$random_blocks)
        if (!is.null(mt$design)) m$design <- mt$design
        custom(m); if (!is.null(mode)) { load_data(df, roles); updateTextInput(session, "c_title", value = m$title) }
        memo[["custom"]] <- memo_new; cur$id <- NULL; open_problem("custom", step = step)
      }
      mm <- cur$meta
      now <- tryCatch(ps_share_summary(mm, cur$s, cur$test), error = function(e) NULL)
      same <- share_same(now, x$summary)
      showModal(modalDialog(title = "Shared analysis opened", easyClose = TRUE, footer = modalButton("Close"),
        p(tags$b(x$title %||% "Analysis"), if (nzchar(x$from %||% "")) sprintf(" · from %s", x$from), if (nzchar(x$saved %||% "")) sprintf(" · saved %s", x$saved)),
        if (nzchar(x$note %||% "")) div(class = "note g", icon("chat", 14), span(x$note)),
        p(sprintf("It is open at step %d, with the student's data, settings and wording.", step)),
        if (isTRUE(same)) note(kind = "g", ic = "check", "The numbers computed here are identical to those in the shared file.")
        else if (isFALSE(same)) note(kind = "w", ic = "alert", "The numbers computed here differ from those in the shared file. The file may come from another version of the app.")))
    }
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
      roles <- list(response = "Value", treatment = gname, block = if (length(keep) == 1) keep else "", row = "", column = "", greek = "")
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
      txt <- input$c_statement %||% ""; seed <- ud$ps_seed
      s <- modifyList(list(alpha = 0.05, alt = "two.sided"), seed$settings %||% list())
      det <- ps_detect_settings(txt)
      for (k in names(det)) if (is.null(seed$settings[[k]])) s[[k]] <- det[[k]]
      paras <- strsplit(txt, "\n\\s*\n")[[1]]; paras <- trimws(paras[nzchar(trimws(paras))])
      roles <- croles()
      same <- !is.null(seed) && identical(vapply(names(role_ids), function(k) seed$roles[[k]] %||% "", ""), vapply(names(role_ids), function(k) roles[[k]] %||% "", ""))
      ctrl <- input$c_control %||% ""; lv <- if (nzchar(roles$treatment)) unique(trimws(as.character(df[[roles$treatment]]))) else character(0)
      list(id = "custom", chapter = 0, num = "Custom", title = trimws(input$c_title %||% ""), statement = if (length(paras)) paras else "No statement was entered.",
           parts = character(0), data = df, display = df, roles = roles, labels = if (same) seed$labels %||% list() else list(), settings = s,
           target = if (isTRUE(input$c_variance)) "variance" else "mean", source = "Your own problem",
           control = if (nzchar(ctrl) && ctrl != NO_ROLE && ctrl %in% lv) ctrl else NULL)
    })
    # A control group is chosen only when the problem names one; Dunnett's test then compares every treatment with it.
    output$c_control_ui <- renderUI({
      df <- cdata(); trt <- croles()$treatment
      if (is.null(df) || !nzchar(trt)) return(NULL)
      lv <- unique(trimws(as.character(df[[trt]]))); lv <- lv[!is.na(lv) & nzchar(lv)]
      if (length(lv) < 3) return(NULL)
      seed <- ud$ps_seed$control
      selectInput(ns("c_control"), "Control group, if the problem names one (enables Dunnett's test)", c("(no control group)" = NO_ROLE, stats::setNames(lv, lv)),
                  if (!is.null(seed) && seed %in% lv) seed else NO_ROLE, width = "100%")
    })
    # An analysis page accepts only its own layouts: the message says what the chosen columns form instead.
    mode_wrong <- function(kind, st) {
      if (is.null(md) || is.na(kind) || kind %in% md$kinds) return(NULL)
      extra <- if (identical(mode, "two") && identical(kind, "k")) sprintf(" Your Group column has %d levels: use the k samples or CRD page.", st$ntrt %||% 0)
               else if (identical(mode, "two") && identical(kind, "rcbd")) " For paired data every pair needs exactly one observation per group."
               else if (mode %in% c("k", "crd") && identical(kind, "two")) " Two groups are compared on the Two samples page."
               else if (mode %in% c("k", "crd") && kind %in% c("rcbd", "paired")) " Set Block to (none) for a completely randomized design, or use the RCBD page."
               else if (mode == "rcbd" && identical(kind, "k")) " Choose the Block column."
               else if (mode == "lsd" && identical(kind, "glsd")) " Set the Greek letter to (none), or use the Graeco-Latin Square page."
               else if (mode == "glsd" && identical(kind, "latin")) " Choose the Greek letter column, or use the Latin Square page."
               else if (mode == "one" && !identical(kind, "one")) " Use the Two samples or k samples page for grouped data."
               else ""
      sprintf("This page analyses %s, but the columns you chose form %s. %s%s", tolower(md$title), tolower(PS_KIND_LABEL[[kind]]), md$need, extra)
    }
    output$c_check <- renderUI({
      df <- cdata()
      if (is.null(df)) return(note(kind = "i", ic = "info", "Bring in a dataset to see the data checks."))
      m <- cmeta(); st <- ps_structure(m$data, m$roles); kind <- ps_kind(st, m)
      wrong <- mode_wrong(kind, st)
      if (!is.null(wrong)) return(kcard(h3(class = "ct", "Data checks"), note(kind = "e", ic = "alert", wrong)))
      wide <- !nzchar(m$roles$treatment) && sum(vapply(df, function(x) { v <- x[!is.na(x)]; length(v) > 0 && all(!is.na(suppressWarnings(as.numeric(v)))) }, TRUE)) >= 2
      kcard(h3(class = "ct", "Data checks"),
            if (is.na(kind)) note(kind = "e", ic = "alert", ps_kind_problem(kind, st))
            else tagList(
              if (wide) note(kind = "i", ic = "info", "Several numeric columns and no Group: if each column is a sample, press Stack (under the data)."),
              div(class = "kv-row", span("Layout"), span(PS_KIND_LABEL[[kind]])),
              div(class = "kv-row", span("Observations"), span(st$n)),
              if (!is.null(st$ntrt)) div(class = "kv-row", span("Groups"), span(sprintf("%d (%s)", st$ntrt, ps_list(levels(st$d$treatment))))),
              if (!is.null(m$control)) div(class = "kv-row", span("Control group"), span(m$control)),
              if (length(st$warnings)) div(class = "col", style = "gap:8px;margin-top:8px", lapply(st$warnings, function(x) note(kind = "w", ic = "alert", x)))))
    })
    observeEvent(input$c_go, {
      df <- cdata()
      if (is.null(df) || !nrow(df)) { showNotification("Add a dataset first: paste it, upload a file or load a class example.", type = "error"); return() }
      m <- cmeta(); st <- ps_structure(m$data, m$roles); kind <- ps_kind(st, m)
      if (is.na(kind)) { showNotification(ps_kind_problem(kind, st), type = "error", duration = 10); return() }
      wrong <- mode_wrong(kind, st); if (!is.null(wrong)) { showNotification(wrong, type = "error", duration = 12); return() }
      custom(m); memo[["custom"]] <- NULL; if (identical(isolate(cur$id), "custom")) cur$id <- NULL
      open_problem("custom", step = 1)
    })

    # ---------- Datasets from Data Studio (analysis pages) ----------
    use_ds <- function(id) {
      ds <- store$datasets[[id]]; req(ds)
      load_data(drop_empty_rows(ds$data))
      if (!nzchar(isolate(input$c_title) %||% "")) updateTextInput(session, "c_title", value = ds$name)
      updateSelectInput(session, "c_ds", selected = id)
    }
    if (!is.null(mode)) {
      observe({
        dss <- store$datasets; ch <- if (length(dss)) stats::setNames(names(dss), vapply(dss, function(d) d$name, "")) else character(0)
        updateSelectInput(session, "c_ds", choices = if (length(ch)) ch else c("(no dataset yet: add one in Data Studio)" = ""), selected = isolate(store$active))
      })
      observeEvent(input$c_use_ds, { req(nzchar(input$c_ds %||% "")); use_ds(input$c_ds) })
      # the first time the page opens with a dataset already in Data Studio, use the active one
      observeEvent(store$active, { if (is.null(isolate(cdata())) && !is.null(store$active)) use_ds(store$active) }, ignoreNULL = TRUE)
    }

    # ---------- For Ask Professor Riaz ----------
    c(list(
      use_dataset = function(id) { use_ds(id); view("edit") },
      set_paired = function(p) { paired_hint(isTRUE(p)); if (!is.null(cdata())) set_roles(cdata()) },
      set_method = function(m) method_hint(m),
      get_state = function() NULL, set_state = function(x) invisible(NULL), restore = restore_share),
      list(ai = function(full) {
      i <- cur$id
      if (is.null(i) && !is.null(md)) return(list(label = sprintf("the %s page", md$title), kind = "general",
        text = if (full) sprintf("%s page: the student is choosing the dataset and what each column is. %s", md$title, md$need)))
      if (is.null(i)) return(list(label = "the Problem Sets list", kind = "general", text = if (full) "The student is browsing the Problem Sets list (Chapter 2 one- and two-sample problems, Chapter 3 CRD (one-way ANOVA) problems, Chapter 4 blocked designs: RCBD, Latin square and Graeco-Latin square, and Chapter 5 factorial designs)."))
      m <- cur$meta; n <- cur$step; r <- isolate(cur$res)
      list(label = sprintf("%s, %s", prob_label(m), ps_step_name(n, cur$kind)), kind = "general",
           text = if (full) paste0(if (!is.null(md)) sprintf("%s page, hypothesis-testing steps on the student's own data. ", md$title) else "Problem Sets page, hypothesis-testing steps. ", prob_label(m), " (", PS_KIND_LABEL[[cur$kind]], "). The student is on ",
                                   ps_step_name(n, cur$kind), ". Statement: ", paste(c(m$statement, m$parts), collapse = " "),
                                   " Data (first rows): ", paste(utils::capture.output(print(utils::head(m$data, 12), row.names = FALSE)), collapse = "\n"),
                                   if (ps_is_fact(isolate(cur$kind))) paste0(" Hypotheses by effect: ", paste(vapply(names(isolate(cur$fh)), function(cd) sprintf("%s: H1 %s", cd, isolate(cur$fh)[[cd]]$h1), ""), collapse = " | "), " alpha = ", format(isolate(cur$s$alpha)), ".")
                                   else paste0(" H0: ", isolate(cur$h0), " H1: ", isolate(cur$h1), " alpha = ", format(isolate(cur$s$alpha)), "."),
                                   if (!is.null(r) && ps_is_fact(r$kind)) paste0(" ", r$t$name, " per effect: ", paste(sprintf("%s stat = %s, %s", r$effects$code, ps_fmt(r$effects$stat, 4), ps_pe(r$effects$p)), collapse = "; "), ".")
                                   else if (!is.null(r)) { d <- ps_decision(r, isolate(cur$h1) %||% ""); paste0(" Test: ", r$t$name, ", ", r$t$stat_name, " = ", ps_fmt(r$t$stat, 4), ", ", ps_pe(r$p), ". ", d$conclusion) }
                                   else " The test has not been run yet.",
                                   { rn <- isolate(cur$res_np); if (!is.null(rn)) paste0(" ", rn$t$name, " (after a failed assumption): ", rn$t$stat_name, " = ", ps_fmt(rn$t$stat, 4), ", ", ps_pe(rn$p), ".") else "" }))
    }))
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
  list(response = resp, treatment = trt, block = blk, row = "", column = "", greek = "")
}

# First guess at the roles for an analysis page, from the columns and what the page needs.
ps_guess_roles_mode <- function(df, mode, paired = FALSE) {
  md <- PS_MODES[[mode]]; base <- ps_guess_roles(df)
  resp <- base$response; pool <- setdiff(names(df), resp)
  pool <- pool[!grepl("^column \\d+$", tolower(pool)) | length(pool) <= 2]
  out <- list(response = resp, treatment = "", factorB = "", factorC = "", factorD = "", factorE = "", block = "", row = "", column = "", greek = "")
  take <- function(pat) { h <- pool[grepl(pat, tolower(pool))]; if (length(h)) { pool <<- setdiff(pool, h[1]); h[1] } else "" }
  nextcol <- function() if (length(pool)) { x <- pool[1]; pool <<- pool[-1]; x } else ""
  switch(mode,
    one = NULL,
    two = , k = , crd = { out$treatment <- if (nzchar(base$treatment) && base$treatment %in% pool) { pool <- setdiff(pool, base$treatment); base$treatment } else nextcol()
      if (mode == "two" && paired) out$block <- if (nzchar(base$block) && base$block %in% pool) base$block else nextcol() },
    rcbd = { out$treatment <- if (nzchar(base$treatment)) base$treatment else nextcol(); pool <- setdiff(pool, out$treatment)
             out$block <- if (nzchar(base$block) && base$block != out$treatment) base$block else nextcol() },
    lsd = , glsd = { out$row <- take("^row|^lot|^batch"); out$column <- take("^col|^operator|^day"); out$treatment <- take("treat|letter|latin|method|fertili|formul|catalyst")
                     if (mode == "glsd") out$greek <- take("greek|^workplace|^catalyst")
                     for (k in c("row", "column", "treatment", if (mode == "glsd") "greek")) if (!nzchar(out[[k]])) out[[k]] <- nextcol() },
    fact = { out$treatment <- nextcol(); out$factorB <- nextcol()
             rest <- pool[!grepl("rep|block", tolower(pool))]
             if (length(rest) && length(unique(df[[rest[1]]])) <= 5) out$factorC <- ""   # a third factor is left to the student
             out$block <- take("^block") })
  out
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
