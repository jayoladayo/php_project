# Test Advisor: a few questions, then the analysis that fits, with the reasons.

mini_grid <- function(cells, ncol = 3) {
  cols <- c(A = "#008540", B = "#003E51", C = "#AA8A00", D = "#5FB98A")
  div(class = "adv-grid", style = sprintf("grid-template-columns:repeat(%d, 22px)", ncol),
      lapply(cells, function(x) span(style = sprintf("background:%s", cols[[substr(x, 1, 1)]]), if (nchar(x) > 0 && !grepl("^[A-D]_$", x)) sub("_", "", x))))
}

ADV_Q <- list(
  goal = list(title = "What do you want to find out?", sub = "Pick the closest match.",
              why = "The goal decides the family of methods: describing data, testing against a target, or comparing groups.",
              opts = list(
                list(v = "describe", t = "Describe my data", d = "Averages, spread, percentiles, charts.", ic = "bar"),
                list(v = "relation", t = "See how two measurements relate", d = "Scatter plot, covariance, correlation.", ic = "trend"),
                list(v = "target", t = "Compare with a target value", d = "Is the mean yield different from 50 kg?", ic = "target"),
                list(v = "groups", t = "Compare existing groups", d = "Two or more groups you observed, e.g. machines or suppliers.", ic = "columns"),
                list(v = "experiment", t = "Compare treatments in an experiment", d = "You assigned treatments to units, e.g. fertilizers to plots.", ic = "flask"))),
  count = list(title = "How many groups or treatments are you comparing?", sub = "Count the levels of the factor you care about.",
               why = "Two groups use a t-test or Mann-Whitney; three or more need ANOVA or Kruskal-Wallis to control the error rate.",
               opts = list(list(v = "2", t = "Two", d = "e.g. Fertilizer A vs B", ic = "columns"),
                           list(v = "3", t = "Three or more", d = "e.g. Fertilizers A, B and C", ic = "layers"))),
  paired = list(title = "Was each unit measured under both conditions?", sub = "For example the same plot, person or batch measured twice.",
                why = "Paired data are compared through their differences, which removes unit-to-unit variation and gives more power.",
                opts = list(list(v = "no", t = "No, different units", d = "Independent samples", ic = "columns"),
                            list(v = "yes", t = "Yes, the same units", d = "Paired samples", ic = "link"))),
  grouping = list(title = "How were your experimental units grouped?",
                  sub = "Grouping (blocking) removes nuisance variation such as farm, day, machine or batch, so treatment differences are easier to detect.",
                  why = "The grouping decides the model. Ignoring a block puts its variation into the error term and can hide a real treatment effect.",
                  opts = list(
                    list(v = "none", t = "No grouping", d = "Treatments were assigned completely at random to all units. → CRD",
                         vis = mini_grid(c("A", "C", "C", "A", "B", "C", "B", "B", "C", "A", "A", "B"), 4)),
                    list(v = "one", t = "One grouping factor", d = "Every treatment appears once in each block (farm, day, batch). → RCBD",
                         vis = mini_grid(c("A", "B", "C", "C", "A", "B", "B", "C", "A"))),
                    list(v = "rowcol", t = "Rows and columns", d = "Two nuisance factors; each treatment once per row and column. → Latin Square",
                         vis = mini_grid(c("A", "B", "C", "B", "C", "A", "C", "A", "B"))),
                    list(v = "greek", t = "Rows, columns and a Greek factor", d = "Three nuisance factors in a square. → Graeco-Latin Square",
                         vis = mini_grid(c("Aα", "Bβ", "Cγ", "Bγ", "Cα", "Aβ", "Cβ", "Aγ", "Bα"))))),
  response = list(title = "What kind of response did you measure?", sub = "The response is the outcome you compare.",
                  why = "Parametric tests need a numeric response with roughly normal errors. Ranks and scores call for rank-based tests.",
                  opts = list(list(v = "numeric", t = "A numeric measurement", d = "Yield, weight, time, strength", ic = "bar"),
                              list(v = "ordinal", t = "Ranks or scores", d = "Ratings 1 to 5, rankings, or very skewed values", ic = "rows")))
)
ADV_LABELS <- list(goal = "Goal", count = "Number of groups", paired = "Same units measured twice?", grouping = "Grouping", response = "Response")

adv_path <- function(a) {
  p <- "goal"; g <- a$goal
  if (is.null(g) || g %in% c("describe", "relation")) return(p)
  if (g == "target") return(c(p, "response"))
  if (g == "groups") return(c(p, "count", if (identical(a$count, "2")) "paired", "response"))
  c(p, "count", "grouping", "response")
}

adv_answer_text <- function(k, v) {
  o <- Filter(function(x) x$v == v, ADV_Q[[k]]$opts); if (length(o)) o[[1]]$t else v
}

adv_recommend <- function(a) {
  ord <- identical(a$response, "ordinal")
  np_line <- function(np) if (ord) c("Main test", np) else c("If assumptions fail", np)
  base <- list(method = if (ord) "nonpar" else NULL)
  r <- NULL
  if (a$goal %in% c("describe", "relation", "target")) r <- switch(a$goal,
    describe = list(page = "desc", name = "Descriptive statistics", icon = "bar", sub = "Summaries and charts for any column, overall or by group.",
                    why = c("You want to describe the data rather than test a hypothesis.", "Start here before any test: look for outliers, skewness and data errors."),
                    run = list(c("Numerical", "Mean, median, mode, quartiles, percentiles, variance, SD, CV, skewness, kurtosis"),
                               c("Graphical", "Histogram with normal curve, box plot, QQ plot, stem-and-leaf")),
                    also = list(list(page = "advisor", name = "Then test", desc = "Come back to the advisor when you are ready to compare."))),
    relation = list(page = "desc", name = "Correlation (in Descriptive statistics)", icon = "trend", sub = "Scatter plot, covariance and three correlation coefficients.",
                    why = c("You want to know how two numeric measurements move together.", "Pearson measures straight-line association; Spearman and Kendall work on ranks."),
                    run = list(c("Chart", "Scatter plot with a fitted line, correlation heat map"),
                               c("Parametric", "Pearson correlation with its p-value"), c("Non-parametric", "Spearman and Kendall correlations"))),
    target = list(page = "one", name = "One-sample test", icon = "target", sub = "Compare one sample with a target value.",
                  why = c("You compare a single sample against a fixed value.", if (ord) "Your response is ranks or scores, so the Wilcoxon signed-rank test is the main result." else "The t-test is used when the data look normal; otherwise the Wilcoxon signed-rank test."),
                  run = list(c("Main test", if (ord) "Wilcoxon signed-rank" else "One-sample t-test (z-test if σ is known)"), c("Assumption", "Shapiro-Wilk normality test, QQ plot"),
                             np_line("Wilcoxon signed-rank, sign test"))))
  if (a$goal %in% c("groups", "experiment")) {
    two <- identical(a$count, "2"); g <- a$grouping %||% "none"
    if (a$goal == "groups" && two && identical(a$paired, "yes")) {
      r <- list(page = "two", name = "Two paired samples", icon = "link", sub = "Compare two conditions measured on the same units.", paired = TRUE,
                why = c("Each unit was measured under both conditions, so the samples are paired.", "Working on the differences removes unit-to-unit variation."),
                run = list(c("Main test", if (ord) "Wilcoxon signed-rank (paired)" else "Paired t-test"), c("Assumption", "Shapiro-Wilk on the differences"), np_line("Wilcoxon signed-rank, sign test")))
    } else if (two && (a$goal == "groups" || g == "none")) {
      r <- list(page = "two", name = "Two independent samples", icon = "columns", sub = "Compare two separate groups of units.", paired = FALSE,
                why = c("You are comparing two groups made of different units.", "The F test and Levene check equal variances to choose between the pooled and Welch t-tests."),
                run = list(c("Main test", if (ord) "Mann-Whitney U" else "Pooled t-test, or Welch if variances differ"), c("Assumptions", "Shapiro-Wilk per group; F test and Levene for variances"),
                           np_line("Mann-Whitney U (Wilcoxon rank-sum)")),
                also = if (a$goal == "experiment") list(list(page = "crd", name = "CRD", desc = "The same comparison as a one-factor ANOVA with post-hoc tests.")) else list())
    } else if (a$goal == "groups") {
      r <- list(page = "k", name = "k independent samples", icon = "layers", sub = "Compare three or more groups.",
                why = c("You are comparing three or more groups of different units.", "Levene decides between the one-way ANOVA and the Welch ANOVA."),
                run = list(c("Main test", if (ord) "Kruskal-Wallis" else "One-way ANOVA, or Welch ANOVA if variances differ"), c("Assumptions", "Shapiro-Wilk on residuals, Levene and Bartlett"),
                           np_line("Kruskal-Wallis with Dunn post-hoc"), c("Post-hoc", "Tukey HSD, Games-Howell or Dunn")),
                also = list(list(page = "crd", name = "CRD", desc = "If you assigned the groups at random, the CRD page adds more post-hoc methods and the R code.")))
    } else {
      d <- switch(g, none = DESIGNS$CRD, one = DESIGNS$RCBD, rowcol = DESIGNS$LSD, greek = DESIGNS$GLSD)
      model <- switch(g, none = "One-way ANOVA (Response ~ Treatment)", one = "Two-way ANOVA (Response ~ Treatment + Block)",
                      rowcol = "Latin square ANOVA (Response ~ Treatment + Row + Column)", greek = "Graeco-Latin ANOVA (Response ~ Treatment + Row + Column + Greek)")
      r <- list(page = d$page, name = d$name, icon = d$icon, sub = sprintf("Compare %s treatments%s.", if (two) "two" else "three or more",
                                                                           switch(g, none = " assigned completely at random", one = " while removing block-to-block differences",
                                                                                  rowcol = " while removing row and column differences", greek = " with a second treatment factor in a square")),
                why = c(sprintf("You are comparing %s treatments on a %s response.", if (two) "two" else "three or more", if (ord) "ranked" else "numeric"),
                        switch(g, none = "Units were not grouped, so a completely randomised design fits.",
                               one = "Units were grouped by one nuisance factor and every treatment appears in every block.",
                               rowcol = "Two nuisance factors are arranged as the rows and columns of a square.",
                               greek = "Three factors besides the treatment are arranged in a square."),
                        "Treatment and blocks do not interact in this model, which matches a design with one unit per treatment per block."),
                run = list(c("Main test", model), c("Assumptions", "Shapiro-Wilk, Breusch-Pagan, Durbin-Watson + 3 plots"),
                           c(if (ord) "Non-parametric" else "If assumptions fail", d$nonpar), c("Post-hoc", "Tukey HSD by default; LSD, Bonferroni, Scheffé, Dunnett")),
                also = switch(g,
                  none = list(list(page = "k", name = "k samples", desc = "The same test for groups you observed rather than assigned."),
                              list(page = "rcbd", name = "RCBD", desc = "If units fall into natural groups such as farms or days.")),
                  one = c(list(list(page = "crd", name = "CRD", desc = "If blocks are very similar, blocking adds little.")),
                          if (two) list(list(page = "two", name = "Paired t-test", desc = "With two treatments, an RCBD is the same as a paired comparison.", paired = TRUE)),
                          list(list(page = "lsd", name = "Latin Square", desc = "If plots also vary in a second direction (row and column)."))),
                  rowcol = list(list(page = "rcbd", name = "RCBD", desc = "If only one direction of variation matters.")),
                  greek = list(list(page = "lsd", name = "Latin Square", desc = "If there is no Greek factor."))))
    }
  }
  r <- r %||% list()
  out <- modifyList(base, r[setdiff(names(r), "also")])
  out$also <- r$also %||% list()
  out
}

advisor_ui <- function(id) {
  ns <- NS(id)
  tagList(page_head("Test Advisor", uiOutput(ns("subtitle"), inline = TRUE), uiOutput(ns("head_btn"), inline = TRUE)),
          uiOutput(ns("body"), class = "rcol"))
}

advisor_server <- function(id, goto, set_method) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    a <- reactiveVal(list()); show_rec <- reactiveVal(FALSE)
    path <- reactive(adv_path(a()))
    current <- reactive({ p <- path(); un <- p[!p %in% names(a())]; if (length(un)) un[1] else NULL })
    output$subtitle <- renderUI(span(if (show_rec()) "Here is the analysis that fits your experiment." else "Answer a few questions and we will open the right analysis with sensible settings."))
    output$head_btn <- renderUI(if (show_rec()) btn(ns("change"), "Change answers", "arrowl", size = "bsm")
                                else if (length(a())) btn(ns("restart"), "Start again", "x", size = "bsm"))
    observeEvent(input$restart, { a(list()); show_rec(FALSE) })
    observeEvent(input$change, { show_rec(FALSE); p <- path(); x <- a(); x[[p[length(p)]]] <- NULL; a(x) })
    observeEvent(input$pick, {
      k <- current(); req(k); x <- a()
      # Later answers depend on earlier ones, so a changed answer clears what follows it.
      p <- path(); x[p[seq_along(p) > match(k, p)]] <- NULL
      x[[k]] <- input$pick$v; a(x)
      if (is.null(current())) show_rec(TRUE)
    })
    observeEvent(input$back, { p <- path(); i <- match(current(), p); req(i > 1); x <- a(); x[[p[i - 1]]] <- NULL; a(x) })

    answers_card <- function(title = "Your answers so far") {
      x <- a()
      kcard(h3(class = "ct", style = "font-size:14px", title),
            if (!length(x)) p(class = "small", style = "margin:8px 0 0", "Nothing yet.")
            else lapply(names(x), function(k) div(class = "kv-row", style = "flex-direction:column;align-items:flex-start;gap:2px",
                                                  span(class = "small", ADV_LABELS[[k]]), tags$b(style = "color:var(--ink);font-size:13.5px", adv_answer_text(k, x[[k]])))))
    }

    output$body <- renderUI({
      if (show_rec()) return(rec_ui())
      k <- current(); req(k); q <- ADV_Q[[k]]; p <- path(); i <- match(k, p)
      total <- max(length(p), if (is.null(a()$goal)) 3 else length(p))
      div(class = "split wide",
          kcard(div(class = "col", style = "gap:18px",
                    div(class = "hrow", span(style = "color:var(--green);font-weight:600;font-size:13px", sprintf("Question %d of %d", i, total)),
                        div(class = "adv-prog", lapply(seq_len(total), function(j) span(class = if (j <= i) "on")))),
                    div(h2(class = "ct", style = "font-size:22px", q$title), p(class = "sub", style = "margin-top:6px", q$sub)),
                    div(class = paste("adv-opts", if (length(q$opts) == 4) "four"),
                        lapply(q$opts, function(o) {
                          on <- identical(a()[[k]], o$v)
                          tags$button(class = paste("adv-opt", if (on) "on"),
                                      onclick = sprintf("Shiny.setInputValue('%s', {v: '%s', t: Date.now()}, {priority: 'event'})", ns("pick"), o$v),
                                      div(class = "hrow", style = "flex-wrap:nowrap;align-items:flex-start", span(class = "adv-radio"), span(class = "adv-t", o$t)),
                                      if (!is.null(o$vis)) o$vis else if (!is.null(o$ic)) div(class = "ico ig", style = "margin:4px 0", icon(o$ic, 18)),
                                      div(class = "small", o$d))
                        })),
                    div(class = "hrow", style = "border-top:1px solid var(--line);padding-top:16px",
                        if (i > 1) btn(ns("back"), "Back", "arrowl"),
                        span(class = "small ml", "Click an answer to continue. You can change any answer later.")))),
          div(class = "col",
              div(class = "note i", icon("info", 16), div(tags$b("Why we ask"), br(), q$why)),
              answers_card()))
    })

    rec <- reactive(adv_recommend(a()))
    rec_ui <- function() {
      r <- rec(); req(r$page)
      div(class = "split wide",
          div(class = "col",
              kcard(div(class = "col", style = "gap:0",
                        div(class = "hrow", style = "flex-wrap:nowrap;align-items:flex-start;padding-bottom:18px",
                            div(class = "ico ig", style = "width:56px;height:56px;border-radius:14px", icon(r$icon, 26)),
                            div(style = "flex:1;min-width:0", pill("Recommended", "gold", "spark"),
                                h2(class = "ct", style = "font-size:24px;margin-top:8px", r$name), p(class = "sub", style = "margin:4px 0 14px", r$sub),
                                btn(ns("open"), sprintf("Open %s", sub(" \\(.*\\)$", "", r$name)), "arrowr", "bp"))),
                        div(class = "grid g2", style = "border-top:1px solid var(--line);padding-top:18px;align-items:start",
                            div(h3(class = "ct", style = "font-size:14px", "Why this fits"),
                                lapply(r$why, function(w) div(class = "hrow", style = "flex-wrap:nowrap;align-items:flex-start;margin-top:10px;gap:10px",
                                                              span(style = "color:var(--green)", icon("check", 16, 2.5)), span(style = "font-size:13.5px", w))),
                                if (!is.null(r$method)) div(style = "margin-top:12px", note(kind = "w", ic = "alert", "Your response is ranks or scores, so the rank-based test is set as the result to report."))),
                            div(h3(class = "ct", style = "font-size:14px", "What the app will run"),
                                tags$table(class = "kt", style = "margin-top:8px", tags$thead(tags$tr(tags$th("Step"), tags$th("Method"))),
                                           tags$tbody(lapply(r$run, function(x) tags$tr(tags$td(x[1]), tags$td(span(style = "color:var(--ink);font-weight:500", x[2]))))))))) ),
              if (length(r$also)) kcard(h3(class = "ct", style = "font-size:14px", "Also consider"),
                                        div(class = "grid g2", style = "margin-top:10px",
                                            lapply(seq_along(r$also), function(j) { o <- r$also[[j]]
                                              tags$button(class = "adv-alt", onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("alt"), j),
                                                          tags$b(o$name), div(class = "small", o$desc)) })))),
          div(class = "col", answers_card("Your answers"),
              note(kind = "g", ic = "book", tags$b("Tip"), br(), "Bring your data into Data Studio first; the analysis page will ask you to choose the dataset.")))
    }
    open_page <- function(o) {
      goto(list(page = o$page, paired = o$paired, t = Sys.time()))
      if (o$page %in% c("one", "two", "k") && !is.null(rec()$method)) set_method(o$page, rec()$method)
    }
    observeEvent(input$open, open_page(rec()))
    observeEvent(input$alt, { o <- rec()$also[[input$alt]]; if (identical(o$page, "advisor")) { a(list()); show_rec(FALSE) } else open_page(o) })
  })
}
