# Paper Replications page (sidebar, under Practice): the published papers the group re-created with the app.
#
# It is built like the Problem Sets: a list of cards with a design bar, a search, filters and "Group by", and for
# each paper a set of steps with a stepper and Back / Next: the paper, its data, the analysis in the app, whether
# the values tally, the insights and the files. The analysis step re-runs every saved analysis (.sdsa) on this
# version of the app and opens it in the matching analysis page, at the five steps, through the same route as
# "Open shared analysis". The page only reads the papers/ folder; it changes nothing on the other pages.

PP_STEPS <- c("The paper", "Data", "Analysis in the app", "Do the values tally?", "Insights", "Files")
PP_STEP_SUB <- c(
  "What the study set out to do, how it was designed and how the group re-created it.",
  "The data used in the app, as labelled CSV files. Open any of them in Data Studio to explore it yourself.",
  "Each analysis was saved from the app. Here it is run again on this version of the app; open it to follow the five steps.",
  "The paper's printed numbers against the app's (and an independent re-computation).",
  "What the replication shows that the paper does not say, and what could be added.",
  "Everything in the replication pack for this paper: the paper, the data, the saved analyses and the scripts.")

papers_ui <- function(id) {
  ns <- NS(id)
  dz <- c("All designs" = "all", stats::setNames(names(PAPER_DESIGNS), vapply(PAPER_DESIGNS, `[[`, "", "label")))
  tabsetPanel(id = ns("views"), type = "hidden", selected = "list",
    # ---------- The list of papers ----------
    tabPanelBody("list",
      page_head("Paper Replications",
                "Published experiments re-created with StatDesign Studio: the paper, its data, the analysis run in the app with the same five steps, and whether the printed results tally.",
                tags$a(class = "btn-k bs bsm", href = "paperfiles/StatDesign_15_papers_all_datasets.sdproj", download = "StatDesign_15_papers_all_datasets.sdproj",
                       `data-tip` = "The project file of the replication pack", icon("download", 15), "Project file"),
                btn(ns("load_all"), "Load all datasets", "database", "bp", "bsm")),
      uiOutput(ns("overview")),
      kcard(class = "ps-toolbar pp-toolbar", div(class = "hrow", style = "align-items:center",
                div(seg_input(ns("design"), dz, "all")),
                div(class = "ml ps-search", textInput(ns("q"), NULL, placeholder = "Search by author, topic, journal or finding"))),
            uiOutput(ns("design_blurb")),
            div(class = "ps-filters pp-filters",
                div(class = "ps-filter", selectInput(ns("f_tally"), "Do the values tally?", c("All verdicts" = "", stats::setNames(names(PAPER_TALLY), PAPER_TALLY)), width = "100%")),
                div(class = "ps-filter", selectInput(ns("f_field"), "Field", c("All fields" = "", stats::setNames(PAPER_FIELDS, PAPER_FIELDS)), width = "100%")),
                div(class = "ps-filter", selectInput(ns("f_sort"), "Order", c("Paper number" = "num", "Newest first" = "new", "Oldest first" = "old"), width = "100%"))),
            div(class = "ps-groupbar", span(class = "ps-gb-lab", "Group by"),
                seg_input(ns("group"), c("Design" = "design", "Do the values tally?" = "tally", "Field" = "field", "None" = "none"), "design"),
                div(class = "ml"), actionLink(ns("f_reset"), tagList(icon("x", 13), " Reset filters"), class = "small ps-link"))),
      uiOutput(ns("cards"))),
    # ---------- One paper, step by step ----------
    tabPanelBody("detail",
      uiOutput(ns("detail_head")),
      uiOutput(ns("stepper")),
      uiOutput(ns("step_body")))
  )
}

papers_server <- function(id, store, show_crumb, open_analysis, open_project, open_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    P <- papers_all()
    sel <- reactiveVal(NULL)
    step <- reactiveVal(1)
    seen <- reactiveValues()   # the steps visited, per paper (for the "Viewed" chip on the cards)

    view <- function(v, crumb) {
      updateTabsetPanel(session, "views", selected = v)
      show_crumb(crumb)
      session$sendCustomMessage("scroll_top", list())
    }
    open_paper <- function(num, at = 1) {
      if (is.null(P[[num]])) return()
      sel(num); step(at); seen[[num]] <- unique(c(seen[[num]], at))
      view("detail", c("Practice", "Paper Replications", sprintf("Paper %s", num)))
    }
    go <- function(n) {
      n <- max(1L, min(length(PP_STEPS), as.integer(n))); step(n)
      i <- sel(); if (!is.null(i)) seen[[i]] <- unique(c(seen[[i]], n))
      session$sendCustomMessage("scroll_top", list())
    }
    observeEvent(input$pick, open_paper(input$pick))
    observeEvent(input$go, go(input$go))
    observeEvent(input$back, view("list", c("Practice", "Paper Replications")))
    observeEvent(input$prev_paper, { k <- names(P); i <- match(sel(), k); if (!is.na(i) && i > 1) open_paper(k[i - 1], step()) })
    observeEvent(input$next_paper, { k <- names(P); i <- match(sel(), k); if (!is.na(i) && i < length(k)) open_paper(k[i + 1], step()) })

    # ---------- The list ----------
    output$overview <- renderUI({
      n_an <- sum(vapply(P, function(p) length(p$analyses), 0))
      ty <- vapply(P, `[[`, "", "tally")
      tile <- function(v, l, s = NULL) div(class = "pp-stat", div(class = "pp-sv", v), div(class = "pp-sl", l), if (!is.null(s)) div(class = "pp-ss", s))
      div(class = "pp-stats",
          tile(length(P), "papers", sprintf("published %d to %d", min(vapply(P, `[[`, 0, "year")), max(vapply(P, `[[`, 0, "year")))),
          tile(n_an, "saved analyses", "each re-run on this version"),
          tile(length(unique(vapply(P, `[[`, "", "design"))), "designs", "one sample, CRD, RCBD, Latin, Graeco-Latin, factorial, 2^k"),
          tile(sum(ty == "yes"), "reproduce exactly", sprintf("%d partly · %d with discrepancies", sum(ty == "partly"), sum(ty == "no"))))
    })
    output$design_blurb <- renderUI({
      d <- input$design %||% "all"
      txt <- if (d == "all") "Every paper was analysed with one of the app's design pages and the analysis saved with Share analysis. Filter by verdict or field, group the papers, and click a paper to go through it step by step."
             else sprintf("%s %s", PAPER_DESIGNS[[d]]$desc, sprintf("These papers were analysed on the %s page.", PAPER_DESIGNS[[d]]$label))
      p(class = "small", style = "margin:12px 0 0", txt)
    })
    observeEvent(input$f_reset, {
      for (n in c("f_tally", "f_field")) updateSelectInput(session, n, selected = "")
      updateSelectInput(session, "f_sort", selected = "num"); updateTextInput(session, "q", value = ""); updateRadioButtons(session, "design", selected = "all")
    })
    card <- function(p) {
      vis <- seen[[p$num]]
      tags$a(class = paste("kcard acard ps-card pp-card", if (identical(sel(), p$num)) "on"), href = "#",
             onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", ns("pick"), p$num),
             div(class = "hrow", span(class = "ps-num", paste("Paper", p$num)),
                 if (length(vis)) span(class = paste("ps-prog", if (all(seq_along(PP_STEPS) %in% vis)) "done" else "doing"),
                                       if (all(seq_along(PP_STEPS) %in% vis)) tagList(icon("check", 12, 3), "Viewed") else "In progress"),
                 div(class = "ml", pill(PAPER_TALLY[[p$tally]], PAPER_TALLY_KIND[[p$tally]], if (p$tally == "yes") "check" else if (p$tally == "no") "alert" else "info"))),
             div(class = "at", p$title),
             div(class = "ad", sprintf("%s · %s", p$cite_short, p$journal)),
             div(class = "pp-find", p$finding),
             div(class = "meta ps-chips",
                 span(class = "chip", icon(PAPER_DESIGNS[[p$design]]$icon, 12), PAPER_DESIGNS[[p$design]]$label),
                 span(class = "chip", icon("flask", 12), plural(length(p$analyses), "analysis", "analyses")),
                 span(class = "chip", icon("tag", 12), p$field)))
    }
    output$cards <- renderUI({
      d <- input$design %||% "all"; q <- tolower(trimws(input$q %||% "")); ft <- input$f_tally %||% ""; ff <- input$f_field %||% ""
      gb <- input$group %||% "design"; so <- input$f_sort %||% "num"
      ps <- P
      if (so != "num") ps <- ps[order(vapply(ps, `[[`, 0, "year"), decreasing = so == "new")]
      hit <- vapply(ps, function(p) {
        (d == "all" || p$design == d) && (!nzchar(ft) || p$tally == ft) && (!nzchar(ff) || p$field == ff) &&
          (!nzchar(q) || grepl(q, tolower(paste(p$num, p$title, p$who, p$year, p$journal, p$finding, p$field, PAPER_DESIGNS[[p$design]]$label,
                                                p$readme$cite, p$readme$did, paste(p$readme$insights, collapse = " "))), fixed = TRUE))
      }, TRUE)
      if (!any(hit)) return(kcard(div(class = "empty", ico_box("search", "io", 24), h2(class = "ct", "No papers match"), p(class = "sub", "Change a filter or the search, or choose All designs."))))
      grid_of <- function(x) div(class = "grid g3", lapply(x, card))
      count_line <- div(class = "ps-count", span(tags$b(sum(hit)), sprintf(" of %d papers", length(P))))
      if (gb == "none") return(div(class = "col ps-groups", count_line, grid_of(ps[hit])))
      key <- vapply(ps, function(p) switch(gb, design = p$design, tally = p$tally, field = p$field), "")
      ord <- switch(gb, design = names(PAPER_DESIGNS), tally = names(PAPER_TALLY), field = PAPER_FIELDS)
      groups <- intersect(ord, unique(key[hit]))
      div(class = "col ps-groups", count_line, lapply(groups, function(g) {
        x <- ps[hit & key == g]
        title <- switch(gb, design = PAPER_DESIGNS[[g]]$label, tally = PAPER_TALLY[[g]], field = g)
        desc <- switch(gb, design = PAPER_DESIGNS[[g]]$desc, tally = PAPER_TALLY_DESC[[g]], field = NULL)
        tags$details(class = "ps-group kcard", open = NA,
                     tags$summary(span(class = "ps-chev", icon("chev", 14)), span(class = "ps-gt", title), span(class = "pill neu", plural(length(x), "paper")),
                                  if (!is.null(desc)) span(class = "small ps-gd", desc)),
                     div(class = "ps-gbody", grid_of(x)))
      }))
    })
    observeEvent(input$load_all, {
      f <- file.path(PAPERS_DIR, "StatDesign_15_papers_all_datasets.sdproj")
      if (!file.exists(f)) { showNotification("The project file of the replication pack is missing from the papers folder.", type = "error"); return() }
      open_project(f)
    })

    # ---------- One paper ----------
    cur <- reactive({ i <- sel(); req(i); P[[i]] })
    output$detail_head <- renderUI({
      p <- cur(); k <- names(P); i <- match(p$num, k)
      div(class = "col pp-head", style = "gap:14px",
          page_head(p$title, sprintf("%s · %s", p$cite_short, p$journal),
                    btn(ns("back"), "All papers", "arrowl", "bs", "bsm"),
                    tags$button(class = "btn-k bs bsm", disabled = if (i <= 1) NA, `aria-label` = "Previous paper", `data-tip` = "Previous paper",
                                onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'})", ns("prev_paper")), icon("arrowl", 15)),
                    tags$button(class = "btn-k bs bsm", disabled = if (i >= length(k)) NA, `aria-label` = "Next paper", `data-tip` = "Next paper",
                                onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'})", ns("next_paper")), icon("arrowr", 15))),
          div(class = "hrow pp-tags", span(class = "ps-num", paste("Paper", p$num)),
              pill(PAPER_DESIGNS[[p$design]]$label, "inf", PAPER_DESIGNS[[p$design]]$icon),
              pill(PAPER_TALLY[[p$tally]], PAPER_TALLY_KIND[[p$tally]], if (p$tally == "yes") "check" else if (p$tally == "no") "alert" else "info"),
              pill(p$field, "neu", "tag"),
              div(class = "ml hrow", style = "gap:8px",
                  if (length(p$readme$doi)) tags$a(class = "btn-k bgh bsm", href = p$readme$doi, target = "_blank", rel = "noopener", icon("link", 14), "DOI"),
                  if (!is.na(p$pdf %||% NA)) tags$a(class = "btn-k bs bsm", href = p$pdf, target = "_blank", rel = "noopener", icon("file", 14), "Paper (PDF)")
                  else if (!is.null(p$link)) tags$a(class = "btn-k bs bsm", href = p$link, target = "_blank", rel = "noopener", icon("globe", 14), "Paper (open access)"))))
    })
    output$stepper <- renderUI({
      i <- sel(); req(i); n <- step(); vis <- seen[[i]]
      parts <- list()
      for (j in seq_along(PP_STEPS)) {
        cls <- if (j == n) "cur" else if (j %in% vis) "done" else ""
        parts[[length(parts) + 1]] <- tags$button(class = paste("st", cls), title = PP_STEPS[j],
          onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("go"), j),
          div(class = "dot", if (j != n && j %in% vis) icon("check", 12, 3) else j), span(class = "lbl", PP_STEPS[j]))
        if (j < length(PP_STEPS)) parts[[length(parts) + 1]] <- div(class = paste("sline", if (j %in% vis && (j + 1) %in% vis) "done"))
      }
      div(class = "ps-stepbar pp-stepbar", div(class = "stepper ps-stepper", parts))
    })
    nav_btn <- function(n, label, primary = FALSE, back = FALSE) tags$button(class = paste("btn-k", if (primary) "bp" else "bs"),
      onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("go"), n), if (back) icon("arrowl", 15), label, if (!back) icon("arrowr", 15))
    step_card <- function(n, ...) {
      last <- n == length(PP_STEPS); k <- names(P); i <- match(sel(), k)
      kcard(class = "ps-step",
            div(class = "ps-step-head", div(class = "ps-stepn", n), div(h2(class = "ct", PP_STEPS[n]), p(class = "sub", PP_STEP_SUB[n]))),
            div(class = "ps-step-body", ...),
            div(class = "ps-nav",
                if (n > 1) nav_btn(n - 1, sprintf("Back: %s", PP_STEPS[n - 1]), back = TRUE), div(class = "ml"),
                if (!last) nav_btn(n + 1, sprintf("Next: %s", PP_STEPS[n + 1]), primary = TRUE)
                else if (i < length(k)) tags$button(class = "btn-k bp", onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'})", ns("next_paper")),
                                                    sprintf("Next paper: %s", P[[k[i + 1]]]$cite_short), icon("arrowr", 15))
                else btn(ns("back"), "All papers", "check", "bp")))
    }
    `%|NA|%` <- function(a, b) if (is.null(a) || is.na(a)) b else a
    tbl <- function(df) if (!is.null(df) && nrow(df)) div(class = "tscroll", html_table(df))
    tally_pill <- function(v) {
      kind <- if (grepl("^yes", v, ignore.case = TRUE) && !grepl("only|except|partly", v, ignore.case = TRUE)) "pass" else if (grepl("^no", v, ignore.case = TRUE)) "fail" else if (grepl("^new", v, ignore.case = TRUE)) "inf" else "warn"
      span(class = paste("pill", kind), v)
    }

    output$step_body <- renderUI({
      p <- cur(); n <- step(); rd <- p$readme
      switch(n,
        # 1. The paper
        step_card(1, div(class = "split pp-s1",
          div(class = "col", style = "gap:16px",
              div(class = "pp-cite", icon("book", 16), div(paper_md_inline(rd$cite))),
              div(h3(class = "ct", "What the paper did"), p(class = "pp-p", paper_md_inline(rd$did))),
              div(h3(class = "ct", "Design"), p(class = "pp-p", paper_md_inline(rd$design))),
              div(h3(class = "ct", "How it was re-created in StatDesign Studio"), p(class = "pp-p", paper_md_inline(rd$how))),
              note(kind = switch(PAPER_TALLY_KIND[[p$tally]], pass = "g", fail = "e", "w"), ic = "flag", tags$b("Main finding. "), p$finding)),
          div(class = "col", style = "gap:16px",
              kcard(class = "soft", h3(class = "ct", "At a glance"),
                    div(style = "margin-top:6px",
                        div(class = "kv-row", span("Authors"), span(p$who)), div(class = "kv-row", span("Year"), span(p$year)),
                        div(class = "kv-row", span("Journal"), span(p$journal)), div(class = "kv-row", span("Field"), span(p$field)),
                        div(class = "kv-row", span("Design"), span(PAPER_DESIGNS[[p$design]]$label)),
                        div(class = "kv-row", span("Analyses saved"), span(length(p$analyses))),
                        div(class = "kv-row", span("Do the values tally?"), span(p$tally_note)),
                        div(class = "kv-row", span("Licence"), span(rd$licence)))),
              kcard(class = "soft", div(class = "hrow", ico_box(PAPER_DESIGNS[[p$design]]$icon, "ig", 18), div(h3(class = "ct", sprintf("Analysed on the %s page", PAPER_DESIGNS[[p$design]]$label)),
                    span(class = "small", "Step 3 opens each saved analysis there, at the five steps.")))))) ),
        # 2. Data
        step_card(2, {
          f <- p$files[p$files$dir == "data" & grepl("\\.csv$", p$files$name), ]
          orig <- p$files[p$files$dir == file.path("data", "source_supplement"), ]
          tagList(
            lapply(seq_len(nrow(f)), function(j) {
              df <- tryCatch(utils::read.csv(file.path(PAPERS_DIR, p$folder, f$path[j]), check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8"), error = function(e) NULL)
              if (is.null(df)) return(NULL)
              kcard(class = "pp-data",
                    div(class = "hrow", ico_box("table", "ig", 16), div(h3(class = "ct", f$name[j]), span(class = "small", sprintf("%d rows · %d columns · %s", nrow(df), ncol(df), paper_size(f$size[j])))),
                        div(class = "ml hrow", style = "gap:8px",
                            tags$a(class = "btn-k bs bsm", href = f$url[j], download = f$name[j], icon("download", 14), "CSV"),
                            tags$button(class = "btn-k bs bsm", onclick = sprintf("Shiny.setInputValue('%s', {f: '%s', t: Math.random()}, {priority: 'event'})", ns("to_studio"), f$path[j]),
                                        icon("database", 14), "Open in Data Studio"))),
                    div(class = "tscroll pp-preview ps-data", html_table(utils::head(df, 10))),
                    if (nrow(df) > 10) tags$details(class = "pp-more", tags$summary(sprintf("Show all %d rows", nrow(df))), div(class = "tscroll pp-preview ps-data", html_table(df))))
            }),
            if (nrow(orig)) note(kind = "i", ic = "info", div(tags$b("The authors' original file. "), "The labelled CSV above was prepared from it: ",
                                                           lapply(seq_len(nrow(orig)), function(j) tags$a(class = "ps-link", href = orig$url[j], download = orig$name[j], orig$name[j])))))
        }),
        # 3. Analysis in the app
        step_card(3, tagList(lapply(seq_along(p$analyses), function(j) analysis_card(p, j)))),
        # 4. Do the values tally?
        step_card(4, tagList(
          note(kind = switch(PAPER_TALLY_KIND[[p$tally]], pass = "g", fail = "e", "w"), ic = if (p$tally == "yes") "check" else "alert",
               div(tags$b(paste0(PAPER_TALLY[[p$tally]], ". ")), paste0(p$tally_note, ". ", PAPER_TALLY_DESC[[p$tally]]))),
          if (!is.null(rd$tally)) div(class = "tscroll pp-tally", tags$table(class = "kt",
            tags$thead(tags$tr(lapply(names(rd$tally), tags$th))),
            tags$tbody(lapply(seq_len(nrow(rd$tally)), function(r) tags$tr(lapply(seq_along(rd$tally), function(cc) {
              v <- rd$tally[r, cc]
              tags$td(if (cc == ncol(rd$tally)) tally_pill(v) else paper_md_inline(v))
            })))))),
          {
            vf <- p$files[p$files$dir == "analysis" & grepl("_verify_.*output\\.txt$", p$files$name), ]
            if (nrow(vf)) tags$details(class = "kcard soft pp-verify", tags$summary(icon("code", 15), tags$b("Independent check"), span(class = "small", " · the printed output of ", vf$name[1])),
              p(class = "small", "The scripts in the scripts folder recompute every analysis without the app: with numpy, pandas and scipy (papers 01 to 09) or with base R (papers 10 to 15)."),
              tags$pre(class = "pp-pre", paste(readLines(file.path(PAPERS_DIR, p$folder, vf$path[1]), warn = FALSE, encoding = "UTF-8"), collapse = "\n")))
          })),
        # 5. Insights
        step_card(5, tagList(
          div(class = "col pp-insights", style = "gap:10px", lapply(seq_along(rd$insights), function(j)
            div(class = "pp-ins", span(class = "pp-ins-n", j), div(paper_md_inline(rd$insights[j]))))),
          note(kind = "i", ic = "chat", "Ask Professor Riaz (bottom right) about any of these points: it can see the paper you are reading."))),
        # 6. Files
        step_card(6, {
          f <- p$files; dirs <- unique(f$dir); ord <- c("paper", "data", file.path("data", "source_supplement"), "analysis", "scripts", "")
          dirs <- c(intersect(ord, dirs), setdiff(dirs, ord))
          lab <- c(paper = "Paper", data = "Data (labelled CSV)", "data/source_supplement" = "The authors' original data", analysis = "Saved analyses and outputs", scripts = "Verification scripts (Python or R)")
          tagList(
            div(class = "grid g2 pp-files", lapply(dirs, function(dd) {
              ff <- f[f$dir == dd, ]
              kcard(class = "soft", h3(class = "ct", if (nzchar(dd)) (lab[dd] %|NA|% dd) else "Read me"),
                    div(style = "margin-top:6px", lapply(seq_len(nrow(ff)), function(j) div(class = "list-row",
                      icon(switch(tolower(tools::file_ext(ff$name[j])), pdf = "file", csv = "table", xlsx = "sheet", sdsa = "link", py = "code", r = "code", txt = "file", md = "book", url = "globe", "file"), 15),
                      div(style = "flex:1;min-width:0;overflow-wrap:anywhere", ff$name[j]), span(class = "small", paper_size(ff$size[j])),
                      tags$a(class = "btn-k bgh bsm", href = ff$url[j], download = ff$name[j], `aria-label` = paste("Download", ff$name[j]), icon("download", 14))))))
            })),
            note(kind = "i", ic = "info", "A saved analysis (.sdsa) also opens with Open shared analysis in the top bar. The Python checks need numpy, pandas and scipy and the R checks only base R: run the script from inside the scripts folder."))
        }))
    })
    # One saved analysis: what was asked, how the columns were used, and the result on this version of the app.
    analysis_card <- function(p, j) {
      x <- p$analyses[[j]]; ck <- paper_check(x); pg <- PAPER_DESIGNS[[p$design]]
      page_lab <- PS_MODES[[x$page]]$title %||% pg$label
      tile <- function(label, value, sub = NULL) div(class = "ps-tile", div(class = "ps-tl", label), div(class = "ps-tv", value), if (!is.null(sub)) div(class = "ps-ts", sub))
      res_ui <- if (!isTRUE(ck$ok)) note(kind = "e", ic = "alert", sprintf("This analysis could not be run on this version of the app: %s", ck$msg %||% "unknown error"))
      else {
        r <- ck$res; now <- ck$now; fact <- ps_is_fact(ck$kind)
        sig <- if (fact) sum(r$effects$p < r$alpha, na.rm = TRUE) else NA
        failed <- unlist(now$failed)
        tagList(
          div(class = "grid g4 pp-tiles",
              tile("Test", r$t$name %||% now$test, now$layout),
              if (fact) tile("Effects significant", sprintf("%d of %d", sig, nrow(r$effects)), sprintf("at α = %s", format(r$alpha)))
              else tile("P-value", ps_pe(r$p), sprintf("%s · α = %s", if (isTRUE(r$reject)) "reject H0" else "do not reject H0", format(r$alpha))),
              if (!is.null(now$assumptions_ok)) tile("Assumptions", if (isTRUE(now$assumptions_ok)) "All hold" else "Not all hold",
                                                     if (length(failed)) paste("Failed:", paste(failed, collapse = ", ")) else "normality, constant variance, independence"),
              if (!is.null(now$rank_test)) tile("Rank test", now$rank_test, if (!fact) ps_pe(now$rank_p) else sprintf("%d of %d effects significant", sum(unlist(now$rank_p) < r$alpha, na.rm = TRUE), length(unlist(now$rank_p))))
              else tile("Observations", ck$n, sprintf("%d columns", ncol(x$data)))),
          if (!is.null(r$anova_df)) tags$details(class = "pp-anova", open = if (j == 1) NA,
            tags$summary(icon("table", 14), tags$b("ANOVA table"), span(class = "small", " · computed now by the app")), tbl(r$anova_df)))
      }
      kcard(class = "pp-an",
        div(class = "hrow", style = "align-items:flex-start",
            div(style = "flex:1;min-width:240px", span(class = "pp-an-n", sprintf("Analysis %d of %d", j, length(p$analyses))), h3(class = "ct", x$title %||% x$key),
                span(class = "small", sprintf("%s page · saved %s", page_lab, x$saved %||% ""))),
            if (isTRUE(ck$same)) pill("Identical to the saved analysis", "pass", "check")
            else if (isFALSE(ck$same)) pill("Differs from the saved analysis", "warn", "alert")),
        if (nzchar(x$note %||% "")) div(class = "note i pp-an-note", icon("chat", 14), span(tags$b("Saved note. "), x$note)),
        div(class = "split pp-an-body",
            div(class = "col", style = "gap:12px",
                div(h3(class = "ct", "The question"), div(class = "ps-statement pp-p", lapply(as.character(x$meta$statement), p))),
                res_ui),
            div(class = "col", style = "gap:12px",
                div(h3(class = "ct", "What each column is"), tbl(paper_roles(x))),
                div(class = "kv-row", span("Significance level"), span(format(x$state$s$alpha %||% 0.05))),
                div(class = "kv-row", span("Post-hoc"), span(ucfirst(x$state$ph %||% "tukey"))))),
        div(class = "hrow pp-an-act",
            tags$button(class = "btn-k bp", onclick = sprintf("Shiny.setInputValue('%s', {p: '%s', j: %d, t: Math.random()}, {priority: 'event'})", ns("open_an"), p$num, j),
                        icon("play", 15), "Open in the 5 steps"),
            tags$a(class = "btn-k bs", href = p$files$url[match(file.path("analysis", basename(x$file)), p$files$path)], download = basename(x$file), icon("download", 15), "Saved analysis (.sdsa)"),
            span(class = "small ml", sprintf("Opens the %s page with the paper's data, columns and settings.", page_lab))))
    }
    observeEvent(input$open_an, {
      o <- input$open_an; p <- P[[o$p]]; req(p); x <- p$analyses[[as.integer(o$j)]]; req(x)
      open_analysis(x, p)
    })
    observeEvent(input$to_studio, {
      o <- input$to_studio; p <- cur(); req(o$f %in% p$files$path)
      df <- tryCatch(utils::read.csv(file.path(PAPERS_DIR, p$folder, o$f), check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8"), error = function(e) NULL)
      if (is.null(df)) { showNotification("The file could not be read.", type = "error"); return() }
      open_data(sprintf("%s %s – %s", p$num, p$cite_short, sub("\\.csv$", "", basename(o$f))), df)
    })

    # ---------- For Ask Professor Riaz ----------
    list(
      open = function(num, at = 1) open_paper(num, at),
      ai = function(full) {
        i <- sel()
        if (is.null(i) || !identical(isolate(input$views), "detail"))
          return(list(label = "the Paper Replications list", kind = "general", text = if (full) paste0(
            "The student is browsing the Paper Replications: published experiments the group re-created with StatDesign Studio. Papers: ",
            paste(vapply(P, function(p) sprintf("%s %s, %s (%s design; %s: %s)", p$num, p$cite_short, p$title, PAPER_DESIGNS[[p$design]]$label, PAPER_TALLY[[p$tally]], p$finding), ""), collapse = " | "), ".")))
        p <- P[[i]]; rd <- p$readme; n <- isolate(step())
        list(label = sprintf("Paper %s (%s), %s", p$num, p$cite_short, PP_STEPS[n]), kind = "general",
             text = if (full) paste0("Paper Replications page, paper ", p$num, ": ", rd$cite, " Design: ", rd$design, " What the paper did: ", rd$did,
                                     " How it was replicated in the app: ", rd$how, " Verdict: ", PAPER_TALLY[[p$tally]], " (", p$tally_note, "). Main finding: ", p$finding,
                                     if (!is.null(rd$tally)) paste0(" Tally table: ", paste(apply(rd$tally, 1, paste, collapse = " ; "), collapse = " || "), "."),
                                     " Insights: ", paste(rd$insights, collapse = " "), " The student is on the step '", PP_STEPS[n], "'."))
      })
  })
}
