# StatDesign Studio: a no-code app for the KFUPM Experimental Design course.

library(shiny)
library(bslib)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, local = TRUE)

# Private settings such as the built-in AI key live in .Renviron next to this file (never committed).
local({
  f <- riaz_load_env(".")
  b <- riaz_builtin()
  message(if (!is.null(b)) sprintf("Ask Professor Riaz: built-in key loaded from %s (%s, model %s). Students need no key.", basename(f %||% "the environment"), AI_PROVIDERS[[b$provider]]$name, b$model)
          else if (is.null(f)) sprintf("Ask Professor Riaz: no built-in key. To add one, put a file named ai_key.txt holding the key in %s and restart.", normalizePath("."))
          else sprintf("Ask Professor Riaz: %s was found but RIAZ_API_KEY is empty or still the placeholder, so students must enter their own key.", basename(f)))
})

options(shiny.maxRequestSize = 50 * 1024^2)

# Classify every problem once at start-up (problem type, test, post-hoc) so the Problem Sets
# categories and filters open instantly.
invisible(lapply(PROBLEMS, ps_classify))

PAGES <- c("home", "advisor", "data", "desc", "one", "two", "k", "problems", "crd", "rcbd", "lsd", "glsd", "planner", "report", "learn", "team")

JUMP_CHOICES <- c(
  "Home" = "home", "Test Advisor" = "advisor", "Data Studio: upload a file" = "data",
  "Descriptive statistics" = "desc", "One sample tests" = "one", "Two sample tests (t-test, Mann-Whitney)" = "two",
  "k samples (one-way ANOVA, Kruskal-Wallis)" = "k",
  "Problem Sets: Chapter 2, 3 and 4 exercises, custom problems" = "problems",
  "CRD: Completely Randomized Design" = "crd", "RCBD: Randomized Complete Block Design" = "rcbd",
  "Latin Square Design" = "lsd", "Graeco-Latin Square Design" = "glsd",
  "Design Planner: randomised layouts" = "planner", "Report builder" = "report", "Learn: method guides" = "learn",
  "About the team (Group 4), aim and objectives" = "team")

ui <- page(
  theme = bs_theme(version = 5, primary = "#008540", secondary = "#003E51", success = "#008540",
                   warning = "#AA8A00", danger = "#A42A1F", info = "#003E51",
                   base_font = font_collection("Poppins", "system-ui", "sans-serif"),
                   code_font = font_collection("Roboto Mono", "ui-monospace", "monospace"),
                   "border-radius" = "10px"),
  title = "StatDesign Studio",
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;500;600;700&family=Roboto+Mono:wght@400;500&display=swap"),
    tags$link(rel = "stylesheet", href = "app.css"),
    tags$link(rel = "icon", href = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24'%3E%3Crect width='24' height='24' rx='6' fill='%23DAC961'/%3E%3Cpath d='M12 18V10M17 18V6M7 18v-4' stroke='%2300573F' stroke-width='2.4' stroke-linecap='round'/%3E%3C/svg%3E"),
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js", defer = NA),
    tags$script(src = "app.js")
  ),
  div(class = "app",
      sidebar_ui(),
      div(class = "main",
          topbar_ui(),
          div(class = "content",
              tabsetPanel(
                id = "pages", type = "hidden",
                tabPanelBody("home", home_ui()),
                tabPanelBody("advisor", advisor_ui("advisor")),
                tabPanelBody("data", data_studio_ui("data")),
                tabPanelBody("desc", desc_ui("desc")),
                tabPanelBody("one", cmp_ui("one", "one")),
                tabPanelBody("two", cmp_ui("two", "two")),
                tabPanelBody("k", cmp_ui("k", "k")),
                tabPanelBody("problems", problems_ui("problems")),
                tabPanelBody("crd", doe_ui("crd", DESIGNS$CRD)),
                tabPanelBody("rcbd", doe_ui("rcbd", DESIGNS$RCBD)),
                tabPanelBody("lsd", doe_ui("lsd", DESIGNS$LSD)),
                tabPanelBody("glsd", doe_ui("glsd", DESIGNS$GLSD)),
                tabPanelBody("planner", planner_ui("planner")),
                tabPanelBody("report", report_ui("report")),
                tabPanelBody("learn", learn_ui("learn")),
                tabPanelBody("team", team_ui())
              ))),
      riaz_ui("riaz"))
)

server <- function(input, output, session) {
  store <- reactiveValues(datasets = list(), active = NULL, grid_rev = 0, ds_tab = NULL, pins = list(), report_src = list(), pin_seq = 0)
  goto <- reactiveVal(NULL)

  # ---------- Navigation ----------
  show_page <- function(page) {
    if (!page %in% PAGES) return()
    updateTabsetPanel(session, "pages", selected = page)
    session$sendCustomMessage("nav", list(page = page, crumb = PAGE_CRUMB[[page]]))
  }
  observeEvent(input$nav, show_page(input$nav))
  updateSelectizeInput(session, "jump", choices = JUMP_CHOICES, selected = "", server = FALSE)
  observeEvent(input$jump, { if (nzchar(input$jump)) { show_page(input$jump); updateSelectizeInput(session, "jump", selected = "") } })

  open_project <- function(path) {
    p <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(p) || !identical(p$app, "StatDesign Studio") || is.null(p$datasets)) {
      showNotification("That file is not a StatDesign Studio project.", type = "error"); return()
    }
    n <- 0; last <- NULL
    for (d in p$datasets) {
      cols <- unlist(d$columns)
      df <- as.data.frame(setNames(lapply(d$data, json_chr), cols), stringsAsFactors = FALSE, check.names = FALSE)
      ds <- list(id = d$id, name = d$name, data = df, types = setNames(unlist(d$types), cols),
                 source = d$source %||% "Project", sheet = d$sheet, file = d$file,
                 notes = unlist(d$notes) %||% character(0), ignored = unlist(d$ignored), created = Sys.time())
      store$datasets[[ds$id]] <- ds; n <- n + 1; last <- ds$id
    }
    if (n) { store$active <- last; store$grid_rev <- store$grid_rev + 1 }
    for (k in names(doe)) if (!is.null(p$analyses[[k]])) doe[[k]]$set_state(json_state(p$analyses[[k]]))
    showNotification(sprintf("Opened project with %d dataset%s.", n, if (n == 1) "" else "s"), type = "message")
  }

  data_studio_server("data", store, goto, open_project)
  doe <- list(
    crd = doe_server("crd", DESIGNS$CRD, store, goto),
    rcbd = doe_server("rcbd", DESIGNS$RCBD, store, goto),
    lsd = doe_server("lsd", DESIGNS$LSD, store, goto),
    glsd = doe_server("glsd", DESIGNS$GLSD, store, goto)
  )

  cmp <- list(one = cmp_server("one", "one", store, goto), two = cmp_server("two", "two", store, goto), k = cmp_server("k", "k", store, goto))

  desc <- desc_server("desc", store)
  advisor_server("advisor", goto, function(page, m) cmp[[page]]$set_method(m))

  problems <- problems_server("problems", store, show_crumb = function(cr) session$sendCustomMessage("crumb", list(crumb = cr)),
    open_design = function(df, title) {
      ds <- new_dataset(title, df, source = "Problem Sets")
      store$datasets[[ds$id]] <- ds; store$active <- ds$id; store$grid_rev <- store$grid_rev + 1
      goto(list(page = "rcbd", dataset = ds$id, t = Sys.time()))
    })
  planner <- planner_server("planner")
  report_server("report", store)
  add_example <- function(ex) {
    ds <- example_dataset(ex); store$datasets[[ds$id]] <- ds; store$active <- ds$id; store$grid_rev <- store$grid_rev + 1; ds$id
  }
  learn <- learn_server("learn", show = function(k) session$sendCustomMessage("crumb", list(crumb = c("Tools", "Learn", LEARN[[k]]$nav))),
    open_example = function(o) {
      id <- if (!is.null(o$ex)) add_example(o$ex) else {
        d <- CLASS_EXAMPLES$crd$data
        ds <- if (identical(o$data, "two")) new_dataset("Fertilizer A vs B (class data)", d[d$Fertilizer %in% c("A", "B"), ], source = "Class example")
              else new_dataset("Fertilizer yields (class data)", d, source = "Class example")
        store$datasets[[ds$id]] <- ds; store$active <- ds$id; store$grid_rev <- store$grid_rev + 1; ds$id
      }
      goto(list(page = o$page, dataset = id, t = Sys.time()))
    })
  observe(session$sendCustomMessage("badge", list(id = "report-count", n = length(store$pins))))

  # Ask Professor Riaz: each page tells the assistant what it is showing.
  ai_fns <- c(lapply(doe, `[[`, "ai"), lapply(cmp, `[[`, "ai"),
              list(desc = desc$ai, planner = planner$ai, learn = learn$ai, problems = problems$ai,
                   report = function(full) {
                     pins <- store$pins
                     list(label = sprintf("your report (%d item%s)", length(pins), if (length(pins) == 1) "" else "s"), kind = "general",
                          text = if (full) paste0("Report builder. Pinned items in order: ",
                                                  if (length(pins)) paste(vapply(pins, function(p) p$title %||% p$kind %||% "item", ""), collapse = "; ") else "none yet", "."))
                   }))
  riaz_server("riaz", store, reactive(input$pages), ai_fns)

  observeEvent(goto(), {
    g <- goto()
    if (!is.null(g$dataset) && g$page %in% names(doe)) doe[[g$page]]$use_dataset(g$dataset)
    if (!is.null(g$dataset) && g$page %in% names(cmp)) cmp[[g$page]]$use_dataset(g$dataset)
    if (!is.null(g$paired) && g$page == "two") cmp$two$set_paired(g$paired)
    show_page(g$page)
  })

  # Sidebar badge with the number of datasets.
  observe(session$sendCustomMessage("badge", list(id = "data-count", n = length(store$datasets))))

  # Home
  output$home_ds <- renderUI({
    dss <- store$datasets
    if (!length(dss)) return(p(class = "small", style = "margin:8px 0 0", "Nothing yet. Upload a file or open a class example to begin."))
    tagList(lapply(rev(dss), function(d) div(class = "list-row", icon("table", 16),
      div(style = "flex:1;min-width:0", div(style = "font-weight:600", d$name),
          div(class = "small", sprintf("%d rows · %d columns · %s", nrow(drop_empty_rows(d$data)), ncol(d$data), d$source))))))
  })
  observeEvent(input$home_ex, {
    k <- input$home_ex
    ds <- example_dataset(k)
    store$datasets[[ds$id]] <- ds; store$active <- ds$id; store$grid_rev <- store$grid_rev + 1
    goto(list(page = DESIGNS[[CLASS_EXAMPLES[[k]]$design]]$page, dataset = ds$id, t = Sys.time()))
  })

  # Help
  observeEvent(input$help, showModal(modalDialog(
    title = "About StatDesign Studio", easyClose = TRUE, footer = modalButton("Close"),
    p("A no-code tool for the KFUPM Experimental Design course. Bring your data into Data Studio, choose a design, map your columns, and run the analysis."),
    tags$ol(class = "small", style = "padding-left:18px",
            tags$li("Data Studio: upload Excel or CSV, pick the sheet, or type and paste data."),
            tags$li("Choose an analysis from the sidebar."),
            tags$li("Map variables: say which column is the response, the treatment and the blocks."),
            tags$li("Context and settings: name things, label coded levels, choose α and post-hoc methods."),
            tags$li("Results: ANOVA, assumption checks, post-hoc and non-parametric tests."),
            tags$li("Not sure which test? The Test Advisor asks a few questions and opens the right page.")),
    p(class = "small", "Save project downloads a file with your datasets and settings. Open it later from Data Studio, Open project.")
  )))

  # Save project
  output$save_project <- downloadHandler(
    filename = function() sprintf("statdesign-project-%s.sdproj", format(Sys.Date())),
    content = function(file) {
      dss <- lapply(unname(store$datasets), function(d) list(
        id = d$id, name = d$name, columns = names(d$data), data = unname(as.list(d$data)),
        types = unname(as.list(d$types[names(d$data)])), source = d$source, sheet = d$sheet, file = d$file,
        notes = as.list(d$notes), ignored = as.list(d$ignored)))
      an <- lapply(doe, function(m) m$get_state())
      jsonlite::write_json(list(app = "StatDesign Studio", version = 1, saved = format(Sys.time()), datasets = dss, analyses = an),
                           file, auto_unbox = TRUE, null = "null", na = "null", digits = NA, pretty = TRUE)
    })
}

shinyApp(ui, server)
