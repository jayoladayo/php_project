# Small UI building blocks that follow the design system.

NAV <- list(
  list(page = "home", label = "Home", icon = "home"),
  list(page = "advisor", label = "Test Advisor", icon = "compass"),
  list(page = "data", label = "Data Studio", icon = "database", badge = "data-count"),
  list(group = "Explore"),
  list(page = "desc", label = "Descriptive statistics", icon = "bar"),
  list(group = "Compare"),
  list(page = "one", label = "One sample", icon = "target"),
  list(page = "two", label = "Two samples", icon = "columns"),
  list(page = "k", label = "k samples", icon = "layers"),
  list(group = "Design of experiments"),
  list(page = "problems", label = "Problem Sets", icon = "clipboard"),
  list(page = "crd", label = "CRD", icon = "shuffle"),
  list(page = "rcbd", label = "RCBD", icon = "rows"),
  list(page = "lsd", label = "Latin Square", icon = "grid"),
  list(page = "glsd", label = "Graeco-Latin Square", icon = "grid4"),
  list(group = "Tools"),
  list(page = "planner", label = "Design Planner", icon = "map"),
  list(page = "report", label = "Report", icon = "file", badge = "report-count"),
  list(page = "learn", label = "Learn", icon = "book")
)

PAGE_CRUMB <- list(
  home = c("StatDesign Studio", "Home"), advisor = c("StatDesign Studio", "Test Advisor"),
  data = c("StatDesign Studio", "Data Studio"), desc = c("Explore", "Descriptive statistics"),
  one = c("Compare", "One sample"), two = c("Compare", "Two samples"), k = c("Compare", "k samples"),
  problems = c("Design of experiments", "Problem Sets"), crd = c("Design of experiments", "CRD"), rcbd = c("Design of experiments", "RCBD"),
  lsd = c("Design of experiments", "Latin Square"), glsd = c("Design of experiments", "Graeco-Latin Square"),
  planner = c("Tools", "Design Planner"), report = c("Tools", "Report"), learn = c("Tools", "Learn")
)

nav_js <- function(page) sprintf("Shiny.setInputValue('nav', '%s', {priority: 'event'}); return false;", page)

sidebar_ui <- function() {
  items <- lapply(NAV, function(n) {
    if (!is.null(n$group)) return(div(class = "glab", n$group))
    tags$a(class = paste("nav-item", if (n$page == "home") "on"), href = "#", `data-page` = n$page,
           onclick = nav_js(n$page), icon(n$icon), span(n$label),
           if (!is.null(n$badge)) span(class = "ct", id = n$badge, style = "display:none", "0"))
  })
  div(class = "side", id = "side",
      div(class = "brand", div(class = "logo", icon("bar", 22, 2.4)),
          div(div(class = "bname", "StatDesign Studio"), div(class = "bsub", "Experimental design, no code"))),
      items,
      div(class = "sfoot", "KFUPM · Experimental Design", br(), "Version 1.0 · ",
          tags$a(href = "#", onclick = "Shiny.setInputValue('help', Math.random()); return false;", style = "color:inherit", "Help & About")))
}

topbar_ui <- function() {
  div(class = "top",
      tags$button(class = "ib menu-btn", onclick = "document.querySelector('.app').classList.toggle('nav-open')", icon("menu", 17)),
      div(class = "crumb", id = "crumb", span("StatDesign Studio"), icon("chev", 14), tags$b("Home")),
      div(class = "sp"),
      div(class = "search-wrap",
          selectizeInput("jump", NULL, choices = NULL, width = "100%",
                         options = list(placeholder = "Search analyses, tests, help",
                                        onInitialize = I("function() { this.setValue(''); }")))),
      tags$button(class = "ib", title = "Light or dark", onclick = "toggleTheme()", icon("moon", 17)),
      tags$button(class = "ib", title = "Help", onclick = "Shiny.setInputValue('help', Math.random())", icon("help", 17)),
      downloadButton("save_project", label = tagList(icon("save", 15), "Save project"), icon = NULL,
                     class = "btn-k bs bsm"))
}

page_head <- function(title, sub = NULL, ...) {
  div(class = "page-head",
      div(class = "grow", h1(class = "pt", title), if (!is.null(sub)) p(class = "sub", sub)),
      ...)
}

kcard <- function(..., class = NULL, id = NULL) div(class = paste("kcard", class), id = id, ...)

pill <- function(text, kind = "neu", ic = NULL) span(class = paste("pill", kind), if (!is.null(ic)) icon(ic, 12, 3), text)

note <- function(..., kind = "i", ic = "info") div(class = paste("note", kind), icon(ic, 16), div(...))

btn <- function(id, label, ic = NULL, kind = "bs", size = NULL, ...) {
  actionButton(id, label = tagList(if (!is.null(ic)) icon(ic, 15), label), icon = NULL,
               class = paste("btn-k", kind, size), ...)
}

ico_box <- function(name, kind = "ig", size = 18) div(class = paste("ico", kind), icon(name, size))

# Radio buttons styled as a segmented control.
seg_input <- function(id, choices, selected = NULL) {
  div(class = "seg", radioButtons(id, NULL, choices = choices, selected = selected, inline = TRUE))
}

stepper_ui <- function(ns, steps, cur, maxstep) {
  parts <- list()
  for (i in seq_along(steps)) {
    cls <- if (i < cur) "done" else if (i == cur) "cur" else ""
    can <- i <= maxstep
    parts[[length(parts) + 1]] <- tags$button(
      class = paste("st", cls), disabled = if (!can) NA,
      onclick = if (can) sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("goto_step"), i),
      div(class = "dot", if (i < cur) icon("check", 14, 3) else i), span(class = "lbl", steps[i]))
    if (i < length(steps)) parts[[length(parts) + 1]] <- div(class = paste("sline", if (i < cur) "done"))
  }
  div(class = "stepper", parts)
}

coming_soon <- function(title, sub, stage, what) {
  tagList(
    page_head(title, sub),
    kcard(div(class = "empty",
              ico_box("spark", "io", 26),
              h2(class = "ct", sprintf("Arrives in stage %d", stage)),
              p(class = "sub", style = "max-width:520px", what),
              tags$button(class = "btn-k bs", onclick = nav_js("data"), icon("database", 15), "Prepare data in Data Studio")))
  )
}

html_table <- function(df, numeric_right = TRUE, types = NULL, max_rows = Inf) {
  df <- head(df, max_rows)
  isnum <- vapply(df, function(c) { p <- suppressWarnings(as.numeric(as.character(c))); all(is.na(c) | !is.na(p)) }, TRUE)
  tags$table(class = "kt",
             tags$thead(tags$tr(lapply(seq_along(df), function(j) tags$th(
               class = if (numeric_right && isnum[j]) "n", names(df)[j],
               if (!is.null(types)) tags$small(names(TYPE_CHOICES)[match(types[[names(df)[j]]], TYPE_CHOICES)]))))),
             tags$tbody(lapply(seq_len(nrow(df)), function(i) tags$tr(lapply(seq_along(df), function(j) {
               v <- df[[j]][i]
               tags$td(class = if (numeric_right && isnum[j]) "n", if (is.na(v)) span(style = "color:var(--faint)", "—") else as.character(v))
             })))))
}
