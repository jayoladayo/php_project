# Home page.

analysis_card <- function(page, ic, title, desc, chips, stage = NULL) {
  tags$a(class = "kcard acard", href = "#", onclick = nav_js(page),
         div(class = "hrow", ico_box(ic, "ig"), if (!is.null(stage)) div(class = "ml", pill(sprintf("Stage %d", stage), "neu"))),
         div(class = "at", title), div(class = "ad", desc),
         div(class = "meta", lapply(chips, function(c) span(class = "chip", c))))
}

home_ui <- function() {
  tagList(
    div(class = "hero",
        div(style = "flex:1;min-width:0",
            span(class = "tag", icon("check", 13, 3), "KFUPM Experimental Design"),
            h1("Analyse any designed experiment. No code required."),
            p("Upload your data, tell us what each column means, and get the ANOVA, assumption checks, post-hoc comparisons and a report you can submit."),
            div(class = "hrow",
                tags$button(class = "btn-k bgld", onclick = nav_js("data"), icon("upload", 15), "Start with your data"),
                tags$button(class = "btn-k", style = "background:rgba(255,255,255,.12);color:#fff", onclick = nav_js("rcbd"), icon("play", 15), "Try the RCBD example"))),
        div(class = "stats",
            div(class = "stat", tags$b("4"), span("designed experiments")),
            div(class = "stat", tags$b("3"), span("sample comparisons")),
            div(class = "stat", tags$b("5"), span("post-hoc methods")),
            div(class = "stat", tags$b("0"), span("lines of code needed")))),
    div(class = "split",
        div(class = "col",
            div(class = "hrow", h2(class = "ct", "Design of experiments"), span(class = "small", "ANOVA, assumptions, post-hoc and non-parametric tests")),
            div(class = "grid g2",
                analysis_card("crd", "shuffle", "Completely Randomised", "Treatments assigned to units fully at random.", c("1 factor", "Kruskal-Wallis fallback")),
                analysis_card("rcbd", "rows", "Randomised Complete Block", "Compare treatments while removing block-to-block variation.", c("Treatment + block", "Friedman fallback")),
                analysis_card("lsd", "grid", "Latin Square", "Two blocking factors arranged as rows and columns.", c("Rows × columns", "p ≥ 3")),
                analysis_card("glsd", "grid4", "Graeco-Latin Square", "Two treatment factors over two blocking factors.", c("4 factors", "p ≥ 4"))),
            div(class = "hrow", style = "margin-top:6px", h2(class = "ct", "Explore and compare"), span(class = "small", "Classic tests for one, two or many groups")),
            div(class = "grid g2",
                analysis_card("desc", "bar", "Descriptive statistics", "Summaries, charts and outliers for any column.", c("Mean, SD, quartiles")),
                analysis_card("one", "target", "One sample", "Compare a mean or median against a target value.", c("t-test", "Wilcoxon")),
                analysis_card("two", "columns", "Two samples", "Independent or paired groups.", c("t-tests", "Mann-Whitney")),
                analysis_card("k", "layers", "k samples", "Three or more groups without blocking.", c("ANOVA", "Kruskal-Wallis")))),
        div(class = "col",
            kcard(div(class = "hrow", h3(class = "ct", "Your datasets"), div(class = "ml", tags$a(href = "#", onclick = nav_js("data"), class = "small", style = "color:var(--green);font-weight:600", "Open Data Studio"))),
                  uiOutput("home_ds")),
            kcard(h3(class = "ct", "Class examples"),
                  p(class = "small", "From the course R scripts. One click opens the analysis."),
                  lapply(names(CLASS_EXAMPLES), function(k) {
                    ex <- CLASS_EXAMPLES[[k]]
                    div(class = "list-row", icon(DESIGNS[[ex$design]]$icon, 16),
                        div(style = "flex:1;min-width:0", div(style = "font-weight:600", ex$title),
                            div(class = "small", sprintf("%s · %d rows", DESIGNS[[ex$design]]$short, nrow(ex$data)))),
                        tags$button(class = "btn-k bgh bsm", onclick = sprintf("Shiny.setInputValue('home_ex', '%s', {priority: 'event'})", k), "Open"))
                  })),
            kcard(div(class = "hrow", ico_box("compass", "io"),
                      div(style = "flex:1", h3(class = "ct", "Not sure which test?"), p(class = "small", style = "margin:0", "The Test Advisor asks a few questions and recommends one.")),
                      tags$button(class = "btn-k bs bsm", onclick = nav_js("advisor"), "Ask"))))))
}
