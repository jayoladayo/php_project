# Home page.

analysis_card <- function(page, ic, title, desc, chips, stage = NULL) {
  tags$a(class = "kcard acard", href = "#", onclick = nav_js(page),
         div(class = "hrow", ico_box(ic, "ig"), if (!is.null(stage)) div(class = "ml", pill(sprintf("Stage %d", stage), "neu"))),
         div(class = "at", title), div(class = "ad", desc),
         div(class = "meta", lapply(chips, function(c) span(class = "chip", c))))
}

# The five steps every analysis follows (shown on the home page and at the start of every analysis).
FIVE_STEPS <- list(
  list("Hypotheses", "State what you want to show"),
  list("Significance level", "How sure you need to be (α)"),
  list("Information available", "What the data and the design tell us"),
  list("Test", "The right test, run for you"),
  list("Decision", "Reject H0 or not, three ways"))

# A compact strip of the five steps (names only: the numbering of the screens is shown inside each analysis).
five_steps_strip <- function(class = NULL) {
  div(class = paste("fs-strip", class), `aria-label` = "The 5-step process",
      lapply(seq_along(FIVE_STEPS), function(i) tagList(
        span(class = "fs-chip", icon("check", 12, 3), FIVE_STEPS[[i]][[1]]),
        if (i < length(FIVE_STEPS)) icon("chev", 14))))
}

# The five steps as a short list (used beside the data set-up of every analysis).
five_steps_list <- function() tags$ol(class = "fs-list", lapply(FIVE_STEPS, function(s) tags$li(tags$b(s[[1]]), span(s[[2]]))))

path_card <- function(page, ic, title, desc, cta) {
  tags$a(class = "path-card", href = "#", onclick = nav_js(page),
         ico_box(ic, "ig", 22), div(class = "pc-t", title), div(class = "pc-d", desc),
         span(class = "pc-go", cta, icon("arrowr", 15)))
}

home_ui <- function() {
  tagList(
    div(class = "hero",
        div(class = "hero-main",
            div(class = "hrow", style = "gap:14px", tags$img(class = "hero-logo", src = "kfupm_logo.svg", alt = "KFUPM"),
                span(class = "tag", icon("check", 13, 3), "KFUPM Experimental Design · Group 4")),
            h1("Analyse any designed experiment."),
            p("Bring your data, follow five clear steps, and finish with a decision you can explain and a report you can submit."),
            div(class = "hrow",
                tags$button(class = "btn-k bgld", onclick = nav_js("data"), icon("upload", 15), "Start with your data"),
                tags$button(class = "btn-k bghero", onclick = nav_js("problems"), icon("clipboard", 15), "Practise with course problems"))),
        div(class = "hero-process", div(class = "hp-t", "Every analysis follows the same 5 steps"),
            lapply(seq_along(FIVE_STEPS), function(i) div(class = "hp-row", span(class = "hp-n", icon("check", 14, 3)), div(div(class = "hp-h", FIVE_STEPS[[i]][[1]]), div(class = "hp-s", FIVE_STEPS[[i]][[2]])))))),
    div(class = "col",
        div(class = "home-sec", h2(class = "ct home-h", "What would you like to do?"), span(class = "small", "Pick the closest match. You can always change your mind.")),
        div(class = "grid g4 paths",
            path_card("data", "database", "Analyse my own data", "Upload or paste a table, say what each column is, and get the five steps on your data.", "Open Data Studio"),
            path_card("advisor", "compass", "I am not sure which test", "Answer three short questions and we open the right analysis with sensible settings.", "Ask the Test Advisor"),
            path_card("problems", "clipboard", "Practise with course problems", sprintf("Chapters 2 to 6 with their complete data, solved step by step (%d problems).", length(PROBLEMS)), "Open Problem Sets"),
            path_card("learn", "book", "Learn the methods", "When to use each design, the model, the assumptions and a worked example from class.", "Open Learn"))),
    div(class = "col",
        div(class = "home-sec", h2(class = "ct home-h", "Designed experiments"), span(class = "small", "ANOVA, assumption checks, post-hoc comparisons and the rank-test fallback, in five steps")),
        div(class = "grid g3",
            analysis_card("crd", "shuffle", "Completely Randomised (CRD)", "Treatments assigned to units fully at random.", c("1 factor", "Kruskal-Wallis fallback")),
            analysis_card("rcbd", "rows", "Randomised Complete Block (RCBD)", "Compare treatments while removing block-to-block variation.", c("Treatment + block", "Friedman fallback")),
            analysis_card("lsd", "grid", "Latin Square", "Two blocking factors arranged as rows and columns.", c("Rows × columns", "Quade fallback")),
            analysis_card("glsd", "grid4", "Graeco-Latin Square", "Two treatment factors over two blocking factors.", c("Rows, columns, Greek", "Quade fallback")),
            analysis_card("fact", "cross", "Factorial", "Two or three crossed factors: main effects and interactions.", c("CRD, RCBD, LSD or GLSD layout", "Rank test per effect")),
            analysis_card("twok", "cross", "2^k Factorial", "Two-level designs with two to five factors, replicated or with one observation per combination.", c("2² to 2⁵", "Pooled error if unreplicated")))),
    div(class = "col",
        div(class = "home-sec", h2(class = "ct home-h", "Explore and compare"), span(class = "small", "Classic tests for one, two or many groups")),
        div(class = "grid g4",
            analysis_card("desc", "bar", "Descriptive statistics", "Summaries, charts and outliers for any column.", c("Mean, SD, quartiles")),
            analysis_card("one", "target", "One sample", "Compare a mean or median against a target value.", c("t-test", "Wilcoxon")),
            analysis_card("two", "columns", "Two samples", "Independent or paired groups.", c("t-tests", "Mann-Whitney")),
            analysis_card("k", "layers", "k samples", "Three or more groups without blocking.", c("ANOVA", "Kruskal-Wallis")))),
    div(class = "grid g2",
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
              }))),
    div(class = "home-sec", h2(class = "ct home-h", "About this project"), span(class = "small", "The aim of the app and the team behind it")),
    div(class = "split", aims_card(), team_home_card()))
}
