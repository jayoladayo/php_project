# The project team (Group 4) and the app's aim and objectives: a section on the home page and
# the About the team page. Photos live in www/team; a member without one shows initials.

TEAM <- list(
  list(id = "qudus", name = "Qudus Owoyemi", dept = "Statistics Department", photo = "team/qudus.jpg?v=2"),
  list(id = "oladayo", name = "Oladayo Jayeola", dept = "Industrial and Systems Engineering (ISE) Department", photo = "team/oladayo.jpg?v=2"),
  list(id = "mubarak", name = "Mubarak Abdhamid", dept = "Statistics Department", role = "Team leader", photo = NULL),
  list(id = "sodiq", name = "Sodiq Babawale", dept = "Industrial and Systems Engineering (ISE) Department", photo = "team/sodiq.jpg?v=2"),
  list(id = "ovie", name = "Ovie Ederhite", dept = "Industrial and Systems Engineering (ISE) Department", photo = "team/ovie.jpg?v=2")
)

APP_AIM <- "To give students of the KFUPM Experimental Design course one place to plan, analyse and report designed experiments correctly, step by step, without writing code."
APP_OBJECTIVES <- c(
  "Analyse the designs taught in the course (CRD, RCBD, Latin and Graeco-Latin squares) and the one, two and k sample tests with the methods used in class.",
  "Guide every analysis through the hypothesis-testing steps: hypotheses, significance level, information available, test, decision by three methods and post-hoc.",
  "Check the model assumptions (normality, constant variance and independence of the residuals) and move to the right non-parametric test when one fails.",
  "Provide the course problem sets (Chapters 2, 3 and 4) with their full data and worked solutions, and let students solve their own problems the same way.",
  "Produce R code, plain-language interpretations and a report ready to submit, with an AI course assistant (Professor Riaz) to explain the results.")

team_initials <- function(name) paste(substr(strsplit(name, " ")[[1]], 1, 1), collapse = "")
# Opens the team page and brings one member's card into view.
team_js <- function(id = NULL) sprintf("Shiny.setInputValue('nav', 'team', {priority: 'event'});%s return false;",
  if (is.null(id)) "" else sprintf(" teamFocus('%s');", id))

team_avatar <- function(m, cls = "tm-av") {
  if (!is.null(m$photo)) tags$img(class = cls, src = m$photo, alt = m$name, loading = "lazy")
  else div(class = paste(cls, "ph"), span(team_initials(m$name)))
}

aims_card <- function() kcard(class = "tm-aims",
  div(class = "hrow", ico_box("target", "ig"), h2(class = "ct", "Aim and objectives")),
  p(class = "tm-aim", APP_AIM),
  tags$ol(class = "tm-obj", lapply(APP_OBJECTIVES, tags$li)))

team_home_card <- function() kcard(class = "tm-home",
  div(class = "hrow", ico_box("users", "io"), div(h2(class = "ct", "Project team"), div(class = "small", "Group 4 · KFUPM Experimental Design")),
      div(class = "ml", tags$a(href = "#", class = "small ps-link", onclick = team_js(), "About the team"))),
  div(class = "tm-list", lapply(TEAM, function(m)
    tags$a(class = "tm-row", href = "#", onclick = team_js(m$id), title = sprintf("About %s", m$name),
           team_avatar(m, "tm-av sm"),
           div(class = "tm-who", div(class = "tm-name", m$name, if (!is.null(m$role)) span(class = "pill gold", icon("star", 11, 2.4), m$role)), div(class = "small", m$dept)),
           icon("chev", 14)))))

team_ui <- function() {
  tagList(
    page_head("About the team", "Group 4 · KFUPM Experimental Design course project",
              tags$button(class = "btn-k bs bsm", onclick = nav_js("home"), icon("arrowl", 15), "Back to Home")),
    kcard(class = "tm-intro",
          div(class = "tm-intro-row",
              tags$img(class = "tm-logo", src = "kfupm_logo.svg", alt = "King Fahd University of Petroleum & Minerals"),
              div(class = "tm-intro-txt",
                  h2(class = "ct", "StatDesign Studio"),
                  p(class = "sub", style = "margin:4px 0 0", "Built by Group 4 for the Experimental Design course at King Fahd University of Petroleum & Minerals: a no-code app for designing and analysing experiments, from the hypotheses to the final report.")))),
    div(class = "tm-grid", lapply(TEAM, function(m)
      div(class = paste("kcard tm-card", if (!is.null(m$role)) "lead"), id = paste0("tm-", m$id),
          div(class = "tm-photo", team_avatar(m, "tm-img"), if (!is.null(m$role)) span(class = "tm-badge", icon("star", 12, 2.4), m$role)),
          div(class = "tm-body",
              div(class = "tm-name lg", m$name),
              div(class = "tm-dept", icon("school", 14), m$dept),
              if (is.null(m$photo)) div(class = "small tm-soon", "Photo coming soon"))))),
    div(class = "split", style = "margin-top:4px", aims_card(),
        kcard(div(class = "hrow", ico_box("school", "io"), h2(class = "ct", "The course")),
              div(class = "kv-row", span("University"), span("King Fahd University of Petroleum & Minerals (KFUPM)")),
              div(class = "kv-row", span("Course"), span("Experimental Design")),
              div(class = "kv-row", span("Team"), span("Group 4: five members from the Statistics and ISE departments")),
              div(class = "kv-row", span("Team leader"), span(vapply(Filter(function(m) !is.null(m$role), TEAM), `[[`, "", "name"))))))
}
