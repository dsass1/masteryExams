#' Tutorial HTML dependency
#'
#' @details HTML dependency for core tutorial JS and CSS. This should be included as a
#' dependency for custom tutorial formats that wish to ensure that that
#' tutorial.js and tutorial.css are loaded prior their own scripts and stylesheets.
#'
#' @export
tutorial_html_dependency <- function() {
  htmltools::htmlDependency(
    name = "tutorial",
    version = utils::packageVersion("masteryExams"),
    src = learnr:::html_dependency_src("lib", "tutorial"),
    script = "tutorial.js",
    stylesheet = "tutorial.css",
    all_files = TRUE
  )
}

