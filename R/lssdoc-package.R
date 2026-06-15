#' @keywords internal
#'
#' @section Example surveys:
#' Two example `.lss` files ship with the package and are reachable with
#' [base::system.file()], so every reader can reproduce the examples and
#' the *Get started* vignette without supplying their own LimeSurvey
#' export:
#'
#' * `demo_survey.lss` -- a clean, synthetic four-language survey
#'   (English, French, German, Spanish) with quotas and a consent block:
#'   `system.file("extdata", "demo_survey.lss", package = "lssdoc")`.
#' * `audit_demo.lss` -- a deliberately flawed survey seeded with every
#'   anomaly [audit_lss()] detects:
#'   `system.file("extdata", "audit_demo.lss", package = "lssdoc")`.
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang abort warn
#' @importFrom utils modifyList
#' @importFrom xml2 read_xml xml_find_all xml_find_first xml_text xml_attr
## usethis namespace: end
NULL
