## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
can_write <- requireNamespace("officer", quietly = TRUE) &&
  requireNamespace("flextable", quietly = TRUE)

## -----------------------------------------------------------------------------
library(lssdoc)

## ----template, eval = can_write-----------------------------------------------
template <- tempfile(fileext = ".docx")
lss_template_docx(template, lang = "en")

## ----kinds, echo = FALSE------------------------------------------------------
ref <- lssdoc:::lss_kinds_reference()
for (v in c("options", "rows", "columns")) {
  ref[[v]] <- ifelse(ref[[v]] == "required", "yes", "")
}
cols <- c(kind = "Type", label = "Meaning", options = "Options",
          rows = "Rows", columns = "Columns", min_options = "Min.",
          other = "Other", exclusive = "Exclusive",
          implicit_codes = "Built-in codes")
knitr::kable(stats::setNames(ref[names(cols)], cols), row.names = FALSE)

## ----defaults, echo = FALSE---------------------------------------------------
d <- lssdoc:::lss_spec_defaults
knitr::kable(data.frame(
  setting = names(d),
  default = vapply(d, function(x) paste(format(x), collapse = ", "), ""),
  row.names = NULL
))

## ----deferred, echo = FALSE---------------------------------------------------
knitr::kable(lssdoc:::lss_kinds_deferred)

## ----check, eval = can_write--------------------------------------------------
check_form_docx(template)

## ----read, eval = can_write---------------------------------------------------
spec <- read_form_docx(template)
spec

## ----write, eval = can_write--------------------------------------------------
lss_file <- tempfile(fileext = ".lss")
write_lss(spec, lss_file)

## ----roundtrip, eval = can_write----------------------------------------------
back <- read_lss(lss_file)
audit_lss(back)

## ----existing, eval = can_write-----------------------------------------------
lss <- read_lss(system.file("extdata", "demo_survey.lss", package = "lssdoc"))
form <- tempfile(fileext = ".docx")
write_form_docx(lss, form, lang = "en", strict = FALSE)

