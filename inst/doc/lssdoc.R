## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(lssdoc)

## -----------------------------------------------------------------------------
demo <- system.file("extdata", "demo_survey.lss", package = "lssdoc")

## -----------------------------------------------------------------------------
lss <- read_lss(demo)
lss$languages

## -----------------------------------------------------------------------------
broken <- system.file("extdata", "audit_demo.lss", package = "lssdoc")
audit_lss(read_lss(broken))

## ----eval = FALSE-------------------------------------------------------------
# # Parse once (above), then render different variants without re-reading.
# render_questionnaire(lss, "review.docx")

## ----echo = FALSE, out.width = "100%"-----------------------------------------
knitr::include_graphics("../man/figures/template_cards.png")

## ----eval = FALSE-------------------------------------------------------------
# render_questionnaire(lss, "questionnaire.docx", template = "table")

## ----echo = FALSE, out.width = "100%"-----------------------------------------
knitr::include_graphics("../man/figures/template_table.png")

## ----eval = FALSE-------------------------------------------------------------
# render_questionnaire(
#   lss, "review_en_fr.docx",
#   languages   = c("en", "fr"),
#   template    = "table",
#   chrome_lang = "en"
# )

## ----eval = FALSE-------------------------------------------------------------
# render_questionnaire(lss, "review.pdf")

## ----eval = FALSE-------------------------------------------------------------
# render_audit(lss, "qa.docx")

