# lssdoc 0.3.0

## New features

* Write a questionnaire in Word and turn it into a LimeSurvey file, with a
  new experimental set of functions: `lss_template_docx()` writes a blank
  form to fill in, `read_form_docx()` reads a filled one back, and
  `check_form_docx()` reports everything wrong with it in one pass. Blank
  forms can also be downloaded from the package website, for authors who do
  not use R.

* `write_form_docx()` renders a specification as that same form, and
  `as_lss_spec()` turns an existing survey into a specification, so a
  questionnaire can leave LimeSurvey, be edited in Word and come back.

* `write_lss()` now writes every declared language, not only the first.

* `lss_template_docx()` can write a blank form for a multilingual
  questionnaire, independently of the language of the form's own labels.

* `lss_spec()` gains an optional group `description` and quota `limit`.

## Minor improvements and bug fixes

* `read_lss()` is about ten times faster on a large survey.

* `read_lss()` now fails with a clear error on a malformed, truncated or
  non-UTF-8 file, instead of terminating the R session on some platforms.

# lssdoc 0.2.0

## New features

* New experimental authoring layer. `lss_spec()` describes a questionnaire
  in R -- groups, 21 question kinds, answer options (including "other" and
  exclusive choices), display conditions and quotas -- and validates it
  against what LimeSurvey accepts. `write_lss()` then writes it as a
  LimeSurvey 6 `.lss` file ready to import. Both functions are experimental
  and their interface may change.

* `lss_spec()` accepts texts in several languages (`languages = c("fr", "en")`,
  each text given as a named vector), but `write_lss()` emits the primary
  language only for now. Multilingual output is planned for 0.3.0.

## Minor improvements and bug fixes

* `read_lss()` now warns (class `lssdoc_newer_dbversion`) when a file comes
  from a newer LimeSurvey than the package targets, instead of reading it
  silently.

* In the table layout, the "Type" column no longer wraps one-word labels
  such as "Computed" onto a second line.

# lssdoc 0.1.1

* `read_lss()` now returns a clear error on a non-XML or empty file, instead
  of, on some systems, crashing the R session.

# lssdoc 0.1.0

* Initial CRAN release.
