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
