#' Write a survey specification to an importable `.lss` file
#'
#' `r lifecycle::badge("experimental")`
#'
#' \strong{Experimental.} Turn an [lss_spec()] specification into a
#' LimeSurvey structure file (`.lss`) that imports directly through
#' *Create survey -> Import*. The output targets LimeSurvey 6
#' (DBVersion 700). The emitted file can be
#' read back with [read_lss()], checked with [audit_lss()] and rendered
#' with [render_questionnaire()] -- so the document reviewers read is
#' produced from the very file LimeSurvey receives.
#'
#' @param spec An [lss_spec()] object, or a plain list with the same
#'   structure (it is then validated through `lss_spec()` first).
#' @param file Character. Path of the `.lss` file to write.
#' @param sid Integer. Survey id embedded in the file. LimeSurvey assigns
#'   a fresh id on import when this one is taken, so the value rarely
#'   matters.
#' @param settings Named list of survey fields overriding the built-in
#'   defaults (e.g. `list(anonymized = "Y", showprogress = "Y")`). A name
#'   belonging to the `surveys` table takes a single string; a name
#'   belonging to the per-language `surveys_languagesettings` table
#'   (`surveyls_dateformat`, `surveyls_numberformat`,
#'   `surveyls_description`, the e-mail templates, ...) takes either a
#'   single string applied to every language or a list or vector keyed by
#'   language code -- see the *Languages* section. The defaults ship with
#'   the package and come from a real LimeSurvey 6 export, scrubbed -- see
#'   `lss_default_surveys_fields` in the sources for the rationale.
#'
#' @return Invisibly, the path to the written file.
#'
#' @details
#' Mapping choices, each validated against real LimeSurvey 6 imports:
#'
#' * Each kind maps to a LimeSurvey type and theme attested by a corpus
#'   of real exports (see `lss_kinds` in the sources). Options of
#'   single-choice lists, rankings and array columns are emitted as
#'   `answers`; options of multiple-choice questions, item batteries and
#'   array rows as `subquestions`; scalar kinds and implicit scales
#'   (yes/no, gender, five-point, 5/10-point arrays) emit none.
#' * The native `other` option is emitted as `other = "Y"` plus the
#'   localized attribute `other_replace_text`. Localized attributes MUST
#'   carry the language code: emitted without one, LimeSurvey silently
#'   ignores them and shows its default wording. Global attributes
#'   (`exclude_all_others`, `max_answers`, ...) stay language-less.
#' * `other_position` / `other_position_code` control where the other
#'   option is displayed; `exclude_all_others` accepts several codes
#'   separated by `;`.
#' * Relevance equations are translated from the minimal syntax of
#'   [lss_spec()] into ExpressionScript (`code.NAOK == "1"`).
#' * Quotas are emitted with the terminate action and the quota's `limit`
#'   (zero unless the spec gives one).
#' * A group's optional `description` is emitted into
#'   `group_l10ns.description` (empty when the spec gives none).
#' * A mandatory or capped ranking also receives `min_answers = 1`,
#'   overridable through the question's `attributes`.
#'
#' @section Languages:
#' Every language the spec declares is written. The `surveys` row carries
#' the base language -- `languages[1]` -- as `language` and the others,
#' space-separated, as `additional_languages`. Every localized section
#' (`surveys_languagesettings`, `group_l10ns`, `question_l10ns` for
#' questions and subquestions alike, `answer_l10ns`,
#' `quota_languagesettings`) receives one row per language, grouped by
#' entity as a real LimeSurvey export groups them. [lss_spec()] already
#' requires every declared language for every text, so no translation can
#' go missing at emission.
#'
#' The `<languages>` element lists the additional languages first and the
#' base language last, the order LimeSurvey itself writes. It is only a
#' membership set: read the base language from `read_lss()$base_language`,
#' never from `read_lss()$languages[1]`.
#'
#' Localized question attributes -- `other_replace_text`, `prefix`,
#' `suffix`, `choice_title`, `rank_title`, `printable_help`, ... -- are
#' emitted once per language, each row carrying its language code; without
#' it LimeSurvey silently ignores the attribute and shows its own default
#' wording. An attribute passed through `question$attributes` under one of
#' those names is treated the same way: a plain string is repeated in every
#' language, a value keyed by language code is resolved language by
#' language (`attributes = list(prefix = "CHF")` or
#' `list(choice_title = c(fr = "Choix", en = "Choice"))`). Every other
#' attribute stays language-less, as LimeSurvey stores it.
#'
#' Per-language survey settings work the same way: `settings` accepts a
#' single value repeated in every `surveys_languagesettings` row, or a list
#' keyed by language code for the fields LimeSurvey genuinely varies --
#' `list(surveyls_dateformat = c(fr = "5", en = "2"))` gives the French
#' respondent a `dd.mm.yyyy` date picker and the English one `mm/dd/yyyy`.
#' A declared language missing from such a value is an error, not a silent
#' fallback.
#'
#' A quota is localized through `quotals_name` and `quotals_message`, one
#' row per language. The administration-side label `quota.name` has a
#' single column in LimeSurvey and therefore keeps the primary-language
#' wording.
#'
#' @examples
#' spec <- lss_spec(
#'   title = "Demo",
#'   groups = list(list(title = "G", questions = list(
#'     list(code = "q1", kind = "single", text = "Oui ou non ?",
#'          options = list(list(text = "Oui"), list(text = "Non")))
#'   )))
#' )
#' out <- tempfile(fileext = ".lss")
#' write_lss(spec, out)
#' audit_lss(out)
#' @seealso [lss_spec()], [read_lss()], [audit_lss()],
#'   [render_questionnaire()].
#' @export
write_lss <- function(spec, file, sid = 100001L, settings = list()) {
  if (!inherits(spec, "lss_spec")) {
    if (!is.list(spec)) {
      lssdoc_abort("{.arg spec} must be an {.fn lss_spec} object or a list.",
                   class = "lssdoc_bad_spec")
    }
    spec <- lss_spec(
      title = spec$title, groups = spec$groups,
      languages = spec$languages %||% spec$language %||% "fr",
      welcome = spec$welcome, end_text = spec$end_text,
      quotas = spec$quotas
    )
  }
  if (!is.character(file) || length(file) != 1L) {
    lssdoc_abort("{.arg file} must be a single file path.",
                 class = "lssdoc_bad_path")
  }
  if (!is.numeric(sid) || length(sid) != 1L || !is.finite(sid) ||
      sid != trunc(sid) || sid < 1) {
    lssdoc_abort("{.arg sid} must be a single positive whole number.",
                 class = "lssdoc_bad_sid")
  }
  sid <- as.integer(sid)
  # an lss_spec object may have been mutated after construction: the
  # revalidation is cheap and prevents emitting a silently wrong file
  spec_validate(spec)

  emit <- lss_emitter(spec, sid, settings)
  xml2::write_xml(emit$doc, file, options = c("format", "no_declaration"))
  # LimeSurvey expects an explicit XML declaration; write_xml() with
  # "no_declaration" plus a manual prepend reproduces the exact framing
  # validated by real imports.
  txt <- readLines(file, warn = FALSE, encoding = "UTF-8")
  writeLines(c('<?xml version="1.0" encoding="UTF-8"?>', txt), file,
             useBytes = TRUE)

  cli::cli_alert_success(
    "Wrote {.path {file}} ({emit$n_questions} question{?s}, {emit$n_groups} group{?s}, {length(spec$quotas)} quota{?s})."
  )
  invisible(file)
}

# ---- relevance translation -------------------------------------------------

#' Translate the minimal condition syntax into ExpressionScript
#'
#' The forms are validated upstream by `spec_validate()`; here we only
#' translate. `autre` maps to LimeSurvey's native other code `-oth-`.
#' For `count()`, the checkbox fields of a multiple-choice question are
#' `code_1.NAOK`, `code_2.NAOK`, ... -- the other option lives in
#' `code_other` and is not counted.
#' @keywords internal
#' @noRd
translate_relevance <- function(expr, defined) {
  expr <- trimws(expr %||% "")
  if (!nzchar(expr)) return(lss_spec_defaults$relevance)
  ls_code <- function(x) ifelse(tolower(trimws(x)) == "autre", "-oth-", trimws(x))

  m <- regmatches(expr, regexec(
    "^count\\(([A-Za-z][A-Za-z0-9]*)\\)\\s*(>=|>|==|<=|<)\\s*([0-9]+)$", expr))[[1]]
  if (length(m) == 4L) {
    codes <- option_codes(defined[[m[2]]]$options)
    fields <- paste0(m[2], "_", codes, ".NAOK")
    return(sprintf("count(%s) %s %s", paste(fields, collapse = ", "), m[3], m[4]))
  }
  m <- regmatches(expr, regexec(
    "^([A-Za-z][A-Za-z0-9]*)\\s+in\\s+\\[([^]]+)\\]$", expr))[[1]]
  if (length(m) == 3L) {
    codes <- ls_code(strsplit(m[3], ",")[[1]])
    return(paste0("(", paste(sprintf('%s.NAOK == "%s"', m[2], codes),
                             collapse = " or "), ")"))
  }
  m <- regmatches(expr, regexec(
    "^([A-Za-z][A-Za-z0-9]*)\\s*=\\s*([A-Za-z0-9-]+)$", expr))[[1]]
  if (length(m) != 3L) {
    lssdoc_abort(
      "Internal error: unvalidated relevance expression reached the emitter.",
      class = "lssdoc_bad_spec"
    )
  }
  sprintf('%s.NAOK == "%s"', m[2], ls_code(m[3]))
}

# ---- emission --------------------------------------------------------------

# How a real LimeSurvey export stores several languages
# -----------------------------------------------------
# Established table by table on `inst/extdata/demo_survey.lss`, a genuine
# LimeSurvey 6 (DBVersion 700) export of a four-language survey (base `fr`,
# additional `en`, `de`, `es`). The emitter below mirrors these facts; where
# it deliberately departs, the reason is given.
#
# * `<languages>`: one `<language>` child per language, NO CDATA. The real
#   export lists the ADDITIONAL languages first and the base language LAST
#   (`en, de, es, fr` for base `fr`) -- LimeSurvey builds it as
#   `array_merge(additionalLanguages, base)`. We emit that same order.
#   The element is only a membership set (the import takes the authority
#   from `surveys.language` / `surveys.additional_languages`), but
#   `read_lss()` reports it verbatim, so emitting primary-first would make
#   `lss$languages[1]` mean the base language on our own files and an
#   additional one on a file LimeSurvey re-exported from them -- the same
#   file, two conventions. The base language is read from `base_language`
#   (`surveys.language`), never from `languages[1]`. A monolingual file has
#   one child either way.
# * `surveys`: `language` holds the base language alone; additional
#   languages live in `additional_languages` as a single SPACE-separated
#   string (`en de es`), in their own order, base language excluded.
# * `surveys_languagesettings`: ONE row per language, all fields repeated,
#   `surveyls_language` telling them apart. Per-language in the real file:
#   `surveyls_title`, `surveyls_description`, `surveyls_welcometext`,
#   `surveyls_endtext`, `surveyls_policy_notice`,
#   `surveyls_policy_notice_label`, `surveyls_policy_error`,
#   `surveyls_url`, `surveyls_urldescription`, `surveyls_alias`,
#   `surveyls_dateformat` (it really does differ: 5 for `de`, 2 for `en`),
#   `surveyls_numberformat`, `surveyls_attributecaptions`, `attachments`
#   and the eight e-mail template fields. The spec localizes title, welcome
#   and end text; the remaining fields repeat their default in every row,
#   exactly as the real export repeats an untranslated e-mail template,
#   unless `settings` gives one of them a value keyed by language code --
#   which is how `surveyls_dateformat` stops being the base language's
#   date picker for every respondent.
# * `group_l10ns`, `question_l10ns`, `answer_l10ns`: one row per entity AND
#   per language, GROUPED BY ENTITY -- all the languages of one gid / qid /
#   aid sit together, never one full pass per language. `question_l10ns`
#   covers questions and subquestions alike (subquestions are rows of
#   `<subquestions>` with their own qid, and their texts land in the same
#   l10n table). The language order WITHIN a group is not stable in the
#   real export (it is insertion order in the database: `de, en, es, fr` in
#   `answer_l10ns` and `question_l10ns`, `fr, es, en, de` in `group_l10ns`),
#   so we use the declared order, primary first, which is deterministic.
#   The `id` column is a plain surrogate key; we keep the single shared
#   counter the monolingual emitter already used.
# * `quota_languagesettings`: one row per quota AND per language, again
#   grouped by quota, with `quotals_language`, `quotals_name` and
#   `quotals_message` per language. The `quota` row itself is
#   language-less. Real exports leave `quotals_name` EMPTY, keeping the
#   label only in the language-less `quota.name`; we fill it per language
#   deliberately, as an improvement rather than an oversight -- the column
#   exists and is NOT NULL, so a filled value imports exactly as an empty
#   one, and it gives the administration screens a translated quota label
#   where LimeSurvey itself shows none. The language-less `quota.name`
#   necessarily keeps the primary-language wording.
# * `question_attributes`: the `language` column is EMPTY for global
#   attributes and carries the language code for localized ones. A
#   localized attribute emitted without its language code is silently
#   ignored on import and LimeSurvey shows its own default wording. The
#   thirteen localized attribute names attested in the real export are
#   `lss_i18n_attributes` below; every other attribute it uses
#   (`answer_order`, `max_answers`, `exclude_all_others`, `other_position`,
#   `time_limit_message_delay`, ...) is language-less. The emitter produces
#   `other_replace_text` itself, and an author-supplied
#   `question$attributes` entry whose name is in that vector is emitted once
#   per language as well -- a plain value repeated in every language, a
#   per-language named value resolved language by language. Every other
#   attribute stays a language-less pass-through. Rows of one localized
#   attribute sit together in the real export, which is what the
#   per-attribute loop below produces.

# The thirteen question attributes LimeSurvey stores PER LANGUAGE, read off
# `inst/extdata/demo_survey.lss` (four languages, four rows each, every row
# carrying its `<language>`; every other attribute in that file has an empty
# one). Emitting any of these without a language code makes LimeSurvey
# ignore the row and fall back to its own wording, in every language -- so
# they are the names `lss_emitter()` fans out over the declared languages.
# Note that only the four `*_message` members of the `time_limit_*` family
# are localized: `time_limit_message_delay`, `time_limit_message_style` and
# the rest are language-less.
lss_i18n_attributes <- c(
  "choice_title", "dualscale_headerA", "dualscale_headerB",
  "em_validation_q_tip", "other_replace_text", "prefix", "printable_help",
  "rank_title", "suffix", "time_limit_countdown_message",
  "time_limit_message", "time_limit_warning_2_message",
  "time_limit_warning_message"
)

#' Read one language out of a localized value, strictly
#'
#' The emitter's counterpart to `loc_text()`. A rendering path reads a real
#' `.lss`, which may genuinely lack a translation, so `loc_text()` falls back
#' to the primary text; the emitter reads a *validated spec*, where every
#' declared language is mandatory, and a fallback there would write the
#' primary wording under another language code -- a wrong translation
#' silently emitted instead of an error. Hence: absent language, abort.
#'
#' A plain (unnamed) value is not localized at all and applies to every
#' language, which is what makes `attributes = list(prefix = "CHF")` work
#' alongside `attributes = list(prefix = c(fr = "CHF", en = "CHF"))`.
#' @keywords internal
#' @noRd
loc_strict <- function(x, language, what, default = "") {
  if (is.null(x) || !length(x)) return(default)
  if (is.null(names(x))) return(if (is.list(x)) x[[1L]] else x)
  if (!language %in% names(x)) {
    lssdoc_abort(
      c(paste0("The ", what, " has no value for the declared language {.val {language}}."),
        "i" = "Given: {.val {names(x)}}."),
      class = "lssdoc_bad_spec"
    )
  }
  x[[language]]
}

#' Build the XML document for a validated spec
#' @keywords internal
#' @noRd
lss_emitter <- function(spec, sid, settings) {
  languages <- as.character(spec$languages %||% spec$language)
  lang <- languages[[1L]]
  st <- new.env(parent = emptyenv())
  st$answers <- list(); st$answer_l10ns <- list()
  st$groups <- list(); st$group_l10ns <- list()
  st$questions <- list(); st$subquestions <- list()
  st$question_l10ns <- list(); st$qattrs <- list()
  st$gid <- sid * 10L; st$qid <- sid * 100L
  st$aid <- sid * 100L; st$lid <- 1L

  defined <- list()
  qid_of <- list()

  for (gi in seq_along(spec$groups)) {
    g <- spec$groups[[gi]]
    st$gid <- st$gid + 1L
    st$groups[[length(st$groups) + 1L]] <- list(
      gid = st$gid, sid = sid, group_order = gi,
      randomization_group = "", grelevance = "1")
    g_label <- paste0("group ", gi)
    for (lg in languages) {
      st$group_l10ns[[length(st$group_l10ns) + 1L]] <- list(
        id = st$lid, gid = st$gid,
        group_name = loc_strict(g$title, lg, paste0("title of ", g_label)),
        description = loc_strict(g$description, lg,
                                 paste0("description of ", g_label)),
        language = lg, sid = sid, group_order = gi,
        randomization_group = "", grelevance = "1")
      st$lid <- st$lid + 1L
    }

    for (qi in seq_along(g$questions)) {
      q <- g$questions[[qi]]
      q_label <- paste0("question ", esc(q$code %||% ""))
      map <- kind_row(q$kind)
      st$qid <- st$qid + 1L
      parent <- st$qid
      qid_of[[q$code]] <- parent

      other_opt <- Filter(function(o) isTRUE(o$other), q[["options"]] %||% list())
      opts <- Filter(function(o) !isTRUE(o$other), q[["options"]] %||% list())

      st$questions[[length(st$questions) + 1L]] <- list(
        qid = parent, parent_qid = 0L, sid = sid, gid = st$gid,
        type = map$type, title = q$code, preg = "",
        other = if (length(other_opt)) "Y" else "N",
        mandatory = if (isTRUE(q$mandatory)) "Y" else "N",
        question_order = qi, scale_id = 0L, same_default = 0L,
        relevance = translate_relevance(q$relevance, defined),
        modulename = "", encrypted = "N",
        question_theme_name = map$theme, same_script = 0L)
      for (lg in languages) {
        st$question_l10ns[[length(st$question_l10ns) + 1L]] <- list(
          id = st$lid, qid = parent,
          question = loc_strict(q$text, lg, paste0("text of ", q_label)),
          help = loc_strict(q$help, lg, paste0("help of ", q_label)),
          language = lg, script = "")
        st$lid <- st$lid + 1L
      }

      add_answers <- function(items) {
        for (k in seq_along(items)) {
          it <- items[[k]]
          st$aid <- st$aid + 1L
          st$answers[[length(st$answers) + 1L]] <- list(
            aid = st$aid, qid = parent, code = it$code,
            sortorder = k - 1L, assessment_value = 0L, scale_id = 0L)
          for (lg in languages) {
            st$answer_l10ns[[length(st$answer_l10ns) + 1L]] <- list(
              id = st$lid, aid = st$aid,
              answer = loc_strict(it$text, lg,
                                  paste0("text of answer ", k, " of ", q_label)),
              language = lg)
            st$lid <- st$lid + 1L
          }
        }
      }
      add_subquestions <- function(items) {
        for (k in seq_along(items)) {
          it <- items[[k]]
          st$qid <- st$qid + 1L
          st$subquestions[[length(st$subquestions) + 1L]] <- list(
            qid = st$qid, parent_qid = parent, sid = sid, gid = st$gid,
            type = map$type, title = it$code, preg = "", other = "N",
            mandatory = "N", question_order = k, scale_id = 0L,
            same_default = 0L, relevance = "1", modulename = "",
            encrypted = "N", question_theme_name = "", same_script = 0L)
          for (lg in languages) {
            st$question_l10ns[[length(st$question_l10ns) + 1L]] <- list(
              id = st$lid, qid = st$qid,
              question = loc_strict(
                it$text, lg,
                paste0("text of subquestion ", k, " of ", q_label)),
              help = "", language = lg, script = "")
            st$lid <- st$lid + 1L
          }
        }
      }

      # subquestions before answers: the array branch emitted rows then
      # columns, and that order fixes every qid / aid / lid in the file
      pick <- function(field) if (identical(field, "options")) opts else q[[field]]
      if (!is.na(map$subquestions_from)) add_subquestions(pick(map$subquestions_from))
      if (!is.na(map$answers_from)) add_answers(pick(map$answers_from))

      attr_add <- function(name, value, language = "") {
        st$qattrs[[length(st$qattrs) + 1L]] <- list(
          qid = parent, attribute = name, value = value, language = language)
      }
      if (!is.null(q$max_answers)) attr_add("max_answers", q$max_answers)
      if (isTRUE(map$implicit_min_answers) &&
          (isTRUE(q$mandatory) || !is.null(q$max_answers)) &&
          is.null((q$attributes %||% list())[["min_answers"]])) {
        attr_add("min_answers", 1L)
      }
      if (length(other_opt)) {
        # localized attribute: MUST carry the language code, or LimeSurvey
        # silently ignores it and shows its default "Other:" wording -- one
        # row per declared language, the rows of one attribute together
        for (lg in languages) {
          attr_add("other_replace_text",
                   loc_strict(other_opt[[1L]]$text, lg,
                              paste0("other label of ", q_label)),
                   language = lg)
        }
        if (!is.null(q$other_position)) {
          attr_add("other_position", q$other_position)
          if (identical(q$other_position, "specific")) {
            attr_add("other_position_code", q$other_position_code)
          }
        }
      }
      excl <- option_codes(Filter(function(o) isTRUE(o$exclusive), opts))
      if (isTRUE(map$exclusive_allowed) && length(excl)) {
        attr_add("exclude_all_others", paste(excl, collapse = ";"))
      }
      # a localized attribute name gets one row per declared language, each
      # carrying its code; every other name passes through language-less
      for (nm in names(q$attributes %||% list())) {
        value <- q$attributes[[nm]]
        if (nm %in% lss_i18n_attributes) {
          for (lg in languages) {
            attr_add(nm,
                     loc_strict(value, lg,
                                paste0("attribute ", esc(nm), " of ", q_label)),
                     language = lg)
          }
        } else {
          attr_add(nm, value)
        }
      }

      defined[[q$code]] <- q
    }
  }

  doc <- xml2::xml_new_root("document")
  xml2::xml_add_child(doc, "LimeSurveyDocType", "Survey")
  xml2::xml_add_child(doc, "DBVersion", LSS_DBVERSION)
  langs <- xml2::xml_add_child(doc, "languages")
  # LimeSurvey's own order: additional languages first, base language LAST
  # (`array_merge(additionalLanguages, base)`) -- see the notes above. One
  # child, unchanged, when a single language is declared.
  for (lg in c(languages[-1L], languages[[1L]])) {
    xml2::xml_add_child(langs, "language", lg)
  }

  add_section(doc, "answers",
              c("aid", "qid", "code", "sortorder", "assessment_value", "scale_id"),
              st$answers)
  add_section(doc, "answer_l10ns", c("id", "aid", "answer", "language"),
              st$answer_l10ns)
  add_section(doc, "groups",
              c("gid", "sid", "group_order", "randomization_group", "grelevance"),
              st$groups)
  add_section(doc, "group_l10ns",
              c("id", "gid", "group_name", "description", "language", "sid",
                "group_order", "randomization_group", "grelevance"),
              st$group_l10ns)
  question_fields <- c(
    "qid", "parent_qid", "sid", "gid", "type", "title", "preg", "other",
    "mandatory", "question_order", "scale_id", "same_default", "relevance",
    "modulename", "encrypted", "question_theme_name", "same_script")
  add_section(doc, "questions", question_fields, st$questions)
  add_section(doc, "subquestions", question_fields, st$subquestions)
  add_section(doc, "question_l10ns",
              c("id", "qid", "question", "help", "language", "script"),
              st$question_l10ns)
  add_section(doc, "question_attributes",
              c("qid", "attribute", "value", "language"), st$qattrs)

  if (length(spec$quotas)) {
    quota <- list(); members <- list(); qls <- list()
    for (k in seq_along(spec$quotas)) {
      qu <- spec$quotas[[k]]
      qu_label <- paste0("quota ", k)
      # the language-less admin label: the primary-language wording, since
      # `quota.name` has no room for more
      quota_name <- loc_strict(qu$name, lang, paste0("name of ", qu_label),
                               default = NULL) %||% qu$question
      quota[[k]] <- list(id = k, sid = sid, name = quota_name,
                         qlimit = as.integer(qu$limit %||% 0L),
                         action = 1L, active = 1L,
                         autoload_url = 0L)
      members[[k]] <- list(id = k, sid = sid, qid = qid_of[[qu$question]],
                           quota_id = k, code = qu$code)
      # one row per language, grouped by quota; `quotals_id` is a surrogate
      # key running over the whole section, so it stays `k` when there is a
      # single language
      for (lg in languages) {
        qls_id <- length(qls) + 1L
        qls[[qls_id]] <- list(
          quotals_id = qls_id, quotals_quota_id = k,
          quotals_language = lg,
          quotals_name = loc_strict(qu$name, lg, paste0("name of ", qu_label),
                                    default = NULL) %||% qu$question,
          quotals_message = loc_strict(qu$message, lg,
                                       paste0("message of ", qu_label)),
          quotals_url = "", quotals_urldescrip = "")
      }
    }
    add_section(doc, "quota",
                c("id", "sid", "name", "qlimit", "action", "active", "autoload_url"),
                quota)
    add_section(doc, "quota_members",
                c("id", "sid", "qid", "quota_id", "code"), members)
    add_section(doc, "quota_languagesettings",
                c("quotals_id", "quotals_quota_id", "quotals_language",
                  "quotals_name", "quotals_message", "quotals_url",
                  "quotals_urldescrip"), qls)
  }

  # the defaults already carry `language` and `additional_languages`:
  # overriding them in place avoids emitting a duplicated field
  surveys_row <- lss_default_surveys_fields
  surveys_row[["language"]] <- lang
  # LimeSurvey stores the additional languages as one space-separated string
  surveys_row[["additional_languages"]] <- paste(languages[-1L], collapse = " ")
  # `settings` addresses two tables: the single `surveys` row, and the
  # per-language `surveys_languagesettings` rows, whose fields LimeSurvey
  # really does vary by language (`surveyls_dateformat` is 5 for `de` and 2
  # for `en` in the reference export). A language-settings field therefore
  # takes either one value for every language or one value per language.
  ls_settings <- list()
  if (length(settings)) {
    if (is.null(names(settings)) || any(!nzchar(names(settings)))) {
      lssdoc_abort("Every {.arg settings} element must be named.",
                   class = "lssdoc_bad_settings")
    }
    reserved <- intersect(names(settings), c("language", "additional_languages"))
    if (length(reserved)) {
      lssdoc_abort(
        c("{.arg settings} cannot override {.val {reserved}}.",
          "i" = "The survey language comes from the spec; overriding it here would leave the localized sections in another language."),
        class = "lssdoc_bad_settings"
      )
    }
    ls_names <- intersect(names(settings), names(lss_default_language_settings))
    ls_settings <- settings[ls_names]
    settings <- settings[setdiff(names(settings), ls_names)]
    bad_ls <- vapply(ls_settings, function(v) {
      if (is.null(v) || !length(v)) return(TRUE)
      if (is.null(names(v))) {
        return(length(v) != 1L || !is.character(v) || is.na(v))
      }
      any(!nzchar(names(v))) ||
        !all(vapply(v, function(one) {
          is.character(one) && length(one) == 1L && !is.na(one)
        }, logical(1)))
    }, logical(1))
    if (any(bad_ls)) {
      lssdoc_abort(
        c("Invalid {.arg settings} value{?s} for {.val {names(ls_settings)[bad_ls]}}.",
          "i" = "A per-language field takes a single character string, or a list or vector keyed by language code."),
        class = "lssdoc_bad_settings"
      )
    }
    unknown <- setdiff(names(settings), names(surveys_row))
    if (length(unknown)) {
      lssdoc_abort("Unknown {.arg settings} field{?s}: {.val {unknown}}.",
                   class = "lssdoc_bad_settings")
    }
    bad_value <- vapply(settings, function(v) {
      is.null(v) || length(v) != 1L || is.na(v) || !is.character(v)
    }, logical(1))
    if (any(bad_value)) {
      lssdoc_abort(
        c("Invalid {.arg settings} value{?s} for {.val {names(settings)[bad_value]}}.",
          "i" = "Each value must be a single character string, as LimeSurvey stores it (e.g. \"Y\", \"N\", \"I\")."),
        class = "lssdoc_bad_settings"
      )
    }
    surveys_row <- utils::modifyList(surveys_row, settings)
  }
  surveys_row <- c(list(sid = sid), surveys_row)
  add_section(doc, "surveys", names(surveys_row), list(surveys_row))

  # one row per language; the fields the spec does not localize repeat their
  # default in every row, as a real export repeats an untranslated template
  ls_rows <- lapply(languages, function(lg) {
    ls_fields <- lss_default_language_settings
    for (nm in names(ls_settings)) {
      ls_fields[[nm]] <- loc_strict(ls_settings[[nm]], lg,
                                    paste0("{.arg settings} value for ",
                                           esc(nm)))
    }
    c(list(surveyls_survey_id = sid, surveyls_language = lg,
           surveyls_title = loc_strict(spec$title, lg, "survey title"),
           surveyls_welcometext = as_html_block(
             loc_strict(spec$welcome, lg, "welcome text", default = NULL)),
           surveyls_endtext = as_html_block(
             loc_strict(spec$end_text, lg, "end text", default = NULL))),
      ls_fields)
  })
  add_section(doc, "surveys_languagesettings", names(ls_rows[[1L]]), ls_rows)

  list(doc = doc,
       n_groups = length(st$groups),
       n_questions = sum(vapply(
         st$questions,
         function(q) q$type %in% lss_kinds$type[lss_kinds$collects_response],
         logical(1))))
}

#' Wrap plain paragraphs in <p> tags; pass HTML through verbatim
#' @keywords internal
#' @noRd
as_html_block <- function(x) {
  if (is.null(x) || !length(x)) return("")
  x <- as.character(x)
  if (length(x) == 1L && grepl("^\\s*<", x)) return(x)
  paste0("<p>", paste(x, collapse = "</p><p>"), "</p>")
}

#' Append one `<fields>/<rows>` section to the document
#' @keywords internal
#' @noRd
add_section <- function(doc, name, fields, rows) {
  sec <- xml2::xml_add_child(doc, name)
  fl <- xml2::xml_add_child(sec, "fields")
  for (f in fields) xml2::xml_add_child(fl, "fieldname", f)
  rs <- xml2::xml_add_child(sec, "rows")
  for (r in rows) {
    row <- xml2::xml_add_child(rs, "row")
    for (f in fields) {
      v <- r[[f]]
      xml2::xml_add_child(row, f, if (is.null(v)) "" else as.character(v))
    }
  }
  invisible(sec)
}
