# The Word authoring form: render an `lss_spec` as a fill-in questionnaire.
#
# The review templates (`cards`, `table`) answer "what does this survey
# contain?"; the form answers "what do I have to type to author one?". It is
# the writing-side counterpart of `render_questionnaire()`: one Word table per
# block (Survey, Group, Question, Quota), two columns, key on the left and
# value on the right, every key a localized label recognized by its TEXT and
# never by its position. `read_form_docx()` (0.3.0, step 3) parses these
# documents back into an `lss_spec`, so every decision here is half of a
# contract:
#
# * a block is a top-level two-column table whose first row is its title row;
# * two empty paragraphs separate two blocks (Word merges adjacent tables);
# * a value cell holds content and nothing else -- hints live in the KEY cell,
#   after a soft return, where the reader's "first line of the key cell" rule
#   discards them;
# * several values in one cell are separated by soft returns (flextable emits
#   one paragraph per cell, so `"\n"` is the only separator available), which
#   is why a single-line field carrying a line break is refused outright
#   rather than written as something the reader would read back as two values;
# * cells are NEVER merged: `merge_at()` / `merge_h()` / `merge_v()` would make
#   the document unreadable to the parser, which refuses `w:gridSpan` and
#   `w:vMerge`;
# * the contract version travels in the custom document property
#   `lssdoc-template-version` (see `LSS_FORM_VERSION`).

# ---- example specification ---------------------------------------------------

#' Pick one wording, or build a localized one over several languages
#'
#' With a single language the value is the plain French or English string,
#' exactly what the monolingual generator has always produced. With several,
#' it is a named list over the declared languages -- the shape
#' `spec_localize()` canonicalizes -- so every declared language carries every
#' text. French and English are real translations; any other declared language
#' reuses the English wording with a `[code]` tag, which is honest about being
#' untranslated while still filling the language LimeSurvey expects.
#' @keywords internal
#' @noRd
example_loc <- function(fr, en, languages) {
  if (length(languages) == 1L) {
    return(if (identical(languages[[1L]], "fr")) fr else en)
  }
  out <- lapply(languages, function(lg) {
    if (identical(lg, "fr")) fr
    else if (identical(lg, "en")) en
    else paste0(en, " [", lg, "]")
  })
  names(out) <- languages
  out
}

#' Wording of the generated example questionnaire, one entry per kind
#'
#' Kept as data rather than as a chain of `switch()` calls so the blank
#' template, the tests and the vignette read the same table. Each entry gives
#' the French and English wording; a chrome language without its own wording
#' falls back to English, exactly as the rest of the package does. Given
#' several languages, every text becomes a named list over them.
#' @param languages The declared survey languages, primary first. A single
#'   code keeps the historical behaviour: `"fr"` French, anything else
#'   English.
#' @keywords internal
#' @noRd
lss_example_wording <- function(languages = "fr") {
  tr <- function(fr, en) example_loc(fr, en, languages)
  o <- function(...) lapply(list(...), function(x) list(text = x))
  list(
    single = list(
      text = tr("Acceptez-vous de participer a cette enquete ?",
                "Do you agree to take part in this survey?"),
      help = tr("Une seule reponse possible.", "One answer only."),
      mandatory = TRUE,
      other_position = "end",
      options = c(
        o(tr("Oui, je participe", "Yes, I take part"),
          tr("Non, je refuse", "No, I decline")),
        list(list(text = tr("Autre situation, merci de preciser",
                            "Another situation, please specify"),
                  other = TRUE)))),
    dropdown = list(
      text = tr("Dans quelle region travaillez-vous ?",
                "Which region do you work in?"),
      other_position = "beginning",
      options = c(
        o(tr("Suisse romande", "Western Switzerland"),
          tr("Suisse alemanique", "German-speaking Switzerland"),
          tr("Tessin", "Ticino")),
        list(list(text = tr("Autre region, merci de preciser",
                            "Another region, please specify"),
                  other = TRUE)))),
    singlecomment = list(
      text = tr("Quel est votre statut professionnel ?",
                "What is your employment status?"),
      help = tr("Vous pourrez commenter votre reponse.",
                "You may comment on your answer."),
      options = o(tr("Salarie-e", "Employee"),
                  tr("Independant-e", "Self-employed"),
                  tr("Sans activite professionnelle", "Not in employment"))),
    multiple = list(
      text = tr("Quelles prestations utilisez-vous ?",
                "Which services do you use?"),
      help = tr("Trois reponses au maximum.", "Three answers at most."),
      mandatory = TRUE,
      max_answers = 3L,
      other_position = "specific", other_position_code = "3",
      options = c(
        o(tr("La cantine", "The canteen"),
          tr("La creche", "Childcare"),
          tr("Le transport scolaire", "School transport")),
        list(list(text = tr("Aucune de ces prestations", "None of these services"),
                  exclusive = TRUE),
             list(text = tr("Autre prestation, merci de preciser",
                            "Another service, please specify"),
                  other = TRUE)))),
    ranking = list(
      text = tr("Classez ces aspects du travail, du plus important au moins important.",
                "Rank these aspects of work, from most to least important."),
      help = tr("Classez exactement trois elements.", "Rank exactly three items."),
      mandatory = TRUE,
      max_answers = 3L,
      attributes = list(min_answers = "3"),
      options = o(tr("La securite de l'emploi", "Job security"),
                  tr("Le salaire", "Pay"),
                  tr("L'ambiance d'equipe", "Team atmosphere"),
                  tr("L'autonomie", "Autonomy"),
                  tr("La formation continue", "Continuing education"))),
    array = list(
      text = tr("Dans quelle mesure etes-vous d'accord avec ces affirmations ?",
                "To what extent do you agree with these statements?"),
      rows = o(tr("Le service est rapide", "The service is fast"),
               tr("Le personnel est disponible", "The staff is available"),
               tr("Les horaires me conviennent", "The opening hours suit me")),
      columns = o(tr("Pas du tout d'accord", "Strongly disagree"),
                  tr("Plutot pas d'accord", "Somewhat disagree"),
                  tr("Plutot d'accord", "Somewhat agree"),
                  tr("Tout a fait d'accord", "Strongly agree"))),
    array5 = list(
      text = tr("Notez ces aspects de 1 a 5.", "Rate these aspects from 1 to 5."),
      rows = o(tr("Ma satisfaction globale", "My overall satisfaction"),
               tr("Mon equilibre vie privee / travail", "My work-life balance"))),
    array10 = list(
      text = tr("Notez ces aspects de 1 a 10.", "Rate these aspects from 1 to 10."),
      rows = o(tr("La qualite des outils informatiques", "The quality of the IT tools"),
               tr("La qualite des locaux", "The quality of the premises"))),
    arrayyesno = list(
      text = tr("Ces evenements vous concernent-ils ?",
                "Do these events apply to you?"),
      rows = o(tr("J'ai eu un entretien annuel", "I had an annual review"),
               tr("J'ai recu un plan de formation", "I received a training plan"))),
    arraytrend = list(
      text = tr("Depuis un an, ces elements ont-ils augmente, diminue ou sont-ils restes identiques ?",
                "Over the past year, have these increased, decreased or stayed the same?"),
      rows = o(tr("Ma charge de travail", "My workload"),
               tr("Mon interet pour le poste", "My interest in the job"))),
    multitext = list(
      text = tr("Decrivez votre poste en quelques mots.",
                "Describe your position in a few words."),
      options = o(tr("Fonction", "Job title"),
                  tr("Service", "Department"),
                  tr("Lieu de travail", "Place of work"))),
    multinumeric = list(
      text = tr("Quelques chiffres sur votre poste.",
                "A few figures about your position."),
      options = o(tr("Annees dans l'institution", "Years in the institution"),
                  tr("Heures hebdomadaires", "Weekly hours"),
                  tr("Personnes encadrees", "People supervised"))),
    text = list(
      text = tr("Qu'est-ce qui ameliorerait le plus votre quotidien de travail ?",
                "What would most improve your working day?"),
      help = tr("Quelques phrases suffisent.", "A few sentences are enough.")),
    shorttext = list(
      text = tr("En un mot, votre etat d'esprit aujourd'hui ?",
                "In one word, how do you feel today?")),
    hugetext = list(
      text = tr("Souhaitez-vous ajouter quelque chose ?",
                "Would you like to add anything?")),
    numeric = list(
      text = tr("Combien d'annees d'experience avez-vous ?",
                "How many years of experience do you have?")),
    date = list(
      text = tr("Quelle est la date de votre entree en fonction ?",
                "When did you take up your position?")),
    yesno = list(
      text = tr("Avez-vous suivi une formation cette annee ?",
                "Did you take any training this year?")),
    gender = list(
      text = tr("Quel est votre genre ?", "What is your gender?")),
    fivepoint = list(
      text = tr("Sur une echelle de 1 a 5, recommanderiez-vous votre employeur ?",
                "On a scale of 1 to 5, would you recommend your employer?")),
    display = list(
      text = tr("Merci d'avoir repondu jusqu'ici. Les dernieres questions portent sur vous.",
                "Thank you for answering so far. The last questions are about you."))
  )
}

#' A ready-made example specification, one question per requested kind
#'
#' The source of the blank form template: `lss_template_docx()` renders the
#' spec this function builds, so the template can never show a field the
#' validator would refuse. Every question is filled with each field its kind
#' supports (an other option and its position where allowed, an exclusive
#' option and a cap for `multiple`, rows and columns for `array`, `min_answers`
#' for `ranking`), so the template documents the syntax by example.
#'
#' Questions are ordered as `lss_kinds` and split into two groups by family
#' (closed questions first, free input and displayed text second); an empty
#' group is dropped, so any subset of kinds yields a valid spec. Filters and
#' the quota are attached only when the question they cite is present, for the
#' same reason.
#'
#' @param kinds Character vector of kinds to include, `lss_kinds$kind` by
#'   default. Unknown kinds are an error; the order of `lss_kinds` is kept.
#' @param lang Language of the wording AND the declared survey language:
#'   `"fr"` uses the French wording, anything else the English one.
#' @param languages The declared survey languages, primary first; the single
#'   `lang` by default. With several, every localizable text is given in each
#'   -- French and English really translated, any other language the English
#'   wording with a `[code]` tag -- so the example exercises the multilingual
#'   path of [write_lss()] and of the Word form. `languages[1]` wins over
#'   `lang` when both are given.
#' @return An `lss_spec`.
#' @keywords internal
#' @noRd
lss_example_spec <- function(kinds = lss_kinds$kind, lang = "fr",
                             languages = lang) {
  if (!is.character(kinds) || !length(kinds) || anyNA(kinds)) {
    lssdoc_abort("{.arg kinds} must be a non-empty character vector of kinds.",
                 class = "lssdoc_bad_spec")
  }
  unknown <- setdiff(kinds, lss_kinds$kind)
  if (length(unknown)) {
    lssdoc_abort(
      c("Unknown kind{?s} {.val {unknown}}.",
        "i" = "Authorable kinds: {.val {lss_kinds$kind}}."),
      class = "lssdoc_bad_spec"
    )
  }
  kinds <- lss_kinds$kind[lss_kinds$kind %in% kinds]
  if (!is.character(languages) || !length(languages) || anyNA(languages)) {
    lssdoc_abort(
      "{.arg languages} must be a non-empty character vector of language codes.",
      class = "lssdoc_bad_spec")
  }
  lang <- languages[[1L]]
  wording <- lss_example_wording(languages)
  tr <- function(fr, en) example_loc(fr, en, languages)

  camel <- function(k) paste0(toupper(substr(k, 1L, 1L)), substr(k, 2L, nchar(k)))
  questions <- lapply(seq_along(kinds), function(i) {
    k <- kinds[[i]]
    w <- wording[[k]]
    q <- c(list(code = sprintf("Q%02d%s", i, camel(k)), kind = k), w)
    q
  })
  names(questions) <- kinds

  fam <- lss_kinds$family[match(kinds, lss_kinds$kind)]
  closed <- questions[fam %in% c("choice", "array", "battery")]
  open <- questions[fam %in% c("scalar", "display")]
  groups <- list(
    list(title = tr("Questions fermees", "Closed questions"),
         description = tr("Listes, cases a cocher, classements et tableaux.",
                          "Lists, checkboxes, rankings and arrays."),
         questions = unname(closed)),
    list(title = tr("Saisie libre, echelles simples et texte affiche",
                    "Free input, simple scales and displayed text"),
         description = tr("Champs texte, nombres, dates et echelles implicites.",
                          "Text fields, numbers, dates and implicit scales."),
         questions = unname(open))
  )
  groups <- Filter(function(g) length(g$questions) > 0L, groups)

  # Filters cite a question asked EARLIER, so they are attached on the
  # flattened order, never on the kind order.
  flat <- unlist(lapply(groups, `[[`, "questions"), recursive = FALSE)
  codes <- vapply(flat, `[[`, character(1), "code")
  role <- vapply(flat, function(q) kind_field(q$kind, "relevance_role"), character(1))
  relevance <- rep(NA_character_, length(flat))
  names(relevance) <- codes
  scal <- which(role == "scalar")
  if (length(scal) && scal[[1L]] < length(flat)) {
    target <- flat[[scal[[1L]]]]
    # the questions are not normalized yet (lss_spec() runs at the end), so
    # the first answer code is read from the kind's implicit scale, or from
    # the auto-numbering rule the normalizer is about to apply
    value <- kind_implicit_codes(target$kind)
    if (is.null(value)) {
      coded <- Filter(function(o) !isTRUE(o$other), target$options %||% list())
      value <- as.character(coded[[1L]]$code %||% lss_spec_defaults$option_code_from)
    }
    relevance[[scal[[1L]] + 1L]] <- paste0(target$code, " = ", value[[1L]])
  }
  cnt <- which(role == "count")
  if (length(cnt) && cnt[[1L]] < length(flat) &&
      is.na(relevance[[cnt[[1L]] + 1L]])) {
    relevance[[cnt[[1L]] + 1L]] <- paste0("count(", flat[[cnt[[1L]]]]$code, ") >= 2")
  }
  groups <- lapply(groups, function(g) {
    g$questions <- lapply(g$questions, function(q) {
      rel <- relevance[[q$code]]
      if (!is.na(rel)) q$relevance <- rel
      q
    })
    g
  })

  quotas <- NULL
  if ("single" %in% kinds) {
    quotas <- list(list(
      name = tr("Refus de participer", "Declined to take part"),
      question = questions[["single"]]$code, code = "2", limit = 0L,
      message = tr(
        "Vous avez indique ne pas souhaiter participer. Le questionnaire s'arrete ici. Merci.",
        "You indicated that you do not wish to take part. The questionnaire ends here. Thank you.")
    ))
  }

  # the welcome text is TWO paragraphs, so it is localized as a whole: one
  # character vector per language, never `c()` of two localized values --
  # that would collide the language names
  lss_spec(
    title = tr("Questionnaire d'exemple lssdoc", "lssdoc example questionnaire"),
    languages = languages,
    welcome = example_loc(
      c("Bienvenue dans ce questionnaire d'exemple.",
        "Il montre une question par type autorisable."),
      c("Welcome to this example questionnaire.",
        "It shows one question per authorable kind."),
      languages),
    end_text = tr("Merci d'avoir repondu.", "Thank you for answering."),
    groups = groups,
    quotas = quotas
  )
}

# ---- single-line discipline ---------------------------------------------------

#' Refuse a line break inside a single-line field
#'
#' An option label, a title or a quota name written on two lines would be read
#' back as TWO values (the reader treats a paragraph and a soft return alike),
#' silently turning one option into two. Better to refuse at write time, where
#' the author still knows which text they meant.
#' @keywords internal
#' @noRd
form_abort_newline <- function(where, field, lines) {
  lssdoc_abort(
    c(paste0("In ", esc(where), ", field {.field ", esc(field),
             "}: a line break is not allowed in a single-line field."),
      "x" = paste0("Line 1: {.val ", esc(lines[[1L]]), "}"),
      "x" = paste0("Line 2: {.val ", esc(lines[[2L]]), "}"),
      "i" = "Keep the value on one line, or move the extra text to a field that accepts several lines."),
    class = c("lssdoc_bad_form_value", "lssdoc_bad_form")
  )
}

#' One line of text, or a classed refusal
#' @keywords internal
#' @noRd
form_single_line <- function(x, where, field) {
  x <- as.character(x %||% "")
  x <- x[!is.na(x)]
  if (!length(x)) return("")
  lines <- unlist(strsplit(paste(x, collapse = "\n"), "\r\n|\r|\n"))
  if (length(lines) > 1L) form_abort_newline(where, field, lines)
  if (!length(lines)) "" else lines[[1L]]
}

#' Several lines of text: a character vector, each element split on newlines
#' @keywords internal
#' @noRd
form_text_lines <- function(x) {
  x <- as.character(x %||% "")
  x <- x[!is.na(x)]
  if (!length(x)) return("")
  unlist(strsplit(x, "\r\n|\r|\n"))
}

#' The text of a localizable value in one language, or `NULL`
#' @keywords internal
#' @noRd
form_lang_text <- function(value, lg) {
  if (is.null(value)) return(NULL)
  if (!is.list(value)) return(as.character(value))
  value[[lg]]
}

#' `<label>` or `<label> [<code>]`
#'
#' The suffix is explicit on every language, including the primary one, as soon
#' as the spec declares more than one: explicit beats implicit and the
#' round-trip stays stable.
#' @keywords internal
#' @noRd
form_key_label <- function(label, code, multi) {
  if (isTRUE(multi)) paste0(label, " [", code, "]") else label
}

# ---- row builders --------------------------------------------------------------

#' One key/value row of a block
#' @keywords internal
#' @noRd
form_row <- function(key, lines, hint = NULL, mono = FALSE) {
  list(key = key, lines = lines, hint = hint, mono = isTRUE(mono))
}

#' One row per declared language for a localizable field
#' @keywords internal
#' @noRd
form_localized_rows <- function(label, value, languages, hint = NULL,
                                single_line = FALSE, where = NULL,
                                lines_of = NULL) {
  multi <- length(languages) > 1L
  lapply(languages, function(lg) {
    raw <- form_lang_text(value, lg)
    lines <- if (!is.null(lines_of)) {
      lines_of(lg)
    } else if (isTRUE(single_line)) {
      form_single_line(raw, where, label)
    } else {
      form_text_lines(raw)
    }
    form_row(form_key_label(label, lg, multi), lines, hint = hint)
  })
}

#' The words that designate the native "other" option, in every language
#'
#' Read-side vocabulary: the writer always writes the reserved word of the
#' chrome language, `read_form_docx()` (step 3) accepts any of these. One set
#' shared by both sides, so a French author writing `Autre` in an English
#' template and an English author writing
#' `Other` in a French one both get the native other option. `autre` is the
#' canonical spelling -- the one `lss_spec()`'s relevance mini-language uses --
#' and the others are accepted aliases. `-oth-` is LimeSurvey's own code.
#' The German UI says "Sonstige" where the chrome string says "Sonstiges":
#' both are in.
#' @keywords internal
#' @noRd
lss_other_keywords <- function() {
  c("other", "autre", "sonstiges", "sonstige", "otro", "altro", "-oth-")
}

#' The `code = label` lines of an option list, in one language
#'
#' Codes are always explicit: `lss_spec()` has already numbered the options, so
#' writing them back is lossless and the reader never has to re-derive a
#' numbering. The native other option carries no code; it is written with the
#' reserved word of the chrome language.
#' @keywords internal
#' @noRd
form_option_lines <- function(options, lg, chrome, where, field) {
  options <- options %||% list()
  if (!length(options)) return("")
  vapply(options, function(o) {
    txt <- form_single_line(form_lang_text(o$text, lg) %||% loc_text(o$text),
                            where, field)
    if (isTRUE(o$other)) {
      # One rule, the same in every language: the bare reserved word when the
      # spec gives NO label, `<reserved word> = <label>` as soon as it gives
      # one. Suppressing the label when it happens to read "Other" would make
      # the written line depend on the language the label is written in, and
      # would silently drop a label the author may have capitalized or
      # punctuated on purpose.
      word <- chrome$form_other
      if (!nzchar(txt)) word else paste0(word, " = ", txt)
    } else {
      paste0(o$code, " = ", txt)
    }
  }, character(1))
}

#' Value of the Filter row: the spec expression, blank for "always shown"
#' @keywords internal
#' @noRd
form_filter_value <- function(q) {
  rel <- q$relevance
  if (is.null(rel) || identical(as.character(rel), lss_spec_defaults$relevance)) {
    return("")
  }
  as.character(rel)
}

#' Value of the Position of "Other" row
#'
#' Written as a real value -- the LimeSurvey default `End` -- for every kind
#' that takes an other option and actually has one, and left blank otherwise,
#' so an author who adds an Other line never has to add a row.
#' @keywords internal
#' @noRd
form_other_position_value <- function(q, chrome) {
  has_other <- any(vapply(q[["options"]] %||% list(),
                          function(o) isTRUE(o$other), logical(1)))
  if (!has_other) return("")
  switch(
    as.character(q$other_position %||% "end"),
    "beginning" = chrome$form_other_position_beginning,
    "specific" = sprintf(chrome$form_other_position_after_fmt,
                         as.character(q$other_position_code %||% "")),
    chrome$form_other_position_end
  )
}

#' The ordered rows of a Question block
#'
#' The row SET is a pure function of the kind's row in `kinds_table`, never a
#' hand-kept list: Mandatory as soon as the kind collects a response, Options
#' when the kind requires options, Rows / Columns from the same columns of the
#' table, the two answer caps when the kind has a cap rule, the other position
#' when the kind accepts an other option. A kind added to `lss_kinds` therefore
#' gets its form rows for free, and the test that compares the two cannot drift.
#'
#' @param q A normalized question of an `lss_spec`.
#' @param kinds_table `lss_kinds`, passed in so the test can feed a subset.
#' @param chrome The chrome string list of the document language.
#' @param languages The declared content languages, primary first.
#' @param hints Add the muted syntax hints of the blank template.
#' @return A list of rows, each `list(key, lines, hint, mono)`.
#' @keywords internal
#' @noRd
form_rows_for_question <- function(q, kinds_table, chrome, languages,
                                   hints = FALSE) {
  shape <- kinds_table[match(q$kind, kinds_table$kind), , drop = FALSE]
  where <- paste0("question ", q$code %||% "")
  hint <- function(key) if (isTRUE(hints)) chrome[[key]] else NULL
  rows <- list()
  add <- function(x) rows[[length(rows) + 1L]] <<- x
  addall <- function(x) for (r in x) add(r)

  # The Type cell carries the spec kind (`single`, `array5`, ...), the first
  # of the two spellings the contract accepts. The localized type label of the
  # review templates -- lss_localized_type_label() -- is NOT written as the
  # value because it is deliberately many-to-one (it collapses L, !, F, A, B,
  # C, E, Y, G and 5 into one "Single choice"): ten labels for twenty-one
  # kinds, so a reader could not recover the kind from it. It is shown as the
  # key's hint instead, where it tells the author what the code means without
  # being read back as content.
  type_label <- lss_localized_type_label(
    list(type = shape$type, type_label = shape$label), list(chrome = chrome))
  add(form_row(
    chrome$meta_type, as.character(q$kind), mono = TRUE,
    hint = if (isTRUE(hints)) {
      paste0(type_label, " (", chrome$form_hint_type, ")")
    }))
  if (isTRUE(shape$collects_response)) {
    add(form_row(
      chrome$meta_mandatory,
      if (isTRUE(q$mandatory)) chrome$mandatory_yes else chrome$mandatory_no,
      hint = hint("form_yes_no_hint")))
  }
  add(form_row(chrome$meta_filter, form_filter_value(q),
               hint = hint("form_hint_filter"), mono = TRUE))
  # The wording row is keyed `form_wording` ("Libelle", "Wording", ...), never
  # the review label `item_question`: in English and in French the latter IS
  # the block word, and a title-row word is how the reader tells one block
  # from the next. A key that repeats it would open a block inside a block.
  addall(form_localized_rows(chrome$form_wording, q$text, languages))
  addall(form_localized_rows(chrome$item_help, q$help, languages))
  if (identical(shape$options, "required")) {
    addall(form_localized_rows(
      chrome$item_options, NULL, languages, hint = hint("form_hint_options"),
      lines_of = function(lg) {
        form_option_lines(q[["options"]], lg, chrome, where, chrome$item_options)
      }))
  }
  if (isTRUE(shape$exclusive_allowed)) {
    excl <- Filter(function(o) isTRUE(o$exclusive), q[["options"]] %||% list())
    add(form_row(
      chrome$item_exclusive,
      if (length(excl)) vapply(excl, `[[`, character(1), "code") else "",
      hint = hint("form_hint_exclusive"), mono = TRUE))
  }
  if (identical(shape$rows, "required")) {
    addall(form_localized_rows(
      chrome$form_rows, NULL, languages, hint = hint("form_hint_rows"),
      lines_of = function(lg) {
        form_option_lines(q[["rows"]], lg, chrome, where, chrome$form_rows)
      }))
  }
  if (identical(shape$columns, "required")) {
    addall(form_localized_rows(
      chrome$form_columns, NULL, languages, hint = hint("form_hint_columns"),
      lines_of = function(lg) {
        form_option_lines(q[["columns"]], lg, chrome, where, chrome$form_columns)
      }))
  }
  if (!identical(shape$max_answers_rule, "none")) {
    min_answers <- (q$attributes %||% list())[["min_answers"]]
    add(form_row(chrome$form_min_answers,
                 if (is.null(min_answers)) "" else as.character(min_answers),
                 hint = hint("form_hint_answers")))
    add(form_row(chrome$form_max_answers,
                 if (is.null(q$max_answers)) "" else as.character(q$max_answers),
                 hint = hint("form_hint_answers")))
  }
  if (isTRUE(shape$other_allowed)) {
    add(form_row(chrome$form_other_position,
                 form_other_position_value(q, chrome),
                 hint = hint("form_hint_other_position")))
  }
  rows
}

#' The ordered rows of the Survey block
#' @keywords internal
#' @noRd
form_rows_for_survey <- function(spec, chrome, languages, hints = FALSE) {
  hint <- function(key) if (isTRUE(hints)) chrome[[key]] else NULL
  c(
    form_localized_rows(chrome$form_title, spec$title, languages,
                        single_line = TRUE, where = "the survey"),
    list(form_row(chrome$cover_languages, paste(languages, collapse = ", "),
                  hint = hint("form_hint_languages"), mono = TRUE)),
    form_localized_rows(chrome$welcome_text_title, spec$welcome, languages),
    form_localized_rows(chrome$end_text_title, spec$end_text, languages)
  )
}

#' The ordered rows of a Group block
#' @keywords internal
#' @noRd
form_rows_for_group <- function(g, gi, chrome, languages) {
  c(
    form_localized_rows(chrome$form_title, g$title, languages,
                        single_line = TRUE,
                        where = paste0("group ", gi)),
    form_localized_rows(chrome$description_title, g$description, languages)
  )
}

#' The ordered rows of a Quota block
#' @keywords internal
#' @noRd
form_rows_for_quota <- function(quota, k, chrome, languages, hints = FALSE) {
  hint <- function(key) if (isTRUE(hints)) chrome[[key]] else NULL
  c(
    form_localized_rows(chrome$form_name, quota$name, languages,
                        single_line = TRUE, where = paste0("quota ", k)),
    list(
      form_row(chrome$quota_limit, as.character(quota$limit %||% 0L),
               hint = hint("form_hint_limit")),
      form_row(chrome$form_action, chrome$quota_action_terminate),
      form_row(chrome$quota_condition,
               paste0(quota$question, " = ", quota$code),
               hint = hint("form_hint_condition"), mono = TRUE)
    ),
    form_localized_rows(chrome$form_message, quota$message, languages)
  )
}

# ---- the block table -----------------------------------------------------------

#' Build one block as a two-column flextable
#'
#' Row 1 is the title row on the dark band (block word on the left, the
#' question code on the right for a Question block, empty otherwise); rows
#' 2..n are key/value rows, the key bold on the light band with its optional
#' muted hint after a soft return, the value on white. Widths come from
#' `theme$content_width_in` so the block sits flush between the margins like
#' every other panel of the package. No cell is ever merged.
#'
#' @keywords internal
#' @noRd
form_block_table <- function(theme, rows, title_key, title_value = "") {
  n <- length(rows) + 1L
  df <- data.frame(key = rep("", n), value = rep("", n),
                   stringsAsFactors = FALSE)
  ft <- flextable::flextable(df)
  ft <- flextable::delete_part(ft, part = "header")

  txt <- function(...) officer::fp_text(font.family = theme$font_body, ...)
  title_key_props <- txt(font.size = theme$size_question, bold = TRUE,
                         color = theme$color_white)
  title_val_props <- officer::fp_text(font.family = theme$font_code,
                                      font.size = theme$size_question,
                                      bold = TRUE, color = theme$color_white)
  key_props <- txt(font.size = theme$size_question, bold = TRUE,
                   color = theme$color_primary)
  hint_props <- txt(font.size = theme$size_help, italic = TRUE,
                    color = theme$color_muted)
  body_props <- txt(font.size = theme$size_question, color = theme$color_text)
  code_props <- officer::fp_text(font.family = theme$font_code,
                                 font.size = theme$size_question,
                                 color = theme$color_text)

  ft <- flextable::compose(
    ft, i = 1L, j = "key",
    value = flextable::as_paragraph(
      flextable::as_chunk(title_key, props = title_key_props)))
  ft <- flextable::compose(
    ft, i = 1L, j = "value",
    value = flextable::as_paragraph(
      flextable::as_chunk(title_value, props = title_val_props)))

  for (k in seq_along(rows)) {
    r <- rows[[k]]
    chunks <- list(flextable::as_chunk(r$key, props = key_props))
    if (!is.null(r$hint) && nzchar(r$hint)) {
      chunks <- c(chunks,
                  list(flextable::as_chunk("\n", props = key_props),
                       flextable::as_chunk(r$hint, props = hint_props)))
    }
    ft <- flextable::compose(ft, i = k + 1L, j = "key",
                             value = do.call(flextable::as_paragraph, chunks))
    props <- if (isTRUE(r$mono)) code_props else body_props
    lines <- lss_form_sanitize(r$lines)
    value_chunks <- list()
    for (i in seq_along(lines)) {
      if (i > 1L) {
        value_chunks[[length(value_chunks) + 1L]] <-
          flextable::as_chunk("\n", props = props)
      }
      value_chunks[[length(value_chunks) + 1L]] <-
        flextable::as_chunk(lines[[i]], props = props)
    }
    ft <- flextable::compose(
      ft, i = k + 1L, j = "value",
      value = do.call(flextable::as_paragraph, value_chunks))
  }

  body_rows <- seq_len(n)[-1L]
  ft <- flextable::bg(ft, i = 1L, bg = theme$color_band_dark, part = "body")
  if (length(body_rows)) {
    ft <- flextable::bg(ft, i = body_rows, j = "key", bg = theme$color_band,
                        part = "body")
    ft <- flextable::bg(ft, i = body_rows, j = "value", bg = theme$color_white,
                        part = "body")
  }
  ft <- flextable::valign(ft, valign = "top", part = "body")
  ft <- flextable::align(ft, align = "left", part = "body")
  ft <- flextable::padding(ft, padding = 3, part = "body")
  ft <- flextable::border_remove(ft)
  thin <- officer::fp_border(color = theme$color_grid, width = 0.5)
  strong <- officer::fp_border(color = theme$color_band_dark, width = 1)
  ft <- flextable::border_outer(ft, border = thin, part = "body")
  ft <- flextable::border_inner_h(ft, border = thin, part = "body")
  ft <- flextable::vline(ft, j = "key", border = thin, part = "body")
  ft <- flextable::hline(ft, i = 1L, border = strong, part = "body")
  ft <- flextable::set_table_properties(ft, layout = "fixed")
  key_width <- 1.60
  ft <- flextable::width(ft, j = "key", width = key_width, unit = "in")
  ft <- flextable::width(ft, j = "value",
                         width = theme$content_width_in - key_width,
                         unit = "in")
  flextable::keep_with_next(ft, part = "body")
}

#' Tame the control characters a cell cannot carry
#'
#' Three different fates, and the doc says which is which: a tab becomes ONE
#' space (Word would store it as an invisible jump and the reader would see it
#' inside a label), a carriage return is DROPPED (the value splitter already
#' cut the text on it, so a leftover CR could only add an empty value), and
#' every other C0 control character is dropped outright -- flextable would
#' emit invalid XML and Word would refuse the file. Line feeds never reach
#' here: they are consumed by the line splitters above and re-emitted as soft
#' returns.
#' @keywords internal
#' @noRd
lss_form_sanitize <- function(lines) {
  lines <- as.character(lines %||% character(0))
  lines <- lines[!is.na(lines)]
  if (!length(lines)) return("")
  lines <- gsub("\t", " ", lines)
  lines <- gsub("\r", "", lines)
  gsub("[\001-\010\013\014\016-\037]", "", lines)
}

#' Append one block plus the two empty paragraphs that separate blocks
#'
#' Word merges two tables that touch into a single table when the document is
#' saved, which would destroy the block boundary; the separator is structural,
#' not cosmetic.
#' @keywords internal
#' @noRd
form_add_block <- function(doc, theme, rows, title_key, title_value = "") {
  ft <- form_block_table(theme, rows, title_key, title_value)
  doc <- flextable::body_add_flextable(doc, ft, align = "left")
  doc <- officer::body_add_par(doc, "")
  officer::body_add_par(doc, "")
}

# ---- the exported renderers ------------------------------------------------------

#' Render a survey specification as a Word authoring form
#'
#' `r lifecycle::badge("experimental")`
#'
#' \strong{Experimental.} Write an [lss_spec()] as a `.docx` **form**: one
#' two-column table per block (Survey, Group, Question, Quota), keys on the
#' left, values on the right. Unlike the review documents produced by
#' [render_questionnaire()], this document is meant to be *edited*: an author
#' fills or changes the value cells in Word and the companion reader
#' (0.3.0) turns the file back into a specification.
#'
#' The rows a Question block carries are decided by the question's kind, so
#' the form shows exactly the fields that kind accepts and never a field the
#' validator would refuse. Defaults are written as real values (Mandatory
#' `No`, an empty Filter, the other option at the End) rather than as
#' placeholders, because anything sitting in a value cell is content.
#'
#' @param spec An [lss_spec()] object, or a plain list with the same structure
#'   (it is then validated through `lss_spec()` first).
#' @param path Character. Path of the `.docx` file to write.
#' @param lang Language of the form's own labels (its "chrome"), one of
#'   `"en"`, `"fr"`, `"de"`, `"es"`, `"it"`. `NULL` (default) follows the
#'   survey's primary language when it is one of them, English otherwise. It
#'   is independent of the survey's content languages.
#' @param hints Logical. Add the muted syntax hint under each key
#'   (`"one per line, \"1 = Label\""`, ...). `FALSE` by default;
#'   [lss_template_docx()] turns it on for the blank template.
#' @param strict Logical, used only when `spec` is an `lss` object read by
#'   [read_lss()]: it is converted with [as_lss_spec()], and `strict` is passed
#'   to it. `TRUE` (default) refuses a survey carrying anything the
#'   specification cannot express; `FALSE` renders the rest of it and warns.
#'
#' @return Invisibly, the path to the written file.
#'
#' @details
#' Conventions the document obeys, and the companion reader relies on:
#'
#' * Each block is a top-level two-column table whose first row is its title
#'   row; two empty paragraphs separate two blocks. No cell is merged.
#' * Multi-valued fields (Options, Rows, Columns, Exclusive) hold one value
#'   per line. A line break inside a single-line field (an option label, a
#'   title, a quota name) is refused with a classed error naming the question,
#'   the field and the offending lines.
#' * The Type row carries the spec kind (`single`, `array5`, ...) as its
#'   value; the localized type label ("Single choice") is deliberately
#'   many-to-one, so it appears only as the key's hint in a blank template,
#'   never as content.
#' * Options are always written with an explicit code, `1 = Label`; the native
#'   other option is written with the reserved word of the form language
#'   (`Other`, `Autre`, `Sonstiges`, `Otro`, `Altro`), alone when the option
#'   has no label and as `Other = Label` as soon as it has one.
#' * When the spec declares several languages, every localizable key is
#'   written once per language and suffixed with the language code -- `Question
#'   [fr]`, `Question [en]` -- primary language first, the primary one
#'   suffixed as well.
#' * The file carries the custom document property `lssdoc-template-version`,
#'   the version of this contract.
#'
#' Requires the suggested packages \pkg{officer} and \pkg{flextable}.
#'
#' @examples
#' if (requireNamespace("officer", quietly = TRUE) &&
#'     requireNamespace("flextable", quietly = TRUE)) {
#'   spec <- lss_spec(
#'     title = "Demo",
#'     groups = list(list(title = "G", questions = list(
#'       list(code = "q1", kind = "single", text = "Oui ou non ?",
#'            options = list(list(text = "Oui"), list(text = "Non")))
#'     )))
#'   )
#'   out <- tempfile(fileext = ".docx")
#'   write_form_docx(spec, out, lang = "fr")
#'   file.exists(out)
#' }
#' @seealso [lss_template_docx()] for a blank template, [lss_spec()],
#'   [write_lss()], [render_questionnaire()].
#' @export
write_form_docx <- function(spec, path, lang = NULL, hints = FALSE,
                            strict = TRUE) {
  for (pkg in c("officer", "flextable")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      lssdoc_abort(                                     # nocov start
        c(
          "Writing a {.file .docx} form requires the {.pkg {pkg}} package.",
          "i" = "Install it with {.run install.packages(\"{pkg}\")}."
        ),
        class = "lssdoc_missing_suggest"
      )                                                 # nocov end
    }
  }
  if (!inherits(spec, "lss_spec")) {
    # A parsed survey is the "modify an existing questionnaire" entry point:
    # it is converted to a specification first, so the author edits the form
    # of a real `.lss` instead of retyping it. The conversion is narrower than
    # the file, hence `strict`.
    if (inherits(spec, "lss")) {
      spec <- as_lss_spec(spec, strict = strict)
    } else {
      # An `lss_audit` or an `lss_model` is a list too, so without this guard
      # it would be re-validated as a spec and fail with a confusing
      # field-level message about a field it was never meant to have.
      if (inherits(spec, c("lss_audit", "lss_model"))) {
        lssdoc_abort(
          c("{.fn write_form_docx} cannot render a {.cls {class(spec)[1]}} as a Word form.",
            "i" = "It renders an {.fn lss_spec}: build one with {.fn lss_spec}, or read a survey with {.fn read_lss} and convert it with {.fn as_lss_spec}."),
          class = "lssdoc_unsupported_input"
        )
      }
      if (!is.list(spec)) {
        lssdoc_abort("{.arg spec} must be an {.fn lss_spec} object or a list.",
                     class = "lssdoc_bad_spec")
      }
    }
  }
  if (!inherits(spec, "lss_spec")) {
    spec <- lss_spec(
      title = spec$title, groups = spec$groups,
      languages = spec$languages %||% spec$language %||%
        lss_spec_defaults$language,
      welcome = spec$welcome, end_text = spec$end_text,
      quotas = spec$quotas
    )
  }
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    lssdoc_abort("{.arg path} must be a single file path.",
                 class = "lssdoc_bad_path")
  }
  languages <- spec$languages %||% spec$language
  lang <- lss_resolve_chrome_lang(lang, languages)
  chrome <- lss_chrome_strings(lang)
  theme <- lss_render_theme()
  theme$content_width_in <- lss_content_width_in("A4-portrait")
  theme$chrome <- chrome
  theme$chrome_lang <- lang

  doc <- officer::read_docx()
  survey_title <- loc_text(spec$title, languages[[1L]])
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(officer::ftext(
      survey_title,
      prop = officer::fp_text(font.family = theme$font_body,
                              font.size = theme$size_heading1,
                              bold = TRUE, color = theme$color_primary))))
  doc <- officer::body_add_par(doc, "")

  doc <- form_add_block(doc, theme,
                        form_rows_for_survey(spec, chrome, languages, hints),
                        chrome$form_block_survey)
  for (gi in seq_along(spec$groups)) {
    g <- spec$groups[[gi]]
    doc <- form_add_block(doc, theme,
                          form_rows_for_group(g, gi, chrome, languages),
                          chrome$form_block_group)
    for (q in g$questions) {
      doc <- form_add_block(
        doc, theme,
        form_rows_for_question(q, lss_kinds, chrome, languages, hints),
        chrome$form_block_question, as.character(q$code))
    }
  }
  for (k in seq_along(spec$quotas)) {
    doc <- form_add_block(
      doc, theme,
      form_rows_for_quota(spec$quotas[[k]], k, chrome, languages, hints),
      chrome$form_block_quota)
  }

  doc <- officer::set_doc_properties(
    doc,
    title = survey_title,
    subject = "lssdoc form",
    values = list(
      `lssdoc-template-version` = as.character(LSS_FORM_VERSION),
      `lssdoc-chrome-lang` = lang
    )
  )
  doc <- officer::body_set_default_section(
    doc,
    lss_render_section_props("A4-portrait", 1L, theme = theme,
                             header_titles = character(0)))
  print(doc, target = path)

  n_q <- sum(vapply(spec$groups, function(g) length(g$questions), integer(1)))
  cli::cli_alert_success(
    "Wrote {.path {path}} ({n_q} question{?s}, {length(spec$groups)} group{?s}, {length(spec$quotas)} quota{?s})."
  )
  invisible(path)
}

#' Write a blank Word template for authoring a questionnaire
#'
#' `r lifecycle::badge("experimental")`
#'
#' \strong{Experimental.} Generate the blank authoring form: one example
#' question per requested kind, every default pre-filled, and a muted hint
#' under each key telling the author the syntax the field expects. The
#' template is always **generated** from the kind table -- the package ships
#' no static `.docx` -- so it cannot describe a field [lss_spec()] would
#' refuse.
#'
#' Delete the example questions you do not need, edit the others, then hand
#' the file to the reader (0.3.0) to obtain an [lss_spec()] and, through
#' [write_lss()], a `.lss` file LimeSurvey imports.
#'
#' @param path Character. Path of the `.docx` file to write.
#' @param lang Language of the template's labels and example wording, one of
#'   `"en"`, `"fr"` (default), `"de"`, `"es"`, `"it"`.
#' @param kinds Character vector of kinds to illustrate, `lss_kinds$kind`
#'   (all 21) by default; the order of the kind table is kept.
#' @param languages Content languages of the questionnaire to be written,
#'   the first one being the primary language. Defaults to `lang`, giving a
#'   single-language form. With several, every text row is repeated once per
#'   language, keyed `Wording [fr]`, `Wording [en]` and so on, and the reader
#'   then requires each of them: a questionnaire with a missing translation
#'   is what [audit_lss()] exists to catch. Independent of `lang`, which only
#'   sets the language of the form's own labels.
#'
#' @return Invisibly, the path to the written file.
#'
#' @details
#' The template is the render of a generated example specification, so the
#' same object feeds the template, the tests and the vignette. The value of a
#' Type row is always the spec kind (`single`, `array5`, ...); the localized
#' type label is shown as the hint under the key, because it is many-to-one
#' and could not be read back. Reserved words
#' are localized: an option line reading `Autre` in a French template creates
#' the native other option, and an ordinary option that happens to read
#' "Autre" is written with an explicit code (`9 = Autre`) -- which the hint
#' under the Options key spells out.
#'
#' Requires the suggested packages \pkg{officer} and \pkg{flextable}.
#'
#' @examples
#' if (requireNamespace("officer", quietly = TRUE) &&
#'     requireNamespace("flextable", quietly = TRUE)) {
#'   out <- tempfile(fileext = ".docx")
#'   lss_template_docx(out, lang = "fr", kinds = c("single", "multiple"))
#'   file.exists(out)
#'
#'   # A bilingual questionnaire, with French labels on the form itself.
#'   both <- tempfile(fileext = ".docx")
#'   lss_template_docx(both, lang = "fr", languages = c("fr", "en"),
#'                     kinds = "single")
#'   file.exists(both)
#' }
#' @seealso [write_form_docx()] to render an existing specification,
#'   [lss_spec()], [write_lss()].
#' @export
lss_template_docx <- function(path, lang = "fr", kinds = lss_kinds$kind,
                              languages = lang) {
  lang <- lss_resolve_chrome_lang(lang, character(0))
  write_form_docx(lss_example_spec(kinds, lang, languages), path, lang = lang,
                  hints = TRUE)
}
