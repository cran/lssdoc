# Target LimeSurvey schema version. `write_lss()` emits it as `<DBVersion>`
# and `read_lss()` compares the file it reads against it: the package
# targets LimeSurvey 6.x. Bump this constant here -- and only here -- when
# LimeSurvey changes its `.lss` schema, and record the bump in NEWS.md.
LSS_DBVERSION <- "700"

# Version of the Word authoring form contract. `write_form_docx()` stores it
# as the custom document property `lssdoc-template-version` and the reader
# requires it: a document carrying another version is refused rather than
# parsed against the wrong row labels. Bump this constant here -- and only
# here -- whenever a row label, a reserved word, a value vocabulary or the
# block layout changes incompatibly, and record the bump in NEWS.md.
LSS_FORM_VERSION <- 1L

# Default values for the `surveys` and `surveys_languagesettings` sections
# of an emitted `.lss` file.
#
# Provenance: a real LimeSurvey 6 (DBVersion 700) export, scrubbed of
# anything dated, personal, or instance-specific: no expiry or start date,
# no last-modified stamp, admin and bounce addresses left to "inherit",
# e-mail templates left empty (LimeSurvey lets them be filled in the
# interface). Inventing these ~60 fields instead of copying them from a
# real export is the surest way to produce an import that LimeSurvey
# accepts in appearance and silently amputates.
#
# Runtime fields (survey id, language, title, welcome and end texts) are
# not listed here: `write_lss()` sets them from the spec.

lss_default_surveys_fields <- list(
  gsid = "1",
  admin = "inherit",
  expires = "",
  adminemail = "inherit",
  anonymized = "N",
  format = "G",
  savetimings = "N",
  template = "vanilla",
  language = "fr",
  additional_languages = "",
  datestamp = "I",
  usecookie = "Y",
  allowregister = "I",
  allowsave = "N",
  autonumber_start = "2",
  autoredirect = "I",
  allowprev = "Y",
  printanswers = "I",
  ipaddr = "N",
  refurl = "N",
  showsurveypolicynotice = "0",
  publicstatistics = "I",
  publicgraphs = "I",
  listpublic = "I",
  htmlemail = "I",
  sendconfirmation = "I",
  tokenanswerspersistence = "I",
  assessments = "I",
  usecaptcha = "E",
  usetokens = "N",
  bounce_email = "inherit",
  attributedescriptions = "",
  emailresponseto = "inherit",
  emailnotificationto = "inherit",
  tokenlength = "-1",
  showxquestions = "N",
  showgroupinfo = "I",
  shownoanswer = "N",
  showqnumcode = "I",
  bouncetime = "",
  bounceprocessing = "N",
  bounceaccounttype = "",
  bounceaccounthost = "",
  bounceaccountpass = "",
  bounceaccountencryption = "",
  bounceaccountuser = "",
  showwelcome = "I",
  showprogress = "N",
  questionindex = "-1",
  navigationdelay = "-1",
  nokeyboard = "I",
  alloweditaftercompletion = "I",
  googleanalyticsstyle = "0",
  googleanalyticsapikey = "",
  tokenencryptionoptions = "{ \"enabled\":\"Y\",\"columns\":{ \"firstname\":\"N\",\"lastname\":\"N\",\"email\":\"N\" } }",
  ipanonymize = "Y",
  othersettings = "",
  access_mode = "O",
  lastmodified = "",
  startdate = ""
)

lss_default_language_settings <- list(
  surveyls_policy_error = "",
  surveyls_email_invite_subj = "",
  surveyls_email_invite = "",
  surveyls_email_remind_subj = "",
  surveyls_email_remind = "",
  surveyls_email_register_subj = "",
  surveyls_email_register = "",
  surveyls_email_confirm_subj = "",
  surveyls_email_confirm = "",
  surveyls_dateformat = "5",
  surveyls_attributecaptions = "",
  email_admin_notification_subj = "",
  email_admin_notification = "",
  email_admin_responses_subj = "",
  email_admin_responses = "",
  surveyls_numberformat = "1",
  attachments = "",
  surveyls_alias = "",
  surveyls_description = "",
  surveyls_policy_notice = "",
  surveyls_policy_notice_label = "",
  surveyls_url = "",
  surveyls_urldescription = ""
)
