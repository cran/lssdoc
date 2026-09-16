# print.lss_spec() output is stable

    Code
      print(spec)
    Message
      <lss_spec> "Enquete" (fr and en)
      1 group, 1 question, 0 quotas

# the unknown-kind error lists every authorable kind

    Code
      lss_spec(title = "T", groups = list(list(title = "G", questions = list(list(
        code = "q1", kind = "wat", text = "Texte ?")))))
    Condition
      Error in `lss_spec()`:
      ! Invalid specification for question "q1".
      x Unknown kind "wat": use one of "single", "dropdown", "singlecomment", "multiple", "array", "array5", "array10", "arrayyesno", "arraytrend", "ranking", "multitext", "multinumeric", "text", "shorttext", "hugetext", "numeric", "date", "yesno", "gender", "fivepoint", "display".

