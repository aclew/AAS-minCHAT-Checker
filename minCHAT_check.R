library(tidyverse)
library(lubridate)

check.annotations <- function(annfile, nameannfile) {
  #txt.input.path <- "input_files/"
  
  #filebatch <- dir(txt.input.path, pattern="*.txt")

  alert.table <- tibble(
    filename = character(),
    alert = character(),
    onset = integer(),
    offset = integer(),
    tier = character(),
    value = character()
  )
  
  convert_ms_to_hhmmssms <- function(msectime) {
    if (is.na(msectime)) {
      return("none")
    } else {
      ms_p_hr <- 60*60*1000
      ms_p_mn <- 60*1000
      ms_p_sc <- 1000
      hh <- floor(msectime/ms_p_hr)
      if (hh < 10) {
        hh <- paste0("0", hh)
      }
      msectime <- msectime%%ms_p_hr
      mm <- floor(msectime/ms_p_mn)
      if (mm < 10) {
        mm <- paste0("0", mm)
      }
      msectime <- msectime%%ms_p_mn
      ss <- floor(msectime/ms_p_sc)
      if (ss < 10) {
        ss <- paste0("0", ss)
      }
      msectime <- msectime%%ms_p_sc
      msec <- msectime
      if (msec < 100) {
        msec <- paste0(msec, "0")
      }
      newtime <- paste0(hh, ":", mm, ":", ss, ".", msec)
      return(newtime)
    }
  }
  
  add_alert <- function (filename, alert, onset, offset, tier, value) {
    bind_rows(alert.table, tibble(
      filename = filename,
      alert = alert,
      onset = onset,
      offset = offset,
      tier = tier,
      value = value))
  }
  
  ##########
  
#  for (annfile in filebatch) {
#    annots <- read_tsv(paste0(txt.input.path, annfile), col_names = FALSE) %>%
  annots <- read_tsv(annfile, col_names = FALSE,
                     locale = locale(encoding = "UTF-8")) %>%
    rename("tier" = X1, "speaker" = X2, "onset" = X3,
           "offset" = X4, "duration" = X5, "value" = X6)
  filename <- unlist((strsplit(nameannfile, "\\.txt")))[1]
#  filename <- as.character(annfile)
  
    
    
    ##---- CHECKS ----##
  
    
    #-- correct tier names --#
    # check if the tier name is either 3 or 7 characters
    bad.format.tier.names <- unique(annots$tier[which(
      !grepl("(^[a-z]{3}@[A-Z]{2}\\d{1}$)|(^[a-z]{3}@CHI$)|(^[A-Z]{2}\\d{1}$)|(^CHI$)",
             annots$tier))])
    if (length(bad.format.tier.names) > 0) {
      alert.table <- add_alert(filename,
        paste0("wrong format tier name(s): ",
               paste(bad.format.tier.names, collapse = " ")), NA, NA, NA, NA)
    }
    # if the pre- or post-fixes don't match one of the limited types
    tier.names <- unique(unlist(strsplit(annots$tier[which(grepl("@", annots$tier))], "@")))
    name.part.matches <- tier.names %in% c("vcm", "lex", "mwu", "xds") |
         grepl("[A-Z]{2}\\d{1}", tier.names) |
         grepl("^CHI$", tier.names)
    if (FALSE %in% name.part.matches) {
      alert.table <- add_alert(filename,
        paste0("illegal tier prefix(es) or speaker name(s) (may overlap with wrong format): ",
               paste(unique(tier.names[which(name.part.matches == FALSE)]),
                     collapse = " ")), NA, NA, NA, NA)
    }
    
    
    #-- missing contingent annotations --#
    n.CHI <- filter(annots, tier == "CHI") %>% nrow()
    # if there are CHI vocalizations...
    if (n.CHI > 0) {
      # if there's a VCM tier, make sure there are the same
      # number of annotations as there are in the CHI tier
      if (TRUE %in% grepl("vcm", annots$tier)) {
        if (filter(annots, tier == "vcm@CHI") %>% nrow() != n.CHI) {
          alert.table <- add_alert(filename, "1+ missing VCM annotations", NA, NA, NA, NA)
        }
        n_cb <- filter(annots, tier == "vcm@CHI" &
                         value == "C") %>% nrow()
        n_lx <- filter(annots, tier == "lex@CHI" &
                         value == "W") %>% nrow()
        n_mw <- filter(annots, tier == "mwu@CHI" &
                         value == "M") %>% nrow()
        # if the child produced canonical babbles...
        if (n_cb > 0) {
          # check if there is a matching number of LEX codes
          # for each canonincal babble
          if (n_cb == nrow(filter(annots, tier == "lex@CHI"))) {
            # if so, check if there is a matching number of MWU codes
            # for each babble with words
            if (n_lx != nrow(filter(annots, tier == "lex@CHI"))) {
              alert.table <- add_alert(filename,
                                       "possible missing MWU annotations when LEX = 'W'",
                                       NA, NA, NA, NA)
            }
          } else {
            alert.table <- add_alert(filename,
                                     "possible missing LEX annotations when VCM = 'C'; re-check MWU too, if relevant",
                                     NA, NA, NA, NA)
          }
        } else {
          if (nrow(filter(annots, tier == "lex@CHI")) > 0 || nrow(filter(annots, tier == "mwu@CHI")) > 0) {
            alert.table <- add_alert(filename,
                                     "too many LEX/MWU annotations", NA, NA, NA, NA)
          }
        }
      } else {
      # if there's no VCM tier, make sure instead that there
      # are the same number of annotations on LEX as there are
      # in the CHI tier
        if (TRUE %in% grepl("lex", annots$tier)) {
          n_lx <- filter(annots, tier == "lex@CHI" &
                           value == "W") %>% nrow()
          n_mw <- filter(annots, tier == "mwu@CHI" &
                           value == "M") %>% nrow()
          # if the child produced lexical words...
          if (n_lx > 0) {
            # check if there is a matching number of MWU codes
            # for each babble with words
            if (n_lx != nrow(filter(annots, tier == "lex@CHI"))) {
              alert.table <- add_alert(filename,
                                       "missing MWU annotations when LEX = 'W'",
                                       NA, NA, NA, NA)
            }
          } else {
            if (nrow(filter(annots, tier == "mwu@CHI")) > 0) {
              alert.table <- add_alert(filename,
                                       "too many MWU annotations",
                                       NA, NA, NA, NA)
            }
          }
        } else {
          alert.table <- add_alert(filename,
                                   "missing LEX or VCM tier",
                                   NA, NA, NA, NA)
        }
      }
    }
    # check whether there are the same number of xds annotations as
    # non-CHI vocalizations
    if (filter(annots, tier != "CHI") %>% nrow() !=
        filter(annots, grepl("xds@", tier)) %>% nrow()) {
      alert.table <- add_alert(filename,
        "missing XDS annotations; compare the # of utterances with the # of XDS annotations for each speaker",
        NA, NA, NA, NA)
    }
  
  
    #-- invalid annotation values: closed vocabulary --#
    # check XDS values
#    if (filter(annots, grepl("xds@", tier)) %>% nrow() > 0) {
      xds.vals <- filter(annots, grepl("xds@", tier)) %>%
        select(value, onset, offset, tier) %>%
        mutate(filename = filename,
               alert = 
                 "illegal XDS annotation value",
               legal = case_when(
                 value == "T" ~ "okay",
                 value == "C" ~ "okay",
                 value == "B" ~ "okay",
                 value == "A" ~ "okay",
                 value == "U" ~ "okay",
                 value == "P" ~ "okay",
                 value == "O" ~ "okay",
                 TRUE ~ "problem"
                 )) %>%
        filter(legal != "okay") %>%
        select(filename, alert, onset, offset, tier, value)
#    }
    # check VCM values
#    if (filter(annots, grepl("vcm@", tier)) %>% nrow() > 0) {
      vcm.vals <- filter(annots, grepl("vcm@", tier)) %>%
        select(value, onset, offset, tier) %>%
        mutate(filename = filename,
               alert = 
                 "illegal VCM annotation value",
               legal = case_when(
                 value == "C" ~ "okay",
                 value == "N" ~ "okay",
                 value == "Y" ~ "okay",
                 value == "L" ~ "okay",
                 value == "U" ~ "okay",
                 TRUE ~ "problem"
                 )) %>%
        filter(legal != "okay") %>%
        select(filename, alert, onset, offset, tier, value)
#    }
    # check LEX values
#    if (filter(annots, grepl("lex@", tier)) %>% nrow() > 0) {
      lex.vals <- filter(annots, grepl("lex@", tier)) %>%
        select(value, onset, offset, tier) %>%
        mutate(filename = filename,
               alert = 
                 "illegal LEX annotation value",
               legal = case_when(
                 value == "W" ~ "okay",
                 value == "0" ~ "okay",
                 TRUE ~ "problem"
                 )) %>%
        filter(legal != "okay") %>%
        select(filename, alert, onset, offset, tier, value)
#    }
    # check MWU values
#    if (filter(annots, grepl("mwu@", tier)) %>% nrow() > 0) {
      mwu.vals <- filter(annots, grepl("mwu@", tier)) %>%
        select(value, onset, offset, tier) %>%
        mutate(filename = filename,
               alert = 
                 "illegal MWU annotation value",
               legal = case_when(
                 value == "M" ~ "okay",
                 value == "1" ~ "okay",
                 TRUE ~ "problem"
                 )) %>%
        filter(legal != "okay") %>%
        select(filename, alert, onset, offset, tier, value)
#    }
    # add closed vocabulary alerts to table
    alert.table <- bind_rows(alert.table,
                             xds.vals, vcm.vals, lex.vals, mwu.vals)
  
    
    
    #-- invalid annotation values: transcription --#
    # note that the regular expressions below are *far* from airtight
    # and improvements would be most welcome
    utts <- filter(annots, tier == speaker)
    # check for utterances without transcription
    empty.utts <- filter(utts, (grepl("^[\\s[[:punct:]]]*$", value) | is.na(value))) %>%
      select(onset, offset, tier, value) %>%
      mutate(filename = filename,
             alert = "empty transcription") %>%
      select(filename, alert, onset, offset, tier, value)
    # check for the presence of a single terminal mark at the end
    nonterminating.utts <- filter(utts, !grepl("[.!?]{1}$", value)) %>%
      select(onset, offset, tier, value) %>%
      mutate(filename = filename,
             alert = "no utterance terminator") %>%
      select(filename, alert, onset, offset, tier, value)
    # check for the presence of multiple terminal marks in the utterance
    utts.nopauses <- gsub(" \\(\\.*\\)", "", utts$value)
    utts.nosqbrackets <- gsub("\\[.*?\\]", "", utts.nopauses)
    utts$value.mod <- utts.nosqbrackets
    overterminating.utts <- utts %>%
      mutate(
      n_terms = str_count(value.mod, "[.!?]"),
      filename = filename,
      alert = "2+ utterance terminators") %>%
      filter(n_terms > 1) %>%
      select(filename, alert, onset, offset, tier, value)
    # check for matching square bracket types
    squarebrace.utts <- filter(utts, grepl("[[]", value)) %>%
      select(onset, offset, tier, value) %>%
      mutate(filename = filename,
             alert = case_when(
        # correctly formatted example: "[: word]"
        grepl("\\[\\: [[:alnum:]]+.*\\]", value) ~ "okay",
        # correctly formatted example: "<word> [=! word]"
        grepl("<[[:alnum:]]+.*> \\[=! [[:alnum:]]+.*\\]", value) ~ "okay",
        # correctly formatted example: "[- lng]"
        grepl("\\[- [[:alnum:]]{3}\\]", value) ~ "okay",
        TRUE ~ "incorrect use of square and/or angle braces"
      )) %>%
      filter(alert != "okay") %>%
      select(filename, alert, onset, offset, tier, value)
    # check for uses of @
    atsign.utts <- filter(utts, grepl("@", value)) %>%
      select(onset, offset, tier, value) %>%
      mutate(filename = filename,
             alert = case_when(
        # correctly formatted example: "word@s:eng"
        grepl("[[:alnum:]]+@s\\:[a-z]{3}[ ,.!?]", value) ~ "okay",
        # correctly formatted example: "word@l"
        grepl("[[:alnum:]]+@l[ ,.!?]", value) ~ "okay",
        # correctly formatted example: "word@c"
        grepl("[[:alnum:]]+@c[ ,.!?]", value) ~ "okay",
        TRUE ~ "incorrect use of @ sign in transcription"
      )) %>%
      filter(alert != "okay") %>%
      select(filename, alert, onset, offset, tier, value)
    # add open transcription alerts to table
    alert.table <- bind_rows(alert.table,
                             empty.utts, nonterminating.utts, overterminating.utts,
                             squarebrace.utts, atsign.utts)
    
    # convert msec times to HHMMSS
    alert.table <- alert.table %>%
      rowwise() %>%
      mutate(start = convert_ms_to_hhmmssms(onset),
             stop = convert_ms_to_hhmmssms(offset)) %>%
      select(-onset, -offset)
  
    # short list of known error types that this script doesn't check:
    # - spelling... anywhere
    # - &=verbs (neither the &= nor the use of present 3ps tense)
    # - [=! verbs] (checks the bracket syntax, but not the use of present 3ps tense)
    # - xxx vs. yyy
    # - the use of capital letters
    # - the use of conventionally spelled expressive words (e.g., mm-hm)
    # - extra spaces
    # - uses of hyphens and ampersands to indicate cut-off/restarted speech (e.g., he-, -in)
    # - the use of things like [+ CHI], that were in some of the ROS files
    # - matching speaker names across related tiers
    # - inner tier structure (i.e., correct hierarchical set-up)
  
#  }
      
    return(list(
      alert.table = alert.table,
      n.a.alerts = nrow(alert.table)
    ))
  
  # write_csv(alert.table,
  #             paste0("possible_errors_detected-",
  #                    gsub(" ", "_", gsub(":", "", Sys.time())), ".csv"))
  
}
