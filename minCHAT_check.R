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
  
  check_minCHATspclchr <- function(utt, minCHATspclchr) {
    if (minCHATspclchr == "squarebraces") {
      utterance <- gsub(
        "<[[:alnum:] ,.!?-_'@&=]+> _bb_",
        "_aa_",
        gsub(
          "(\\[: [[:alnum:] ,.!?-_'@]+\\])|(\\[=! [[:alnum:]]+\\])",
          "_bb_",
          gsub(
            "(\\[- [[:alnum:]]{3}\\])",
            "_lg_",
            utt,
            perl = TRUE),
          perl = TRUE),
        perl = TRUE)
      # and now in case of <<xxx> [xxx]> [xxx] double embeddings...
      embedded.braces.pattern <- "<[[:alnum:] ,.!?-_'@&=]*_aa_[[:alnum:] ,.!?-_'@&=]*> _bb_"
      if (grepl(embedded.braces.pattern, utterance)) {
        utterance <- gsub(embedded.braces.pattern, "", utterance)
      }
      utterance <- gsub("(_aa_)|(^_lg_)", "", utterance)
      if (grepl("([][<>])|(_bb_)|(_aa_)|(_lg_)", utterance)) {
        return("incorrect use of square braces")
      } else {
        return("okay")
      }
    } else if (minCHATspclchr == "atsign") {
      utterance <- gsub(
        "([[:alnum:]]+@s\\:[a-z]{3}[ ,.!?-])|([[:alnum:]]+@[lc])",
        "atat",
        utt)
      if (grepl("@", utterance)) {
        return("incorrect use of @ sign")
      } else {
        return("okay")
      }
    } else if (minCHATspclchr == "ampersand") {
      utterance <- gsub(
        "(&=[[:alnum:]]+)|(&[[:alnum:]_'@-]+)|([[:alnum:]_'@-]+&)",
        "mpmp",
        utt)
      if (grepl("&", utterance)) {
        return("incorrect use of & sign")
      } else {
        return("okay")
      }
    } else {
      return("ERROR: contact app developer")
    }
  }
  
  legal.tier.names <- "(^xds@[FMU][ACU](\\d{1}|E)$)|(^xds@EE1$)|(^(vcm|lex|mwu)@CHI$)|(^[FMU][ACU](\\d{1}|E)$)|(^EE1$)|(^CHI$)|(^context$)|(^code_num$)|(^code$)|(^notes$)|(^on_off)"
  
  ##########
  
#  for (annfile in filebatch) {
#    annots <- read_tsv(paste0(txt.input.path, annfile), col_names = FALSE) %>%
  annots <- read_tsv(annfile, col_names = FALSE, # annfile <- "input_files/rely_1499.txt"
                     locale = locale(encoding = "UTF-8")) %>%
    rename("tier" = X1, "speaker" = X2, "onset" = X3,
           "offset" = X4, "duration" = X5, "value" = X6)
  filename <- unlist((strsplit(nameannfile, "\\.txt")))[1] # nameannfile <- "rely_14991.txt"
#  filename <- as.character(annfile)
  
    
    
    ##---- CHECKS ----##
  
    
    #-- correct tier names --#
    # check if the tier name is either 3 or 7 characters
  
    bad.format.tier.names <- unique(annots$tier[which(
      !grepl(legal.tier.names, annots$tier))])
    if (length(bad.format.tier.names) > 0) {
      alert.table <- add_alert(filename,
        paste0("wrong format tier name(s): ",
               paste(
                 bad.format.tier.names, collapse = " ")),
        min(annots$onset), max(annots$offset), "", "")
    }
    # if the pre- or post-fixes don't match one of the limited types
    tier.names <- unique(unlist(strsplit(annots$tier[which(grepl("@", annots$tier))], "@")))
    name.part.matches <- tier.names %in% c("vcm", "lex", "mwu", "xds") |
         grepl("([FMU][ACU](\\d{1}|E))|(EE1)", tier.names) |
         grepl("^CHI$", tier.names)
    if (FALSE %in% name.part.matches) {
      alert.table <- add_alert(filename,
        paste0("illegal tier prefix(es) or speaker name(s) (may overlap with wrong format): ",
               paste(unique(tier.names[which(name.part.matches == FALSE)]),
                     collapse = " ")),
        min(annots$onset), max(annots$offset), "", "")
    }
    
    
    #-- missing contingent annotations --#
    n.CHI <- filter(annots, tier == "CHI") %>% nrow()
    # if there are CHI vocalizations...
    if (n.CHI > 0) {
      # if there's a VCM tier, make sure there are the same
      # number of annotations as there are in the CHI tier
      if (TRUE %in% grepl("vcm", annots$tier)) {
        if (filter(annots, tier == "vcm@CHI") %>% nrow() != n.CHI) {
          alert.table <- add_alert(
            filename, "incorrect number of VCM annotations",
            min(annots$onset), max(annots$offset), "", "")
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
            if (n_lx != nrow(filter(annots, tier == "mwu@CHI"))) {
              alert.table <- add_alert(
                filename,
                "incorrect number of MWU annotations; should be equal to number of LEX = 'W'",
                min(annots$onset), max(annots$offset), "", "")
            }
          } else {
            alert.table <- add_alert(
              filename,
              "incorrect number of LEX annotations; should be equal to number of VCM = 'C'; re-check MWU too, if relevant",
              min(annots$onset), max(annots$offset), "", "")
          }
        } else {
          # if the child produced no canonical babbles but there are 
          # LEX and MWU annots send an alert
          if (nrow(filter(annots, tier == "lex@CHI")) > 0 ||
              nrow(filter(annots, tier == "mwu@CHI")) > 0) {
            alert.table <- add_alert(
              filename,
              "too many LEX/MWU annotations because there are no cases of VCM = 'C'",
              min(annots$onset), max(annots$offset), "", "")
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
            if (n_lx != nrow(filter(annots, tier == "mwu@CHI"))) {
              alert.table <- add_alert(
                filename,
                "incorrect number of MWU annotations; should be equal to number of LEX = 'W'",
                min(annots$onset), max(annots$offset), "", "")
            }
          } else {
            # if the child produced no lexical utterances but there are
            # MWU annots send an alert
            if (nrow(filter(annots, tier == "mwu@CHI")) > 0) {
              alert.table <- add_alert(
                filename,
                "too many MWU annotations because there are no cases of LEX = 'W'",
                min(annots$onset), max(annots$offset), "", "")
            }
          }
        } else {
          alert.table <- add_alert(
            filename,
            "missing LEX or VCM tier",
            min(annots$onset), max(annots$offset), "", "")
        }
      }
    }
    # check whether there are the same number of xds annotations as
    # non-CHI vocalizations
    nonCHI.vocs <- annots %>%
      filter(grepl(
        "(^[A-Z]{2}\\d{1}$)|(^xds@[A-Z]{2}\\d{1}$)", tier)) %>%
      group_by(tier, speaker) %>%
      summarize(nvocs = n()) %>%
      ungroup() %>%
      mutate(tier = case_when(
        grepl("^xds@", tier) ~ "xds.vocs",
        TRUE ~ "spkr.vocs"
      )) %>%
      group_by(speaker) %>%
      spread(tier, nvocs, drop = FALSE) %>%
      mutate(match = spkr.vocs == xds.vocs) %>%
      filter(match == FALSE) %>%
      pull(speaker)
    if (length(nonCHI.vocs) > 0) {
      nonCHI.vocs.str <- paste0(nonCHI.vocs, collapse = ", ")
      alert.xds <- paste0(
        "missing XDS annotations; compare the # of utterances with the # of XDS annotations for ",
        nonCHI.vocs.str)
      alert.table <- add_alert(
        filename,
        alert.xds,
        min(annots$onset), max(annots$offset), "", "")
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
    empty.utts <- filter(utts,
      (grepl("^[\\s[[:punct:]]]*$", value) | is.na(value))) %>%
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
    utts.nopausessqbrackets <- gsub("\\[.*?\\]", "",
      gsub(" \\(\\.*\\)", "", utts$value))
    utts$value.mod <- utts.nopausessqbrackets
    overterminating.utts <- utts %>%
      mutate(
      n_terms = str_count(value.mod, "[.!?]"),
      filename = filename,
      alert = "2+ utterance terminators") %>%
      filter(n_terms > 1) %>%
      select(filename, alert, onset, offset, tier, value)

    # check for uses of square bracket expressions
    squarebrace.errs <- filter(utts, grepl("[[]", value)) %>%
      select(onset, offset, tier, value)
    if (nrow(squarebrace.errs) > 0) {
      squarebrace.errs <- squarebrace.errs %>%
        rowwise() %>%
        mutate(alert.sq = check_minCHATspclchr(value, "squarebraces")) %>%
        filter(alert.sq != "okay")
    }
    # check for uses of @
    atsign.errs <- filter(utts, grepl("@", value)) %>%
      select(onset, offset, tier, value)
    if (nrow(atsign.errs) > 0) {
      atsign.errs <- atsign.errs %>%
        rowwise() %>%
        mutate(alert.at = check_minCHATspclchr(value, "atsign")) %>%
        filter(alert.at != "okay")
    }
    # check for uses of &
    ampsnd.errs <- filter(utts, grepl("&", value)) %>%
      select(onset, offset, tier, value)
    if (nrow(ampsnd.errs) > 0) {
      ampsnd.errs <- ampsnd.errs %>%
        rowwise() %>%
        mutate(alert.am = check_minCHATspclchr(value, "ampersand")) %>%
        filter(alert.am != "okay")
    }
    spchchr.errs <- full_join(squarebrace.errs, atsign.errs) %>%
      full_join(ampsnd.errs)
    if (nrow(spchchr.errs) > 0) {
      spchchr.errs <- spchchr.errs %>%
        mutate(
          filename = filename,
          alert = case_when(
            is.na(alert.sq) & is.na(alert.at) ~ "okay",
            is.na(alert.sq) & !is.na(alert.at) ~ alert.at,
            !is.na(alert.sq) & is.na(alert.at) ~ alert.sq,
            !is.na(alert.sq) & !is.na(alert.at) ~ paste0(
              alert.at, " and ", alert.sq))) %>%
        select(filename, alert, onset, offset, tier, value)
    }

    # add open transcription alerts to table
    alert.table <- bind_rows(
      alert.table,
      empty.utts,
      nonterminating.utts,
      overterminating.utts,
      spchchr.errs)
    
    # List capitalized words found
    no.na.utts <- filter(utts, !is.na(value))
    all.utts.together <- paste0(
      " ", no.na.utts$value, collapse = " ")
    capitalwords.used <- tibble(
      `capitalized words` = sort(unique(unlist(
        regmatches(all.utts.together,
          gregexpr(" [A-Z][A-Za-z@_]*",
            all.utts.together)))))) %>%
      mutate(`capitalized words` = trimws(
        `capitalized words`))

    # List of hyphenated words found
    hyphenwords.used <- tibble(
      `hyphenated words` = sort(unique(unlist(
        regmatches(all.utts.together,
          gregexpr("[A-Za-z]+-[A-Za-z]+",
            all.utts.together)))))) %>%
      mutate(`hyphenated words` = trimws(
        `hyphenated words`))
    
    # convert msec times to HHMMSS and return assessment
    if (nrow(alert.table) > 0) {
      alert.table <- alert.table %>%
        rowwise() %>%
        mutate(start = convert_ms_to_hhmmssms(onset),
          stop = convert_ms_to_hhmmssms(offset)) %>%
        select(-onset, -offset)
      return(list(
        alert.table = alert.table,
        n.a.alerts = nrow(alert.table),
        capitals = capitalwords.used,
        n.capitals = nrow(capitalwords.used),
        hyphens = hyphenwords.used,
        n.hyphens = nrow(hyphenwords.used)
      ))
    } else {
        alert.table.NA = tibble(
          filename = filename,
          alerts = "No errors detected! :D"
        )
        return(list(
          alert.table = alert.table.NA,
          n.a.alerts = 0,
          capitals = capitalwords.used,
          n.capitals = nrow(capitalwords.used),
          hyphens = hyphenwords.used,
          n.hyphens = nrow(hyphenwords.used)
        ))
    }
#  }
      
  
  # write_csv(alert.table,
  #             paste0("possible_errors_detected-",
  #                    gsub(" ", "_", gsub(":", "", Sys.time())), ".csv"))
  
}
