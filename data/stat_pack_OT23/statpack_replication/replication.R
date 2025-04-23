################################################################################
# Empirical SCOTUS StatPack OT2023 Replication
# Code Developed by Jake S. Truscott, Ph.D (CCSE, Purdue)
# Updated April 2024
################################################################################

################################################################################
# Data Disclosure & Notes
################################################################################
' We are grateful to the organized efforts of those maintaining Oyez (Chicago-Kent) and the Supreme Court Database (WashU St. Louis). We identify sources of data not explicitly collected or processed by us in the code below.

----------------------
Recommended Citations:
----------------------
Oyez: Oyez. (n.d.). Retrieved from https://www.oyez.org/

Supreme Court Database: Spaeth, H. J., Epstein, L., Martin, A. D., Segal, J. A., Ruger, T. J., & Benesh, S. C. (2023). Supreme Court Database, Version 2023 Release 01. Retrieved from http://supremecourtdatabase.org

Statpack: Feldman, A. & Truscott, J. S. (2024, June 30). Supreme Court 2023-2024 Term Stat Pack (Version 0.1). EmpiricalSCOTUS. Available at: https://empiricalscotus.com/ '

################################################################################
# Load Packages
################################################################################
library(kableExtra); library(dplyr);  library(tidyr); library(scotustext); library(htmltools); library(ggplot2); library(png); library(dplyr); library(stringi); library(stringr); library(ggplot2); library(ggthemes); library(anytime); library(tm); library(scotustext); library(readxl); library(ggpattern); library(png); library(ggtext); library(grid); library(tidyr); library(readxl); library(anytime); library(sf); library(purrr); library(readxl); library(pdftools)

################################################################################
# Amended SCOTUSTEXT Fix for Decision Processor
################################################################################
{

  decision_processor2 <- function(dir_path) {
    files <- list.files(dir_path, full.names = TRUE)
    num_files <- length(files)
    cat("\nDecisions to Process: ", num_files)
    all_decisions <- NULL

    decisions_cleaner <- function(file_path, dir_path) {

      {
        rtext <- readtext::readtext(file_path)
        corpus <- Corpus(VectorSource(rtext$text))
        corpus <- tm_map(corpus, content_transformer(iconv), to = "ASCII//TRANSLIT")
        corpus <- tm_map(corpus, content_transformer(gsub), pattern = "on writ of certiorari", replacement = "<DECISION BREAK> on writ of certiorari", ignore.case = TRUE)
        corpus <- tm_map(corpus, content_transformer(gsub), pattern = "on exception to", replacement = "<DECISION BREAK> on writ of certiorari", ignore.case = TRUE)
        corpus <- tm_map(corpus, content_transformer(gsub), pattern = "on petition for writ of certiorari", replacement = "<DECISION BREAK> on writ of certiorari", ignore.case = TRUE)
        corpus <- tm_map(corpus, content_transformer(gsub), pattern = "on writ of certiorari", replacement = "<DECISION BREAK> <KEEP>", ignore.case = TRUE)
        corpus <- tm_map(corpus, content_transformer(gsub), pattern = "on petition for writ of certiorari", replacement = "<DECISION BREAK> <KEEP>", ignore.case = TRUE)
        corpus <- tm_map(corpus, content_transformer(gsub), pattern = "certiorari to the (United States|Court|Supreme)", replacement = "<DECISION BREAK> <KEEP>", ignore.case = TRUE)
        corpus <- tm_map(corpus, content_transformer(gsub), pattern = "appeal from the united states district court", replacement = "<DECISION BREAK> <KEEP>", ignore.case = TRUE)
        corpus <- tm_map(corpus, content_transformer(gsub), pattern = "on bill of complaint", replacement = "<DECISION BREAK> <KEEP>", ignore.case = TRUE)
        decisions <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)
      } #Collect and Pre-Process Text

      # Check if 'decisions' data frame is empty
      if (nrow(decisions) == 0) {
        message("\nError Processing PDF:", file_path)
        return(NULL)
      }


      {
        file_names_processed <- gsub(paste0(dir_path, "/"), "", file_path)
        file_names_processed <- gsub("\\_.*", "", file_names_processed)
        file_names_processed <- gsub(".pdf", "", file_names_processed)
        decisions$argument <- file_names_processed

        case_names <- c()

        for (pdf_file in file_path) {
          pdf_metadata <- pdf_info(pdf_file)
          title <- pdf_metadata$keys$Title
          case_names <- c(case_names, title)
        }

        case_names <- data.frame(case_names)
        case_names$docket_number <- sapply(case_names$case_names, function(x) {
          if (grepl(" ", x) & grepl("-", x)) {
            return(sub(" .*", "", x))
          } else {
            return(NA)
          }
        })
        case_names$published <- sapply(case_names$case_names, function(x) {
          if (grepl("\\(", x)) {
            x <- sub(".*\\(|\\).*", "", x)
            x <- paste0("(", x)
            return(x)
          } else {
            return(NA)
          }
        })

        case_names$case_names <- mapply(function(case_name, published, docket_number) {
          if (!is.na(published)) {
            case_name <- gsub(published, "", case_name, fixed = TRUE)
          }
          if (!is.na(docket_number)) {
            case_name <- gsub(docket_number, "", case_name, fixed = TRUE)
          }
          return(case_name)
        }, case_names$case_names, case_names$published, case_names$docket_number)

        decisions$argument <- case_names$case_names
        decisions$docket_id <- case_names$docket_number
        decisions$docket_id <- gsub("\\.", "", decisions$docket_id)
        decisions$published <- case_names$published


      } #File Metadata
      {
        decisions_test <- decisions %>%
          mutate(text = str_split(text, "\\<DECISION BREAK\\>")) %>%
          unnest(text) %>%
          filter(grepl("\\<Keep\\>", text, ignore.case = TRUE)) %>%
          filter(!grepl("syllabus\\n\\n", text, ignore.case = TRUE)) %>%
          mutate(text = gsub("J.\\n\\n", "<END HEADER>", text, perl = TRUE)) %>%
          mutate(text = gsub("(?<!\\S) {2,}(?!\\S)", " ", text, perl = TRUE)) %>%
          mutate(text = gsub("(?<!\\S)\n(?!\\S)", " \n", text, perl = TRUE)) %>%
          mutate(text = gsub("\\n\\n", " \n\n ", text, perl = TRUE)) %>%
          mutate(text = gsub("\\n\\n ------\\n[ ]*", "<BEGIN FOOTNOTE> ", text)) %>%
          mutate(text = gsub("\\n------\\n", " \n------\n ", text, perl = TRUE)) %>%
          mutate(text = gsub("\\n------\\n", " <BEGIN FOOTNOTE> ", text, perl = TRUE)) %>%
          mutate(text = gsub("in part \\n\\n", "<END HEADER> ", text, ignore.case = T)) %>%
          mutate(text = gsub("in judgement \\n\\n", "<END HEADER> ", text, ignore.case = T)) %>%
          mutate(text = gsub("C\\.J\\.\\, concurring", "<END HEADER> ", text, ignore.case = T)) %>%
          mutate(text = gsub("J\\.\\, concurring", "<END HEADER> ", text, ignore.case = T)) %>%
          mutate(text = gsub("C\\.J\\.\\, dissenting", "<END HEADER> ", text, ignore.case = T)) %>%
          mutate(text = gsub("J\\.\\, dissenting", "<END HEADER> ", text, ignore.case = T)) %>%
          mutate(text = str_replace_all(text, "Opinion of the Court\\s+\\n\\n", "<END HEADER> ")) %>%
          mutate(text = str_replace_all(text, "Opinion of the Court", "<END HEADER> ")) %>%
          mutate(text = str_replace_all(text, "Opinion of .*? , J", "<END HEADER>")) %>%
          mutate(text = str_replace_all(text, "Order of (.*?), C\\.J\\.", "<END HEADER>")) %>%
          mutate(text = str_replace_all(text, "\\n\\n\\s+\\d+", "<BEGIN HEADER>")) %>%
          mutate(text = gsub("\\s{2,}", " ", text)) %>%
          mutate(text = str_replace_all(text, "Cite as:", " <BEGIN HEADER> Cite as:"))  %>%
          mutate(footnotes = text) %>%
          mutate(footnotes = str_extract_all(text, "(?<=<BEGIN FOOTNOTE>)([\\s\\S]*?)(?=<BEGIN HEADER>)")) %>%
          mutate(footnotes = sapply(footnotes, function(x) if (length(x) > 0) paste(x, collapse = "\n\n") else NA_character_))  %>%
          mutate(text = gsub("<BEGIN FOOTNOTE>.*?<END HEADER>", "", text)) %>%
          mutate(text = str_replace_all(text, "<BEGIN HEADER>.*?(<END HEADER>|<END SPECIAL HEADER>)", "")) %>%
          mutate(text = gsub("<BEGIN HEADER>.*?<END SPECIAL HEADER>", "", text)) %>%
          mutate(text = gsub(" C\\. J\\.\\,", "", text)) %>%
          mutate(text = gsub(" C\\.J\\.\\,", "", text)) %>%
          mutate(text = gsub(" J\\.\\,", "", text)) %>%
          mutate(footnotes = gsub("\\n\\n {3,}(\\d+)", "<FOOTNOTE BREAK> \\1", footnotes)) %>%
          mutate(footnotes = gsub("\\n\\n", "", footnotes)) %>%
          mutate(footnotes = gsub("<FOOTNOTE BREAK>", "\n\n", footnotes)) %>%
          mutate(footnotes = gsub("\\-  ", "", footnotes)) %>%
          mutate(footnotes = gsub("\\- ", "", footnotes)) %>%
          mutate(footnotes = gsub("\\n", " ", footnotes)) %>%
          mutate(footnotes = gsub("  (\\d+)", "\n\n\\1", footnotes)) %>%
          mutate(footnotes = gsub("(?<![A-Za-z0-9\\n])\\s{5,}(?![A-Za-z0-9\\n])", " ", footnotes, perl = TRUE)) %>%
          mutate(footnotes = gsub("^\\s+", "", footnotes)) %>%
          mutate(footnotes = gsub("(?<=\\n)(\\d+)", "[\\1]", footnotes, perl = TRUE)) %>%
          mutate(footnotes = gsub("\\n\\n(?=\\[\\d{4,}\\])", "", footnotes, perl = TRUE)) %>%
          mutate(footnotes = str_replace(footnotes, "^(\\d+)", "[\\1]")) %>%
          mutate(footnotes = gsub("\\- ", "", footnotes)) %>%
          mutate(text = gsub("\\n(?=\\d)", " [", text, perl = TRUE)) %>%
          mutate(text = gsub("\\s(?=\\d)", "] ", text, perl = TRUE)) %>%
          mutate(text = gsub("\\s(?=\\n)", "] ", text, perl = TRUE)) %>%
          mutate(text = gsub("\\[(?=\\d)", "", text, perl = TRUE)) %>%
          mutate(text = gsub("\\](?=\\n)", "", text, perl = TRUE)) %>%
          mutate(text = gsub("\\](?=\\s)", "", text, perl = TRUE)) %>%
          mutate(opinion = sub("\\.(.*)", "", text)) %>%
          mutate(opinion = sub("\\.(.*)", "", text)) %>%
          mutate(opinion = trimws(opinion) %>%
                   paste0(".")) %>%
          mutate(opinion_writer = str_extract_all(opinion, "(CHIEF JUSTICE|JUSTICE)\\s+(\\w+)") %>%
                   sapply(paste, collapse = "; ") %>%
                   sapply(function(x) gsub(" joins| join", "", x))) %>%
          mutate(opinion_type = case_when(
            grepl("deliv", opinion, ignore.case = T) & grepl("court", opinion, ignore.case = T) ~ "Majority Opinion",
            grepl("con", opinion) ~ "Concurrence",
            grepl("dis", opinion) ~ "Dissent",
            grepl("con", opinion) & grepl("dis", opinion) & grepl("in part", opinion) ~ "Concur & Dissent (In Part)")) %>%
          mutate(opinion_type = ifelse(is.na(opinion_type), "Per Curiam", opinion_type))  %>%
          filter(!(opinion_type == "Per Curiam" & !grepl("PER CURIAM", text, ignore.case = TRUE))) %>%
          mutate(text = ifelse(opinion_type == "Per Curiam", gsub(".*PER CURIAM", "", text), text)) %>%
          mutate(opinion_writer = ifelse(opinion_type == "Per Curiam", "Per Curiam", opinion_writer)) %>%
          mutate(text = sub(".*?\\.", "", text)) %>%
          mutate(text = trimws(text)) %>%
          mutate(text = gsub("U\\.\\sS\\.\\sC\\.\\s\\?", "U. S. C. \u00A7", text)) %>%
          mutate(text = gsub("\\?(?=\\d)", "\u00A7", text, perl = T)) %>%
          mutate(text = gsub("\\s{2,}", " ", text)) %>%
          mutate(text = gsub("SUPREME COURT OF THE UNITED STATES.*", "", text)) %>%
          mutate(text = gsub("-\\n", "", text)) %>%
          mutate(text = gsub("\\n", " ", text)) %>%
          select(-c(opinion)) %>%
          mutate(word_count = str_count(text, "\\w+")) %>%
          mutate(text = gsub("\\.(?!.*\\..*$)\\s?.*", ".", text, perl = TRUE)) %>%
          mutate(text = gsub("\\<END HEADER\\>", "", text, perl = T)) %>%
          mutate(text = gsub("\\<BEGIN HEADER\\>", "", text, perl = T)) %>%
          mutate(text = gsub("\\,\\)", ", J.)", text, perl = T)) %>%
          mutate(text = gsub("\\, \\)", ", J.)", text, perl = T)) %>%
          select(argument, docket_id, published, text, footnotes, opinion_writer, opinion_type, word_count)
      } #Process & Clean Docket Frame

    } #Process Opinions

    footnotes <- NULL
    opinion <- NULL
    opinion_type <- NULL
    opinion_writer <- NULL
    argument <- NULL
    docket_id <- NULL
    published <- NULL


    start_time <- Sys.time()
    count <- 1
    for (i in files) {
      tryCatch({
        cleaned_decisions <- suppressWarnings(decisions_cleaner(file_path = i, dir_path = dir_path))
        all_decisions <- rbind(all_decisions, cleaned_decisions)
        if (count %% 25 == 0) {
          message("\nCompleted ", count, " Decisions of ", length(files))
        }
        count <- count + 1
      }, error = function(e) {
        cat("Error with Decision ", i, ":\n")
        print(e)
      })
    }

    end_time <- Sys.time()
    elapsed_time <- end_time - start_time

    cat("\n- - - - - - - - COMPLETION SUMMARY - - - - - - - -")
    cat("\nCompletion Time: ", round(as.numeric(elapsed_time), 2), "Seconds")
    cat("\nNumber of Unique Decisions: ", length(unique(all_decisions$argument)))
    cat("\nNumber of Majority Opinions: ", sum(all_decisions$opinion_type == "Majority Opinion"))
    cat("\nNumber of Dissents: ", sum(all_decisions$opinion_type == "Dissent"))
    cat("\nNumber of Concurrences: ", sum(all_decisions$opinion_type == "Concurrence"))
    cat("\nNumber of Per Curiam: ", sum(all_decisions$opinion_type == "Per Curiam"))

    return(all_decisions)
  }


} # Decision Processor 2

################################################################################
# Load Data
################################################################################
{
  ROBERTS <- readPNG("stat_pack_OT23/Figures/justice_images/Roberts.png")
  ALITO <- readPNG("stat_pack_OT23/Figures/justice_images/Alito.png")
  THOMAS <- readPNG("stat_pack_OT23/Figures/justice_images/Thomas.png")
  SOTOMAYOR <- readPNG("stat_pack_OT23/Figures/justice_images/Sotomayor.png")
  KAGAN <- readPNG("stat_pack_OT23/Figures/justice_images/Kagan.png")
  GORSUCH <- readPNG("stat_pack_OT23/Figures/justice_images/Gorsuch.png")
  KAVANAUGH <- readPNG("stat_pack_OT23/Figures/justice_images/Kavanaugh.png")
  BARRETT <- readPNG("stat_pack_OT23/Figures/justice_images/Barrett.png")
  JACKSON <- readPNG("stat_pack_OT23/Figures/justice_images/Jackson.png")
  BREYER <- readPNG("stat_pack_OT23/Figures/justice_images/Breyer.png")
  GINSBURG <- readPNG("stat_pack_OT23/Figures/justice_images/Ginsburg.png")
  KENNEDY <- readPNG("stat_pack_OT23/Figures/justice_images/Kennedy.png")

  justice_image_labels <- c(
    ALITO = "<img src='stat_pack_OT23/Figures/justice_images/Alito.png' width='75' /><br>",
    ROBERTS = "<img src='stat_pack_OT23/Figures/justice_images/Roberts.png' width='75' /><br>",
    THOMAS = "<img src='stat_pack_OT23/Figures/justice_images/Thomas.png' width='75' /><br>",
    SOTOMAYOR = "<img src='stat_pack_OT23/Figures/justice_images/Sotomayor.png' width='75' /><br>",
    KAGAN = "<img src='stat_pack_OT23/Figures/justice_images/Kagan.png' width='75' /><br>",
    GORSUCH = "<img src='stat_pack_OT23/Figures/justice_images/Gorsuch.png' width='75' /><br>",
    KAVANAUGH = "<img src='stat_pack_OT23/Figures/justice_images/Kavanaugh.png' width='75' /><br>",
    BARRETT = "<img src='stat_pack_OT23/Figures/justice_images/Barrett.png' width='75' /><br>",
    JACKSON = "<img src='stat_pack_OT23/Figures/justice_images/Jackson.png' width='75' /><br>",
    BREYER = "<img src='stat_pack_OT23/Figures/justice_images/Breyer.png' width='75' /><br>",
    GINSBURG = "<img src='stat_pack_OT23/Figures/justice_images/Ginsburg.png' width='75' /><br>",
    KENNEDY = "<img src='stat_pack_OT23/Figures/justice_images/Kennedy.png' width='75' /><br>"
  )
} #Justice Images (Oyez)
{

  shorthand_case_names <- read.csv('ot23_decisions/shorthand_case_names.csv', as.is = T)


} #Shorthand Case Names...
{

  scdb_justice_names <- data.frame(
    justice = c(103:108, 109:118),
    justice_name = c('STEVENS', "O'CONNOR", 'SCALIA', 'KENNEDY', 'SOUTER', 'THOMAS', 'GINSBURG', 'BREYER', 'ROBERTS', 'ALITO', 'SOTOMAYOR', 'KAGAN', 'GORSUCH', 'KAVANAUGH', 'BARRETT', 'JACKSON'))

} #SCDB Justice ID Conversion
{
  scdb_cases_2023 <- get(load("stat_pack_OT23/statpack_replication/Misc Data/scdb_cases_2023.rdata"))
  load("stat_pack_OT23/statpack_replication/Misc Data/scdb_justices_2023.rdata")

} #SCDB Data (Updated September 2023)
{

  decisions_ot_23 <- read.csv(file = "ot23_decisions/OT_23_Decisions.csv")
  decisions_ot_23 <- decisions_ot_23 %>%
    mutate(`Case` = gsub('\\,', '', `Case`),
           Decision = gsub('\\,', '', Decision))


} # Load OT23 Decisions Table
{

  combined_sitting_calendar <- read.csv('stat_pack_OT23/Statpack Replication Data/Oral Arguments/Calendar - EmpiricalSCOTUS/combined_sittings_calendar.csv', as.is = T)
  names(combined_sitting_calendar) = gsub('\\.', ' ', names(combined_sitting_calendar))

} #OA Sitting Calendars
{

  feldman_attorney <- readxl::read_excel("stat_pack_OT23/statpack_replication/attorney_information_feldman/AA-FINAL.xlsx")
  feldman_attorney <- feldman_attorney[,c(1:14)]
  names(feldman_attorney) <- c('name', 'law_school', 'scotus_clerkship', 'clerkship_justice', 'present_SG', 'previous_SG', 'SG_experience', 'firm', 'state', 'gender', 'undergrad_school', 'repeater', 'previous_cases', 'arguments_2023')

} # Load Feldman Attorney Information Data
{

  split_data <- read.csv("stat_pack_OT23/statpack_replication/Misc Data/Splits2020-2022.csv")
  split_data <- data.frame(term = c(2020, 2021, 2022),
                           total_cases = c(13, 19, 11),
                           ideological_split = c(8, 14, 5))

  splits_23 <- decisions_ot_23 %>%
    filter(Coalition %in% c('(6-3)', '(5-3)')) %>%
    select(Docket, ROBERTS:JACKSON) %>%
    mutate(across(ROBERTS:JACKSON, ~ ifelse(. >= 1, 1, 0)))  %>%
    pivot_longer(cols = ROBERTS:JACKSON, names_to = "justice_name", values_to = "majority") %>%
    mutate(justice_name = ifelse(justice_name %in% c('KAGAN', 'SOTOMAYOR', 'JACKSON'), 'Liberal', 'Conservative'))

  ideological_splits <- data.frame()

  for (i in unique(splits_23$Docket)){
    temp_docket <- splits_23 %>%
      filter(Docket == i)
    majority <- temp_docket$justice_name[temp_docket$majority == 1]
    minority <- temp_docket$justice_name[temp_docket$majority == 0]

    if (all(majority == 'Conservative') & all(minority == 'Liberal')){
      ideo_split <- 1
    } else {
      ideo_split <- 0
    }

    temp_complete <- data.frame(docket = i,
                                ideo_split = ideo_split)

    ideological_splits <- bind_rows(ideological_splits, temp_complete)

  }


  ideological_splits <- ideological_splits %>%
    group_by(ideo_split) %>%
    summarise(count = n())

  split_data <- split_data %>%
    add_row(term = 2023,
            total_cases = sum(ideological_splits$count),
            ideological_split = ideological_splits$count[ideological_splits$ideo_split == 1])

} # Feldman Ideological Split Data

################################################################################
# Topline Info
################################################################################

{

  toplines <- list()

  {

    toplines[['Most Authored Opinions']] <- list()

    decisions_23 <- decision_processor2(dir_path = "ot23_decisions/decision_pdfs_OT23") #OT23

    most_authored_opinions <- decisions_23 %>%
      select(opinion_writer, opinion_type) %>%
      mutate(opinion_writer = gsub('\\;.*', '', opinion_writer),
             opinion_writer = gsub('(CHIEF JUSTICE |JUSTICE )', '', opinion_writer)) %>%
      group_by(opinion_type, opinion_writer) %>%
      summarise(count = n())

    opinion_types <- c('Majority Opinion', 'Concurrence', 'Dissent')

    for (i in opinion_types){

      temp <- most_authored_opinions %>%
        filter(opinion_type == i) %>%
        filter(count == max(count))

      toplines[['Most Authored Opinions']][[i]] <- temp$opinion_writer

    }

  } # Most Authored Opinions

  {

    toplines[['Decisions (Coalition)']] <- list()

    by_coalition <- decisions_ot_23 %>%
      select(Coalition) %>%
      group_by(Coalition) %>%
      summarise(count = n()) %>%
      mutate(Coalition = ifelse(Coalition == '(6-3)', '(6-3) Any Combination', Coalition))

    ideologically_split <- decisions_ot_23 %>%
      filter(Coalition == '(6-3)') %>%
      mutate(split = ifelse(SOTOMAYOR < 0 & KAGAN < 0 & JACKSON < 0, 1, 0)) %>%
      select(split)

    by_coalition <- by_coalition %>%
      add_row(Coalition = '(6-3) Ideologically Split', count = sum(ideologically_split$split)) %>%
      mutate(count = ifelse(Coalition == '(6-3) Any Combination', count - sum(ideologically_split$split), count)) %>%
      arrange(Coalition)

    for (i in 1:nrow(by_coalition)){

      temp_row <- by_coalition[i,]

      toplines[['Decisions (Coalition)']][[temp_row$Coalition]] <- temp_row$count


    }

  } #Decisions by Coalition

  {
    toplines[['Longest Argument (Min)']] <- list()

    base_url <- "https://github.com/JakeTruscott/scotustext/raw/master/Data/"
    rdata_url <- paste0(base_url, "scotus_transcripts_23.rdata")
    load(url(rdata_url))

    longest_argument <- scotus_OT23 %>%
      mutate(elapsed = text_stop - text_start) %>%
      group_by(case_name) %>%
      summarise(elapsed_time = sum(elapsed)) %>%
      arrange(desc(elapsed_time)) %>%
      filter(elapsed_time == max(elapsed_time))

    toplines[['Longest Argument (Min)']] <- longest_argument$case_name

    most_frequent_arguing_counsel <- scotus_OT23 %>%
      filter(speaker_type == 'Attorney') %>%
      select(case_name, speaker) %>%
      unique() %>%
      group_by(speaker) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      filter(count == max(count))

    toplines[['Most Frequent Arguing Counsel']] <- most_frequent_arguing_counsel$speaker

    toplines[['Most Time Speaking']]

    most_time_speaking_term_justice <- scotus_OT23 %>%
      mutate(elapsed = text_stop - text_start) %>%
      filter(speaker_type == 'Justice') %>%
      group_by(speaker) %>%
      summarise(total_time_speaking = sum(elapsed)) %>%
      filter(total_time_speaking == max(total_time_speaking))

    toplines[['Most Time Speaking']][['(Term, Justice)']] <- most_time_speaking_term_justice$speaker

    most_time_speaking_argument_justice <- scotus_OT23 %>%
      mutate(elapsed = text_stop - text_start) %>%
      filter(speaker_type == 'Justice') %>%
      group_by(case_name, speaker) %>%
      summarise(total_time_speaking = sum(elapsed)) %>%
      ungroup() %>%
      filter(total_time_speaking == max(total_time_speaking))

    toplines[['Most Time Speaking']][['(Argument, Justice)']]  <- most_time_speaking_argument_justice$speaker

    most_time_speaking_argument_attorney <- scotus_OT23 %>%
      mutate(elapsed = text_stop - text_start) %>%
      filter(speaker_type == 'Attorney') %>%
      group_by(case_name, speaker) %>%
      summarise(total_time_speaking = sum(elapsed)) %>%
      ungroup() %>%
      filter(total_time_speaking == max(total_time_speaking))

    toplines[['Most Time Speaking']][['(Argument, Attorney)']]  <- most_time_speaking_argument_attorney$speaker


  } # Arguments

  {

    load('docket_parser/docket_filings_ot_23.rdata') #Load OT23 Dockets

    total_petitions <- dockets %>%
      filter(grepl('\\-', docket_number))

    toplines[['Total Petitions Filed (Approx.)']] <- nrow(total_petitions)

  } # Dockets

  toplines

} #Topline Information (Returns 'toplines')


################################################################################
# Attorney Information (Feldman RA)
################################################################################

{

  colours <- data.frame(
    school = c('Other', 'Harvard', 'Yale', 'Chicago', 'Virginia', 'Texas', 'NYU', 'Georgetown', 'Michigan', 'Stanford', 'Duke'),
    outline = c('gray5', '#808285', '#978d85', '#767676', '#F84C1E', '#333F48', '#8B139C', '#8D817B', '#00274C', '#4D4F53', '#012169'),
    fill = c('gray5', '#A41034', '#00356b', '#800000', '#232D4B', '#BF5700', '#56018D', '#041E42', '#FFCB05', '#8C1515', '#00539B')
  )

  law_school_data <- feldman_attorney %>%
    group_by(law_school) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    mutate(law_school = ifelse(count >= 3, law_school, 'Other')) %>%
    group_by(law_school) %>%
    summarise(count = sum(count)) %>%
    mutate(other = ifelse(law_school == 'Other', 0, 1)) %>%
    arrange(desc(other), desc(count)) %>%
    select(-c(other)) %>%
    mutate(law_school = gsub(' University', '', gsub('University of ', '', law_school)))

  law_school_figure <- law_school_data %>%
    left_join(colours, by = c("law_school" = "school")) %>%
    ggplot( aes(x = forcats::fct_reorder(law_school, count), y = count)) +
    geom_bar(stat = 'identity', aes(fill = fill, colour = outline), size = 1) +
    geom_hline(yintercept = 0) +
    geom_label(aes(x = forcats::fct_reorder(law_school, count), y = count, label = count, hjust = 2, size = 7)) +
    scale_y_continuous(breaks = seq(5, 30, 5)) +
    labs(x = '',
         y = '') +
    coord_flip() +
    theme_bw() +
    scale_fill_identity() +  # Ensure the fill colors are used directly
    scale_colour_identity() +  # Ensure the outline colors are used directly
    theme(legend.position = 'none',
          legend.text = element_text(size = 15, colour = 'gray5'),
          plot.title = element_text(size = 18, hjust = 0.5),
          axis.title = element_text(size = 15),
          axis.ticks.y = element_blank(),
          axis.text = element_text(size = 20, colour = 'black'),
          axis.line = element_line(colour = 'black'),
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'))


} # Law Schools

{

  attorney_gender <- feldman_attorney %>%
    select(gender) %>%
    group_by(gender) %>%
    summarise(count = n()) %>%
    mutate(percent = round(count/nrow(feldman_attorney), 2)) %>%
    ggplot(aes(x = "", y = count, fill = gender)) +
    geom_bar(stat = "identity", colour = 'gray5') +
    coord_polar(theta = 'y', start = 0) +
    theme_void() +
    labs(
      x = ' ',
      y = ' ',
      fill = ' ') +
    scale_fill_manual(values = c('deepskyblue3', 'coral')) +
    geom_label(aes(label = paste0(percent*100, '%\n', gender)), position = position_stack(vjust = 0.5), color = "gray5", size=10, show.legend = F) +
    theme(legend.position = 'none',
          legend.text = element_text(size = 15, colour = 'gray5'),
          plot.title = element_text(size = 25, hjust = 0.5),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = 12),
          axis.line = element_line(colour = 'black'),
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'))

} # Gender

{

  scotus_clerkship <- feldman_attorney %>%
    select(scotus_clerkship) %>%
    mutate(scotus_clerkship = ifelse(scotus_clerkship == 'Y', 'Clerkship', 'None')) %>%
    group_by(scotus_clerkship) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    select(scotus_clerkship, count) %>%
    mutate(percent = round(count/nrow(feldman_attorney), 2)) %>%
    ggplot(aes(x =  '', y = count, fill = scotus_clerkship)) +
    geom_bar(stat = "identity", colour = 'gray5') +
    coord_polar(theta = 'y', start = 0) +
    theme_void() +
    labs(
      x = ' ',
      y = ' ',
      fill = ' ') +
    scale_fill_manual(values = c('grey50', 'white')) +
    geom_label(aes(label = paste0(percent*100, '%\n', scotus_clerkship)), position = position_stack(vjust = 0.5), color = "gray5", size=10, show.legend = F) +
    theme(legend.position = 'none',
          plot.title = element_text(size = 25, hjust = 0.5),
          legend.text = element_text(size = 15, colour = 'gray5'),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = 12),
          axis.line = element_line(colour = 'black'),
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'))


  clerkships_by_justice <- feldman_attorney %>%
    select(scotus_clerkship, clerkship_justice) %>%
    mutate(scotus_clerkship = ifelse(scotus_clerkship == 'Y', 'Yes', 'No'),
           clerkship_justice = ifelse(grepl('\\,', clerkship_justice), 'Multiple', clerkship_justice),
           clerkship_justice = ifelse(clerkship_justice == "Day O'Connor", "O'Connor", clerkship_justice)) %>%
    group_by(scotus_clerkship, clerkship_justice) %>%
    filter(!clerkship_justice == 'N/A') %>%
    summarise(count = n()) %>%
    mutate(clerkship_justice = ifelse(clerkship_justice == 'N/A', 'None', clerkship_justice)) %>%
    ungroup() %>%
    select(clerkship_justice, count) %>%
    ggplot(aes(x = forcats::fct_reorder(clerkship_justice, count), y = count)) +
    geom_bar(stat = "identity", colour = 'gray5', fill = 'gray') +
    theme_bw() +
    scale_y_continuous(lim = c(0, 9), breaks = seq(1, 9, 1)) +
    coord_flip() +
    geom_label(aes(x = forcats::fct_reorder(clerkship_justice, count), y = count, label = count, hjust = 2, size = 7)) +
    labs(x = '',
         y = '') +
    theme(legend.position = 'none',
          plot.title = element_text(size = 18, hjust = 0.5),
          legend.text = element_text(size = 15, colour = 'gray5'),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 20, colour = 'black'),
          axis.line = element_line(colour = 'black'),
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'))

} # SCOTUS Clerkship

{

  sg_experience <- feldman_attorney %>%
    select(SG_experience) %>%
    mutate(SG_experience = ifelse(SG_experience == 'Y', 'Yes', 'No')) %>%
    group_by(SG_experience) %>%
    summarise(count = n()) %>%
    mutate(percent = round(count/nrow(feldman_attorney), 2)) %>%
    ggplot(aes(x =  '', y = count, fill = SG_experience)) +
    geom_bar(stat = "identity", colour = 'gray5') +
    coord_polar(theta = 'y', start = 0) +
    theme_void() +
    labs(
      x = ' ',
      y = ' ',
      fill = ' ') +
    scale_fill_manual(values = c('grey50', 'white')) +
    geom_label(aes(label = paste0(percent*100, '%\n', SG_experience)), position = position_stack(vjust = 0.5), color = "gray5", size=10, show.legend = F) +
    theme(legend.position = 'none',
          plot.title = element_text(size = 25, hjust = 0.5),
          legend.text = element_text(size = 15, colour = 'gray5'),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = 12),
          axis.line = element_line(colour = 'black'),
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'))
} #SG Experience

{

  ggsave(scotus_clerkship, file = 'stat_pack_OT23/Figures/statpack_figures/percentage_scotus_clerkship.png')
  ggsave(attorney_gender, file = 'stat_pack_OT23/Figures/statpack_figures/percentage_gender.png')
  ggsave(sg_experience, file = 'stat_pack_OT23/Figures/statpack_figures/percentage_sg_experience.png')
  ggsave(law_school_figure, file = 'stat_pack_OT23/Figures/statpack_figures/law_school_figure.png', width = 12, height = 8)
  ggsave(clerkships_by_justice, file = 'stat_pack_OT23/Figures/statpack_figures/clerkships_by_justice.png', width = 12, height = 8)


} # Save

{

  attorney_summary_stats <- feldman_attorney %>%
    select(name, firm, law_school, clerkship_justice, repeater, previous_cases) %>%
    mutate(name = str_to_title(name),
           name = gsub('\\,', '', name),
           clerkship_justice = ifelse(clerkship_justice == 'N/A', NA, clerkship_justice),
           law_school = gsub(' University', '', gsub('University of ', '', law_school)),
           clerkship_justice = ifelse(clerkship_justice == "Day O'Connor", "O'Connor", clerkship_justice),
           clerkship_justice = gsub('\\,', ' and ', clerkship_justice),
           firm = gsub('\\&', 'and', firm),
           firm = gsub('\\,', '', firm),
           law_school = gsub('\\,', '', law_school),
           repeater = ifelse(repeater == 1, 'Yes', 'No')) %>%
    rename(`Attorney` = name,
           `Firm` = firm,
           `Law School` = law_school,
           `SCOTUS Clerkship` = clerkship_justice,
           `Repeat Appearance` = repeater,
           `Num. Previous Cases` = previous_cases) %>%
    arrange(`Attorney`)

  attorney_summary_stats$Attorney[13] <- 'Benjamin Aguinaga'

  write.csv(attorney_summary_stats[c(1:25),], "stat_pack_OT23/Statpack Replication Data/Oral Arguments/Attorney Information/attorney_summary_stats_1.csv", row.names = F, quote = F)
  write.csv(attorney_summary_stats[c(26:50),], "stat_pack_OT23/Statpack Replication Data/Oral Arguments/Attorney Information/attorney_summary_stats_2.csv", row.names = F, quote = F)
  write.csv(attorney_summary_stats[c(51:75),], "stat_pack_OT23/Statpack Replication Data/Oral Arguments/Attorney Information/attorney_summary_stats_3.csv", row.names = F, quote = F)
  write.csv(attorney_summary_stats[c(76:102),], "stat_pack_OT23/Statpack Replication Data/Oral Arguments/Attorney Information/attorney_summary_stats_4.csv", row.names = F, quote = F)

} # Summary Tables (Name, Firm, Law School, Clerkship, Repeat, Former Cases )

################################################################################
# Decisions
################################################################################

{

  decisions_matrix_data <- decisions_ot_23 %>%
    select(ROBERTS, THOMAS, ALITO, SOTOMAYOR, KAGAN, GORSUCH, KAVANAUGH, BARRETT, JACKSON) %>%
    mutate(across(everything(), ~ ifelse(. > 0, 1, 0))) #Get Justices and Replace Vote w/ Majority or Dissent
  judge_matrix <- as.matrix(decisions_matrix_data) #Convert to Matrix

  n_judges <- ncol(decisions_matrix_data) # Number of Justices
  agreement_matrix <- matrix(NA, nrow = n_judges, ncol = n_judges) # Create Empty Matrix

  # Calculate the percentage agreement between each pair of judges
  for (i in 1:n_judges) {
    for (j in 1:n_judges) {
      valid_cases <- !is.na(decisions_matrix_data[, i]) & !is.na(decisions_matrix_data[, j])
      agreement_matrix[i, j] <- round(sum(decisions_matrix_data[valid_cases, i] == decisions_matrix_data[valid_cases, j], na.rm = TRUE) / sum(valid_cases) * 100, 2)
    }
  }

  for (i in 1:ncol(agreement_matrix)) {
    for (j in 1:ncol(agreement_matrix)) {
      if (i == j) {
        agreement_matrix[i, j] <- ""
      } else if (i < j) {
        agreement_matrix[i, j] <- ""
      }
    }
  } # Replace Diagonal & Above w/ ""

  colnames(agreement_matrix) <- colnames(decisions_matrix_data) #Add Column Names
  rownames(agreement_matrix) <- colnames(decisions_matrix_data) #Add Row Names

  row_names_column <- paste0(stringr::str_to_title(colnames(decisions_matrix_data)), ".png") #Add Row Name for Image Path

  agreement_matrix <- data.frame(cbind(row_names_column, agreement_matrix)) #Combine Row Names Column to Matrix
  names(agreement_matrix)[1] <- ""


  write.table(agreement_matrix[-1,-10], file = 'stat_pack_OT23/Tables/decision_tables/agreement_matrix.csv', sep = ',', quote = FALSE, row.names = F) #Save

  agreement_matrix[,1] = gsub('\\.png', '', toupper(rownames(agreement_matrix)))

  write.table(agreement_matrix, file = 'stat_pack_OT23/Statpack Replication Data/Decisions/Agreement Matrix/OT23_justice_agreement_matrix.csv', sep = ',', quote = FALSE, row.names = F) #Save



} # Justice Agreement Vote Matrix (OT23)

{

  for (i in unique(shorthand_case_names$sitting)){

    decision_descriptions_test <- decisions_ot_23 %>%
      rowwise() %>%
      mutate(Coalition = ifelse(any(c_across(ROBERTS:JACKSON) %in% c(2,4,7)), paste0(Coalition, '*'), Coalition)) %>%
      ungroup() %>%
      select(Docket, Date_Argued, Date_Decided, Lower_Court, Decision, Author, Coalition) %>%
      rename(docket = Docket) %>%
      left_join(shorthand_case_names, by = 'docket') %>%
      mutate(docket = paste0('(', docket, ')')) %>%
      mutate(Decision = gsub('\\&', '\\\\&', Decision),
             short_hand = gsub('\\,', ';', short_hand)) %>%
      select(short_hand, description, docket, Lower_Court, Decision, Author, Coalition, sitting) %>%
      filter(sitting == i) %>%
      select(-c(sitting)) %>%
      mutate(description = iconv(description, to ='utf-8')) %>%
      mutate(short_hand = ifelse(docket == '(22-451)', paste0(short_hand, ' (Together w. 22-1219)'), short_hand))


    write.table(decision_descriptions_test, file = paste0('stat_pack_OT23/Tables/decision_tables/decision_description_', i, '.csv'), sep = ',', quote = FALSE, row.names = F, fileEncoding = 'UTF-8')

    write.table(decision_descriptions_test, file = paste0('stat_pack_OT23/Statpack Replication Data/Decisions/Decision Descriptions/', i, '_Decision_Descriptions_OT23.csv'), sep = ',', quote = F, row.names = F, fileEncoding = 'UTF-8')


  }


} # Decision-Level Breakdowns

{

  opinion_type_by_justice_ot23 <- decisions_ot_23 %>%
    select(ROBERTS, THOMAS, ALITO, SOTOMAYOR, KAGAN, GORSUCH, KAVANAUGH, BARRETT, JACKSON) %>%
    pivot_longer(cols = c(ROBERTS, THOMAS, ALITO, SOTOMAYOR, KAGAN, GORSUCH, KAVANAUGH, BARRETT, JACKSON), names_to = "justice", values_to = "decision") %>%
    mutate(decision_type = case_when(
      .default = NA,
      decision == 100 ~ "Majority",
      decision == 2 ~ "Concurrence",
      decision == 4 ~ "Concurrence",
      decision == 5 ~ "Other Concurrence",
      decision == 7 ~ "Other Concurrence",
      decision == -1 ~ "Dissent",
      decision == -3 ~ "Dissent")) %>%
    select(-c(decision)) %>%
    filter(!is.na(decision_type)) %>%
    group_by(justice, decision_type) %>%
    summarise(count = n()) %>%
    pivot_wider(names_from = decision_type, values_from = count, values_fill = 0) %>%
    select(justice, Majority, Concurrence, `Other Concurrence`, Dissent)

  write.table(opinion_type_by_justice_ot23, file = 'stat_pack_OT23/Tables/decision_tables/opinion_type_by_justice_ot23.csv', sep = ',', quote = FALSE, row.names = F) #Save



} #Table of Opinion Types by Justice (OT23)

{


  decisions_by_justice_1 <- decisions_ot_23 %>%
    select(Case, Docket, Coalition, ROBERTS, THOMAS, ALITO, SOTOMAYOR, KAGAN) %>%
    pivot_longer(cols = c(ROBERTS, THOMAS, ALITO, SOTOMAYOR, KAGAN), names_to = "justice", values_to = "decision") %>%
    mutate(justice = factor(justice, levels = c('ROBERTS', 'THOMAS', 'ALITO', 'SOTOMAYOR', 'KAGAN'))) %>%
    filter(decision == 100) %>%
    select(-c(decision)) %>%
    arrange(justice) %>%
    rename(docket = Docket) %>%
    left_join(shorthand_case_names, by = 'docket') %>%
    select(short_hand, Coalition, justice) %>%
    mutate(Case = paste0(short_hand, ' ', Coalition)) %>%
    select(Case, justice) %>%
    group_by(justice) %>%
    mutate(justice_count = row_number()) %>%
    mutate(justice = ifelse(justice_count == 1, as.character(justice), "")) %>%
    select(-c(justice_count))

  write.table(decisions_by_justice_1, file = 'stat_pack_OT23/Tables/decision_tables/decisions_by_justice_1.csv', sep = ',', quote = FALSE, row.names = F) #Save

  decisions_by_justice_2 <- decisions_ot_23 %>%
    select(Case, Docket, Coalition, GORSUCH, KAVANAUGH, BARRETT, JACKSON) %>%
    pivot_longer(cols = c(GORSUCH, KAVANAUGH, BARRETT, JACKSON), names_to = "justice", values_to = "decision") %>%
    mutate(justice = factor(justice, levels = c('GORSUCH', 'KAVANAUGH', 'BARRETT', 'JACKSON'))) %>%
    filter(decision == 100) %>%
    select(-c(decision)) %>%
    arrange(justice) %>%
    rename(docket = Docket) %>%
    left_join(shorthand_case_names, by = 'docket') %>%
    select(short_hand, Coalition, justice) %>%
    mutate(Case = paste0(short_hand, ' ', Coalition)) %>%
    select(Case, justice) %>%
    group_by(justice) %>%
    mutate(justice_count = row_number()) %>%
    mutate(justice = ifelse(justice_count == 1, as.character(justice), "")) %>%
    select(-c(justice_count))

  write.table(decisions_by_justice_2, file = 'stat_pack_OT23/Tables/decision_tables/decisions_by_justice_2.csv', sep = ',', quote = FALSE, row.names = F) #Save


} # Majority Opinions by Justice (OT23)

{

  decisions_by_coalition <- decisions_ot_23 %>%
    rename(docket = Docket) %>%
    left_join(shorthand_case_names, by = 'docket') %>%
    select(short_hand, Coalition) %>%
    rename(case = short_hand,
           coalition = Coalition) %>%
    mutate(case = ifelse(grepl('U\\.\\S\\.', case), case, gsub(' v\\..*', '', case)))

  max_decision_rows <- decisions_by_coalition %>%
    group_by(coalition) %>%
    summarise(count = n()) %>%
    select(count) %>%
    filter(count == max(count))

  coalition_types <- c('(9-0)', '(8-1)', '(7-2)', '(6-3)', '(5-4)')

  decisions_by_coalition_combined <- data.frame(matrix(nrow = max_decision_rows$count, ncol = 5))
  colnames(decisions_by_coalition_combined) <- factor(coalition_types)

  for (i in unique(decisions_by_coalition$coalition)){

    temp_coalition <- i
    temp_decisions <- decisions_by_coalition %>%
      filter(coalition == i)
    temp_cases <- temp_decisions$case

    for (case in 1:length(temp_cases)){
      decisions_by_coalition_combined[case, temp_coalition] <- temp_cases[case]
    }


  } #Populate Coalition-Level Cases

  decisions_by_coalition_combined <- decisions_by_coalition_combined %>%
    mutate_all(~ifelse(is.na(.), "       ", .))


  write.table(decisions_by_coalition_combined, file = 'stat_pack_OT23/Tables/decision_tables/decisions_by_coalition.csv', row.names = F, quote = F, sep = ',')

  write.table(decisions_by_coalition_combined, file = 'stat_pack_OT23/Statpack Replication Data/Decisions/Decisions by Coalition/decisions_by_coalition_OT23.csv', row.names = F, quote = F, sep = ',')


} #Cases by Coalition Type

{

  decisions_by_coalition_longitudinal <- decisions_ot_23 %>%
    rename(docket = Docket) %>%
    left_join(shorthand_case_names, by = 'docket') %>%
    select(short_hand, Coalition) %>%
    rename(case = short_hand,
           coalition = Coalition) %>%
    mutate(coalition = case_when(
      .default = '(9-0)',
      coalition %in% c('(8-1)') ~ '(8-1) or (8-0)',
      coalition %in% c('(7-2)', '(7-1)') ~  '(7-2) or (7-1)',
      coalition %in% c('(6-3)', '(6-2)') ~ '(6-3) or (6-2)',
      coalition %in% c('(5-4)', '(5-3)') ~ '(5-4) or (5-3)',
      coalition == '(4-4)' ~ '(4-4)')) %>%
    mutate(case = ifelse(grepl('U\\.\\S\\.', case), case, gsub(' v\\..*', '', case)))


  decisions_by_coalition_2018_2022 <- scdb_justices_2023 %>%
    filter(!is.na(dateArgument)) %>%
    select(docket, term, majVotes) %>%
    unique() %>%
    filter(term >= 2018) %>%
    mutate(coalition = case_when(
      majVotes == 9 ~ '(9-0)',
      majVotes == 8 ~ '(8-1) or (8-0)',
      majVotes == 7 ~ '(7-2) or (7-1)',
      majVotes == 6 ~ '(6-3) or (6-2)',
      majVotes == 5 ~ '(5-4) or (5-3)',
      majVotes == 4 ~ '(4-4)')) %>%
    filter(!majVotes == 4) %>%
    select(term, coalition, docket) %>%
    bind_rows(decisions_by_coalition_longitudinal %>%
                mutate(term = 2023)) %>%
    group_by(term, coalition) %>%
    summarise(count = n()) %>%
    ggplot(aes(x = factor(term), y = count, group = coalition)) +
    geom_bar(stat = 'identity', fill = 'gray50', position = position_dodge2(0.9), colour = 'gray5') +
    scale_y_continuous(lim = c(0, 50), breaks = seq(10, 40, 10)) +
    facet_wrap(~coalition, nrow = 6) +
    geom_label(aes(label = count), vjust = -0.75) +
    geom_hline(yintercept = 0) +
    theme_bw() +
    labs(
      x = '\nTerm\n',
      y = '\nCount\n') +
    theme(legend.position = 'none',
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'),
          axis.text = element_text(size = 14, colour = 'black'),
          axis.title = element_text(size = 12, colour = 'black'))


    ggsave("stat_pack_OT23/Figures/statpack_figures/decisions_by_coalition_2018_2023.png", decisions_by_coalition_2018_2022, width = 8, height = 10, unit = 'in')


    decisions_by_coalition_2018_2022 <- scdb_justices_2023 %>%
      filter(!is.na(dateArgument)) %>%
      select(docket, term, majVotes) %>%
      unique() %>%
      filter(term >= 2018) %>%
      mutate(coalition = case_when(
        majVotes == 9 ~ '(9-0)',
        majVotes == 8 ~ '(8-1) or (8-0)',
        majVotes == 7 ~ '(7-2) or (7-1)',
        majVotes == 6 ~ '(6-3) or (6-2)',
        majVotes == 5 ~ '(5-4) or (5-3)',
        majVotes == 4 ~ '(4-4)')) %>%
      filter(!majVotes == 4) %>%
      select(term, coalition, docket) %>%
      bind_rows(decisions_by_coalition_longitudinal %>%
                  mutate(term = 2023)) %>%
      group_by(term, coalition) %>%
      summarise(count = n())

    write.table(decisions_by_coalition_2018_2022, file = 'stat_pack_OT23/Statpack Replication Data/Decisions/Decisions by Coalition/decisions_by_coalition_OT18_OT23.csv', sep = ',', quote = F, row.names = F)

    } #Distribution of Coalitions (Current v. Past)

{

  decisions_by_coalition_longitudinal <- decisions_ot_23 %>%
    rename(docket = Docket) %>%
    left_join(shorthand_case_names, by = 'docket') %>%
    select(short_hand, Coalition) %>%
    rename(case = short_hand,
           coalition = Coalition) %>%
    mutate(coalition = case_when(
      coalition %in% c('(9-0)', '(8-0)') ~ '(9-0) or (8-0)',
      coalition %in% c('(8-1)') ~ '(8-1)',
      coalition %in% c('(7-2)', '(7-1)') ~  '(7-2) or (7-1)',
      coalition %in% c('(6-3)', '(6-2)') ~ '(6-3) or (6-2)',
      coalition %in% c('(5-4)', '(5-3)') ~ '(5-4) or (5-3)',
      coalition == '(4-4)' ~ '(4-4)')) %>%
    mutate(case = ifelse(grepl('U\\.\\S\\.', case), case, gsub(' v\\..*', '', case)))

  share_of_unanimity <- scdb_cases_2023 %>%
    filter(term >= 2018) %>%
    mutate(coalition = case_when(
      majVotes == 9 ~ '(9-0) or (8-0)',
      majVotes == 8 ~ '(8-1)',
      majVotes == 7 ~ '(7-2) or (7-1)',
      majVotes == 6 ~ '(6-3) or (6-2)',
      majVotes == 5 ~ '(5-4) or (5-3)',
      majVotes == 4 ~ '(4-4)')) %>%
    select(term, coalition, docket) %>%
    bind_rows(decisions_by_coalition_longitudinal %>%
                mutate(term = 2023)) %>%
    mutate(coalition = ifelse(coalition == '(9-0) or (8-0)', 'Unanimous', "Other"),
           coalition = factor(coalition, levels = c('Unanimous', 'Other'))) %>%
    group_by(term, coalition) %>%
    summarise(count = n()) %>%
    group_by(term) %>%
    reframe(total_cases = sum(count),
            count = count,
            coalition = coalition) %>%
    mutate(percent = round((count/total_cases)*100, 2),
           term = paste0(term, ' Term (', total_cases, ')')) %>%
    ggplot(aes(x = "", y = percent, fill = coalition)) +
    geom_bar(stat = 'identity', colour = 'gray5') +
    coord_polar(theta = "y", start = 0) +
    facet_wrap(~term) +
    theme_void() +
    labs(
      x = ' ',
      y = ' ',
      fill = ' ') +
    scale_fill_manual(values = c('deepskyblue3', 'coral')) +
    geom_label(aes(label = paste0(percent, ' %')), position = position_stack(vjust = 0.5), color = "gray5", size=5, show.legend = F) +
    theme(legend.position = 'bottom',
          legend.text = element_text(size = 15, colour = 'gray5'),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = 12),
          axis.line = element_line(colour = 'black'),
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'))


  ggsave(share_of_unanimity, file = "stat_pack_OT23/Figures/statpack_figures/share_of_unanimity_2018_2023.png", width = 10, height = 10, units = 'in')


  share_of_unanimity <- scdb_cases_2023 %>%
    filter(term >= 2018) %>%
    mutate(coalition = case_when(
      majVotes == 9 ~ '(9-0) or (8-0)',
      majVotes == 8 ~ '(8-1)',
      majVotes == 7 ~ '(7-2) or (7-1)',
      majVotes == 6 ~ '(6-3) or (6-2)',
      majVotes == 5 ~ '(5-4) or (5-3)',
      majVotes == 4 ~ '(4-4)')) %>%
    select(term, coalition, docket) %>%
    bind_rows(decisions_by_coalition_longitudinal %>%
                mutate(term = 2023)) %>%
    mutate(coalition = ifelse(coalition == '(9-0) or (8-0)', 'Unanimous', "Other"),
           coalition = factor(coalition, levels = c('Unanimous', 'Other'))) %>%
    group_by(term, coalition) %>%
    summarise(count = n()) %>%
    group_by(term) %>%
    reframe(total_cases = sum(count),
            count = count,
            coalition = coalition) %>%
    mutate(percent = round((count/total_cases)*100, 2),
           term = paste0(term, ' Term (', total_cases, ')')) %>%
    select(-c(total_cases)) %>%
    rename(`Term (Total Decisions)` = term,
           `Count` = count,
           `Coalition` = coalition,
           `Percent` = percent)


  write.table(share_of_unanimity, file = 'stat_pack_OT23/Statpack replication Data/Decisions/Share of Unanimity/share_of_unanimity_OT18_OT23.csv', sep = ",", quote = F, row.names = F)

} #Share of Unanimity Over Time

{


  opinion_type_share_18_23 <- scdb_justices_2023 %>%
    filter(term >= 2018) %>%
    select(vote, docket, term) %>%
    mutate(justice_vote = case_when(
      .default = 'Joined Majority',
      vote %in% c(2, 6, 7) ~ 'Dissent',
      vote %in% c(3:5) ~ 'Concurrence',
      vote == 8 ~ 'Equally Divided')) %>%
    filter(!justice_vote == 'Equally Divided') %>%
    mutate(justice_vote = ifelse(justice_vote == 'Joined Majority', 'Majority', 'Other')) %>%
    group_by(docket, justice_vote) %>%
    reframe(count = n(),
              term) %>%
    unique() %>%
    group_by(docket) %>%
    reframe(total_voting = sum(count),
              justice_vote,
              term,
              docket,
            count)  %>%
    group_by(term) %>%
    pivot_wider(values_from = count, names_from = justice_vote) %>%
    mutate(other_votes = ifelse(Other == 0 | is.na(Other), 0, 1)) %>%
    group_by(term) %>%
    reframe(total_cases = length(unique(docket)),
            term,
            concurrence_dissent = sum(other_votes)) %>%
    unique() %>%
    mutate(majority_only = total_cases - concurrence_dissent) %>%
    pivot_longer(cols = c(concurrence_dissent, majority_only), names_to = 'votes') %>%
    mutate(percent = round((value/total_cases)*100, 2)) %>%
    bind_rows( decisions_ot_23 %>%
                 rowwise() %>%
                 mutate(other_votes = ifelse(any(!c_across(ROBERTS:JACKSON) %in% c(1, 100)), "concurrence_dissent", "majority_only")) %>%
                 select(other_votes) %>%
                 group_by(other_votes) %>%
                 reframe(value = n(),
                         votes = other_votes,
                         total_cases = nrow(decisions_ot_23)) %>%
                 unique() %>%
                 mutate(term = 2023,
                        percent = round((value/total_cases)*100, 2)) %>%
                 select(term, total_cases, votes, value, percent)) %>%
    mutate(votes = ifelse(votes == 'majority_only', 'Majority Only', 'Concurrences and (or) Dissents'),
           votes = factor(votes, levels = c('Majority Only', 'Concurrences and (or) Dissents')),
           term = paste0(term, ' Term (', total_cases, ')')) %>%
    ggplot(aes(x = "", y = percent, fill = votes)) +
    geom_bar(stat = 'identity', colour = 'gray5') +
    coord_polar(theta = "y", start = 0) +
    facet_wrap(~term) +
    theme_void() +
    labs(
      x = ' ',
      y = ' ',
      fill = ' ') +
    scale_fill_manual(values = c('deepskyblue3', 'coral')) +
    geom_label(aes(label = paste0(percent, ' %')), position = position_stack(vjust = 0.5), color = "gray5", size=5, show.legend = F) +
    theme(legend.position = 'bottom',
          legend.text = element_text(size = 15, colour = 'gray5'),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = 12),
          axis.line = element_line(colour = 'black'),
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'))


  ggsave("stat_pack_OT23/Figures/statpack_figures/opinion_type_share_18_23.png", opinion_type_share_18_23, dpi = 300)


  opinion_type_share_18_23 <- scdb_justices_2023 %>%
    filter(term >= 2018) %>%
    select(vote, docket, term) %>%
    mutate(justice_vote = case_when(
      .default = 'Joined Majority',
      vote %in% c(2, 6, 7) ~ 'Dissent',
      vote %in% c(3:5) ~ 'Concurrence',
      vote == 8 ~ 'Equally Divided')) %>%
    filter(!justice_vote == 'Equally Divided') %>%
    mutate(justice_vote = ifelse(justice_vote == 'Joined Majority', 'Majority', 'Other')) %>%
    group_by(docket, justice_vote) %>%
    reframe(count = n(),
            term) %>%
    unique() %>%
    group_by(docket) %>%
    reframe(total_voting = sum(count),
            justice_vote,
            term,
            docket,
            count)  %>%
    group_by(term) %>%
    pivot_wider(values_from = count, names_from = justice_vote) %>%
    mutate(other_votes = ifelse(Other == 0 | is.na(Other), 0, 1)) %>%
    group_by(term) %>%
    reframe(total_cases = length(unique(docket)),
            term,
            concurrence_dissent = sum(other_votes)) %>%
    unique() %>%
    mutate(majority_only = total_cases - concurrence_dissent) %>%
    pivot_longer(cols = c(concurrence_dissent, majority_only), names_to = 'votes') %>%
    mutate(percent = round((value/total_cases)*100, 2)) %>%
    bind_rows( decisions_ot_23 %>%
                 rowwise() %>%
                 mutate(other_votes = ifelse(any(!c_across(ROBERTS:JACKSON) %in% c(1, 100)), "concurrence_dissent", "majority_only")) %>%
                 select(other_votes) %>%
                 group_by(other_votes) %>%
                 reframe(value = n(),
                         votes = other_votes,
                         total_cases = nrow(decisions_ot_23)) %>%
                 unique() %>%
                 mutate(term = 2023,
                        percent = round((value/total_cases)*100, 2)) %>%
                 select(term, total_cases, votes, value, percent)) %>%
    mutate(votes = ifelse(votes == 'majority_only', 'Majority Only', 'Concurrences and (or) Dissents'),
           votes = factor(votes, levels = c('Majority Only', 'Concurrences and (or) Dissents')),
           term = paste0(term, ' Term (', total_cases, ')'))


  write.table(opinion_type_share_18_23, file = 'stat_pack_OT23/Statpack Replication Data/Decisions/Share of Unanimity/opinion_type_share_OT18_OT23.csv', quote = F, row.names = F, sep = ',')

} #Cases with Dissents/Concurrence Over Time

{

  decisions_23 <- decision_processor2(dir_path = "stat_pack_OT23/ot23_decisions/decision_pdfs_OT23") #OT23

  ten_longest <- decisions_23 %>%
    rename(docket = docket_id) %>%
    left_join(shorthand_case_names, by = 'docket') %>%
    select(short_hand, docket, sitting, opinion_writer, opinion_type, word_count) %>%
    arrange(desc(word_count)) %>%
    mutate(opinion_writer = gsub('\\;.*', '', opinion_writer),
           opinion_writer = gsub('(CHIEF JUSTICE |JUSTICE )', '', opinion_writer),
           opinion_writer = str_to_title(opinion_writer),
           opinion_type = ifelse(opinion_type == 'Majority Opinion', 'Majority', opinion_type),
           short_hand = gsub('\\,', ' ', short_hand)) %>%
    head(10)

  ten_shortest <- decisions_23 %>%
    rename(docket = docket_id) %>%
    left_join(shorthand_case_names, by = 'docket') %>%
    select(short_hand, docket, sitting, opinion_writer, opinion_type, word_count) %>%
    arrange(word_count) %>%
    mutate(opinion_writer = gsub('\\;.*', '', opinion_writer),
           opinion_writer = gsub('(CHIEF JUSTICE |JUSTICE )', '', opinion_writer),
           opinion_writer = str_to_title(opinion_writer),
           opinion_type = ifelse(opinion_type == 'Majority Opinion', 'Majority', opinion_type),
           short_hand = gsub('\\,', ' ', short_hand)) %>%
    head(10)

  all_decisions <- decisions_23 %>%
    rename(docket = docket_id) %>%
    left_join(shorthand_case_names, by = 'docket') %>%
    select(short_hand, docket, sitting, opinion_writer, opinion_type, word_count) %>%
    mutate(short_hand = gsub('\\,', ' ', short_hand)) %>%
    arrange(word_count) %>%
    rename(`Docket` = docket,
           `Case` = short_hand,
           `Sitting` = sitting,
           `Author` = opinion_writer,
           `Type` = opinion_type,
           `Word Count` = word_count)

  write.table(ten_longest, file = 'stat_pack_OT23/Statpack Replication Data/Decisions/Opinion Lengths/ten_longest_decisions.csv', row.names = F, sep = ',', quote = F)
  write.table(ten_shortest, file = 'stat_pack_OT23/Statpack Replication Data/Decisions/Opinion Lengths/ten_shortest_decisions.csv', row.names = F, sep = ',', quote = F)
  write.table(all_decisions, file = 'stat_pack_OT23/Statpack Replication Data/Decisions/Opinion Lengths/all_decision_lengths.csv', row.names = F, sep = ',', quote = T)


} # Decision Word Counts/Opinion Lengths (OT23)

{

  earlier_decisions <- get(load('ot23_decisions/earlier_decisions_processed.rdata')) # Load Older Decisions

  combined_decisions <- data.frame() #Initialize Empty Df for Combined

  for (i in names(earlier_decisions)){
    temp_term <- earlier_decisions[[i]]
    temp_term <- cbind(term = i,
                       temp_term)
    combined_decisions <- bind_rows(combined_decisions, temp_term)

    message('Completed ', i, ' Term')

  } #Append Earlier Decisions to DF

  combined_decisions <- combined_decisions %>%
    bind_rows(decisions_23 %>%
                mutate(term = "2023")) #Combine to Single Object for OT18-23 Decisions


  opinion_lengths_longitudinal <- combined_decisions %>%
    select(term, opinion_type, word_count) %>%
    group_by(term, opinion_type) %>%
    summarise(average_length = round(mean(word_count), 0)) %>%
    group_by(opinion_type) %>%
    reframe(mean_mean = mean(average_length),
            term = term,
            average_length = average_length) %>%
    mutate(opinion_type = factor(opinion_type, levels = c('Majority Opinion', 'Concurrence', 'Dissent', 'Per Curiam')))


  write.csv(opinion_lengths_longitudinal, file = 'stat_pack_OT23/Statpack Replication Data/Decisions/Opinion Lengths/average_opinion_lengths_by_type_OT18_OT23.csv', row.names = F)


  opinion_lengths_longitudinal <- combined_decisions %>%
    select(term, opinion_type, word_count) %>%
    group_by(term, opinion_type) %>%
    summarise(average_length = round(mean(word_count), 0)) %>%
    group_by(opinion_type) %>%
    reframe(mean_mean = mean(average_length),
            term = term,
            average_length = average_length) %>%
    mutate(opinion_type = factor(opinion_type, levels = c('Majority Opinion', 'Concurrence', 'Dissent', 'Per Curiam'))) %>%
    mutate(term = case_when(
      term == 2016 ~ 16,
      term == 2017 ~ 17,
      term == 2018 ~ 18,
      term == 2019 ~ 19,
      term == 2020 ~ 20,
      term == 2021 ~ 21,
      term == 2022 ~ 22,
      term == 2023 ~ 23)) %>%
    mutate(term_label = paste0(term, "'")) %>%
    ggplot(aes(x = term, y = average_length)) +
    geom_bar(stat = 'identity', fill = 'gray50', position = position_dodge2(), colour = 'gray5') +
    scale_y_continuous(breaks = seq(1000, 6000, 1000), lim = c(0, 6500)) +
    geom_hline(aes(yintercept = mean_mean), linetype = 2, colour = 'coral4', linewidth = 1.1) +
    geom_label(aes(label = average_length), vjust = -0.5) +
    scale_x_continuous(breaks = seq(16, 23, 1), labels = function(x) paste0(x, "'")) +
    geom_hline(yintercept = 0) +
    facet_wrap(~opinion_type) +
    theme_bw() +
    labs(
      x = '\nTerm\n',
      y = '\nAverage Word Count\n') +
    theme(legend.position = 'none',
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'),
          axis.text = element_text(size = 14, colour = 'black'),
          axis.title = element_text(size = 12, colour = 'black'))


  ggsave(opinion_lengths_longitudinal, file = 'stat_pack_OT23/Figures/statpack_figures/opinion_lengths_longitudinal.png', width = 10, height = 6.5)



} #Average Word Counts by Type and Term

{

  turnover_ot23 <- decisions_ot_23 %>%
    select(Date_Argued, Date_Decided, Docket) %>%
    rename(date_argued = Date_Argued,
           date_decided = Date_Decided,
           docket = Docket) %>%
    mutate(turnover = anydate(date_decided) - anydate(date_argued),
           turnover = as.numeric(turnover)) %>%
    left_join(shorthand_case_names, by = 'docket') %>%
    rename(case = short_hand) %>%
    select(case, docket, sitting, turnover) %>%
    group_by(sitting) %>%
    reframe(average_turnover = round(mean(turnover), 0)) %>%
    mutate(sitting = case_when(
      sitting == 'October' ~ 'Oct',
      sitting == 'November' ~ 'Nov',
      sitting == 'December' ~ 'Dec',
      sitting == 'January' ~ 'Jan',
      sitting == 'February' ~ 'Feb',
      sitting == 'March' ~ 'Mar',
      sitting == 'April' ~ 'Apr')) %>%
    mutate(sitting = factor(sitting, levels = c('Oct', 'Nov', 'Dec', 'Jan', "Feb", 'Mar', 'Apr'))) %>%
    ggplot(aes(x = factor(sitting), y = average_turnover)) +
    geom_bar(stat = 'identity', colour = 'gray5', fill = 'gray50') +
    geom_hline(yintercept = 0) +
    geom_label(aes(label = paste0(average_turnover)), vjust = -0.5, size = 7) +
    scale_y_continuous(breaks = seq(25, 200, 25), lim = c(0, 200)) +
    theme_bw() +
    labs(
      x = '\nSitting\n',
      y = '\nAverage Turnover (Days)\n') +
    theme(legend.position = 'none',
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'),
          axis.text = element_text(size = 14, colour = 'black'),
          axis.title = element_text(size = 14, colour = 'black'))


  ggsave(turnover_ot23, file = 'stat_pack_OT23/Figures/statpack_figures/decision_turnover_OT23.png', height = 8, width = 10, units = 'in')

  turnover_ot23 <- decisions_ot_23 %>%
    select(Date_Argued, Date_Decided, Docket) %>%
    rename(date_argued = Date_Argued,
           date_decided = Date_Decided,
           docket = Docket) %>%
    mutate(turnover = anydate(date_decided) - anydate(date_argued),
           turnover = as.numeric(turnover)) %>%
    left_join(shorthand_case_names, by = 'docket') %>%
    rename(case = short_hand) %>%
    select(case, docket, sitting, turnover) %>%
    group_by(sitting) %>%
    reframe(average_turnover = round(mean(turnover), 0),
            min = min(turnover),
            max = max(turnover)) %>%
    mutate(sitting = factor(sitting, levels = c('October', 'November', "December", 'January', 'February', 'March', 'April')))

  write.csv(turnover_ot23, file = 'stat_pack_OT23/Statpack Replication Data/Decisions/Decision Turnover/decision_turnover_OT23.csv', row.names = F)

} #Decision Turnover (OT23)

{

  average_turnover_ot18_ot23 <- scdb_cases_2023 %>%
    filter(term >= 2005) %>%
    filter(!is.na(dateArgument)) %>%
    select(dateDecision, dateArgument, dateRearg, term) %>%
    mutate(dateArgument = ifelse(is.na(dateRearg), dateArgument, dateRearg)) %>%
    select(dateArgument, dateDecision, term) %>%
    rename(date_decided = dateDecision,
           date_argued = dateArgument) %>%
    mutate(date_decided = anydate(date_decided),
           date_argued = anydate(date_argued),
           turnover = date_decided - date_argued) %>%
    filter(!is.na(turnover)) %>%
    group_by(term) %>%
    summarise(average_turnover = round(mean(turnover), 0),
              average_turnover = as.numeric(average_turnover),
              min = as.numeric(min(turnover)),
              max = as.numeric(max(turnover))) %>%
    bind_rows(turnover_ot23 %>%
                summarise(average_turnover = round(mean(average_turnover), 0),
                          min = min(min),
                          max = max(max)) %>%
                mutate(term = 2023)) %>%
    ggplot(aes(x = factor(term), y = average_turnover)) +
    geom_bar(stat = 'identity', aes(colour = 'Mean'),  fill = 'gray50') +
    geom_errorbar(aes(ymin = average_turnover, ymax = max, color = 'Error'), show.legend = TRUE) +
    geom_label(aes(label = average_turnover), vjust = 1.5, size = 7) +
    geom_text(aes(label = max, y = max -2 ), vjust = -1, size = 7) +
    scale_color_manual(values = c('black', 'gray5'), labels = c('Maximum Turnover (Days)  ', 'Average Turnover (Days)')) +
    geom_hline(yintercept = 0) +
    scale_x_discrete(breaks = seq(2006, 2022, 2)) +
    scale_y_continuous(breaks = seq(50, 300, 50)) +
    theme_bw() +
    labs(
      x = '\nSitting\n',
      y = '\nTurnover (Days)\n',
      colour = ' ') +
    theme(legend.position = 'bottom',
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'),
          axis.text = element_text(size = 18, colour = 'black'),
          axis.title = element_text(size = 18, colour = 'black'),
          legend.text = element_text(size = 18))


  ggsave(average_turnover_ot18_ot23, file = 'stat_pack_OT23/Figures/statpack_figures/decision_turnover_OT18_OT23.png', height = 12, width = 14, unit = 'in')

  average_turnover_ot18_ot23 <- scdb_cases_2023 %>%
    filter(term >= 2005) %>%
    filter(!is.na(dateArgument)) %>%
    select(dateDecision, dateArgument, dateRearg, term) %>%
    mutate(dateArgument = ifelse(is.na(dateRearg), dateArgument, dateRearg)) %>%
    select(dateArgument, dateDecision, term) %>%
    rename(date_decided = dateDecision,
           date_argued = dateArgument) %>%
    mutate(date_decided = anydate(date_decided),
           date_argued = anydate(date_argued),
           turnover = date_decided - date_argued) %>%
    filter(!is.na(turnover)) %>%
    group_by(term) %>%
    summarise(average_turnover = round(mean(turnover), 0),
              average_turnover = as.numeric(average_turnover),
              min = as.numeric(min(turnover)),
              max = as.numeric(max(turnover))) %>%
    bind_rows(turnover_ot23 %>%
                summarise(average_turnover = round(mean(average_turnover), 0),
                          min = min(min),
                          max = max(max)) %>%
                mutate(term = 2023))

  write.csv(average_turnover_ot18_ot23, file = 'stat_pack_OT23/Statpack Replication Data/Decisions/Decision Turnover/decision_average_turnover_ot18_ot23.csv', row.names = F)


} #Decision Turnover (OT18-OT23)

{

  percent_majority_justice <- scdb_justices_2023 %>%
    filter(term >= 2006) %>%
    left_join(scdb_justice_names, by = 'justice') %>%
    select(majority, justice_name) %>%
    group_by(justice_name, majority) %>%
    filter(!is.na(majority)) %>%
    bind_rows(decisions_ot_23 %>%
                select(ROBERTS:JACKSON) %>%
                pivot_longer(cols = c(ROBERTS:JACKSON)) %>%
                mutate(majority = ifelse(value >= 1, 2, 1),
                       name = toupper(name)) %>%
                select(name, majority) %>%
                rename(justice_name = name)) %>%
    mutate(majority = ifelse(majority == 1, 'Minority', 'Majority')) %>%
    summarise(count = n()) %>%
    pivot_wider(names_from = majority, values_from = count) %>%
    mutate(percent_majority = ((Majority/(Majority + Minority))*100),
           percent_minority = 100-percent_majority) %>%
    pivot_longer(cols = c('percent_majority', 'percent_minority')) %>%
    mutate(value = round(value, 2),
           name = ifelse(name == 'percent_majority', 'Majority', 'Minority')) %>%
    select(justice_name, name, value) %>%
    mutate(justice_name = str_to_title(justice_name),
           justice_name = factor(justice_name, levels = c('Roberts', 'Stevens', 'Kennedy', 'Scalia', 'Souter', 'Thomas', 'Ginsburg', 'Breyer', 'Alito', 'Sotomayor', 'Kagan', 'Gorsuch', 'Kavanaugh', 'Barrett', 'Jackson'))) %>%
    ggplot(aes(x = "", y = value, fill = name)) +
      geom_bar(stat = "identity", colour = 'gray5') +
      coord_polar(theta = 'y', start = 0) +
      facet_wrap(~justice_name, nrow = 3) +
    theme_void() +
    labs(
      x = ' ',
      y = ' ',
      fill = ' ') +
    scale_fill_manual(values = c('deepskyblue3', 'coral')) +
    geom_label(aes(label = paste0(value, ' %')), position = position_stack(vjust = 0.5), color = "gray5", size=5, show.legend = F) +
    theme(legend.position = 'bottom',
          legend.text = element_text(size = 15, colour = 'gray5'),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = 12),
          axis.line = element_line(colour = 'black'),
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'))


  ggsave(percent_majority_justice, file = 'stat_pack_OT23/Figures/statpack_figures/percent_majority_justice.png')


  percent_majority_justice <- scdb_justices_2023 %>%
    filter(term >= 2006) %>%
    left_join(scdb_justice_names, by = 'justice') %>%
    select(majority, justice_name) %>%
    group_by(justice_name, majority) %>%
    filter(!is.na(majority)) %>%
    bind_rows(decisions_ot_23 %>%
                select(ROBERTS:JACKSON) %>%
                pivot_longer(cols = c(ROBERTS:JACKSON)) %>%
                mutate(majority = ifelse(value >= 1, 2, 1),
                       name = toupper(name)) %>%
                select(name, majority) %>%
                rename(justice_name = name)) %>%
    mutate(majority = ifelse(majority == 1, 'Minority', 'Majority')) %>%
    summarise(count = n()) %>%
    pivot_wider(names_from = majority, values_from = count) %>%
    mutate(percent_majority = ((Majority/(Majority + Minority))*100),
           percent_minority = 100-percent_majority) %>%
    pivot_longer(cols = c('percent_majority', 'percent_minority')) %>%
    mutate(value = round(value, 2),
           name = ifelse(name == 'percent_majority', 'Majority', 'Minority')) %>%
    select(justice_name, name, value) %>%
    mutate(justice_name = str_to_title(justice_name),
           justice_name = factor(justice_name, levels = c('Roberts', 'Stevens', 'Kennedy', 'Scalia', 'Souter', 'Thomas', 'Ginsburg', 'Breyer', 'Alito', 'Sotomayor', 'Kagan', 'Gorsuch', 'Kavanaugh', 'Barrett', 'Jackson'))) %>%
    rename(Justice = justice_name,
           Coalition = name,
           Percent = value)

  write.csv(percent_majority_justice, file = 'stat_pack_OT23/Statpack Replication Data/Decisions/Percent in Majority/percent_majority_justice_OT02_OT23.csv', row.names = F)


} #Frequency in Majority Over Time

{

  percent_majority_OT23 <- decisions_ot_23 %>%
    select(Docket, ROBERTS:JACKSON) %>%
    mutate(across(ROBERTS:JACKSON, ~ ifelse(. >= 1, 1, 0))) %>%
    pivot_longer(cols = ROBERTS:JACKSON, names_to = "justice_name", values_to = "majority") %>%
    mutate(majority = ifelse(majority == 1, 'Majority', 'Minority')) %>%
    filter(!is.na(majority)) %>%
    mutate(total_cases = length(unique(Docket))) %>%
    group_by(justice_name, majority) %>%
    reframe(count = n(),
            total_cases = total_cases) %>%
    mutate(justice_name = str_to_title(justice_name),
           justice_name = factor(justice_name, levels = c('Roberts', 'Thomas', 'Alito', 'Sotomayor', 'Kagan', 'Gorsuch', 'Kavanaugh', 'Barrett', 'Jackson')),
           percent = round((count/total_cases)*100, 2)) %>%
    unique() %>%
    ggplot(aes(x = "", y = count, fill = majority)) +
    geom_bar(stat = "identity", colour = 'gray5') +
    coord_polar(theta = 'y', start = 0) +
    facet_wrap(~justice_name, nrow = 3) +
    theme_void() +
    labs(
      x = ' ',
      y = ' ',
      fill = ' ') +
    scale_fill_manual(values = c('deepskyblue3', 'coral')) +
    geom_label(aes(label = paste0(percent, ' %')), position = position_stack(vjust = 0.5), color = "gray5", size=5, show.legend = F) +
    theme(legend.position = 'bottom',
          legend.text = element_text(size = 15, colour = 'gray5'),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = 12),
          axis.line = element_line(colour = 'black'),
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'))

  ggsave(percent_majority_OT23, file = 'stat_pack_OT23/Figures/statpack_figures/percent_majority_OT23.png', height = 8, width = 8, units = 'in')

  percent_majority_OT23 <- decisions_ot_23 %>%
    select(Docket, ROBERTS:JACKSON) %>%
    mutate(across(ROBERTS:JACKSON, ~ ifelse(. >= 1, 1, 0))) %>%
    pivot_longer(cols = ROBERTS:JACKSON, names_to = "justice_name", values_to = "majority") %>%
    mutate(majority = ifelse(majority == 1, 'Majority', 'Minority')) %>%
    filter(!is.na(majority)) %>%
    mutate(total_cases = length(unique(Docket))) %>%
    group_by(justice_name, majority) %>%
    reframe(count = n(),
            total_cases = total_cases) %>%
    mutate(justice_name = str_to_title(justice_name),
           justice_name = factor(justice_name, levels = c('Roberts', 'Thomas', 'Alito', 'Sotomayor', 'Kagan', 'Gorsuch', 'Kavanaugh', 'Barrett', 'Jackson')),
           percent = round((count/total_cases)*100, 2)) %>%
    unique() %>%
    select(-c(total_cases)) %>%
    rename(`Justice` = justice_name,
           `Vote` = majority,
           `Raw Count` = count,
           `Percent` = percent)

  write.csv(percent_majority_OT23, file = 'stat_pack_OT23/Statpack Replication Data/Decisions/Percent in Majority/percent_majority_OT23.csv', row.names = F)


} # Frequency in Majority (OT23)

{

  percent_majority_split_cases <- decisions_ot_23 %>%
    filter(Coalition %in% c('(6-3)', '(5-4)')) %>%
    select(Docket, ROBERTS:JACKSON) %>%
    mutate(across(ROBERTS:JACKSON, ~ ifelse(. >= 1, 1, 0))) %>%
    pivot_longer(cols = ROBERTS:JACKSON, names_to = "justice_name", values_to = "majority") %>%
    mutate(majority = ifelse(majority == 1, 'Majority', 'Minority')) %>%
    filter(!is.na(majority)) %>%
    mutate(total_cases = length(unique(Docket))) %>%
    group_by(justice_name, majority) %>%
    reframe(count = n(),
            total_cases = total_cases) %>%
    mutate(justice_name = str_to_title(justice_name),
           justice_name = factor(justice_name, levels = c('Roberts', 'Thomas', 'Alito', 'Sotomayor', 'Kagan', 'Gorsuch', 'Kavanaugh', 'Barrett', 'Jackson')),
           percent = round((count/total_cases)*100, 2)) %>%
    unique() %>%
    ggplot(aes(x = "", y = count, fill = majority)) +
    geom_bar(stat = "identity", colour = 'gray5') +
    coord_polar(theta = 'y', start = 0) +
    facet_wrap(~justice_name, nrow = 3) +
    theme_void() +
    labs(
      x = ' ',
      y = ' ',
      fill = ' ') +
    scale_fill_manual(values = c('deepskyblue3', 'coral')) +
    geom_label(aes(label = paste0(percent, ' %')), position = position_stack(vjust = 0.5), color = "gray5", size=5, show.legend = F) +
    theme(legend.position = 'bottom',
          legend.text = element_text(size = 15, colour = 'gray5'),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = 12),
          axis.line = element_line(colour = 'black'),
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'))


  ggsave(percent_majority_split_cases, file = 'stat_pack_OT23/Figures/statpack_figures/percent_majority_split_cases.png', height = 8, width = 8, units = 'in')


  percent_majority_split_cases <- decisions_ot_23 %>%
    filter(Coalition %in% c('(6-3)', '(5-4)')) %>%
    select(Docket, ROBERTS:JACKSON) %>%
    mutate(across(ROBERTS:JACKSON, ~ ifelse(. >= 1, 1, 0))) %>%
    pivot_longer(cols = ROBERTS:JACKSON, names_to = "justice_name", values_to = "majority") %>%
    mutate(majority = ifelse(majority == 1, 'Majority', 'Minority')) %>%
    filter(!is.na(majority)) %>%
    mutate(total_cases = length(unique(Docket))) %>%
    group_by(justice_name, majority) %>%
    reframe(count = n(),
            total_cases = total_cases) %>%
    mutate(justice_name = str_to_title(justice_name),
           justice_name = factor(justice_name, levels = c('Roberts', 'Thomas', 'Alito', 'Sotomayor', 'Kagan', 'Gorsuch', 'Kavanaugh', 'Barrett', 'Jackson')),
           percent = round((count/total_cases)*100, 2)) %>%
    unique() %>%
    select(-c(total_cases)) %>%
    rename(`Justice` = justice_name,
           `Vote` = majority,
           `Raw Count` = count,
           `Percent` = percent)

  write.csv(percent_majority_split_cases, file = 'stat_pack_OT23/Statpack Replication Data/Decisions/Percent in Majority/percent_majority_split_cases.csv', row.names = F)


} # Frequency in Majority (OT23 -- 6-3 or 5-4 ONLY)

{

  circuit_scorecard_argued_decided_only <- decisions_ot_23 %>%
    filter(Lower_Court %in% c('CA1', 'CA2', 'CA3', 'CA4', 'CA5', 'CA6', 'CA7', 'CA8', 'CA9', 'CA10', 'CA11', 'CADC')) %>%
    select(Lower_Court, Decision) %>%
    mutate(decision = case_when(
      grepl('Affirm', Decision, ignore.case = TRUE) ~ 'Affirmed',
      grepl('Reverse|Remand', Decision, ignore.case = TRUE) ~ 'Reverse, Vacate,\n(and/or) Remand',
      TRUE ~ 'Other')) %>%
    #add_row(Lower_Court = 'CA1', Decision = 'Vacate & Remand', decision = 'Reverse, Vacate,\n(and/or) Remand') %>% # Relentless
    #add_row(Lower_Court = 'CA5', Decision = 'Vacate & Remand', decision = 'Reverse, Vacate,\n(and/or) Remand') %>% # Add Paxton v. Net Choice
    group_by(Lower_Court, decision) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    group_by(Lower_Court) %>%
    mutate(total_cases = sum(count),
           Lower_Court = case_when(
             Lower_Court == 'CA1' ~ '1st Circuit',
             Lower_Court == 'CA2' ~ '2nd Circuit',
             Lower_Court == 'CA3' ~ '3rd Circuit',
             Lower_Court == 'CA4' ~ '4th Circuit',
             Lower_Court == 'CA5' ~ '5th Circuit',
             Lower_Court == 'CA6' ~ '6th Circuit',
             Lower_Court == 'CA7' ~ '7th Circuit',
             Lower_Court == 'CA8' ~ '8th Circuit',
             Lower_Court == 'CA9' ~ '9th Circuit',
             Lower_Court == 'CA10' ~ '10th Circuit',
             Lower_Court == 'CA11' ~ '11th Circuit',
             Lower_Court == 'CADC' ~ 'DC Circuit'),
           circuit_label = paste0(Lower_Court, ' (', total_cases, ')'),
           percent = round((count / total_cases) * 100, 2)) %>%
    select(circuit_label, Lower_Court, decision, percent, total_cases) %>%
    mutate(Lower_Court = factor(Lower_Court, levels = c('1st Circuit', '2nd Circuit', '3rd Circuit', '4th Circuit', '5th Circuit', '6th Circuit', '7th Circuit', '8th Circuit', '9th Circuit', '10th Circuit', '11th Circuit', 'DC Circuit'))) %>%
    arrange(Lower_Court) %>%
    mutate(circuit_label = factor(circuit_label, levels = unique(circuit_label)),
           decision = factor(decision, levels = c('Affirmed', 'Reverse, Vacate,\n(and/or) Remand', 'Other'))) %>%
    ggplot(aes(x = "", y = percent, fill = decision)) +
    geom_bar(stat = "identity", colour = 'gray5') +
    coord_polar(theta = 'y', start = 0) +
    facet_wrap(~circuit_label, nrow = 3) +
    theme_void() +
    labs(
      x = ' ',
      y = ' ',
      fill = ' ') +
    scale_fill_manual(values = c('deepskyblue3', 'coral3', 'gray')) +
    geom_label(aes(label = paste0(percent, ' %')), position = position_stack(vjust = 0.5), color = "gray5", size=5, show.legend = F) +
    theme(legend.position = 'bottom',
          legend.text = element_text(size = 15, colour = 'gray5'),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = 12),
          axis.line = element_line(colour = 'black'),
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'))

  ggsave(circuit_scorecard_argued_decided_only, file = 'stat_pack_OT23/Figures/statpack_figures/circuit_scorecard_argued_decided_only.png', width = 10, height = 8, units = 'in')

  circuit_scorecard_argued_decided_only <- decisions_ot_23 %>%
    filter(Lower_Court %in% c('CA1', 'CA2', 'CA3', 'CA4', 'CA5', 'CA6', 'CA7', 'CA8', 'CA9', 'CA10', 'CA11', 'CADC')) %>%
    select(Lower_Court, Decision) %>%
    mutate(decision = case_when(
      grepl('Affirm', Decision, ignore.case = TRUE) ~ 'Affirmed',
      grepl('Reverse|Remand', Decision, ignore.case = TRUE) ~ 'Reverse, Vacate, (and/or) Remand',
      TRUE ~ 'Other')) %>%
    #add_row(Lower_Court = 'CA1', Decision = 'Vacate & Remand', decision = 'Reverse, Vacate,\n(and/or) Remand') %>% # Relentless
    #add_row(Lower_Court = 'CA5', Decision = 'Vacate & Remand', decision = 'Reverse, Vacate,\n(and/or) Remand') %>% # Add Paxton v. Net Choice
    group_by(Lower_Court, decision) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    group_by(Lower_Court) %>%
    mutate(total_cases = sum(count),
           Lower_Court = case_when(
             Lower_Court == 'CA1' ~ '1st Circuit',
             Lower_Court == 'CA2' ~ '2nd Circuit',
             Lower_Court == 'CA3' ~ '3rd Circuit',
             Lower_Court == 'CA4' ~ '4th Circuit',
             Lower_Court == 'CA5' ~ '5th Circuit',
             Lower_Court == 'CA6' ~ '6th Circuit',
             Lower_Court == 'CA7' ~ '7th Circuit',
             Lower_Court == 'CA8' ~ '8th Circuit',
             Lower_Court == 'CA9' ~ '9th Circuit',
             Lower_Court == 'CA10' ~ '10th Circuit',
             Lower_Court == 'CA11' ~ '11th Circuit',
             Lower_Court == 'CADC' ~ 'DC Circuit'),
           circuit_label = paste0(Lower_Court, ' (', total_cases, ')'),
           percent = round((count / total_cases) * 100, 2)) %>%
    select(circuit_label, Lower_Court, decision, percent, total_cases) %>%
    mutate(Lower_Court = factor(Lower_Court, levels = c('1st Circuit', '2nd Circuit', '3rd Circuit', '4th Circuit', '5th Circuit', '6th Circuit', '7th Circuit', '8th Circuit', '9th Circuit', '10th Circuit', '11th Circuit', 'DC Circuit'))) %>%
    arrange(Lower_Court) %>%
    mutate(circuit_label = factor(circuit_label, levels = unique(circuit_label))) %>%
    select(Lower_Court, total_cases, decision, percent) %>%
    ungroup() %>%
    rename(`Lower Court (Circuit)` = Lower_Court,
           `Total Cases` = total_cases,
           `Decision (Type)` = decision,
           `Percent Outcome` = percent)

  write.csv(circuit_scorecard_argued_decided_only, file = 'stat_pack_OT23/Statpack Replication Data/Decisions/Circuit Scorecard/circuit_scorecard_argued_decided_only.csv', row.names = F)


} # Circuit Scorecard (Orally Argued & Decided -- No Consolidations)

{

  additional_cases_consolidated <- data.frame(Lower_Court = c('CA11', 'CA9', 'CADC', 'CADC', 'CADC', 'CA10', 'CA5', 'CA9', 'CA9'),
                                              Decision = c('Affirmed', 'DIG', 'Stay Granted', 'Stay Granted', 'Stay Granted', 'Affirmed', 'Reverse', 'Reverse', 'Reverse')) # Jackson (Brown), Idaho (Moyle), Kinder Morgan (Ohio), American Forest (Ohio), US Steel Corp (Ohio), Becerra (Becerra), Danco (FDA), Garland-Singh (Campos Chavez), Garland-Mendez (Campos Chavez)

  circuit_scorecard_with_consolidations <- decisions_ot_23 %>%
    filter(Lower_Court %in% c('CA1', 'CA2', 'CA3', 'CA4', 'CA5', 'CA6', 'CA7', 'CA8', 'CA9', 'CA10', 'CA11', 'CADC')) %>%
    select(Lower_Court, Decision) %>%
    bind_rows(additional_cases_consolidated) %>%
    mutate(decision = case_when(
      grepl('Affirm', Decision, ignore.case = TRUE) ~ 'Affirmed',
      grepl('Reverse|Remand', Decision, ignore.case = TRUE) ~ 'Reverse, Vacate,\n(and/or) Remand',
      TRUE ~ 'Other')) %>%
    add_row(Lower_Court = 'CA1', Decision = 'Vacate & Remand', decision = 'Reverse, Vacate,\n(and/or) Remand') %>% # Relentless
    add_row(Lower_Court = 'CA5', Decision = 'Vacate & Remand', decision = 'Reverse, Vacate,\n(and/or) Remand') %>% # Add Paxton v. Net Choice
    group_by(Lower_Court, decision) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    group_by(Lower_Court) %>%
    mutate(total_cases = sum(count),
           Lower_Court = case_when(
             Lower_Court == 'CA1' ~ '1st Circuit',
             Lower_Court == 'CA2' ~ '2nd Circuit',
             Lower_Court == 'CA3' ~ '3rd Circuit',
             Lower_Court == 'CA4' ~ '4th Circuit',
             Lower_Court == 'CA5' ~ '5th Circuit',
             Lower_Court == 'CA6' ~ '6th Circuit',
             Lower_Court == 'CA7' ~ '7th Circuit',
             Lower_Court == 'CA8' ~ '8th Circuit',
             Lower_Court == 'CA9' ~ '9th Circuit',
             Lower_Court == 'CA10' ~ '10th Circuit',
             Lower_Court == 'CA11' ~ '11th Circuit',
             Lower_Court == 'CADC' ~ 'DC Circuit'),
           circuit_label = paste0(Lower_Court, ' (', total_cases, ')'),
           percent = round((count / total_cases) * 100, 2)) %>%
    select(circuit_label, Lower_Court, decision, percent, total_cases) %>%
    mutate(Lower_Court = factor(Lower_Court, levels = c('1st Circuit', '2nd Circuit', '3rd Circuit', '4th Circuit', '5th Circuit', '6th Circuit', '7th Circuit', '8th Circuit', '9th Circuit', '10th Circuit', '11th Circuit', 'DC Circuit'))) %>%
    arrange(Lower_Court) %>%
    mutate(circuit_label = factor(circuit_label, levels = unique(circuit_label)),
           decision = factor(decision, levels = c('Affirmed', 'Reverse, Vacate,\n(and/or) Remand', 'Other'))) %>%
    ggplot(aes(x = "", y = percent, fill = decision)) +
    geom_bar(stat = "identity", colour = 'gray5') +
    coord_polar(theta = 'y', start = 0) +
    facet_wrap(~circuit_label, nrow = 3) +
    theme_void() +
    labs(
      x = ' ',
      y = ' ',
      fill = ' ') +
    scale_fill_manual(values = c('deepskyblue3', 'coral3', 'gray')) +
    geom_label(aes(label = paste0(percent, ' %')), position = position_stack(vjust = 0.5), color = "gray5", size=5, show.legend = F) +
    theme(legend.position = 'bottom',
          legend.text = element_text(size = 15, colour = 'gray5'),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = 12),
          axis.line = element_line(colour = 'black'),
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'))

  ggsave(circuit_scorecard_with_consolidations , file = 'stat_pack_OT23/Figures/statpack_figures/circuit_scorecard_with_consolidations .png', width = 10, height = 8, units = 'in')


  circuit_scorecard_with_consolidations <- decisions_ot_23 %>%
    filter(Lower_Court %in% c('CA1', 'CA2', 'CA3', 'CA4', 'CA5', 'CA6', 'CA7', 'CA8', 'CA9', 'CA10', 'CA11', 'CADC')) %>%
    select(Lower_Court, Decision) %>%
    bind_rows(additional_cases_consolidated) %>%
    mutate(decision = case_when(
      grepl('Affirm', Decision, ignore.case = TRUE) ~ 'Affirmed',
      grepl('Reverse|Remand', Decision, ignore.case = TRUE) ~ 'Reverse, Vacate,\n(and/or) Remand',
      TRUE ~ 'Other')) %>%
    add_row(Lower_Court = 'CA1', Decision = 'Vacate & Remand', decision = 'Reverse, Vacate,\n(and/or) Remand') %>% # Relentless
    add_row(Lower_Court = 'CA5', Decision = 'Vacate & Remand', decision = 'Reverse, Vacate,\n(and/or) Remand') %>% # Add Paxton v. Net Choice
    group_by(Lower_Court, decision) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    group_by(Lower_Court) %>%
    mutate(total_cases = sum(count),
           Lower_Court = case_when(
             Lower_Court == 'CA1' ~ '1st Circuit',
             Lower_Court == 'CA2' ~ '2nd Circuit',
             Lower_Court == 'CA3' ~ '3rd Circuit',
             Lower_Court == 'CA4' ~ '4th Circuit',
             Lower_Court == 'CA5' ~ '5th Circuit',
             Lower_Court == 'CA6' ~ '6th Circuit',
             Lower_Court == 'CA7' ~ '7th Circuit',
             Lower_Court == 'CA8' ~ '8th Circuit',
             Lower_Court == 'CA9' ~ '9th Circuit',
             Lower_Court == 'CA10' ~ '10th Circuit',
             Lower_Court == 'CA11' ~ '11th Circuit',
             Lower_Court == 'CADC' ~ 'DC Circuit'),
           circuit_label = paste0(Lower_Court, ' (', total_cases, ')'),
           percent = round((count / total_cases) * 100, 2)) %>%
    select(circuit_label, Lower_Court, decision, percent, total_cases) %>%
    mutate(Lower_Court = factor(Lower_Court, levels = c('1st Circuit', '2nd Circuit', '3rd Circuit', '4th Circuit', '5th Circuit', '6th Circuit', '7th Circuit', '8th Circuit', '9th Circuit', '10th Circuit', '11th Circuit', 'DC Circuit'))) %>%
    arrange(Lower_Court) %>%
    mutate(circuit_label = factor(circuit_label, levels = unique(circuit_label)),
           decision = factor(decision, levels = c('Affirmed', 'Reverse, Vacate,\n(and/or) Remand', 'Other'))) %>%
  rename(`Lower Court (Circuit)` = Lower_Court,
         `Total Cases` = total_cases,
         `Decision (Type)` = decision,
         `Percent Outcome` = percent)

  write.csv(circuit_scorecard_with_consolidations, file = 'stat_pack_OT23/Statpack Replication Data/Decisions/Circuit Scorecard/circuit_scorecard_with_consolidations.csv', row.names = F)

} # Circuit Scorecard (with Consolidations Included)

{


  ideological_splits <- split_data %>%
    group_by(term) %>%
    mutate(no_ideological_split = total_cases - ideological_split) %>%
    pivot_longer(cols = c(ideological_split, no_ideological_split)) %>%
    mutate(name = ifelse(name == 'ideological_split', 'Ideological Split    ', 'No Ideological Split')) %>%
    ggplot(aes(x = term, y = value, fill = name)) +
    geom_bar(stat = 'identity', colour = 'gray5', position = position_dodge(width = 0.9)) +
    geom_label(aes(label = value), position = position_dodge2(width = 0.9, preserve = "single"), color = "gray5", size=7, fill = 'white', show.legend = FALSE, vjust = 1.5) +
  scale_fill_manual(values = c('gray75', 'gray10')) +
    scale_y_continuous(breaks = seq(2, 14, 2), lim = c(0, 15)) +
    labs(x = '\nTerm',
         y = '\n',
         fill = '') +
    geom_hline(yintercept = 0) +
    theme(legend.position = 'bottom',
          legend.text = element_text(size = 15, colour = 'gray5'),
          axis.text = element_text(size = 18, colour = 'gray5'),
          axis.ticks = element_blank(),
          axis.title = element_text(size = 20),
          axis.line = element_line(colour = 'black'),
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'))


  ggsave(ideological_splits , file = 'stat_pack_OT23/Figures/statpack_figures/ideological_splits_OT20_OT23.png', width = 10, height = 8, units = 'in')

  ideological_splits <- split_data %>%
    group_by(term) %>%
    mutate(no_ideological_split = total_cases - ideological_split) %>%
    pivot_longer(cols = c(ideological_split, no_ideological_split)) %>%
    mutate(name = ifelse(name == 'ideological_split', 'Ideological Split', 'No Ideological Split')) %>%
    rename(`Term` = term,
           `Total (6-3) or (5-3) Cases` = total_cases,
           `Split` = name,
           `Value` = value)

  write.csv(ideological_splits, file = 'stat_pack_OT23/Statpack Replication Data/Decisions/Decisions by Coalition/ideological_splits_OT20_OT23.csv', row.names = F)


} # Ideological Splits

{

  opinions_by_justice_OT23 <- decisions_ot_23 %>%
    select(ROBERTS:JACKSON) %>%
    mutate(across(ROBERTS:JACKSON, ~ case_when(
      . == 100 ~ 'Majority',
      . %in% c(2, 4, 5, 7) ~ 'Concurrence',
      . %in% c(-1, -3) ~ 'Dissent',
      .default = NA
    ))) %>%
    pivot_longer(cols = ROBERTS:JACKSON, names_to = "opinion_writer", values_to = "opinion_type") %>%
    filter(!is.na(opinion_writer)) %>%
    filter(!is.na(opinion_type)) %>%
    group_by(opinion_writer, opinion_type) %>%
    summarise(count = n()) %>%
    mutate(opinion_writer = str_to_title(opinion_writer)) %>%
    mutate(opinion_type = factor(opinion_type, levels = c('Majority', 'Concurrence', 'Dissent')),
           opinion_writer = factor(opinion_writer, levels = c('Roberts', 'Thomas', 'Alito', 'Sotomayor', 'Kagan', 'Gorsuch', 'Kavanaugh', 'Barrett', 'Jackson'))) %>%
    ggplot(aes(x = forcats::fct_rev(opinion_writer), y = count, group = opinion_type)) +
    geom_bar(stat = 'identity', colour = 'gray5', position = position_stack(reverse = TRUE), aes(fill = opinion_type)) +
    scale_y_continuous(breaks = seq(2, 20, 2)) +
    labs(x = '\n',
         y = '',
         fill = '') +
    geom_hline(yintercept = 0) +
    scale_fill_manual(values = c('deepskyblue3', 'chartreuse4', 'coral3')) +
    geom_label(aes(label = count),
               position=position_stack(vjust=0.5, reverse = T),  size = 7) +
    coord_flip() +
    theme(legend.position = 'bottom',
          legend.text = element_text(size = 15, colour = 'gray5'),
          axis.text = element_text(size = 18, colour = 'gray5'),
          axis.ticks = element_blank(),
          axis.title = element_text(size = 20),
          axis.line = element_line(colour = 'black'),
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'))


  ggsave(opinions_by_justice_OT23 , file = 'stat_pack_OT23/Figures/statpack_figures/opinions_by_justice_OT23.png', width = 12, height = 8, units = 'in')

  opinions_by_justice_OT23 <- decisions_ot_23 %>%
    select(ROBERTS:JACKSON) %>%
    mutate(across(ROBERTS:JACKSON, ~ case_when(
      . == 100 ~ 'Majority',
      . %in% c(2, 4, 5, 7) ~ 'Concurrence',
      . %in% c(-1, -3) ~ 'Dissent',
      .default = NA
    ))) %>%
    pivot_longer(cols = ROBERTS:JACKSON, names_to = "opinion_writer", values_to = "opinion_type") %>%
    filter(!is.na(opinion_writer)) %>%
    filter(!is.na(opinion_type)) %>%
    group_by(opinion_writer, opinion_type) %>%
    summarise(count = n()) %>%
    mutate(opinion_writer = str_to_title(opinion_writer)) %>%
    mutate(opinion_type = factor(opinion_type, levels = c('Majority', 'Concurrence', 'Dissent')),
           opinion_writer = factor(opinion_writer, levels = c('Roberts', 'Thomas', 'Alito', 'Sotomayor', 'Kagan', 'Gorsuch', 'Kavanaugh', 'Barrett', 'Jackson'))) %>%
    rename(`Author` = opinion_writer,
           `Opinion Type` = opinion_type,
           `Count` = count)


  write.csv(opinions_by_justice_OT23, file = 'stat_pack_OT23/Statpack Replication Data/Decisions/Decisions by Coalition/opinions_by_justice_OT23.csv', row.names = F)


} # Total Opinions by Justice (OT23)

{

  majority_minority_votes <- decisions_ot_23 %>%
    select(Docket, ROBERTS:JACKSON) %>%
    mutate(across(ROBERTS:JACKSON, ~ ifelse(. >= 1, 1, 0))) %>%
    pivot_longer(cols = ROBERTS:JACKSON, names_to = "justice_name", values_to = "majority") %>%
    mutate(majority = ifelse(majority == 1, 'Majority Vote', 'Minority Vote')) %>%
    group_by(justice_name, majority) %>%
    summarise(count = n()) %>%
    filter(!is.na(majority)) %>%
    mutate(justice_name = str_to_title(justice_name),
      justice_name = factor(justice_name, levels = c('Roberts', 'Thomas', 'Alito', 'Sotomayor', 'Kagan', 'Gorsuch', 'Kavanaugh', 'Barrett', 'Jackson')),
      justice_name = forcats::fct_rev(justice_name),
      majority = factor(majority, levels = c('Majority Vote', 'Minority Vote'))) %>%
    ggplot(aes(x = justice_name, y = count, fill = majority)) +
    geom_bar(stat = 'identity', colour = 'gray5', position = position_dodge2(width = 0.9)) +
    geom_hline(yintercept = 0 ) +
    scale_fill_manual(values = c('deepskyblue3', 'coral3')) +
    geom_label(aes(label = count), position = position_dodge2(width = 0.9, preserve = "single"), color = "gray5", size=7, fill = 'white', show.legend = FALSE) +

    labs(x = '',
         y = '',
         fill = '') +
    scale_y_continuous(breaks = seq(10, 60, 10)) +
    coord_flip() +
    theme(legend.position = 'bottom',
          legend.text = element_text(size = 15, colour = 'gray5'),
          axis.text = element_text(size = 18, colour = 'gray5'),
          axis.ticks = element_blank(),
          axis.title = element_text(size = 20),
          axis.line = element_line(colour = 'black'),
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'))


  ggsave(majority_minority_votes, file = 'stat_pack_OT23/Figures/statpack_figures/majority_minority_votes_by_justice.png', height = 8, width = 10, units = 'in')

  } # Dissent Counts


################################################################################
#Oral Arguments
# Calendar
# Participation - Justices & Attorneys
# Time & Word Counts - By Argument
################################################################################

{

  base_url <- "https://github.com/JakeTruscott/scotustext/raw/master/Data/"
  rdata_url <- paste0(base_url, "scotus_transcripts_23.rdata")
  oa23 <- get(load(url(rdata_url)))

} #Load OT2023 OA Data

{

  for (i in unique(scotus_OT23$sitting)){

    attorneys <- scotus_OT23 %>%
      filter(sitting == i) %>%
      mutate(response_to = ifelse(lag(speaker_type) == 'Justice', lag(speaker), NA)) %>%
      filter(speaker_type == "Attorney") %>%
      mutate(speaker = ifelse(speaker == 'MR. SYNDER', 'MR. SNYDER', speaker)) %>%
      filter(!is.na(response_to)) %>%
      group_by(docket, speaker, response_to) %>%
      summarise(total_words = sum(word_count, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(response_to = gsub("(CHIEF JUSTICE |JUSTICE )", "", response_to)) %>%
      pivot_wider(names_from = response_to, values_from = total_words) %>%
      mutate(total_words = rowSums(select(., -c(docket, speaker)), na.rm = TRUE)) %>%
      select(docket, speaker, total_words) %>%
      mutate(speaker = gsub('(\\, Jr\\.| II$| III$)', '', speaker),
             speaker = str_extract(speaker, "\\b\\w+$")) %>%
      group_by(docket) %>%
      reframe(combined = paste0('\\\\ ', speaker, ' (', total_words, ')')) %>%
      group_by(docket) %>%
      summarize(combined = paste(combined, collapse = " ")) %>%
      left_join(shorthand_case_names, by = 'docket') %>%
      select(short_hand, combined, order) %>%
      arrange(order) %>%
      select(-c(order)) %>%
      rename(case_name = short_hand) %>%
      mutate(case_name = gsub('\\,', '', case_name))

    output_path = paste0("stat_pack_OT23/Tables/oral_argument_speaking/attorney_participation_", i, ".csv")
    write.table(attorneys, file = output_path,  row.names = F, quote = F, sep = ',')

    attorneys <- scotus_OT23 %>%
      filter(sitting == i) %>%
      mutate(response_to = ifelse(lag(speaker_type) == 'Justice', lag(speaker), NA)) %>%
      filter(speaker_type == "Attorney") %>%
      mutate(speaker = ifelse(speaker == 'MR. SYNDER', 'MR. SNYDER', speaker)) %>%
      filter(!is.na(response_to)) %>%
      group_by(case_name, speaker, response_to) %>%
      summarise(total_words = sum(word_count, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(response_to = gsub("(CHIEF JUSTICE |JUSTICE )", "", response_to)) %>%
      pivot_wider(names_from = response_to, values_from = total_words) %>%
      mutate(total_words = rowSums(select(., -c(case_name, speaker)), na.rm = TRUE)) %>%
      select(case_name, speaker, total_words) %>%
      mutate(speaker = gsub('(\\, Jr\\.| II$| III$)', '', speaker),
             speaker = str_extract(speaker, "\\b\\w+$"),
             case_name = gsub('\\,', '', case_name))

    output_path = paste0("stat_pack_OT23/Statpack Replication Data/Oral Arguments/Attorney Participation/", i, "_Sitting_Calendar_OT23.csv")
    write.csv(attorneys, file = output_path,  row.names = F, quote = F)

  }


} #Attorney Participation Table - Shortened for StatPack

{


  for (i in unique(scotus_OT23$sitting)){

     calendar_temp <- scotus_OT23 %>%
      filter(sitting == i) %>%
      filter(speaker_type == 'Attorney') %>%
      select(docket, speaker) %>%
      unique() %>%
      left_join(shorthand_case_names, by = 'docket') %>%
      unique() %>%
      rename(case_name = short_hand) %>%
      group_by(case_name) %>%
      reframe(attorney_count = max(row_number()),
              speaker = speaker,
              docket = docket,
              order = order) %>%
      mutate(speaker = gsub('(\\, Jr\\.| II$| III$)', '', speaker),
             speaker = str_extract(speaker, "\\b\\w+$"),
             case_name = gsub('\\,', '', case_name)) %>%
      group_by(case_name, docket) %>%
      reframe(combined = paste0('\\\\ ', speaker),
              attorney_count = attorney_count,
              order = order) %>%
      group_by(case_name) %>%
      mutate(attorney_number = row_number(),
             combined = ifelse(attorney_count == 2 & attorney_number == 2,  paste0(combined, ' \\\\'), combined)) %>%
      group_by(case_name, docket) %>%
      reframe(combined = paste(combined, collapse = ' '),
                order = order) %>%
       unique() %>%
       arrange(order)



    output_path = paste0("stat_pack_OT23/Tables/oral_argument_speaking/argument_calendar_", i, ".csv")

    write.table(calendar_temp, file = output_path,  row.names = F, quote = F, sep = ',')

  }



} #Argument Calendar

{


  {
    custom_colors <- c("#5FA934", "#3B5E8B", "#522559")

    scotus_term <- scotus_OT23 %>%
      filter(speaker_type == 'Justice') %>%
      group_by(speaker, case_name) %>%
      summarise(total_word_count = sum(word_count)) %>%
      group_by(speaker) %>%
      summarise(total_word_count = sum(total_word_count)) %>%
      mutate(speaker = gsub("(CHIEF JUSTICE |JUSTICE )", "", speaker)) %>%
      mutate(speaker = factor(speaker, levels = c('ROBERTS', 'THOMAS', 'ALITO', 'SOTOMAYOR', 'KAGAN', 'GORSUCH', 'KAVANAUGH', 'BARRETT', 'JACKSON')))

    word_count_plot <- ggplot(data = scotus_term, aes(y = total_word_count, x = speaker, fill = total_word_count)) +
      #scale_y_continuous(labels = scales::comma, breaks = seq(10000, 80000, 10000)) +
      geom_hline(yintercept = 0, colour = 'gray5') +
      geom_col(colour = 'gray5') +
      geom_text(aes(label = total_word_count), vjust = -0.5, size = 5) +
      geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1) + # Add a vertical line for the y-axis
      scale_fill_gradientn(
        colors = custom_colors,
        breaks = c(min(scotus_term$total_word_count), mean(scotus_term$total_word_count), max(scotus_term$total_word_count)), # Custom breaks
        labels = scales::comma_format(),
        guide = guide_colorbar(
          title = "Total Word Count",
          title.position = "top")) +
      labs(y  = " ",
           x = " ",
           title = " ") +
      scale_x_discrete(labels = justice_image_labels) +
      theme_classic() +
      theme(
        panel.grid = element_blank(),
        axis.text.x = ggtext::element_markdown(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.background = element_rect(linewidth = 1, fill = "NA", colour = "black"),
        legend.box.background = element_rect(fill = NA, colour = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.title.align = 0.5,
        legend.text = element_text(size = 12),
        plot.caption = element_text(hjust = 0.5, size = 12),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5))


  } #Totals Graph (Active)
  ggsave("stat_pack_OT23/Figures/statpack_figures/word_count_plot_OT23.png", word_count_plot, dpi = 300)

  oa <- scotus_OT23 %>%
    filter(speaker_type == 'Justice') %>%
    group_by(speaker, docket) %>%
    summarise(total_word_count = sum(word_count)) %>%
    pivot_wider(names_from = speaker, values_from = total_word_count, names_prefix = "word_count_") %>%
    left_join(shorthand_case_names, by = 'docket')

  names(oa) <- gsub('(CHIEF JUSTICE |JUSTICE )', '', names(oa))
  names(oa) <- gsub('word_count_', '', names(oa))

  oa <- oa %>%
    rename(case_name = short_hand) %>%
    relocate(case_name) %>%
    select(-c(docket))

  oa_data <- oa


  for (i in unique(oa_data$sitting)){

    oa_data_temp <- oa_data %>%
      filter(sitting == i) %>%
      arrange(order) %>%
      select(-c(sitting, order)) %>%
      mutate(case_name = gsub('\\,', '', case_name)) %>%
      select(case_name, ROBERTS, THOMAS, ALITO, SOTOMAYOR, KAGAN, GORSUCH, KAVANAUGH, BARRETT, JACKSON)

    write.table(oa_data_temp, file = paste0("stat_pack_OT23/Tables/oral_argument_speaking/oral_argument_participation_", i, ".csv"), row.names = F, quote = F, sep = ',')

    write.table(oa_data_temp, file = paste0('stat_pack_OT23/Statpack Replication Data/Oral Arguments/Justice Word Counts/', i, '_Sitting_OT23.csv'), row.names = F, quote = F, sep = ',')

  }


} #Word Count (By Sitting)

{
  custom_colors <- c("#5FA934", "#3B5E8B", "#522559")

  word_counts <- scotus_OT23 %>%
    filter(speaker_type == 'Justice') %>%
    group_by(speaker) %>%
    summarise(word_count = sum(word_count)) %>%
    mutate(speaker = factor(speaker, levels = c('ROBERTS', 'THOMAS', 'ALITO', 'SOTOMAYOR', 'KAGAN', 'GORSUCH', 'KAVANAUGH', 'BARRETT', 'JACKSON')))

  word_count_plot <- ggplot(data = word_counts, aes(y = word_count, x = speaker, fill = word_count)) +
    geom_hline(yintercept = 0, colour = 'gray5') +
    geom_col(colour = 'gray5') +
    geom_text(aes(label = scales::comma(word_count)), vjust = -0.5, size = 7) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1) + # Add a vertical line for the y-axis
    scale_fill_gradientn(
      colors = custom_colors,
      breaks = c(min(word_counts$word_count), mean(word_counts$word_count), max(word_counts$word_count)), # Custom breaks
      labels = scales::comma_format(),
      guide = guide_colorbar(
        title = "Total Word Count",
        title.position = "top")) +
    labs(y  = " ",
         x = " ",
         title = " ") +
    scale_x_discrete(labels = justice_image_labels) +
    theme_classic() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = ggtext::element_markdown(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      legend.background = element_rect(linewidth = 1, fill = "NA", colour = "black"),
      legend.box.background = element_rect(fill = NA, colour = "black"),
      legend.position = "none",
      legend.title = element_blank(),
      legend.title.align = 0.5,
      legend.text = element_text(size = 12),
      plot.caption = element_text(hjust = 0.5, size = 12),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 15, hjust = 0.5))

  ggsave(word_count_plot, file = 'stat_pack_OT23/Figures/statpack_figures/word_count_plot_OT23.png', height = 10, width = 10, units = 'in')

} # Word Count (Total Figure)

{

  base_url <- "https://github.com/JakeTruscott/scotustext/raw/master/Data/"
  rdata_url <- paste0(base_url, "scotus_transcripts_23.rdata")
  load(url(rdata_url))

  {
    custom_colors <- c("#5FA934", "#3B5E8B", "#522559")

    scotus_term <- scotus_OT23 %>%
      filter(speaker_type == 'Justice') %>%
      mutate(case_name = ifelse(grepl('Kinder Morgan', case_name), 'Ohio, Et Al. Applicants v. Epa', case_name)) %>%
      mutate(time_spoken = text_stop - text_start) %>%
      group_by(speaker) %>%
      summarise(total_time_spoken = sum(time_spoken)) %>%
      mutate(total_time_spoken_minutes = round(total_time_spoken/60, 2)) %>%
      select(speaker, total_time_spoken_minutes) %>%
      mutate(speaker = factor(speaker, levels = c('ROBERTS', 'THOMAS', 'ALITO', 'SOTOMAYOR', 'KAGAN', 'GORSUCH', 'KAVANAUGH', 'BARRETT', 'JACKSON')))

    total_time_spoken_plot <- ggplot(data = scotus_term, aes(y = total_time_spoken_minutes, x = speaker, fill = total_time_spoken_minutes)) +
      geom_hline(yintercept = 0, colour = 'gray5') +
      geom_col(colour = 'gray5') +
      geom_text(aes(label = total_time_spoken_minutes), vjust = -0.5, size = 5) +
      geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1) + # Add a vertical line for the y-axis
      scale_fill_gradientn(
        colors = custom_colors,
        breaks = c(min(scotus_term$total_time_spoken_minutes), mean(scotus_term$total_time_spoken_minutes), max(scotus_term$total_time_spoken_minutes)), # Custom breaks
        labels = scales::comma_format(),
        guide = guide_colorbar(
          title = "Total Word Count",
          title.position = "top")) +
      labs(y  = " ",
           x = " ",
           title = " ") +
      scale_x_discrete(labels = justice_image_labels) +
      theme_classic() +
      theme(
        panel.grid = element_blank(),
        axis.text.x = ggtext::element_markdown(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.background = element_rect(linewidth = 1, fill = "NA", colour = "black"),
        legend.box.background = element_rect(fill = NA, colour = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.title.align = 0.5,
        legend.text = element_text(size = 12),
        plot.caption = element_text(hjust = 0.5, size = 12),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5))



  } #Totals Graph (Active)
  ggsave("stat_pack_OT23/Figures/statpack_figures/total_time_spoken_plot_OT23.png", total_time_spoken_plot, dpi = 300)

  for (i in unique(scotus_OT23$sitting)){

    time_spoken_total <- scotus_OT23 %>%
      filter(speaker_type == 'Justice') %>%
      filter(sitting == i) %>%
      mutate(time_spoken = text_stop - text_start) %>%
      group_by(speaker, docket) %>%
      summarise(total_time_spoken = sum(time_spoken)) %>%
      mutate(total_time_spoken_minutes = round(total_time_spoken/60, 2)) %>%
      group_by(docket) %>%
      pivot_wider(names_from = speaker, values_from = total_time_spoken_minutes, names_prefix = "time_spoken_") %>%
      summarise_all(.funs = sum, na.rm = T) %>%
      mutate(total_time_spoken = rowSums(across(-c(total_time_spoken, docket)))) %>%
      rename_with(~str_replace(., "time_spoken_", ""), -total_time_spoken) %>%
      left_join(shorthand_case_names, by = 'docket') %>%
      arrange(order) %>%
      select(short_hand, total_time_spoken, ROBERTS, THOMAS, ALITO, SOTOMAYOR, KAGAN, GORSUCH, KAVANAUGH, BARRETT, JACKSON) %>%
      mutate(short_hand = gsub('\\,', '', short_hand))



    output_path = paste0("stat_pack_OT23/Tables/oral_argument_speaking/oral_argument_speaking_times_", i, '.csv')
    write.table(time_spoken_total, file = output_path, row.names = F, quote = F, sep = ',')


    output_path = paste0("stat_pack_OT23/Statpack Replication Data/Oral Arguments/Justice Speaking Times/", i, '_Sitting_OT23.csv')
    write.table(time_spoken_total, file = output_path, row.names = F, quote = F, sep = ",")


  }




} #Speaking Times (By Sitting)

{

  custom_colors <- c("#5FA934", "#3B5E8B", "#522559")

  time_spoken_total <- scotus_OT23 %>%
    filter(speaker_type == 'Justice') %>%
    mutate(time_spoken = text_stop - text_start) %>%
    group_by(speaker, docket) %>%
    summarise(total_time_spoken = sum(time_spoken)) %>%
    mutate(total_time_spoken_minutes = round(total_time_spoken/60, 2)) %>%
    group_by(docket) %>%
    pivot_wider(names_from = speaker, values_from = total_time_spoken_minutes, names_prefix = "time_spoken_") %>%
    summarise_all(.funs = sum, na.rm = T) %>%
    mutate(total_time_spoken = rowSums(across(-c(total_time_spoken, docket)))) %>%
    rename_with(~str_replace(., "time_spoken_", ""), -total_time_spoken) %>%
    left_join(shorthand_case_names, by = 'docket') %>%
    arrange(order) %>%
    select(short_hand, total_time_spoken, ROBERTS, THOMAS, ALITO, SOTOMAYOR, KAGAN, GORSUCH, KAVANAUGH, BARRETT, JACKSON) %>%
    mutate(short_hand = gsub('\\,', '', short_hand)) %>%
    select(-c(short_hand, total_time_spoken)) %>%
    summarise(across(everything(), sum)) %>%
    pivot_longer(everything(),
                 names_to = "speaker",
                 values_to = "total_word_count") %>%
    mutate(speaker = factor(speaker, levels = c('ROBERTS', 'THOMAS', 'ALITO', 'SOTOMAYOR', 'KAGAN', 'GORSUCH', 'KAVANAUGH', 'BARRETT', 'JACKSON')))

  custom_colors <- c("#5FA934", "#3B5E8B", "#522559")

  speaking_time_plot <- ggplot(data = time_spoken_total, aes(y = total_word_count, x = speaker, fill = total_word_count)) +
    #scale_y_continuous(labels = scales::comma, breaks = seq(10000, 80000, 10000)) +
    geom_hline(yintercept = 0, colour = 'gray5') +
    geom_col(colour = 'gray5') +
    geom_text(aes(label = scales::comma(round(total_word_count, 0))), vjust = -0.5, size = 7) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1) + # Add a vertical line for the y-axis
    scale_fill_gradientn(
      colors = custom_colors,
      breaks = c(min(time_spoken_total$total_word_count), mean(time_spoken_total$total_word_count), max(time_spoken_total$total_word_count)), # Custom breaks
      labels = scales::comma_format(),
      guide = guide_colorbar(
        title = "Total Word Count",
        title.position = "top")) +
    labs(y  = " ",
         x = " ",
         title = " ") +
    scale_x_discrete(labels = justice_image_labels) +
    theme_classic() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = ggtext::element_markdown(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      legend.background = element_rect(linewidth = 1, fill = "NA", colour = "black"),
      legend.box.background = element_rect(fill = NA, colour = "black"),
      legend.position = "none",
      legend.title = element_blank(),
      legend.title.align = 0.5,
      legend.text = element_text(size = 12),
      plot.caption = element_text(hjust = 0.5, size = 12),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 15, hjust = 0.5))

    ggsave(speaking_time_plot, file = 'stat_pack_OT23/Figures/statpack_figures/total_time_spoken_plot_OT23.png', height = 10, width = 10, units = 'in')

} #Speaking Times (Total Figure)


################################################################################
# Docket
################################################################################

{

  docket_path <- list.files("docket_parser/OT23_docket_sheets", full.names = T)

  dockets <- data.frame()

  for (i in 1:length(docket_path)){

    temp_docket <- get(load(docket_path[i]))
    dockets <- bind_rows(dockets, temp_docket)

    if (i %% 100 == 0){
      message('Completed ', i, ' of ', length(docket_path))
    }

    #rm(docket_combined) #Remove Temp Docket
    rm(temp_docket)

  } #Combine Docket Sheets Into Single DF

  for (i in 1:nrow(dockets)){

    if (dockets$docketed[i] == '') {
      dockets$docketed[i] <- format(as.Date(dockets$docket[i][[1]][[1]][1], format = "%Y-%m-%d"), "%B %d, %Y")
    }

  } #Fix Dockets w/out Docketed Date (Applications & Motions)

  dockets <- dockets %>%
    unique() #Filter to Unique

  save(dockets, file = 'docket_parser/docket_filings_ot_23.rdata')



} #Compile OT23 Dockets (IF Needed)

load('docket_parser/docket_filings_ot_23.rdata') #Load OT23 Dockets

load('docket_parser/OT18_OT22_dockets.rdata') #Load OT23 Dockets

{

  dockets_ot_23 <- dockets %>%
    mutate(filing_year = 2023)


  dockets_ot18_ot22 <- ot18_ot22_dockets %>%
    unique() %>%
    mutate(filing_year = gsub('(\\-.*|a.*|m.*)', '', docket_number),
           filing_year = paste0('20', filing_year),
           filing_year = as.numeric(filing_year))

  combined_dockets <- dockets_ot18_ot22 %>%
    bind_rows(dockets_ot_23)

} #Create Combined OT18 - OT23 Docket Frame ('combined_dockets')

{

  dockets_ot_23 <- dockets %>%
    mutate(docket_type = case_when(
      .default = 'Petitions',
      grepl('A', docket_number) ~ 'Applications',
      grepl('M', docket_number) ~ 'Motions'),
      docketed = lubridate::mdy(docketed),
      docketed = format(docketed, "%Y-%m")) %>%
    group_by(docketed, docket_type) %>%
    summarise(count = n()) %>%
    ggplot(aes(x = docketed, y = count)) +
    geom_bar(stat = 'identity', color = 'gray5', aes(fill = docket_type), position = position_dodge2()) +
    scale_fill_manual(values = c('coral3', 'deepskyblue4', 'gray50')) +
    theme_minimal() +
    facet_wrap(~ docket_type, scales = "free_y", nrow = 3) +
    geom_label(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.2)))  +
    scale_x_discrete(labels = c('Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun')) +
    geom_hline(yintercept = 0) +
    labs(
      x = '\n',
      y = '\n') +
    theme(
      panel.border = element_rect(size = 1, colour = 'gray5', fill = NA),
      axis.text = element_text(size = 12, colour = 'black'),
      axis.title = element_text(size = 14, colour = 'black'),
      plot.title = element_text(size = 16, colour = 'black', face = 'bold'),
      plot.subtitle = element_text(size = 14, colour = 'black'),
      legend.position = 'none',
      legend.title = element_blank(),
      legend.text = element_text(size = 10, colour = 'black'),
      strip.text = element_text(size = 12, colour = 'black', face = 'bold'),
      strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
      panel.background = element_rect(size = 1, fill = 'white', colour = 'white'),
      plot.background = element_rect(size = 1, fill = 'white', colour = 'white'))

  ggsave("stat_pack_OT23/Figures/statpack_figures/dockets_ot_23.png", dockets_ot_23 ,height = 10, width = 10, units = 'in')

  dockets_ot_23 <- dockets %>%
    mutate(docket_type = case_when(
      .default = 'Petitions',
      grepl('A', docket_number) ~ 'Applications',
      grepl('M', docket_number) ~ 'Motions'),
      docketed = lubridate::mdy(docketed),
      docketed = format(docketed, "%Y-%m")) %>%
    group_by(docketed, docket_type) %>%
    summarise(count = n()) %>%
    pivot_wider(names_from = 'docket_type', values_from = 'count') %>%
    mutate(month = gsub('.*\\-', '', docketed),
           month = case_when(
             month == '06' ~ 'June',
             month == '07' ~ 'July',
             month == '08' ~ 'August',
             month == '09' ~ 'September',
             month == '10' ~ 'October',
             month == '11' ~ 'November',
             month == '12' ~ 'December',
             month == '01' ~ 'January',
             month == '02' ~ 'February',
             month == '03' ~ 'March',
             month == '04' ~ 'April',
             month == '05' ~ 'May'),
           year = gsub('\\-.*', '', docketed),
           docketed = paste0(month, ' ', year)) %>%
    select(-c(month, year))


  write.csv(dockets_ot_23, file = 'stat_pack_OT23/Statpack Replication Data/Docket/filing_trends_by_month_OT23.csv', row.names = F)


} #Main Filing Trends (OT23) by Month

{

  dockets_ot_23_origin <- dockets %>%
    mutate(docket_type = case_when(
      .default = 'Petitions',
      grepl('A', docket_number) ~ 'Applications',
      grepl('M', docket_number) ~ 'Motions'),
      docketed = lubridate::mdy(docketed),
      docketed = format(docketed, "%Y-%m")) %>%
    filter(docket_type == 'Petitions') %>%
    filter(!lower_court == '') %>%
    group_by(lower_court) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    mutate(origin_type = ifelse(grepl('United States Court of Appeals for the', lower_court) & grepl('Circuit', lower_court), 'Circuit', 'State/Other')) %>%
    mutate(lower_court = ifelse(origin_type == 'Circuit', gsub('United States Court of Appeals for the ', '', lower_court), lower_court),
           lower_court = gsub('United States ', '', lower_court)) %>%
    mutate(lower_court = gsub('\\,', ' -', lower_court)) %>%
    mutate(lower_court = gsub('Division', 'Div.', lower_court),
           lower_court = gsub('District', 'Dist.', lower_court),
           lower_court = gsub('Appellate', 'App.', lower_court),
           lower_court = gsub('Department', 'Dept.', lower_court)) %>%
    select(lower_court, origin_type, count) %>%
    rename(`Origin` = lower_court,
           `Petitions` = count,
           `Type` = origin_type)

  write.csv(dockets_ot_23_origin, file = 'stat_pack_OT23/Statpack Replication Data/Docket/filings_court_of_origin_OT23.csv', row.names = F)

  fifth_circuit_petitions <- sum(dockets_ot_23_origin$Petitions[dockets_ot_23_origin$Origin == 'Fifth Circuit'])
  total_petitions <- sum(dockets_ot_23_origin$Petitions)
  (fifth_circuit_petitions / total_petitions) * 100

  max <- seq(20, nrow(dockets_ot_23_origin), by = 20)
  min = c()
  for (i in 1:length(max)){
    temp_min = max[i] - 19
    min = c(min, temp_min)
  }

  for (i in 1:length(min)){

    start = min[i]
    end = max[i]

    temp_data <- dockets_ot_23_origin[c(start:end),]
    temp_path = paste0('C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/stat_pack_OT23/Tables/dockets_ot_23_origin_', i, '.csv')

    write.table(temp_data, file = temp_path, row.names = F, quote = F, sep = ',')

  }







} #Filing Trends by Court of Origin

{

  shapefile_circuit<- st_read("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/stat_pack_OT23/statpack_replication/circuit_court_maps/US_CourtOfAppealsCircuits.shp") #Circuit Maps

  circuit_origin <- dockets_ot_23_origin %>%
    filter(Type == 'Circuit') %>%
    mutate(NAME = toupper(Origin)) %>%
    left_join(shapefile_circuit, by = 'NAME')

  circuit_origin <- circuit_origin %>%
    ggplot() +
    geom_sf(aes(fill = Petitions, geometry = geometry), color = 'black') +
    coord_sf(xlim = c(xmin = -130, xmax = -65), ylim = c(ymin = 25, ymax = 50)) +
    geom_segment(aes(x = -65, xend = -69, y = 47, yend = 47)) +
    geom_label(mapping = aes(x = -65, y = 47), label = '1st') +
    geom_segment(aes(x = -75, xend = -75, y = 47, yend = 45)) +
    geom_label(mapping = aes(x = -75, y = 47), label = '2nd') +
    geom_segment(aes(x = -68, xend = -74, y = 40, yend = 40)) +
    geom_label(mapping = aes(x = -68, y = 40), label = '3rd') +
    geom_label(mapping = aes(x = -80, y = 35), label = '4th') +
    geom_label(mapping = aes(x = -99, y = 31), label = '5th') +
    geom_segment(aes(x = -82, xend = -83, y = 48, yend = 44)) +
    geom_label(mapping = aes(x = -82, y = 48), label = '6th')  +
    geom_label(mapping = aes(x = -88, y = 40), label = '7th') +
    geom_label(mapping = aes(x = -97, y = 45), label = '8th') +
    geom_label(mapping = aes(x = -118, y = 40), label = '9th') +
    geom_label(mapping = aes(x = -108, y = 40), label = '10th') +
    geom_label(mapping = aes(x = -85, y = 33), label = '11th') +
    geom_segment(aes(x = -72, xend = -77, y = 37, yend = 38.5)) +
    geom_label(mapping = aes(x = -72, y = 37), label = 'DC')  +
    scale_fill_continuous(low = 'gray',
                          high = 'black') +
    labs(fill = 'Petitions Filed (OT2023)') +
    theme_void() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.title = element_text(size = 10), # Adjust the size of the legend title here
      legend.text.align = 0.5,
      legend.text = element_text(size = 10, colour = "gray5"),
      legend.position = "bottom",
      legend.box="horizontal",
      legend.key.width = unit(1.5, "cm"),  # Adjust the width here (e.g., "4cm" for a 4 cm width)
      strip.text = element_text(size = 14, face = "bold"),
      strip.background = element_rect(fill = "gray", colour = "gray5"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 15, hjust = 0.5)) +
    guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5),
           size = guide_legend(title.position="top", title.hjust = 0.5)) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

  ggsave(circuit_origin, file = "stat_pack_OT23/Figures/statpack_figures/circuit_court_map.png", width = 6, height = 3.5) # Adjust width and height as needed


} #Circuit Map

{

  load('docket_parser/OT18_OT22_dockets.rdata') #Load (Already Compiled...)

  dockets_ot_23 <- dockets %>%
    mutate(docket_type = case_when(
      .default = 'Petitions',
      grepl('A', docket_number) ~ 'Applications',
      grepl('M', docket_number) ~ 'Motions'),
      docketed = lubridate::mdy(docketed),
      docketed = format(docketed, "%Y-%m")) %>%
    group_by(docketed, docket_type) %>%
    summarise(count = n()) %>%
    rename(filing_type = docket_type) %>%
    group_by(filing_type) %>%
    summarise(count = sum(count)) %>%
    mutate(docketed = 2023)


  longitudinal_docketing_trends_2018_2023 <- ot18_ot22_dockets %>%
    mutate(filing_type = case_when(
      .default = 'Petitions',
      grepl('(M|m)', docket_number) ~ 'Motions',
      grepl('(a|A)', docket_number) ~ 'Applications')) %>%
    mutate(filing_term = as.numeric(gsub('(\\-.*|a.*|A.*|m.*|M.*)', '', docket_number))) %>%
    select(filing_term, filing_type)  %>%
    group_by(filing_term, filing_type) %>%
    summarise(count = n()) %>%
    unique() %>%
    rename(docketed = filing_term) %>%
    mutate(docketed = paste0('20', docketed),
           docketed = as.numeric(docketed)) %>%
    bind_rows(dockets_ot_23) %>%
    ggplot(aes(x = factor(docketed), y = count)) +
    geom_bar(stat = 'identity', color = 'gray5', aes(fill = filing_type), position = position_dodge2()) +
    scale_fill_manual(values = c('coral3', 'deepskyblue4', 'gray50')) +
    theme_minimal() +
    facet_wrap(~ filing_type, scales = "free_y", nrow = 3) +
    geom_label(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.2)))  +
    geom_hline(yintercept = 0) +
    labs(
      x = '\n',
      y = '\n') +
    theme(
      panel.border = element_rect(size = 1, colour = 'gray5', fill = NA),
      axis.text = element_text(size = 12, colour = 'black'),
      axis.title = element_text(size = 14, colour = 'black'),
      plot.title = element_text(size = 16, colour = 'black', face = 'bold'),
      plot.subtitle = element_text(size = 14, colour = 'black'),
      legend.position = 'none',
      legend.title = element_blank(),
      legend.text = element_text(size = 10, colour = 'black'),
      strip.text = element_text(size = 12, colour = 'black', face = 'bold'),
      strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
      panel.background = element_rect(size = 1, fill = 'white', colour = 'white'),
      plot.background = element_rect(size = 1, fill = 'white', colour = 'white'))


  ggsave("stat_pack_OT23/Figures/statpack_figures/longitudinal_docketing_trends_2018_2023.png", longitudinal_docketing_trends_2018_2023 , height = 10, width = 10, units = 'in')


  longitudinal_docketing_trends_2018_2023 <- ot18_ot22_dockets %>%
    mutate(filing_type = case_when(
      .default = 'Petitions',
      grepl('(M|m)', docket_number) ~ 'Motions',
      grepl('(a|A)', docket_number) ~ 'Applications')) %>%
    mutate(filing_term = as.numeric(gsub('(\\-.*|a.*|A.*|m.*|M.*)', '', docket_number))) %>%
    select(filing_term, filing_type)  %>%
    group_by(filing_term, filing_type) %>%
    summarise(count = n()) %>%
    unique() %>%
    rename(docketed = filing_term) %>%
    mutate(docketed = paste0('20', docketed),
           docketed = as.numeric(docketed)) %>%
    bind_rows(dockets_ot_23) %>%
    pivot_wider(names_from = 'filing_type', values_from = 'count')

  write.csv(longitudinal_docketing_trends_2018_2023, file = 'stat_pack_OT23/Statpack Replication Data/Docket/filing_trends_OT18_OT23.csv', row.names = F)


} #Combined Docket Trends (18-23)

{

  amici_dockets <- data.frame() #Create Empty DF to Store Output

  combined_petitions <- combined_dockets %>%
    filter(!grepl('(a|m)', docket_number, ignore.case = T))

  {

    grants_lists <- list.files('stat_pack_OT23/statpack_replication/Misc Data/SCOTUS_granted_cases_lists', full.names = T)
    combined_grants_lists <- list()

    for (i in 1:length(grants_lists)){
      temp_pdf <- pdftools::pdf_text(grants_lists[i])
      combined_grants_lists[[i]] <- temp_pdf
    }


    filter_words <- function(input_string) {
      words <- str_split(input_string, "\\s+")[[1]]
      filtered_words <- str_subset(words, "(16\\-|17\\-|18\\-|19\\-|20\\-|21\\-|22\\-|23\\-)")
      return(filtered_words)
    }

    cases <- c()

    for (i in 1:length(combined_grants_lists)){

      pdf_text = combined_grants_lists[[i]]

      temp_cases <- c()

      for (page in 1:length(pdf_text)){
        words <- str_split(pdf_text, "\\s+")[[page]]
        filtered_words <- str_subset(words, "(16\\-|17\\-|18\\-|19\\-|20\\-|21\\-|22\\-|23\\-)")
        filtered_words <- gsub("[^0-9-]", "", filtered_words)
        filtered_words <- unique(filtered_words)
        temp_cases <- c(temp_cases, filtered_words)
      }

      cases <- c(cases, temp_cases)

      cases <- unique(cases)

    }




    } #Get List of Granted Cases from Grants Lists

  granted_cases <- cases

  for (i in 1:nrow(combined_petitions)){

    temp_row <- combined_petitions[i,]

    temp_docket_number <- temp_row$docket_number[1]
    temp_filing_year <- temp_row$filing_year[1]
    temp_docket <- temp_row$docket[[1]]
    temp_docket <- temp_docket %>%
      mutate(amici = ifelse(grepl('(Amicus Brief of|Brief amici curiae of|Brief amicus curiae of)', entry, ignore.case = T), 1, 0),
             amici = ifelse(grepl('not accepted', entry, ignore.case = T) & amici == 1, 0, amici))
    total_amici <- sum(temp_docket$amici)
    petition_granted = ifelse(temp_docket_number %in% granted_cases, 1, 0)

    temp_combined <- data.frame(docket_number = temp_docket_number,
                                filing_year = temp_filing_year,
                                total_amici = total_amici,
                                petition_granted = petition_granted)

    amici_dockets <- bind_rows(amici_dockets, temp_combined)

    if (i %% 1000 == 0){
      message('Completed ', i, ' of ', nrow(combined_dockets))
    }

  }

  write.csv(amici_dockets, file = 'stat_pack_OT23/Statpack Replication Data/Docket/amici_filed_by_filing_year.csv', row.names = F)

} # Amici Filings Compilation

{

  average_amici_filed_by_granted_type_granted <- amici_dockets %>%
    filter(grepl('\\-', docket_number)) %>%
    group_by(filing_year, petition_granted) %>%
    summarise(total_amici = sum(total_amici),
              total_cases = n()) %>%
    mutate(average_per_case = round(total_amici/total_cases, 2)) %>%
    mutate(petition_granted = ifelse(petition_granted == 1, 'Petition Granted', 'Petition Denied'),
           petition_granted = factor(petition_granted, levels = c('Petition Granted', 'Petition Denied'))) %>%
    filter(petition_granted == 'Petition Granted') %>%
    ggplot(aes(x = factor(filing_year), y = average_per_case)) +
    geom_bar(stat = 'identity', fill = 'gray50', colour = 'gray5') +
    scale_y_continuous(breaks = seq(5, 30, 5)) +
    geom_label(aes(label = average_per_case), vjust = 2, size = 7) +
    geom_hline(yintercept = 0) +
    theme_bw() +
    labs(
      x = '\nFiling Year\n',
      y = '\nAverage Amici Filed\n') +
    theme(legend.position = 'none',
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'),
          axis.text = element_text(size = 18, colour = 'black'),
          axis.title = element_text(size = 18, colour = 'black'))


  average_amici_filed_by_granted_type_denied <- amici_dockets %>%
    filter(grepl('\\-', docket_number)) %>%
    group_by(filing_year, petition_granted) %>%
    summarise(total_amici = sum(total_amici),
              total_cases = n()) %>%
    mutate(average_per_case = round(total_amici/total_cases, 2)) %>%
    mutate(petition_granted = ifelse(petition_granted == 1, 'Petition Granted', 'Petition Denied'),
           petition_granted = factor(petition_granted, levels = c('Petition Granted', 'Petition Denied')),
           filing_year = as.numeric(str_replace(as.character(filing_year), "^\\d{2}", ""))) %>%
    filter(petition_granted == 'Petition Denied') %>%
    ggplot(aes(x = factor(filing_year), y = average_per_case)) +
    geom_bar(stat = 'identity', fill = 'gray50', colour = 'gray5') +
    scale_y_continuous(breaks = seq(0.05, 0.15, 0.05), lim = c(0, 0.15)) +
    geom_label(aes(label = average_per_case), vjust = 2) +
    geom_hline(yintercept = 0) +
    theme_bw() +
    labs(
      x = '\nFiling Year\n',
      y = '\nAverage Amici Filed\n') +
    theme(legend.position = 'none',
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'),
          axis.text = element_text(size = 18, colour = 'black'),
          axis.title = element_text(size = 18, colour = 'black'))

  ggsave(average_amici_filed_by_granted_type_granted, file = 'stat_pack_OT23/Figures/statpack_figures/average_amici_filed_by_granted_type_granted.png', height = 10, width = 10, units = 'in')
  ggsave(average_amici_filed_by_granted_type_denied, file = 'stat_pack_OT23/Figures/statpack_figures/average_amici_filed_by_granted_type_denied.png', height = 10, width = 10, units = 'in')


} #Analyzing Amicus Filing Trends (2018-23)

{

  amici_stages <- combined_sitting_calendar %>%
    group_by(Sitting) %>%
    summarise(cert_amici = round(mean(`Cert Stage Amici`), 2),
              merits_amici = round(mean(`Merits Stage Amici`), 2)) %>%
    pivot_longer(cols = c(cert_amici, merits_amici)) %>%
    mutate(Sitting = factor(Sitting, levels = c('October', 'November', 'December', 'January', 'February', 'March', 'April')),
           name = ifelse(name == 'cert_amici', 'Cert Stage', 'Merits Stage')) %>%
    ggplot(aes(x = factor(Sitting), y = value)) +
    geom_bar(stat = 'identity', aes(fill = name), position = position_dodge2(0.9), colour = 'gray5') +
    scale_y_continuous(breaks = seq(5, 30, 5), lim = c(0, 32)) +
    scale_fill_manual(values = c('coral3', 'deepskyblue4')) +
    geom_label(aes(label = value), vjust = -0.5, position = position_dodge2(0.9), size = 5) +
    labs(x = '\nSitting',
         y = 'Number of Amici Filed\n') +
    geom_hline(yintercept = 0) +
    theme_minimal() +
    theme(
      panel.border = element_rect(size = 1, colour = 'gray5', fill = NA),
      axis.text = element_text(size = 12, colour = 'black'),
      axis.title = element_text(size = 14, colour = 'black'),
      plot.title = element_text(size = 16, colour = 'black', face = 'bold'),
      plot.subtitle = element_text(size = 14, colour = 'black'),
      legend.position = 'bottom',
      legend.title = element_blank(),
      legend.text = element_text(size = 10, colour = 'black'),
      strip.text = element_text(size = 12, colour = 'black', face = 'bold'),
      strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
      panel.background = element_rect(size = 1, fill = 'white', colour = 'white'),
      plot.background = element_rect(size = 1, fill = 'white', colour = 'white'))

  ggsave(amici_stages, file = 'stat_pack_OT23/Figures/statpack_figures/cert_merits_amici_OT23.png', height = 8, width = 10, units = 'in')


} #Amici Filed OT23 (Use Adam's Tables)
