################################################################################
# Scotuswatch Source
# Jake S. Truscott & Adam Feldman
# Created April 2025
################################################################################



###############################################################################
# Check Update History
###############################################################################

file_path <- sys.frame(1)$ofile  # Works when sourcing the file

if (!is.null(file_path)) {
  created_time <- file.info(file_path)$ctime  # File creation time
  modified_time <- file.info(file_path)$mtime  # Last modified time
  message('--------------------------------------')
  message('---------------- Meta ----------------')
  message('--------------------------------------')
  message("File Created: ", created_time)
  message("Last Modified: ", modified_time)
  rm(created_time, modified_time, file_path)
}

###############################################################################
# Load Necessary Packages & Dependencies for Reproduction
###############################################################################

check_and_install_packages <- function(){

  packages <- c('anytime', 'dplyr', 'ggplot2', 'ggpattern', 'ggtext', 'ggthemes',
                'grid', 'htmltools', 'httr', 'jsonlite', 'kableExtra', 'png',
                'readxl', 'reticulate', 'rvest', 'scotustext', 'stringi',
                'stringr', 'tidyr', 'tm')

  missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]

  # If there are missing packages
  if (length(missing_packages) > 0) {
    message("The following necessary packages are missing: ", paste(missing_packages, collapse = ", "))

    response <- readline("Would you like to install them? (Y/N): ")
    if (tolower(response) %in% c("yes", "y")) {
      install.packages(missing_packages, dependencies = TRUE, quiet = TRUE)
      message("Packages installed successfully.")
    } else {
      stop("Packages must be installed prior to compilation. Please install and try again.")
    }
  }

  invisible(lapply(packages, function(pkg) {
    suppressWarnings(suppressPackageStartupMessages(library(pkg, character.only = TRUE)))
  }))

  message('All Packages Installed and Deployed Correctly')
} # Check and Install Necessary Packages
check_and_install_packages() # Deploy



###############################################################################
# Justice Image Labels
###############################################################################

{

  justice_image_labels <- c(

    JACKSON = "<img src='https://api.oyez.org/sites/default/files/filefield_paths/thumbnail_ketanji_brown_jackson.png' style='width: 75px; height: 75px; object-fit: cover;' />",
    BARRETT = "<img src='https://api.oyez.org/sites/default/files/filefield_paths/barret-thumb.png' style='width: 75px; height: 75px; object-fit: cover;' />",
    KAVANAUGH = "<img src='https://api.oyez.org/sites/default/files/filefield_paths/Kavanaugh-thumb.png' style='width: 75px; height: 75px; object-fit: cover;' />",
    GORSUCH = "<img src='https://api.oyez.org/sites/default/files/filefield_paths/neil_gorsuch.thumb__0.png' style='width: 75px; height: 75px; object-fit: cover;' />",
    SOTOMAYOR = "<img src='https://api.oyez.org/sites/default/files/images/people/sonia_sotomayor/sonia_sotomayor.thumb.png' style='width: 75px; height: 75px; object-fit: cover;' />",
    KAGAN = "<img src='https://api.oyez.org/sites/default/files/images/people/elena_kagan/elena_kagan.thumb.png' style='width: 75px; height: 75px; object-fit: cover;' />",
    ALITO = "<img src='https://api.oyez.org/sites/default/files/images/people/samuel_alito_jr/samuel_alito_jr.thumb.png' style='width: 75px; height: 75px; object-fit: cover;' />",
    ROBERTS = "<img src='https://api.oyez.org/sites/default/files/images/people/john_g_roberts_jr/john_g_roberts_jr.thumb.png' style='width: 75px; height: 75px; object-fit: cover;' />",
    BREYER = "<img src='https://api.oyez.org/sites/default/files/images/people/stephen_g_breyer/stephen_g_breyer.thumb.png' style='width: 75px; height: 75px; object-fit: cover;' />",
    GINSBURG = "<img src='https://api.oyez.org/sites/default/files/images/people/ruth_bader_ginsburg/ruth_bader_ginsburg.thumb.png' style='width: 75px; height: 75px; object-fit: cover;' />",
    THOMAS = "<img src='https://api.oyez.org/sites/default/files/images/people/clarence_thomas/clarence_thomas.thumb.png' style='width: 75px; height: 75px; object-fit: cover;' />",
    SOUTER = "<img src='https://api.oyez.org/sites/default/files/images/people/david_h_souter/david_h_souter.thumb.png' style='width: 75px; height: 75px; object-fit: cover;' />",
    KENNEDY = "<img src='https://api.oyez.org/sites/default/files/images/people/anthony_m_kennedy/anthony_m_kennedy.thumb.png' style='width: 75px; height: 75px; object-fit: cover;' />"


  )

} # Justice Images (Oyez)

###############################################################################
# Decisions Table (HTML/CSV Output)
###############################################################################

#decisions <- read.csv("data/stat_pack_OT24/ot24_decisions/OT_24_Decisions.csv", as.is = T)

decisions_table <- function(input_path, output_path, output_type = 'html', cases_break = 10){

  {

'100 = Wrote Majority
1 = Joined Majority
2 = Wrote Regular Concurrence
3 = Joined Regular Concurrence
4 = Wrote Regular Concurrence & Joined Regular Concurrence
5 = Wrote Concurrence in Judgement
6 = Joined Concurrence in Judgement
7 = Wrote Special Concurrence
8 = Joined Special Concurrence
-1 = Wrote Dissenting Opinion
-2 = Joined Dissenting Opinion
-3 = Wrote Dissent & Joined Dissent'


  } # Coding Rules
  {

    get_vote_color <- function(value){
      if (value == "M*") {
        return('darkolivegreen')
      } else if (value == 'M'){
        return('#99CCFF')
      } else if (value == "RC"){
        return('#66B2FF')
      } else if (value == 'JRC'){
        return('#3399FF')
      } else if (value == 'RC & JRC'){
        return('#3399FF')
      } else if (value == 'CJ'){
        return('#FF9933')
      } else if(value == 'JCJ'){
        return('#FFCC99')
      } else if (value == 'SC'){
        return('#B265FF')
      } else if(value == 'JSC'){
        return('#6600CC')
      } else if (value == 'D'){
        return('#FF3333')
      } else if (value == 'JD'){
        return('#CC0000')
      } else if (value == 'D & JD'){
        return('#990000')
      } else if (value == 'DNP'){
        return('#FFFFFF')
      }
    } #Assign Color to Box by Vote Type

  } # Vote Colors

  if (grepl('.csv', decisions_file_path)){
    decisions <- read.csv(decisions_file_path, as.is = T)
  } else if (grepl('.rdata', decisions_path, ignore.case = T)){
    decisions <- get(load(decisions_path))
  } else {
    message('Unrecognized File Path: Please Load a CSV or .Rdata File')
    break
  } # Load File -- If Not CSV or Rdata -- STOP

  {

    decisions <- decisions %>%
      mutate(Date_Argued = anydate(Date_Argued),
             Date_Decided = anydate(Date_Decided)) %>%
      rename('Date Decided' = Date_Decided,
             'Date Argued' = Date_Argued,
             'Lower Court' = Lower_Court) %>%
      mutate(across(Coalition:ncol(.), ~ case_when(
        . == 100 ~ 'M*',
        . == 1 ~ 'M',
        . == 2 ~ 'RC',
        . == 3 ~ 'JRC',
        . == 4 ~ 'RC & JCR',
        . == 5 ~ 'CJ',
        . == 6 ~ 'JCJ',
        . == 7 ~ 'SC',
        . == 8 ~ 'JSC',
        . == -1 ~ 'D',
        . == -2 ~ 'JD',
        . == -3 ~ 'D & JD',
        is.na(.) ~ 'DNP',
        TRUE ~ as.character(.)
      )))


    decisions$`Date Argued` <- format(decisions$`Date Argued`, "%m/%d/%y")
    decisions$`Date Decided` <- format(decisions$`Date Decided`, "%m/%d/%y")

  } # Convert Values to DF

  {

    decisions_data <- decisions
    matching_columns <- intersect(colnames(decisions_data), names(justice_image_labels))
    original_column_names <- colnames(decisions_data)
    matching_columns <- colnames(decisions_data)[8:16]

  } # Compile Tables

  {

    decisions_data <- decisions_data %>%
      rowwise() %>%
      mutate_at(vars(all_of(matching_columns)),
                ~cell_spec(., background = get_vote_color(.))) %>%
      mutate(across(all_of(matching_columns),
                    ~gsub('border-radius: 4px;',
                          'border-radius: 4px; color: white; ', .))) %>%
      rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])

  } # Match w/ Justice Images

  {

    decisions_data <- decisions_data %>%
      ungroup() %>%
      mutate(case_id = row_number())

    n_chunks <- ceiling(nrow(decisions_data) / cases_break)
    decision_partitions <- split(decisions_data, ceiling(seq_len(nrow(decisions_data)) / cases_break)) # Split the dataframe into chunks of 10 rows

  } # Partition Into Smaller Files

  {

    for (i in 1:length(decision_partitions)){

      temp_decisions <- decision_partitions[[i]]
      min_case_id <- min(temp_decisions$case_id)
      max_case_id <- max(temp_decisions$case_id)
      temp_output_file_name <- file.path(output_path, paste0('decision_info_', min_case_id, '_', max_case_id))
      temp_decisions <- temp_decisions %>%
        dplyr::select(-c(case_id))

      if (output_type == 'html'){

        temp_decisions <- temp_decisions %>%
          mutate(`Date Decided` = as.Date(`Date Decided`, "%m/%d/%y"),
                 `Date Argued` = as.Date(`Date Argued`, "%m/%d/%y")) %>%
          arrange(`Date Decided`) %>%
          kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
          column_spec(c(1:6), bold = TRUE) %>%
          column_spec(7, bold = TRUE, border_right = TRUE) %>%
          row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
          row_spec(seq(1, nrow(temp_decisions), 1), align = 'center') %>%
          row_spec(nrow(temp_decisions), extra_css = "border-bottom: 2px solid;") %>%
          kable_styling(font_size = 18, bootstrap_options = c("striped", "hover", "condensed", "responsive"))

        temp_decisions <- as.character(temp_decisions)
        writeLines(temp_decisions, paste0(temp_output_file_name, '_html.txt'))

      } else {

        temp_decisions <- temp_decisions %>%
          mutate(`Date Decided` = as.Date(`Date Decided`, "%m/%d/%y"),
                 `Date Argued` = as.Date(`Date Argued`, "%m/%d/%y")) %>%
          dplyr::select(Case, `Date Argued`, `Date Decided`, `Lower Court`, Decision, Author, Coalition, Docket) %>%
          left_join(decisions %>%
                      dplyr::select(-any_of(names(t)), Docket), by = 'Docket') %>%
          arrange(`Date Decided`) %>%
          relocate(Docket, .after = last_col())

        write.csv(temp_decisions, file = paste0(temp_output_file_name, '.csv'))

      } # If HTML or CSV Output

      message('Completed Decisions Info For Cases ', min_case_id, ' to ', max_case_id)

    } # For Each Partition of Cases


  } # Recover Decisions Info for Each In decision_partitions

} # Decisions Table

###############################################################################
# Oral Argument Sittings
###############################################################################

print_oral_argument_sittings_template <- function(output_path = NA){

  default_oral_argument_sittings_template <- data.frame(
    "case" = character(),
    "date_argued" = character(),
    "docket" = character(),
    "petitioner_counsel" = character(),
    "respondent_counsel" = character(),
    "arguing_amici" = character(),
    "lower_court" = character(),
    "cert_amici" = character(),
    "merits_amici" = character(),
    stringsAsFactors = FALSE
  )


  if (is.na(output_path)){
    write.csv(default_oral_argument_sittings_template, file = 'code/templates/oral_argument_sittings_template.csv', row.names = F)
  }

}

#sitting_path <- 'data/oral_argument_sittings/input/2024/oral_argument_sitting_march.csv'
#output_path = 'data/oral_argument_sittings/output/2024'

oral_argument_sittings <- function(sitting_path,
                                   output_path,
                                   output_month = NULL){

  {

    sitting_file_type = case_when(
      grepl('.csv', sitting_path, ignore.case = T) ~ 'csv',
      grepl('.xlsx', sitting_path, ignore.case = T) ~ 'xlsx',
      grepl('(.rdata|.Rdata)', sitting_path, ignore.case = T) ~ 'rdata'
    )

    if (sitting_file_type == 'csv'){

      temp_csv <- read.csv(sitting_path, as.is = T)

    } else if (sitting_file_type %in% c('rdata', 'Rdata')){

      temp_csv <- get(load(sitting_file_path))

    } else if (sitting_file_type == 'xlxs'){

      temp_csv <- read_xlsx(path = sitting_path)

      } else {
      message('Incompatable File Type -- Please Upload as CSV, XLSX, or RData')
      break
    }

  } # Load Into R

  {

    output_month = ifelse(is.null(output_month), NA, output_month) # Set output Month (If Null - Name w/ No Identifier)
    temp_output_path_txt = file.path(output_path, ifelse(is.na(output_month), paste0('sitting', output_file_type), paste0(output_month, '_sitting.txt')))
    temp_output_path_html= file.path(output_path, ifelse(is.na(output_month), paste0('sitting', output_file_type), paste0(output_month, '_sitting.html')))


  } # Set Output Path

  {

    names(temp_csv) <- c('Case', 'Date Argued', 'Docket', 'Petitioner Counsel', 'Respondent Counsel', 'Arguing Amici', 'Lower Court', 'Cert Amici', 'Merits Amici')

    temp_csv <- temp_csv %>%
      mutate(across(everything(), ~ iconv(., from = "UTF-8", to = "UTF-8", sub = ""))) %>%
      mutate(across(everything(), as.character)) %>%
      kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
      column_spec(1, bold = TRUE, border_right = TRUE, width = "6cm") %>%  # Set the width of the first column
      column_spec(2:9, width = "3cm") %>%  # Set the width of columns 2 to 8
      row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
      row_spec(seq(1, nrow(temp_csv), 1), align = 'center') %>%
      kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed"))

    save_kable(temp_csv, temp_output_path_html) # Save as HTML

    temp_csv <- as.character(temp_csv)
    writeLines(temp_csv, temp_output_path_txt) # Save as TXT

  } # Recompile & Export


} # Recompile Sittings CSVs and Export as HTML & Txt (HTML Structure)



###############################################################################
# Oyez Scraper
###############################################################################


oyez_transcript_search <- function(docket = NULL, # Docket Number (Character, Req.)
                                   term = NULL,
                                   output_path = NULL,
                                   output_name = NULL){ # Term (Character, Req)

  {

    oyez_meta_search <- "

import pandas as pd
import requests
import oyez_api_wrapper
import json

def get_case_data(term, docket):
    temp_case_obj = oyez_api_wrapper.court_case(str(term), str(docket))
    temp_audio_links = temp_case_obj.get_audio_links()
    return temp_audio_links

term = {term}
docket = '{docket}'

case_data = get_case_data(term, docket)
"


  } # Oyez Meta Search (Python)

  check_dependencies_oyez <- function(){

    installed <- reticulate::py_list_packages() # Check Installed Py Packages

    install_if_needed <- function(packages) {

      for (pkg in packages) {
        if (!(pkg %in% installed$package)) {
          cat(paste("Installing", pkg, "...\n"))
          py_install(pkg)
        } else {
          next
        }
      }
    }

    packages_to_check <- c("pandas", "requests", "oyez-api-wrapper") # Check if Already Downloaded

    if (!all(packages_to_check %in% installed$package)){
      install_if_needed(packages_to_check) # Install If Needed
    }

  } # Check Dependencies

  check_venv <- function() {
    env_name <- "~/.virtualenvs/r-reticulate"

    if (!reticulate::py_available(initialize = FALSE)) {
      # Python not initialized yet — proceed to set up
      if (!reticulate::virtualenv_exists(env_name)) {
        reticulate::virtualenv_create(env_name)
        message("Virtual environment created.")
      }
      reticulate::use_virtualenv(env_name, required = TRUE)
      message("Virtual environment activated.")
    } else {
      # Python is already initialized; use the existing interpreter
      current_py <- reticulate::py_config()$python
      message("Python already initialized at: ", current_py)
    }
  }

  run_oyez_script <- function(term, docket) {

    python_script <- gsub("\\{term\\}", term, oyez_meta_search)
    python_script <- gsub("\\{docket\\}", docket, python_script)

    # Write the script to a temporary file
    temp_file <- tempfile(fileext = ".py")
    writeLines(python_script, temp_file)

    output <- reticulate::py_run_file(temp_file) # Run Script

    if (file.exists(temp_file)){
      file.remove(temp_file)
    } # Delete Temp Python File

    # Parse the JSON output
    case_data <- output$case_data # Retrieve Oral Argument Meta

    return(case_data) # Return Meta
  }

  retrieve_argument_transcript <- function(api_location, docket, term){


    {

      tryCatch({
        url <- api_location
        response <- httr::GET(url)

        if (status_code(response) == 200) {
          json_content <- httr::content(response, as = "text", encoding = "UTF-8")  # Specify the encoding
          parsed_json <- jsonlite::fromJSON(json_content)

        } else {
          stop(paste("Failed to retrieve JSON data from the URL with status code:", status_code(response)))
        }
      }, error = function(e) {
        cat("Error downloading file: ", json_output_file, "\n")
      }) # Get Json (parsed_json)


    } # Retrieve JSON

    {

      temp <- data.frame()

      for (j in 1:length(parsed_json$transcript$sections$turns)) {
        temp_text <- parsed_json$transcript$sections$turns[j]
        temp <- bind_rows(temp, temp_text)
      }

      temp_meta <- data.frame(
        case_name = parsed_json$transcript$title[1]
      )

      temp_complete <- data.frame()

      for (k in 1:nrow(temp)) {
        text_blocks <- temp$text_blocks[k]
        flattened_text <- paste(sapply(text_blocks, function (block) block$text), collapse = " ")

        temp_full <- data.frame(
          case_name = temp_meta$case_name,
          text_start = temp$start[k],
          text_stop = temp$stop[k],
          speaker = temp$speaker$name[k],
          role = ifelse(is.null(temp$speaker$roles[k][[1]]$role_title) && is.null(temp$speaker$name[k]), "NA",
                        ifelse(is.null(temp$speaker$roles[k][[1]]$role_title), "Attorney", temp$speaker$roles[k][[1]]$role_title)),
          text = flattened_text,
          word_count = str_count(flattened_text, "\\w+"),
          row_id = k,
          object_title = parsed_json$title
        )

        temp_complete <- bind_rows(temp_complete, temp_full)
      }


      temp_complete <- temp_complete %>%
        mutate(
          argument_duration = temp_meta$argument_duration,
          docket = docket,
        ) %>%
        relocate(case_name, docket) %>%
        mutate(role = ifelse(is.na(speaker), NA, role)) %>%
        mutate(role = ifelse(grepl('Justice', role, ignore.case = T), 'Justice', role))



    } # Convert to Rdata

    return(temp_complete)

  }


  check_venv() # Check Venv (Created & Active)
  check_dependencies_oyez() # Check if Modules Installed

  transcripts <- data.frame()

  for (temp_docket in 1:length(docket)) {

    argument_links <- tryCatch({
      run_oyez_script(term = term, docket = docket[temp_docket])
    }, error = function(e) {
      message("\033[97mERROR\033[0m: No Oral Argument Found For ", docket[temp_docket], " (", term, ")")
      return(NULL)
    })

    if (is.null(argument_links)) {
      next
    }

    for (i in 1:length(argument_links)) {
      temp_transcript <- retrieve_argument_transcript(api_location = argument_links[[i]], docket = docket[temp_docket], term = term)
      transcripts <- bind_rows(transcripts, temp_transcript)
    }


    message('Completed Transcript Compilation For ', docket[temp_docket], ' (', term, ')')

  }

  if (!is.null(output_path)){

    if (is.null(output_name)){

      temp_export_path = file.path(output_path, 'argument_transcripts.rdata')

    } else {

      temp_export_path = file.path(output_path, paste0(output_name, '.rdata'))

    }

    save(transcripts, file = temp_export_path)

  } # Export as Rdata (if Output Path Identified)


  return(transcripts)


} # Main Function - Returns Transcript Object

#temp_csv <- read.csv('data/oral_argument_sittings/input/2024/oral_argument_sitting_march.csv', as.is = T)
#unique_dockets <- as.character(unique(temp_csv$docket))
temp <- oyez_transcript_search(docket = unique_dockets,
                               term = '2024',
                               output_path = 'code/oral_argument_oyez/oral_arguments_processed/2024',
                               output_name = 'OT_24_March')


###############################################################################
# Compile Term-Level SCOTUS Transcripts Frame (Combined)
###############################################################################

combined_transcripts_term <- function(transcripts_folder){

  combined_transcript <- data.frame() # Empty DF to Store Output

  transcript_list <- list.files(transcripts_folder, full.names = T) # Get Files in Folder
  for (file in transcript_list) {
    loaded_data <- get(load(file))
    combined_transcript <- bind_rows(combined_transcript, loaded_data)
  } # Load each RData file and store the objects in the list

  {
    combined_transcript <- combined_transcript %>%
      mutate(speaker = case_when(
        .default = speaker,
        speaker == "John G. Roberts, Jr." & role == "Justice" ~ "ROBERTS",
        speaker == "Clarence Thomas" & role == "Justice" ~ 'THOMAS',
        speaker == 'Elena Kagan' & role == "Justice" ~ 'KAGAN',
        speaker == "Ketanji Brown Jackson" & role == "Justice" ~ 'JACKSON',
        speaker == "Amy Coney Barrett" & role == "Justice" ~ 'BARRETT',
        speaker == 'Samuel A. Alito, Jr.' & role == "Justice" ~ "ALITO",
        speaker == "Brett M. Kavanaugh" & role == "Justice" ~ "KAVANAUGH",
        speaker == "Neil M. Gorsuch" & role == "Justice" ~ 'GORSUCH',
        speaker == "Neil Gorsuch" & role == "Justice" ~ 'GORSUCH',
        speaker == "Sonia Sotomayor" & role == "Justice" ~ 'SOTOMAYOR'
      )) %>%
      rename(speaker_type = role) %>%
      mutate(case_name = gsub("(\\, Petitioner\\,|\\, Petitioners\\,|\\, Appellants\\,|\\, Appellant\\,| Petitioner\\,)", "", case_name)) %>%
      mutate(case_name = gsub("(\\, Respondent|\\, Respondents|\\, Appellees|\\, Appellee| Respondent\\,)", "", case_name)) %>%
      mutate(case_name = str_to_title(case_name)) %>%
      mutate(case_name = gsub("Llc", "LLC", case_name)) %>%
      mutate(case_name = gsub( " V.\\ ", " v. ", case_name))
  } # Processing

  return(combined_transcript)


}

test <- combined_transcripts_term(transcripts_folder = 'code/oral_argument_oyez/oral_arguments_processed/2024')





