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








