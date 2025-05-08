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
                'stringr', 'tidyr', 'tm', 'webshot2', 'purrr', 'tibble', 'httr', 'httr2', 'tokenizers', 'lubridate', 'scales')

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
{
  cases_master <- read.csv('Stat Reviews/OT24_StatReview/cases_master/cases_master_file_OT24.csv', as.is = T)
} # Cases Master File
{

  scdb_cases <- get(load('Stat Reviews/OT24_StatReview/decisions/data/scdb_cases_2024.rdata'))
  scdb_justices <- get(load('Stat Reviews/OT24_StatReview/decisions/data/scdb_justices_2024.rdata'))
  rm(list = ls(pattern = "^SCDB"))

} # SCDB Data

###############################################################################
# Decisions Table (HTML/CSV Output)
###############################################################################


decisions_analysis <- function(input_path,
                            output_path,
                            output_type = 'html',
                            cases_break = 15,
                            master_file = cases_master,
                            remove_existing_files = T,
                            current_term = '2024'){

  decisions_file_path = input_path

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
      } else if (value == 'RC-JRC'){
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
      } else if (value == 'D-JD'){
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

  if (remove_existing_files == T){
    invisible(do.call(file.remove, list(list.files(paste0(output_path, 'tables'), full.names = TRUE))))
    invisible(do.call(file.remove, list(list.files(paste0(output_path, 'figures'), full.names = TRUE))))

  } # Delete Existing Files

  decisions <- decisions %>%
    relocate(Docket, .after = 'Case') %>%
    mutate(Author = stringr::str_to_title(Author))

  decisions_figure_data <- decisions
  decisions_coalitions <- decisions

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
        . == 4 ~ 'RC-JRC',
        . == 5 ~ 'CJ',
        . == 6 ~ 'JCJ',
        . == 7 ~ 'SC',
        . == 8 ~ 'JSC',
        . == -1 ~ 'D',
        . == -2 ~ 'JD',
        . == -3 ~ 'D-JD',
        is.na(.) ~ 'DNP',
        TRUE ~ as.character(.)
      )))

    decisions$`Date Argued` <- format(decisions$`Date Argued`, "%m/%d/%y")
    decisions$`Date Decided` <- format(decisions$`Date Decided`, "%m/%d/%y")

    decisions <- decisions %>%
      mutate(across(everything(), ~str_replace_all(.x, " & ", "-")))

    } # Convert Values to DF

  {

    decisions_data <- decisions
    matching_columns <- intersect(colnames(decisions_data), names(justice_image_labels))

  } # Compile Column Names

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

    decisions_original <- decisions %>%
      ungroup() %>%
      mutate(case_id = row_number())

    decision_original_partitions <- split(decisions_original, ceiling(seq_len(nrow(decisions_original)) / cases_break)) # Split the dataframe into chunks of 10 rows


  } # Partition Into Smaller Files

  {

    for (i in 1:length(decision_partitions)){

      temp_decisions <- decision_partitions[[i]]
      min_case_id <- min(temp_decisions$case_id)
      max_case_id <- max(temp_decisions$case_id)
      temp_output_file_name <- file.path(output_path, 'tables', paste0('decision_info_', i))
      temp_decisions <- temp_decisions %>%
        dplyr::select(-c(case_id))

      if (output_type == 'html'){

        temp_decisions <- temp_decisions %>%
          select(c('Case', 'Docket', 'Date Argued', 'Date Decided', 'Lower Court', 'Decision', 'Author', 'Coalition')) %>%
          mutate(`Date Decided` = as.Date(`Date Decided`, "%m/%d/%y"),
                 `Date Argued` = as.Date(`Date Argued`, "%m/%d/%y")) %>%
          arrange(`Date Decided`) %>%
          kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
          column_spec(1, bold = TRUE) %>%
          row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
          row_spec(nrow(temp_decisions), extra_css = "border-bottom: 2px solid;") %>%
          kable_styling(font_size = 14, bootstrap_options = c("striped", "hover", "condensed", "responsive"))


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

  {

    for (i in 1:length(decision_partitions)){

      temp_decisions <- decision_partitions[[i]]
      min_case_id <- min(temp_decisions$case_id)
      max_case_id <- max(temp_decisions$case_id)
      temp_vote_matrix_path <- file.path(output_path, 'tables', paste0('decisions_vote_matrix_', i))
      original_column_names <- names(decisions)
      original_column_names = original_column_names[!original_column_names %in% c('Case', 'Date Argued', 'Date Decided', 'Lower Court', 'Decision', 'Docket', 'Coalition', 'Author')]
      original_column_names <- c(rep('', length = 2), original_column_names)
      temp_decisions <- temp_decisions %>%
        dplyr::select(-c(case_id, Author, `Date Argued`)) %>%
        mutate(`Date Decided` = as.Date(`Date Decided`, "%m/%d/%y")) %>%
        arrange(`Date Decided`) %>%
        relocate(Docket, .after = 'Coalition') %>%
        select(-c('Date Decided', 'Lower Court', 'Decision', 'Coalition'))

      {

        temp_vote_matrix <- temp_decisions %>%
          kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
          add_header_above(c(original_column_names)) %>%
          column_spec(1, width = "3cm", bold = TRUE, border_right = TRUE) %>%
          column_spec(2, width = '2cm', bold = TRUE, border_right = TRUE) %>%
          column_spec(2:ncol(temp_decisions), width = "1.25cm", border_right = TRUE, extra_css = "vertical-align: middle; font-size: 18px;") %>%
          row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
          row_spec(seq(1, nrow(temp_decisions), 1), align = 'center') %>%
          row_spec(nrow(temp_decisions), extra_css = "border-bottom: 2px solid;") %>%
          kable_styling(font_size = 14, bootstrap_options = c("striped", "hover", "responsive")) %>%
          add_footnote(
            c(
              "<span style=\"border-radius: 0px; padding: 1px; background-color: white !important; color: white;\"></span>",
              "<span style=\"border-radius: 0px; padding: 0px; background-color: white !important; color: white;\"></span>",

              "<span style=\"margin-left: 10px;\"></span>
        <span style=\"border-radius: 1px; padding: 1px; background-color: darkolivegreen !important; color: white;\">M*</span> = Majority Author
         <span style=\"margin-left: 10px;\"></span>
         <span style=\"border-radius: 1px; padding: 1px; background-color: #99CCFF !important; color: white;\">M</span> = Joined Majority
         <span style=\"margin-left: 10px;\"></span>
         <span style=\"border-radius: 1px; padding: 1px; background-color: #66B2FF !important; color: white;\">RC</span> = Wrote Concurrence
         <span style=\"margin-left: 10px;\"></span>
         <span style=\"border-radius: 1px; padding: 1px; background-color: #3399FF !important; color: white;\">JRC</span> = Joined Concurrence
         <span style=\"margin-left: 10px;\"></span>
         <span style=\"border-radius: 1px; padding: 1px; background-color: #66B2FF !important; color: white;\">RC-JRC</span> = Wrote & Joined Concurrence",

              "<span style=\"border-radius: 3px; padding: 1px; background-color: white !important; color: white;\"> </span>",

              "<span style=\"margin-left: 10px;\"></span>
        <span style=\"border-radius: 1px; padding: 1px; background-color: #FF9933 !important; color: white;\">CJ</span> = Wrote Concurrence In Judgement
         <span style=\"margin-left: 10px;\"></span>
        <span style=\"border-radius: 1px; padding: 1px; background-color: #FFCC99 !important; color: white;\">JCJ</span> = Joined Concurrence In Judgement
         <span style=\"margin-left: 10px;\"></span>
        <span style=\"border-radius: 1px; padding: 1px; background-color: #B265FF !important; color: white;\">SC</span> = Wrote Special Concurrence
         <span style=\"margin-left: 10px;\"></span>
        <span style=\"border-radius: 1px; padding: 1px; background-color: #6600CC !important; color: white;\">JSC</span> = Joined Special Concurrence
         <span style=\"margin-left: 10px;\"></span>",

              "<span style=\"border-radius: 3px; padding: 1px; background-color: white !important; color: white;\"> </span>",

              "<span style=\"margin-left: 10px;\"></span>
        <span style=\"border-radius: 1px; padding: 1px; background-color: #FF3333 !important; color: white;\">D</span> = Wrote Dissent
         <span style=\"margin-left: 10px;\"></span>
        <span style=\"border-radius: 1px; padding: 1px; background-color: #CC0000 !important; color: white;\">JD</span> = Joined Dissent
         <span style=\"margin-left: 10px;\"></span>
        <span style=\"border-radius: 1px; padding: 1px; background-color: #990000 !important; color: white;\">D-JD</span> = Wrote & Joined Dissent
         <span style=\"margin-left: 10px;\"></span>"


            ),
            notation = "none",
            escape = FALSE  # Add escape parameter to allow HTML formatting
          )

        } # Compile Temp Vote Matrix

      {

        save_kable(temp_vote_matrix, file = paste0(temp_vote_matrix_path, '.html'))
        temp_vote_matrix <- as.character(temp_vote_matrix)
        writeLines(temp_vote_matrix, paste0(temp_vote_matrix_path, '_html.txt'))
        message('Completed Decisions Vote Matrix For Cases ', min_case_id, ' to ', max_case_id)


      } # Export

    }



  } #Compile Vote Matrix Table

  {

    for (i in 1:length(decision_original_partitions)){

      temp_decisions <- decision_original_partitions[[i]]
      temp_information_path <- file.path(output_path, 'tables', paste0('decisions_information_', i, '.csv'))

      temp_decisions <- temp_decisions %>%
        dplyr::select(Case, Docket, 'Date Argued', 'Date Decided', 'Lower Court', Decision, Author, Coalition) %>%
        left_join(master_file %>%
                    select(case, docket) %>%
                    rename(case_short = case,
                           Docket = docket), by = 'Docket') %>%
        left_join(master_file %>%
                    select(docket, consolidated_case_1, consolidated_docket_1, consolidated_lower_court_1) %>%
                    mutate(across(everything(), ~replace_na(.x, ' '))) %>%
                    rename(Docket = docket), by = 'Docket') %>%
        mutate(Case = ifelse(is.na(case_short), Case, case_short)) %>%
        select(-c(case_short)) %>%
        unique() %>%
        mutate(Case = gsub('\\&', 'and', Case),
               Case = gsub('\\,', '', Case)) %>%
        relocate(Case) %>%
        arrange(`Date Decided`) %>%
        mutate(across(everything(), ~replace_na(.x, ' '))) %>%
        mutate(Docket = ifelse(!consolidated_docket_1 %in% c('', ' '), paste0(Docket, ' (', consolidated_docket_1, ')'), Docket),
               'Lower Court' = ifelse(!`Lower Court` %in% c('', ' '), paste0(`Lower Court`, ' (', consolidated_lower_court_1, ')'), `Lower Court`),
               'Lower Court' = gsub(' \\(\\)', '', `Lower Court`),
               'Lower Court' = gsub(' \\( \\)', '', `Lower Court`)) %>%
        select(-c(tidyr::starts_with('consolidated_')))


      {

        write.csv(temp_decisions, file = temp_information_path, row.names = FALSE, quote = FALSE)


        } # Export

    }

    message('Completed Decision Info Summary CSV Export')


  } #Compile Decisions Info Table Summary CSV

  {

    for (i in 1:length(decision_original_partitions)){

      temp_decisions <- decision_original_partitions[[i]]
      temp_decision_path <- file.path(output_path, 'tables', paste0('decisions_vote_matrix_', i, '.csv'))

      temp_decisions <- temp_decisions %>%
        dplyr::select(-c('Date Argued', 'Lower Court', 'Decision', 'Coalition')) %>%
        left_join(master_file %>%
                    select(case, docket) %>%
                    rename(case_short = case,
                           Docket = docket), by = 'Docket', relationship = 'many-to-many') %>%
        mutate(Case = ifelse(is.na(case_short), Case, case_short)) %>%
        select(-c(case_short)) %>%
        unique() %>%
        mutate(Case = gsub('\\&', 'and', Case),
               Case = gsub('\\,', '', Case)) %>%
        relocate(Case) %>%
        arrange(`Date Decided`) %>%
        mutate(across(everything(), ~replace_na(.x, ' '))) %>%
        select(-c('Date Decided', case_id))


      {

        write.csv(temp_decisions, file = temp_decision_path, row.names = FALSE, quote = FALSE)


        } # Export

    }

    message('Completed Decision Vote Matrix CSV Export')


  } #Compile Decisions Vote Matrix Summary CSV

  {

    {

      agreement_binary <- decisions_figure_data %>%
        select(all_of(c('ROBERTS', 'THOMAS', 'ALITO', 'SOTOMAYOR', 'KAGAN', 'GORSUCH', 'KAVANAUGH', 'BARRETT', 'JACKSON'))) %>%
        mutate(across(everything(), ~ifelse(.x > 0, 1, 0)))

      justice_pairs <- expand.grid(names(agreement_binary), names(agreement_binary), stringsAsFactors = FALSE)

      agreement_df <- justice_pairs %>%
        rename(j1 = Var1, j2 = Var2) %>%
        mutate(agreement = map2_dbl(j1, j2, ~{
          v1 <- agreement_binary[[.x]]
          v2 <- agreement_binary[[.y]]
          valid <- !is.na(v1) & !is.na(v2)
          mean(v1[valid] == v2[valid], na.rm = TRUE)
        })) %>%
        mutate(agreement = round(agreement, 2))

      agreement_matrix <- agreement_df %>%
        pivot_wider(names_from = j2, values_from = agreement) %>%
        column_to_rownames("j1")  # Compute agreement for each pair

      agreement_matrix[upper.tri(agreement_matrix, diag = TRUE)] <- ' '
      agreement_matrix <- agreement_matrix[c(-1),c(-ncol(agreement_matrix))]

      agreement_long <- agreement_matrix %>%
        as.data.frame() %>%
        rownames_to_column("Justice1") %>%
        pivot_longer(cols = -Justice1, names_to = "Justice2", values_to = "Agreement") %>%
        mutate(Agreement = as.numeric(Agreement))


      suppressWarnings(agreement_long <- agreement_long %>%
        mutate(Justice1 = factor(Justice1, levels = c('ROBERTS', 'THOMAS', 'ALITO', 'SOTOMAYOR', 'KAGAN', 'GORSUCH', 'KAVANAUGH', 'BARRETT', 'JACKSON')),
               Justice2 = factor(Justice2, levels = c('ROBERTS', 'THOMAS', 'ALITO', 'SOTOMAYOR', 'KAGAN', 'GORSUCH', 'KAVANAUGH', 'BARRETT', 'JACKSON'))) %>%
        mutate(Justice1_label = justice_image_labels[Justice1],
               Justice2_label = justice_image_labels[Justice2]) %>%
          filter(!is.na(Agreement))

        )


    } # Construct Agreement Matrix (Regular & Colored)

    {

      Justice2_labels <- agreement_long %>%
        select(Justice2) %>%
        rename(justice = Justice2) %>%
        mutate(image_labels = justice_image_labels[match(justice, names(justice_image_labels))],
               image_labels = gsub(' style\\=.*', '', image_labels),
               image_labels = paste0(image_labels, " width='100' /><br><strong>", justice, "</strong>")) %>%
        unique() %>%
        mutate(image_labels = gsub('75px', '50px', image_labels)) %>%
        pull(image_labels)

      Justice1_labels <- agreement_long %>%
        select(Justice1) %>%
        rename(justice = Justice1) %>%
        mutate(image_labels = justice_image_labels[match(justice, names(justice_image_labels))],
               image_labels = gsub(' style\\=.*', '', image_labels),
               image_labels = paste0(image_labels, " width='100' /><br><strong>", justice, "</strong>")) %>%
        unique() %>%
        mutate(image_labels = gsub('75px', '50px', image_labels)) %>%
        pull(image_labels)

    } # Justice Image Labels

    {

      suppressWarnings(justice_agreement_matrix_figure <- ggplot(data = agreement_long, aes(x = Justice1, y = Justice2)) +
                         geom_tile(color = "white", size = 0.5, aes(fill = Agreement)) +
                         geom_label(aes(label = Agreement), fill = 'white', size = 5) +
                         scale_fill_gradient(low = "coral4", high = "deepskyblue3", na.value = "white") +  # Adjust colors
                         theme_minimal() +
                         scale_x_discrete(labels = Justice1_labels) +  # Use the labels with images for the x-axis
                         scale_y_discrete(labels = Justice2_labels) +  # Use the labels with images for the y-axis
                         labs(x = '',
                              y = '',
                              fill = '') +
                         theme(
                           axis.text.x = ggtext::element_markdown(),  # This allows HTML rendering (for image tags) on the x-axis
                           axis.text.y = ggtext::element_markdown(hjust = 0.5),  # Center the y-axis text under the images
                           panel.grid = element_blank(),
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
                           plot.subtitle = element_text(size = 15, hjust = 0.5)
                         ))

    } # Render Figure

    {

      temp_output_path <- file.path(output_path, 'figures', 'justice_agreement_matrix.png')
      suppressWarnings(ggsave(filename = temp_output_path,
                              justice_agreement_matrix_figure,
                              width = 8,
                              height = 10,
                              units = 'in',
                              bg = 'white'))

    } # Export

    message('Completed Agreement Matrix Figure Export')


  } # Vote Agreement Matrix Figure

  {

    {

      coalitions <- decisions_coalitions %>%
        select(Docket, Coalition) %>%
        mutate(Coalition = gsub('Per Curiam', '(9-0)', Coalition),
               Coalition = gsub('\\(', '', Coalition),
               Coalition = gsub('\\).*', '', Coalition)) %>%
        rowwise() %>%
        mutate(maj_votes = unlist(stringr::str_split(Coalition, pattern = '-'))[1],
               min_votes = unlist(stringr::str_split(Coalition, pattern = '-'))[2])


      coalitions_current_term <- coalitions %>%
        group_by(Coalition) %>%
        summarise(count = n(), .groups = 'drop') %>%
        mutate(Coalition = paste0('(', Coalition, ')')) %>%
        ggplot(aes(x = Coalition, y = count)) +
        geom_col(colour = 'black', fill = 'gray50') +
        scale_y_continuous(breaks = seq(5, 30, 5)) +
        geom_hline(yintercept = 0) +
        labs(x = '\nCoalition',
             y = 'Number of Decisions\n') +
        geom_label(aes(label = count), size = 6, vjust = 1) +
        theme_minimal() +
        theme(panel.border = element_rect(size = 1, colour = 'black', fill = NA),
              axis.text = element_text(size = 14, colour = 'black'),
              axis.title = element_text(size = 16, colour = 'black'))

    } # Configure Figure

    {

      temp_export_path <- file.path(output_path, 'figures', 'coalitions_current_term.png')
      ggsave(temp_export_path,
             coalitions_current_term,
             width = 8,
             height = 8,
             units = 'in',
             bg = 'white')

    } # Export

    message('Completed Coalitions (Current Term) Figure')

  } # Coalitions Current Term

  {

    {

      coalitions_current_term <- decisions_coalitions %>%
        select(Docket, Coalition) %>%
        mutate(Coalition = gsub('Per Curiam', '(9-0)', Coalition),
               Coalition = gsub('\\(', '', Coalition),
               Coalition = gsub('\\).*', '', Coalition)) %>%
        rowwise() %>%
        mutate(maj_votes = unlist(stringr::str_split(Coalition, pattern = '-'))[1],
               min_votes = unlist(stringr::str_split(Coalition, pattern = '-'))[2]) %>%
        ungroup() %>%
        mutate(term = as.integer('2024'),
               Coalition = paste0('(', Coalition, ')')) %>%
        select(Coalition, term)

      coalitions_longitudinal_figure <- scdb_cases %>%
        filter(term >= 2021) %>%
        select(majVotes, minVotes, term, docket) %>%
        rename(min_votes = minVotes,
               maj_votes = majVotes) %>%
        mutate(Coalition = paste0('(', maj_votes, '-', min_votes, ')')) %>%
        select(Coalition, term) %>%
        bind_rows(coalitions_current_term) %>%
        ungroup() %>%
        mutate(Coalition =  case_when(
          .default = Coalition,
          Coalition %in% c('(4-4') ~ '(4-4)',
          Coalition %in% c('(5-4)') ~ '(5-4)',
          Coalition %in% c('(6-2)', '(6-3)') ~ '(6-3)\n(6-2)',
          Coalition %in% c('(7-2)') ~ '(7-2)',
          Coalition %in% c('(8-0)', '(8-1)') ~ '(8-1)\n(8-0)',
          Coalition %in% c('(9-0)') ~ '(9-0)'
        )) %>%
        group_by(term, Coalition) %>%
        summarise(count = n(), .groups = "drop") %>%
        ungroup() %>%
        mutate(term = paste0(term, ' Term')) %>%
        ggplot(aes(x = Coalition, y = count)) +
        facet_wrap(~term, nrow = 4) +
        geom_col(colour = 'black', fill = 'gray45') +
        scale_y_continuous(breaks = seq(10, 30, 10), lim = c(0, 40)) +
        geom_hline(yintercept = 0) +
        labs(x = '\nCoalition',
             y = 'Number of Decisions\n') +
        geom_label(aes(label = count), size = 4, vjust = -0.5) +
        theme_minimal() +
        theme(panel.border = element_rect(size = 1, colour = 'black', fill = NA),
              axis.text = element_text(size = 14, colour = 'black'),
              axis.title = element_text(size = 16, colour = 'black'),
              strip.text = element_text(size = 14, colour = 'black'),
              strip.background = element_rect(size = 1, colour = 'black', fill = 'gray75'))

    } # Coalitions (Comparison) Figure

    {

      temp_export_path <- file.path(output_path, 'figures', 'coalitions_longitudinal_figure.png')
      ggsave(temp_export_path,
             coalitions_longitudinal_figure,
             height = 10,
             width = 8,
             units = 'in',
             bg = 'white')

      } # Export

    message('Completed Coalitions (Longitudinal Comparison) Figure')


  } # Coalitions (Versus Previous 5 Terms)

  {

    {

      unanimity_figure <- scdb_cases %>%
        filter(term >= 2021) %>%
        select(majVotes, minVotes, term, docket) %>%
        rename(min_votes = minVotes,
               maj_votes = majVotes) %>%
        select(min_votes, term) %>%
        bind_rows(decisions_coalitions %>%
                    select(Docket, Coalition) %>%
                    mutate(Coalition = gsub('Per Curiam', '(9-0)', Coalition),
                           Coalition = gsub('\\(', '', Coalition),
                           Coalition = gsub('\\).*', '', Coalition)) %>%
                    rowwise() %>%
                    mutate(maj_votes = unlist(stringr::str_split(Coalition, pattern = '-'))[1],
                           min_votes = unlist(stringr::str_split(Coalition, pattern = '-'))[2]) %>%
                    mutate(term = as.integer('2024'),
                           Coalition = paste0('(', Coalition, ')'),
                           min_votes = as.numeric(min_votes)) %>%
                    select(term, min_votes)) %>%
        group_by(min_votes, term) %>%
        summarise(count = n(), .groups = 'drop') %>%
        ungroup() %>%
        group_by(term) %>%
        mutate(total_term_cases = sum(count)) %>%
        ungroup() %>%
        filter(min_votes == 0) %>%
        rowwise() %>%
        mutate(count_non = total_term_cases - count) %>%
        select(term, count, count_non, total_term_cases) %>%
        pivot_longer(cols = c(count, count_non),
                     names_to = "decision_type",
                     values_to = "n_cases") %>%
        mutate(decision_type = recode(decision_type,
                                      count = "Unanimous",
                                      count_non = "Non-unanimous"),
               term = paste0(term, " Term (", total_term_cases, ' Cases)')) %>%
        group_by(term) %>%
        mutate(
          total_cases = sum(n_cases),
          percent = (n_cases / total_cases) * 100,
          ypos = cumsum(percent) - 0.5 * percent,
          label = paste0("(", round(percent), "%)")
        ) %>%
        ungroup() %>%
        ggplot(aes(x = "", y = percent, fill = decision_type)) +
        geom_col(width = 2, color = "black", size = 1) +
        coord_polar(theta = "y") +
        facet_wrap(~ term, nrow = 3) +
        scale_fill_manual(values = c('coral2', 'deepskyblue3')) +
        geom_label(aes(y = ypos, label = label), fill = 'white', size = 5, show.legend = FALSE) +
        labs(fill = "") +
        theme_void() +
        theme(
          panel.border = element_rect(size = 1, colour = 'black', fill = NA),
          legend.position = 'bottom',
          legend.text = element_text(size = 14, colour = 'black'),
          legend.box.background = element_rect(size = 1, colour = 'black'),
          legend.box.margin = margin(t = 10, r = 10, b = 10, l = 10),
          strip.text = element_text(size = 14, color = 'black', margin = margin(t = 6, b = 6)),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray75')
        )



    } # Unanimity Pie Charts

    {

      temp_export_path <- file.path(output_path, 'figures', 'unanimity_figure.png')
      ggsave(temp_export_path,
             unanimity_figure,
             height = 10,
             width = 8,
             units = 'in',
             bg = 'white')

    } # Export

    message('Completed Unanimity Comparison Figure')

  } # Unanimity over Time

  {

    {

      lower_courts <- decisions_coalitions %>%
        select(Lower_Court, Decision, Docket) %>%
        rename(lower_court = Lower_Court,
               decision = Decision,
               docket = Docket) %>%
        left_join(cases_master %>%
                    select(docket, starts_with('consolidated')), by = 'docket')


      combined_lower_courts <- lower_courts %>%
        select(lower_court, decision) %>%
        mutate(type = 'main') %>%
        bind_rows(data.frame(lower_court = lower_courts$consolidated_lower_court_1,
                             decision = lower_courts$decision,
                             type = 'consolidated')) %>%
        filter(!is.na(lower_court)) %>%
        filter(!lower_court == '') %>%
        filter(grepl('^CA', lower_court)) %>%
        mutate(lower_court = factor(lower_court, levels = c('CA1', 'CA2', 'CA3', 'CA4', 'CA5', 'CA6', 'CA7', 'CA8', 'CA9', 'CA10', 'CA11', 'CADC', 'CAFC')))

    } # Recover Data

    {

      circuit_scorecard_main <- combined_lower_courts %>%
        filter(type == 'main') %>%
        group_by(lower_court, decision) %>%
        summarise(count = n(), .groups = "drop") %>%
        pivot_wider(names_from = decision, values_from = count, values_fill = 0) %>%
        mutate(across(-lower_court, ~ ifelse(. == 0, "", .)))

      circuit_scorecard_with_consolidated <- combined_lower_courts %>%
        group_by(lower_court, decision) %>%
        summarise(count = n(), .groups = "drop") %>%
        pivot_wider(names_from = decision, values_from = count, values_fill = 0) %>%
        mutate(across(-lower_court, ~ ifelse(. == 0, "", .)))


    } # 2 Tables -- One w/ & w/out Consolidated

    {

      temp_export_path <- file.path(output_path, 'tables', 'circuit_scorecard_main.csv')
      write.csv(circuit_scorecard_main, file = temp_export_path, row.names = F, quote = F)

      temp_export_path <- file.path(output_path, 'tables', 'circuit_scorecard_with_consolidated.csv')
      write.csv(circuit_scorecard_with_consolidated, file = temp_export_path, row.names = F, quote = T)


    } # Export

    message('Completed Circuit Scorecards')

  } # Circuit Scorecard

} # Decisions Tables (Info + Vote Matrix)


###############################################################################
# Justia Opinions Recovery
###############################################################################

justia_opinion_recovery <- function(opinions_path,
                                    current_term,
                                    master_file){

  {

    terms = c(current_term, current_term + 1) # Justia Indexes by Year
    term_cases_combined <- data.frame() # Empty DF

    for (term in 1:length(terms)) {

      tryCatch({

        url <- paste0('https://supreme.justia.com/cases/federal/us/year/', terms[term], '.html') # Create Term-Level Link
        source <- tryCatch(
          read_html(url),
          error = function(e) return(NULL)
        )

        if (is.null(source)) next # Skip to next term if read_html failed

        elements <- source %>%
          html_elements("div.has-padding-content-block-30.-zb.search-result") # Retrieve Case Entries

        term_cases <- data.frame() # Empty DF to Store Case-Level Entries for Term[Term]

        for (element in 1:length(elements)) {

          page <- elements[element] # Get Case Entry

          case_name <- page %>%
            html_nodes(".case-name strong") %>%
            html_text(trim = TRUE) # Case Name

          embedded_link <- page %>%
            html_nodes(".color-green a") %>%
            html_attr("href") # Link to Justia Entry

          key_value_pairs <- page %>%
            html_nodes("strong") %>%
            html_text(trim = TRUE) # US Report Citation, Docket Number, Court, Date, etc.
          key_value_pairs <- key_value_pairs[!key_value_pairs %in% case_name] # Remove Case Name (Duplicate)

          values <- page %>%
            html_nodes("strong") %>%
            html_nodes(xpath = "following-sibling::text()[normalize-space()]") %>%
            html_text(trim = TRUE) # Associated Values to Keys

          case_info <- data.frame(key = key_value_pairs, value = values) %>%
            add_row(key = 'Link', value = embedded_link) %>%
            add_row(key = 'Case_Name', value = case_name) %>%
            mutate(
              key = gsub('\\:', '', key),
              key = gsub(' ', '_', key),
              key = tolower(key)
            ) %>%
            tidyr::pivot_wider(names_from = 'key') # Pivot Wider So Keys Become Columns

          term_cases <- bind_rows(term_cases, case_info)
        }

        term_cases_combined <- bind_rows(term_cases_combined, term_cases)

      }, error = function(e) {
        # silently skip this term on error
        next
      })
    } # Recover Justia Links for Term

    {

      term_year <- terms[1]  # Set Current Term
      cutoff_date <- as.Date(paste0(term_year, "-10-01"))
      term_cases_combined <- term_cases_combined %>%
        mutate(date = as.Date(date, format = '%B %d, %Y')) %>%
        filter(date > cutoff_date)

      completed_dockets <- list.files(file.path(opinions_path, 'opinions', 'opinions_processed')) # Dockets Already Completed
      completed_dockets <- gsub('\\.rdata', '', completed_dockets)

      term_cases_combined <- term_cases_combined %>%
        filter(!docket_number %in% c(completed_dockets)) # Filter Out

    } # Filter to Current Term

    if (nrow(term_cases_combined) == 0){
      message('Every Opinion Already Processed')

    } else {

      message('Located ', nrow(term_cases_combined), ' Decisions to Process...')

      {

        justices <- scdb_justices %>%
          mutate(justice = sub(".*([A-Z][a-zA-Z]*).*$", "\\1", justiceName)) %>%
          select(justice) %>%
          unique() %>%
          pull(justice)

      } # Unique Justices

      for (case in 1:nrow(term_cases_combined)){

        temp_case <- term_cases_combined[case,]
        temp_case_link <- paste0('https://supreme.justia.com/', temp_case$link) # Compile Justia Link
        case_source_temp <- read_html(temp_case_link) #Retrieve Case Source

        {

          opinions_list <- case_source_temp %>%
            html_nodes("#opinions-list") %>%
            html_nodes("li") # List of Opinions for Case

          href_values <- opinions_list %>%
            html_nodes("a") %>%
            html_attr("href")  # Retrieve Justia ID #'s

          title_values <- opinions_list %>%
            html_nodes("a") %>%
            html_text() %>%
            gsub("\\t+", " ", .) # Retrieve Opinion Type (Author)

          temp_opinions <- data.frame(opinion = title_values,
                                      justice = gsub('.*\\(', '', gsub('\\)', '',  title_values)),
                                      opinion_type = trimws(gsub('\\(.*', '', title_values)),
                                      justia_id = href_values,
                                      updated_justia_link = paste0( temp_case_link, href_values)) # Combine Into DF

          temp_opinions <- temp_opinions %>%
            group_by(opinion_type) %>%
            mutate(opinion_count = row_number()) %>%
            mutate(opinion_type = ifelse(opinion_type == 'Opinion' & opinion_count >= 2, 'Special Concurrence', opinion_type)) %>%
            select(-c(opinion_count)) # Fix -- Justia Counts In Part Concurrences as Opinions

        } # Get Opinions List - Href Values - Title Values -- Recover Temp Opinions Before Processing

        unique_justices <- if (any(temp_opinions$justice == "Per Curiam")) {
          justices
        } else {
          unique(temp_opinions$justice)
        }


        combined_opinions <- data.frame()

        {

          for (opinion in 1:nrow(temp_opinions)){

            temp_opinion <- temp_opinions[opinion,] # Get Temp Opinion
            temp_opinion_link <- temp_opinion$updated_justia_link # Opinion-Level Link
            response <- GET(temp_case_link) # Get Full Source

            {

              raw_html <- tryCatch({
                # First attempt using httr's content()
                response <- GET(temp_case_link)
                content(response, as = "text", encoding = "UTF-8")
              }, error = function(e) {
                # If the first method fails, fall back to httr2
                response2 <- request(temp_case_link) |> req_perform()
                resp_body_string(response2)
              }) # content() fails -- try httr2 request & resp_body_string


              parsed_html <- read_html(raw_html) # Parse to R
              opinion_element_temp <- html_nodes(parsed_html, paste0('#tab-opinion ', temp_opinion$justia_id)) # Reduce to Opinion Element (Justia ID)
              opinion_element_temp <- html_text(opinion_element_temp)

              blocks <- strsplit(as.character(opinion_element_temp), split = '(\\t{5}|\\n\\n)') # Split by &nbsp line break notation
              html_elements_to_toss <- c('<em>', '</em>', '<p>', '</p>', '<a>', '</a>', '</font>', '(<div|<div>)', '</div>', '<br>', '<span>', '</span>', '<strong>', '</strong>') # Regex for Tossing HTML Encoding

            } # Process Raw HTML

            {

              oar_1 <- paste0(
                "JUSTICE ", toupper(unique_justices), " ",
                rep(c(", dissent", ", concur", "delivered", ", in which", " would ", ' dissents', ", with whom"), each = length(unique_justices)))
              oar_2 <- paste0(
                "MR. JUSTICE ", toupper(unique_justices), ".$")
              oar_3 <- paste0(
                "MS. JUSTICE ", toupper(unique_justices), ".$")
              oar_4 <- paste0(
                "MS. CHIEF JUSTICE ", toupper(unique_justices), ".$")
              oar_5 <- paste0(
                "MS. CHIEF JUSTICE ", toupper(unique_justices), ".$")
              oar_6 <- paste0('^PER CURIAM.$')
              oar_7 <- paste0(
                "MR. JUSTICE ", toupper(unique_justices), " ",
                rep(c(", dissent", ", concur", "delivered", ", in which", " would ", ' dissents'), each = length(unique_justices)))
              oar_8 <- paste0(
                "MS. JUSTICE ", toupper(unique_justices), " ",
                rep(c(", dissent", ", concur", "delivered", ", in which", " would ", ' dissents'), each = length(unique_justices)))
              oar_9 <- paste0("MR. JUSTICE ", toupper(unique_justices), ".*? delivered")
              oar_10 <- paste0("MS. JUSTICE ", toupper(unique_justices), ".*? delivered")
              oar_11 <- paste0("MR. CHIEF JUSTICE ", toupper(unique_justices), ".*? delivered")

              opinion_authors_regex <- c(oar_1, oar_2, oar_3, oar_4, oar_5, oar_6, oar_7, oar_8, oar_9, oar_10, oar_11)
              opinion_authors_regex <- gsub(' \\,', ',', opinion_authors_regex)
              opinion_authors_regex <- gsub('  ', ' ', opinion_authors_regex)

            } # Regex to Identify Opinions w/out Justia Demarcation

            {

              if (any(grepl('court\\:', unlist(blocks)))){
                opinion_text_temp <- data.frame(text = unlist(blocks)) %>%
                  mutate(
                    text_split = str_split(text, "(?<=court:)", n = 2)
                  ) %>%
                  select(-text) %>%  # Remove original text column to avoid name collision
                  tidyr::unnest_wider(text_split, names_sep = "_") %>%
                  rename(text_1 = text_split_1, text_2 = text_split_2) %>%
                  tidyr::pivot_longer(cols = starts_with("text_"), names_to = NULL, values_to = "text") %>%
                  filter(!is.na(text))
              } else {
                opinion_text_temp <- data.frame(text = unlist(blocks))
              }

              opinion_text_temp <- opinion_text_temp %>%
                mutate(text = gsub('\\s{2,}', ' ', trimws(text)))  %>%
                filter(!text == '') %>%
                filter(!text == ' ') %>% # Remove Blank Rows
                mutate(text = gsub(paste(html_elements_to_toss, collapse = '|'), '', text)) %>%
                mutate(text = trimws(text)) %>%  # Trim WS
                mutate(total_letters = str_count(text, '[a-zA-Z]')) %>% # Get Count of Letters in Row Strings
                filter(total_letters > 1) %>%  # Toss Where Letters <= 1 (Usually * * * to denote footnotes)
                mutate(text = ifelse(!is.na(text), gsub('^[^a-zA-Z]*', '', text), text),
                       text = gsub('\\n', ' ', text)) %>%
                mutate(toss = case_when(
                  .default = 0,
                  grepl(paste0('(I$|II$|III$|IV$|V$|VI$|VII$|VIII$)'), text, ignore.case = T) ~ 1,
                  text == c('^</div>') ~ 1,
                  grepl('^<a name=', text) ~ 1,
                  grepl('^class=', text) ~ 1,
                  grepl('^id=', text) ~ 1,
                  row_number() == 1 ~ 1)) %>% # Remove Section Indicators & Div Element Encodings # Replace 2+ Spaces W/ One
                filter(!toss == 1) %>%  # Toss Where Toss = 1
                mutate(
                  test = ifelse(
                    grepl(paste0("(?s)", paste(opinion_authors_regex, collapse = '|')),
                          text,
                          ignore.case = TRUE,
                          perl = TRUE),
                    1, 0
                  ),
                  tokenized = tokenizers::tokenize_words(text),
                  opinion_author = ifelse(
                    test == 1,
                    sapply(tokenized, function(tokens) {
                      match <- intersect(toupper(tokens), toupper(unique_justices))
                      if (length(match) > 0) match[1] else NA
                    }),
                    NA
                  )
                ) %>%
                relocate(opinion_author) %>%
                mutate(opinion_author = ifelse(grepl('^Per Curiam', text, ignore.case = T), 'Per Curiam', opinion_author)) %>%
                tidyr::fill(opinion_author, .direction = 'down') %>%
                filter(!grepl("Page .*? U\\. S\\.", text, ignore.case = T))

              if (all(is.na(opinion_text_temp$opinion_author))){
                opinion_text_temp$opinion_author <- 'Unknown'
              } else {
                opinion_text_temp <- opinion_text_temp %>%
                  filter(!is.na(opinion_author)) %>%
                  mutate(opinion_author = stringr::str_to_title(opinion_author))
                opinion_text_temp <- opinion_text_temp[-1,]
              } # IF All NA (Indicating Per Curiam or Something?) -- Replace


            } # Process Text & Identify Author

            {

              individual_opinions <- list()
              individual_authors <- unique(opinion_text_temp$opinion_author)

              for (individual_author in 1:length(individual_authors)){

                temp_author_opinion <- opinion_text_temp %>%
                  filter(opinion_author == individual_authors[individual_author]) %>%
                  select(-c(opinion_author))

                individual_opinions[[as.character(individual_authors[individual_author])]] <- temp_author_opinion


              }

            } # Demarcate and Separate -- Return as 'individual_opinions'

            {

              for (author in 1:length(individual_opinions)){

                temp_authored_opinion <- individual_opinions[[author]] %>%
                  mutate(text =  gsub("\\[\\s*Footnote\\s*\\d+(/\\d+)?\\s*\\]", "", text))
                temp_authorship_retrieval <- names(individual_opinions)[author]

                opinion_text_cleaned <- paste(trimws(gsub('\\n', ' ', temp_authored_opinion$text)), collapse = ' ') # Retrieve Cleaned Opinion

                combined_opinions <- bind_rows(combined_opinions, data.frame(authorship = temp_opinion$justice,
                                                                             opinion_type = temp_opinion$opinion_type,
                                                                             opinion_text = opinion_text_cleaned, # Use I() to ensure list storage
                                                                             stringsAsFactors = FALSE))

              }


            } # Separate Individual Opinions & Recombine as DF

          }


        } # For Each Opinion -- Recover & Append

        {

          combined_opinions <- combined_opinions %>%
            mutate(docket = term_cases_combined[case,]$docket_number,
                   date = term_cases_combined[case,]$date,
                   case = term_cases_combined[case,]$case_name,
                   justia_summary = term_cases_combined[case,]$justia_opinion_summary)

        } # Assign Meta

        {

          temp_export_path = file.path(opinions_path, 'opinions', 'opinions_processed', paste0(term_cases_combined[case,]$docket_number, '.rdata'))
          save(combined_opinions, file = temp_export_path)


        } # Export

        message(' ---- Completed ', term_cases_combined[case,]$docket_number)

      } # For Each Case

    }

  } # Individually Recover

  {

    completed_opinions <- list.files(file.path(opinions_path, 'opinions', 'opinions_processed'), full.names = T)
    term_opinions <- data.frame()

    for (i in 1:length(completed_opinions)){
      temp_opinion <- get(load(completed_opinions[i]))
      term_opinions <- bind_rows(term_opinions, temp_opinion)
    }

    temp_export_path <- file.path(opinions_path, 'opinions', 'combined_opinions_processed', paste0('combined_opinions_OT', as.character(current_term), '.rdata'))
    save(term_opinions, file = temp_export_path)

  } # Combine All on Backend

  {

    justice_term_opinion_counts <- term_opinions %>%
      filter(!authorship == 'Per Curiam') %>%
      group_by(authorship, opinion_type) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(opinion_type = ifelse(opinion_type == 'Opinion', 'Majority', opinion_type),
             opinion_type = ifelse(opinion_type == 'Special Concurrence', 'Concurrence', opinion_type)) %>%
      pivot_wider(names_from = opinion_type, values_from = count, values_fill = 0) %>%
      mutate(Majority = as.numeric(Majority),
             Concurrence = as.numeric(Concurrence),
             Dissent = as.numeric(Dissent)) %>%
      rename(Justice = authorship) %>%
      dplyr::select(Justice, Majority, Concurrence, Dissent)

    temp_export_path = file.path(opinions_path, 'tables', 'justice_term_opinion_counts.csv')
    write.csv(justice_term_opinion_counts, file = temp_export_path, row.names = F, quote = F)

    message('Completed Term-Level Authorship Counts')


  } # Term-Level Opinion Authorship Counts

  {

    justice_comparison_opinion_counts <- scdb_justices %>%
      filter(term >= 2021) %>%
      filter(opinion %in% c(2, 3)) %>%
      mutate(justice = sub(".*([A-Z][a-zA-Z]*).*$", "\\1", justiceName)) %>%
      select(justice, vote, docket, term) %>%
      mutate(vote = case_when(
        .default = 'Majority',
        vote %in% c(3, 4, 5) ~ 'Concurrence',
        vote %in% c(2, 6, 7) ~ 'Dissent',
        vote == 8 ~ 'Divided Court')) %>%
      rename(opinion_type = vote) %>%
      group_by(term, justice, opinion_type) %>%
      summarise(count = n(), .groups = 'drop') %>%
      bind_rows(term_opinions %>%
                  filter(!authorship == 'Per Curiam') %>%
                  group_by(authorship, opinion_type) %>%
                  summarise(count = n(), .groups = 'drop') %>%
                  mutate(opinion_type = ifelse(opinion_type == 'Opinion', 'Majority', opinion_type),
                         opinion_type = ifelse(opinion_type == 'Special Concurrence', 'Concurrence', opinion_type),
                         term = 2024) %>%
                  rename(justice = authorship)) %>%
      pivot_wider(names_from = opinion_type, values_from = count, values_fill = 0) %>%
      mutate(Majority = as.numeric(Majority),
             Concurrence = as.numeric(Concurrence),
             Dissent = as.numeric(Dissent)) %>%
      select(term, justice, Majority, Concurrence, Dissent) %>%
      arrange(justice, term)

    temp_export_path = file.path(opinions_path, 'tables', 'justice_comparison_opinion_counts.csv')
    write.csv(justice_comparison_opinion_counts, file = temp_export_path, row.names = F, quote = F)

    message('Completed Comparison of Authorship Counts')

  } # Comparison Opinion Authorship Counts

  {

    opinion_lengths_term <- term_opinions %>%
      filter(!authorship == 'Per Curiam') %>%
      left_join(master_file %>%
                  select(case, docket) %>%
                  rename(case_short = case), by = 'docket', relationship = 'many-to-many') %>%
      mutate(case = ifelse(!is.na(case_short), case_short, case)) %>%
      select(-c(case_short)) %>%
      rowwise() %>%
      mutate(word_count = lengths(gregexpr("\\W+", opinion_text)) + 1) %>%
      select(case, authorship, opinion_type, word_count) %>%
      mutate(case = gsub('\\,', '', case))

    opinion_partitions <- split(opinion_lengths_term, ceiling(seq_len(nrow(opinion_lengths_term)) / 30)) # Split the dataframe into chunks of 10 rows

    for (opinion in 1:length(opinion_partitions)){

      temp_opinion_rows <- opinion_partitions[[opinion]]
      temp_output_path <- file.path(opinions_path, 'tables', paste0('term_opinion_lengths_', opinion, '.csv'))
      write.csv(temp_opinion_rows, file = temp_output_path, row.names = F, quote = F)
    }

    message('Completed Term-Level Opinion Lengths -- ', length(opinion_partitions), ' Total')

  } # Opinion Lengths (Current Term)

  {

    average_opinion_lengths_term <- term_opinions %>%
      filter(!authorship == 'Per Curiam') %>%
      rowwise() %>%
      mutate(word_count = lengths(gregexpr("\\W+", opinion_text)) + 1) %>%
      select(opinion_type, authorship, word_count) %>%
      mutate(opinion_type = ifelse(opinion_type == 'Opinion', 'Majority', opinion_type)) %>%
      group_by(authorship, opinion_type) %>%
      summarise(average_length = mean(word_count), .groups = 'drop') %>%
      pivot_wider(names_from = opinion_type, values_from = average_length, values_fill = 0) %>%
      mutate(Majority = round(Majority, 2),
             Concurrence = round(Concurrence, 2),
             Dissent = round(Dissent, 2)) %>%
      rename(Justice = authorship) %>%
      select(Justice, Majority, Concurrence, Dissent) %>%
      mutate(Majority = ifelse(Majority == 0, '', Majority),
             Concurrence = ifelse(Concurrence == 0, '', Concurrence),
             Dissent = ifelse(Dissent == 0, '', Dissent))

    temp_output_path <- file.path(opinions_path, 'tables', paste0('average_opinion_lengths_term .csv'))
    write.csv(average_opinion_lengths_term , file = temp_output_path, row.names = F, quote = F)

    message('Completed Average Opinion Lengths by Type (Current Term)')

  } # Average Lengths by Opinion Type (Current Term)

} # Recover Decisions from Justia


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




###############################################################################
# Compile Term-Level SCOTUS Transcripts Frame (Combined) == Export
###############################################################################

combined_transcripts_term <- function(transcripts_folder,
                                      output_folder = NULL,
                                      output_name = NULL,
                                      assign_file_name = TRUE){

  combined_transcript <- data.frame() # Empty DF to Store Output

  transcript_list <- list.files(transcripts_folder, full.names = T) # Get Files in Folder
  for (file in transcript_list) {
    loaded_data <- get(load(file))

    if (assign_file_name == TRUE){
      file_name <- gsub(transcripts_folder, '', file)
      file_name <- gsub('\\/', '', file_name)
      sitting <- gsub('.*\\_', '' , gsub('\\.rdata', '', file_name))
    } else {
      file_name = NA
      sitting = NA
    }

    loaded_data <- loaded_data %>%
      mutate(file_name = file_name,
             sitting = sitting)

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
      mutate(case_name = str_to_title(case_name)) %>%
      relocate(sitting, .after = 'docket')

    } # Processing

  if (!is.null(output_folder)){

    if (!is.null(output_name)){

      temp_csv_output = file.path(output_folder, paste0(output_name, '.csv'))
      temp_rdata_output = file.path(output_folder, paste0(output_name, '.rdata'))
    } else {
      temp_csv_output = file.path(output_folder, 'transcripts_combined.csv')
      temp_rdata_output = file.path(output_folder, 'transcripts_combined.rdata')
    }

    save(combined_transcript, file = temp_rdata_output)
    write.csv(combined_transcript, file = temp_csv_output)

  } # Export (If Declared)


  return(combined_transcript)


}




###############################################################################
# Oral Argument Analyses
# Justices & Attorney Participation
###############################################################################

oa_analysis <- function(transcript,
                        master_file = NULL,
                        check_folder_status = T,
                        output_path){

  {

    color_cells_justice <- function(data) {

      colfunc <- colorRampPalette(c("grey50", "olivedrab"))(9)

      colored_data <- data.frame()

      for (i in 1:nrow(data)) {

        row_data <- data[i, ] %>%
          relocate(case_name) %>%
          relocate(docket, .after = 'case_name')

        values <- unlist(row_data[ , !(names(row_data) %in% c('case_name', 'docket')) ])
        values[is.na(values)] <- 0
        unique_values <- unique(values)

        if (length(unique_values) > 1) {
          unique_breaks <- quantile(unique_values, probs = seq(0, 1, length.out = 10), na.rm = TRUE)
        } else {
          unique_breaks <- c(min(values) - 1, max(values) + 1)
        }

        color_index <- as.numeric(cut(values, breaks = unique_breaks, labels = 1:9, include.lowest = TRUE))
        values <- cell_spec(values, color = 'white', bold = TRUE, background = colfunc[color_index], font_size = 'large')

        temp_combined_df <- data.frame(
          case_name = data[i, ]$case_name,
          docket = data[i, ]$docket,
          t(values),
          check.names = FALSE
        )
        names(temp_combined_df) <- names(row_data)


        colored_data <- bind_rows(colored_data, temp_combined_df)
      }

      return(colored_data)
    }

    color_cells_attorney <- function(data, type = NULL) {


      if (type == 'Participation'){

        colfunc <- colorRampPalette(c("grey50", "olivedrab"))(9)

        colored_data <- data.frame()

        for (i in 1:nrow(data)) {

          row_data <- data[i, ]

          values <- unlist(row_data[ , !(names(row_data) %in% c('Case', 'Docket', 'Attorney')) ])
          values[is.na(values)] <- 0
          unique_values <- unique(values)

          if (length(unique_values) > 1) {
            unique_breaks <- quantile(unique_values, probs = seq(0, 1, length.out = 10), na.rm = TRUE)
          } else {
            unique_breaks <- c(min(values) - 1, max(values) + 1)
          }

          color_index <- as.numeric(cut(values, breaks = unique_breaks, labels = 1:9, include.lowest = TRUE))
          values <- cell_spec(values, color = 'white', bold = TRUE, background = colfunc[color_index], font_size = 'large')

          temp_combined_df <- data.frame(
            Case = data[i, ]$Case,
            Docket = data[i, ]$Docket,
            Attorney = data[i,]$Attorney,
            t(values),
            check.names = FALSE
          )
          names(temp_combined_df) <- names(row_data)


          colored_data <- bind_rows(colored_data, temp_combined_df)
        }

        return(colored_data)


      } else {
        colfunc <- colorRampPalette(c("grey50", "olivedrab"))(9)

        colored_data <- data.frame()

        values <- unlist(data[ , !(names(data) %in% c('Case', 'Docket', 'Attorney')) ])
        unique_values <- unique(values)

        if (length(unique_values) > 1) {
          unique_breaks <- quantile(unique_values, probs = seq(0, 1, length.out = 10), na.rm = TRUE)
        } else {
          unique_breaks <- c(min(values) - 1, max(values) + 1)
        }

        color_index <- as.numeric(cut(values, breaks = unique_breaks, labels = 1:9, include.lowest = TRUE))

        values <- cell_spec(values, color = 'white', bold = TRUE, background = colfunc[color_index], font_size = 'large')


        if (type == 'Time'){
          temp_combined_df <- data.frame(
            data[names(data) %in% c('Case', 'Docket', 'Attorney')],
            `Time Speaking` = values,
            check.names = FALSE
          )
        } else {
          temp_combined_df <- data.frame(
            data[names(data) %in% c('Case', 'Docket', 'Attorney')],
            `Words` = values,
            check.names = FALSE
          )

        }

        return(temp_combined_df)
      }



    }

  } # Color Cells Function (HTML)

  {
    temp_data <- transcript %>%
      mutate(speaker = gsub('\\,.*', '', speaker))

    if (!is.null(master_file)){
      temp_data <- temp_data %>%
        left_join(master_file %>%
                    dplyr::select(case, docket), by = 'docket') %>%
        select(-c(case_name)) %>%
        rename(case_name = case) %>%
        relocate(case_name)
    }

    unique_sittings <- unique(temp_data$sitting)

    if (check_folder_status == T){
      for (sitting in 1:length(unique_sittings)){

        if (!dir.exists(file.path(output_path, unique_sittings[sitting]))){
          dir.create(file.path(output_path, unique_sittings[sitting]))
        }

        if (!dir.exists(file.path(output_path, 'Combined'))){
          dir.create(file.path(output_path, 'Combined'))
        }

      } # Check if Folder Already Created -- If Not, Add
    } # Create Output Folders (If Necessary)



  } # Recover temp_data using transcript & Create Subfolders (If Needed)

  {

    for (i in 1:length(unique_sittings)){

      temp_justice_words_sitting <- temp_data %>%
        filter(speaker_type == 'Justice') %>%
        filter(sitting == unique_sittings[i]) %>%
        group_by(speaker, docket) %>%
        summarise(total_words = sum(word_count), .groups = 'drop') %>%
        ungroup() %>%
        unique() %>%
        pivot_wider(
          names_from = speaker,
          values_from = total_words) %>%
        left_join(master_file %>%
                    select(docket, case) %>%
                    rename(case_name = case), by = 'docket') %>%
        unique() %>%
        relocate(case_name) %>%
        mutate(case_name = gsub('\\&', 'and', case_name)) %>%
        mutate(across(everything(), ~replace_na(.x, 0))) %>%
        select(case_name, docket, ROBERTS, THOMAS, ALITO, SOTOMAYOR, KAGAN, GORSUCH, KAVANAUGH, BARRETT, JACKSON)


      {

        temp_justice_words_sitting_colored <- color_cells_justice(temp_justice_words_sitting)

        temp_justice_words_sitting_kable <- temp_justice_words_sitting_colored %>%
          rename('Case' = case_name,
                 'Docket' = docket) %>%
          kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
          column_spec(1, bold = TRUE, border_right = TRUE) %>%
          row_spec(0,
                   bold = TRUE,
                   color = 'white',
                   background = '#080808',
                   align = 'center',
                   extra_css = "padding: 0; margin: 0;") %>%
          row_spec(seq(1, nrow(temp_justice_words_sitting_colored), 1), align = 'center') %>%
          kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive"))

        temp_html_output_path <- file.path(output_path, unique_sittings[i], paste0(unique_sittings[i], '_justice_words.html'))
        save_kable(temp_justice_words_sitting_kable, temp_html_output_path) # Save HTML Kable

        temp_txt_output_path <- file.path(output_path, unique_sittings[i], paste0(unique_sittings[i], '_justice_words.txt'))
        temp_justice_words_sitting_txt <- as.character(temp_justice_words_sitting_kable)
        writeLines(temp_justice_words_sitting_txt, temp_txt_output_path) # Save HTML Txt


        } # Kable HTML & TXT HTML

      {

        temp_csv_output_path <- file.path(output_path, unique_sittings[i], paste0(unique_sittings[i], '_justice_words.csv'))
        write.csv(temp_justice_words_sitting, file = temp_csv_output_path, row.names = F, quote = F)

      } # CSV Output

    } # For Each Sitting -- Produce Individual Values & Figures -- Export HTML (Kable), HTML TXT, CSV, and Figure


  } # Justice Words by Sitting (KBL HTMl, TXT HTML, and CSV)

  message('Compiled Justice-Level Word Counts')

  {

    for (i in 1:length(unique_sittings)){

      temp_justice_sitting_time <- temp_data %>%
        filter(speaker_type == 'Justice') %>%
        filter(sitting == unique_sittings[i]) %>%
        mutate(elapsed = text_stop - text_start) %>%
        group_by(docket, speaker) %>%
        summarise(total_elapsed = sum(elapsed), .groups = 'drop') %>%
        mutate(total_elapsed = round(total_elapsed/60, 2)) %>%
        pivot_wider(
          names_from = speaker,
          values_from = total_elapsed) %>%
        left_join(master_file %>%
                    select(docket, case) %>%
                    rename(case_name = case), by = 'docket') %>%
        unique() %>%
        relocate(case_name) %>%
        mutate(case_name = gsub('\\&', 'and', case_name)) %>%
        mutate(across(everything(), ~replace_na(.x, 0))) %>%
        select(case_name, docket, ROBERTS, THOMAS, ALITO, SOTOMAYOR, KAGAN, GORSUCH, KAVANAUGH, BARRETT, JACKSON) # Recover Time in Minutes by Speaker


      {

        temp_justice_time_sitting_colored <- color_cells_justice(temp_justice_sitting_time)

        temp_justice_time_sitting_kable <- temp_justice_time_sitting_colored %>%
          rename('Case' = case_name,
                 'Docket' = docket) %>%
          kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
          column_spec(1, bold = TRUE, border_right = TRUE) %>%
          row_spec(0,
                   bold = TRUE,
                   color = 'white',
                   background = '#080808',
                   align = 'center',
                   extra_css = "padding: 0; margin: 0;") %>%
          row_spec(seq(1, nrow(temp_justice_time_sitting_colored), 1), align = 'center') %>%
          kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive"))

        temp_html_output_path <- file.path(output_path, unique_sittings[i], paste0(unique_sittings[i], '_justice_time.html'))
        save_kable(temp_justice_time_sitting_kable, temp_html_output_path) # Save HTML Kable

        temp_txt_output_path <- file.path(output_path, unique_sittings[i], paste0(unique_sittings[i], '_justice_time.txt'))
        temp_justice_time_sitting_txt <- as.character(temp_justice_time_sitting_kable)
        writeLines(temp_justice_time_sitting_txt, temp_txt_output_path) # Save HTML Txt


        } # Kable HTML & TXT HTML

      {

        temp_csv_output_path <- file.path(output_path, unique_sittings[i], paste0(unique_sittings[i], '_justice_time.csv'))
        write.csv(temp_justice_sitting_time, file = temp_csv_output_path, row.names = F, quote = F)

      } # CSV Output

    } # For Each Sitting -- Produce Individual Values & Figures -- Export HTML (Kable), HTML TXT, CSV, and Figure


  } # Justice Time by Sitting (KBL HTMl, TXT HTML, and CSV)

  message('Compiled Justice-Level Speaking Times')

  {
    for (i in 1:length(unique_sittings)){

      temp_attorney_sitting_words <- temp_data %>%
        filter(speaker_type == 'Attorney') %>%
        filter(sitting == unique_sittings[i]) %>%
        group_by(speaker, docket) %>%
        summarise(total_words = sum(word_count), .groups = 'drop') %>%
        ungroup() %>%
        unique() %>%
        left_join(master_file %>%
                    select(docket, case) %>%
                    rename(case_name = case), by = 'docket', relationship = 'many-to-many') %>%
        unique() %>%
        relocate(case_name) %>%
        mutate(case_name = gsub('\\&', 'and', case_name)) %>%
        mutate(across(everything(), ~replace_na(.x, 0))) %>%
        rename(Case = case_name,
               Attorney = speaker,
               Docket = docket) %>%
        arrange(Docket)


      {

        temp_attorney_words_sitting_colored <- color_cells_attorney(data = temp_attorney_sitting_words, type = 'Words')

        temp_attorney_words_sitting_kable <- temp_attorney_words_sitting_colored %>%
          kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
          column_spec(1, bold = TRUE, border_right = TRUE) %>%
          row_spec(0,
                   bold = TRUE,
                   color = 'white',
                   background = '#080808',
                   align = 'center',
                   extra_css = "padding: 0; margin: 0;") %>%
          row_spec(seq(1, nrow(temp_attorney_words_sitting_colored), 1), align = 'center') %>%
          kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive"))

        temp_html_output_path <- file.path(output_path, unique_sittings[i], paste0(unique_sittings[i], '_attorney_words.html'))
        save_kable(temp_attorney_words_sitting_kable, temp_html_output_path) # Save HTML Kable

        temp_txt_output_path <- file.path(output_path, unique_sittings[i], paste0(unique_sittings[i], '_attorney_words.txt'))
        temp_attorney_time_sitting_txt <- as.character(temp_attorney_words_sitting_kable)
        writeLines(temp_attorney_time_sitting_txt, temp_txt_output_path) # Save HTML Txt


        } # Kable HTML & TXT HTML

      {

        temp_csv_output_path <- file.path(output_path, unique_sittings[i], paste0(unique_sittings[i], '_attorney_words.csv'))
        write.csv(temp_attorney_sitting_words, file = temp_csv_output_path, row.names = F, quote = F)

      } # CSV Output

    }


  } # Attorney Words

  message('Completed Attorney Word Counts')

  {
    for (i in 1:length(unique_sittings)){

      temp_attorney_sitting_time <- temp_data %>%
        filter(speaker_type == 'Attorney') %>%
        filter(sitting == unique_sittings[i]) %>%
        mutate(elapsed = text_stop - text_start) %>%
        group_by(docket, speaker) %>%
        summarise(total_elapsed = sum(elapsed), .groups = 'drop') %>%
        mutate(total_elapsed = round(total_elapsed/60, 2)) %>%
        left_join(master_file %>%
                    select(docket, case) %>%
                    rename(case_name = case), by = 'docket', relationship = 'many-to-many') %>%
        unique() %>%
        relocate(case_name) %>%
        mutate(total_elapsed = ifelse(is.na(total_elapsed), 0, total_elapsed)) %>%
        mutate(case_name = gsub('\\&', 'and', case_name)) %>%
        rename(Case = case_name,
               Docket = docket,
               Attorney = speaker,
               `Time Speaking` = total_elapsed) %>%
        arrange(Docket)


      {

        temp_attorney_time_sitting_colored <- color_cells_attorney(temp_attorney_sitting_time, type = 'Time')

        temp_attorney_time_sitting_kable <- temp_attorney_time_sitting_colored %>%
          kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
          column_spec(1, bold = TRUE, border_right = TRUE) %>%
          row_spec(0,
                   bold = TRUE,
                   color = 'white',
                   background = '#080808',
                   align = 'center',
                   extra_css = "padding: 0; margin: 0;") %>%
          row_spec(seq(1, nrow(temp_attorney_time_sitting_colored), 1), align = 'center') %>%
          kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive"))

        temp_html_output_path <- file.path(output_path, unique_sittings[i], paste0(unique_sittings[i], '_attorney_time.html'))
        save_kable(temp_attorney_time_sitting_kable, temp_html_output_path) # Save HTML Kable

        temp_txt_output_path <- file.path(output_path, unique_sittings[i], paste0(unique_sittings[i], '_attorney_time.txt'))
        temp_attorney_time_sitting_txt <- as.character(temp_attorney_time_sitting_kable)
        writeLines(temp_attorney_time_sitting_txt, temp_txt_output_path) # Save HTML Txt


        } # Kable HTML & TXT HTML

      {

        temp_csv_output_path <- file.path(output_path, unique_sittings[i], paste0(unique_sittings[i], '_attorney_time.csv'))
        write.csv(temp_attorney_sitting_time, file = temp_csv_output_path, row.names = F, quote = F)

      } # CSV Output

    }


  } # Attorney Time

  message('Completed Attorney Speaking Times')

  {

    for (i in 1:length(unique_sittings)){

      temp_attorney_sitting_time <- temp_data %>%
        filter(speaker_type == 'Attorney') %>%
        filter(sitting == unique_sittings[i]) %>%
        mutate(elapsed = text_stop - text_start) %>%
        group_by(docket, speaker) %>%
        summarise(total_elapsed = sum(elapsed), .groups = 'drop') %>%
        mutate(total_elapsed = round(total_elapsed/60, 2)) %>%
        left_join(master_file %>%
                    select(docket, case) %>%
                    rename(case_name = case), by = 'docket', relationship = 'many-to-many') %>%
        unique() %>%
        relocate(case_name) %>%
        mutate(total_elapsed = ifelse(is.na(total_elapsed), 0, total_elapsed)) %>%
        mutate(case_name = gsub('\\&', 'and', case_name)) %>%
        rename(Case = case_name,
               Docket = docket,
               Attorney = speaker,
               `Time Speaking` = total_elapsed) %>%
        arrange(Docket)

      temp_attorney_sitting_words <- temp_data %>%
        filter(speaker_type == 'Attorney') %>%
        filter(sitting == unique_sittings[i]) %>%
        group_by(speaker, docket) %>%
        summarise(total_words = sum(word_count), .groups = 'drop') %>%
        ungroup() %>%
        unique() %>%
        left_join(master_file %>%
                    select(docket, case) %>%
                    rename(case_name = case), by = 'docket', relationship = 'many-to-many') %>%
        unique() %>%
        relocate(case_name) %>%
        mutate(case_name = gsub('\\&', 'and', case_name)) %>%
        mutate(across(everything(), ~replace_na(.x, 0))) %>%
        rename(Case = case_name,
               Attorney = speaker,
               Docket = docket) %>%
        arrange(Docket)

      temp_attorney_sitting_words_time_combined <- temp_attorney_sitting_time %>%
        mutate(`Total Words` = temp_attorney_sitting_words$total_words) %>%
        mutate(Attorney = gsub('\\, Jr.*', '', Attorney))

      {

        temp_csv_output_path <- file.path(output_path, unique_sittings[i], paste0(unique_sittings[i], '_attorney_time_words_combined.csv'))
        write.csv(temp_attorney_sitting_words_time_combined, file = temp_csv_output_path, row.names = F, quote = F)

        } # CSV Export


    }

  } # Attorney Time + Words Combined

  message('Completed Attorney Word Counts + Speaking Time (Combined)')

  {

    for (i in 1:length(unique_sittings)){

      temp_sitting_attorney_participation <- temp_data %>%
        filter(sitting == unique_sittings[i]) %>%
        mutate(follows_justice = ifelse(speaker_type == 'Attorney' & lag(speaker_type) == 'Justice', 1, 0),
               preceding_justice = ifelse(follows_justice == 1, lag(speaker), NA)) %>%
        filter(follows_justice == 1) %>%
        select(-c(follows_justice)) %>%
        group_by(docket, speaker, preceding_justice) %>%
        summarise(total_words = sum(word_count), .groups = 'drop') %>%
        pivot_wider(
          names_from = preceding_justice,
          values_from = total_words) %>%
        left_join(master_file %>%
                     select(docket, case) %>%
                     rename(case_name = case), by = 'docket', relationship = 'many-to-many') %>%
        unique() %>%
        relocate(case_name) %>%
        mutate(case_name = gsub('\\&', 'and', case_name)) %>%
        mutate(across(everything(), ~replace_na(.x, 0))) %>%
        rename(Case = case_name,
               Docket = docket,
               Attorney = speaker) %>%
        select(Case, Docket, Attorney, ROBERTS, THOMAS, ALITO, SOTOMAYOR, KAGAN, GORSUCH, KAVANAUGH, BARRETT, JACKSON) %>%
        arrange(Docket)


      {

        temp_sitting_attorney_participation_colored <- color_cells_attorney(temp_sitting_attorney_participation, type = 'Participation')

        temp_sitting_attorney_participation_kable <- temp_sitting_attorney_participation_colored %>%
          kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
          column_spec(1, bold = TRUE, border_right = TRUE) %>%
          row_spec(0,
                   bold = TRUE,
                   color = 'white',
                   background = '#080808',
                   align = 'center',
                   extra_css = "padding: 0; margin: 0;") %>%
          row_spec(seq(1, nrow(temp_sitting_attorney_participation_colored ), 1), align = 'center') %>%
          kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive"))

        temp_html_output_path <- file.path(output_path, unique_sittings[i], paste0(unique_sittings[i], '_attorney_participation.html'))
        save_kable(temp_sitting_attorney_participation_kable, temp_html_output_path) # Save HTML Kable

        temp_txt_output_path <- file.path(output_path, unique_sittings[i], paste0(unique_sittings[i], '_attorney_participation.txt'))
        temp_sitting_attorney_participation_txt <- as.character(temp_sitting_attorney_participation_kable)
        writeLines(temp_sitting_attorney_participation_txt, temp_txt_output_path) # Save HTML Txt


        } # Kable HTML & TXT HTML

      {

        temp_csv_output_path <- file.path(output_path, unique_sittings[i], paste0(unique_sittings[i], '_attorney_participation.csv'))
        write.csv(temp_sitting_attorney_participation, file = temp_csv_output_path, row.names = F, quote = F)

      } # CSV Output




    }

  } # Attorney Participation (Responses to Justices)

  message('Completed Attorney Participation')

  {

    {

      figure_data <- temp_data %>%
        filter(speaker_type == 'Justice') %>%
        select(speaker, word_count) %>%
        group_by(speaker) %>%
        summarise(total_words = sum(word_count), .groups = 'drop') %>%
        mutate(justice = gsub('(Justice |Chief Justice)', '', speaker, ignore.case = T)) %>%
        mutate(image_labels = justice_image_labels[match(justice, names(justice_image_labels))],
               image_labels = gsub(' style\\=.*', '', image_labels),
               image_labels = paste0(image_labels, " width='100' /><br><strong>", justice, "</strong>"))

      image_labels <- figure_data$image_labels

      justice_words_combined_figure <- ggplot(figure_data, aes(x = factor(justice), y = total_words)) +
        geom_col(aes(fill = total_words), colour = 'gray5') +
        scale_fill_gradient(low = 'gray50', high = 'olivedrab') +
        geom_label(aes(label = scales::comma(total_words)), vjust = 1, size = 5) +
        geom_hline(yintercept = 0) +
        scale_x_discrete(labels = image_labels) +
        theme_minimal() +
        labs(x = '',
             y = '',
             fill = '') +
        theme(
          axis.text.x = ggtext::element_markdown(),
          panel.grid = element_blank(),
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


      temp_combined_figure_output_path <- file.path(output_path, 'Combined', 'justice_words_combined.png')
      ggsave(justice_words_combined_figure, filename = temp_combined_figure_output_path, height = 8, width = 8, bg = 'white')


    } # Justice Words Combined (Figure)

    {

      figure_data <- temp_data %>%
        filter(speaker_type == 'Justice') %>%
        mutate(elapsed = text_stop - text_start,
               elapsed = round(elapsed/60, 2)) %>%
        select(speaker, elapsed) %>%
        group_by(speaker) %>%
        summarise(total_time = sum(elapsed), .groups = 'drop') %>%
        mutate(justice = gsub('(Justice |Chief Justice)', '', speaker, ignore.case = T)) %>%
        mutate(image_labels = justice_image_labels[match(justice, names(justice_image_labels))],
               image_labels = gsub(' style\\=.*', '', image_labels),
               image_labels = paste0(image_labels, " width='100' /><br><strong>", justice, "</strong>"))

      image_labels <- figure_data$image_labels

      justice_time_combined_figure <- ggplot(figure_data, aes(x = factor(justice), y = total_time)) +
        geom_col(aes(fill = total_time), colour = 'gray5') +
        scale_fill_gradient(low = 'gray50', high = 'olivedrab') +
        geom_label(aes(label = scales::comma(total_time)), vjust = 1, size = 5) +
        geom_hline(yintercept = 0) +
        scale_x_discrete(labels = image_labels) +
        theme_minimal() +
        labs(x = '',
             y = '',
             fill = '') +
        theme(
          axis.text.x = ggtext::element_markdown(),
          panel.grid = element_blank(),
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

      temp_combined_figure_output_path <- file.path(output_path, 'Combined', 'justice_time_combined.png')
      ggsave(justice_time_combined_figure, filename = temp_combined_figure_output_path, height = 8, width = 8, bg = 'white')


    } # Justice Time Combined (Figure)

    {

      justice_words_combined <- temp_data %>%
        filter(speaker_type == 'Justice') %>%
        group_by(speaker, sitting) %>%
        summarise(total_words = sum(word_count), .groups = 'drop') %>%
        select(speaker, total_words, sitting) %>%
        mutate(sitting = factor(sitting, levels = c(
          'October','November','December','January','February',
          'March','April','May','June','July'
        ))) %>%
        pivot_wider(names_from = speaker, values_from = total_words) %>%
        arrange(sitting) %>%
        {
          total_row <- summarise(
            .,
            across(
              where(is.numeric),
              ~ sum(.x, na.rm = TRUE)
            )
          ) %>%
            mutate(sitting = "TOTAL") %>%
            select(names(.))
          bind_rows(., total_row)
        }


      {

        temp_csv_output_path = file.path(output_path, 'Combined', 'justice_words_combined.csv')
        write.csv(justice_words_combined, file = temp_csv_output_path, row.names = F, quote = F)

      } # CSV Output



    } # Justice Words Combined (Table)

    {

      justice_time_combined <- temp_data %>%
        filter(speaker_type == 'Justice') %>%
        mutate(elapsed = text_stop - text_start,
               elapsed = round(elapsed / 60, 2)) %>%
        select(speaker, elapsed, sitting) %>%
        group_by(speaker, sitting) %>%
        summarise(total_time = sum(elapsed), .groups = 'drop') %>%
        mutate(sitting = factor(sitting, levels = c(
          'October','November','December','January','February',
          'March','April','May','June','July'
        ))) %>%
        pivot_wider(names_from = speaker, values_from = total_time) %>%
        arrange(sitting) %>%
        {
          total_row <- summarise(
            .,
            across(
              where(is.numeric),
              ~ sum(.x, na.rm = TRUE)
            )
          ) %>%
            mutate(sitting = "TOTAL") %>%
            select(names(.))
          bind_rows(., total_row)
        }


      {

        temp_csv_output_path = file.path(output_path, 'Combined', 'justice_time_combined.csv')
        write.csv(justice_words_combined, file = temp_csv_output_path, row.names = F, quote = F)

      } # CSV Output



    } # Justice Time Combined (Table)



  } # Combined Analyses

  message('Completed Justice-Level Combined Analysis')


}


###############################################################################
# Attorney Information
###############################################################################

attorney_information_template <- function(transcript,
                                 master_file,
                                 output_path){


  temp_data <- transcript %>%
    filter(speaker_type == 'Attorney') %>%
    select(docket, sitting, speaker) %>%
    left_join(cases_master %>%
                select(docket, case) %>%
                rename(case_name = case), by = 'docket', relationship = 'many-to-many') %>%
    unique() %>%
    rename(attorney = speaker) %>%
    relocate(case_name) %>%
    arrange(attorney) %>%
    group_by(attorney) %>%
    mutate(argument_number = row_number(),
           total_arguments = max(argument_number)) %>%
    ungroup() %>%
    select(attorney, total_arguments) %>%
    unique() %>%
    mutate(gender = '',
           repeat_attorney = '',
           law_school = '',
           ivy = '',
           scotus_clerk = '',
           firm = '',
           SG_experience = '',
           oyez_link = '')

  temp_output_path = file.path(output_path, 'attorney_information_base.csv')
  write.csv(temp_data, file = temp_output_path)


}



###############################################################################
# Docket Recovery
###############################################################################

docket_search <- function(docket_id, output_path){

  process_docket_id <- function(docket_id) {
    docket_url_newer <- "https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/"
    docket_url_older <- "https://www.supremecourt.gov/search.aspx?filename=/docketfiles/"
    term <- as.data.frame(docket_id)
    term <- term %>%
      mutate(type = 3) %>%
      mutate(type = ifelse(grepl("m", docket_id, ignore.case = T), 2, type)) %>%
      mutate(type = ifelse(grepl("a", docket_id, ignore.case = T), 1, type)) %>%
      mutate(type = ifelse(grepl("o", docket_id, ignore.case = T), 4, type)) %>%
      mutate(year = as.numeric(gsub("\\-.*|a.*|A.*|m.*|M.*|Original.*|original.*|o.*|O.*|Orig.*|orig.*", "", docket_id))) %>%
      mutate(year = ifelse(as.numeric(year) %in% 0:9, as.character(paste0(sprintf("%02d", year))), as.character(year))) %>%
      mutate(docket_number = gsub(".*\\-|.*a|.*A|.*m|.*M|.*Original|.*original|.*o|.*O|.*Orig|.*orig", "", docket_id)) %>%
      mutate(year = gsub("[^0-9]", "", year),
             docket_number = gsub("[^0-9]", "", docket_number)) %>% #Delete Non-Numbers
      mutate(html = ifelse(as.numeric(year) > 50, "HTM",
                           ifelse(as.numeric(year) < 17, "HTM",
                                  ifelse(as.numeric(year) >= 17 & as.numeric(year) < 30, "HTML", "HTM")))) %>%
      mutate(url = case_when(
        type == 1 & html == "HTM" ~ paste0(docket_url_older, year, "a", docket_number, ".htm"),
        type == 1 & html == "HTML" ~ paste0(docket_url_newer, year, "a", docket_number, ".html"),

        type == 2 & html == "HTM" ~ paste0(docket_url_older, year, "m", docket_number, ".htm"),
        type == 2 & html == "HTML" ~ paste0(docket_url_newer, year, "m", docket_number, ".html"),

        type == 3 & html == "HTM" ~ paste0(docket_url_older, year, "-", docket_number, ".htm"),
        type == 3 & html == "HTML" ~ paste0(docket_url_newer, year, "-", docket_number, ".html"),

        type == 4 & html == "HTM" ~ paste0(docket_url_older, year, "o", docket_number, ".htm"),
        type == 4 & html == "HTML" ~ paste0(docket_url_newer, year, "o", docket_number, ".html")
      ))
    urls <- term$url
    return(urls)
  } #Translate Docket IDs to URLs

  docket_info <- function(response, docket){

    html_content <- httr::content(response, as = 'text')
    parsed_html <- rvest::read_html(html_content)

    info_meta <- parsed_html %>%
      rvest::html_element('#docketinfo') %>%
      rvest::html_table(fill = T) %>%
      dplyr::select(X1, X2) %>%
      dplyr::rename(information = X1,
                    entry = X2) %>%
      dplyr::mutate(information = ifelse(grepl('Linked with', information, ignore.case = T), gsub('Linked with', 'Linked with:', information, ignore.case = T), information),
                    entry = ifelse(grepl('Linked with\\:', information, ignore.case = T), gsub('.*with\\:', '', trimws(information)), entry),
                    information = ifelse(grepl('Linked with\\:', information), gsub('with\\:.*', 'with:', information), information),
                    entry = trimws(entry)) %>%
      dplyr::filter(grepl('\\:', information)) %>%
      dplyr::mutate(information = gsub('\\:', '', information),
                    entry = ifelse(entry == '', NA, entry)) %>%
      tidyr::pivot_wider(names_from = information,
                         values_from = entry) %>%
      dplyr::rename_with(~ gsub(' ', '_', tolower(.x))) %>%
      dplyr::mutate(petitioner = trimws(gsub(' v\\..*', '', title)),
                    petitioner = trimws(gsub(' Petitioner.*', '', petitioner)),
                    petitioner = trimws(gsub('\\, et al\\.\\,$', ', et al.', petitioner)),
                    respondent = trimws(gsub('.* v\\.', '', title)),
                    docket = docket) %>%
      dplyr::relocate(petitioner, respondent, .after = title) %>%
      dplyr::relocate(docket, .after = title)

    if (!'linked_with' %in% names(info_meta)){
      info_meta <- info_meta %>%
        dplyr::mutate(linked_with = NA) %>%
        dplyr::relocate(linked_with, .after = docketed)
    }

    return(info_meta)

  } # Docket Info Table

  docket_entries <- function(response){

    temp_docket_entries <- httr::content(response, as = 'text') %>%
      rvest::read_html(html_content) %>%
      rvest::html_element('#proceedings')  %>%
      rvest::html_table(fill = T) %>%
      select(X1, X2) %>%
      rename(date = X1,
             entry = X2) %>%
      mutate(date = ifelse(date == '', NA, date),
             date = ifelse(date == 'Date', NA, date)) %>%
      filter(!is.na(date)) %>%
      mutate(date = lubridate::mdy(date))

    return(temp_docket_entries)

  } # Docket Entries

  counsel <- function(response){

    html_content <- httr::content(response, as = 'text')

    counsel_temp <- gsub("(<br>|<br/>|<br />|\\n)", "\n", html_content) %>%
      rvest::read_html(.) %>%
      rvest::html_element("#Contacts") %>%
      rvest::html_table(fill = TRUE) %>%
      dplyr::mutate(across(everything(), ~ gsub("\\n", " ", .))) %>%
      dplyr::select(X1, X2, X3) %>%
      dplyr::rename(name = X1,
                    address = X2,
                    contact = X3) %>%
      dplyr::filter(!grepl('NAME', name, ignore.case = T)) %>%
      dplyr::filter(!grepl('Party name\\:', name, ignore.case = T)) %>%
      dplyr::mutate(counsel_of_record = ifelse(grepl('Counsel of Record', name, ignore.case = T), 'Yes', 'No'),
                    name = gsub('Counsel of Record', '', name, ignore.case = T),
                    name = trimws(name),
                    party = case_when(
                      grepl('(Petitioner|Applicant|Appellant)', name, ignore.case = T) ~ 'Petitioner',
                      grepl('(Respondent|Appellee)', name, ignore.case = T) ~ 'Respondent',
                      name %in% c('Other', 'other') ~ 'Other')) %>%
      tidyr::fill(party, .direction = 'down') %>%
      dplyr::filter(!grepl('Attorneys for ', name)) %>%
      dplyr::filter(!name %in% c('Other', 'other')) %>%
      filter(!name == '')

    return(counsel_temp)

  } # Return Counsel

  check_package_install <- function(){

    check_and_load_packages <- function(packages) {
      for (pkg in packages) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
          cat(sprintf("Package '%s' is not installed. Do you want to install it? (Y/N): ", pkg))
          response <- tolower(readline())
          if (response == "y") {
            install.packages(pkg)
          } else if (response == "n") {
            stop(sprintf("Cannot proceed without '%s' installed. Exiting.", pkg))
          } else {
            stop("Invalid input. Please enter Y or N. Exiting.")
          }
        }
        # Load the library after installation or if already installed
        library(pkg, character.only = TRUE)
      }
    }

    required_packages <- c('dplyr', 'rvest', 'reticulate', 'httr', 'tm', 'stringr', 'tm', 'tidyr', 'graphics', 'utils', 'htm2txt', 'zoo', 'pdftools')
    check_and_load_packages(required_packages)
    library(dplyr); library(rvest); library(reticulate); library(httr); library(tm); library(stringr); library(tm); library(tidyr); library(graphics); library(utils); library(htm2txt); library(zoo); library(pdftools)

  } # Check if dplyr, rvest, reticulate, and httr are installed

  check_package_install() # Check If Necessary Packages are Installed

  temp_docket_url <- process_docket_id(docket_id)
  response <- httr::GET(temp_docket_url) # Retrieve HTML
  temp_docket_info <- docket_info(response, docket_id) # Get Docket Meta Info
  temp_docket_entries <- docket_entries(response) # Get Docket Entries
  temp_counsel <- counsel(response) # Get Counsel (Full)


  combined_temp <- temp_docket_info %>%
    dplyr::mutate(docket_entries = list(temp_docket_entries),
                  counsel = list(temp_counsel))

  temp_output_path <- file.path(output_path, paste0(docket_id, '.rdata'))
  save(combined_temp, file = temp_output_path)

} # Docket Search


###############################################################################
# Docket Analysis
###############################################################################

dockets_combine <- function(dockets_path, output_path){

  combined_dockets <- data.frame() # Empty DF to Combine
  files <- list.files(dockets_path, full.names = T) # Link to Processed Dockets

  for (i in 1:length(files)){

    temp_docket <- get(load(files[i]))
    combined_dockets <- bind_rows(combined_dockets, temp_docket)

    if (i %% 100 == 0){
      message('Combined Docket ', i, ' of ', length(files))
    }

  } # Append to Combined Dockets

  save(combined_dockets, file = output_path)


} # Combine Dockets Into Single DF

dockets_analysis <- function(combined_dockets, output_path){

  dockets <- get(load(combined_dockets))

  {

    filing_types_distribution <- dockets %>%
      select(docket) %>%
      mutate(type = case_when(
        .default = 'Certiorari_Petition',
        grepl('a', docket, ignore.case = T) ~ 'Application',
        grepl('m', docket, ignore.case = T) ~ 'Motion'
      )) %>%
      group_by(type) %>%
      summarise(count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = type, values_from = count, values_fill = 0) %>%
      select('Certiorari_Petition', 'Application', 'Motion')


    temp_output_path <- file.path(output_path, 'tables', 'filing_types_distribution.csv')
    write.csv(filing_types_distribution, file = temp_output_path, row.names = F, quote = F)

    message('Completed Distribution of Filings by Type')

  } # Distribution of Filings Type (CSV)

  {

    {

      docketed_data <- dockets %>%
        select(docket, docketed) %>%
        mutate(
          type = case_when(
            grepl('a', docket, ignore.case = TRUE) ~ 'Application',
            grepl('m', docket, ignore.case = TRUE) ~ 'Motion',
            .default = 'Certiorari_Petition'
          ),
          docketed_date = as.Date(docketed, format = "%B %d, %Y"),
          docketed_month_year = lubridate::floor_date(docketed_date, "month")  # Proper Date object
        )

    } # Docketed Data

    {

      {

        cumulative_docket_count_data <- docketed_data %>%
          mutate(docketed_month_year = floor_date(as.Date(docketed, format = "%B %d, %Y"), "month"),
                 docketed_month_year_label = format(as.Date(docketed, format = "%B %d, %Y"), "%B %Y")) %>%
          group_by(type, docketed_month_year) %>%
          reframe(n = n(),
                  docketed_month_year_label) %>%
          ungroup() %>%
          unique() %>%
          filter(!is.na(docketed_month_year)) %>%
          complete(
            type,
            docketed_month_year = seq(
              min(docketed_month_year),
              max(docketed_month_year),
              by = "month"),  fill = list(n = 0)) %>%
          group_by(type) %>%
          mutate(
            cumulative_n = cumsum(n),
            cumulative_n = zoo::na.locf(cumulative_n, na.rm = FALSE)) %>%
          mutate(type = factor(type, levels = c('Certiorari_Petition', 'Application', 'Motion')))

        temp_export_path <- file.path(output_path, 'tables', 'cumulative_docket_count.csv')
        write.csv(cumulative_docket_count_data %>%
                    arrange(docketed_month_year) %>%
                    filter(!is.na(docketed_month_year_label)) %>%
                    select(type, docketed_month_year_label, cumulative_n) %>%
                    pivot_wider(names_from = type, values_from = cumulative_n, values_fill = 0) %>%
                    rename(month_year = docketed_month_year_label) %>%
                    select(month_year, Certiorari_Petition, Application, Motion), file = temp_export_path, row.names = F, quote = F)

      } # Recover Data & Export

      {

        cumulative_docket_count <- ggplot(data = cumulative_docket_count_data, aes(x = docketed_month_year, y = cumulative_n)) +
          geom_col(colour = 'black', fill ='gray50') +
          geom_hline(yintercept = 0) +
          facet_wrap(~type, nrow = 3, scales = 'free_y') +
          labs(x = ' ',
               y = '\nCumulative Count\n') +
          scale_x_date(
            date_breaks = "1 month",
            date_labels = "%b\n%Y",
            expand = expansion(mult = c(0.025, 0.025))) +
          scale_y_continuous(labels = function(l) {
            labels <- label_comma()(l)
            labels[l == 0] <- ''
            labels},
            expand = expansion(mult = c(0, 0.5))) +
          geom_label(aes(label = cumulative_n), vjust = -0.5, size = 5) +
          theme_minimal() +
          theme(
            panel.border = element_rect(size = 1, colour = 'black', fill = NA),
            axis.text = element_text(size = 14, colour = 'black'),
            axis.title = element_text(size = 16, colour = 'black'),
            legend.background = element_rect(linewidth = 1, fill = NA, colour = "black"),
            legend.box.background = element_rect(fill = NA, colour = "black"),
            legend.position = "none",
            legend.title = element_blank(),
            legend.title.align = 0.5,
            legend.text = element_text(size = 12),
            strip.text = element_text(size = 14, colour = 'black'),
            strip.background = element_rect(size = 1, colour = 'black', fill = 'gray75'))

        temp_export_path <- file.path(output_path, 'figures', 'cumulative_docket_count.png')
        ggsave(temp_export_path,
               cumulative_docket_count,
               height = 10,
               width = 10,
               units = 'in',
               bg = 'white')

      } # Compile Figure & Export

      message('Completed Cumulative Filing Types')


    } # Cumulative Count by Month

    {

      {

        docket_trends_count_data <- docketed_data %>%
          mutate(docketed_month_year = floor_date(as.Date(docketed, format = "%B %d, %Y"), "month"),
                 docketed_month_year_label = format(as.Date(docketed, format = "%B %d, %Y"), "%B %Y")) %>%
          group_by(type, docketed_month_year) %>%
          reframe(count = n(),
                  docketed_month_year_label) %>%
          unique() %>%
          mutate(type = factor(type, levels = c('Certiorari_Petition', 'Application', 'Motion')))

        temp_export_path <- file.path(output_path, 'tables', 'docket_trends.csv')
        write.csv(docket_trends_count_data %>%
                    arrange(docketed_month_year) %>%
                    filter(!is.na(docketed_month_year_label)) %>%
                    select(type, docketed_month_year_label, count) %>%
                    pivot_wider(names_from = type, values_from = count, values_fill = 0) %>%
                    rename(month_year = docketed_month_year_label) %>%
                    select(month_year, Certiorari_Petition, Application, Motion), file = temp_export_path, row.names = F, quote = F)

      } # Recover Data & Export

      {

        docket_trends <- docketed_data %>%
          mutate(type = ifelse(type == 'Certiorari_Petition', 'Certiorari Petition', type),
                 docketed_month_year = floor_date(as.Date(docketed, format = "%B %d, %Y"), "month")) %>%
          group_by(type, docketed_month_year) %>%
          summarise(count = n(), .groups = "drop") %>%
          mutate(type = factor(type, levels = c('Certiorari Petition', 'Application', 'Motion'))) %>%
          ggplot(aes(x = docketed_month_year, y = count)) +
          geom_col(colour = 'black', fill ='gray50') +
          geom_hline(yintercept = 0) +
          facet_wrap(~type, nrow = 3, scales = 'free_y') +
          labs(x = ' ',
               y = '\nCount by Month\n') +
          scale_x_date(
            date_breaks = "1 month",
            date_labels = "%b\n%Y",
            expand = expansion(mult = c(0.025, 0.025))) +
          scale_y_continuous(labels = function(l) {
            labels <- label_comma()(l)
            labels[l == 0] <- ''
            labels},
            expand = expansion(mult = c(0, 0.5))) +
          geom_label(aes(label = count), vjust = -0.5, size = 5) +
          theme_minimal() +
          theme(
            panel.border = element_rect(size = 1, colour = 'black', fill = NA),
            axis.text = element_text(size = 14, colour = 'black'),
            axis.title = element_text(size = 16, colour = 'black'),
            legend.background = element_rect(linewidth = 1, fill = NA, colour = "black"),
            legend.box.background = element_rect(fill = NA, colour = "black"),
            legend.position = "none",
            legend.title = element_blank(),
            legend.title.align = 0.5,
            legend.text = element_text(size = 12),
            strip.text = element_text(size = 14, colour = 'black'),
            strip.background = element_rect(size = 1, colour = 'black', fill = 'gray75'))

        temp_export_path <- file.path(output_path, 'figures', 'docket_trends.png')
        ggsave(temp_export_path,
               docket_trends,
               height = 10,
               width = 10,
               units = 'in',
               bg = 'white')

      } # Compile Figure & Export


    } # Individual Count by Month


  } # Distribution of When Things Were Filed by Date & Month/Year (Figure & CSV)

  {

    {

      origins <- dockets %>%
        filter(grepl('-', docket)) %>%
        select(lower_ct) %>%
        filter(!is.na(lower_ct)) %>%
        group_by(lower_ct) %>%
        summarise(count = n(), .groups = 'drop') %>%
        ungroup() %>%
        mutate(lower_ct = gsub('United States Court of Appeals for the ', '', lower_ct),
               lower_ct = gsub('\\,', '', lower_ct)) %>%
        arrange(desc(count))

      origins_partitions <- split(origins, ceiling(seq_len(nrow(origins)) / 25)) # Split the dataframe into chunks of 10 rows

      for (i in 1:length(origins_partitions)){

        temp_partition <- origins_partitions[[i]]
        temp_export_path <- file.path(output_path, 'tables', paste0('petition_origins_', i, '.csv'))
        write.csv(temp_partition, file = temp_export_path, row.names = F, quote = F)

      }

    } # All Courts

    {

      origins_circuits <- dockets %>%
        mutate(type = case_when(
          .default = 'Cert_Petition',
          grepl('a', docket, ignore.case = T) ~ 'Application',
          grepl('m', docket, ignore.case = T) ~ 'Motion')) %>%
        select(lower_ct, type) %>%
        filter(!is.na(lower_ct)) %>%
        filter(grepl('United States Court of Appeals for the ', lower_ct, ignore.case = T)) %>%
        group_by(lower_ct, type) %>%
        summarise(count = n(), .groups= 'drop') %>%
        mutate(lower_ct = gsub('United States Court of Appeals for the ', '', lower_ct),
               lower_ct = gsub('\\,', '', lower_ct),
               lower_ct = gsub('Federal Circuit', 'Federal', lower_ct),
               lower_ct = gsub('District of Columbia Circuit', 'District of Columbia', lower_ct),
               lower_ct = factor(lower_ct, levels = c('First Circuit', 'Second Circuit', 'Third Circuit', 'Fourth Circuit', 'Fifth Circuit', 'Sixth Circuit', 'Seventh Circuit', 'Eighth Circuit', 'Ninth Circuit', 'Tenth Circuit', 'Eleventh Circuit', 'District of Columbia', 'Federal', 'Armed Forces'))) %>%
        pivot_wider(names_from = type, values_from = count) %>%
        arrange(lower_ct) %>%
        select(lower_ct, Cert_Petition, Application, Motion)

      temp_export_path <- file.path(output_path, 'tables', paste0('circuit_origins.csv'))
      write.csv(origins_circuits, file = temp_export_path, row.names = F, quote = F)

    } # Just Circuits

    message('Completed Source Origins of Filings')


  } # Source of Filings (CSV)

  {

    ifp_paid <- dockets %>%
      filter(grepl('-', docket)) %>%
      mutate(number = as.numeric(gsub('.*\\-', '', docket)),
             type = ifelse(number >= 5000, 'IFP', 'Paid')) %>%
      group_by(type) %>%
      summarise(count = n())

    temp_export_path <- file.path(output_path, 'tables', 'ifp_paid.csv')
    write.csv(ifp_paid, file = temp_export_path, row.names = F, quote = F)

    message('Completed IFP v. Paid Filings')


  } # Paid v. IFP

  {

    amici_combined <- data.frame()

    for (i in 1:nrow(dockets)){

      temp_docket <- dockets[i,] # Gather Temp Docket
      temp_docket_sheet <- temp_docket$docket_entries[[1]] %>%
        filter(grepl('(Amicus|Amici)', entry, ignore.case = T)) %>%
        filter(grepl('Filed', entry, ignore.case = T)) %>%
        filter(grepl('Distributed', entry, ignore.case = T))


      number_amici <- nrow(temp_docket_sheet)

      amici_combined <- bind_rows(amici_combined, data.frame(docket = temp_docket$docket,
                                                             amici = number_amici))

    }

    amici_filings_variance <- amici_combined %>%
      filter(grepl('\\-', docket)) %>%
      group_by(amici) %>%
      summarise(count = n())

    temp_export_path <- file.path(output_path, 'tables', 'amici_variance.csv')
    write.csv(amici_filings_variance, file = temp_export_path, row.names = F, quote = F)

    message('Completed Amici Analyses')


  } # Amicus

} # Docket Analysis



