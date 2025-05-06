################################################################################
# Scotuswatch Analyses Retrieval
# Jake S. Truscott & Adam Feldman
# Created April 2025
################################################################################

################################################################################
# Load SCOTUSWatch source
################################################################################

source('code/R/scotuswatch_source.R') # Load Source & Functions (Load Packages Too)

################################################################################
# Move Copy of Statpack to 'releases' folder
################################################################################

file.copy(from = 'Stat Reviews/OT24_StatReview/compilation/compilation_main_StatReview_OT24.pdf', to = 'Stat Reviews/OT24_StatReview/releases/OT24_extended.pdf', overwrite = TRUE) # Copy Extended Release to Releases


###############################################################################
# Oral Argument Sittings
###############################################################################


sittings_files <- list.files('Stat Reviews/OT24_StatReview/oral_arguments/sittings/input', full.names = T)

for (i in 1:length(sittings_files)){

  temp_sitting_name <- gsub('\\.csv', '', gsub('.*sitting\\_', '', sittings_files[i]))
  oral_argument_sittings(sitting_path = sittings_files[i],
                         output_path = 'Stat Reviews/OT24_StatReview/oral_arguments/sittings/output',
                         output_month = as.character(temp_sitting_name))


} # Run for Each Month



###############################################################################
# Oyez Scraper
###############################################################################

cases_master <- read.csv('Stat Reviews/OT24_StatReview/cases_master/cases_master_file_OT24.csv', as.is = T)
unique_sittings <- unique(cases_master$sitting)

for (i in 1:length(unique_sittings)){

  temp_sitting <- unique_sittings[i] # Get Temp Sitting
  temp_cases <- cases_master %>%
    filter(sitting == unique_sittings[i]) # Get Temp Cases
  temp_dockets <- temp_cases$docket
  temp_output_name <- paste0('OT_24_', temp_sitting) # Create Output Name

  temp <- oyez_transcript_search(docket = temp_dockets,
                                 term = '2024',
                                 output_path = 'Stat Reviews/OT24_StatReview/oral_arguments/transcripts',
                                 output_name = temp_output_name)

  message('------ Completed ', temp_sitting)

}


###############################################################################
# Compile Term-Level SCOTUS Transcripts Frame (Combined) == Export
###############################################################################

combined_transcript <- combined_transcripts_term(
  transcripts_folder = 'Stat Reviews/OT24_StatReview/oral_arguments/transcripts',
  output_folder = 'data/term_level_combined_transcripts',
  output_name = 'scotus_OT24')



###############################################################################
# Oral Argument Analyses
# Justices & Attorney Participation
###############################################################################

combined_transcript <- get(load('data/term_level_combined_transcripts/scotus_OT24.rdata'))

oa_analysis(transcript = combined_transcript,
            check_folder_status = T,
            master_file = cases_master,
            output_path = 'Stat Reviews/OT24_StatReview/oral_arguments/analyses')



################################################################################
# Attorney Information Template & Analysis
################################################################################

combined_transcript <- get(load('data/term_level_combined_transcripts/scotus_OT24.rdata'))

attorney_information_template(transcript = combined_transcript,
                              master_file = cases_master,
                              output_path = 'Stat Reviews/OT24_StatReview/oral_arguments/attorney_information')



################################################################################
# Decisions Tables
################################################################################

decisions_analysis(input_path = "C:/Users/jaketruscott/Github/scotuswatch/Stat Reviews/OT24_StatReview/decisions/data/OT_24_Decisions.csv",
                output_path = "C:/Users/jaketruscott/Github/scotuswatch/Stat Reviews/OT24_StatReview/decisions",
                output_type = 'html',
                cases_break = 12,
                master_file = cases_master,
                remove_existing_files = T,
                current_term = '2024')


################################################################################
# Recover Justia Opinions
################################################################################

justia_opinion_recovery(opinions_path = "C:/Users/jaketruscott/Github/scotuswatch/Stat Reviews/OT24_StatReview/decisions",
                        current_term = 2024,
                        master_file = cases_master)


################################################################################
# Docket Recovery
################################################################################

petitions <- c(1:1129, 5001:7125)
applications <- c(1:1050)
motions <- c(1:88)
dockets <- c(paste0('24-', petitions), paste0('24a', applications), paste0('24m', motions))
temp_output_path <- 'Stat Reviews/OT24_StatReview/dockets/processed_dockets'
completed_dockets <- gsub('\\.rdata', '', list.files(temp_output_path))
dockets <- dockets[!dockets %in% completed_dockets]

for (docket in 1:length(dockets)){

  temp_docket <- dockets[docket]
  temp_output_path <- 'Stat Reviews/OT24_StatReview/dockets/processed_dockets'
  completed_dockets <- gsub('\\.rdata', '', list.files(temp_output_path))

  if (temp_docket %in% completed_dockets){
    next
  } else {
    docket_search(docket_id = temp_docket,
                  output_path = temp_output_path) # Search for Docket
  } # If Not Already Collected -- Grab it

  if (docket %% 25 == 0){
    message('Completed ', docket, ' of ', length(dockets))
  }

  Sys.sleep(5)




} # Collect Dockets


################################################################################
# Docket Analysis (Combine & Analyze)
################################################################################

source('code/R/scotuswatch_source.R') # Load Source & Functions (Load Packages Too)

dockets_combine(dockets_path = 'Stat Reviews/OT24_StatReview/dockets/processed_dockets',
                output_path = 'Stat Reviews/OT24_StatReview/dockets/combined_dockets/combined_dockets_OT24.rdata') # Combine Dockets to Single Frame

dockets_analysis(combined_dockets <- 'Stat Reviews/OT24_StatReview/dockets/combined_dockets/combined_dockets_OT24.rdata',
                output_path = 'Stat Reviews/OT24_StatReview/dockets/analysis')
