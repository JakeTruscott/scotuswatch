################################################################################
# Scotuswatch Analyses Retrieval
# Jake S. Truscott & Adam Feldman
# Created April 2025
################################################################################

################################################################################
# Load SCOTUSWatch source
################################################################################

source('code/R/scotuswatch_source.R') # Load Source & Functions (Load Packages Too)


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


files <- list.files('Stat Reviews/OT24_StatReview/oral_arguments/sittings/input', full.names = T)

for (i in 2:length(files)){
  temp_file <- read.csv(files[i], as.is = T)
  temp_sitting <- stringr::str_to_title(gsub('.*sitting\\_', '', gsub('\\.csv', '', files[i])))
  temp_output_name = paste0('OT_24_', temp_sitting)

  temp_dockets <- temp_file %>%
    select(docket) %>%
    mutate(docket = gsub(' .*', '', docket)) %>%
    pull(docket)

  temp <- oyez_transcript_search(docket = temp_dockets,
                                 term = '2024',
                                 output_path = 'Stat Reviews/OT24_StatReview/oral_arguments/transcripts',
                                 output_name = temp_output_name)

}




###############################################################################
# Compile Term-Level SCOTUS Transcripts Frame (Combined) == Export
###############################################################################

combined_transcripts <- combined_transcripts_term(
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
            output_path = 'Stat Reviews/OT24_StatReview/oral_arguments/analyses')


################################################################################
# Decisions Tables
################################################################################


decisions_table(input_path = "C:/Users/jaketruscott/Github/scotuswatch/Stat Reviews/OT24_StatReview/decisions/data/OT_24_Decisions.csv",
                output_path = "C:/Users/jaketruscott/Github/scotuswatch/Stat Reviews/OT24_StatReview/decisions/tables",
                output_type = 'html',
                cases_break = 15,
                remove_existing_files = T)

