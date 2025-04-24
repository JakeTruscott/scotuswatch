################################################################################
# SCOTUSWatch - Oral Argument Transcript Parser
# Author: Jake S. Truscott, Ph.D
# Updated March 2024
################################################################################

###############################################################################
#Load Packages & Libraries
###############################################################################
library(dplyr); library(httr); library(tidyr); library(stringr); library(stringi); library(rvest); library(jsonlite)


################################################################################
#Load URL Dataframe
################################################################################
links <- read.csv("oral_argument_oyez/ot_24_arguments/ot_24_media_links.csv", as.is = T)
to_run <- read.csv('oral_argument_oyez//ot_24_arguments/ot_24_arguments_list.csv', as.is = T) %>%
  filter(run == 1)
links <- links[links$docket %in% to_run$docket,]


################################################################################
# Retrieve Transcript - Save as JSON
# If Errors -- Get Missing Links from setdiff()
################################################################################

unique_terms <- unique(links$term)

for (i in unique_terms){

  temp_data <- links %>%
    filter(term == i) # Temp Data for Term[i]

  message('Beginning Retrieval for ', nrow(temp_data), ' Argument Links from ', i, ' Term')

  sittings <- unique(temp_data$sitting) # Unique Sittings for Term[t]
  term_output_path <- paste0('oral_argument_oyez/oral_argument_jsons/') # Output Path for JSONs

  {

    if (!dir.exists(paste0(term_output_path, '/', i))){
      dir.create(paste0(term_output_path, '/', i))
    } # Create Term-Level if Doesn't Already Exist

    for (sitting in sittings){

      term_sitting_output_path = paste0(term_output_path, '/', i, '/', sitting)

      if (!dir.exists(term_sitting_output_path)){
        dir.create(term_sitting_output_path)
      }

    }


  } # Create Folder Paths for JSONS (If Needed)

  for (j in 1:nrow(temp_data)) {

    active_link <- temp_data[j,]

    json_output_file <- paste0(term_output_path, active_link$term, '/', active_link$sitting, '/', active_link$docket, '_', gsub('.*\\/', '', active_link$audio_link), '.json')


    tryCatch({
      url <- active_link$audio_link
      response <- GET(url)

      if (status_code(response) == 200) {
        json_content <- content(response, as = "text", encoding = "UTF-8")  # Specify the encoding
        json_data <- fromJSON(json_content)
        write_json(json_data, json_output_file)

        if (j %% 20 == 0) {
          message('Completed ', j, ' of ', nrow(term_data), ' for (', i, ')')
        }

      } else {
        stop(paste("Failed to retrieve JSON data from the URL with status code:", status_code(response)))
      }
    }, error = function(e) {
      cat("Error downloading file: ", json_output_file, "\n")
    })
  } # Process Audio - Create JSONS


} # Convert Audio Link to JSON Transcript


################################################################################
# Get List of JSON Files After Collection
################################################################################

{

  list_json_files <- function(folder_path) {
    files <- list.files(path = folder_path, full.names = TRUE)
    json_files <- files[grepl(".json$", files, ignore.case = TRUE)]
    return(json_files)
  } #Collect Single Folder

  list_json_files_recursive <- function(folder_path, min_file_size = 2) {
    all_json_files <- character(0)
    items <- list.files(path = folder_path, full.names = TRUE)
    json_files <- items[grepl(".json$", items, ignore.case = TRUE)]

    # Filter JSON files by file size
    for (json_file in json_files) {
      file_info <- file.info(json_file)
      if (file_info$size >= min_file_size * 1024) {  # Convert min_file_size to bytes
        all_json_files <- c(all_json_files, json_file)
      }
    }

    subdirs <- list.dirs(path = folder_path, full.names = TRUE, recursive = FALSE)

    for (subdir in subdirs) {
      subdir_json_files <- list_json_files_recursive(subdir, min_file_size)
      all_json_files <- c(all_json_files, subdir_json_files)
    }

    return(all_json_files)
  } #Recursively Search Folder to Folder

  directory_path <- "oral_argument_oyez/oral_argument_jsons/2024"

  min_file_size_kb <- 2 #Only Keep Files Greater than 2kb (Indicating They Aren't Empty)
  transcript_json_list <- list_json_files_recursive(directory_path)


} # Filter to Only Non_Empty Transcripts...

{

  output_directory = "oral_argument_oyez/oral_arguments_processed/2024"

  files <- data.frame(files = transcript_json_list)
  files <- data.frame(files = files)

  json_files <- files %>%
    mutate(sitting = gsub('(.*\\/2023\\/|.*\\/2024\\/|.*\\/2022\\/)', '', files),
           sitting = gsub('\\/.*', '', sitting))

  for (sitting in unique(json_files$sitting)){
    files <- json_files$files[json_files$sitting == sitting] #Subset Files to sitting
    transcripts <- data.frame() #Create Empty Frame

    message('Beginning ', sitting, "\n")

    for (i in 1:length(files)) {
      tryCatch({
        file_path <- files[i]
        json_text <- paste0(readLines(file_path), collapse = "\n")
        parsed_json <- fromJSON(json_text)

        docket <- gsub(".*\\/", "", files[i])
        docket <- gsub("\\_.*", "", docket)

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
            row_id = k
          )

          temp_complete <- bind_rows(temp_complete, temp_full)
        }


        temp_complete <- temp_complete %>%
          mutate(
            argument_duration = temp_meta$argument_duration,
            sitting = sitting,
            docket = docket
          ) %>%
          relocate(sitting, case_name, docket) %>%
          mutate(role = ifelse(is.na(speaker), NA, role)) %>%
          mutate(role = ifelse(grepl('Justice', role, ignore.case = T), 'Justice', role))

        transcripts <- bind_rows(transcripts, temp_complete)

        if (i %% 20 == 0) {
          message("          Completed ", temp_meta$case_name, "   -- ", i, " of ", length(files))
        }



      }, error = function(e) {
        message("Error processing file ", i, ": ", e$message, "...Moving On")
      })
    }

    active_data_term <- paste0('OT_24_', sitting)
    assign(active_data_term, transcripts)

    save(transcripts, file = file.path(output_directory, paste0(active_data_term, ".RData")))

    message('Completed ', sitting, ' Sitting...Moving on\n')

    objects_to_remove <- ls(pattern = '^transcripts|^temp')
    rm(list = objects_to_remove) #Remove from Global Before Moving on - Saves Spac


  } #Run Loop Across JSON


} #Convert to Processed RDATA


transcript_list <- list.files("oral_argument_oyez/oral_arguments_processed/2024", full.names = T) # Retrieve All Individual Processed Transcripts


################################################################################
#Put Into OT2024 Dataframe
################################################################################
scotus_OT24 <- data.frame()

for (file in transcript_list) {
  loaded_data <- get(load(file))
  scotus_OT24 <- bind_rows(scotus_OT24, loaded_data)
} # Load each RData file and store the objects in the list

{
  scotus_OT24 <- scotus_OT24 %>%
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
} #Specialized Fixes for OT2024


################################################################################
#Save as Excel & rdata
################################################################################

save(scotus_OT24, file = "C:/Users/jaketruscott/Github/scotustext/Data/scotus_transcripts_24.rdata")
writexl::write_xlsx(scotus_OT24, path = "C:/Users/jaketruscott/Github/scotustext/Data/scotus_transcripts_24.xlsx")


#save(scotus_OT24, file = "C:/Users/Jake Truscott/Documents/Github/scotustext/Data/scotus_transcripts_24.rdata")
#writexl::write_xlsx(scotus_OT24, path = "C:/Users/Jake Truscott/Documents//Github/scotustext/Data/scotus_transcripts_24.xlsx")


