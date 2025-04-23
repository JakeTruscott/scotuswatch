###############################################################################
# SCOTUS TEXT V2 -- OYEZ Oral Argument Retrieval
# Jake S. Truscott
# Updated December 2024
# For Usage, Please Assign Proper Citation to the Oyez Project
###############################################################################

###############################################################################
# Code Description
###############################################################################
"
This code allows for docket-level retrieval of Supreme Court argument transcripts from the Oyez Project API. Using a query structure, researchers will be able to input term and docket parameters to retrieve fully parsed transcripts, organized in order of utterance with proper indications of speaker.

The routine follows:
1) The Oyez API scraper is built as a Python module, so R bridges using reticulate::() to mimic a python environment. From there, the code builds a temporary virtual environment (venv) to house the relevant code, files, and processes.
2) Using your query parameters (term and docket number), it constructs and saves a temporary python file, executes the request to the API, returns the relevent API location of the transcript, then deletes all the temporary files.
3) Using R (rvest package), R retrieves the JSON file from the Oyez API.
4) Depending on the number of arguments (e.g., some arguments like Roe v. Wade were argued multiple times, or across several days...), R will autoamtically parse the transcript into a dataframe object and assign necessary metadata.
5) The dataframe object is returned and a compilation message will print indicating the successful retrieval of the transcript.

Any questions can be directed to jaketruscott@ufl.edu

NOTE: This code should *not* require downloading Python or an interpreter. So long as you are compiling using the RStudio IDE, this function should be able to compile using reticulate::() without adjusting the global environment to Python.

"

###############################################################################
# Libraries
###############################################################################

library(reticulate); library(jsonlite); library(dplyr); library(httr); library(tidyr); library(stringr); library(stringi); library(rvest); library(jsonlite)


###############################################################################
# Dependency Functions
###############################################################################


oyez_transcript_search <- function(docket = NULL, # Docket Number (Character, Req.)
                            term = NULL){ # Term (Character, Req)

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

  check_venv <- function(){

    env_name <- "~/.virtualenvs/r-reticulate"

    if (is.null(py_config()$python) || !grepl('virtualenvs/r-reticulate', py_config()$python)) {
      virtualenv_create(env_name)
      use_virtualenv(env_name, required = TRUE)
      message("Virtual Environment Created & Activated")
    }
  } # Check if venv is active

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
        response <- GET(url)

        if (status_code(response) == 200) {
          json_content <- content(response, as = "text", encoding = "UTF-8")  # Specify the encoding
          parsed_json <- fromJSON(json_content)

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
  argument_links <- run_oyez_script(term = term, docket = docket)

  transcripts <- data.frame()

  for (i in 1:length(argument_links)){

    temp_transcript <- retrieve_argument_transcript(api_location = argument_links[[i]], docket = docket, term = term)
    transcripts <- bind_rows(transcripts, temp_transcript)

  }

  title_date_combinations = paste(unique(transcripts$case_name), ' ', unique(transcripts$object_title))

  message('Completed Transcript Compilation for:')
  for (i in 1:length(title_date_combinations)){
    message(title_date_combinations[i])
  }

  return(transcripts)


} # Main Function - Returns Transcript Object


###############################################################################
# Test Function - Roe v. Wade (2 Arguments) & Dobbs (1 Argument)
###############################################################################

roe_test <- oyez_transcript_search(docket = '70-18',
                                   term = '1971')

dobbs_test <- oyez_transcript_search(docket = '19-1392',
                                     term = '2021')
