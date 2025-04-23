########################################################
# Import Packages
########################################################
import pandas as pd
import requests
import oyez_api_wrapper

########################################################
# Open CSV w/ Oral Arguments
########################################################
arguments_frame = pd.read_csv('ot_24_arguments/ot_24_arguments_list.csv') # Load CSV
arguments_frame = arguments_frame[arguments_frame['run'] == 1] # Filter to Where 'Run' = 1

########################################################
# Open Media Links Csv
########################################################
media_links_frame = pd.read_csv('ot_24_arguments/ot_24_media_links.csv') 

########################################################
# Initialize a list to hold new rows
########################################################
new_rows = []

for index, row in arguments_frame.iterrows():
    temp_term = row['term']
    temp_sitting = row['sitting']
    temp_docket_number = row['docket']
    print(f'Compiling For {temp_term} Term Case: {temp_docket_number}, ({temp_sitting})')
    
    temp_case_obj = oyez_api_wrapper.court_case(str(temp_term), str(temp_docket_number))
    temp_audio_links = temp_case_obj.get_audio_links() 
    
    # Check if there are audio links and append to new_rows
    for title, link in temp_audio_links.items():
        new_rows.append({
            'term': temp_term,
            'docket': temp_docket_number,
            'sitting': temp_sitting,
            'audio_title': title,
            'audio_link': link
        })
    print(temp_audio_links)

########################################################
# Convert new rows to a DataFrame
########################################################
new_rows_df = pd.DataFrame(new_rows)

########################################################
# Append the new rows DataFrame to the media_links_frame
########################################################
media_links_frame = pd.concat([media_links_frame, new_rows_df], ignore_index=True)

########################################################
# Save the updated DataFrame back to the CSV
########################################################
media_links_frame.to_csv('ot_24_arguments/ot_24_media_links.csv', index=False)
