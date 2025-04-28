################################################################################
# SCOTUSWatch - Oral Argument Summary Statistics
# Author: Jake S. Truscott, Ph.D
# Updated March 2024
################################################################################

################################################################################
#Load Packages & Libraries
################################################################################
library(kableExtra); library(dplyr); library(tidyr); library(scotustext); library(htmltools); library(ggplot2); library(png); library(dplyr); library(stringi); library(stringr); library(ggplot2); library(ggthemes); library(anytime); library(tm); library(scotustext); library(readxl); library(ggpattern); library(png); library(ggtext); library(grid); library(wesanderson); library(tidyr)



################################################################################
# Justice Images
# Note: Set Directory to Folder Containing Data Repository (Will Include 'justice_images' folder...)
################################################################################
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







} #Justice Images
{

  shorthand_case_names <- read.csv("oral_argument_oyez/ot_24_arguments/shorthand_case_names.csv", as.is = T)

  scotus_OT24 <- scotus_OT24 %>%
    left_join(shorthand_case_names, by = 'docket') %>%
    mutate(case_name = short_hand)


} # Replace Case Name w/ Shorthand

################################################################################
# OT 2024 (Totals -- Justices)
################################################################################


{

  {
    oa <- scotus_OT24 %>%
      filter(speaker_type == 'Justice') %>%
      group_by(speaker, case_name) %>%
      summarise(total_word_count = sum(word_count)) %>%
      pivot_wider(names_from = speaker, values_from = total_word_count, names_prefix = "word_count_")

    names(oa) <- gsub('(CHIEF JUSTICE |JUSTICE )', '', names(oa))
    names(oa) <- gsub('word_count_', '', names(oa))


    totals <- oa %>%
      select(-case_name) %>%
      summarise_all(.funs = sum, na.rm = T)

    totals <- cbind(data.frame('case_name' = 'Totals'), totals)

    oa <- oa %>%
      mutate(case_name = ifelse(grepl('Kinder Morgan', case_name), 'Ohio, Et Al. Applicants v. Epa (Consolidated w/ 23A350, 23A351, 23A384)', case_name)) %>%
      rename(' ' = case_name)

    oa_data <- oa
    matching_columns <- intersect(colnames(oa_data), names(justice_image_labels))

    original_column_names <- colnames(oa_data)

    oa_data <- oa_data %>%
      rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])

  } #Process Data

  {
    colfunc <- colorRampPalette(c("grey50", "olivedrab"))(9)

    colored_data <- data.frame() %>%
      bind_rows(oa_data[1,])

    for (i in 1:nrow(oa_data)){

      temp_data <- oa_data[i,]
      temp_case <- temp_data[,1]
      colnames(temp_data) <- NULL
      combined_values <- c(temp_data[, 1])
      values <- unlist(temp_data[, -1])
      values[is.na(values)] <- 0
      unique_values <- unique(values)

      if (length(unique_values) > 1) {
        unique_breaks <- quantile(unique_values, probs = seq(0, 1, length.out = 10), na.rm = TRUE)
      } else {
        unique_breaks <- c(min(values) - 1, max(values) + 1) # Ensure we have at least 2 breaks
      }

      color_index <- as.numeric(cut(values, breaks = unique_breaks, labels = 1:9, include.lowest = TRUE))
      values <- cell_spec(values, color = 'white', bold = T, background = colfunc[color_index], font_size = 'large')

      temp_combined_df <- data.frame(ncol = 10)
      for (value in 1:length(values)){
        temp_combined_df[, value] <- values[value]}
      temp_combined_df <- data.frame(temp_case, temp_combined_df)
      colnames(temp_combined_df) <- NULL

      colored_data[i, ] <- temp_combined_df

    } # Assign Colors to Cells


  } # Cell Color Assignment

  {

    oa_words_table <- colored_data %>%
      kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
      add_header_above(original_column_names) %>%
      column_spec(1, bold = TRUE, border_right = TRUE) %>%
      #row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
      row_spec(0,
               bold = TRUE,
               color = 'white',
               background = '#080808',
               align = 'center',
               extra_css = "padding: 0; margin: 0;") %>%
      row_spec(seq(1, nrow(colored_data), 1), align = 'center') %>%
      kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive"))


    oa_words_table

  } #Totals By Argument Table

  html_output <- as.character(oa_words_table)
  writeLines(html_output, 'stat_pack_OT24/Oral Arguments/Combined/combined_total_words.txt')


} # Word Totals Table

{

  {
    oa <- scotus_OT24 %>%
      filter(speaker_type == 'Justice') %>%
      group_by(speaker, case_name) %>%
      summarise(total_word_count = sum(word_count)) %>%
      pivot_wider(names_from = speaker, values_from = total_word_count, names_prefix = "word_count_")

    names(oa) <- gsub('(CHIEF JUSTICE |JUSTICE )', '', names(oa))
    names(oa) <- gsub('word_count_', '', names(oa))


    totals <- oa %>%
      select(-case_name) %>%
      summarise_all(.funs = sum, na.rm = T)

    totals <- cbind(data.frame('case_name' = 'Totals'), totals)

    oa <- oa %>%
      mutate(case_name = ifelse(grepl('Kinder Morgan', case_name), 'Ohio, Et Al. Applicants v. Epa (Consolidated w/ 23A350, 23A351, 23A384)', case_name)) %>%
      rename(' ' = case_name)

    oa_data <- oa
    matching_columns <- intersect(colnames(oa_data), names(justice_image_labels))

    original_column_names <- colnames(oa_data)


  } #Process Data

  {



    figure_data <- oa_data %>%
      pivot_longer(cols = -1, names_to = "justice", values_to = "count") %>%
      mutate(count = ifelse(is.na(count), 0, count)) %>%
      group_by(justice) %>%
      summarize(total = sum(count), .groups = "drop") %>%
      mutate(image_labels = justice_image_labels[match(justice, names(justice_image_labels))],
             image_labels = gsub(' style\\=.*', '', image_labels),
             image_labels = paste0(image_labels, " width='100' /><br><strong>", justice, "</strong>"))

    image_labels <- figure_data$image_labels

    words_spoken_figure <- ggplot(figure_data, aes(x = factor(justice), y = total)) +
      geom_col(aes(fill = total), colour = 'gray5') +
      labs(y = " ",
           x = " ",
           title = " ") +
      scale_fill_gradient(low = 'gray50', high = 'olivedrab') +
      geom_label(aes(label = scales::comma(total)), vjust = 1, size = 4) +
      geom_hline(yintercept = 0) +
      scale_x_discrete(labels = image_labels) +
      theme_classic() +
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



  } # Totals Figure


  ggsave(words_spoken_figure, file = 'stat_pack_OT24/Oral Arguments/Combined/words_spoken_total.png', height = 6, width = 8, units = 'in')



} # Word Totals Figure


{

  {

    time_spoken_total <- scotus_OT24 %>%
      filter(speaker_type == 'Justice') %>%
      mutate(time_spoken = text_stop - text_start) %>%
      group_by(speaker, case_name) %>%
      summarise(total_time_spoken = sum(time_spoken)) %>%
      mutate(total_time_spoken_minutes = round(total_time_spoken/60, 2)) %>%
      pivot_wider(names_from = speaker, values_from = total_time_spoken_minutes, names_prefix = "time_spoken_") %>%
      select(-case_name) %>%
      summarise_all(.funs = sum, na.rm = T) %>%
      mutate(total_time_spoken = rowSums(across(-total_time_spoken))) %>%
      rename_with(~str_replace(., "time_spoken_", ""), -total_time_spoken) %>%
      rename("Total Time\n(Minutes)" = total_time_spoken)


    speaking_data <- time_spoken_total
    matching_columns <- intersect(colnames(speaking_data), names(justice_image_labels))

    original_column_names <- colnames(speaking_data)
    original_column_names[1] <- ' '

    speaking_data <- speaking_data %>%
      rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])


  } # Process Data

  {

    {
      colfunc <- colorRampPalette(c("grey50", "olivedrab"))(9)

      colored_data <- data.frame()

      for (i in 1:nrow(speaking_data)){

        temp_data <- speaking_data[i,]
        temp_case <- temp_data[,1]
        colnames(temp_data) <- NULL
        combined_values <- c(temp_data[, 1])
        values <- unlist(temp_data[, -1])
        values[is.na(values)] <- 0
        unique_values <- unique(values)

        if (length(unique_values) > 1) {
          unique_breaks <- quantile(unique_values, probs = seq(0, 1, length.out = 10), na.rm = TRUE)
        } else {
          unique_breaks <- c(min(values) - 1, max(values) + 1) # Ensure we have at least 2 breaks
        }

        color_index <- as.numeric(cut(values, breaks = unique_breaks, labels = 1:9, include.lowest = TRUE))
        values <- cell_spec(values, color = 'white', bold = T, background = colfunc[color_index], font_size = 'large')

        temp_combined_df <- data.frame(ncol = 10)
        for (value in 1:length(values)){
          temp_combined_df[, value] <- values[value]}
        temp_combined_df <- data.frame(temp_case, temp_combined_df)
        colnames(temp_combined_df) <- NULL

        colored_data <- temp_combined_df

      } # Assign Colors to Cells
      names(colored_data) <- names(speaking_data)


    } # Cell Color Assignment

    {

      oa_time_table <- colored_data %>%
        kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
        add_header_above(original_column_names) %>%
        column_spec(1, bold = TRUE, border_right = TRUE) %>%
        row_spec(0,
                 bold = TRUE,
                 color = 'white',
                 background = '#080808',
                 align = 'center',
                 extra_css = "padding: 0; margin: 0;") %>%
        row_spec(seq(1, nrow(colored_data), 1), align = 'center') %>%
        kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive"))


      oa_time_table

    } #Totals By Argument Table


    html_output <- as.character(oa_time_table)
    writeLines(html_output, 'stat_pack_OT24/Oral Arguments/Combined/combined_total_speaking_time.txt')

  } # Cell Color Assignment

} # Speech Totals Table

{


  {

    time_spoken_total <- scotus_OT24 %>%
      filter(speaker_type == 'Justice') %>%
      mutate(time_spoken = text_stop - text_start) %>%
      group_by(speaker, case_name) %>%
      summarise(total_time_spoken = sum(time_spoken)) %>%
      mutate(total_time_spoken_minutes = round(total_time_spoken/60, 2)) %>%
      pivot_wider(names_from = speaker, values_from = total_time_spoken_minutes, names_prefix = "time_spoken_") %>%
      select(-case_name) %>%
      summarise_all(.funs = sum, na.rm = T) %>%
      mutate(total_time_spoken = rowSums(across(-total_time_spoken))) %>%
      rename_with(~str_replace(., "time_spoken_", ""), -total_time_spoken) %>%
      rename("Total Time\n(Minutes)" = total_time_spoken)


    speaking_data <- time_spoken_total
    matching_columns <- intersect(colnames(speaking_data), names(justice_image_labels))

    original_column_names <- colnames(speaking_data)
    original_column_names[1] <- ' '


  } # Process Data

  {



     figure_data <- speaking_data %>%
      pivot_longer(cols = -1, names_to = "justice", values_to = "count") %>%
      mutate(count = ifelse(is.na(count), 0, count)) %>%
      group_by(justice) %>%
      summarize(total = sum(count), .groups = "drop") %>%
      mutate(image_labels = justice_image_labels[match(justice, names(justice_image_labels))],
             image_labels = gsub(' style\\=.*', '', image_labels),
             image_labels = paste0(image_labels, " width='100' /><br><strong>", justice, "</strong>"))

    image_labels <- figure_data$image_labels

    time_spoken_figure <- ggplot(figure_data, aes(x = factor(justice), y = total)) +
      geom_col(aes(fill = total), colour = 'gray5') +
      labs(y = " ",
           x = " ",
           title = " ") +
      scale_fill_gradient(low = 'gray50', high = 'olivedrab') +
      geom_label(aes(label = scales::comma(total)), vjust = 1, size = 4) +
      geom_hline(yintercept = 0) +
      scale_x_discrete(labels = image_labels) +
      theme_classic() +
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



  } # Totals Figure

  ggsave(time_spoken_figure, file = 'stat_pack_OT24/Oral Arguments/Combined/time_spoken_total.png', height = 6, width = 8, units = 'in')


} # Speech Totals Figure


################################################################################
# OT 2024 (By Sitting -- Justices)
################################################################################

{

  {
    oa <- scotus_OT24 %>%
      filter(speaker_type == 'Justice') %>%
      filter(sitting == 'March') %>%
      group_by(speaker, case_name) %>%
      summarise(total_word_count = sum(word_count)) %>%
      pivot_wider(names_from = speaker, values_from = total_word_count, names_prefix = "word_count_")

    names(oa) <- gsub('(CHIEF JUSTICE |JUSTICE )', '', names(oa))
    names(oa) <- gsub('word_count_', '', names(oa))


    totals <- oa %>%
      select(-case_name) %>%
      summarise_all(.funs = sum, na.rm = T)

    totals <- cbind(data.frame('case_name' = 'Totals'), totals)


    oa_data <- oa
    matching_columns <- intersect(colnames(oa_data), names(justice_image_labels))

    original_column_names <- colnames(oa_data)

    oa_data <- oa_data %>%
      rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])

  } #Process Data

  {
    colfunc <- colorRampPalette(c("grey50", "olivedrab"))(9)

    colored_data <- data.frame()

    for (i in 1:nrow(oa_data)){

      temp_data <- oa_data[i,]
      temp_case <- temp_data[,1]
      colnames(temp_data) <- NULL
      combined_values <- c(temp_data[, 1])
      values <- unlist(temp_data[, -1])
      values[is.na(values)] <- 0
      unique_values <- unique(values)

      if (length(unique_values) > 1) {
        unique_breaks <- quantile(unique_values, probs = seq(0, 1, length.out = 10), na.rm = TRUE)
      } else {
        unique_breaks <- c(min(values) - 1, max(values) + 1) # Ensure we have at least 2 breaks
      }

      color_index <- as.numeric(cut(values, breaks = unique_breaks, labels = 1:9, include.lowest = TRUE))
      values <- cell_spec(values, color = 'white', bold = T, background = colfunc[color_index], font_size = 'large')

      temp_combined_df <- data.frame(ncol = 10)
      for (value in 1:length(values)){
        temp_combined_df[, value] <- values[value]}
      temp_combined_df <- data.frame(temp_case, temp_combined_df)

      colored_data <- bind_rows(colored_data, temp_combined_df)

    } # Assign Colors to Cells
    names(colored_data) <- names(oa_data)
    names(colored_data)[1] <- 'Case'
    original_column_names[1] <- ''


  } # Cell Color Assignment

  {

    oa_speaking_table <- colored_data %>%
      kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
      add_header_above(original_column_names) %>%
      column_spec(1, bold = TRUE, border_right = TRUE) %>%
      row_spec(0,
               bold = TRUE,
               color = 'white',
               background = '#080808',
               align = 'center',
               extra_css = "padding: 0; margin: 0;") %>%
      row_spec(seq(1, nrow(colored_data), 1), align = 'center') %>%
      kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive"))


    oa_speaking_table

  } # Compile Figure

  html_output <- as.character(oa_speaking_table)
  writeLines(html_output, 'stat_pack_OT24/Oral Arguments/March/march_total_words_table.txt')


} # Words by Sitting Table

{

  {

    time_spoken_sitting <- scotus_OT24 %>%
      filter(speaker_type == 'Justice') %>%
      filter(sitting == 'March') %>%
      mutate(time_spoken = text_stop - text_start) %>%
      group_by(speaker, case_name) %>%
      summarise(total_time_spoken = sum(time_spoken)) %>%
      mutate(total_time_spoken_minutes = round(total_time_spoken/60, 2)) %>%
      group_by(case_name) %>%
      pivot_wider(names_from = speaker, values_from = total_time_spoken_minutes, names_prefix = "time_spoken_") %>%
      summarise_all(.funs = sum, na.rm = T) %>%
      mutate(total_time_spoken = rowSums(across(-c(total_time_spoken, case_name)))) %>%
      rename_with(~str_replace(., "time_spoken_", ""), -total_time_spoken) %>%
      rename("Total Time\n(Minutes)" = total_time_spoken)


    speaking_data <- time_spoken_sitting
    matching_columns <- intersect(colnames(speaking_data), names(justice_image_labels))

    original_column_names <- colnames(speaking_data)
    original_column_names[1:2] <- ' '

    speaking_data <- speaking_data %>%
      rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.]) %>%
      rename(`Case` = case_name)


  } # Process Data

  {
    colfunc <- colorRampPalette(c("grey50", "olivedrab"))(9)

    colored_data <- data.frame()

    for (i in 1:nrow(speaking_data)){

      temp_data <- speaking_data[i,]
      temp_case <- temp_data[,1]
      colnames(temp_data) <- NULL
      combined_values <- c(temp_data[, 1])
      values <- unlist(temp_data[, -1])
      values[is.na(values)] <- 0
      unique_values <- unique(values)

      if (length(unique_values) > 1) {
        unique_breaks <- quantile(unique_values, probs = seq(0, 1, length.out = 10), na.rm = TRUE)
      } else {
        unique_breaks <- c(min(values) - 1, max(values) + 1) # Ensure we have at least 2 breaks
      }

      color_index <- as.numeric(cut(values, breaks = unique_breaks, labels = 1:9, include.lowest = TRUE))
      values <- cell_spec(values, color = 'white', bold = T, background = colfunc[color_index], font_size = 'large')

      temp_combined_df <- data.frame(ncol = 10)
      for (value in 1:length(values)){
        temp_combined_df[, value] <- values[value]}
      temp_combined_df <- data.frame(temp_case, temp_combined_df)

      colored_data <- bind_rows(colored_data, temp_combined_df)

    } # Assign Colors to Cells
    names(colored_data) <- names(speaking_data)


  } # Cell Color Assignment

  {

    oa_time_table <- colored_data %>%
      kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
      add_header_above(original_column_names) %>%
      column_spec(1, bold = TRUE, border_right = TRUE) %>%
      row_spec(0,
               bold = TRUE,
               color = 'white',
               background = '#080808',
               align = 'center',
               extra_css = "padding: 0; margin: 0;") %>%
      row_spec(seq(1, nrow(colored_data), 1), align = 'center') %>%
      kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive"))


    oa_time_table

  } # Compile Figure


  html_output <- as.character(oa_time_table)
  writeLines(html_output, 'stat_pack_OT24/Oral Arguments/March/march_total_speaking_time.txt')

} # Speech by Sitting Table


################################################################################
# OT 2024 (By Sitting -- Attorneys)
################################################################################

{

  {


    attorneys <- scotus_OT24 %>%
      mutate(response_to = ifelse(lag(speaker_type) == 'Justice', lag(speaker), NA)) %>%
      filter(speaker_type == "Attorney") %>%
      mutate(speaker = ifelse(speaker == 'MR. SYNDER', 'MR. SNYDER', speaker)) %>%
      filter(!is.na(response_to)) %>%
      group_by(case_name, speaker, response_to) %>%
      summarise(total_words = sum(word_count, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(response_to = gsub("(CHIEF JUSTICE |JUSTICE )", "", response_to)) %>%
      pivot_wider(names_from = response_to, values_from = total_words) %>%
      mutate('Total Words' = rowSums(select(., -c(case_name, speaker)), na.rm = TRUE)) %>%
      relocate('Total Words', .after = speaker) %>%
      rename(' ' = case_name,
             'Attorney' = speaker)

    attorney_participation <- attorneys
    original_column_names <- colnames(attorney_participation)

    original_column_names[1] <- " "
    original_column_names[2] <- " "
    original_column_names[3] <- " "

    attorney_participation <- attorney_participation %>%
      rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])

  } # Process Data

  {
    colfunc <- colorRampPalette(c("grey50", "olivedrab"))(9)

    colored_data <- data.frame() %>%
      bind_rows(attorney_participation[1,])

    for (i in 1:nrow(attorney_participation)){

      temp_data <- attorney_participation[i,]
      temp_case <- temp_data[,1]
      temp_attorney <- temp_data[,2]
      temp_total_words <- temp_data[,3]
      colnames(temp_data) <- NULL
      combined_values <- c(temp_data[, 3:ncol(temp_data)])
      values <- unlist(temp_data[, c(4:ncol(temp_data))])
      values[is.na(values)] <- 0
      unique_values <- unique(values)

      if (length(unique_values) > 1) {
        unique_breaks <- quantile(unique_values, probs = seq(0, 1, length.out = 10), na.rm = TRUE)
      } else {
        unique_breaks <- c(min(values) - 1, max(values) + 1) # Ensure we have at least 2 breaks
      }

      color_index <- as.numeric(cut(values, breaks = unique_breaks, labels = 1:9, include.lowest = TRUE))
      values <- cell_spec(values, color = 'white', bold = T, background = colfunc[color_index], font_size = 'large')

      temp_combined_df <- matrix(nrow = 1, ncol = 9)

      for (value in 1:length(values)){
        temp_combined_df[, value] <- values[value]}
      temp_combined_df <- data.frame(temp_case, temp_attorney, temp_total_words, temp_combined_df)
      colnames(temp_combined_df) <- NULL

      colored_data[i, ] <- temp_combined_df

    } # Assign Colors to Cells


  } # Cell Color Assignment

  {


   attorney_participation_table <- colored_data %>%
      kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
      add_header_above(original_column_names) %>%
      column_spec(1, bold = TRUE, border_right = TRUE) %>%
      row_spec(0,
               bold = TRUE,
               color = 'white',
               background = '#080808',
               align = 'center',
               extra_css = "padding: 0; margin: 0;") %>%
      row_spec(seq(1, nrow(colored_data), 1), align = 'center') %>%
      kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive"))

  } # Attorney Participation Table

  html_output <- as.character(attorney_participation_table)
  writeLines(html_output, 'stat_pack_OT24/Oral Arguments/Combined/combined_attorney_participation.txt')

} # Attorney Engagement Table (Total)

{

  {


    attorneys <- scotus_OT24 %>%
      mutate(response_to = ifelse(lag(speaker_type) == 'Justice', lag(speaker), NA)) %>%
      filter(speaker_type == "Attorney") %>%
      filter(sitting == 'March') %>%
      filter(!is.na(response_to)) %>%
      group_by(case_name, speaker, response_to) %>%
      summarise(total_words = sum(word_count, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(response_to = gsub("(CHIEF JUSTICE |JUSTICE )", "", response_to)) %>%
      pivot_wider(names_from = response_to, values_from = total_words) %>%
      mutate('Total Words' = rowSums(select(., -c(case_name, speaker)), na.rm = TRUE)) %>%
      relocate('Total Words', .after = speaker) %>%
      rename(' ' = case_name,
             'Attorney' = speaker)

    attorney_participation <- attorneys
    original_column_names <- colnames(attorney_participation)

    original_column_names[1] <- " "
    original_column_names[2] <- " "
    original_column_names[3] <- " "

    attorney_participation <- attorney_participation %>%
      rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])

  } # Process Data

  {
    colfunc <- colorRampPalette(c("grey50", "olivedrab"))(9)

    colored_data <- data.frame() %>%
      bind_rows(attorney_participation[1,])

    for (i in 1:nrow(attorney_participation)){

      temp_data <- attorney_participation[i,]
      temp_case <- temp_data[,1]
      temp_attorney <- temp_data[,2]
      temp_total_words <- temp_data[,3]
      colnames(temp_data) <- NULL
      combined_values <- c(temp_data[, 3:ncol(temp_data)])
      values <- unlist(temp_data[, c(4:ncol(temp_data))])
      values[is.na(values)] <- 0
      unique_values <- unique(values)

      if (length(unique_values) > 1) {
        unique_breaks <- quantile(unique_values, probs = seq(0, 1, length.out = 10), na.rm = TRUE)
      } else {
        unique_breaks <- c(min(values) - 1, max(values) + 1) # Ensure we have at least 2 breaks
      }

      color_index <- as.numeric(cut(values, breaks = unique_breaks, labels = 1:9, include.lowest = TRUE))
      values <- cell_spec(values, color = 'white', bold = T, background = colfunc[color_index], font_size = 'large')

      temp_combined_df <- matrix(nrow = 1, ncol = 9)

      for (value in 1:length(values)){
        temp_combined_df[, value] <- values[value]}
      temp_combined_df <- data.frame(temp_case, temp_attorney, temp_total_words, temp_combined_df)
      colnames(temp_combined_df) <- NULL

      colored_data[i, ] <- temp_combined_df

    } # Assign Colors to Cells


  } # Cell Color Assignment

  {


    attorney_participation_table <- colored_data %>%
      kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
      add_header_above(original_column_names) %>%
      column_spec(1, bold = TRUE, border_right = TRUE) %>%
      row_spec(0,
               bold = TRUE,
               color = 'white',
               background = '#080808',
               align = 'center',
               extra_css = "padding: 0; margin: 0;") %>%
      row_spec(seq(1, nrow(colored_data), 1), align = 'center') %>%
      kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
      footnote(
        general = c(
          "<div style='font-size: 15px;'>Note: Values corresponding with Justice columns indicates volume of words they offered in response to arguing attorney</div>"
        ),
        general_title = "<br>",
        footnote_as_chunk = TRUE,
        escape = FALSE
      )


    attorney_participation_table

  } # Attorney Participation Table

  html_output <- as.character(attorney_participation_table)
  writeLines(html_output, 'stat_pack_OT24/Oral Arguments/March/march_attorney_participation.txt')

} # Attorney Engagement Table (By Sitting)






















#######################################################################################################################
# OT 2023 & 22 (Old code)
################################################################################

{


  ################################################################################
  # OT 2023 (Aggregate Total Term Stats - Words)
  ################################################################################
  {

    base_url <- "https://github.com/JakeTruscott/scotustext/raw/master/Data/"
    rdata_url <- paste0(base_url, "scotus_transcripts_23.rdata")
    load(url(rdata_url))

    {
      oa <- scotus_OT24 %>%
        filter(speaker_type == 'Justice') %>%
        group_by(speaker, case_name) %>%
        summarise(total_word_count = sum(word_count)) %>%
        pivot_wider(names_from = speaker, values_from = total_word_count, names_prefix = "word_count_")

      names(oa) <- gsub('(CHIEF JUSTICE |JUSTICE )', '', names(oa))
      names(oa) <- gsub('word_count_', '', names(oa))


      totals <- oa %>%
        select(-case_name) %>%
        summarise_all(.funs = sum, na.rm = T)

      totals <- cbind(data.frame('case_name' = 'Totals'), totals)

      oa <- oa %>%
        mutate(case_name = ifelse(grepl('Kinder Morgan', case_name), 'Ohio, Et Al. Applicants v. Epa (Consolidated w/ 23A350, 23A351, 23A384)', case_name)) %>%
        rename(' ' = case_name)
    } #Process Data

    {
      oa_data <- oa
      matching_columns <- intersect(colnames(oa_data), names(justice_image_labels))

      original_column_names <- colnames(oa_data)

      oa_data <- oa_data %>%
        rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])


      oa_table <- oa_data %>%
        kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
        add_header_above(original_column_names) %>%
        column_spec(1, bold = TRUE, border_right = TRUE) %>%
        row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
        row_spec(seq(1, nrow(oa_data), 1), align = 'center') %>%
        kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
        column_spec(2, color = "white", background = spec_color(oa$ROBERTS, end = 0.5), popover = paste("am:", oa$ROBERTS)) %>%
        column_spec(3, color = "white", background = spec_color(oa$ALITO, end = 0.5), popover = paste("am:", oa$ALITO)) %>%
        column_spec(4, color = "white", background = spec_color(oa$BARRETT, end = 0.5), popover = paste("am:", oa$BARRETT)) %>%
        column_spec(5, color = "white", background = spec_color(oa$GORSUCH, end = 0.5), popover = paste("am:", oa$GORSUCH)) %>%
        column_spec(6, color = "white", background = spec_color(oa$JACKSON, end = 0.5), popover = paste("am:", oa$JACKSON)) %>%
        column_spec(7, color = "white", background = spec_color(oa$KAGAN, end = 0.5), popover = paste("am:", oa$KAGAN)) %>%
        column_spec(8, color = "white", background = spec_color(oa$KAVANAUGH, end = 0.5), popover = paste("am:", oa$KAVANAUGH)) %>%
        column_spec(9, color = "white", background = spec_color(oa$SOTOMAYOR, end = 0.5), popover = paste("am:", oa$SOTOMAYOR)) %>%
        column_spec(10, color = "white", background = spec_color(oa$THOMAS, end = 0.5), popover = paste("am:", oa$THOMAS)) %>%
        scroll_box(height = "500px", extra_css = "thead th { position: sticky; top: 0; background: #080808; color: white; }")



      oa_table

      save_kable(oa_table, "C:/Users/Jake Truscott/Desktop/test.html")

    } #By Argument Table
    save_kable(oa_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/oa_table_23_active.html")
    html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/oa_table_23_active.html"
    #webshot::install_phantomjs(force = T)
    phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
    webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/oa_table_23_active.png", vwidth = 900, vheight = 70)

    {



      totals_data <- totals
      matching_columns <- intersect(colnames(totals_data), names(justice_image_labels))
      original_column_names <- colnames(totals_data)
      original_column_names[1] <- " "

      totals_data <- totals_data %>%
        rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])

      totals_table <- totals_data %>%
        rename(' ' = case_name) %>%
        kbl(longtable = T, escape = F, booktabs =  T, align = "c") %>%
        add_header_above( original_column_names) %>%
        row_spec(1, align = 'center') %>%
        column_spec(1, bold = T, border_right = T) %>%
        row_spec(0, bold = T, color = 'white', background = '#080808', align = 'center') %>%
        kable_styling(full_width = T, font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive"), latex_options = "striped") %>%
        column_spec(1, bold = T, border_right = T) %>%
        row_spec(0, bold = T, color = 'white', background = '#080808', align = 'center') %>%
        #row_spec(seq(1, nrow(oa_data), 1), align = 'center') %>%
        column_spec(2, color = "white",
                    background = spec_color(totals$ROBERTS, end = 0.5),
                    popover = paste("am:", totals$ROBERTS)) %>%
        column_spec(3, color = "white",
                    background = spec_color(totals$ALITO, end = 0.5),
                    popover = paste("am:", totals$ALITO)) %>%
        column_spec(4, color = "white",
                    background = spec_color(totals$BARRETT, end = 0.5),
                    popover = paste("am:", totals$BARRETT)) %>%
        column_spec(5, color = "white",
                    background = spec_color(totals$GORSUCH, end = 0.5),
                    popover = paste("am:", totals$GORSUCH)) %>%
        column_spec(6, color = "white",
                    background = spec_color(totals$JACKSON, end = 0.5),
                    popover = paste("am:", totals$JACKSON)) %>%
        column_spec(7, color = "white",
                    background = spec_color(totals$KAGAN, end = 0.5),
                    popover = paste("am:", totals$KAGAN)) %>%
        column_spec(8, color = "white",
                    background = spec_color(totals$KAVANAUGH, end = 0.5),
                    popover = paste("am:", totals$KAVANAUGH)) %>%
        column_spec(9, color = "white",
                    background = spec_color(totals$SOTOMAYOR, end = 0.5),
                    popover = paste("am:", totals$SOTOMAYOR)) %>%
        column_spec(10, color = "white",
                    background = spec_color(totals$THOMAS, end = 0.5),
                    popover = paste("am:", totals$THOMAS)) %>%
        scroll_box(height = "500px", extra_css = "thead th { position: sticky; top: 0; background: #080808; color: white; }")




      totals_table
    } #Totals Table (Active)
    save_kable(totals_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/totals_table_OT23_active.html")

    html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/totals_table_OT23_active.html"
    #webshot::install_phantomjs(force = T)
    #phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
    webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/totals_table_OT23_active.png", vwidth = 900, vheight = 70)

    {
      custom_colors <- c("#5FA934", "#3B5E8B", "#522559")

      scotus_term <- scotus_OT24 %>%
        filter(speaker_type == 'Justice') %>%
        group_by(speaker, case_name) %>%
        summarise(total_word_count = sum(word_count)) %>%
        group_by(speaker) %>%
        summarise(total_word_count = sum(total_word_count)) %>%
        mutate(speaker = gsub("(CHIEF JUSTICE |JUSTICE )", "", speaker))

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

      word_count_plot

    } #Totals Graph (Active)
    ggsave("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/word_count_plot_OT23.png", word_count_plot, dpi = 300)

    {


      attorneys <- scotus_OT24 %>%
        mutate(response_to = ifelse(lag(speaker_type) == 'Justice', lag(speaker), NA)) %>%
        filter(speaker_type == "Attorney") %>%
        mutate(speaker = ifelse(speaker == 'MR. SYNDER', 'MR. SNYDER', speaker)) %>%
        filter(!is.na(response_to)) %>%
        group_by(case_name, speaker, response_to) %>%
        summarise(total_words = sum(word_count, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(response_to = gsub("(CHIEF JUSTICE |JUSTICE )", "", response_to)) %>%
        pivot_wider(names_from = response_to, values_from = total_words) %>%
        mutate('Total Words' = rowSums(select(., -c(case_name, speaker)), na.rm = TRUE)) %>%
        relocate('Total Words', .after = speaker) %>%
        rename(' ' = case_name,
               'Attorney' = speaker)


      attorney_participation <- attorneys
      original_column_names <- colnames(attorney_participation)

      original_column_names[1] <- " "
      original_column_names[2] <- " "
      original_column_names[3] <- " "

      attorney_participation <- attorney_participation %>%
        rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])


      attorney_participation_table <- attorney_participation %>%
        kbl(longtable = T, escape = F, booktabs =  T, align = "l") %>%
        add_header_above( original_column_names) %>%
        row_spec(1, align = 'center') %>%
        column_spec(1, bold = T, border_right = T) %>%
        row_spec(0, bold = T, color = 'white', background = '#080808', align = 'center') %>%
        kable_styling(full_width = T, font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive"), latex_options = c("striped", "HOLD_position")) %>%
        column_spec(1, bold = T, border_right = T) %>%
        row_spec(0, bold = T, color = 'white', background = '#080808', align = 'center') %>%
        row_spec(seq(1, nrow(oa_data), 1), align = 'center') %>%
        column_spec(4, color = "white",
                    background = spec_color(attorneys$ROBERTS, end = 0.5),
                    popover = paste("am:", attorneys$ROBERTS),
                    latex_valign = 'p') %>%
        row_spec(1:dim(attorneys)[1],  align = "c") %>%
        column_spec(5, color = "white",
                    background = spec_color(attorneys$ALITO, end = 0.5),
                    popover = paste("am:", attorneys$ALITO)) %>%
        column_spec(6, color = "white",
                    background = spec_color(attorneys$BARRETT, end = 0.5),
                    popover = paste("am:", attorneys$BARRETT)) %>%
        column_spec(7, color = "white",
                    background = spec_color(attorneys$GORSUCH, end = 0.5),
                    popover = paste("am:", attorneys$GORSUCH)) %>%
        column_spec(8, color = "white",
                    background = spec_color(attorneys$JACKSON, end = 0.5),
                    popover = paste("am:", attorneys$JACKSON)) %>%
        column_spec(9, color = "white",
                    background = spec_color(attorneys$KAGAN, end = 0.5),
                    popover = paste("am:", attorneys$KAGAN)) %>%
        column_spec(10, color = "white",
                    background = spec_color(attorneys$KAVANAUGH, end = 0.5),
                    popover = paste("am:", attorneys$KAVANAUGH)) %>%
        column_spec(11, color = "white",
                    background = spec_color(attorneys$SOTOMAYOR, end = 0.5),
                    popover = paste("am:", attorneys$SOTOMAYOR)) %>%
        column_spec(12, color = "white",
                    background = spec_color(attorneys$THOMAS, end = 0.5),
                    popover = paste("am:", attorneys$THOMAS))


      attorney_participation_table

    } #Attorneys OA Participation Figure
    save_kable(attorney_participation_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/attorney_participation_23_active.html")

    html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/attorney_participation_23_active.html"
    #webshot::install_phantomjs(force = T)
    #phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
    webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/attorney_participation_23_active.png", vwidth = 800, vheight = 70)


  } #OT 2023 (Totals)

  ################################################################################
  # OT 2023 (By Active Sitting - Words)
  # Currently *April Sitting*
  ################################################################################
  {

    base_url <- "https://github.com/JakeTruscott/scotustext/raw/master/Data/"
    rdata_url <- paste0(base_url, "scotus_transcripts_23.rdata")
    load(url(rdata_url))

    {
      oa <- scotus_OT24 %>%
        filter(speaker_type == 'Justice') %>%
        filter(sitting == 'April') %>%
        group_by(speaker, case_name) %>%
        summarise(total_word_count = sum(word_count)) %>%
        pivot_wider(names_from = speaker, values_from = total_word_count, names_prefix = "word_count_")

      names(oa) <- gsub('(CHIEF JUSTICE |JUSTICE )', '', names(oa))
      names(oa) <- gsub('word_count_', '', names(oa))


      totals <- oa %>%
        select(-case_name) %>%
        summarise_all(.funs = sum, na.rm = T)

      totals <- cbind(data.frame('case_name' = 'Totals'), totals)

      oa <- oa %>%
        mutate(case_name = ifelse(grepl('Kinder Morgan', case_name), 'Ohio, Et Al. Applicants v. Epa (Consolidated w/ 23A350, 23A351, 23A384)', case_name)) %>%
        rename(' ' = case_name)
    } #Process Data

    {
      oa_data <- oa
      matching_columns <- intersect(colnames(oa_data), names(justice_image_labels))

      original_column_names <- colnames(oa_data)

      oa_data <- oa_data %>%
        rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])


      oa_table <- oa_data %>%
        kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
        add_header_above( original_column_names) %>%
        column_spec(1, bold = TRUE, border_right = TRUE) %>%
        row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
        row_spec(seq(1, nrow(oa_data), 1), align = 'center') %>%
        kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
        column_spec(2, color = "white",
                    background = spec_color(oa$ROBERTS, end = 0.5),
                    popover = paste("am:", oa$ROBERTS)) %>%
        column_spec(3, color = "white",
                    background = spec_color(oa$ALITO, end = 0.5),
                    popover = paste("am:", oa$ALITO)) %>%
        column_spec(4, color = "white",
                    background = spec_color(oa$BARRETT, end = 0.5),
                    popover = paste("am:", oa$BARRETT)) %>%
        column_spec(5, color = "white",
                    background = spec_color(oa$GORSUCH, end = 0.5),
                    popover = paste("am:", oa$GORSUCH)) %>%
        column_spec(6, color = "white",
                    background = spec_color(oa$JACKSON, end = 0.5),
                    popover = paste("am:", oa$JACKSON)) %>%
        column_spec(7, color = "white",
                    background = spec_color(oa$KAGAN, end = 0.5),
                    popover = paste("am:", oa$KAGAN)) %>%
        column_spec(8, color = "white",
                    background = spec_color(oa$KAVANAUGH, end = 0.5),
                    popover = paste("am:", oa$KAVANAUGH)) %>%
        column_spec(9, color = "white",
                    background = spec_color(oa$SOTOMAYOR, end = 0.5),
                    popover = paste("am:", oa$SOTOMAYOR)) %>%
        column_spec(10, color = "white",
                    background = spec_color(oa$THOMAS, end = 0.5),
                    popover = paste("am:", oa$THOMAS))


      oa_table

    } #By Argument Table
    save_kable(oa_table, "stat_pack_OT24/Oral Arguments/Test Folder/test_html_figure.html")
    html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/April Sitting 2023/oa_table_23_april.html"
    #webshot::install_phantomjs(force = T)
    phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
    webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/April Sitting 2023/oa_table_23_april.png", vwidth = 1250, vheight = 100)

    html_output <- as.character(oa_table)
    writeLines(html_output, 'stat_pack_OT24/Oral Arguments/Test Folder/test_html_figure.txt')

    {



      totals_data <- totals
      matching_columns <- intersect(colnames(totals_data), names(justice_image_labels))
      original_column_names <- colnames(totals_data)
      original_column_names[1] <- " "

      totals_data <- totals_data %>%
        rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])

      totals_table <- totals_data %>%
        rename(' ' = case_name) %>%
        kbl(longtable = T, escape = F, booktabs =  T, align = "c") %>%
        add_header_above( original_column_names) %>%
        row_spec(1, align = 'center') %>%
        column_spec(1, bold = T, border_right = T) %>%
        row_spec(0, bold = T, color = 'white', background = '#080808', align = 'center') %>%
        kable_styling(full_width = T, font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive"), latex_options = "striped") %>%
        column_spec(1, bold = T, border_right = T) %>%
        row_spec(0, bold = T, color = 'white', background = '#080808', align = 'center') %>%
        #row_spec(seq(1, nrow(oa_data), 1), align = 'center') %>%
        column_spec(2, color = "white",
                    background = spec_color(totals$ROBERTS, end = 0.5),
                    popover = paste("am:", totals$ROBERTS)) %>%
        column_spec(3, color = "white",
                    background = spec_color(totals$ALITO, end = 0.5),
                    popover = paste("am:", totals$ALITO)) %>%
        column_spec(4, color = "white",
                    background = spec_color(totals$BARRETT, end = 0.5),
                    popover = paste("am:", totals$BARRETT)) %>%
        column_spec(5, color = "white",
                    background = spec_color(totals$GORSUCH, end = 0.5),
                    popover = paste("am:", totals$GORSUCH)) %>%
        column_spec(6, color = "white",
                    background = spec_color(totals$JACKSON, end = 0.5),
                    popover = paste("am:", totals$JACKSON)) %>%
        column_spec(7, color = "white",
                    background = spec_color(totals$KAGAN, end = 0.5),
                    popover = paste("am:", totals$KAGAN)) %>%
        column_spec(8, color = "white",
                    background = spec_color(totals$KAVANAUGH, end = 0.5),
                    popover = paste("am:", totals$KAVANAUGH)) %>%
        column_spec(9, color = "white",
                    background = spec_color(totals$SOTOMAYOR, end = 0.5),
                    popover = paste("am:", totals$SOTOMAYOR)) %>%
        column_spec(10, color = "white",
                    background = spec_color(totals$THOMAS, end = 0.5),
                    popover = paste("am:", totals$THOMAS))



      totals_table
    } #Totals Table (Active)
    save_kable(totals_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/April Sitting 2023/totals_table_OT23_april.html")
    html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/April Sitting 2023/totals_table_OT23_april.html"
    webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/April Sitting 2023/totals_table_OT23_april.png", vwidth = 1250, vheight = 100)

    {
      custom_colors <- c("#5FA934", "#3B5E8B", "#522559")

      scotus_term <- scotus_OT24 %>%
        filter(sitting == 'April') %>%
        filter(speaker_type == 'Justice') %>%
        group_by(speaker, case_name) %>%
        summarise(total_word_count = sum(word_count)) %>%
        group_by(speaker) %>%
        summarise(total_word_count = sum(total_word_count)) %>%
        mutate(speaker = gsub("(CHIEF JUSTICE |JUSTICE )", "", speaker))

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

      word_count_plot

    } #Totals Graph (Active)
    ggsave("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/April Sitting 2023/word_count_plot_OT23_april.png", word_count_plot, dpi = 300)

    {


      attorneys <- scotus_OT24 %>%
        filter(sitting == "April") %>%
        mutate(response_to = ifelse(lag(speaker_type) == 'Justice', lag(speaker), NA)) %>%
        filter(speaker_type == "Attorney") %>%
        mutate(speaker = ifelse(speaker == 'MR. SYNDER', 'MR. SNYDER', speaker)) %>%
        filter(!is.na(response_to)) %>%
        group_by(case_name,  speaker, response_to) %>%
        summarise(total_words = sum(word_count, na.rm = TRUE)) %>%
        unique() %>%
        ungroup() %>%
        mutate(response_to = gsub("(CHIEF JUSTICE |JUSTICE )", "", response_to)) %>%
        pivot_wider(names_from = response_to, values_from = total_words) %>%
        unique() %>%
        mutate('Total Words' = rowSums(select(., -c(case_name, speaker)), na.rm = TRUE)) %>%
        relocate('Total Words', .after = speaker) %>%
        group_by(case_name, speaker) %>%
        mutate(case_name = ifelse(grepl('Kinder Morgan', case_name), 'Ohio, Et Al. Applicants v. Epa (Consolidated w/ 23A350, 23A351, 23A384)', case_name)) %>%
        rename(' ' = case_name,
               'Attorney' = speaker)

      attorney_participation <- attorneys
      original_column_names <- colnames(attorney_participation)

      original_column_names[1] <- " "
      original_column_names[2] <- " "
      original_column_names[3] <- " "

      attorney_participation <- attorney_participation %>%
        rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])


      attorney_participation_table <- attorney_participation %>%
        kbl(longtable = T, escape = F, booktabs =  T, align = "l") %>%
        add_header_above( original_column_names) %>%
        row_spec(1, align = 'center') %>%
        column_spec(1, bold = T, border_right = T) %>%
        row_spec(0, bold = T, color = 'white', background = '#080808', align = 'center') %>%
        kable_styling(full_width = T, font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive"), latex_options = c("striped", "HOLD_position")) %>%
        column_spec(1, bold = T, border_right = T) %>%
        row_spec(0, bold = T, color = 'white', background = '#080808', align = 'center') %>%
        row_spec(seq(1, nrow(oa_data), 1), align = 'center') %>%
        column_spec(4, color = "white",
                    background = spec_color(attorneys$ROBERTS, end = 0.5),
                    popover = paste("am:", attorneys$ROBERTS),
                    latex_valign = 'p') %>%
        row_spec(1:dim(attorneys)[1],  align = "c") %>%
        column_spec(5, color = "white",
                    background = spec_color(attorneys$ALITO, end = 0.5),
                    popover = paste("am:", attorneys$ALITO)) %>%
        column_spec(6, color = "white",
                    background = spec_color(attorneys$BARRETT, end = 0.5),
                    popover = paste("am:", attorneys$BARRETT)) %>%
        column_spec(7, color = "white",
                    background = spec_color(attorneys$GORSUCH, end = 0.5),
                    popover = paste("am:", attorneys$GORSUCH)) %>%
        column_spec(8, color = "white",
                    background = spec_color(attorneys$JACKSON, end = 0.5),
                    popover = paste("am:", attorneys$JACKSON)) %>%
        column_spec(9, color = "white",
                    background = spec_color(attorneys$KAGAN, end = 0.5),
                    popover = paste("am:", attorneys$KAGAN)) %>%
        column_spec(10, color = "white",
                    background = spec_color(attorneys$KAVANAUGH, end = 0.5),
                    popover = paste("am:", attorneys$KAVANAUGH)) %>%
        column_spec(11, color = "white",
                    background = spec_color(attorneys$SOTOMAYOR, end = 0.5),
                    popover = paste("am:", attorneys$SOTOMAYOR)) %>%
        column_spec(12, color = "white",
                    background = spec_color(attorneys$THOMAS, end = 0.5),
                    popover = paste("am:", attorneys$THOMAS))


      attorney_participation_table

    } #Attorneys OA Participation Figure
    save_kable(attorney_participation_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/April Sitting 2023/attorney_participation_23_april.html")
    html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/April Sitting 2023/attorney_participation_23_april.html"
    webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/April Sitting 2023/attorney_participation_23_april.png", vwidth = 1250, vheight = 100)

  } #OT 2023 (By Sitting) -- Currently April


  ################################################################################
  # OT 2023 (Aggregate Total Term Stats - Speaking Time)
  ################################################################################

  {

    base_url <- "https://github.com/JakeTruscott/scotustext/raw/master/Data/"
    rdata_url <- paste0(base_url, "scotus_transcripts_23.rdata")
    load(url(rdata_url))
  } #Compile Data from Github (If OT23 Data Not Already Loaded...)

  {
    time_spoken_total <- scotus_OT24 %>%
      filter(speaker_type == 'Justice') %>%
      mutate(time_spoken = text_stop - text_start) %>%
      group_by(speaker, case_name) %>%
      summarise(total_time_spoken = sum(time_spoken)) %>%
      mutate(total_time_spoken_minutes = round(total_time_spoken/60, 2)) %>%
      pivot_wider(names_from = speaker, values_from = total_time_spoken_minutes, names_prefix = "time_spoken_") %>%
      select(-case_name) %>%
      summarise_all(.funs = sum, na.rm = T) %>%
      mutate(total_time_spoken = rowSums(across(-total_time_spoken))) %>%
      rename_with(~str_replace(., "time_spoken_", ""), -total_time_spoken) %>%
      rename("Total Time\n(Minutes)" = total_time_spoken)


    speaking_data <- time_spoken_total
    matching_columns <- intersect(colnames(speaking_data), names(justice_image_labels))

    original_column_names <- colnames(speaking_data)
    original_column_names[1] <- ' '

    speaking_data <- speaking_data %>%
      rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])
  } #Compile Speaking Times from scotus_OT24

  {
    speaking_times_total_OT23 <- speaking_data %>%
      kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
      add_header_above( original_column_names) %>%
      column_spec(1, bold = TRUE, border_right = TRUE) %>%
      row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
      row_spec(seq(1, nrow(speaking_data), 1), align = 'center') %>%
      kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
      column_spec(2, color = "white",
                  background = spec_color(time_spoken_total$ROBERTS, end = 0.5),
                  popover = paste("am:", time_spoken_total$ROBERTS)) %>%
      column_spec(3, color = "white",
                  background = spec_color(time_spoken_total$ALITO, end = 0.5),
                  popover = paste("am:", time_spoken_total$ALITO)) %>%
      column_spec(4, color = "white",
                  background = spec_color(time_spoken_total$BARRETT, end = 0.5),
                  popover = paste("am:", time_spoken_total$BARRETT)) %>%
      column_spec(5, color = "white",
                  background = spec_color(time_spoken_total$GORSUCH, end = 0.5),
                  popover = paste("am:", time_spoken_total$GORSUCH)) %>%
      column_spec(6, color = "white",
                  background = spec_color(time_spoken_total$JACKSON, end = 0.5),
                  popover = paste("am:", time_spoken_total$JACKSON)) %>%
      column_spec(7, color = "white",
                  background = spec_color(time_spoken_total$KAGAN, end = 0.5),
                  popover = paste("am:", time_spoken_total$KAGAN)) %>%
      column_spec(8, color = "white",
                  background = spec_color(time_spoken_total$KAVANAUGH, end = 0.5),
                  popover = paste("am:", time_spoken_total$KAVANAUGH)) %>%
      column_spec(9, color = "white",
                  background = spec_color(time_spoken_total$SOTOMAYOR, end = 0.5),
                  popover = paste("am:", time_spoken_total$SOTOMAYOR)) %>%
      column_spec(10, color = "white",
                  background = spec_color(time_spoken_total$THOMAS, end = 0.5),
                  popover = paste("am:", time_spoken_total$THOMAS))
  } #Compile Table

  speaking_times_total_OT23  #Print Preview
  save_kable(speaking_times_total_OT23, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/oa23_speaking_times_active.html")
  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/oa23_speaking_times_active.html"
  phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/oa23_speaking_times_active.png", vwidth = 1250, vheight = 100)



  ################################################################################
  # OT 2023 (By Active Sitting - Speaking Time)
  # Currently *april Sitting*
  ################################################################################

  {
    time_spoken_total_april <- scotus_OT24 %>%
      filter(speaker_type == 'Justice') %>%
      filter(sitting == 'April') %>%
      mutate(case_name = ifelse(grepl('Kinder Morgan', case_name), 'Ohio, Et Al. Applicants v. Epa (Consolidated w/ 23A350, 23A351, 23A384)', case_name)) %>%
      mutate(time_spoken = text_stop - text_start) %>%
      group_by(speaker, case_name) %>%
      summarise(total_time_spoken = sum(time_spoken)) %>%
      mutate(total_time_spoken_minutes = round(total_time_spoken/60, 2)) %>%
      group_by(case_name) %>%
      pivot_wider(names_from = speaker, values_from = total_time_spoken_minutes, names_prefix = "time_spoken_") %>%
      summarise_all(.funs = sum, na.rm = T) %>%
      mutate(total_time_spoken = rowSums(across(-c(total_time_spoken, case_name)))) %>%
      rename_with(~str_replace(., "time_spoken_", ""), -total_time_spoken) %>%
      rename("Total Time\n(Minutes)" = total_time_spoken)


    speaking_data <- time_spoken_total_april
    matching_columns <- intersect(colnames(speaking_data), names(justice_image_labels))

    original_column_names <- colnames(speaking_data)
    original_column_names[1:2] <- ' '

    speaking_data <- speaking_data %>%
      rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.]) %>%
      rename(`Case` = case_name)
  } #Compile Speaking Times from scotus_OT24

  {
    speaking_times_april <- speaking_data %>%
      kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
      add_header_above( original_column_names) %>%
      column_spec(1, bold = TRUE, border_right = TRUE, width = "350px") %>%
      row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
      row_spec(seq(1, nrow(speaking_data), 1), align = 'center') %>%
      kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
      column_spec(3, color = "white",
                  background = spec_color(time_spoken_total_april$ROBERTS, end = 0.5),
                  popover = paste("am:", time_spoken_total_april$ROBERTS)) %>%
      column_spec(4, color = "white",
                  background = spec_color(time_spoken_total_april$ALITO, end = 0.5),
                  popover = paste("am:", time_spoken_total_april$ALITO)) %>%
      column_spec(5, color = "white",
                  background = spec_color(time_spoken_total_april$BARRETT, end = 0.5),
                  popover = paste("am:", time_spoken_total_april$BARRETT)) %>%
      column_spec(6, color = "white",
                  background = spec_color(time_spoken_total_april$GORSUCH, end = 0.5),
                  popover = paste("am:", time_spoken_total_april$GORSUCH)) %>%
      column_spec(7, color = "white",
                  background = spec_color(time_spoken_total_april$JACKSON, end = 0.5),
                  popover = paste("am:", time_spoken_total_april$JACKSON)) %>%
      column_spec(8, color = "white",
                  background = spec_color(time_spoken_total_april$KAGAN, end = 0.5),
                  popover = paste("am:", time_spoken_total_april$KAGAN)) %>%
      column_spec(9, color = "white",
                  background = spec_color(time_spoken_total_april$KAVANAUGH, end = 0.5),
                  popover = paste("am:", time_spoken_total_april$KAVANAUGH)) %>%
      column_spec(10, color = "white",
                  background = spec_color(time_spoken_total_april$SOTOMAYOR, end = 0.5),
                  popover = paste("am:", time_spoken_total_april$SOTOMAYOR)) %>%
      column_spec(11, color = "white",
                  background = spec_color(time_spoken_total_april$THOMAS, end = 0.5),
                  popover = paste("am:", time_spoken_total_april$THOMAS))
  } #Compile Table

  speaking_times_april #Print Preview
  save_kable(speaking_times_april, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/April Sitting 2023/total_speaking_time.html")
  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/April Sitting 2023/total_speaking_time.html"
  phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/April Sitting 2023/total_speaking_time.png", vwidth = 1250, vheight = 100)

  ################################################################################
  # OT 2023 (By Active Sitting - Speaking Time)
  # By Sitting
  ################################################################################

  {

    for (i in unique(scotus_OT24$sitting)){

      time_spoken_total <- scotus_OT24 %>%
        filter(speaker_type == 'Justice') %>%
        filter(sitting == i) %>%
        mutate(case_name = ifelse(grepl('Kinder Morgan', case_name), 'Ohio, Et Al. Applicants v. Epa', case_name)) %>%
        mutate(time_spoken = text_stop - text_start) %>%
        group_by(speaker, case_name) %>%
        summarise(total_time_spoken = sum(time_spoken)) %>%
        mutate(total_time_spoken_minutes = round(total_time_spoken/60, 2)) %>%
        group_by(case_name) %>%
        pivot_wider(names_from = speaker, values_from = total_time_spoken_minutes, names_prefix = "time_spoken_") %>%
        summarise_all(.funs = sum, na.rm = T) %>%
        mutate(total_time_spoken = rowSums(across(-c(total_time_spoken, case_name)))) %>%
        rename_with(~str_replace(., "time_spoken_", ""), -total_time_spoken) %>%
        rename("Total Time\n(Minutes)" = total_time_spoken)


      speaking_data <- time_spoken_total
      matching_columns <- intersect(colnames(speaking_data), names(justice_image_labels))

      original_column_names <- colnames(speaking_data)
      original_column_names[1:2] <- ' '

      speaking_data <- speaking_data %>%
        rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.]) %>%
        rename(`Case` = case_name)


      speaking_times <- speaking_data %>%
        kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
        add_header_above( original_column_names) %>%
        column_spec(1, bold = TRUE, border_right = TRUE, width = "1000px", extra_css = 'font-size: 14px;') %>%
        row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
        row_spec(seq(1, nrow(speaking_data), 1), align = 'center') %>%
        kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
        column_spec(1, color = 'black') %>%
        column_spec(2, color = "black",
                    popover = paste("am:", time_spoken_total$`Total Time
                                    (Minutes)`), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;")  %>%
        column_spec(3, color = "white",
                    background = spec_color(time_spoken_total$ROBERTS, end = 0.5),
                    popover = paste("am:", time_spoken_total$ROBERTS), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
        column_spec(4, color = "white",
                    background = spec_color(time_spoken_total$ALITO, end = 0.5),
                    popover = paste("am:", time_spoken_total$ALITO), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
        column_spec(5, color = "white",
                    background = spec_color(time_spoken_total$BARRETT, end = 0.5),
                    popover = paste("am:", time_spoken_total$BARRETT), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
        column_spec(6, color = "white",
                    background = spec_color(time_spoken_total$GORSUCH, end = 0.5),
                    popover = paste("am:", time_spoken_total$GORSUCH), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
        column_spec(7, color = "white",
                    background = spec_color(time_spoken_total$JACKSON, end = 0.5),
                    popover = paste("am:", time_spoken_total$JACKSON), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
        column_spec(8, color = "white",
                    background = spec_color(time_spoken_total$KAGAN, end = 0.5),
                    popover = paste("am:", time_spoken_total$KAGAN), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
        column_spec(9, color = "white",
                    background = spec_color(time_spoken_total$KAVANAUGH, end = 0.5),
                    popover = paste("am:", time_spoken_total$KAVANAUGH), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
        column_spec(10, color = "white",
                    background = spec_color(time_spoken_total$SOTOMAYOR, end = 0.5),
                    popover = paste("am:", time_spoken_total$SOTOMAYOR), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
        column_spec(11, color = "white",
                    background = spec_color(time_spoken_total$THOMAS, end = 0.5),
                    popover = paste("am:", time_spoken_total$THOMAS), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;")

      output_path = paste0("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/statpack_oa_times/oa_speaking_times_", i, '.html')

      save_kable(speaking_times, file = output_path)


    }


    for (i in unique(scotus_OT24$sitting)){

      html_file_path = paste0("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/statpack_oa_times/oa_speaking_times_", i, '.html')
      png_file_path = paste0("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/statpack_oa_times/oa_speaking_times_", i, '.png')
      webshot::webshot(html_file_path, png_file_path, vwidth = 1000, vheight = 100)

    }



  } #Speaking Times by Sitting - For Statpack




  ################################################################################
  #Earlier Terms - Words
  ################################################################################
  {

    base_url <- "https://github.com/JakeTruscott/scotustext/raw/master/Data/"
    rdata_url <- paste0(base_url, "scotus_transcripts_04-22.rdata")
    load(url(rdata_url))

    oa <- scotus %>%
      filter(term == 2022) %>%
      mutate(case_name = ifelse(argument == '21-376', "Chad Everet Brackeen, et al., Petitioners v. Deb Haaland, Secretary of the Interior, et al.", case_name)) %>%
      filter(type == 'Justice') %>%
      group_by(speaker, case_name) %>%
      summarise(total_word_count = sum(word_count)) %>%
      pivot_wider(names_from = speaker, values_from = total_word_count, names_prefix = "word_count_")

    names(oa) <- gsub('(CHIEF JUSTICE |JUSTICE )', '', names(oa))
    names(oa) <- gsub('word_count_', '', names(oa))


    totals <- oa %>%
      select(-case_name) %>%
      summarise_all(.funs = sum, na.rm = T)

    totals <- cbind(data.frame('case_name' = 'Totals'), totals)

    oa <- oa %>%
      rename(' ' = case_name)


    oa_table <- oa %>%
      kbl(longtable = T, escape = F, booktabs =  T, align = "c") %>%
      column_spec(1, bold = T, border_right = T) %>%
      row_spec(0, bold = T, color = 'white', background = '#080808', align = 'center') %>%
      row_spec(seq(1, nrow(oa), 1), align = 'center') %>%
      kable_styling(font_size = 10, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
      column_spec(2, color = "white",
                  background = spec_color(oa$ROBERTS, end = 0.7),
                  popover = paste("am:", oa$ROBERTS)) %>%
      column_spec(3, color = "white",
                  background = spec_color(oa$ALITO, end = 0.7),
                  popover = paste("am:", oa$ALITO)) %>%
      column_spec(4, color = "white",
                  background = spec_color(oa$BARRETT, end = 0.7),
                  popover = paste("am:", oa$BARRETT)) %>%
      column_spec(5, color = "white",
                  background = spec_color(oa$GORSUCH, end = 0.7),
                  popover = paste("am:", oa$GORSUCH)) %>%
      column_spec(6, color = "white",
                  background = spec_color(oa$JACKSON, end = 0.7),
                  popover = paste("am:", oa$JACKSON)) %>%
      column_spec(7, color = "white",
                  background = spec_color(oa$KAGAN, end = 0.7),
                  popover = paste("am:", oa$KAGAN)) %>%
      column_spec(8, color = "white",
                  background = spec_color(oa$KAVANAUGH, end = 0.7),
                  popover = paste("am:", oa$KAVANAUGH)) %>%
      column_spec(9, color = "white",
                  background = spec_color(oa$SOTOMAYOR, end = 0.7),
                  popover = paste("am:", oa$SOTOMAYOR)) %>%
      column_spec(10, color = "white",
                  background = spec_color(oa$THOMAS, end = 0.7),
                  popover = paste("am:", oa$THOMAS))

    oa_table

    save_kable(oa_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/oa_table.html")



    totals_table <- totals %>%
      rename(' ' = case_name) %>%
      kbl(title = 'October Term 2022 - Oral Argument Rhetoric') %>%
      row_spec(1, align = 'center') %>%
      column_spec(1, bold = T, border_right = T) %>%
      row_spec(0, bold = T, color = 'white', background = '#080808', align = 'center') %>%
      kable_styling(full_width = T, font_size = 10, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
      column_spec(2, color = "white",
                  background = spec_color(oa$ROBERTS, end = 0.7),
                  popover = paste("am:", oa$ROBERTS)) %>%
      column_spec(3, color = "white",
                  background = spec_color(oa$ALITO, end = 0.7),
                  popover = paste("am:", oa$ALITO)) %>%
      column_spec(4, color = "white",
                  background = spec_color(oa$BARRETT, end = 0.7),
                  popover = paste("am:", oa$BARRETT)) %>%
      column_spec(5, color = "white",
                  background = spec_color(oa$GORSUCH, end = 0.7),
                  popover = paste("am:", oa$GORSUCH)) %>%
      column_spec(6, color = "white",
                  background = spec_color(oa$JACKSON, end = 0.7),
                  popover = paste("am:", oa$JACKSON)) %>%
      column_spec(7, color = "white",
                  background = spec_color(oa$KAGAN, end = 0.7),
                  popover = paste("am:", oa$KAGAN)) %>%
      column_spec(8, color = "white",
                  background = spec_color(oa$KAVANAUGH, end = 0.7),
                  popover = paste("am:", oa$KAVANAUGH)) %>%
      column_spec(9, color = "white",
                  background = spec_color(oa$SOTOMAYOR, end = 0.7),
                  popover = paste("am:", oa$SOTOMAYOR)) %>%
      column_spec(10, color = "white",
                  background = spec_color(oa$THOMAS, end = 0.7),
                  popover = paste("am:", oa$THOMAS))

    totals_table
    save_kable(totals_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/totals_table.html")

    custom_colors <- c("#5FA934", "#3B5E8B", "#522559")

    scotus_term <- scotus %>%
      filter(term == 2022) %>%
      mutate(case_name = ifelse(argument == '21-376', "Chad Everet Brackeen, et al., Petitioners v. Deb Haaland, Secretary of the Interior, et al.", case_name)) %>%
      filter(type == 'Justice') %>%
      group_by(speaker, case_name) %>%
      summarise(total_word_count = sum(word_count)) %>%
      group_by(speaker) %>%
      summarise(total_word_count = sum(total_word_count)) %>%
      mutate(speaker = gsub("(CHIEF JUSTICE |JUSTICE )", "", speaker)) %>%
      mutate(speaker = str_to_title(speaker))

    word_count_plot <- ggplot(data = scotus_term, aes(y = total_word_count, x = speaker, fill = total_word_count)) +
      scale_y_continuous(labels = scales::comma, breaks = seq(10000, 80000, 10000)) +
      geom_hline(yintercept = 0, colour = 'gray5') +
      geom_col(colour = 'gray5') +
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
           title = "Total Words Spoken in 2022 Term Oral Arguments",
           subtitle = paste0("(As of 2023-07-01)")) +
      scale_x_discrete(labels = justice_image_labels) +
      theme_classic() +
      theme(
        panel.grid = element_blank(),
        axis.text.x = ggtext::element_markdown(),
        axis.text.y = element_text(size = 15),
        axis.ticks.x = element_blank(),
        legend.background = element_rect(linewidth = 1, fill = "NA", colour = "black"),
        legend.box.background = element_rect(fill = NA, colour = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.title.align = 0.5,
        legend.text = element_text(size = 12),
        plot.caption = element_text(hjust = 0.5, size = 12),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5))

    word_count_plot

    ggsave("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/word_count_plot_OT22.png", word_count_plot, dpi = 300)

  } #OT 2022



} # OT23 & 22 (Old Code)
