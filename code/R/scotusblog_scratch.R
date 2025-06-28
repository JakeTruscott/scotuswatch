scotusblog_stats <- function(decisions_path,
                             oral_arguments,
                             opinions_processed,
                             older_opinions_processed,
                             output_folder,
                             master_file = cases_master,
                             scdb_cases_data = scdb_cases,
                             scdb_justices_data = scdb_justices){

  decisions <- read.csv(decisions_path, as.is = T)

  combined_list <- list()

  {

    term_index <- list()
    master_file$sitting <- ifelse(master_file$sitting == '', 'No Argument', master_file$sitting)
    unique_sittings <- unique(master_file$sitting)

    for (i in 1:length(unique_sittings)){

      temp_sitting_data <- master_file[master_file$sitting == unique_sittings[i],] %>%
        left_join(decisions %>%
                    rename_with(tolower) %>%
                    select(docket, date_argued, date_decided, decision, coalition, author), by = 'docket') %>%
        mutate(date_argued = as.Date(date_argued, format = "%m/%d/%Y"),
               date_decided = as.Date(date_decided, format = "%m/%d/%Y"),
               days_elapsed = ifelse(!is.na(date_decided) & !is.na(date_argued),
                                     as.numeric(date_decided - date_argued) + 1, NA),
               major_case = ifelse(is.na(major_case), 0, 1)) %>%
        select(-c(starts_with('consolidated'))) %>%
        select(sitting, docket, short_hand, lower_court, major_case, date_argued, date_decided, days_elapsed, decision, coalition, author)

      term_index[[as.character(unique_sittings[i])]] <- temp_sitting_data



    }

    combined_list[['term_index']] <- term_index

  } # Term Index

  message('Completed Term Index')

  {

    circuit_scorecard <- master_file %>%
      select(lower_court, docket) %>%
      left_join(decisions %>%
                  rename_with(tolower) %>%
                  select(docket, decision), by = 'docket') %>%
      unique() %>%
      filter(!is.na(decision)) %>%
      mutate(decision = case_when(
        grepl('Reverse', decision) ~ 'Reverse',
        grepl('Granted', decision) ~ 'Toss',
        grepl('Vacate', decision) ~ 'Reverse',
        grepl('Affirm', decision) ~ 'Affirm',
        decision %in% c('DIG', 'dig') ~ 'DIG')) %>%
      filter(lower_court %in% c('CA1', 'CA2', 'CA3', 'CA4', 'CA5', 'CA6', 'CA7', 'CA8', 'CA9', 'CA10', 'CA11', 'CADC', 'CAFED', 'CAFC')) %>%
      filter(!decision == 'Toss') %>%
      arrange(lower_court) %>%
      group_by(lower_court, decision) %>%
      summarise(count = n(), .groups = "drop") %>%
      pivot_wider(names_from = 'decision', values_from = 'count') %>%
      mutate(across(everything(), ~replace_na(., 0))) %>%
      mutate(
        total_cases = rowSums(across(-lower_court)),
        total_decided = rowSums(across(c(Affirm, Reverse))),  # include only decided outcomes
        total_affirm = round(total_decided - Reverse, 3),
        total_reverse = round(total_decided - Affirm, 3)) %>%
      ungroup() %>%
      mutate(
        total_proportion = paste0(round(total_cases / sum(total_cases), 3)*100, '%'),
        percent_affirm = paste0(round(total_affirm / total_decided, 3)*100, '%'),
        percent_reverse = paste0(round(total_reverse / total_decided, 3)*100, '%')) %>%
      select(lower_court, total_cases, total_proportion, total_decided, total_affirm, total_reverse, percent_affirm, percent_reverse) %>%
      mutate(percent_affirm = ifelse(percent_affirm == 'NaN%', 'NA', percent_affirm),
             percent_reverse = ifelse(percent_reverse == 'NaN%', 'NA', percent_reverse)) %>%
      mutate(lower_court = factor(lower_court, levels = c('CA1', 'CA2', 'CA3', 'CA4', 'CA5', 'CA6', 'CA7', 'CA8', 'CA9', 'CA10', 'CA11', 'CADC', 'CAFED', 'Armed Forces', 'State Court', 'District Court', 'Original'))) %>%
      arrange(lower_court) %>%
      rename(`Court Below` = lower_court,
             `# of Cases` = total_cases,
             `% of Cases` = total_proportion,
             `# Decided` = total_decided,
             `# Affirm` = total_affirm,
             `# Reversed` = total_reverse,
             `% Affirmed` = percent_affirm,
             `% Reversed` = percent_reverse)

    combined_list[['circuit_scorecard']] <- circuit_scorecard


  } # Circuit Scorecard

  message('Completed Circuit Scorecard')

  {

    combined_list[['days_elapsed']] <- list()

    days_elapsed <- master_file %>%
      left_join(decisions %>%
                  rename_with(tolower) %>%
                  select(docket, date_argued, date_decided, decision, coalition, author), by = 'docket') %>%
      filter(!is.na(date_argued)) %>%
      mutate(date_argued = as.Date(date_argued, format = "%m/%d/%Y"),
             date_decided = as.Date(date_decided, format = "%m/%d/%Y"),
             days_elapsed = ifelse(!is.na(date_decided) & !is.na(date_argued),
                                   as.numeric(date_decided - date_argued) + 1, NA),
             major_case = ifelse(is.na(major_case), 0, 1)) %>%
      select(-c(starts_with('consolidated'))) %>%
      select(short_hand, days_elapsed, author, coalition, date_argued, date_decided) %>%
      filter(!is.na(days_elapsed))

    {

      days_elapsed_shortest <- days_elapsed %>%
        arrange(days_elapsed) %>%
        slice_head(n = 5)

      days_elapsed_longest <- days_elapsed %>%
        arrange(desc(days_elapsed)) %>%
        slice_head(n = 5)

      combined_list[['days_elapsed']][['shortest']] <- days_elapsed_shortest
      combined_list[['days_elapsed']][['longest']] <- days_elapsed_longest

      } # Longest & Shortest Times

    {

      days_elapsed_figure <- scdb_cases_data %>%
        filter(term >= 2005) %>%
        select(dateArgument, dateDecision, dateRearg, term) %>%
        mutate(dateArgument = coalesce(as.Date(dateRearg), as.Date(dateArgument))) %>%
        rename(date_argued = dateArgument,
               date_decided = dateDecision) %>%
        filter(!is.na(date_argued)) %>%
        mutate(date_argued = as.Date(date_argued, format = "%m/%d/%Y"),
               date_decided = as.Date(date_decided, format = "%m/%d/%Y"),
               days_elapsed = ifelse(!is.na(date_decided) & !is.na(date_argued),
                                     as.numeric(date_decided - date_argued) + 1, NA)) %>%
        filter(!is.na(days_elapsed)) %>%
        group_by(term) %>%
        summarise(
          mean_elapsed = mean(days_elapsed)
        )

      all_data <- scdb_cases_data %>%
        filter(term >= 2005) %>%
        mutate(dateArgument = coalesce(as.Date(dateRearg), as.Date(dateArgument)),
               date_argued = as.Date(dateArgument, format = "%m/%d/%Y"),
               date_decided = as.Date(dateDecision, format = "%m/%d/%Y"),
               days_elapsed = ifelse(!is.na(date_decided) & !is.na(date_argued),
                                     as.numeric(date_decided - date_argued) + 1, NA)) %>%
        filter(!is.na(days_elapsed))


      mean_all <- mean(all_data$days_elapsed)
      p25_all <- quantile(all_data$days_elapsed, 0.25)
      p75_all <- quantile(all_data$days_elapsed, 0.75)

      days_elapsed_figure <- bind_rows(
        days_elapsed_figure,
        data.frame(
          term = 2024,
          mean_elapsed = mean(days_elapsed$days_elapsed)
        )
      ) %>%
        unique()

      days_elapsed_figure <-  ggplot(data = days_elapsed_figure, aes(x = term, y = mean_elapsed)) +
        geom_point(size = 3) +
        geom_line(linetype = 2) +
        geom_label(aes(label = round(mean_elapsed, 0)), vjust = -1.5, size = 4) +
        labs(
          x = '\nTerm',
          y = 'Mean Days Between\nArgument & Decision\n') +
        scale_y_continuous(limits = c(45, 170), breaks = seq(60, 160, 20)) +
        scale_x_continuous(breaks = seq(2006, 2024, 2)) +
        theme_minimal() +
        theme(
          panel.border = element_rect(size = 1, colour = 'black', fill = NA),
          axis.text = element_text(size = 14, colour = 'black'),
          axis.title = element_text(size = 16, colour = 'black')
        )


      ggsave(days_elapsed_figure,
             filename = file.path(output_folder, 'days_elapsed_figure.png'),
             width = 10,
             height = 6,
             bg = 'white')

      combined_list[['days_elapsed']][['days_elapsed_figure']] <- days_elapsed_figure


    } # Longitudinal Days Elapsed Figure

    {

      days_elapsed_author <- days_elapsed %>%
        filter(!author == 'Per Curiam') %>%
        mutate(author = factor(author, levels = c('Roberts', 'Thomas', 'Alito', 'Sotomayor', 'Kagan', 'Gorsuch', 'Kavanaugh', 'Barrett', 'Jackson'))) %>%
        group_by(author) %>%
        summarise(mean_elapsed = round(mean(days_elapsed), 2)) %>%
        arrange(author)

      combined_list[['days_elapsed']][['days_elapsed_author']] <- days_elapsed_author

    } # Average Days Elapsed by Majority Author


  } # Days b/w Argument & Opinion (Elapsed)

  message('Completed Days b/w Oral Arguments & Decision')

  {

    combined_list[['opinions']] <- list()

    {


      ot24_decisions <- decisions[,c(6, 8:16)] %>%
        mutate(per_curiam = ifelse(Author == 'Per Curiam', 100, 0)) %>%
        select(-c(Author)) %>%
        rowwise() %>%
        mutate(
          Majority = sum(c_across(1:10) %in% c(100), na.rm = TRUE),
          Concurrence = sum(c_across(1:10) %in% c(2, 4, 5, 7), na.rm = TRUE),
          Dissent = sum(c_across(1:10) %in% c(-1, -3), na.rm = TRUE)) %>%
        ungroup() %>%
        select(Majority, Concurrence, Dissent) %>%
        mutate(Majority = sum(Majority),
               Dissent = sum(Dissent),
               Concurrence = sum(Concurrence)) %>%
        unique() %>%
        pivot_longer(cols = everything(), values_to = "opinion") %>%
        rename(vote = name,
               count = opinion) %>%
        mutate(term = 2024) %>%
        mutate(vote = factor(vote, levels = c('Majority', 'Concurrence', 'Dissent')))



      decisions_combined <- scdb_justices_data %>%
        filter(term >= 2005) %>%
        filter(opinion == 2) %>%
        group_by(term, vote, decisionType, docket) %>%
        summarise(count = n(), .groups = 'drop') %>%
        unique() %>%
        mutate(count = ifelse(decisionType %in% c(2, 6), 1, count)) %>%
        mutate(vote = case_when(
          vote %in% c(2, 6, 7) ~ 'Dissent',
          vote %in% c(3, 4, 6, 8) ~ 'Concurrence',
          .default = 'Majority')) %>%
        group_by(term, vote) %>%
        summarise(count = sum(count), .groups = 'drop') %>%
        unique() %>%
        mutate(vote = factor(vote, levels = c('Majority', 'Concurrence', 'Dissent'))) %>%
        bind_rows(ot24_decisions)

      cases_with_no_signed_opinions <- scdb_justices_data %>%
        filter(term >= 2005) %>%
        select(opinion, docket) %>%
        group_by(docket) %>%
        filter(all(opinion == 1)) %>%
        ungroup() %>%
        unique() %>%
        left_join(scdb_cases %>%
                    select(docket, term, decisionType), by = 'docket') %>%
        filter(term >= 2005) %>%
        filter(!decisionType == 5) %>% # Remove Equally Divided (Only Keep Per Curiams & Decrees)
        group_by(term) %>%
        summarise(count = n(), .groups = 'drop') %>%
        mutate(vote = 'Majority')

      decisions_merged <- decisions_combined %>%
        bind_rows(cases_with_no_signed_opinions) %>%
        group_by(term, vote) %>%
        summarise(count = sum(count), .groups = 'drop') %>%
        mutate(vote = factor(vote, levels = c('Dissent', 'Concurrence', 'Majority'))) # Combine PCs with Existing Metrics

      df_labels <- decisions_merged %>%
        group_by(term) %>%
        summarise(total = sum(count), .groups = 'drop')


      decisions_over_time <- decisions_merged %>%
        ggplot(aes(x = term, y = count, fill = vote)) +
        geom_col(colour = 'black') +
        geom_label(
          aes(label = count, group = vote),
          fill = "white",  # label background
          position = position_stack(vjust = 0.5),
          size = 4,
          colour = 'black'  # label text color
        ) +
        scale_x_continuous(breaks = seq(2006, 2024, 2)) +
        scale_y_continuous(lim = c(0, 225), breaks = seq(50, 200, 50)) +
        scale_fill_manual(
          values = c('Majority' = '#4E478A', 'Concurrence' = '#0F9147', 'Dissent' = '#FDD532'),
          breaks = c('Majority', 'Concurrence', 'Dissent')
        ) +
        geom_hline(yintercept = 0) +
        labs(x = '\nTerm', y = '', fill = '') +
        theme_minimal() +
        theme(
          panel.border = element_rect(size = 1, colour = 'black', fill = NA),
          axis.text = element_text(size = 14, colour = 'black'),
          axis.title = element_text(size = 16, colour = 'black'),
          legend.text = element_text(size = 14, colour = 'black'),
          legend.position = 'top',
          legend.title = element_blank(),
          legend.box.background = element_rect(size = 1, colour = 'black', fill = NA)
        ) +
        geom_text(
          data = df_labels,
          aes(x = term, y = total + 8, label = total),  # offset above bar
          inherit.aes = FALSE,
          size = 4.5,
          fontface = "bold"
        )


      combined_list[['opinions']][['opinions_over_time_figure']] <- decisions_over_time

      ggsave(decisions_over_time,
             filename = file.path(output_folder, 'decisions_over_time.png'),
             width = 10,
             height = 6,
             units = 'in',
             bg = 'white')

    } # Total Opinions by Term

    {

      ot24_decisions <- decisions[, c(8:17)] %>%
        left_join(master_file %>%
                    select(short_hand, docket) %>%
                    rename(Docket = docket), by = 'Docket')

      opinions <- list()
      justices <- names(ot24_decisions[,c(1:9)])

      for (i in justices){

        opinions[[as.character(i)]] <- list()

        temp_col <- ot24_decisions[, c(as.character(i), 'short_hand')]

        opinions[[as.character(i)]][['Majority']] <- c(temp_col$short_hand[which(temp_col[,1] == 100)])
        opinions[[as.character(i)]][['Concurrence']] <- c(temp_col$short_hand[which(temp_col[,1] %in% c(2, 4, 5, 7))])
        opinions[[as.character(i)]][['Dissent']] <- c(temp_col$short_hand[which(temp_col[,1] %in% c(-1, -3))])

      }

      combined_list[['opinions']][['2024 Opinions by Justice']] <- opinions


    } # Opinions Authored by Justice

    {

      opinions_by_justice <- data.frame()

      for (i in 1:length(opinions)){

        temp_justice <- opinions[[i]]
        temp_justice_name <- str_to_title(names(opinions[i]))
        majority <- length(temp_justice$Majority)
        concurrence <- length(temp_justice$Concurrence)
        dissent <- length(temp_justice$Dissent)

        opinions_by_justice <- bind_rows(opinions_by_justice,
                                         data.frame(justice = temp_justice_name,
                                                    majority = majority,
                                                    concurrence = concurrence,
                                                    dissent = dissent))



      }

      opinions_long <- opinions_by_justice %>%
        pivot_longer(cols = -justice, names_to = "type", values_to = "count") %>%
        group_by(justice) %>%
        mutate(total_opinions = sum(count)) %>%
        ungroup() %>%
        mutate(
          # Update justice label to include total
          justice_label = paste0(justice, " (", total_opinions, ")"),
          # Reorder with respect to totals
          justice_label = fct_reorder(justice_label, total_opinions),
          type = str_to_title(type),
          type = factor(type, levels = c('Majority', 'Concurrence', 'Dissent'))
        )

      opinions_by_justice <- ggplot(opinions_long, aes(x = count, y = justice_label)) +
        geom_col(aes(fill = factor(type, levels = rev(levels(type)))), colour = 'black') +
        geom_label(
          data = function(d) d %>% filter(count != 0),
          aes(label = count),
          position = position_stack(vjust = 0.5),
          colour = 'black',
          size = 4,
          fill = "white",
          label.size = 0
        ) +
        scale_x_continuous(lim = c(0, 30), breaks = seq(4, 28, 4)) +
        labs(x = '', y = '', fill = '') +
        scale_fill_manual(
          values = c('Majority' = '#4E478A', 'Concurrence' = '#0F9147', 'Dissent' = '#FDD532'),
          breaks = c('Majority', 'Concurrence', 'Dissent')
        ) +
        theme_minimal() +
        theme(
          panel.border = element_rect(size = 1, colour = 'black', fill = NA),
          axis.text = element_text(size = 14, colour = 'black'),
          axis.title = element_text(size = 16, colour = 'black'),
          legend.text = element_text(size = 14, colour = 'black'),
          legend.position = 'top',
          legend.title = element_blank(),
          legend.box.background = element_rect(size = 1, colour = 'black', fill = NA)
        )

      combined_list[['opinions']][['opinions_by_justice_figure']] <- opinions_by_justice

      ggsave(opinions_by_justice,
             filename = file.path(output_folder, 'opinions_by_justice.png'),
             width = 12,
             height = 6,
             units = 'in',
             bg = 'white')


    } # Opinion Types by Each Justice



  } # Opinion Authorship (By Justice, Type, Etc.)

  message('Completed Opinion Authorship')

  {



    {

      justice_columns <- decisions[,c(8:16)]
      total_cases <- nrow(decisions)
      majorities <- data.frame()

      for (i in 1:ncol(justice_columns)){

        temp_justice <- justice_columns[, i]
        temp_justice <- temp_justice[!is.na(temp_justice)] # Filter to Only Cases Participated
        temp_justice_name <- str_to_title(names(justice_columns[i]))
        majority <- round(length(which(temp_justice >= 1))/length(temp_justice), 2)
        ideology <- ifelse(temp_justice_name %in% c('Kagan', 'Sotomayor', 'Jackson'), 'Democrat Appointee', 'Republican Appointee')

        majorities <- bind_rows(majorities, data.frame(justice = temp_justice_name,
                                                       percent_majority = majority,
                                                       ideology = ideology))

      }


      justice_levels <- c('Roberts', 'Thomas', 'Alito', 'Sotomayor', 'Kagan', 'Gorsuch', 'Kavanaugh', 'Barrett', 'Jackson')


      majorities <- majorities %>%
        mutate(justice_order = factor(justice, levels = justice_levels))

      justice_labels <- majorities %>%
        select(justice) %>%
        mutate(justice = toupper(justice)) %>%
        mutate(image_labels = justice_image_labels[match(justice, names(justice_image_labels))],
               image_labels = gsub(' style\\=.*', '', image_labels),
               image_labels = paste0(image_labels, " width='100' /><br><strong>", justice, "</strong>")) %>%
        distinct(justice, .keep_all = TRUE) %>%
        mutate(justice = factor(justice, levels = toupper(justice_levels))) %>%
        arrange(justice) %>%
        pull(image_labels)


      percent_in_majority <- ggplot(majorities, aes(x = justice_order, y = percent_majority)) +
        geom_col(aes(fill = percent_majority), colour = 'black') +
        #scale_fill_manual(values = c('deepskyblue3', 'coral4')) +
        scale_fill_distiller(palette = 'Blues', direction = 1) +
        scale_y_continuous(lim = c(0, 1)) +
        geom_label(aes(label = paste0(percent_majority*100, '%'), vjust = -0.25), size = 5) +
        geom_hline(yintercept = 0) +
        theme_minimal() +
        scale_x_discrete(labels = justice_labels) +  # Use the labels with images for the x-axis
        labs(x = '',
             y = '',
             fill = '') +
        theme_minimal() +
        theme(
          panel.background = element_rect(size = 1, colour = 'black', fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = ggtext::element_markdown(size = 10, vjust = 1),
          axis.text.y = element_blank(),
          axis.title = element_text(size = 16, colour = 'black'),
          legend.text = element_text(size = 14, colour = 'black'),
          legend.position = 'none',
          legend.title = element_blank(),
          legend.box.background = element_rect(size = 1, colour = 'black', fill = NA))



      combined_list[['frequency_in_majority']][['percent_in_majority_all_cases']] <- percent_in_majority

      ggsave(percent_in_majority,
             filename = file.path(output_folder, 'percent_in_majority.png'),
             width = 10,
             height = 6,
             units = 'in',
             bg = 'white')

    } # Percent in Majority (All Cases)

    {

      justice_columns <- decisions[,c(7:16)] %>%
        filter(!grepl('(Per Curiam|(9-0)|(8-0))', Coalition)) %>%
        select(-c(Coalition))
      total_cases <- nrow(justice_columns)
      majorities <- data.frame()

      for (i in 1:ncol(justice_columns)){

        temp_justice <- justice_columns[, i]
        temp_justice <- temp_justice[!is.na(temp_justice)] # Filter to Only Cases Participated

        temp_justice_name <- str_to_title(names(justice_columns[i]))
        majority <- round(length(which(temp_justice >= 1))/length(temp_justice), 2)
        ideology <- ifelse(temp_justice_name %in% c('Kagan', 'Sotomayor', 'Jackson'), 'Democrat Appointee', 'Republican Appointee')

        majorities <- bind_rows(majorities, data.frame(justice = temp_justice_name,
                                                       percent_majority = majority,
                                                       ideology = ideology))

      }



      majorities <- majorities %>%
        mutate(justice_order = factor(justice, levels = justice_levels))

      justice_labels <- majorities %>%
        select(justice) %>%
        mutate(justice = toupper(justice)) %>%
        mutate(image_labels = justice_image_labels[match(justice, names(justice_image_labels))],
               image_labels = gsub(' style\\=.*', '', image_labels),
               image_labels = paste0(image_labels, " width='100' /><br><strong>", justice, "</strong>")) %>%
        distinct(justice, .keep_all = TRUE) %>%
        mutate(justice = factor(justice, levels = toupper(justice_levels))) %>%
        arrange(justice) %>%
        pull(image_labels)


      percent_in_majority_divided_cases <- ggplot(majorities, aes(x = justice_order, y = percent_majority)) +
        geom_col(aes(fill = percent_majority), colour = 'black') +
        scale_fill_distiller(palette = 'Blues', direction = 1) +
        scale_y_continuous(lim = c(0, 1)) +
        geom_label(aes(label = paste0(percent_majority*100, '%'), vjust = -0.25), size = 5) +
        geom_hline(yintercept = 0) +
        theme_minimal() +
        scale_x_discrete(labels = justice_labels) +  # Use the labels with images for the x-axis
        labs(x = '',
             y = '',
             fill = '') +
        theme_minimal() +
        theme(
          panel.background = element_rect(size = 1, colour = 'black', fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = ggtext::element_markdown(size = 10, vjust = 1),
          axis.text.y = element_blank(),
          axis.title = element_text(size = 16, colour = 'black'),
          legend.text = element_text(size = 14, colour = 'black'),
          legend.position = 'none',
          legend.title = element_blank(),
          legend.box.background = element_rect(size = 1, colour = 'black', fill = NA))


      combined_list[['frequency_in_majority']][['percent_in_majority_divided_cases']] <- percent_in_majority_divided_cases

      ggsave(percent_in_majority_divided_cases,
             filename = file.path(output_folder, 'percent_in_majority_non_unanimous_cases.png'),
             width = 10,
             height = 6,
             units = 'in',
             bg = 'white')

    } # Percent in Majority (Non-Unanimous Cases)

    {

      justice_columns <- decisions[,c(7:16)] %>%
        filter(grepl('((6-3)|(5-4))', Coalition)) %>%
        select(-c(Coalition))
      total_cases <- nrow(justice_columns)
      majorities <- data.frame()

      for (i in 1:ncol(justice_columns)){

        temp_justice <- justice_columns[, i]
        temp_justice <- temp_justice[!is.na(temp_justice)] # Filter to Only Cases Participated
        temp_justice_name <- str_to_title(names(justice_columns[i]))
        majority <- round(length(which(temp_justice >= 1))/length(temp_justice), 2)
        ideology <- ifelse(temp_justice_name %in% c('Kagan', 'Sotomayor', 'Jackson'), 'Democrat Appointee', 'Republican Appointee')

        majorities <- bind_rows(majorities, data.frame(justice = temp_justice_name,
                                                       percent_majority = majority,
                                                       ideology = ideology))

      }

      majorities <- majorities %>%
        mutate(justice_order = factor(justice, levels = justice_levels))

      justice_labels <- majorities %>%
        select(justice) %>%
        mutate(justice = toupper(justice)) %>%
        mutate(image_labels = justice_image_labels[match(justice, names(justice_image_labels))],
               image_labels = gsub(' style\\=.*', '', image_labels),
               image_labels = paste0(image_labels, " width='100' /><br><strong>", justice, "</strong>")) %>%
        distinct(justice, .keep_all = TRUE) %>%
        mutate(justice = factor(justice, levels = toupper(justice_levels))) %>%
        arrange(justice) %>%
        pull(image_labels)


      percent_in_majority_close_cases <- ggplot(majorities, aes(x = justice_order, y = percent_majority)) +
        geom_col(aes(fill = percent_majority), colour = 'black') +
        scale_fill_distiller(palette = 'Blues', direction = 1) +
        scale_y_continuous(lim = c(0, 1)) +
        geom_label(aes(label = paste0(percent_majority*100, '%'), vjust = -0.25), size = 5) +
        geom_hline(yintercept = 0) +
        theme_minimal() +
        scale_x_discrete(labels = justice_labels) +  # Use the labels with images for the x-axis
        labs(x = '',
             y = '',
             fill = '') +
        theme_minimal() +
        theme(
          panel.background = element_rect(size = 1, colour = 'black', fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = ggtext::element_markdown(size = 10, vjust = 1),
          axis.text.y = element_blank(),
          axis.title = element_text(size = 16, colour = 'black'),
          legend.text = element_text(size = 14, colour = 'black'),
          legend.position = 'none',
          legend.title = element_blank(),
          legend.box.background = element_rect(size = 1, colour = 'black', fill = NA))

      combined_list[['frequency_in_majority']][['percent_in_majority_close_cases']] <- percent_in_majority_close_cases

      ggsave(percent_in_majority_close_cases,
             filename = file.path(output_folder, 'percent_in_majority_close_cases.png'),
             width = 10,
             height = 6,
             units = 'in',
             bg = 'white')

    } # Percent in Majority (Close Cases)

    {


      strength_of_majority <- decisions[, 6:16] %>%
        mutate(majority_size = gsub('\\-.*', '', gsub('\\(', '', Coalition))) %>%
        rowwise() %>%
        mutate(
          majority_size = as.numeric(
            ifelse(
              majority_size == 'Per Curiam',
              9 - sum(c_across(3:11) < 1, na.rm = TRUE),
              majority_size))) %>%
        rename(justice = Author) %>%
        filter(justice != 'Per Curiam') %>%
        count(justice, majority_size) %>%
        pivot_wider(
          names_from = majority_size,
          values_from = n,
          values_fill = 0) %>%
        rowwise() %>%
        mutate(total = sum(c_across(2:6))) %>%
        select(justice, total, '9', '8', '7', '6', '5') %>%
        mutate(average_majority = ( `9` * 9 + `8` * 8 + `7` * 7 + `6` * 6 + `5` * 5) / total) %>%
        mutate(justice = factor(justice, levels = c('Roberts', 'Thomas', 'Alito', 'Sotomayor', 'Kagan', 'Gorsuch', 'Kavanaugh', 'Barrett', 'Jackson'))) %>%
        arrange(justice)

      percent_unanimous <- strength_of_majority %>%
        select(justice, `9`, total) %>%
        mutate(percent_unanimous = round(`9`/total, 2)) %>%
        select(justice, percent_unanimous)

      combined_list[['frequency_in_majority']][['strength_of_majority']] <- strength_of_majority
      combined_list[['frequency_in_majority']][['percent_author_opinion_unanimous']] <- percent_unanimous

    } # Strength of Majority

    {

      coalition_sizes_by_justice <- decisions %>%
        select(Author, Coalition) %>%
        rename(justice = Author, coalition = Coalition) %>%
        filter(justice != 'Per Curiam') %>%
        mutate(majority_size = case_when(
          grepl('(Per Curiam|\\-0)', coalition, ignore.case = TRUE) ~ 9,
          grepl('-1', coalition) ~ 8,
          grepl('-2', coalition) ~ 7,
          grepl('-3', coalition) ~ 6,
          grepl('-4', coalition) ~ 5
        )) %>%
        group_by(justice) %>%
        mutate(mean_coalition = round(mean(majority_size), 2),
               total_opinions = n()) %>%
        ungroup() %>%
        group_by(justice, majority_size) %>%
        summarise(opinion_count = n(),
                  mean_coalition = mean(mean_coalition),
                  total_opinions = mean(total_opinions), .groups = "drop") %>%
        pivot_wider(names_from = majority_size,
                    values_from = opinion_count,
                    names_prefix = "maj_") %>%
        arrange(justice) %>%
        select(justice, total_opinions, maj_9, maj_8, maj_7, maj_6, maj_5, mean_coalition) %>%
        replace_na(list(maj_5 = 0, maj_6 = 0, maj_7 = 0, maj_8 = 0, maj_9 = 0)) %>%
        mutate(justice = factor(justice, levels = c('Roberts', 'Thomas', 'Alito', 'Sotomayor', 'Kagan', 'Gorsuch', 'Kavanaugh', 'Barrett', 'Jackson'))) %>%
        arrange(justice)


      percent_opinions_unanimous <- coalition_sizes_by_justice %>%
        select(justice, total_opinions, maj_9) %>%
        mutate(percent_unanimous = round(maj_9/total_opinions, 2)*100) %>%
        select(justice, percent_unanimous)

      combined_list[['frequency_in_majority']][['coalition_sizes_by_justice']] <- coalition_sizes_by_justice
      combined_list[['frequency_in_majority']][['percent_opinions_unanimous']] <- percent_opinions_unanimous


    } # Opinion Sizes by Justice Author

    {

      solo_dissents <- scdb_justices_data %>%
        filter(term >= 2005) %>%
        filter(minVotes == 1) %>%
        filter(vote %in% c(2, 6, 7) & opinion == 2) %>%
        group_by(term, docket) %>%
        filter(n() == 1) %>%  # Ensure there is only one dissenting justice per docket
        ungroup() %>%
        select(term, docket, justiceName) %>%
        mutate(justice = sub(".*([A-Z][a-zA-Z]*).*$", "\\1", justiceName)) %>%
        filter(!justice %in% c('Ginsburg', 'Scalia', 'Stevens', 'Breyer', 'Kennedy', 'Souter')) %>%
        arrange(justice) %>%
        group_by(justice) %>%
        summarise(count = n()) %>%
        mutate(justice = factor(justice, levels = c('Roberts', 'Thomas', 'Alito', 'Sotomayor', 'Kagan', 'Gorsuch', 'Kavanaugh', 'Barrett', 'Jackson'))) %>%
        arrange(justice) %>%
        rename(solo_dissents = count)

      combined_list[['frequency_in_majority']][['solo_dissents']] <- solo_dissents


    } # Solo Dissents (Current & Past Terms)



  } # Frequency in Majority (And Strength of Majority)

  message('Completed Frequency in & Strength of Majority')

  {

    combined_list[['coalitions']] <- list()

    {


      mean_unanimous <- scdb_cases_data %>%
        filter(term >= 2005) %>%
        select(term, minVotes, docket) %>%
        group_by(term) %>%
        mutate(total_cases = n()) %>%
        ungroup() %>%
        filter(minVotes == 0) %>%
        group_by(term) %>%
        mutate(unanimous_cases = n()) %>%
        reframe(unanimous_percentage = round(unanimous_cases / total_cases, 2) * 100) %>%
        ungroup() %>%
        unique() %>%
        bind_rows(
          decisions %>%
            select(Coalition) %>%
            rename(coalition = Coalition) %>%
            mutate(total_cases = n()) %>%
            filter(grepl('(Per Curiam|9-|(8-0))', coalition, ignore.case = TRUE)) %>%
            mutate(unanimous_cases = n()) %>%
            mutate(unanimous_percentage = round(unanimous_cases / total_cases, 2) * 100) %>%
            unique() %>%
            mutate(term = 2024) %>%
            select(term, unanimous_percentage)
        )

      mean_unanimous_value <- round(mean(mean_unanimous$unanimous_percentage), 2)

      unanimity_over_time <- suppressMessages(suppressWarnings(
        mean_unanimous %>%
          ggplot(aes(x = term, y = unanimous_percentage)) +
          geom_point(size = 3) +
          geom_line(linewidth = 1) +
          scale_y_continuous(
            limits = c(25, 70),
            breaks = seq(25, 70, 5),
            labels = function(x) paste0(x, "%")) +
          scale_x_continuous(breaks = seq(2006, 2024, 2)) +
          geom_hline(yintercept = mean_unanimous_value, linetype = 2, colour = 'red') +
          #geom_label(aes(x = 2019, y = mean_unanimous_value + 3),
           #          label = paste0('Mean = ', mean_unanimous_value, '%'),
            #         size = 5, colour = 'red') +
          geom_label(aes(label = paste0(unanimous_percentage, '%')), size = 4) +
          theme_minimal() +
          labs(x = '\nTerm', y = 'Cases Decided Unanimously\n') +
          theme(
            panel.border = element_rect(size = 1, colour = 'black', fill = NA),
            axis.text = element_text(size = 14, colour = 'black'),
            axis.title = element_text(size = 16, colour = 'black'),
            legend.text = element_text(size = 14, colour = 'black'),
            legend.position = 'none',
            legend.title = element_blank(),
            legend.box.background = element_rect(size = 1, colour = 'black', fill = NA)
          )
      ))


      combined_list[['coalitions']][['unanimity_over_time']] <- suppressWarnings(unanimity_over_time)

      suppressWarnings(ggsave(unanimity_over_time,
                              filename = file.path(output_folder, 'unanimity_over_time.png'),
                              width = 10,
                              height = 6,
                              units = 'in',
                              bg = 'white'))


    } # Unanimity Current v/ Over Time (2 Lines -- One as percentage other as normalized docket)

    {

      unanimity_binomial_proportion <- scdb_cases_data %>%
        filter(term >= 2005) %>%
        select(term, minVotes, docket) %>%
        group_by(term) %>%
        mutate(total_cases = n()) %>%
        ungroup() %>%
        filter(minVotes == 0) %>%
        group_by(term) %>%
        mutate(unanimous_cases = n()) %>%
        reframe(unanimous_percentage = round(unanimous_cases/total_cases, 2)*100,
                total_cases = total_cases,
                unanimous_cases = unanimous_cases) %>%
        ungroup() %>%
        unique() %>%
        bind_rows(decisions %>%
                    select(Coalition) %>%
                    rename(coalition = Coalition) %>%
                    mutate(total_cases = n()) %>%
                    filter(grepl('(Per Curiam|(9-0))', coalition, ignore.case = T)) %>%
                    mutate(unanimous_cases = n()) %>%
                    mutate(unanimous_percentage = round(unanimous_cases/total_cases, 2)*100) %>%
                    unique() %>%
                    mutate(term = 2024) %>%
                    select(term, unanimous_percentage, total_cases, unanimous_cases)) %>%
        unique() %>%
        mutate(
          mean_rate = mean(unanimous_cases / total_cases),
          expected = total_cases * mean_rate,
          se = sqrt(total_cases * mean_rate * (1 - mean_rate)),
          z_score = (unanimous_cases - expected) / se) %>%
        ggplot(aes(x = term, y = z_score)) +
        geom_col(colour = 'black', fill = 'gray75') +
        geom_smooth(colour = 'black', linetype = 2, method = 'loess', formula = 'y~x') +
        scale_x_continuous(breaks = seq(2006, 2024, 2)) +
        geom_hline(aes(yintercept = 0)) +
        scale_y_continuous(lim = c(-3.5, 3.5), breaks = seq(-3, 3, 1)) +
        theme_minimal() +
        labs(x = '\nTerm',
             y = 'How Unanimous Each Term Was\nCompared to Average\n') +
        theme(
          panel.border = element_rect(size = 1, colour = 'black', fill = NA),
          axis.text = element_text(size = 14, colour = 'black'),
          axis.title = element_text(size = 16, colour = 'black'),
          legend.text = element_text(size = 14, colour = 'black'),
          legend.position = 'none',
          legend.title = element_blank(),
          legend.box.background = element_rect(size = 1, colour = 'black', fill = NA)
        )

      combined_list[['coalitions']][['unanimity_binomial_proportion']] <- unanimity_binomial_proportion

      ggsave(unanimity_binomial_proportion,
             filename = file.path(output_folder, 'unanimity_binomial_proportion.png'),
             width = 10,
             height = 6,
             units = 'in',
             bg = 'white')

    } # Unanimity (Binomial Proportion) Z = (Unanimous Cases - Total_Cases*MeanUnanimityRate)/squareroot[total_cases*mean_rate_across_all_terms*(1-p)]: This gives you a z-score that adjusts for caseload: how surprising was the number of unanimous decisions given how many cases there were?

    {


      {

        coalitions_ot24 <- decisions %>%
          select(Coalition) %>%
          rename(coalition = Coalition) %>%
          mutate(coalition = case_when(
            .default = 'Unanimous',
            grepl('(5-4)|(5-3)', coalition) ~ '(5-3) & (5-4)',
            grepl('(7-1)|(7-2)', coalition) ~ '(7-1) & (7-2)',
            grepl('(6-2)|(6-3)', coalition) ~ '(6-2) & (6-3)',
            grepl('(8-1)', coalition) ~ '(8-1)',
            grepl('(7-2)', coalition) ~ '(7-2)',
            grepl('(4-4)', coalition) ~ '(4-4)')) %>%
          group_by(coalition) %>%
          summarise(count = n(), .groups = 'drop') %>%
          mutate(term = 2024)

        coalition_colors <- c(
          "Unanimous" = "#2166ac",             # blue
          "(8-1)" = "#67a9cf",                 # light blue
          "(7-1) & (7-2)" = "#fddbc7",         # peach
          "(6-2) & (6-3)" = "#ef8a62",         # orange-red
          "(5-3) & (5-4)" = "#b2182b",         # deep red
          "(4-4)" = "#d6604d",                 # reddish
          "Other" = "#cccccc"                  # grey
        )


        ot05_ot24 <- scdb_cases_data %>%
          filter(term >= 2005) %>%
          select(minVotes, majVotes) %>%
          mutate(total_cases = n(),
                 coalition = paste0('(', majVotes, '-', minVotes, ')'),
                 coalition = ifelse(grepl('-0)', coalition, ignore.case = T), 'Unanimous', coalition)) %>%
          ungroup() %>%
          group_by(coalition) %>%
          reframe(coalition_count = n(),
                  total_cases = total_cases) %>%
          unique() %>%
          mutate(coalition_percentage = coalition_count/total_cases) %>%
          bind_rows(coalitions_ot24 %>%
                      select(-c(term)) %>%
                      rename(coalition_count = count)) %>%
          mutate(coalition = ifelse(coalition %in% c('(5-4)', '(5-3)'), '(5-3) & (5-4)', coalition),
                 coalition = ifelse(grepl('\\(4-', coalition), '(4-4)', coalition),
                 coalition = ifelse(coalition %in% c('(7-1)', '(7-2)'), '(7-1) & (7-2)', coalition),
                 coalition = ifelse(coalition %in% c('(6-2)', '(6-3)'), '(6-2) & (6-3)', coalition)) %>%
          group_by(coalition) %>%
          reframe(coalition_count = sum(coalition_count),
                  total_cases = sum(coalitions_ot24$count) + total_cases,
                  coalition_percentage = coalition_count/total_cases) %>%
          unique() %>%
          filter(!is.na(total_cases)) %>%
          mutate(label_text = paste0(coalition, "\n", round(coalition_percentage * 100, 1), "%"),
                 label_pos = cumsum(coalition_percentage) - coalition_percentage / 2) %>%
          mutate(coalition = ifelse(!coalition %in% c("(5-3) & (5-4)", "(6-2) & (6-3)", "(7-1) & (7-2)", "(8-1)", "Unanimous"), 'Other', coalition)) %>%
          group_by(coalition) %>%
          reframe(coalition_percentage = sum(coalition_percentage),
                  label_text = paste0(coalition, "\n", round(coalition_percentage * 100, 1), "%"),
                  label_pos = cumsum(coalition_percentage) - coalition_percentage / 2) %>%
          unique() %>%
          ggplot(aes(x = "", y = coalition_percentage, fill = coalition)) +
          geom_bar(stat = "identity", width = 1, color = "black") +
          coord_polar(theta = "y") +
          scale_fill_manual(values = coalition_colors) +
          theme_void() +
          labs(title = '2005-2024 Terms') +
          theme(legend.title = element_blank(),
                plot.title = element_markdown(hjust = 0.5, size = 18, face = 'bold'),
                legend.position = 'none') +
          geom_text(aes(x = 1.85, label = paste0(coalition, '\n', scales::percent(coalition_percentage, accuracy = .1))),
                    position = position_stack(vjust = .5), size = 4)


        ot20_ot24 <-  scdb_cases_data %>%
          filter(term >= 2020) %>%
          select(minVotes, majVotes) %>%
          mutate(total_cases = n(),
                 coalition = paste0('(', majVotes, '-', minVotes, ')'),
                 coalition = ifelse(grepl('-0)', coalition, ignore.case = T), 'Unanimous', coalition)) %>%
          ungroup() %>%
          group_by(coalition) %>%
          reframe(coalition_count = n(),
                  total_cases = total_cases) %>%
          unique() %>%
          mutate(coalition_percentage = coalition_count/total_cases) %>%
          bind_rows(coalitions_ot24 %>%
                      select(-c(term)) %>%
                      rename(coalition_count = count)) %>%
          mutate(coalition = ifelse(coalition %in% c('(5-4)', '(5-3)'), '(5-3) & (5-4)', coalition),
                 coalition = ifelse(grepl('\\(4-', coalition), '(4-4)', coalition),
                 coalition = ifelse(coalition %in% c('(7-1)', '(7-2)'), '(7-1) & (7-2)', coalition),
                 coalition = ifelse(coalition %in% c('(6-2)', '(6-3)'), '(6-2) & (6-3)', coalition)) %>%
          group_by(coalition) %>%
          reframe(coalition_count = sum(coalition_count),
                  total_cases = sum(coalitions_ot24$count) + total_cases,
                  coalition_percentage = coalition_count/total_cases) %>%
          unique() %>%
          filter(!is.na(total_cases)) %>%
          mutate(label_text = paste0(coalition, "\n", round(coalition_percentage * 100, 1), "%"),
                 label_pos = cumsum(coalition_percentage) - coalition_percentage / 2) %>%
          mutate(coalition = ifelse(!coalition %in% c("(5-3) & (5-4)", "(6-2) & (6-3)", "(7-1) & (7-2)", "(8-1)", "Unanimous"), 'Other', coalition)) %>%
          group_by(coalition) %>%
          reframe(coalition_percentage = sum(coalition_percentage),
                  label_text = paste0(coalition, "\n", round(coalition_percentage * 100, 1), "%"),
                  label_pos = cumsum(coalition_percentage) - coalition_percentage / 2) %>%
          unique() %>%
          ggplot(aes(x = "", y = coalition_percentage, fill = coalition)) +
          geom_bar(stat = "identity", width = 1, color = "black") +
          coord_polar(theta = "y") +
          scale_fill_manual(values = coalition_colors) +
          theme_void() +
          labs(title = '2020-2024 Terms') +
          theme(legend.title = element_blank(),
                plot.title = element_markdown(hjust = 0.5, size = 18, face = 'bold'),
                legend.position = 'none') +
          geom_text(aes(x = 1.85, label = paste0(coalition, '\n', scales::percent(coalition_percentage, accuracy = .1))),
                    position = position_stack(vjust = .5), size = 4)


        ot24 <- coalitions_ot24 %>%
          select(-c(term)) %>%
          rename(minVotes = coalition,
                 coalition_count = count) %>%
          group_by(minVotes) %>%
          reframe(coalition_count = sum(coalition_count),
                  total_cases = sum(coalitions_ot24$count),
                  coalition_percentage = coalition_count/total_cases) %>%
          unique() %>%
          ggplot(aes(x = "", y = coalition_percentage, fill = minVotes)) +
          geom_bar(stat = "identity", width = 1, color = "black") +
          coord_polar(theta = "y") +
          scale_fill_manual(values = coalition_colors) +
          theme_void() +
          labs(title = '2024 Term') +
          theme(legend.title = element_blank(),
                plot.title = element_markdown(hjust = 0.5, size = 18, face = 'bold'),
                legend.position = 'none') +
          geom_text(aes(x = 1.85, label = paste0(minVotes, '\n', scales::percent(coalition_percentage, accuracy = .1))),
                    position = position_stack(vjust = .5), size = 4)


      } # 3 Indiviudal Figures

      combined_coalitions <- ot05_ot24 + ot20_ot24 + ot24 +
        plot_layout(guides = 'collect') &
        theme(legend.position = "none")

      combined_list[['coalitions']][['combined_coalitions']] <- combined_coalitions


      ggsave(combined_coalitions,
             filename = file.path(output_folder, 'combined_coalitions.png'),
             width = 14,
             height = 8,
             units = 'in')

    } # Coalition Split Pie Charts (O5-24, 20-24, 24)

    {

      {

        justice_ideologies <- scdb_justices_data %>%
          filter(term >= 2005) %>%
          select(justiceName) %>%
          mutate(justice = sub(".*([A-Z][a-zA-Z]*).*$", "\\1", justiceName),
                 justice = ifelse(justice == 'Connor', "O'Connor", justice)) %>%
          select(justice) %>%
          unique() %>%
          mutate(ideology = ifelse(justice %in% c('Stevens', 'Ginsburg', 'Breyer', 'Sotomayor', 'Kagan', 'Jackson', 'Souter'), 'Liberal', 'Conservative'))

        libs <- justice_ideologies$justice[justice_ideologies$ideology == 'Liberal']

        ideologically_split <- c()
        unique_dockets <- scdb_justices_data %>%
          filter(term >= 2005) %>%
          filter(minVotes %in% c(3, 4)) %>%
          select(docket) %>%
          unique() %>%
          pull(docket)

        for (i in 1:length(unique_dockets)){

          temp_case <- scdb_justices_data %>%
            filter(term >= 2005) %>%
            filter(docket == unique_dockets[i]) %>%
            mutate(justice = sub(".*([A-Z][a-zA-Z]*).*$", "\\1", justiceName),
                   justice = ifelse(justice == 'Connor', "O'Connor", justice)) %>%
            select(justice, majority, docket)

          maj <- temp_case$justice[temp_case$majority == 2]
          min <- temp_case$justice[!temp_case$majority == 2]

          if (all(min %in% libs) & all(!(maj %in% libs))) {
            ideologically_split <- c(ideologically_split, temp_case$docket[1])
          }

        }

        ot24_decisions <- decisions[,c(8:17)] %>%
          pivot_longer(cols = -Docket,
                       names_to = "justice",
                       values_to = "vote") %>%
          rename(docket = Docket) %>%
          mutate(majority = ifelse(vote >= 1, 2, 1))

        unique_dockets <- unique(ot24_decisions$docket)

        for (i in 1:length(unique_dockets)){

          temp_case <- ot24_decisions %>%
            filter(docket == unique_dockets[i]) %>%
            mutate(justice = str_to_title(justice))

          maj <- temp_case$justice[temp_case$majority == 2]
          min <- temp_case$justice[!temp_case$majority == 2]

          if (all(min %in% libs) & all(!(maj %in% libs))) {
            ideologically_split <- c(ideologically_split, temp_case$docket[1])
          }

        }


      } # Get Ideologically Split Cases

      {

        total_cases_1 <- nrow(scdb_cases_data[scdb_cases_data$term >= 2005,]) + length(unique(ot24_decisions$docket))
        total_cases_2 <- nrow(scdb_cases_data[scdb_cases_data$term >= 2020,]) + length(unique(ot24_decisions$docket))

        splits_ot05_ot24 <- scdb_cases_data %>%
          filter(term >= 2005) %>%
          select(term, docket) %>%
          bind_rows(ot24_decisions %>%
                      select(docket) %>%
                      unique() %>%
                      mutate(term = 2024)) %>%
          mutate(ideologically_split = ifelse(docket %in% ideologically_split, 1, 0)) %>%
          group_by(ideologically_split) %>%
          summarise(split_percentage = n() / total_cases_1 * 100) %>%
          ungroup() %>%
          mutate(ideologically_split = ifelse(ideologically_split == 1, 'Ideologically Split', 'No Split')) %>%
          ggplot(aes(x = "", y = split_percentage, fill = ideologically_split)) +
          geom_bar(stat = "identity", width = 1, color = "black") +
          coord_polar(theta = "y") +
          scale_fill_brewer(name = 'RdB') +
          theme_void() +
          labs(title = '2005-2024 Terms') +
          theme(legend.title = element_blank(),
                plot.title = element_markdown(hjust = 0.5, size = 18, face = 'bold'),
                legend.position = 'none') +
          geom_text(aes(x = 1.7, label = paste0(ideologically_split, '\n', round(split_percentage, 2), '%')),
                    position = position_stack(vjust = .5), size = 4)

        splits_ot20_ot24 <- scdb_cases_data %>%
          filter(term >= 2020) %>%
          select(term, docket) %>%
          bind_rows(ot24_decisions %>%
                      select(docket) %>%
                      unique() %>%
                      mutate(term = 2024)) %>%
          mutate(ideologically_split = ifelse(docket %in% ideologically_split, 1, 0)) %>%
          group_by(ideologically_split) %>%
          summarise(split_percentage = n() / total_cases_2 * 100) %>%
          ungroup() %>%
          mutate(ideologically_split = ifelse(ideologically_split == 1, 'Ideologically Split', 'No Split')) %>%
          ggplot(aes(x = "", y = split_percentage, fill = ideologically_split)) +
          geom_bar(stat = "identity", width = 1, color = "black") +
          coord_polar(theta = "y") +
          scale_fill_brewer(name = 'RdB') +
          theme_void() +
          labs(title = '2020-2024 Terms') +
          theme(legend.title = element_blank(),
                plot.title = element_markdown(hjust = 0.5, size = 18, face = 'bold'),
                legend.position = 'none') +
          geom_text(aes(x = 1.7, label = paste0(ideologically_split, '\n', round(split_percentage, 2), '%')),
                    position = position_stack(vjust = .5), size = 4)

        splits_ot24 <- ot24_decisions %>%
          select(docket) %>%
          unique() %>%
          mutate(ideologically_split = ifelse(docket %in% ideologically_split, 1, 0)) %>%
          group_by(ideologically_split) %>%
          summarise(split_percentage = n() / length(unique(ot24_decisions$docket)) * 100) %>%
          ungroup() %>%
          mutate(ideologically_split = ifelse(ideologically_split == 1, 'Ideologically Split', 'No Split')) %>%
          ggplot(aes(x = "", y = split_percentage, fill = ideologically_split)) +
          geom_bar(stat = "identity", width = 1, color = "black") +
          coord_polar(theta = "y") +
          scale_fill_brewer(name = 'RdB') +
          theme_void() +
          labs(title = '2024 Term') +
          theme(legend.title = element_blank(),
                plot.title = element_markdown(hjust = 0.5, size = 18, face = 'bold'),
                legend.position = 'none') +
          geom_text(aes(x = 1.7, label = paste0(ideologically_split, '\n', round(split_percentage, 2), '%')),
                    position = position_stack(vjust = .5), size = 4)

      } # Compile Figures

      combined_splits <-  (splits_ot05_ot24 + splits_ot20_ot24 + splits_ot24) +
        plot_layout(guides = 'collect') &
        theme(legend.position = "none")

      combined_list[['coalitions']][['ideologically_split_cases']] <- combined_splits


      ggsave(combined_splits,
             filename = file.path(output_folder, 'combined_splits.png'),
             width = 12,
             height = 8,
             units = 'in')


    } # Ideologically Split Cases (Pie Charts)

    {

      custom_order <- c("ROBERTS", "ALITO", "THOMAS", "SOTOMAYOR", "KAGAN",
                        "GORSUCH", "KAVANAUGH", "BARRETT", "JACKSON")


      OT24_majority_coalitions <- decisions %>%
        left_join(master_file %>%
                    select(short_hand, docket) %>%
                    rename(Docket = docket), by = "Docket") %>%
        filter(!grepl('(9-0|Per Curiam)', Coalition, ignore.case = T)) %>%
        rowwise() %>%
        mutate(
          majority = paste0(names(decisions)[8:16][c_across(8:16) >= 1], collapse = ", ")
        ) %>%
        ungroup() %>%
        select(short_hand, majority) %>%
        mutate(
          majority_sorted = majority %>%
            str_split(",\\s*") %>%
            lapply(function(justices) {
              ordered <- factor(justices, levels = custom_order)
              sort(ordered)
            }) %>%
            sapply(paste, collapse = ", ")
        ) %>%
        group_by(majority_sorted) %>%
        summarise(
          count = n(),
          case = paste(short_hand, collapse = '; '),
          .groups = "drop"
        ) %>%
        arrange(desc(count)) %>%
        mutate(majority_sorted = str_to_title(majority_sorted))

      combined_list[['coalitions']][['ot24_coalitions']] <- OT24_majority_coalitions


    } # Most Common Coalitions (OT24)

    {

      custom_order <- c('Roberts', 'Stevens', 'Scalia', 'Kennedy', 'Souter', 'Thomas', 'Ginsburg', 'Breyer', 'Alito', 'Sotomayor', 'Kagan', 'Gorsuch', 'Kavanaugh', 'Barrett', 'Jackson')
      libs <- c('Stevens', 'Sotomayor', 'Kagan', 'Jackson', 'Ginsburg', 'Breyer')


      majorities <- data.frame()
      unique_dockets <- scdb_cases_data %>%
        filter(term >= 2010) %>%
        select(docket) %>%
        pull(docket)

      for (i in 1:length(unique_dockets)){

        temp_case <- scdb_justices_data %>%
          filter(docket == unique_dockets[i]) %>%
          select(justiceName, majority, decisionType, docket, term, caseId)

        if (temp_case$decisionType[1] %in% c(4, 5)){
          next
        }

        if (nrow(temp_case) >= 10){
          unique_case_ids <- unique(temp_case$caseId)
          temp_case <- temp_case %>%
            filter(caseId == unique_case_ids[1])
        }

        temp_justices <- temp_case %>%
          mutate(justice = sub(".*([A-Z][a-zA-Z]*).*$", "\\1", justiceName)) %>%
          pull(justice)

        temp_majority <- temp_case %>%
          filter(majority == 2) %>%
          mutate(justice = sub(".*([A-Z][a-zA-Z]*).*$", "\\1", justiceName)) %>%
          select(justice) %>%
          mutate(
            majority_sorted = justice %>%
              str_split(",\\s*") %>%
              lapply(function(justices) {
                ordered <- factor(justices, levels = custom_order)
                sort(ordered)
              }) %>%
              sapply(paste, collapse = ", ")
          ) %>%
          pull(majority_sorted)

        temp_cons <- temp_justices[!temp_justices %in% libs]
        temp_libs <- temp_justices[temp_justices %in% libs]
        majority_size <- length(which(temp_case$majority == 2))
        minority_size <- length(which(temp_case$majority == 1))
        coalition_combined <- paste0('(', majority_size, '-', minority_size, ')')
        ideological_split <- ifelse(any(temp_libs %in% temp_majority), 0, 1)
        conservative_win <- ifelse(all(temp_libs %in% temp_majority), 0, 1)

        temp_combined <- data.frame(
          docket = temp_case$docket[1],
          term = temp_case$term[1],
          majority_size = majority_size,
          minority_size = minority_size,
          coalition_combined = coalition_combined,
          ideological_split = ideological_split,
          conservative_win = conservative_win,
          stringsAsFactors = FALSE
        )

        temp_combined$majority_coalition <- list(temp_majority)
        temp_combined$temp_justices <- list(temp_justices)

        majorities <- bind_rows(majorities, temp_combined)

      }

      {

        all_splits <- majorities %>%
          filter(majority_size %in% c(5, 6)) %>%
          bind_rows(decisions %>%
                      filter(grepl('(5-4|5-3|6-3|6-2)', Coalition, ignore.case = T)) %>%
                      rename(majority_size = Coalition) %>%
                      mutate(majority_size = gsub('\\-.*', '', gsub('\\(', '', majority_size)),
                             majority_size = as.numeric(majority_size)) %>%
                      select(majority_size) %>%
                      mutate(term = 2024)) %>%
          group_by(term) %>%
          summarise(count = n()) %>%
          mutate(term = as.character(term))

        all_splits <- all_splits %>%
          add_row(term = 'Average',
                  count = mean(all_splits$count))

        combined_list[['coalitions']][['all_splits']] <- all_splits

      } # All Splits

      {

        total_cases_term <- scdb_cases_data %>%
          filter(term >= 2010) %>%
          select(term) %>%
          group_by(term) %>%
          summarise(total_cases = n()) %>%
          bind_rows(data.frame(term = 2024,
                               total_cases = nrow(decisions)))

        percent_split <- majorities %>%
          mutate(split = ifelse(majority_size %in% c(5, 6), 1, 0)) %>%
          bind_rows(decisions %>%
                      mutate(split = grepl('(5-4|5-3|6-3|6-2)', Coalition, ignore.case = T),
                             Coalition = ifelse(Coalition == 'Per Curiam', '(9-0)', Coalition)) %>%
                      rename(majority_size = Coalition) %>%
                      mutate(majority_size = gsub('\\-.*', '', gsub('\\(', '', majority_size)),
                             majority_size = as.numeric(majority_size)) %>%
                      select(majority_size, split) %>%
                      mutate(term = 2024)) %>%
          select(split, term) %>%
          filter(split == 1) %>%
          group_by(term) %>%
          summarise(count = n(), .groups = 'drop') %>%
          left_join(total_cases_term, by = 'term') %>%
          mutate(percent_split = round(count/total_cases, 2)*100) %>%
          select(term, percent_split) %>%
          mutate(term = as.character(term))

        percent_split <- percent_split %>%
          add_row(term = 'Average',
                  percent_split = mean(percent_split$percent_split))


        combined_list[['coalitions']][['percent_split_of_total_cases']] <- percent_split

      } # Splits As Percent of Total Cases

      {

        ot24_ideological_splits <- decisions[, c(7:17)] %>%
          filter(grepl('6-3', Coalition)) %>%
          rowwise() %>%
          mutate(ideological_split = ifelse(all(c(JACKSON, SOTOMAYOR, KAGAN) <= -1), 1, 0)) %>%
          select(ideological_split) %>%
          summarise(ideological_split = length(which(ideological_split == 1))) %>%
          mutate(term = 2024,
                 total_cases = nrow(decisions))


        ideological_splits <- majorities %>%
          group_by(term) %>%
          mutate(total_cases = n()) %>%
          filter(ideological_split == 1) %>%
          group_by(term) %>%
          reframe(total_cases = total_cases,
                  ideological_split = n()) %>%
          unique() %>%
          bind_rows(ot24_ideological_splits) %>%
          mutate(percent_ideological_split = round(ideological_split/total_cases, 2)*100) %>%
          select(term, percent_ideological_split) %>%
          mutate(term = as.character(term))


        ideological_splits <- ideological_splits %>%
          add_row(term = 'Average',
                  percent_ideological_split = mean(ideological_splits$percent_ideological_split))

        combined_list[['coalitions']][['percent_ideological_splits']] <- ideological_splits

      } # Ideological Splits

      {

        unique_coalitions <- data.frame()
        terms <- unique(majorities$term)

        for (i in 1:length(unique(terms))){

          temp_coalitions <- majorities %>%
            filter(term == terms[i]) %>%
            pull(majority_coalition)

          temp_coalitions <- unique(lapply(temp_coalitions, function(x) sort(x)))
          temp_coalitions <- length(temp_coalitions)
          unique_coalitions <- bind_rows(unique_coalitions, data.frame(term = terms[i],
                                                                       unique_coalitions = temp_coalitions))
        }

        unique_coalitions <- unique_coalitions %>%
          add_row(data.frame(term = 2024,
                             unique_coalitions = length(unique(OT24_majority_coalitions$majority_sorted)))) %>%
          mutate(term = as.character(term)) %>%
          add_row(term = 'Average',
                  unique_coalitions = mean(unique_coalitions$unique_coalitions, length(unique(OT24_majority_coalitions$majority_sorted))))

        combined_list[['coalitions']][['unique_coalitions']] <- unique_coalitions

      } # Different Majority Coalitions




    } # Most Common Coalitions (OT10-23)

    {

      custom_order <- c('Roberts', 'Stevens', 'Scalia', 'Kennedy', 'Souter', 'Thomas', 'Ginsburg', 'Breyer', 'Alito', 'Sotomayor', 'Kagan', 'Gorsuch', 'Kavanaugh', 'Barrett', 'Jackson')
      libs <- c('Stevens', 'Sotomayor', 'Kagan', 'Jackson', 'Ginsburg', 'Breyer')


      {

        breaks_1 <- data.frame()
        unique_dockets <- scdb_cases_data %>%
          filter(term >= 2020) %>%
          filter(minVotes >= 1) %>%
          select(docket) %>%
          pull(docket)

        for (i in 1:length(unique_dockets)){

          temp_case <- scdb_justices_data %>%
            filter(docket == unique_dockets[i]) %>%
            filter(term >= 2020) %>%
            filter(!is.na(majority)) %>%
            select(justiceName, majority, decisionType, docket, term, caseId)

          if (temp_case$decisionType[1] %in% c(4, 5)){
            next
          }

          if (nrow(temp_case) >= 10){
            unique_case_ids <- unique(temp_case$caseId)
            temp_case <- temp_case %>%
              filter(caseId == unique_case_ids[1])
          }

          temp_justices <- temp_case %>%
            mutate(justice = sub(".*([A-Z][a-zA-Z]*).*$", "\\1", justiceName)) %>%
            pull(justice)

          temp_majority <- temp_case %>%
            filter(majority == 2) %>%
            mutate(justice = sub(".*([A-Z][a-zA-Z]*).*$", "\\1", justiceName)) %>%
            select(justice) %>%
            mutate(
              majority_sorted = justice %>%
                str_split(",\\s*") %>%
                lapply(function(justices) {
                  ordered <- factor(justices, levels = custom_order)
                  sort(ordered)
                }) %>%
                sapply(paste, collapse = ", ")
            ) %>%
            pull(majority_sorted)

          temp_cons <- temp_justices[!temp_justices %in% libs]
          temp_libs <- temp_justices[temp_justices %in% libs]
          majority_size <- length(which(temp_case$majority == 2))
          minority_size <- length(which(temp_case$majority == 1))
          temp_majority <- temp_majority
          temp_minority <- temp_justices[!temp_justices %in% temp_majority]

          lib_majority <- ifelse(all(temp_libs %in% temp_majority), 1, 0)
          lib_minority <- ifelse(all(temp_libs %in% temp_minority), 1, 0)

          conservative_defectors <- c()
          liberal_defectors <- c()

          if (lib_majority == 1){
            conservative_defectors <- temp_majority[!temp_majority %in% temp_libs]
          } else if (lib_minority == 1){
            conservative_defectors <- temp_minority[!temp_minority %in% temp_libs]
          }

          if (lib_majority == 0){
            liberal_defectors <- temp_majority[!temp_majority %in% temp_cons]
          } else if (lib_minority == 0){
            liberal_defectors <- temp_minority[!temp_minority %in% temp_cons]
          }

          temp_combined <- data.frame(
            docket = temp_case$docket[1],
            term = temp_case$term[1],
            majority_size = majority_size,
            minority_size = minority_size,
            stringsAsFactors = FALSE
          )

          temp_combined$majority_coalition <- list(temp_majority)
          temp_combined$minority_coalition <- list(temp_minority)
          temp_combined$conservative_defectors <- ifelse(length(conservative_defectors) == 0, NA, list(conservative_defectors))
          temp_combined$liberal_defectors <- ifelse(length(liberal_defectors) == 0, NA, list(liberal_defectors))

          temp_combined$temp_justices <- list(temp_justices)

          breaks_1 <- bind_rows(breaks_1, temp_combined)

        }

      } # OT20-23 Breaks (Measure 1)

      {

        breaks_2 <- data.frame()
        unique_dockets <- scdb_cases_data %>%
          filter(term >= 2020) %>%
          filter(minVotes >= 1) %>%
          select(docket) %>%
          pull(docket)

        for (i in 1:length(unique_dockets)){

          temp_case <- scdb_justices_data %>%
            filter(docket == unique_dockets[i]) %>%
            filter(term >= 2020) %>%
            filter(!is.na(majority)) %>%
            select(justiceName, majority, decisionType, docket, term, caseId)

          if (temp_case$decisionType[1] %in% c(4, 5)){
            next
          }

          if (nrow(temp_case) >= 10){
            unique_case_ids <- unique(temp_case$caseId)
            temp_case <- temp_case %>%
              filter(caseId == unique_case_ids[1])
          }

          temp_justices <- temp_case %>%
            mutate(justice = sub(".*([A-Z][a-zA-Z]*).*$", "\\1", justiceName)) %>%
            pull(justice)

          temp_majority <- temp_case %>%
            filter(majority == 2) %>%
            mutate(justice = sub(".*([A-Z][a-zA-Z]*).*$", "\\1", justiceName)) %>%
            select(justice) %>%
            mutate(
              majority_sorted = justice %>%
                str_split(",\\s*") %>%
                lapply(function(justices) {
                  ordered <- factor(justices, levels = custom_order)
                  sort(ordered)
                }) %>%
                sapply(paste, collapse = ", ")
            ) %>%
            pull(majority_sorted)

          temp_cons <- temp_justices[!temp_justices %in% libs]
          temp_libs <- temp_justices[temp_justices %in% libs]
          majority_size <- length(which(temp_case$majority == 2))
          minority_size <- length(which(temp_case$majority == 1))
          temp_majority <- temp_majority
          temp_minority <- temp_justices[!temp_justices %in% temp_majority]


          if (sum(temp_cons %in% temp_majority) == length(temp_cons) - 1) {
            conservative_defectors <- temp_cons[!temp_cons %in% temp_majority]
          } else {
            conservative_defectors <- NA
          } # Lone Conservative Defector

          if (sum(temp_libs %in% temp_majority) == length(temp_libs) - 1) {
            liberal_defectors <- temp_libs[!temp_libs %in% temp_majority]
          } else {
            liberal_defectors <- NA
          } # Lone Liberal Defector

          temp_combined <- data.frame(
            docket = temp_case$docket[1],
            term = temp_case$term[1],
            majority_size = majority_size,
            minority_size = minority_size,
            stringsAsFactors = FALSE
          )

          temp_combined$majority_coalition <- list(temp_majority)
          temp_combined$minority_coalition <- list(temp_minority)
          temp_combined$liberal_defectors <- ifelse(is.na(liberal_defectors), NA, list(liberal_defectors))
          temp_combined$conservative_defectors <- ifelse(is.na(conservative_defectors), NA, list(conservative_defectors))
          temp_combined$temp_justices <- list(temp_justices)

          breaks_2 <- bind_rows(breaks_2, temp_combined)

        }

      } # OT20-23 Breaks (Measure 2)

      {

        total_cases_OT24 <- nrow(decisions)
        ot24_breaks_1 <- data.frame()
        temp_libs <- libs[libs %in% str_to_title(names(decisions[8:16]))]

        for (i in 1:nrow(decisions)){

          if (decisions[i,]$Coalition %in% c('(9-0)', 'Per Curiam', '(4-4)*', '(8-0)')){
            next
          }

          temp_case <- decisions[i,c(8:16)]
          temp_majority <- str_to_title(names(temp_case)[temp_case >= 1])
          temp_minority <- str_to_title(names(temp_case)[temp_case <= -1])
          lib_majority <- ifelse(all(temp_libs %in% temp_majority), 1, 0)
          lib_minority <- ifelse(all(temp_libs %in% temp_minority), 1, 0)
          lib_majority <- ifelse(all(temp_libs %in% temp_majority), 1, 0)
          lib_minority <- ifelse(all(temp_libs %in% temp_minority), 1, 0)

          conservative_defectors <- c()
          liberal_defectors <- c()

          if (lib_majority == 1){
            conservative_defectors <- temp_majority[!temp_majority %in% temp_libs]
          } else if (lib_minority == 1){
            conservative_defectors <- temp_minority[!temp_minority %in% temp_libs]
          }

          if (lib_majority == 0){
            liberal_defectors <- temp_majority[!temp_majority %in% temp_cons]
          } else if (lib_minority == 0){
            liberal_defectors <- temp_minority[!temp_minority %in% temp_cons]
          }


          temp_combined <- data.frame(
            docket = decisions[i,]$Docket,
            term = 2024,
            stringsAsFactors = FALSE
          )

          temp_combined$majority_coalition <- list(temp_majority)
          temp_combined$minority_coalition <- list(temp_minority)
          temp_combined$conservative_defectors <- ifelse(length(conservative_defectors) == 0, NA, list(conservative_defectors))
          temp_combined$liberal_defectors <- ifelse(length(liberal_defectors) == 0, NA, list(liberal_defectors))

          temp_combined$temp_justices <- list(temp_justices)

          ot24_breaks_1 <- bind_rows(ot24_breaks_1, temp_combined)

        }



      } # OT24 Breaks (Measure 1)

      {

        total_cases_OT24 <- nrow(decisions)
        ot24_breaks_2 <- data.frame()
        temp_libs <- libs[libs %in% str_to_title(names(decisions[8:16]))]
        temp_cons <- str_to_title(names(decisions[8:16]))
        temp_cons <- temp_cons[!temp_cons %in% temp_libs]

        for (i in 1:nrow(decisions)){

          if (decisions[i,]$Coalition %in% c('(9-0)', 'Per Curiam', '(4-4)*', '(8-0)')){
            next
          }

          temp_case <- decisions[i,c(8:16)]
          temp_majority <- str_to_title(names(temp_case)[temp_case >= 1])
          temp_majority <- temp_majority[!is.na(temp_majority)]
          temp_minority <- str_to_title(names(temp_case)[temp_case <= -1])
          temp_minority <- temp_minority[!is.na(temp_minority)]
          lib_majority <- ifelse(all(temp_libs %in% temp_majority), 1, 0)
          lib_minority <- ifelse(all(temp_libs %in% temp_minority), 1, 0)

          if (sum(temp_cons %in% temp_majority) == length(temp_cons) - 1) {
            conservative_defectors <- temp_cons[!temp_cons %in% temp_majority]
          } else {
            conservative_defectors <- NA
          } # Lone Conservative Defector

          if (sum(temp_libs %in% temp_majority) == length(temp_libs) - 1) {
            liberal_defectors <- temp_libs[!temp_libs %in% temp_majority]
          } else {
            liberal_defectors <- NA
          } # Lone Liberal Defector


          temp_combined <- data.frame(
            docket = decisions[i,]$Docket,
            term = 2024,
            stringsAsFactors = FALSE
          )

          temp_combined$majority_coalition <- list(temp_majority)
          temp_combined$minority_coalition <- list(temp_minority)
          temp_combined$liberal_defectors <- ifelse(is.na(liberal_defectors), NA, list(liberal_defectors))
          temp_combined$conservative_defectors <- ifelse(is.na(conservative_defectors), NA, list(conservative_defectors))
          temp_combined$temp_justices <- list(temp_justices)

          ot24_breaks_2 <- bind_rows(ot24_breaks_2, temp_combined)

        }



      } # OT24 Breaks (Measure 2)

      {

        total_cases_term <- scdb_cases_data %>%
          filter(term >= 2020) %>%
          filter(minVotes >= 1) %>%
          select(term, docket) %>%
          unique() %>%
          group_by(term) %>%
          summarise(total_cases = n()) %>%
          add_row(term = 2024, total_cases = nrow(decisions[!grepl('(9-0|Per Curiam|4-4|8-0)', decisions$Coalition, ignore.case = T),]))

        breaks <- breaks_1 %>%
          mutate(type = 'M1') %>%
          bind_rows(breaks_2 %>%
                      mutate(type = 'M2')) %>%
          bind_rows(ot24_breaks_1 %>%
                      mutate(type = 'M1')) %>%
          bind_rows(ot24_breaks_2 %>%
                      mutate(type = 'M2')) %>%
          select(term, conservative_defectors, liberal_defectors, type, docket) %>%
          mutate(conservative_defectors = lapply(conservative_defectors, function(x) if (is.null(x)) NA else x)) %>%
          mutate(liberal_defectors = lapply(liberal_defectors, function(x) if (is.null(x)) NA else x)) %>%
          unnest(conservative_defectors) %>%
          unnest(liberal_defectors) %>%
          mutate(defectors = ifelse(!is.na(conservative_defectors), conservative_defectors, liberal_defectors)) %>%
          filter(!is.na(defectors)) %>%
          group_by(term, defectors, type) %>%
          summarise(count = n(),
                    .groups = 'drop') %>%
          unique() %>%
          group_by(defectors, type, term) %>%
          summarise(total_defections = sum(count), .groups = 'drop') %>%
          group_by(defectors, type) %>%
          summarise(total_defections = sum(total_defections), .groups = 'drop') %>%
          unique() %>%
          mutate(total_cases = sum(total_cases_term$total_cases)) %>%
          mutate(percent_defection = round(total_defections/total_cases, 2)) %>%
          rename(justice = defectors) %>%
          filter(!justice == 'Breyer')


        justice_labels <- breaks %>%
          select(justice) %>%
          mutate(justice = toupper(justice)) %>%
          mutate(image_labels = justice_image_labels[match(justice, names(justice_image_labels))],
                 image_labels = gsub(' style\\=.*', '', image_labels),
                 image_labels = paste0(image_labels, " width='100' /><br><strong>", justice, "</strong>")) %>%
          distinct(justice, .keep_all = TRUE) %>%
          arrange(justice) %>%
          pull(image_labels)

        defections <-  breaks %>%
          #add_row(justice = 'Kavanaugh', type = 'M2', percent_defection = 0) %>%
          mutate(type = ifelse(type == 'M1', 'Joined Coalition w/\nAll Democratic-Appointees', 'Sole Dissenter')) %>%
          ggplot(aes(x = justice, y = percent_defection, fill = type)) +
          geom_col(colour = 'black', position = position_dodge(0.9)) +
          scale_fill_manual(values = c('grey25', 'grey75')) +
          scale_y_continuous(lim = c(0, 0.5)) +
          geom_label(
            aes(
              label = paste0(round(percent_defection * 100), '%'),
              color = type,  # text color
              group = type   # ensures correct dodging
            ),
            fill = "white",  # label background
            position = position_dodge(0.9),
            vjust = -0.25,
            size = 4,
            show.legend = FALSE) +
          scale_color_manual(values = c('black', 'black')) +
          geom_hline(yintercept = 0) +
          theme_minimal() +
          scale_x_discrete(labels = justice_labels) +  # Use the labels with images for the x-axis
          labs(x = '',
               y = '',
               fill = '') +
          theme_minimal() +
          theme(
            panel.background = element_rect(size = 1, colour = 'black', fill = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = ggtext::element_markdown(size = 10, vjust = 1),
            axis.text.y = element_blank(),
            axis.title = element_text(size = 16, colour = 'black'),
            legend.text = element_text(size = 14, colour = 'black'),
            legend.position = 'none',
            legend.title = element_blank(),
            legend.box.background = element_rect(size = 1, colour = 'black', fill = NA))

      } # Conservative Defectors

      combined_list[['coalitions']][['conservative_defections']] <- breaks
      combined_list[['coalitions']][['conservative_defections_figure']] <- defections

      suppressWarnings(ggsave(defections,
                              filename = file.path(output_folder, 'conservative_defections.png'),
                              width = 10,
                              height = 6,
                              units = 'in',
                              bg = 'white'))


    } # Most Common Defectors (2020-2024)

    # Measure 1: Conservatives Defecting to Coalitions with All Libs
    # Measure 2: Conservatives Being Only Defectors When All Other C's in Coalition
    # Defections/Breaking Ranks (Most Common C to Defect -- Situation where all L's are in 1 Coalition )

  } # Coalitions & Unanimity

  message('Completed Coalitions & Unanimity')

  {

    combined_list[['agreement_matrix']] <- list()

    {

      decisions_figure_data <- decisions %>%
        relocate(Docket, .after = 'Case') %>%
        mutate(Author = stringr::str_to_title(Author))

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
          mutate(image_labels = gsub('75px', '10px', image_labels)) %>%
          pull(image_labels)

        Justice1_labels <- agreement_long %>%
          select(Justice1) %>%
          rename(justice = Justice1) %>%
          mutate(image_labels = justice_image_labels[match(justice, names(justice_image_labels))],
                 image_labels = gsub(' style\\=.*', '', image_labels),
                 image_labels = paste0(image_labels, " width='100' /><br><strong>", justice, "</strong>")) %>%
          unique() %>%
          mutate(image_labels = gsub('75px', '10px', image_labels)) %>%
          pull(image_labels)

        agreement_long <- agreement_long %>%
          mutate(Agreement = Agreement * 100)

      } # Justice Image Labels

      {

        suppressWarnings(agreement_matrix_all_cases <- ggplot(data = agreement_long, aes(x = Justice1, y = Justice2)) +
                           geom_tile(color = "white", size = 0.5, aes(fill = Agreement)) +
                           geom_label(aes(label = paste0(Agreement, '%')), fill = 'white', size = 5) +
                           scale_fill_distiller(palette = 'Blues', direction = 1) +
                           theme_minimal() +
                           scale_x_discrete(labels = Justice1_labels) +  # Use the labels with images for the x-axis
                           scale_y_discrete(labels = Justice2_labels) +  # Use the labels with images for the y-axis
                           labs(x = '',
                                y = '',
                                fill = '') +
                           theme(
                             axis.text.x = ggtext::element_markdown(size = 10),  # This allows HTML rendering (for image tags) on the x-axis
                             axis.text.y = ggtext::element_markdown(hjust = 0.5, size = 10),  # Center the y-axis text under the images
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

      combined_list[['agreement_matrix']][['agreement_matrix_all_cases']] <- agreement_matrix_all_cases

      ggsave(agreement_matrix_all_cases,
             filename = file.path(output_folder, 'agreement_matrix_all_cases.png'),
             width = 12,
             height = 10,
             units = 'in',
             bg = 'white')

    } # All Cases

    {

      decisions_figure_data <- decisions %>%
        relocate(Docket, .after = 'Case') %>%
        mutate(Author = stringr::str_to_title(Author)) %>%
        filter(grepl('((5-4)|(6-3))', Coalition))

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
          mutate(image_labels = gsub('75px', '10px', image_labels)) %>%
          pull(image_labels)

        Justice1_labels <- agreement_long %>%
          select(Justice1) %>%
          rename(justice = Justice1) %>%
          mutate(image_labels = justice_image_labels[match(justice, names(justice_image_labels))],
                 image_labels = gsub(' style\\=.*', '', image_labels),
                 image_labels = paste0(image_labels, " width='100' /><br><strong>", justice, "</strong>")) %>%
          unique() %>%
          mutate(image_labels = gsub('75px', '10px', image_labels)) %>%
          pull(image_labels)

        agreement_long <- agreement_long %>%
          mutate(Agreement = Agreement * 100)

      } # Justice Image Labels

      {

        suppressWarnings(agreement_matrix_close_cases <- ggplot(data = agreement_long, aes(x = Justice1, y = Justice2)) +
                           geom_tile(color = "white", size = 0.5, aes(fill = Agreement)) +
                           geom_label(aes(label = paste0(Agreement, '%')), fill = 'white', size = 5) +
                           scale_fill_distiller(palette = 'Blues', direction = 1) +
                           theme_minimal() +
                           scale_x_discrete(labels = Justice1_labels) +  # Use the labels with images for the x-axis
                           scale_y_discrete(labels = Justice2_labels) +  # Use the labels with images for the y-axis
                           labs(x = '',
                                y = '',
                                fill = '') +
                           theme(
                             axis.text.x = ggtext::element_markdown(size = 10),  # This allows HTML rendering (for image tags) on the x-axis
                             axis.text.y = ggtext::element_markdown(hjust = 0.5, size = 10),  # Center the y-axis text under the images
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

      combined_list[['agreement_matrix']][['agreement_matrix_close_cases']] <- agreement_matrix_close_cases

      ggsave(agreement_matrix_close_cases,
             filename = file.path(output_folder, 'agreement_matrix_close_cases.png'),
             width = 12,
             height = 10,
             units = 'in',
             bg = 'white')

    } # Close Cases (5-4, 6-3)

  } # Vote Agreement

  message('Completed Vote Agreement')

  {

    combined_list[['opinion_lengths']] <- list()

    opinions_processed <- get(load(opinions_processed)) %>%
      group_by(docket, authorship) %>%
      summarise(
        opinion_text = paste(opinion_text, collapse = " "),
        opinion_type = first(opinion_type),
        date = first(date),
        case = first(case),
        justia_summary = first(justia_summary),
        .groups = "drop"
      )

    {

      opinion_lengths_by_justice <- opinions_processed %>%
        group_by(authorship) %>%
        mutate(word_count =  stri_count_words(opinion_text)) %>%
        summarise(mean_words = round(mean(word_count), 0)) %>%
        filter(!authorship %in% 'Per Curiam') %>%
        rename(justice = authorship) %>%
        mutate(justice = factor(justice, levels = c('Roberts', 'Thomas', 'Alito', 'Sotomayor', 'Kagan', 'Gorsuch', 'Kavanaugh', 'Barrett', 'Jackson'))) %>%
        arrange(justice)

      combined_list[['opinion_lengths']][['average_opinion_lengths_by_justice']] <- opinion_lengths_by_justice

      } # Average Opinion Lengths by Justice

    {
      shortest_opinions <- opinions_processed %>%
        mutate(word_count =  stri_count_words(opinion_text)) %>%
        select(authorship, docket, opinion_type, word_count) %>%
        left_join(master_file %>%
                    select(docket, short_hand), by = 'docket') %>%
        left_join(decisions %>%
                    select(Coalition, Docket, Date_Argued, Date_Decided) %>%
                    rename(coalition = Coalition,
                           docket = Docket,
                           date_argued = Date_Argued,
                           date_decided = Date_Decided), by = 'docket') %>%
        arrange(word_count) %>%
        slice_head(n = 5)

      longest_opinions <- opinions_processed %>%
        mutate(word_count =  stri_count_words(opinion_text)) %>%
        select(authorship, docket, opinion_type, word_count) %>%
        left_join(master_file %>%
                    select(docket, short_hand), by = 'docket') %>%
        left_join(decisions %>%
                    select(Coalition, Docket, Date_Argued, Date_Decided) %>%
                    rename(coalition = Coalition,
                           docket = Docket,
                           date_argued = Date_Argued,
                           date_decided = Date_Decided), by = 'docket') %>%
        arrange(desc(word_count)) %>%
        slice_head(n = 5)


      combined_list[['opinion_lengths']][['longest_opinions']] <- longest_opinions
      combined_list[['opinion_lengths']][['shortest_opinions']] <- shortest_opinions

    } # Shortest and Longest Opinions (Individual Opinions)

    {

      older_opinions <- get(load(older_opinions_processed))
      older_opinions_combined <- data.frame()

      for (i in 1:length(older_opinions)){
        temp_term <- older_opinions[[i]]
        temp_term_number <- names(older_opinions[i])
        older_opinions_combined <- bind_rows(older_opinions_combined,
                                             temp_term %>% mutate(term = temp_term_number))

      }

      opinion_lengths <- opinions_processed %>%
        mutate(word_count =  stri_count_words(opinion_text))

      opinion_lengths_by_term <-  older_opinions_combined %>%
        select(opinion_writer, opinion_type, word_count, term) %>%
        mutate(opinion_writer = gsub('(CHIEF JUSTICE |JUSTICE )', '', gsub('\\;.*', '', opinion_writer)),
               opinion_writer = str_to_title(opinion_writer),
               opinion_type = ifelse(opinion_type == 'Majority Opinion', 'Majority', opinion_type)) %>%
        bind_rows(opinion_lengths %>%
                    select(authorship, opinion_type, word_count) %>%
                    rename(opinion_writer = authorship) %>%
                    mutate(term = '2024',
                           opinion_type = case_when(
                             opinion_type %in% c('Concurrence', 'Special Concurrence') ~ 'Concurrence',
                             opinion_type %in% c('Dissent') ~ 'Dissent',
                             opinion_type == 'Per Curiam' ~ 'Per Curiam',
                             TRUE ~ 'Majority'))) %>%
        group_by(term, opinion_type) %>%
        summarise(
          mean_words = mean(word_count),
          se = sd(word_count) / sqrt(n()),
          p25 = as.numeric(quantile(word_count, 0.25)),
          p75 = as.numeric(quantile(word_count, 0.75)),
          .groups = "drop"
        ) %>%
        mutate(opinion_type = factor(opinion_type, levels = c('Majority', 'Concurrence', 'Dissent', 'Per Curiam')),
               term = as.numeric(term)) %>%
        ggplot(aes(x = term, y = mean_words, color = opinion_type, group = opinion_type)) +
        geom_point(colour = 'black') +
        geom_line(linetype = 2, colour = 'black') +
        geom_label(aes(label = round(mean_words, 0)), size = 3.5, vjust = -1, colour = 'black') +
        facet_wrap(~opinion_type, scales = 'free_y') +
        scale_y_continuous(expand = expansion(mult = c(0.25, 0.5))) +
        scale_x_continuous(breaks = seq(2016, 2024, 2), expand = expansion(mult = c(0.1, 0.1)))  +
        labs(y = 'Average Word Count\n',
             x = '\nTerm') +
        theme_minimal() +
        theme(panel.border = element_rect(size = 1, colour = 'black', fill = NA),
              axis.text = element_text(size = 14, colour = 'black'),
              axis.title = element_text(size = 16, colour = 'black'),
              strip.background = element_rect(fill = 'grey', colour = 'black'),
              strip.text = element_text(size = 14, colour = 'black'))

      combined_list[['opinion_lengths']][['opinion_lengths_by_term']] <- opinion_lengths_by_term

      ggsave(opinion_lengths_by_term,
             filename = file.path(output_folder, 'opinion_lengths_by_term.png'),
             width = 10,
             height = 6,
             units = 'in',
             bg = 'white')

    }  # Opinion Length Comparison

    {

      total_opinion_lengths <- opinions_processed %>%
        mutate(word_count =  stri_count_words(opinion_text)) %>%
        select(authorship, docket, opinion_type, word_count) %>%
        left_join(master_file %>%
                    select(docket, short_hand), by = 'docket') %>%
        left_join(decisions %>%
                    select(Coalition, Docket, Date_Argued, Date_Decided) %>%
                    rename(coalition = Coalition,
                           docket = Docket,
                           date_argued = Date_Argued,
                           date_decided = Date_Decided), by = 'docket') %>%
        group_by(docket) %>%
        reframe(total_words = sum(word_count),
                total_opinions = n(),
                total_authors = list(authorship),
                short_hand = short_hand,
                coalition = coalition,
                date_decided = date_decided) %>%
        unique() %>%
        rename(case_name = short_hand) %>%
        rowwise() %>%
        mutate(total_authors = paste(unlist(total_authors), collapse = "; ")) %>%
        ungroup() %>%
        select(case_name, total_opinions, total_authors, coalition, total_words, date_decided) %>%
        arrange(desc(total_words))


      combined_list[['opinion_lengths']][['combined_opinion_lengths']] <- total_opinion_lengths

    } # Longest Opinions (Total)

    {

      all_opinion_lengths <- opinions_processed %>%
        mutate(word_count =  stri_count_words(opinion_text)) %>%
        select(authorship, docket, opinion_type, word_count) %>%
        left_join(master_file %>%
                    select(docket, short_hand), by = 'docket') %>%
        left_join(decisions %>%
                    select(Coalition, Docket, Date_Argued, Date_Decided) %>%
                    rename(coalition = Coalition,
                           docket = Docket,
                           date_argued = Date_Argued,
                           date_decided = Date_Decided), by = 'docket') %>%
        arrange(word_count)

      combined_list[['opinion_lengths']][['all_individual_opinion_lengths']] <- all_opinion_lengths


    } # Individual Opinion Lengths

  } # Opinion Lengths

  message('Completed Opinion Lengths')

  {

    combined_list[['precedent_unconstitutional']] <- list()


    {

    } # OT24 Declaration Unconstitutional/Precedent Alteration


    {

      precedent_unconstitutional <- scdb_cases_data %>%
        filter(term >= 1986) %>%
        group_by(term) %>%
        summarise(
          declarationUncon_count = sum(!declarationUncon == 1, na.rm = TRUE),
          precedentAlteration_count = sum(precedentAlteration == 1, na.rm = TRUE),
          total_cases = n())

      precedents_altered <- precedent_unconstitutional %>%
        mutate(chief = ifelse(term >= 2005, 'Roberts Court (2005-Present)', 'Rehnquist Court (1986-2004)'),
               chief = factor(chief, levels = c('Rehnquist Court (1986-2004)', 'Roberts Court (2005-Present)'))) %>%
        ggplot(aes(x = term, y = precedentAlteration_count)) +
        geom_col(aes(fill = chief), colour = 'black') +
        theme_minimal() +
        labs(x = '',
             y = '',
             title = 'Precedents Altered\n',
             fill = 'Chief Justice') +
        scale_y_continuous(breaks = seq(1, 6, 1), lim = c(0, 6)) +
        scale_x_continuous(breaks = seq(1986, 2024, 4)) +
        geom_vline(xintercept = 2004.5, linetype = 2, size = 1.2) +
        geom_hline(yintercept = 0) +
        scale_fill_manual(values = c('grey25', 'grey')) +
        theme(
          panel.border = element_rect(size = 1, colour = 'black', fill = NA),
          axis.text = element_text(size = 12, colour = 'black'),
          axis.title = element_text(size = 16, colour = 'black'),
          legend.text = element_text(size = 14, colour = 'black'),
          legend.position = 'top',
          legend.title = element_blank(),
          legend.box.background = element_rect(size = 1, colour = 'black', fill = NA),
          plot.title = element_text(size = 16, colour = 'black', hjust = 0.5, face = 'bold')
        )

      unconstitutional <- precedent_unconstitutional %>%
        mutate(chief = ifelse(term >= 2005, 'Roberts Court (2005-Present)', 'Rehnquist Court (1986-2004)'),
               chief = factor(chief, levels = c('Rehnquist Court (1986-2004)', 'Roberts Court (2005-Present)'))) %>%
        ggplot(aes(x = term, y = declarationUncon_count)) +
        geom_col(aes(fill = chief), colour = 'black') +
        theme_minimal() +
        labs(x = '',
             y = '',
             title = 'Federal, State, or Municipal Laws & Acts\nDeclared Unconstitutional\n',
             fill = 'Chief Justice') +
        scale_y_continuous(breaks = seq(4, 20, 4), lim = c(0, 20)) +
        scale_x_continuous(breaks = seq(1986, 2024, 4)) +
        geom_vline(xintercept = 2004.5, linetype = 2, size = 1.2) +
        geom_hline(yintercept = 0) +
        scale_fill_manual(values = c('grey25', 'grey')) +
        theme(
          panel.border = element_rect(size = 1, colour = 'black', fill = NA),
          axis.text = element_text(size = 12, colour = 'black'),
          axis.title = element_text(size = 16, colour = 'black'),
          legend.text = element_text(size = 14, colour = 'black'),
          legend.position = 'top',
          legend.title = element_blank(),
          legend.box.background = element_rect(size = 1, colour = 'black', fill = NA),
          plot.title = element_text(size = 16, colour = 'black', hjust = 0.5, face = 'bold')

        )

      total_cases <- precedent_unconstitutional %>%
        mutate(chief = ifelse(term >= 2005, 'Roberts Court (2005-Present)', 'Rehnquist Court (1986-2004)'),
               chief = factor(chief, levels = c('Rehnquist Court (1986-2004)', 'Roberts Court (2005-Present)'))) %>%
        ggplot(aes(x = term, y = total_cases)) +
        geom_col() +
        geom_col(aes(fill = chief), colour = 'black') +
        theme_minimal() +
        labs(x = '\nTerm',
             y = '',
             title = 'Total Cases Decided in Term\n',
             fill = 'Chief Justice') +
        scale_y_continuous(breaks = seq(50, 150, 50), lim = c(0, 165)) +
        scale_x_continuous(breaks = seq(1986, 2024, 4)) +
        geom_vline(xintercept = 2004.5, linetype = 2, size = 1.2) +
        geom_hline(yintercept = 0) +
        scale_fill_manual(values = c('grey25', 'grey')) +
        theme(
          panel.border = element_rect(size = 1, colour = 'black', fill = NA),
          axis.text = element_text(size = 14, colour = 'black'),
          axis.title = element_text(size = 16, colour = 'black'),
          legend.text = element_text(size = 14, colour = 'black'),
          legend.position = 'top',
          legend.title = element_blank(),
          legend.box.background = element_rect(size = 1, colour = 'black', fill = NA),
          plot.title = element_text(size = 16, colour = 'black', hjust = 0.5, face = 'bold')

        )



    } # OT1986-OT23

    combined_list[['precedent_unconstitutional']][['precedent_unconstitutional']] <- precedent_unconstitutional

    constitutional_precedents <- (precedents_altered + unconstitutional)/total_cases +
      plot_layout(guides = 'collect') &
      theme(legend.position = "bottom",
            legend.background = element_rect(size = 1, colour = 'black', fill = NA))

    ggsave(constitutional_precedents,
           filename = file.path(output_folder, 'constitutional_precedents.png'),
           width = 12,
           height = 8,
           units = 'in')

  } # Precedent Alteration/Declaration Unconstitutional

  return(combined_list)


}

decisions_path <- "C:/Users/jaketruscott/Github/scotuswatch/Stat Reviews/OT24_StatReview/decisions/data/OT_24_Decisions.csv"
decisions <- read.csv(decisions_path, as.is = T)
master_file = cases_master
oral_arguments <- get(load('data/term_level_combined_transcripts/scotus_OT24.rdata'))
output_folder = file.path('Stat Reviews', 'OT24_StatReview', 'scotusblog_replication', 'figures')
opinions_processed = file.path('Stat Reviews', 'OT24_StatReview', 'decisions', 'opinions', 'combined_opinions_processed', 'combined_opinions_OT2024.rdata')
older_opinions_processed <- "C:/Users/jaketruscott/Github/scotuswatch/data/decisions/earlier_decisions_processed.rdata"
scdb_cases_data = scdb_cases
scdb_justices_data = scdb_justices



export_scotusblog_stats <- function(scotusblog_stats_object,
                                    output_path = file.path('Stat Reviews', 'OT24_StatReview', 'scotusblog_replication', 'data')){

  for (i in 1:length(scotusblog_stats_object)){

    temp_topic <- scotusblog_stats_object[[i]]
    if ('list' %in% class(temp_topic)){
      temp_topic <- Filter(function(x) any(class(x) == "data.frame"), temp_topic)
    }

    temp_topic_name <- names(scotusblog_stats_object[i])

    {

      if (temp_topic_name == 'opinions'){
        temp_topic <- scotusblog_stats_object[[i]][['2024 Opinions by Justice']]
        temp_item <- temp_topic %>%
          purrr::imap_dfr(function(justice_data, justice_name) {
            purrr::imap_dfr(justice_data, function(opinion_data, opinion_type) {
              tibble(
                Justice = str_to_title(justice_name),
                Opinion_Type = opinion_type,
                Case = opinion_data
              )
            })
          })
        temp_wb <- createWorkbook()
        suppressWarnings(addWorksheet(temp_wb, as.character('opinions_by_justice')))
        suppressWarnings(writeData(temp_wb, as.character('opinions_by_justice'), temp_item))
        temp_export_path = file.path(output_path, paste0(as.character(temp_topic_name), '.xlsx')) # Temp Excel Output Path
        suppressWarnings(saveWorkbook(temp_wb, temp_export_path, overwrite = T))
        next
      }

    } # Special Run for 'opinions'

    if (length(temp_topic) == 0){
      next
    }

    temp_wb <- createWorkbook()

    for (item in 1:length(temp_topic)){

      temp_item <- temp_topic[[item]]
      temp_item_class <- class(temp_item)

      if (!'data.frame' %in% class(temp_item)){
        next
      } else {
        temp_item_name <- gsub('\\_by\\_', '', gsub(' ', '_', names(temp_topic[item])))
        temp_item_name <- gsub('percent', 'perc', temp_item_name)
        suppressWarnings(addWorksheet(temp_wb, as.character(temp_item_name)))
        suppressWarnings(suppressWarnings(writeData(temp_wb, as.character(temp_item_name), temp_item)))
      }

    } # For Each Dataframe -- Populate temp_wb

    temp_export_path = file.path(output_path, paste0(as.character(temp_topic_name), '.xlsx')) # Temp Excel Output Path
    suppressWarnings(saveWorkbook(temp_wb, temp_export_path, overwrite = T))

    message('Exported ', temp_topic_name)

  } # For Each Topic -- Export to Excel w/ Tabs by Area


} # Export SCOTUSBLOG Stats to Excel by Topic


c <- get(load(opinions_processed))







