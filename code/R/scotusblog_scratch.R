decisions_scotusblog <- function(input_path,
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

input_path = "C:/Users/jaketruscott/Github/scotuswatch/Stat Reviews/OT24_StatReview/decisions/data/OT_24_Decisions.csv"
output_path = "C:/Users/jaketruscott/Github/scotuswatch/Stat Reviews/OT24_StatReview/decisions"
output_type = 'html'
cases_break = 12
remove_existing_files = T
current_term = '2024'

{
  cases_master <- read.csv('Stat Reviews/OT24_StatReview/cases_master/cases_master_file_OT24.csv', as.is = T)
  term_index_master <- read.csv('Stat Reviews/OT24_StatReview/cases_master/term_index_master_template.csv', as.is = T, header = F)
} # Load Cases Master Files

{

  decisions <- read.csv(input_path, as.is = T)

  decisions <- decisions %>%
    relocate(Docket, .after = 'Case') %>%
    mutate(Author = stringr::str_to_title(Author))

  names(decisions)[1:8] <- tolower(names(decisions)[1:8])

  decisions_master_meta <- cases_master %>%
    left_join(decisions %>%
                select(-c(case, lower_court)), by = 'docket') %>%
    mutate(sitting = ifelse(sitting == '', 'No Argument', sitting),
           sitting = ifelse(sitting %in% c('April', 'May'), 'April-May', sitting),
           major_case = ifelse(major_case == 1, 1, 0)) %>%
    group_by(sitting) %>%
    mutate(sitting_count = n(),
           sitting_label = paste0(sitting, ' (', sitting_count, ')')) %>%
    ungroup() %>%
    relocate(sitting_label) %>%
    relocate(sitting)

} # Merge Decisions w/ Master Cases File


# Then pass it to kbl
temp_index[, 1:15] %>%
  kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c",
      col.names = rep("", 15)) %>%
  column_spec(c(1, 6, 11), width = '10cm', bold = TRUE, border_right = FALSE) %>%
  column_spec(setdiff(1:15, c(1, 6, 11, 5, 10, 15)), width = '3cm') %>%
  column_spec(c(5, 10, 15), width = '10cm') %>%
  row_spec(c(header_rows), bold = TRUE, color = 'white', background = '#080808', align = 'center', font_size = 16) %>%
  row_spec(nrow(temp_index), extra_css = "border-bottom: 2px solid;") %>%
  kable_styling(font_size = 16, bootstrap_options = c("striped", "hover"))

{

  {

    agg_decisions_table <- decisions_master_meta %>%
      select(sitting, sitting_label, short_hand, author, coalition, decision, lower_court) %>%
      mutate(coalition = ifelse(coalition == 'Per Curiam', 'PC', coalition),
             decision = case_when(
               .default = 'O',
               grepl('Affirm', decision) ~ 'A',
               grepl('(Reverse|Vacate)', decision) ~ 'R',
               decision == 'DIG' ~ 'DIG',
               decision == 'Granted' ~ 'G',
               is.na(decision) ~ NA),
             author = ifelse(author == 'Per Curiam', 'PC', author))

    meta <- list(
      c('Totals', ''),
      c('Cases Heard for Oral Argument', length(which(!agg_decisions_table$sitting == 'No Argument'))),
      c('Cases Summarily Decided (No Arg.)', length(which(agg_decisions_table$sitting == 'No Argument'))),
      c('Total Cases', nrow(agg_decisions_table)),
      c(' ', ''),
      c('Signed Opinions of the Court', length(which(!agg_decisions_table$author %in% c('PC', 'DIG')))),
      c('Unsigned (Per Curiam)', length(which(agg_decisions_table$author == 'PC'))),
      c('Cases Dismissed as Improvidently Granted', length(which(agg_decisions_table$decision == 'DIG')))
    )

    meta_matrix <- do.call(rbind, meta)
    meta_matrix <- matrix(unlist(meta), ncol = 2, byrow = TRUE)


    term_index_list <- list()

    unique_sittings <- c('October', 'November', 'December', 'January', 'February', 'March', 'April-May', 'No Argument')

    for (i in 1:length(unique_sittings)){

      temp_sitting_id <- unique_sittings[i]

      temp_sitting <- agg_decisions_table %>%
        filter(sitting == temp_sitting_id) # Collect Temp Sitting

      term_index_list[[as.character(temp_sitting_id)]] <- list(temp_sitting %>%
                                                                 select(short_hand, author, coalition, decision, lower_court) %>%
                                                                 rename(Case = short_hand,
                                                                        Author = author,
                                                                        Vote = coalition,
                                                                        Result = decision,
                                                                        'Court Below' = lower_court),
                                                               label = unique(temp_sitting$sitting_label))# Compile and Export to List

    }

  } # Compile Totals Meta & Sitting-Level Term Index Data (as list -- term_index_list)

  {

    sitting_indices <- c('October', 'November', 'December', 'January', 'February', 'March', 'April-May', 'No Argument')

    temp_index <- as.matrix(term_index_master)

    for (i in 1:length(sitting_indices)){
      idx <- which(temp_index == sitting_indices[i], arr.ind = TRUE)
      temp_label <- toupper(term_index_list[[as.character(sitting_indices[i])]]$label)
      temp_index[idx] <- temp_label
      current_idx_row <- idx[1] + 1
      current_idx_col <- idx[2]

      temp <- unname(as.matrix(term_index_list[[as.character(sitting_indices[i])]][[1]]))
      expand_idx_row <- current_idx_row + nrow(temp) - 1
      expand_idx_col <- current_idx_col + ncol(temp) - 1

      # Assign
      temp_index[current_idx_row:expand_idx_row, current_idx_col:expand_idx_col] <- temp
    }

    temp_index <- data.frame(temp_index) %>%
      mutate(across(everything(), ~ na_if(.x, "")))

    temp_index <- temp_index %>%
      mutate(all_na = if_all(everything(), is.na)) %>%
      mutate(next_not_all_na = lead(!all_na, default = FALSE)) %>%
      mutate(filler = as.integer(all_na & next_not_all_na)) %>%
      mutate(keep = case_when(
        .default = 0,
        filler == 1 ~ 1,
        !all_na ~ 1
      )) %>%
      filter(keep == 1) %>%
      mutate(header_row = ifelse(lag(filler) == 1, 1, 0),
             header_row = ifelse(is.na(header_row), 1, header_row)) %>%
      filter(filler == 0) %>%
      select(-c(all_na, next_not_all_na, filler,  keep)) %>%
      mutate(across(everything(), ~replace_na(.x, "")))

    header_rows <- which(temp_index$header_row == 1)

    {
      idx <- which(temp_index == 'Totals', arr.ind = TRUE)
      current_idx_row <- idx[1]
      current_idx_col <- idx[2]

      temp <- meta_matrix
      expand_idx_row <- current_idx_row + nrow(temp) - 1
      expand_idx_col <- current_idx_col + ncol(temp) - 1

      temp_index[current_idx_row:expand_idx_row, current_idx_col:expand_idx_col] <- temp

    } # Totals IDX

  } # Compile Temp Index Table

  # Apply cell_spec row-wise to column 11



  colnames(temp_index) <- as.character(temp_index[1, ])
  names(temp_index) <- gsub('\\..*', '', names(temp_index))
  # Remove the first row since it is now used as column names
  temp_index <- temp_index[-1, ]
  ft <- flextable(temp_index[, 1:15])

  ft %>%
    fontsize(size = 10, part = "all") %>%
    align(align = "center", part = "all") %>%
    border_outer(border = officer::fp_border(color = "black", width = 1)) %>%
    border_inner_h(border = officer::fp_border(color = "gray", width = 0.5))














} # SCOTUSBLOG - Aggregate Decisions Table
