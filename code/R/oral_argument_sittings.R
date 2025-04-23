################################################################################
# SCOTUSWatch - Oral Argument Sittings OT 2023
# Author: Jake S. Truscott, Ph.D
# Updated March 2024
################################################################################

###############################################################################
#Load Packages & Libraries
###############################################################################
library(kableExtra); library(dplyr);  library(tidyr); library(scotustext); library(htmltools); library(ggplot2); library(png); library(dplyr); library(stringi); library(stringr); library(ggplot2); library(ggthemes); library(anytime); library(tm); library(scotustext); library(readxl); library(ggpattern); library(png); library(ggtext); library(grid); library(wesanderson); library(tidyr); library(readxl)

custom_css <- "
<style>
  /* Make table columns automatically adjust on smaller screens */
  @media (max-width: 768px) {
    .kable-table td, .kable-table th {
      width: auto;
      font-size: smaller; /* Adjust font size for smaller screens if needed */
      padding: 4px; /* Adjust padding if needed */
    }
  }
  /* Add other styles as necessary */
</style>
"

{
  case <- c('Nancy Williams, et al. v. Fitzgerald Washington, AL Sec. of Labor', "Royal Canin U.S.A, Inc., et al. v. Anastasia Wullschleger, et al.", "Garland, Attorney Gen., et al. v. Jennifer VanDerStok, et al.", "Gerald F. Lackey, Comm. VA DMV v. Damian Stinnie, et al.", "Richard Eugene Glossip v. Oklahoma", "Medical Marijuana, Inc., et al. v. Douglas J. Horn", "Amina Bouarfa v. Alejandro Mayorkas, Sec. of Homeland Security, et al.", "Joshua Bufkin v. Denis R. McDonough, Sec. of Veterans Affairs", "City and County of San Francisco, CA v. EPA")
  docket <- c('23-191', '23-677', '23-852', '23-621',  '22-7466', '23-365', '23-583', '23-713', '23-753')
  argued <- c('10/07/24', '10/07/24', '10/08/24', '10/08/24', '10/09/24', '10/15/24', '10/15/24', '10/16/24', '10/16/24')
  petitioner <- c('Adam G. Unikowsky', 'Katherine B. Wellington', 'Elizabeth B. Prelogar', 'Erika L. Maley', 'Seth P. Waxman', 'Lisa S. Blatt', 'Samir Deger-Sen', 'Melanie L. Bostwick', 'Tara M. Steeley')
  respondent <- c('Edmund G. LaCour, Jr.', 'Ashley C. Keller', 'Peter A. Patterson', 'Brian D. Schmalzbach', 'Paul D. Clement', 'Easha Anand', 'Colleen R. Sinzdak', 'Sopan Joshi', 'Frederick Liu')
  amici <- c('', '', '', 'Anthony A. Yang', 'Christopher G. Michel', '', '', '', '')
  lower_court <- c('SCAL', 'CA8',  'CA5', 'CA4', 'CCAOK', 'CA2', 'CA11', 'CAFC', 'CA9')
  cert_briefs <- c(0, 1, 2, 1, 3, 1, 1, 4, 2)
  merits_briefs <- c(6, 6, 24, 14, 14, 7, 8, 5, 10)


  october_sitting <- data.frame(
    'Case' = case,
    'Docket' = docket,
    'Argued' = argued,
    'Petitioner Counsel' = petitioner,
    'Respondent Counsel' = respondent,
    'Arguing Amici' = amici,
    'Lower Court' = lower_court,
    'Cert Amici' = cert_briefs,
    'Merits Amici' = merits_briefs
  )
  names(october_sitting) <- gsub("\\.", " ", names(october_sitting))

  october_sitting_cases <- october_sitting %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    column_spec(1, bold = TRUE, border_right = TRUE, width = "6cm") %>%  # Set the width of the first column
    column_spec(2:8, width = "3cm") %>%  # Set the width of columns 2 to 8
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(october_sitting), 1), align = 'center') %>%
    kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed"))


  html_output <- as.character(october_sitting_cases)
  writeLines(html_output, 'stat_pack_OT24/Oral Arguments/October/october_sitting.txt')





} #October Sitting

{

  november_sitting <- read.csv('oral_argument_oyez/argument_sittings_csvs/november_sitting.csv', as.is = T)


  november_sitting <- data.frame(
    'Case' = november_sitting$Case,
    'Docket' = november_sitting$Docket,
    #'Argued' = november_sitting$,
    'Petitioner Counsel' = november_sitting$Petitioner,
    'Respondent Counsel' = november_sitting$Respondent,
    'Arguing Amici' = november_sitting$Amicus,
    'Lower Court' = november_sitting$Lower.Ct,
    'Cert Amici' = november_sitting$Cert.Amici,
    'Merits Amici' = november_sitting$Merits.Amici
  )
  names(november_sitting) <- gsub("\\.", " ", names(november_sitting))

  november_sitting_cases <- november_sitting %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    column_spec(1, bold = TRUE, border_right = TRUE, width = "6cm") %>%  # Set the width of the first column
    column_spec(2:8, width = "3cm") %>%  # Set the width of columns 2 to 8
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(november_sitting), 1), align = 'center') %>%
    kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed"))


  html_output <- as.character(november_sitting_cases)
  writeLines(html_output, 'stat_pack_OT24/Oral Arguments/November/November_sitting.txt')






} # November Sitting

{

  december_sitting <- read.csv('oral_argument_oyez/argument_sittings_csvs/december_sitting.csv', as.is = T)


  december_sitting <- data.frame(
    'Case' = december_sitting$Case,
    'Docket' = december_sitting$Docket,
    'Argued' = december_sitting$Date,
    'Petitioner Counsel' = december_sitting$Petitioner,
    'Respondent Counsel' = december_sitting$Respondent,
    'Arguing Amici' = december_sitting$Amicus,
    'Lower Court' = december_sitting$Lower.Ct
  )
  names(december_sitting) <- gsub("\\.", " ", names(december_sitting))

  december_sitting_cases <- december_sitting %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    column_spec(1, bold = TRUE, border_right = TRUE, width = "6cm") %>%  # Set the width of the first column
    column_spec(2:7, width = "3cm") %>%  # Set the width of columns 2 to 8
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(december_sitting), 1), align = 'center') %>%
    kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed"))


  html_output <- as.character(december_sitting_cases)
  writeLines(html_output, 'stat_pack_OT24/Oral Arguments/December/december_sitting.txt')






} # december Sitting

{

  january_sitting <- read.csv('oral_argument_oyez/argument_sittings_csvs/january_sitting.csv', as.is = T)


  january_sitting <- data.frame(
    'Case' = january_sitting[,2],
    'Docket' = january_sitting[,1],
    'Petitioner Counsel' = january_sitting[,4],
    'Respondent Counsel' = january_sitting[,5],
    'Arguing Amici' = january_sitting[,6],
    'Lower Court' = january_sitting[,3],
    'Cert Amici' = january_sitting[,7],
    'Merits Amici' = january_sitting[,8]
  )
  names(january_sitting) <- gsub("\\.", " ", names(january_sitting))

  january_sitting_cases <- january_sitting %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    column_spec(1, bold = TRUE, border_right = TRUE, width = "6cm") %>%  # Set the width of the first column
    column_spec(2:8, width = "3cm") %>%  # Set the width of columns 2 to 8
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(january_sitting), 1), align = 'center') %>%
    kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed"))


  html_output <- as.character(january_sitting_cases)
  writeLines(html_output, 'stat_pack_OT24/Oral Arguments/january/january_sitting.txt')


} # January Sitting

{

  february_sitting <- read.csv('oral_argument_oyez/argument_sittings_csvs/february_sitting.csv', as.is = T)


  february_sitting <- data.frame(
    'Case' = february_sitting[,2],
    'Docket' = february_sitting[,1],
    'Petitioner Counsel' = february_sitting[,4],
    'Respondent Counsel' = february_sitting[,5],
    'Arguing Amici' = february_sitting[,6],
    'Lower Court' = february_sitting[,3],
    'Cert Amici' = february_sitting[,7],
    'Merits Amici' = february_sitting[,8]
  )
  names(february_sitting) <- gsub("\\.", " ", names(february_sitting))

  february_sitting_cases <- february_sitting %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    column_spec(1, bold = TRUE, border_right = TRUE, width = "6cm") %>%  # Set the width of the first column
    column_spec(2:8, width = "3cm") %>%  # Set the width of columns 2 to 8
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(february_sitting), 1), align = 'center') %>%
    kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed"))


  html_output <- as.character(february_sitting_cases)
  writeLines(html_output, 'stat_pack_OT24/Oral Arguments/february/february_sitting.txt')


} # Feb Sitting

{

  march_sitting <- read.csv('oral_argument_oyez/argument_sittings_csvs/march_sitting.csv', as.is = T)


  march_sitting <- data.frame(
    'Case' = march_sitting[,2],
    'Docket' = march_sitting[,1],
    'Petitioner Counsel' = march_sitting[,4],
    'Respondent Counsel' = march_sitting[,5],
    'Arguing Amici' = march_sitting[,6],
    'Lower Court' = march_sitting[,3],
    'Cert Amici' = march_sitting[,7],
    'Merits Amici' = march_sitting[,8]
  )
  names(march_sitting) <- gsub("\\.", " ", names(march_sitting))

  march_sitting_cases <- march_sitting %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    column_spec(1, bold = TRUE, border_right = TRUE, width = "6cm") %>%  # Set the width of the first column
    column_spec(2:8, width = "3cm") %>%  # Set the width of columns 2 to 8
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(march_sitting), 1), align = 'center') %>%
    kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed"))


  html_output <- as.character(march_sitting_cases)
  writeLines(html_output, 'stat_pack_OT24/Oral Arguments/March/march_sitting.txt')


} # Feb Sitting






###############################################################################
###############################################################################
#Old Terms
###############################################################################
###############################################################################

###############################################################################
# Tables by Sitting
###############################################################################

{
  case <- c("Pulsifer v. U.S. ", "CFPB v. Community Financial Services", "Acheson Hotels v. Laufer", "Murray v. UBS Securities", "Great Lakes Insurance v. Raiders Resort Realty", "Alexander v. South Carolina State Conference of the NAACP")
  argued <- c("10/02/2023", "10/03/2023", "10/04/2023", "10/10/2023", "10/10/2023", "10/11/2023")
  petitioner <- c("Shay Dvoretzky", "Elizabeth Prelogar", "Adam Unikowsky", "Easha Anand", "Jeffrey Wall", "John Gore")
  respondent <- c("Frederick Liu", "Noel Francisco", "Kelsi Corkran", "Eugene Scalia", "Howard Bashman", "Leah Aden")
  amici <- c("", "", "Erica Ross", "Anthony Yang", "", "Caroline Flynn")
  lower_court <- c("CA8", "CA5", "CA1", "CA2", "CA3", "Dist S.C.")
  cert_amici <- c("", 2, 5, 2, "", 2)
  merits_amici <- c(4, 29, 15, 12, 7, 14)

  october_sitting <- data.frame(
    'Case' = case,
    'Argued' = argued,
    'Petitioner Counsel' = petitioner,
    'Respondent Counsel' = respondent,
    'Arguing Amici' = amici,
    'Lower Court' = lower_court,
    'Cert-Stage Amici' = cert_amici,
    'Merits-Stage Amici' = merits_amici
  )
  names(october_sitting) <- gsub("\\.", " ", names(october_sitting))

  october_sitting_cases <- october_sitting %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    column_spec(1, bold = TRUE, border_right = TRUE, width = "6cm") %>%  # Set the width of the first column
    column_spec(2:8, width = "4cm") %>%  # Set the width of columns 2 to 8
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(october_sitting), 1), align = 'center') %>%
    kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed"))



  save_kable(october_sitting_cases, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/october_sitting_cases.html")

  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/october_sitting_cases.html"
  #webshot::install_phantomjs(force = T)
  phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/october_sitting_cases.png", vwidth = 900, vheight = 70)




} #October Sitting

{
  case <- c("Culley v. Marshall", "O'Connor Ratcliff v. Garnier", "Lindke v. Freed", "Vidal v. Elster", "Dept. of Agric. Rural Div. v. Kirtz", "U.S. v. Rahimi", "Rudisill v. McDonough")
  argued <- c("10/30/2023", "10/31/2023", "10/31/2023", "11/01/2023", "11/06/2023", "11/07/2023", "11/08/2023")
  petitioner <- c("Shay Dvoretzky", "Hashim Moonpan", "Allon Kedem", "Malcolm Stewart", "Benjamin Snyder", "Elizabeth Prelogar", "Misha Tseytlin")
  respondent <- c("Edmund LaCour, Jr.", "Pamela Karlan", "Victoria Ferres", "Jonathan Taylor", "Nandan Joshi", "J. Matthew Wright", "Vivek Suri")
  amici <- c("Nicole Reaves", "Sopan Joshi", "Masha Hansford", "", "", "", "")
  lower_court <- c("CA11", "CA9", "CA6", "CAFed", "CA3", "CA5", "CAFed")
  cert_amici <- c(3, 0, 0, 1, 0, 7, 4)
  merits_amici <-  c(11, 14, 11, 7, 2, 60, 11)

  november_sitting <- data.frame(
    'Case' = case,
    'Argued' = argued,
    'Petitioner Counsel' = petitioner,
    'Respondent Counsel' = respondent,
    'Arguing Amici' = amici,
    'Lower Court' = lower_court,
    'Cert-Stage Amici' = cert_amici,
    'Merits-Stage Amici' = merits_amici
  )
  names(november_sitting) <- gsub("\\.", " ", names(november_sitting))

  november_sitting_cases <- november_sitting %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    column_spec(1, bold = TRUE, border_right = TRUE, width = "6cm") %>%  # Set the width of the first column
    column_spec(2:8, width = "4cm") %>%  # Set the width of columns 2 to 8
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(november_sitting), 1), align = 'center') %>%
    kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed"))

  november_sitting_cases

  save_kable(november_sitting_cases, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/november_sitting_cases.html")

  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/november_sitting_cases.html"
  #webshot::install_phantomjs(force = T)
  phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/november_sitting_cases.png", vwidth = 900, vheight = 70)
} #November Sitting

{
  case <- c("Brown v. United States &\n Jackson v. United States", 'McElrath v. Georgia', "Wilkinson v. Garland, Att'y Gen.", 'SEC v. Jarkesy', 'Harrington v. Purdue Pharma L.P.', 'Moore v. United States', 'Muldrow v. St. Louis')
  argued <- c('11/27/2023', '11/28/2023', '11/28/2023', '11/29/2023', '12/04/2023', '12/05/2023', '12/06/2023')
  petitioner <- c('Jeffrey T. Green \n(For Brown) & \n Andrew Lee Adler \n(For Jackson)', 'Richard A. Simpson', 'Jamie A. Santos', 'Brian H. Fletcher', 'Curtis E. Gannon', 'Andrew M. Grossman', 'Brian Wolfman')
  respondent <- c('Austin Raynor', 'Stephen J. Petrany', 'Colleen R. Sinzdak', 'S. Michael McColloch', 'Gregory G. Garre (For Purdue Pharma L.P) & Pratik A. Shah \n(For Unsecured Creditors', 'Elizabeth B. Prelogar', 'Robert M. Loeb')
  amici <- c("", "", "", "", "", "", 'Aimee W. Brown')
  lower_court <- c("CA3 \n& CA11", "SCGA", "CA3", "CA5", "CA2", "CA9", "CA8")
  cert_amici <- c(1, 1, 0, 0, 0, 8, 3)
  merits_amici <-  c(4, 4, 3, 38, 26, 43, 12)

  december_sitting <- data.frame(
    'Case' = case,
    'Argued' = argued,
    'Petitioner Counsel' = petitioner,
    'Respondent Counsel' = respondent,
    'Arguing Amici' = amici,
    'Lower Court' = lower_court,
    'Cert-Stage Amici' = cert_amici,
    'Merits-Stage Amici' = merits_amici
  )
  names(december_sitting) <- gsub("\\.", " ", names(december_sitting))

  december_sitting_cases <- december_sitting %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    column_spec(1, bold = TRUE, border_right = TRUE, width = "6cm") %>%  # Set the width of the first column
    column_spec(2, width = "4cm") %>%  # Set the width of columns 2 to 8
    column_spec(3:4, width = "6cm") %>%  # Set the width of the first column
    column_spec(5:8, width = "4cm") %>%  # Set the width of columns 2 to 8
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(december_sitting), 1), align = 'center') %>%
    kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed"))

  december_sitting_cases

  save_kable(december_sitting_cases, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/december_sitting_cases.html")

  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/december_sitting_cases.html"
  #webshot::install_phantomjs(force = T)
  phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/december_sitting_cases.png", vwidth = 900, vheight = 70)
} #December Sitting

{
  case <- c('CAMPOS-CHAVES V. GARLAND, ATT’Y GEN &\n GARLAND, ATT’Y. GEN. V. SINGH\n(Consolidated)', 'FBI V. FIKRE', 'SHEETZ V. COUNTY OF EL DORADO', 'UNITED STATES TRUSTEE V. JOHN Q. HAMMONS FALL 2006, LLC', 'SMITH V. ARIZONA', 'MACQUAIRE INFRASTRUCTURE CORP. V. MOAB PARTNERS, L.P.', 'DEVILLIER V. TEXAS', 'RELENTLESS, INC. V. DEPT. OF COMMERCE', 'LOPER BRIGHT ENTERPRISES, INC. RAIMONDO, SEC. OF COMMERCE')
  argued <- c('01/08/2024', '01/08/2024', '01/09/2024', '01/09/2024', '01/10/2024', '01/16/2024', '01/16/2024', '01/17/2024', '01/17/2024')
  petitioner <- c('Easha Anand', 'Sopan Joshi', 'Paul J. Beard II', 'Masha G. Hansford', 'Hari Santhanam', 'Linda T. Coberly', 'Robert J. McNamara', 'Roman Martinez', 'Paul D. Clement')
  respondent <- c('Charles L. McCloud', 'Gadeir Abbas', 'Aileen M. McGrath', 'Daniel L. Geyser', 'Alexander W. Samuels', 'David C. Frederick', 'Aaron L. Nielson', 'Elizabeth B. Prelogar', 'Elizabeth B. Prelogar')
  amici <- c("", "", 'Erica L. Ross', '', 'Eric J. Feigin', 'Ephraim McDowell', 'Edwin S. Kneedler', '', '')
  lower_court <- c('CA5', 'CA9', 'Cal. Ct. App.', 'CA10', 'Ariz. Ct. App.', 'CA2', 'CA5', 'CA1', 'CADC')
  cert_amici <- c(0, 0, 5, 0, 0, 2, 2, 2, 8)
  merits_amici <-  c(5, 10, 23, 6, 10, 9, 12, 11, 62)

  january_sitting <- data.frame(
    'Case' = case,
    'Argued' = argued,
    'Petitioner Counsel' = petitioner,
    'Respondent Counsel' = respondent,
    'Arguing Amici' = amici,
    'Lower Court' = lower_court,
    'Cert-Stage Amici' = cert_amici,
    'Merits-Stage Amici' = merits_amici
  )
  names(january_sitting) <- gsub("\\.", " ", names(january_sitting))

  january_sitting_cases <- january_sitting %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    column_spec(1, bold = TRUE, border_right = TRUE, width = "6cm") %>%  # Set the width of the first column
    column_spec(2, width = "4cm") %>%  # Set the width of columns 2 to 8
    column_spec(3:4, width = "6cm") %>%  # Set the width of the first column
    column_spec(5:8, width = "4cm") %>%  # Set the width of columns 2 to 8
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(january_sitting), 1), align = 'center') %>%
    kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed"))

  january_sitting_cases

  save_kable(january_sitting_cases, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/january_sitting_cases.html")

  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/january_sitting_cases.html"
  #webshot::install_phantomjs(force = T)
  phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/january_sitting_cases.png", vwidth = 900, vheight = 70)
} #January Sitting

{
  case <- c('TRUMP V. ANDERSON', 'CORNER POST, INC. V. BD. OF GOVERNORS, FRS', 'BISSONNETTE V. LEPAGE BAKERIES PARK ST., LLC', 'OHIO V. EPA | KINDER MORGAN, INC. V. EP | AMERICAN FOREST & PAPER ASSN. V. EPA | U.S. STEEL CORP. V. EPA (Consolidated)', 'WARNER CHAPPELL MUSIC, INC. V. NEALY', 'MOODY V. NETCHOICE (Linked w/ Paxton)', 'NETCHOICE V. PAXTON', 'McINTOSH V. UNITED STATES', 'CANTERO V. BANK OF AMERICA, N.A.', "GARLAND ATT'Y GEN. V. CARGILL", 'COINBASE, INC. V. SUSKI')
  argued <- c('02/09/2024', '02/20/2024', '02/20/2024', '02/21/2024', '02/21/2024', '02/26/2024', '02/26/2024', '02/27/2024', '02/27/2024', '02/28/2024', '02/28/2024')
  petitioner <- c('Jonathan F. Mitchell', 'Bryan Weir', 'Jennifer Bennett', 'Mathura Sridharan (State App.) & Catherine Stetson (Industry App.)', 'Kannon Shanmugam', 'Henry Whitaker', 'Paul Clement', 'Steven Yurowitz', 'Jonathan Taylor', 'Brian Fletcher', 'Jessica Ellsworth')
  respondent <- c('Jason C. Murray (Anderson) & Shannon W. Stevenson (Griswold)', 'Benjamin Snyder', 'Traci Lovett', 'Malcolm Stewart (Federal Resp.) & Judith Vale (State Resp.)', 'Joe Wesley', 'Paul Clement', 'Aaron Nielson', 'Mattherw Guarnieri', 'Lisa Blatt', 'Jonathan Mitchell', 'David Harris')
  amici <- c('', '', '', '', 'Yaira Dubin', 'Elizabeth Prelogar', 'Elizabeth Prelogar', '', 'Malcolm Stewart', '', '')
  lower_court <- c('SCCO', 'CA8', 'CA2', 'CADC', 'CA11', 'CA11', 'CA5', 'CA2', 'CA2', 'CA5', 'CA9')
  cert_amici <- c(1, 3, 0, 1, 3, 7, 1, 0, 1, 1, 0)
  merits_amici <-  c(76, 12, 14, 0, 12, 76, 80, 2, 13, 20, 6)

  february_sitting <- data.frame(
    'Case' = case,
    'Argued' = argued,
    'Petitioner Counsel' = petitioner,
    'Respondent Counsel' = respondent,
    'Arguing Amici' = amici,
    'Lower Court' = lower_court,
    'Cert-Stage Amici' = cert_amici,
    'Merits-Stage Amici' = merits_amici
  )
  names(february_sitting) <- gsub("\\.", " ", names(february_sitting))

  february_sitting_cases <- february_sitting %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    column_spec(1, bold = TRUE, border_right = TRUE, width = "6cm") %>%  # Set the width of the first column
    column_spec(2, width = "4cm") %>%  # Set the width of columns 2 to 8
    column_spec(3:4, width = "6cm") %>%  # Set the width of the first column
    column_spec(5:8, width = "4cm") %>%  # Set the width of columns 2 to 8
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(february_sitting), 1), align = 'center') %>%
    kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed"))

  february_sitting_cases

  save_kable(february_sitting_cases, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/february_sitting_cases.html")

  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/february_sitting_cases.html"
  #webshot::install_phantomjs(force = T)
  phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/february_sitting_cases.png", vwidth = 900, vheight = 70)
} #February Sitting

{
  case <- c('MURTHY v. MISSOURI', 'NRA v. VULLO', 'GUADALUPE DIAZ v. UNITED STATES', 'TRUCK INSURANCE EXCHANGE v. KAISER GYPSUM CO.', 'GONZALEZ v. TREVINO', 'TEXAS v. NEW MEXICO & COLORADO', 'BECERRA v. SAN CARLOS APACHE TRIBE (Con. w/ 23-253)', 'HARROW v. DOD', 'FDA v. ALLIANCE FOR HIPPOCRATIC MEDICINE (Con. w/ 23-236)', 'ERLINGER v. UNITED STATES', 'THOMAS A. CONNELLY v. UNITED STATES')
  argued <- c('03/18/2024', '03/18/2024', '03/19/2024', '03/19/2024', '03/20/2024', '03/20/2024', '03/25/2024', '03/25/2024', '03/26/2024', '03/27/2024', '03/27/2024' )
  petitioner <- c('Brian Fletcher', 'David Cole', 'Jeffrey Fisher', 'Allyson Ho', 'Anya Bidwell', 'Frederick Lui', 'Carolyn Flynn', 'Joshua Davis', 'Elizabeth Prelogar (Federal) & Jessica Ellsworth (Danco)', 'Jeffrey Fisher', 'Kannon Shanmugam')
  respondent <- c('J. Benjamin Aguinaga', 'Neal Katyal', 'Matthew Guarnieri', 'C. Kevin Marshall (Debtor) & David Frederick (Claimant)', 'Lisa Blatt', 'Lanora Pettit (Texas) & Jeffrey Wechsler (New Mexico)', 'Adam Unikowsky (23-253) & Lloyd Miller (23-250)', 'Aimee w. Brown', 'Erin Hawley', 'Eric Feigin (Resp. Supporting Petitioner)', 'Yaira Dubin')
  amici <- c('', 'Ephraim McDowell', '', 'Anthony Yang', 'Nicole Reaves', '', 'Lloyd Miller (23-250)', '', '', 'D. Nick Harper', '')
  lower_court <- c('CA5', 'CA2', 'CA9', 'CA4', 'CA5', 'Orig.', 'CA9 & CA10', 'CAFC', 'CA5', 'CA7', 'CA8')
  cert_amici <- c(5,7,0,1,7,'NA', 0,0,14,0,0)
  merits_amici <-  c(44,32,3,7,16,7,4,5,74,3,3)

  March_sitting <- data.frame(
    'Case' = case,
    'Argued' = argued,
    'Petitioner Counsel' = petitioner,
    'Respondent Counsel' = respondent,
    'Arguing Amici' = amici,
    'Lower Court' = lower_court,
    'Cert-Stage Amici' = cert_amici,
    'Merits-Stage Amici' = merits_amici
  )
  names(March_sitting) <- gsub("\\.", " ", names(March_sitting))

  March_sitting_cases <- March_sitting %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    column_spec(1, bold = TRUE, border_right = TRUE, width = "6cm") %>%  # Set the width of the first column
    column_spec(2, width = "4cm") %>%  # Set the width of columns 2 to 8
    column_spec(3:4, width = "6cm") %>%  # Set the width of the first column
    column_spec(5:8, width = "4cm") %>%  # Set the width of columns 2 to 8
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(March_sitting), 1), align = 'center') %>%
    kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed"))

  March_sitting_cases

  save_kable(March_sitting_cases, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/march_sitting_cases.html")

  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/march_sitting_cases.html"
  #webshot::install_phantomjs(force = T)
  phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/march_sitting_cases.png", vwidth = 1000, vheight = 70)
} #March Sitting

{
  case <- c('Snyder v. United States', 'Chiaverini v. City of Napoleon, Ohio', 'Fischer v. United States', 'Thornell v. Jones', 'City of Grants Pass, Oregon v. Johnson', 'Smith v. Spizzirri', 'Department of State v. Munoz', 'Starbucks Corp. v. McKinney', 'Moyle v. United States', 'Trump v. United States')
  argued <- c('04/15/2024', '04/15/2024', '04/16/2024', '04/17/2024', '04/22/2024', '04/22/2024', '04/23/2024', '04/23/2024', '04/24/2024', '04/25/2024')
  petitioner <- c('Lisa S. Blatt', 'Easha Anand', 'Jeffrey T. Green', 'Jason D. Lewis', 'Thomas D. Evangelis', 'Daniel L. Geyser', 'Curtis E. Gannon', 'Lisa S. Blatt', 'Joshua N. Turner', 'D. John Sauer')
  respondent <- c('Colleen R. Sindzak', 'Vivek Suri', 'Elizabeth B. Prelogar', 'Jean-Claude Andre', 'Edwin S. Kneedler', 'E. Joshua Rosenkranz', 'Eric T. Lee', 'Austin Raynor', 'Elizabeth B. Prelogar', 'Michael R. Dreeben')
  amici <- c('', 'Megan M. Wold', '', '', 'Kelsi B. Corkran', '', '', '', '', '')
  lower_court <- c('CA7', 'CA6', 'CADC', 'CA9', 'CA9', 'CA9', 'CA9', 'CA6', 'CA9', 'CADC')
  cert_amici <- c(2, 1, 1, 0, 25, 0, 0, 3, 0, 8)
  merits_amici <-  c(7, 8, 11, 4, 84, 3, 16, 13, 45, 36)

  April_sitting <- data.frame(
    'Case' = case,
    'Argued' = argued,
    'Petitioner Counsel' = petitioner,
    'Respondent Counsel' = respondent,
    'Arguing Amici' = amici,
    'Lower Court' = lower_court,
    'Cert-Stage Amici' = cert_amici,
    'Merits-Stage Amici' = merits_amici
  )
  names(April_sitting) <- gsub("\\.", " ", names(April_sitting))

  April_sitting_cases <- April_sitting %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    column_spec(1, bold = TRUE, border_right = TRUE, width = "6cm") %>%  # Set the width of the first column
    column_spec(2, width = "4cm") %>%  # Set the width of columns 2 to 8
    column_spec(3:4, width = "6cm") %>%  # Set the width of the first column
    column_spec(5:8, width = "4cm") %>%  # Set the width of columns 2 to 8
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(April_sitting), 1), align = 'center') %>%
    kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed"))

  April_sitting_cases

  save_kable(April_sitting_cases, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/april_sitting_cases.html")

  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/april_sitting_cases.html"
  #webshot::install_phantomjs(force = T)
  phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/april_sitting_cases.png", vwidth = 1000, vheight = 70)
} #April Sitting


combined_calendar <- october_sitting %>%
  mutate(Sitting = 'October',
        `Cert Stage Amici` = as.numeric(`Cert Stage Amici`)) %>%
  bind_rows(november_sitting %>%
              mutate(Sitting = 'November',
                    `Cert Stage Amici` = as.numeric(`Cert Stage Amici`))) %>%
  bind_rows(december_sitting %>%
              mutate(Sitting = 'December',
                     `Cert Stage Amici` = as.numeric(`Cert Stage Amici`))) %>%
  bind_rows(january_sitting %>%
              mutate(Sitting = 'January',
                     `Cert Stage Amici` = as.numeric(`Cert Stage Amici`))) %>%
  bind_rows(february_sitting %>%
              mutate(Sitting = 'February',
                     `Cert Stage Amici` = as.numeric(`Cert Stage Amici`))) %>%
  bind_rows(March_sitting %>%
              mutate(Sitting = 'March',
                    `Cert Stage Amici` = as.numeric(`Cert Stage Amici`))) %>%
  bind_rows(April_sitting %>%
              mutate(Sitting = 'April',
                     `Cert Stage Amici` = as.numeric(`Cert Stage Amici`))) %>%
  mutate(`Cert Stage Amici` = ifelse(is.na(`Cert Stage Amici`), 0, `Cert Stage Amici`))

write.csv(combined_calendar, file = 'stat_pack_OT23/Statpack Replication Data/Oral Arguments/Calendar - EmpiricalSCOTUS/combined_sittings_calendar.csv', row.names = F)
