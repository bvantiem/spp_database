# Set up ####
source("scripts/00_packages.R")
research_participants <- readRDS("data/processed/research_participants.Rds")

# Prepare Summary Statistics Table  ####
data <- research_participants %>% mutate(any_surveys = ifelse(nsurveys>0,1,0),
                                         any_surveys_pre = ifelse(nsurveys_pre>0,1,0),
                                         any_surveys_post = ifelse(nsurveys_post>0,1,0)) %>%
  filter(any_surveys==1)

data <- dummy_cols(data, select_columns = "marital_status_code")

prepare_summary_column <- function(data){
  # Note: numbering the rows because tidyr::gather puts variables in alphabetical order
  tab <- data %>%
    transmute(
      # Sentence Characteristics
      minimum_sentence = min_sent_days/365,
      maximum_sentence = max_sent_days/365,
      life = life,
      # Survey Vars
      no_surveys = nsurveys)

    tab <- data %>%
    transmute(
      # Sentence Characteristics, 1
      r01a_minimum_sentence = min_sent_days/365,
      r01b_maximum_sentence = max_sent_days/365,
      r01c_life = life,
      # Offense Characteristics, 4
      r02a_violent_offense = violent_offense,
      r02b_property_offense = property_offense,
      r02c_drugs_offense = drugs_offense,
      r02d_publicorder_offense = publicorder_offense,
      # Demographics, 8
      r03a_black = race_black,
      r03b_white = race_white,
      r03c_married = marital_status_code_MAR,
      r03d_high_school_degree = high_school,
      r03e_security_threat_group = STG,
      r03f_children = children,
      r03g_partner = partner,
      r03h_foreign_born = foreign_born,
      # Survey Vars, 16
      r04a_no_surveys = nsurveys,
      r04e_any_pre_treatment_surveys = any_surveys_pre,
      r04f_any_post_treatment_surveys = any_surveys_post)

  tab <- tab %>%
    tidyr::gather(variable, value) %>%
    # Summarize by variable
    group_by(variable) %>%
    # summarise all columns
    summarise(`Mean` = round(mean(value, na.rm=TRUE),3)) %>% # Check number of NA values after getting full data
    mutate(Mean = format(Mean, digits = 3))

  tab_2 <- data %>%
    select(
    r99a_participants = research_id) %>%
    tidyr::gather(variable, value) %>%
    # Summarize by variable
    group_by(variable) %>%
    # summarise all columns
    summarise(`Mean` = length(unique(value)))

  tab <- rbind(tab,
               tab_2)

  variable_names <- gsub("(....)(_)(.*)", "\\3", tab$variable)
  variable_names <- str_to_title(gsub("_", " ", variable_names))
  variable_names[which(variable_names=="Publicorder Offense")] <- "Public Order Offense"
  tab$variable <- variable_names
  names(tab) <- c("Variable", "Share/Mean")

  return(tab)
}

tab <- cbind(prepare_summary_column(data),
             prepare_summary_column(data %>% filter(treated == 1))[2],
             prepare_summary_column(data %>% filter(treated == 0))[2]) %>%
  setNames(c("Variable","All Respondents","Treated","Control"))

# Fix up table
tab$`All Respondents`[tab$Variable=="Any Pre Treatment Surveys"] <- NA
tab$`All Respondents`[tab$Variable=="Any Post Treatment Surveys"] <- NA

# Build Summary Statistics Table ####
# PDF
tab %>%
  kbl(caption = "Summary Statistics",
      align = c("lrrr")) %>%
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>%
  #kable_styling(font_size = 14) %>%
  add_header_above(c(" " = 1, "Subset" = 3)) %>%
  pack_rows("Sentence Characteristics",1,3,bold=T) %>%
  pack_rows("Offense Characteristics",4,7,bold=T) %>%
  pack_rows("Demographics",8,15,bold=T) %>%
  pack_rows("Survey Participation",16,18,bold=T) %>%
  row_spec(c(15,16),
           hline_after=TRUE,
           extra_css = "border-bottom: 1px solid") %>%
  save_kable(file = "output/tables/tabx_summary_statistics.pdf",
             self_contained = T,density = 200)

# Latex
tab %>%
  kbl(caption = "Summary Statistics",
      align = c("lrrr"),
      format = "latex",
      label = "summary_statistics",format.args = list(big.mark = ",")) %>%
  kable_styling(latex_options = "hold_position") %>%
  kable_styling(font_size = 7) %>%
  add_header_above(c(" " = 1, "Subset" = 3)) %>%
  pack_rows("Sentence Characteristics",1,3,bold=T) %>%
  pack_rows("Offense Characteristics",4,7,bold=T) %>%
  pack_rows("Demographics",8,15,bold=T) %>%
  pack_rows("Survey Participation",16,18,bold=T) %>%
  row_spec(c(15,16),
           hline_after=TRUE,
           extra_css = "border-bottom: 1px solid") %>%
  footnote(threeparttable = T,
           "The variables 'Any Pre/Post Treatment Surveys' are not individuals who were never treated.") %>%
  save_kable(file = "output/tables/tabx_summary_statistics.tex",
             self_contained = T,density = 200)

