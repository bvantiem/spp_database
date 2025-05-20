# Set up ####
source("scripts/00_packages.R")
source("scripts/0_utils.R")
pcq_mf <- readRDS("data/processed/processing_layer_2/pcq2_missforest.rds")

data <- pcq_mf
# Prepare Summary Statistics Table  ####
prepare_summary_column <- function(data){
  # Note: numbering the rows because tidyr::gather puts variables in alphabetical order
  tab <- data %>%
    transmute(
      # Actsat
      r01a_q55 = q55_dummy,
      r01b_q56 = q56_dummy,
      r01c_q57 = q57_dummy,
      r01d_q58 = q58_dummy,
      r01e_q59 = q59_dummy,
      r01f_q60 = q60_dummy,
      r01g_q61 = q61_dummy,
      # Visits
      r02a_q133 = q133_dummy,
      r02b_q134 = q134_dummy,
      r02c_q138 = q138_dummy,
      # care
      r03a_q115 = q115_dummy,
      r03b_q116 = q116_dummy,
      r03c_q117 = q117_dummy,
      r03d_q118 = q118_dummy,
      # safety
      r04a_q43 = q43_dummy,
      r04a_q44 = q44_dummy,
      r04a_q45 = q45_dummy,
      r04a_q46 = q46_dummy,
      r04a_q47 = q47_dummy,
      # shop
      r05a_q107 = q107_dummy,
      r05b_q108 = q108_dummy,
      # discrimination
      r06a_q147 = q147_dummy,
      r07a_q148 = q148_dummy)

  tab <- tab %>%
    tidyr::gather(variable, value) %>%
    # Summarize by variable
    group_by(variable) %>%
    # summarise all columns
    summarise(`Mean` = round(mean(value, na.rm=TRUE),3)) %>%
    mutate(Mean = format(Mean, digits = 3))

  variable_names <- gsub("(....)(_)(.*)", "\\3", tab$variable)
  variable_names <- gsub("_", " ", variable_names)
  # pcq_lookup[pcq_lookup$question_qno %in% variable_names,c("question_pa_2022a", "answer_scale_pa" )]
  i <- match(variable_names, pcq_lookup$question_qno)
  variable_names <- pcq_lookup[i,c("question_pa_2022a")]
  variable_names <- gsub("In the past month, (how often )?", "", variable_names)
  variable_names <- gsub("(did )?(have )?you (been )?", "", variable_names)
  variable_names <- gsub("I have been ", "", variable_names)
  variable_names <- gsub("on this unit", "on this unit?", variable_names)
  tab$variable <- variable_names
  names(tab) <- c("Variable", "Share/Mean")

  return(tab)
}

tab <- cbind(prepare_summary_column(data),
             prepare_summary_column(data %>% filter(treated == 1))[2],
             prepare_summary_column(data %>% filter(treated == 0))[2]) %>%
  setNames(c("Variable","All Respondents","Treated","Control"))

# Build Summary Statistics Table ####
# PDF
tab %>%
  kbl(caption = "Summary Statistics",
      align = c("lrrr")) %>%
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>%
  #kable_styling(font_size = 14) %>%
  add_header_above(c(" " = 1, "Subset" = 3)) %>%
  pack_rows("Meaningful Activity",1,7,bold=T) %>%
  pack_rows("Contact with the Outside World",8,10,bold=T) %>%
  pack_rows("Health Care",11,14,bold=T) %>%
  pack_rows("Safety",15,19,bold=T) %>%
  pack_rows("Food",20,21,bold=T) %>%
  pack_rows("Discrimination",22,23,bold=T) %>%
  save_kable(file = "output/tables/tabx_summary_experiences.pdf",
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
  pack_rows("Meaningful Activity",1,7,bold=T) %>%
  pack_rows("Contact with the Outside World",8,10,bold=T) %>%
  pack_rows("Health Care",11,14,bold=T) %>%
  pack_rows("Safety",15,19,bold=T) %>%
  pack_rows("Food",20,21,bold=T) %>%
  pack_rows("Discrimination",22,23,bold=T) %>%
  footnote(threeparttable = T,
           "Footnote text") %>%
  save_kable(file = "output/tables/tabx_summary_experiencess.tex",
             self_contained = T,density = 200)

