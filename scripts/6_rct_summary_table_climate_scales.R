# Set up ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")
pcq_mf <- readRDS("data/processed/processing_layer_2/pcq2_missforest.rds")

data <- pcq_mf
# Prepare Summary Statistics Table  ####
prepare_summary_column <- function(data){
  # Note: numbering the rows because tidyr::gather puts variables in alphabetical order
  tab <- data %>%
    transmute(
      # Prisoners
      r01_q10 = q10, # 1
      r01_q11 = q11,
      r01_q12 = q12,
      r01_q13 = q13,
      r01_q14 = q14,
      # Staff and procedure
      r02_q15 = q15, # 6
      r02_q16 = q16,
      r02_q17 = q17,
      r02_q18 = q18,
      r02_q19 = q19,
      r02_q20 = q20,
      r02_q21 = q21,
      r02_q22 = q22,
      # safety
      r03_q26 = q26, # 14
      r03_q29 = q29,
      r03_q30 = q30,
      r03_q31 = q32,
      r03_q32 = q32,
      # visits
      r04_q139 = q139, # 19
      r04_q140 = q140,
      r04_q141 = q141,
      r04_q142 = q142,
      r04_q143 = q143,
      r04_q144 = q144,
      # contact
      r05_q135 = q135, #25
      r05_q136 = q136,
      r05_q137 = q137,
      # sleep
      r06_q94 = q94, #28
      r06_q95 = q95,
      r06_q96 = q96,
      # care
      r07_q111 = q111, #31
      r07_q112 = q112,
      r07_q119 = q119,
      r07_q120 = q120,
      r07_q121 = q121,
      r07_q122 = q122,
      # shop
      r08_q99 = q99, #37
      r08_q100 = q100,
      r08_q101 = q101,
      # complaints
      r09_q24 = q24, #40
      r09_q25 = q25,
      # actsat
      r10_q62 = q62, #42
      r10_q63 = q63,
      r10_q64 = q64,
      r10_q65 = q65,
      r10_q66 = q66,
      r10_q67 = q67,
      r10_q68 = q68,
      # actav
      r11_q69 = q69, #49
      r11_q70 = q70,
      r11_q71 = q71,
      r11_q72 = q72,
      # reint
      r12_q73 = q73, # 53
      r12_q74 = q74,
      r12_q75 = q75,
      r12_q76 = q76,
      # autonomy
      r13_q82 = q82, #57
      r13_q83 = q82,
      r13_q84 = q83,
      r13_q85 = q84)

  tab <- tab %>%
    tidyr::gather(variable, value) %>%
    # Summarize by variable
    group_by(variable) %>%
    # summarise all columns
    summarise(`Mean` = round(mean(value, na.rm=TRUE),3)) %>%
    mutate(Mean = format(Mean, digits = 3))

  variable_names <- gsub("(...)(_)(.*)", "\\3", tab$variable)
  i <- match(variable_names, pcq_lookup$question_qno)
  variable_names <- pcq_lookup[i,c("question_pa_2022a")]
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
  pack_rows("Prisoner Relationships",1,5,bold=T) %>%
  pack_rows("Staff-Prisoner Relationships and Procedural Justice",6,13,bold=T) %>%
  pack_rows("Safety",14,18,bold=T) %>%
  pack_rows("Visits",19,24,bold=T) %>%
  pack_rows("Contact with the Outside World",25,27,bold=T) %>%
  pack_rows("Sleep",28,30,bold=T) %>%
  pack_rows("Care",31,36,bold=T) %>%
  pack_rows("Shop",37,39,bold=T) %>%
  pack_rows("Complaints",40,41,bold=T) %>%
  pack_rows("Satisfaction with Activities",42,48,bold=T) %>%
  pack_rows("Availability of Activities",49,52,bold=T) %>%
  pack_rows("Reintegration",53,56,bold=T) %>%
  pack_rows("Autonomy",57,60,bold=T) %>%
  save_kable(file = "output/tables/tabx_summary_climate_scales.pdf",
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
  pack_rows("Prisoner Relationships",1,5,bold=T) %>%
  pack_rows("Staff-Prisoner Relationships and Procedural Justice",6,13,bold=T) %>%
  pack_rows("Safety",14,18,bold=T) %>%
  pack_rows("Visits",19,24,bold=T) %>%
  pack_rows("Contact with the Outside World",25,27,bold=T) %>%
  pack_rows("Sleep",28,30,bold=T) %>%
  pack_rows("Care",31,36,bold=T) %>%
  pack_rows("Shop",37,39,bold=T) %>%
  pack_rows("Complaints",40,41,bold=T) %>%
  pack_rows("Satisfaction with Activities",42,48,bold=T) %>%
  pack_rows("Availability of Activities",49,52,bold=T) %>%
  pack_rows("Reintegration",53,56,bold=T) %>%
  pack_rows("Autonomy",57,60,bold=T) %>%
  footnote(threeparttable = T,
           "Footnote text") %>%
  save_kable(file = "output/tables/tabx_summary_climate_scales.tex",
             self_contained = T,density = 200)

