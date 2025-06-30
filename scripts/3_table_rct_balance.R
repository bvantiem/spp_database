# ================================================================= ####
# Notes to Script ####
# -- Objective ####
# This script prepares the balance table for an interim AV report in June/July 2025
# -- Readme ####

# -- To do ####


# ================================================================= ####
# Set up ####
# -- Prepare environment ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")
# source("scripts_restricted_access/0_id_masking_function.R")

# -- Read in Data ####
randassign <- readRDS("data/processed/de_identified/1b_randassign_masked.Rds")
basic <- readRDS("data/processed/de_identified/2_basic_cleaned.Rds")
admissions <- readRDS("data/processed/de_identified/2b_admissions.Rds")
conduct <- readRDS("data/processed/de_identified/2_conduct_rct_cleaned.Rds")

# -- Functions ####
prepare_summary_column <- function(data.balance){
  # Note: numbering the rows because tidyr::gather puts variables in alphabetical order
  tab <- data.balance %>%
    transmute(
      # Sentence Characteristics
      r01a_minimum_sentence = sent_min_in_days/365,
      r01b_maximum_sentence = sent_max_in_days/365,
      r01c_time_served_at_treatment = rct_mnths_pretreat,
      # r01c_life = life,
      # Offense Characteristics
      r02a_violent_offense = sent_off_asca_violent,
      r02b_property_offense = sent_off_asca_prop,
      r02c_drugs_offense = sent_off_asca_drugs,
      # Demographics
      r03a_black = dem_race_black,
      r03b_white = dem_race_white,
      r03c_age_at_treatment = dem_age_at_treatment,
      r03d_married = dem_marital_married,
      r03e_high_school_degree = dem_edu_high_school,
      #Misconduct guilty rates
      r04a_overall = cndct_pretreat_guilty_rate,
      r04b_category_a_incidents = cndct_pretreat_guilty_rate_a,
      r04c_drugs_incidents = cndct_pretreat_guilty_rate_drug,
      r04d_violent_incidents = cndct_pretreat_guilty_rate_violent
    )
  
  tab <- tab %>%
    tidyr::gather(variable, value) %>%
    # Summarize by variable
    group_by(variable) %>%
    # summarise all columns
    summarise(`Mean` = round(mean(value, na.rm=TRUE),3)) %>% # Check number of NA values after getting full data.balance
    mutate(Mean = format(Mean, digits = 3))
  
  tab_2 <- data.balance %>%
    select(
      r99a_N = research_id) %>%
    tidyr::gather(variable, value) %>%
    # Summarize by variable
    group_by(variable) %>%
    # summarise all columns
    summarise(`Mean` = length(unique(value)))
  
  tab <- rbind(tab,
               tab_2)
  
  variable_names <- gsub("(....)(_)(.*)", "\\3", tab$variable)
  variable_names <- str_to_title(gsub("_", " ", variable_names))
  # variable_names[which(variable_names=="Publicorder Offense")] <- "Public Order Offense"
  tab$variable <- variable_names
  names(tab) <- c("Variable", "Share/Mean")
  rownames(tab) <- NULL
  return(tab)
}
# ================================================================= ####
# Build dataframe for balance table 
# Prepare data for balance table 
# -- Merge with static characteristics from basic ####
data.balance <- left_join(randassign, 
                          basic[,c("research_id", 
                                   "dem_age_at_treatment",  
                                   "dem_edu_high_school",
                                   "dem_marital_married",
                                   "dem_race_black",
                                   "dem_race_white",
                                   "sent_min_in_days",
                                   "sent_max_in_days",
                                   "sent_off_asca_violent",
                                   "sent_off_asca_drugs",
                                   "sent_off_asca_prop")],
                          by = "research_id",
                          relationship = "one-to-one")

# -- Merge with pre-treatment time served in admissions data ####
# Admissions file is at the admissions level 
# Subset admissions data to rct sample only
# Get unique combinations of research_id and rct_time_pretreat 
admissions_rct <- admissions %>%
  filter(rct %in% c(0,1)) %>%
  select(research_id, rct_mnths_pretreat) %>%
  distinct()

# Merge in admissions data 
data.balance <- left_join(data.balance,
                          admissions_rct[,c("research_id",
                                        "rct_mnths_pretreat")],
                          by = "research_id",
                          relationship = "one-to-one")

# -- Merge with pre-treatment misconduct rates ####
conduct_by_id <- conduct %>%
  distinct(research_id, 
           cndct_pretreat_guilty_rate, 
           cndct_pretreat_guilty_rate_a,
           cndct_pretreat_guilty_rate_drug,
           cndct_pretreat_guilty_rate_violent)

# Assign people who do not appear in data.balance a zero rate on all columns 
# -- They don't appear because they did not have incidents 
data.balance <- data.balance %>%
  left_join(conduct_by_id, by = "research_id") %>%
  mutate(
    across(
      c(
        cndct_pretreat_guilty_rate, 
        cndct_pretreat_guilty_rate_a,
        cndct_pretreat_guilty_rate_drug,
        cndct_pretreat_guilty_rate_violent
      ),
      ~ replace_na(.x, 0)
    )
  )

# -- Drop lifers and those with commuted death sentences ####
data.balance <- data.balance[which(data.balance$rct_stratum %ni% c("lifer", "commuted death")),]
# -- Finalize dataframe #### 
# -- Note: These should be in the same order as the variables in the means table
data.balance <- data.balance[,c("research_id",
                                "rct", 
                                "rct_stratum",
                                "sent_min_in_days",
                                "sent_max_in_days",
                                "rct_mnths_pretreat",
                                "sent_off_asca_violent",
                                "sent_off_asca_prop",
                                "sent_off_asca_drugs",
                                "dem_race_black",
                                "dem_race_white",
                                "dem_age_at_treatment",
                                "dem_marital_married",
                                "dem_edu_high_school",
                                "cndct_pretreat_guilty_rate",
                                "cndct_pretreat_guilty_rate_a",
                                "cndct_pretreat_guilty_rate_drug",
                                "cndct_pretreat_guilty_rate_violent")]

data.balance.reg <- data.balance %>%
  select(-research_id, -rct_stratum)

# Retain stratum for regressions 
data.balance.reg.ri <- data.balance %>%
  select(-research_id) %>%
  rename(block = rct_stratum) %>%
  rename(treated = rct)
  
  

# -- Define covariates for regression ####
# -- Note: These should be in the same order as the variables in the means table
# -- -- as the code just cbinds them later. 
covariates <- names(data.balance)[which(names(data.balance) %ni% c("research_id", "rct", "rct_stratum"))]

# ================================================================= ####
# Build Table ####
# -- Means ####
tab <- cbind(prepare_summary_column(data.balance),
             prepare_summary_column(data.balance %>% filter(rct == 1))[2],
             prepare_summary_column(data.balance %>% filter(rct == 0))[2]) %>%
  setNames(c("Variable","All","Treat","Control"))

tab$`T-C Diff` <- round(as.numeric(tab$Treat)-as.numeric(tab$Control),3)
tab$`T-C Diff`[nrow(tab)] <- ""



# -- Randomization inference for pvalues ####
# -- -- F-test on Balance test

balance.test <- lm(rct ~ ., data=data.balance.reg)
summary(balance.test)
fstat.pvalue.sample <- pf(summary(balance.test)$fstatistic[1],
                          summary(balance.test)$fstatistic[2],
                          summary(balance.test)$fstatistic[3],lower.tail=FALSE)
# -- -- Assign blocks
declaration <-
  with(data.balance.reg.ri,{
    declare_ra(
      blocks = block
    )
  })

# -- -- Balance test - omnibus F test
balance_fun <- function(data) {
  summary(lm(treated ~ ., data = data))$f[1]
}

ri2_out.ftest <-
  conduct_ri(
    test_function = balance_fun,
    declaration = declaration,
    assignment = "treated",
    sharp_hypothesis = 0,
    data = data.balance.reg.ri[,-which(names(data.balance.reg.ri)=="block")]
  )

ri2_out.ftest

# -- -- Balance test - by covariate
# -- WARNING: This relies on variables in covariates being in the same order as the variables in tab
df.pvalues <- data.frame(var = covariates,
                         pvalue = NA)

for(i in covariates){
  print(i)
  ri2_out <-
    conduct_ri(
      test_function = balance_fun,
      declaration = declaration,
      assignment = "treated",
      sharp_hypothesis = 0,
      data = data.balance.reg.ri[,c("treated", i)]
    )
  df.pvalues[which(df.pvalues$var==i), "pvalue"] <- summary(ri2_out)$two_tailed_p_value
}
df.pvalues

# -- -- Build in extra row for f-test
tab_for_latex <- rbind(tab[1:nrow(tab)-1,],
             c("", "", "", "", ""), # empty row needed for merge later
             c("Joint F-Test (pvalue)", "", "", "", ""),
             tab[nrow(tab),])

# -- -- Add p values to table
tab_for_latex <- cbind(tab_for_latex,
             Pvalue= c(df.pvalues$pvalue, "", summary(ri2_out.ftest)$two_tailed_p_value, ""))
tab_for_latex <- tab_for_latex[-16,] #drop empty row. HARD CODED
rownames(tab_for_latex) <- seq_len(nrow(tab_for_latex))
tab_for_latex


# ================================================================= ####
# Output Latex Table to Overleaf Folder ####
file_name <- "tabx_balance"
report_date <- unique(conduct$date_datapull)

tab_for_latex %>%
  kbl(caption = "Balance Table",
      align = c("lrrrrr"),
      format = "latex",
      label = "balance_table",
      format.args = list(big.mark = ","),
      row.names = FALSE,
      booktabs = T) %>%
  kable_styling(latex_options = "hold_position") %>%
  kable_styling(font_size = 7) %>%
  #add_header_above(c(" " = 1, "Subset" = 3)) %>%
  pack_rows("Sentence Characteristics",1,3,bold=T) %>%
  pack_rows("Offense Characteristics",4,6,bold=T) %>%
  pack_rows("Demographics",7,11,bold=T) %>%
  pack_rows("Monthly Misconduct Rates",12,15,bold=T) %>%
  row_spec(c(15),
           hline_after=TRUE,
           extra_css = "border-bottom: 1px solid") %>%
  footnote(threeparttable = T,
           paste0("P-values are computed using randomization inference and with randomization permuted at the level of each stratum. Time served at treatment is calculated from the latest admission prior to treatment. Misconduct rates are the average monthly rate of unique misconduct numbers between the date of admission and date of treatment on which the individual was found guilty on at least one charge. Category A incidents are a subset of all incidents in which the most serious charge on which the individual was found guilty was a Category A charge. The three most common Category A charge categories consist of the possession or use of dangerous or controlled substances, using abusive, obscene or inappropriate language and fighting. Violent incidents include assault, aggravated assault, fighting and sexual harrassment. Drugs charges include the possession or use of controlled substances. This table was generated using data up until ", report_date, ".")) %>%
  save_kable(file = paste0("C:/Users/britt/Dropbox/Apps/Overleaf/SPP/tables_arnold_report/", file_name, ".tex"),
             self_contained = T,
             density = 200)



# ================================================================= ####
# # Old Code ####
# basic <- dummy_cols(basic, select_columns = "marital_status_code")
# Override one indidivual whose sentence was presumably commuted (checking this with Jordan) ####
# data.balance[which(data.balance$research_id=="rid_hy1664"),"life"] <- 0
# Manual randomization infoerence ####
# data.balance.reg <- data.balance[,c("rct", "rct_stratum", covariates)]
# balance.test <- lm(treated ~ ., data=data.balance.reg[,-which(names(data.balance.reg)=="stratum")])
# summary(balance.test)
# fstat.pvalue.sample <- pf(summary(balance.test)$fstatistic[1],
#                           summary(balance.test)$fstatistic[2],
#                           summary(balance.test)$fstatistic[3],lower.tail=FALSE)

# Manual randomization inference with 1000 permutations
# f.stat.list <- list()
# for(i in 1:1000){
#   temp <- data.balance.reg
#   temp$treated[which(temp$stratum=="00_06_m")] <- with(data.balance.reg[which(data.balance.reg$stratum=="00_06_m"),], sample(treated))
#   temp$treated[which(temp$stratum=="06_12_m")] <- with(data.balance.reg[which(data.balance.reg$stratum=="06_12_m"),], sample(treated))
#   temp$treated[which(temp$stratum=="12_60_m")] <- with(data.balance.reg[which(data.balance.reg$stratum=="12_60_m"),], sample(treated))
#   temp$treated[which(temp$stratum=="60_pl_m")] <- with(data.balance.reg[which(data.balance.reg$stratum=="60_pl_m"),], sample(treated))
#   balance.test <- lm(treated ~ ., data=temp[,-which(names(temp)=="stratum")])
#   fstat.pvalue <- pf(summary(balance.test)$fstatistic[1],
#                      summary(balance.test)$fstatistic[2],
#                      summary(balance.test)$fstatistic[3],lower.tail=FALSE)
#   f.stat.list[[i]] <- fstat.pvalue
# }
# f.stat.pvalues.vector <- unlist(f.stat.list)
# percentile_rank_fstat.pvalue.sample <- sum(f.stat.pvalues.vector <= fstat.pvalue.sample) / length(f.stat.pvalues.vector) * 100
# percentile_rank_fstat.pvalue.sample

# Do this using randomization inference package
# Continue here tomorrow:
# https://declaredesign.org/r/randomizr/articles/randomizr_vignette.html#block-random-assignment
# https://cran.r-project.org/web/packages/ri2/vignettes/ri2_vignette.html

# Output balance table to PDF ####
# # PDF
# file_name <- "tabx_balance"
# tab %>%
#   kbl(caption = "Balance Table",
#       align = c("lrrrrr"),
#       row.names = FALSE) %>%
#   kable_classic(full_width = F,
#                 html_font = "Times New Roman") %>%
#   #kable_styling(font_size = 14) %>%
#   #add_header_above(c(" " = 1, "Subset" = 3)) %>%
#   pack_rows("Sentence Characteristics",1,2,bold=T) %>%
#   pack_rows("Offense Characteristics",3,6,bold=T) %>%
#   pack_rows("Demographics",7,11,bold=T) %>%
#   row_spec(c(13,14),
#            hline_after=TRUE,
#            extra_css = "border-bottom: 1px solid") %>%
#   save_kable(file = paste0("C:/Users/britt/Dropbox/Apps/Overleaf/SPP/tables_arnold_report/", file_name, ".pdf"),
#              self_contained = T) 