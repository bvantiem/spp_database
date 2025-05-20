# Set up ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")
source("scripts/0_id_masking_function.R")
randassign <- readRDS("data/processed/randassign.Rds")
basic <- readRDS("data/processed/basic.Rds")
basic <- dummy_cols(basic, select_columns = "marital_status_code")

# Merge with static characteristics from basic ####
static_vars <- c(#"est_days_served_on_20220501",
                 "min_sent_days", "max_sent_days",
                 #"days_to_min_at_treatment",
                 # "life",
                 #"commit_cnty", "cnty_name",
                 #"asca",
                 "violent_offense", "property_offense", "drugs_offense","publicorder_offense",
                 # "notanarrest_offense",
                 # "race_code", "race",
                 "race_black","race_white",
                 "age_at_treatment",
                 #"sex_type",
                 "marital_status_code_MAR",
                 # "mh_code",
                 #"STG",
                 #"grade_complete",
                 # "age_on_20220501",
                 "high_school")


data.balance <- left_join(randassign, basic[-which(duplicated(basic$research_id)),c("research_id", static_vars)])

# Drop lifers
data.balance <- data.balance[which(data.balance$stratum!="lifer"),]

# Override one indidivual whose sentence was presumably commuted (checking this with Jordan)
data.balance[which(data.balance$research_id=="rid_hy1664"),"life"] <- 0

# Check for missing data
for(i in static_vars){
  print(i)
  print(length(which(is.na(data.balance[,i]))))
}

# Generate basic table with means ####
prepare_summary_column <- function(data.balance){
  # Note: numbering the rows because tidyr::gather puts variables in alphabetical order
  tab <- data.balance %>%
    transmute(
      # Sentence Characteristics
      minimum_sentence = min_sent_days/365,
      maximum_sentence = max_sent_days/365,
      life = life)

  tab <- data.balance %>%
    transmute(
      # Sentence Characteristics
      r01a_minimum_sentence = min_sent_days/365,
      r01b_maximum_sentence = max_sent_days/365,
      # r01c_life = life,
      # Offense Characteristics
      r02a_violent_offense = violent_offense,
      r02b_property_offense = property_offense,
      r02c_drugs_offense = drugs_offense,
      r02d_publicorder_offense = publicorder_offense,
      # Demographics
      r03a_black = race_black,
      r03b_white = race_white,
      r03c_age_at_treatment = age_at_treatment,
      r03d_married = marital_status_code_MAR,
      r03e_high_school_degree = high_school)
      #r03e_security_threat_group = STG)

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
  variable_names[which(variable_names=="Publicorder Offense")] <- "Public Order Offense"
  tab$variable <- variable_names
  names(tab) <- c("Variable", "Share/Mean")
  rownames(tab) <- NULL
  return(tab)
}

tab <- cbind(prepare_summary_column(data.balance),
             prepare_summary_column(data.balance %>% filter(treated == 1))[2],
             prepare_summary_column(data.balance %>% filter(treated == 0))[2]) %>%
  setNames(c("Variable","All","LSU","Control"))

tab$`T-C Diff` <- round(as.numeric(tab$LSU)-as.numeric(tab$Control),3)
tab$`T-C Diff`[nrow(tab)] <- ""

# Randomization inference for pvalues ####
# F-test on Balance test
data.balance.reg <- data.balance[,c("treated", "stratum", static_vars)]
balance.test <- lm(treated ~ ., data=data.balance.reg[,-which(names(data.balance.reg)=="stratum")])
summary(balance.test)
fstat.pvalue.sample <- pf(summary(balance.test)$fstatistic[1],
                          summary(balance.test)$fstatistic[2],
                          summary(balance.test)$fstatistic[3],lower.tail=FALSE)

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

# Assign blocks
data.balance.reg.ri <- data.balance.reg
names(data.balance.reg.ri)[which(names(data.balance.reg.ri)=="stratum")] <- "block"
declaration <-
  with(data.balance.reg.ri,{
    declare_ra(
      blocks = block
    )
  })


# Balance test - omnibus F test
balance_fun <- function(data) {
  summary(lm(treated ~ ., data = data))$f[1]
}

balance_fun(data.balance.reg.ri[,-which(names(data.balance.reg.ri)=="block")])

ri2_out.ftest <-
  conduct_ri(
    test_function = balance_fun,
    declaration = declaration,
    assignment = "treated",
    sharp_hypothesis = 0,
    data = data.balance.reg.ri
  )

ri2_out.ftest

# Balance test - by covariate
# WARNING: This relies on variables in static_vars being in the same order as the variables in tab
df.pvalues <- data.frame(var = static_vars,
                         pvalue = NA)

for(i in static_vars){
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

# Build in extra row for f-test
tab <- rbind(tab[1:nrow(tab)-1,],
             c("", "", "", "", ""),
             c("Joint F-Test (pvalue)", "", "", "", ""),
             tab[nrow(tab),])

# Add p values to table
tab <- cbind(tab,
             Pvalue= c(df.pvalues$pvalue, "", summary(ri2_out.ftest)$two_tailed_p_value, ""))
tab

# Output balance table to PDF and latex ####
# PDF
tab %>%
  kbl(caption = "Balance Table",
      align = c("lrrrrr"),
      row.names = FALSE) %>%
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>%
  #kable_styling(font_size = 14) %>%
  #add_header_above(c(" " = 1, "Subset" = 3)) %>%
  pack_rows("Sentence Characteristics",1,2,bold=T) %>%
  pack_rows("Offense Characteristics",3,6,bold=T) %>%
  pack_rows("Demographics",7,11,bold=T) %>%
  row_spec(c(13,14),
           hline_after=TRUE,
           extra_css = "border-bottom: 1px solid") %>%
  save_kable(file = "output/tables/tabx_balance_table.pdf",
             self_contained = T,density = 200)

# Latex
tab %>%
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
  pack_rows("Sentence Characteristics",1,2,bold=T) %>%
  pack_rows("Offense Characteristics",3,6,bold=T) %>%
  pack_rows("Demographics",7,11,bold=T) %>%
  row_spec(c(13,14),
           hline_after=TRUE,
           extra_css = "border-bottom: 1px solid") %>%
  footnote(threeparttable = T,
           "P-values are computed using randomization inference and with randomization permuted at the level of each stratum.") %>%
  save_kable(file = "output/tables/tabx_balance_table.tex",
             self_contained = T,density = 200)

