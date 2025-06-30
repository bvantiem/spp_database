# ================================================================= ####
# Notes to Script ####
# -- Objective ####

# -- Readme ####

# -- To do (delete if none) ####
## @Britte, review code and outputs
## @Jonas, ensure consistency and explained code throughout 
## Fix PDF export error (userside? Ghostscript not found) - all pdf exports are disabled with #!
## Consider redefining rct_wave_lookup to cut code

# ================================================================= ####
# Set up ####
# -- Prepare environment ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")

# -- Functions ####

# -- Data ####
respart <- readRDS("data/processed/de_identified/3_research_participants_masked.Rds")
randassign <- readRDS("data/processed/de_identified/1b_randassign_masked.Rds")
admission <- readRDS("data/processed/de_identified/2b_admissions.Rds")
conduct <- readRDS("data/processed/de_identified/2_conduct_rct_cleaned.Rds")
# -- Constants ####
report_date <- unique(conduct$date_datapull)
# ================================================================= ####
# Merge release dates into randassign ####
admission_rel_dates <- admission %>%
  filter(rct %in% c(0,1)) %>%
  select(research_id, rel_rct, rct_mnths_to_exit, rct_mnths_to_release, rct_mnths_since_release) %>%
  distinct() 

randassign <- left_join(randassign,
                        admission_rel_dates,
                        by = "research_id",
                        relationship = "one-to-one")


# -- -- Fix factors for stratum ####
randassign <- randassign %>%
  mutate(rct_stratum = factor(
    rct_stratum,
    levels = c("lifer", "commuted death", "00_06_m", "06_12_m", "12_60_m", "60_pl_m"),
    labels = c(
      "lifer",
      "commuted death",
      "0–6 Months",
      "6–12 Months",
      "12–60 Months",
      "60+ Months"
    )
  ))

# ================================================================= ####
# TREATMENT COMPLIANCE ####
# -- By Cohort ####
# -- -- Prepare table ####
# -- -- -- Subset to treated
treatment_compliance <- randassign %>%
  filter(rct == 1) %>%
  filter(rct_stratum %ni% c("lifer", "commuted death")) %>%
  mutate(onunit = ifelse(is.na(rct_exit_dt), 1, 0)) 

# -- -- -- Integer rows: number of people 
tab_numbers <- treatment_compliance %>% 
  group_by(rct_treat_wave) %>%
  summarise(currently_on_unit = sum(onunit, na.rm=T),
            released = sum(rct_exit_community, na.rm=T),
            transferred = sum(rct_exit_transferred, na.rm=T),
            removed = sum(rct_exit_removed, na.rm=T),
            refused_treatment = sum(rct_exit_refused, na.rm=T),
            total_cohort = sum(currently_on_unit, 
                               released, 
                               transferred, 
                               removed, 
                               refused_treatment, na.rm = T))

# -- -- -- Calculate grand total across all categories and cohorts 
grand_total <- tab_numbers %>%
  summarise(across(currently_on_unit:refused_treatment, sum, na.rm = TRUE)) %>%
  sum()

# -- -- -- Calculate percent of grand total 
tab_percents <- tab_numbers %>%
  mutate(across(currently_on_unit:refused_treatment,
                ~ .x / grand_total * 100,
                .names = "{.col}_pct")) %>%
  mutate(across(ends_with("_pct"),
                ~ paste0(formatC(.x, format = "f", digits = 2), "%")))

# -- -- -- Combine number and percent rows 
tab_combined <- tab_numbers

for (col in names(tab_numbers)[-1]) { # Loop through all columns except the wave-variable
  if (col != "row_type") {
    tab_combined[[col]] <- paste0(tab_numbers[[col]], " (", trimws(tab_percents[[paste0(col, "_pct")]]), ")")
  }
}

# -- -- -- Add wave date to cohort label
rct_wave_lookup <- treatment_compliance %>% 
  select(rct_treat_wave, rct_treat_dt) %>% 
  distinct()

tab_combined <- tab_combined %>% 
  left_join(rct_wave_lookup, by = "rct_treat_wave") %>% 
  mutate(rct_treat_wave = paste0("Cohort ", rct_treat_wave, " (", rct_treat_dt, ")")) %>% 
  rename(cohort = rct_treat_wave) %>% 
  select(-rct_treat_dt)

# -- -- -- Compute sums and add as last row 
totals <- tab_numbers %>%
  summarise(
    cohort = "Total",
    currently_on_unit = sum(as.numeric(currently_on_unit), na.rm = T),
    released = sum(as.numeric(released), na.rm = T),
    removed = sum(as.numeric(removed), na.rm = T),
    transferred = sum(as.numeric(transferred), na.rm = T),
    refused_treatment = sum(as.numeric(refused_treatment), na.rm = T),
    total_cohort = sum(currently_on_unit, released, removed, transferred, refused_treatment, na.rm = T)
  ) 

for (col in names(tab_numbers)[-1]) { # Loop through all columns except the wave-variable
  if (col != "row_type") {
    tab_combined[[col]] <- paste0(tab_numbers[[col]], " (", trimws(tab_percents[[paste0(col, "_pct")]]), ")")
  }
}

tab_combined_with_totals <- rbind(tab_combined, totals) %>%
  mutate(across(everything(), ~ ifelse(str_detect(as.character(.), "NA"), "-", .))) %>%
  mutate(total_cohort = sub(" \\(.*\\)", "", total_cohort)) #remove () for total cohort count

names(tab_combined_with_totals) <- str_to_title(gsub("_", " ", names(tab_combined_with_totals)))
names(tab_combined_with_totals)[which(names(tab_combined_with_totals)=="Released")] <- "Released from Unit"
# -- -- Save table ####
# -- -- -- Latex Table - treatment compliance 
file_name <- "tabx_treatment_compliance"
tab_combined_with_totals %>%
  kbl(caption = "Treatment Compliance by Cohort",
      align = c("lrrrrrr"),
      label = file_name,
      format.args = list(big.mark = ","),
      booktabs = T,
      linesep = "",
      row.names = F,
      format = "latex") %>%
  kable_styling(latex_options = "hold_position") %>%
  kable_styling(font_size = 7) %>%
  add_header_above(c(" " = 3, "Left Unit for Reason Other Than Release" = 3, " " = 1)) %>%
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>%
  # column_spec(7, border_left=T, extra_css = "border-left: 1px solid black;")  %>% 
  row_spec(c(nrow(tab_combined_with_totals)-1),
           hline_after=TRUE,
           extra_css = "border-bottom: 1px solid") %>%
  footnote(threeparttable = T,
  "This table summarizes the distribution of treatment compliance by cohort for individuals enrolled in the RCT.  Percentages are based on the full table total (n = 164).") %>% 
  landscape() %>%
  # kable_styling(latex_options = c("scale_down")) %>% 
  save_kable(file = paste0("output/tables/", file_name, ".tex"),
             self_contained = T,density = 200) %>%
  save_kable(file = paste0("C:/Users/britt/Dropbox/Apps/Overleaf/SPP/tables_arnold_report/", file_name, ".tex"),
             self_contained = T,density = 200)

# -- By Stratum ####
# -- -- Prepare table ####
# -- -- -- Group by stratum instead of wave (same logic) 
tab_numbers_stratum <- treatment_compliance %>% 
  group_by(rct_stratum) %>% #here is the only change
  summarise(
    currently_on_unit = sum(onunit, na.rm=T),
    released = sum(rct_exit_community, na.rm=T),
    transferred = sum(rct_exit_transferred, na.rm=T),
    removed = sum(rct_exit_removed, na.rm=T),
    refused_treatment = sum(rct_exit_refused, na.rm=T ),
    total_cohort = sum(currently_on_unit, released, transferred, removed, refused_treatment, na.rm = TRUE)
  )


# -- -- -- Grand total across entire table 
grand_total_stratum <- tab_numbers_stratum %>%
  summarise(across(currently_on_unit:refused_treatment, sum, na.rm = TRUE)) %>%
  sum()

# -- -- -- Calculate percent of total 
tab_percents_stratum <- tab_numbers_stratum %>%
  mutate(across(currently_on_unit:refused_treatment,
                ~ .x / grand_total_stratum * 100,
                .names = "{.col}_pct")) %>%
  mutate(across(ends_with("_pct"),
                ~ paste0(formatC(.x, format = "f", digits = 2), "%")))

# -- -- -- Combine count + percent (excluding total_cohort from % formatting) 
tab_combined_stratum <- tab_numbers_stratum

for (col in names(tab_combined_stratum)[-1]) { # Loop through all columns except the wave-variable
  if (col != "row_type") {
    tab_combined_stratum[[col]] <- paste0(tab_combined_stratum[[col]], " (", trimws(tab_percents_stratum[[paste0(col, "_pct")]]), ")")
  }
}

# -- -- -- Compute sums and add as last row 
totals_stratum <- tab_numbers_stratum %>%
  summarise(
    rct_stratum = "Total",
    currently_on_unit = sum(as.numeric(currently_on_unit), na.rm = TRUE),
    released = sum(as.numeric(released), na.rm = TRUE),
    removed = sum(as.numeric(removed), na.rm = TRUE),
    transferred = sum(as.numeric(transferred), na.rm = TRUE),
    refused_treatment = sum(as.numeric(refused_treatment), na.rm = TRUE),
    total_cohort = sum(currently_on_unit, released, removed, transferred, refused_treatment, na.rm = TRUE)
  ) %>% mutate(across(everything(), ~ as.character(.)))

tab_combined_stratum <- bind_rows(tab_combined_stratum, totals_stratum) %>%
  mutate(across(everything(), ~ ifelse(str_detect(as.character(.), "NA"), "-", .))) %>%
  mutate(total_cohort = sub(" \\(.*\\)", "", total_cohort)) #remove () for total cohort count

names(tab_combined_stratum) <- str_to_title(gsub("_", " ", names(tab_combined_stratum)))
names(tab_combined_stratum)[which(names(tab_combined_with_totals)=="Released")] <- "Released from Unit"

# -- -- Save table #### 
# -- -- -- Latex Table - treatment compliance 
file_name <- "tabx_treatment_compliance_stratum"
tab_combined_stratum %>%
  kbl(caption = "Treatment Compliance by Stratum",
      align = c("lrrrrrr"),
      label = file_name,
      format.args = list(big.mark = ","),
      booktabs = T,
      linesep = "",
      row.names = F,
      format = "latex") %>%
  kable_styling(latex_options = "hold_position") %>%
  kable_styling(font_size = 7) %>%
  add_header_above(c(" " = 3, "Left Unit for Reason Other Than Release" = 3, " " = 1)) %>%
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>%
  # column_spec(7, border_left=T, extra_css = "border-left: 1px solid black;")  %>% 
  row_spec(c(nrow(tab_combined_with_totals)-1),
           hline_after=TRUE,
           extra_css = "border-bottom: 1px solid") %>%
  footnote(threeparttable = T,
           "This table summarizes the distribution of treatment compliance by stratum for individuals enrolled in the RCT.  Percentages are based on the full table total (n = 164).") %>% 
  landscape() %>%
  # kable_styling(latex_options = c("scale_down")) %>% 
  save_kable(file = paste0("output/tables/", file_name, ".tex"),
             self_contained = T,density = 200) %>%
  save_kable(file = paste0("C:/Users/britt/Dropbox/Apps/Overleaf/SPP/tables_arnold_report/", file_name, ".tex"),
             self_contained = T,density = 200)

# ================================================================= ####
# ENROLLMENT ####
# -- By Treatment cohort ####

# -- -- Prepare table: Enrolled ####
# -- -- -- Subset relevant groups 
enrollment <- randassign %>%
  filter(rct_stratum %ni% c("lifer", "commuted death")) 

# -- -- -- Prepare table A: Integrer rows, count enrolled per cohort and treatment group 
enrollment_count <- enrollment %>% 
  group_by(rct_treat_wave, rct) %>%
  summarise(
    enrolled = n()
  ) %>% 
  pivot_wider(names_from = "rct",
              values_from = "enrolled") %>% 
  rename(control = `0`, treatment = `1`) %>% 
  relocate(treatment, .before= control) %>% 
  mutate(total = control+treatment) 

# -- -- -- -- Grand total across entire table  
grand_total_enrolled <- sum(enrollment_count$total)

# -- -- -- -- Percent rows, enrolled, of total treatmnet+control 
enrollment_pct <- enrollment_count %>%
  mutate(pct_treatment = round(treatment/grand_total_enrolled*100,2),
         pct_control = round(control/grand_total_enrolled*100, 2),
         pct_total = round(total/grand_total_enrolled*100, 2)) %>% 
  mutate(across(everything(), ~ as.character(.)))

# -- -- -- -- Combine number and percent rows 
tab_combined_enrollment <- enrollment_count

for (col in names(enrollment_count)[-1]) {  # Skip rct_treat_wave
  tab_combined_enrollment[[col]] <- paste0(
    enrollment_count[[col]], " (", trimws(enrollment_pct[[paste0("pct_", col)]]), "%)"
  )
}

# -- -- -- -- Add wave date to cohort label 
rct_wave_lookup <- enrollment %>% 
  select(rct_treat_wave, rct_treat_dt) %>% 
  distinct()

tab_combined_enrollment <- tab_combined_enrollment %>% 
  left_join(rct_wave_lookup, by = "rct_treat_wave") %>% 
  mutate(rct_treat_wave = paste0("Cohort ", rct_treat_wave, " (", rct_treat_dt, ")")) %>% 
  rename(cohort = rct_treat_wave) %>% 
  select(-rct_treat_dt)

# -- -- -- -- Create total row, enrolled 
totals_enrollment <- enrollment_count %>% ungroup() %>% 
  summarise(
    cohort = "Total enrolled [released] (n)",
    treatment = sum(as.numeric(treatment, na.rm = T)),
    control = sum(as.numeric(control), na.rm = T),
    total = sum(treatment, control, na.rm = T)
  ) %>% mutate(across(everything(), ~ as.character(.)))

# -- -- -- -- Create (empty) total row for target 
totals_enrollment_cumpct <- tibble(
  cohort = "Cumulative percent of targeted releases (N=300)",
  treatment = "-",
  control = "-",
  total = "-"
)

# -- -- -- -- Combine the totals rows with data 
tab_combined_enrolled_with_totals <- bind_rows(tab_combined_enrollment, 
                                               totals_enrollment,
                                               totals_enrollment_cumpct) %>%
  mutate(across(everything(), ~ ifelse(str_detect(as.character(.), "NA"), "-", .))) #if zero, add dash

names(tab_combined_enrolled_with_totals) <- str_to_title(gsub("_", " ", names(tab_combined_enrolled_with_totals)))

# -- -- Prepare table: Enrolled & Released ####
# -- -- -- Subset relevant groups: only those released  
enrollment_released <- randassign %>%
  filter(rct_stratum %ni% c("lifer", "commuted death")) %>%
  mutate(released = ifelse(is.na(rel_rct), 0, 1)) %>%
  filter(released == 1)

# -- -- -- Integer rows, count enrolled/released per cohort and treatment group 
enrollment_released_count <- enrollment_released %>% 
  group_by(rct_treat_wave, rct) %>%
  summarise(
    enrolled = n()
  ) %>% 
  pivot_wider(names_from = "rct",
              values_from = "enrolled") %>%
  rename(control = `0`, treatment = `1`) %>% 
  relocate(treatment, .before= control) %>% 
  mutate(total = sum(control, treatment, na.rm=T)) 

# -- -- Cumulative percent of targeted release variable
n_target_release <- 300

# -- -- Grand total released
grand_total_released <- sum(enrollment_released_count$total, na.rm=T)
grand_total_released

# -- -- Percent rows, enrolled and released, of total treatment+control
enrollment_released_pct <- enrollment_released_count %>%
  mutate(
    pct_treatment = round(sum(treatment, na.rm=T) / grand_total_released * 100, 2),
    pct_control   = round(sum(treatment, na.rm=T) / grand_total_released * 100, 2),
    pct_total     = round(sum(treatment, na.rm=T) / grand_total_released * 100, 2)
  )

# -- Combine counts and percent for enrolled and released
tab_combined_released <- enrollment_released_count

for (col in names(enrollment_released_count)[-1]) {
  tab_combined_released[[col]] <- paste0(
    enrollment_released_count[[col]], " (", trimws(enrollment_released_pct[[paste0("pct_", col)]]), "%)")
}

# -- -- -- Add wave date to cohort label 
rct_wave_lookup <- enrollment_released %>% 
  select(rct_treat_wave, rct_treat_dt) %>% 
  distinct()

tab_combined_released <- tab_combined_released %>% 
  left_join(rct_wave_lookup, by = "rct_treat_wave") %>% 
  mutate(rct_treat_wave = paste0("Cohort ", rct_treat_wave, " (", rct_treat_dt, ")")) %>% 
  rename(cohort = rct_treat_wave) %>% 
  select(-rct_treat_dt)

# -- -- -- Total rows 
# for counts, released
totals_enrollment_released_count <- enrollment_released_count %>% ungroup() %>% 
  summarise(
    rct_treat_wave = "Total enrolled [released] (n)",
    treatment = sum(as.numeric(treatment), na.rm = T),
    control = sum(as.numeric(control), na.rm = T),
    total = sum(total, na.rm = T)) %>% 
  rename(cohort = rct_treat_wave) %>% 
  mutate(across(everything(), ~ as.character(.)))

# for pct_target, released
totals_enrollment_released_pct_target <- enrollment_released_count %>% 
  ungroup() %>% 
  summarise(
    rct_treat_wave = "Cumulative percent of targeted releases (N=300)",
    treatment = paste0(round(sum(as.numeric(treatment), na.rm = T)/n_target_release*100, 2), "%"),
    control = paste0(round(sum(as.numeric(control), na.rm = T)/n_target_release*100,2), "%"),
    total = paste0(round(sum(total, na.rm = T)/n_target_release*100, 2), "%")) %>% 
  rename(cohort = rct_treat_wave) %>% 
  mutate(across(everything(), ~ as.character(.)))

# -- -- Combine final table b, enrolled and released
totals_enrollment_comb <- rbind(tab_combined_released,
                                totals_enrollment_released_count,
                                totals_enrollment_released_pct_target
)

# -- -- -- Create placeholder for Cohort 7, if missing 
# -- -- -- Check if "Cohort 7" is missing 
if (!any(grepl("Cohort 7", totals_enrollment_comb$cohort))) {
  cohort7_row <- tibble(
    cohort = "Cohort 7 (2025-05-22)",
    treatment = "-",
    control = "-",
    total = "-"
  )
  
  # Insert cohort 7 before the summary rows
  totals_enrollment_comb <- bind_rows(
    totals_enrollment_comb[1:6, ],
    cohort7_row,
    totals_enrollment_comb[7:nrow(totals_enrollment_comb), ]
  )
}

# to title
names(totals_enrollment_comb) <- str_to_title(gsub("_", " ", names(totals_enrollment_comb)))

# -- -- -- Merge table Enrolled and Enrolled released 
totals_enrollment_comb_trimmed <- totals_enrollment_comb %>% ungroup %>% select(-Cohort)

tab_combined_enrolled_released <- bind_cols(tab_combined_enrolled_with_totals, 
                                            totals_enrollment_comb_trimmed)

# -- -- Save table ####
# -- -- Create Latex table enrollment, and enrolled and released 
file_name <- "tabx_enrollment"
tab_combined_enrolled_released %>%
  kbl(caption = "RCT Enrollment by Cohort",
      align = c("lrrr"),
      label = file_name,
      format.args = list(big.mark = ","),
      booktabs = T,
      col.names = c("Cohort (date)", 
                    rep(c("Treatment", "Control", "Total"),2)),
      linesep = "",
      row.names = F,
      format = "latex") %>%
  kable_styling(latex_options = "hold_position") %>%
  kable_styling(font_size = 7) %>%
  add_header_above(c(" " = 1, "Enrolled" = 3, "Enrolled and Released" = 3)) %>%
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>%
  # column_spec(7, border_left=T, extra_css = "border-left: 1px solid black;")  %>% 
  row_spec(c(nrow(tab_combined_with_totals)-1),
           hline_after=TRUE,
           extra_css = "border-bottom: 1px solid") %>%
  footnote(threeparttable = T,
           paste0("This table demonstrates the number of individuals enrolled in the RCT by cohort and the treatment they received while enrolled. 
            Enrolled shows total enrollment (n = 492), while the Enrolled and Released shows how many of those were released (n = 134).
            For Enrolled, percentages are based on the total enrolled sample (n = ", nrow(enrollment), ").
            For Enrolled and Released, percentages are based on total enrolled and released (n = ", nrow(enrollment_released), ").")) %>% 
  landscape() %>%
  # kable_styling(latex_options = c("scale_down")) %>% 
  save_kable(file = paste0("output/tables/", file_name, ".tex"),
             self_contained = T,density = 200) %>%
  save_kable(file = paste0("C:/Users/britt/Dropbox/Apps/Overleaf/SPP/tables_arnold_report/", file_name, ".tex"),
             self_contained = T,density = 200)

# -- By Stratum ####
# -- Prepare table A: Enrolled, stratum
# -- -- Prepare table: Enrolled ####
enrollment_count_stratum <- enrollment %>% 
  group_by(rct_stratum, rct) %>%
  summarise(
    enrolled = n()
  ) %>% 
  pivot_wider(names_from = "rct",
              values_from = "enrolled") %>% 
  rename(control = `0`, treatment = `1`) %>% 
  relocate(treatment, .before= control) %>% 
  mutate(total = control+treatment) 

# -- -- -- Grand total across entire table  
grand_total_enrolled_stratum <- sum(enrollment_count_stratum$total)

# -- -- -- Percent rows, enrolled, of total treatmnet+control 
enrollment_pct_stratum <- enrollment_count_stratum %>%
  mutate(pct_treatment = round(treatment/grand_total_enrolled_stratum*100,2),
         pct_control = round(control/grand_total_enrolled_stratum*100, 2),
         pct_total = round(total/grand_total_enrolled_stratum*100, 2)) %>% 
  mutate(across(everything(), ~ as.character(.)))

# -- -- -- Combine number and percent rows 
tab_combined_enrollment_stratum <- enrollment_count_stratum

for (col in names(enrollment_count_stratum)[-1]) {  # Skip rct_treat_wave
  tab_combined_enrollment_stratum[[col]] <- paste0(
    enrollment_count_stratum[[col]], " (", trimws(enrollment_pct_stratum[[paste0("pct_", col)]]), "%)"
  )
}

# -- -- -- Create total row, enrolled 
totals_enrollment_stratum <- enrollment_count_stratum %>% ungroup() %>% 
  summarise(
    rct_stratum = "Total enrolled [released] (n)",
    treatment = sum(as.numeric(treatment, na.rm = T)),
    control = sum(as.numeric(control), na.rm = T),
    total = sum(treatment, control, na.rm = T)
  ) %>% mutate(across(everything(), ~ as.character(.)))

# -- -- -- Create (empty) total row for target 
totals_enrollment_cumpct_stratum <- tibble(
  rct_stratum = "Cumulative percent of targeted releases (N=300)",
  treatment = "-",
  control = "-",
  total = "-"
)

# -- -- -- Combine the totals rows with data 
tab_combined_enrolled_with_totals_stratum <- bind_rows(tab_combined_enrollment_stratum, 
                                                       totals_enrollment_stratum,
                                                       totals_enrollment_cumpct_stratum) %>%
  mutate(across(everything(), ~ ifelse(str_detect(as.character(.), "NA"), "-", .))) #if zero, add dash

names(tab_combined_enrolled_with_totals_stratum) <- str_to_title(gsub("_", " ", names(tab_combined_enrolled_with_totals_stratum)))

# -- -- Prepare table: Enrolled & Released ####
# -- -- -- Integrer rows, count enrolled/released per cohort and treatment group 
enrollment_released_count_stratum <- enrollment_released %>% 
  group_by(rct_stratum, rct) %>%
  summarise(
    enrolled = n()
  ) %>% 
  pivot_wider(names_from = "rct",
              values_from = "enrolled") %>%
  rename(control = `0`, treatment = `1`) %>% 
  relocate(treatment, .before= control) %>% 
  mutate(total = sum(control,treatment, na.rm=T)) 

# -- -- Grand total released
grand_total_released_stratum <- sum(enrollment_released_count_stratum$total, na.rm=T)

# -- -- Percent rows, enrolled and released, of total treatment+control
enrollment_released_pct_stratum <- enrollment_released_count_stratum %>%
  mutate(
    pct_treatment = round(sum(treatment, na.rm=T) / grand_total_released_stratum * 100, 2),
    pct_control   = round(sum(control, na.rm=T) / grand_total_released_stratum * 100, 2),
    pct_total     = round(sum(total, na.rm=T) / grand_total_released_stratum * 100, 2)
  )

# -- Combine counts and percent for enrolled and released
tab_combined_released_stratum <- enrollment_released_count_stratum

for (col in names(enrollment_released_count_stratum)[-1]) {
  tab_combined_released_stratum[[col]] <- paste0(
    enrollment_released_count_stratum[[col]], " (", trimws(enrollment_released_pct_stratum[[paste0("pct_", col)]]), "%)")
}

# -- -- -- Total rows 
# -- -- for counts, released
totals_enrollment_released_count_stratum <- enrollment_released_count_stratum %>% ungroup() %>% 
  summarise(
    rct_stratum = "Total enrolled [released] (n)",
    treatment = sum(as.numeric(treatment, na.rm = T)),
    control = sum(as.numeric(control), na.rm = T),
    total = sum(total, na.rm = T)) %>% 
  mutate(across(everything(), ~ as.character(.)))

# -- -- for pct_target, released 
totals_enrollment_released_pct_target_stratum <- enrollment_released_count_stratum %>% 
  ungroup() %>% 
  summarise(
    rct_stratum = "Cumulative percent of targeted releases (N=300)",
    treatment = paste0(round(sum(as.numeric(treatment, na.rm = T))/n_target_release*100, 2), "%"),
    control = paste0(round(sum(as.numeric(control), na.rm = T)/n_target_release*100,2), "%"),
    total = paste0(round(sum(total, na.rm = T)/n_target_release*100, 2), "%")) %>%
  mutate(across(everything(), ~ as.character(.)))

# -- -- -- Combine final table b, enrolled and released 
totals_enrollment_comb_stratum <- bind_rows(tab_combined_released_stratum,
                                totals_enrollment_released_count_stratum,
                                totals_enrollment_released_pct_target_stratum)

# Merge table Enrolled and Enrolled released 
totals_enrollment_comb_trimmed_stratum <- totals_enrollment_comb_stratum %>% ungroup %>% select(-rct_stratum)

tab_combined_enrolled_released_stratum <- bind_cols(tab_combined_enrolled_with_totals_stratum, 
                                                    totals_enrollment_comb_trimmed_stratum)

# to title
names(tab_combined_enrolled_released_stratum) <- str_to_title(gsub("_", " ", names(tab_combined_enrolled_released_stratum)))

# -- -- Save table ####
# -- -- Create Latex table enrollment, and enrolled and released
file_name <- "tabx_enrollment_stratum"
tab_combined_enrolled_released_stratum %>%
  kbl(caption = "RCT Enrollment by Stratum",
      align = c("lrrr"),
      label = file_name,
      format.args = list(big.mark = ","),
      booktabs = T,
      col.names = c("Stratum", 
                    rep(c("Treatment", "Control", "Total"),2)),
      linesep = "",
      row.names = F,
      format = "latex") %>%
  kable_styling(latex_options = "hold_position") %>%
  kable_styling(font_size = 7) %>%
  add_header_above(c(" " = 1, "Enrolled" = 3, "Enrolled and Released" = 3)) %>%
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>%
  # column_spec(7, border_left=T, extra_css = "border-left: 1px solid black;")  %>% 
  row_spec(c(nrow(tab_combined_enrolled_released_stratum)-2),
           hline_after=TRUE,
           extra_css = "border-bottom: 1px solid") %>%
  footnote(threeparttable = T,
           paste0("This table demonstrates the number of individuals enrolled in the RCT by stratum and the treatment they received while enrolled. 
            Enrolled shows total enrollment (n = 492), while the Enrolled and Released shows how many of those were released (n = 134).
            For Enrolled, percentages are based on the total enrolled sample (n = ", nrow(enrollment), ").
            For Enrolled and Released, percentages are based on total enrolled and released (n = ", nrow(enrollment_released), ").")) %>% 
  landscape() %>%
  # kable_styling(latex_options = c("scale_down")) %>% 
  save_kable(file = paste0("output/tables/", file_name, ".tex"),
             self_contained = T,density = 200) %>%
  save_kable(file = paste0("C:/Users/britt/Dropbox/Apps/Overleaf/SPP/tables_arnold_report/", file_name, ".tex"),
             self_contained = T,density = 200)


# ================================================================= ####
# TREATMENT DURATION ####
# -- By Cohort ####
# -- -- Subset relevant groups 
time_spent <- randassign %>%
  filter(rct_stratum %ni% c("lifer", "commuted death")) %>%
  mutate(released = ifelse(is.na(rel_rct), 0, 1)) %>%
  filter(released == 1)

# -- -- Integer rows: number of people 
time_spent_mean <- time_spent %>% 
  complete(rct_treat_wave, rct) %>%  # ensure all combinations exist
  group_by(rct_treat_wave, rct) %>%
  summarise(released = n(),
            mean_rct_mnths_to_exit = round(mean(rct_mnths_to_exit, na.rm=T),1),
            mean_rct_mnths_to_release = round(mean(rct_mnths_to_release, na.rm=T),1),
            mean_rct_mnths_since_release = round(mean(rct_mnths_since_release, na.rm=T),1), 
            .groups = "drop")

# -- -- Add wave date to cohort label 
rct_wave_lookup <- time_spent %>% 
  select(rct_treat_wave, rct_treat_dt) %>% 
  distinct()

# change cohort name
time_spent_mean <- time_spent_mean %>% 
  left_join(rct_wave_lookup, by = "rct_treat_wave") %>% 
  mutate(rct_treat_wave = paste0("Cohort ", rct_treat_wave, " (", rct_treat_dt, ")")) %>% 
  rename(cohort = rct_treat_wave) %>% 
  select(-rct_treat_dt)

# -- -- -- Table for treated 
time_spent_treated <- time_spent_mean %>% 
  filter(rct==1) %>% 
  select(-rct) %>%
  mutate(across(everything(), as.character)) #for binding

# -- -- -- -- Add total row for treated 
total_time_spent_treated <- time_spent_treated %>% 
  summarise(
    cohort = "Average",
    released = round(mean(as.numeric(released), na.rm = TRUE),1),
    mean_rct_mnths_to_exit  = round(mean(as.numeric(mean_rct_mnths_to_exit), na.rm = TRUE),1),
    mean_rct_mnths_to_release  = round(mean(as.numeric(mean_rct_mnths_to_release), na.rm = TRUE),1),
    mean_rct_mnths_since_release  = round(mean(as.numeric(mean_rct_mnths_since_release), na.rm = TRUE),1),
    .groups="drop"
  ) %>%
  mutate(across(everything(), as.character)) 

# -- -- Bind rows and clean NaN for final treated table
tab_time_spent_treated <- bind_rows(time_spent_treated,
                                    total_time_spent_treated) %>%
  mutate(across(everything(), ~ ifelse(str_detect(as.character(.), "NaN"), "-", .))) %>%
  mutate(released = ifelse(cohort == "Average", "-", released))

# -- -- -- Table for control 
time_spent_control <- time_spent_mean %>% 
  filter(rct==0) %>% 
  select(-rct) %>%
  mutate(across(everything(), as.character)) #for binding

# remove 0 that must be error
time_spent_control <- time_spent_control %>% mutate(mean_rct_mnths_to_exit = ifelse(str_detect(mean_rct_mnths_to_exit, "0"), "NaN", mean_rct_mnths_to_exit))

# -- -- -- -- Add total row for released 
total_time_spent_control <- time_spent_control %>% 
  summarise(
    cohort = "Average",
    released = round(mean(as.numeric(released), na.rm = TRUE),1),
    mean_rct_mnths_to_exit  = round(mean(as.numeric(mean_rct_mnths_to_exit), na.rm = TRUE),1),
    mean_rct_mnths_to_release  = round(mean(as.numeric(mean_rct_mnths_to_release), na.rm = TRUE),1),
    mean_rct_mnths_since_release  = round(mean(as.numeric(mean_rct_mnths_since_release), na.rm = TRUE),1),
    .groups="drop"
  ) %>%
  mutate(across(everything(), as.character))

# bind rows and clean NaN
tab_time_spent_control <- bind_rows(time_spent_control,
          total_time_spent_control) %>%
  mutate(across(everything(), ~ ifelse(str_detect(as.character(.), "NaN"), "-", .))) %>%
  mutate(released = ifelse(cohort == "Average", "-", released)) %>%
  mutate(cohort = ifelse(cohort == "Average", "Average Time in Months", cohort))

# -- -- Save table ####
# -- --- Merge table tab_time_spent_treated and tab_time_spent_control 
time_spent_control_trimmed <- tab_time_spent_control %>% select(-cohort)

tab_time_spent <- bind_cols(tab_time_spent_treated, 
                            time_spent_control_trimmed)
# to title
names(tab_time_spent) <- str_to_title(gsub("_", " ", names(tab_time_spent)))

# -- -- Create Latex table for time_spent 
file_name <- "tabx_treatment_duration"
tab_time_spent[,-7] %>%
  kbl(caption = "Treatment Duration in Months, by Cohort and Treatment Status",
      align = c("lrrrrrrr"),
      label = file_name,
      format.args = list(big.mark = ","),
      booktabs = T,
      col.names = c("Cohort",
                    "Released", 
                    "Time to Unit Exit", 
                    "Time to Release",
                    "Time Since Release",
                    "Released", 
                    "Time to Release",
                    "Time Since Release"),
      linesep = "",
      row.names = F,
      format = "latex") %>%
  kable_styling(latex_options = "hold_position") %>%
  kable_styling(font_size = 7) %>%
  add_header_above(c(" " = 1, "Treated" = 4, "Control" = 3)) %>%
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>%
  row_spec(c(nrow(tab_time_spent)-1),
           hline_after=TRUE,
           extra_css = "border-bottom: 1px solid") %>%
  footnote(threeparttable = T,
           paste0("This table contains the average number of months between randomization and release, as well as the time between release and the date until data was available (", report_date, "). 
                  For treated individuals, the table also contains the time between randomization and exit from the Little Scandinavia Unit, as some individuals leave the unit prior to release. 
                  The table is split by treatment cohort and treatment group. Dashes inidicate no data.")) %>% 
  landscape() %>%
  save_kable(file = paste0("output/tables/", file_name, ".tex"),
             self_contained = T,density = 200) %>%
  save_kable(file = paste0("C:/Users/britt/Dropbox/Apps/Overleaf/SPP/tables_arnold_report/", file_name, ".tex"),
             self_contained = T,density = 200)

# -- By Stratum ####
# -- -- Integer rows: number of people
time_spent_mean_stratum <- time_spent %>% 
  complete(rct_stratum, rct) %>%  # ensure all combinations exist
  group_by(rct_stratum, rct) %>%
  mutate(released = ifelse(is.na(rel_rct), 0, 1)) %>%
  summarise(released = sum(released, na.rm=T),
            mean_rct_mnths_to_exit = round(mean(rct_mnths_to_exit, na.rm=T),1),
            mean_rct_mnths_to_release = round(mean(rct_mnths_to_release, na.rm=T),1),
            mean_rct_mnths_since_release = round(mean(rct_mnths_since_release, na.rm=T),1), 
            .groups = "drop") %>%
  filter(rct_stratum %ni% c("lifer", "commuted death"))
  

# -- -- -- Table for treated 
time_spent_treated_stratum <- time_spent_mean_stratum %>% 
  filter(rct==1) %>% 
  select(-rct) %>%
  mutate(across(everything(), as.character)) #for binding

# -- -- -- -- Add total row for treated 
total_time_spent_treated_stratum <- time_spent_treated_stratum %>% 
  summarise(
    rct_stratum = "Average",
    released = round(mean(as.numeric(released), na.rm = TRUE),1),
    mean_rct_mnths_to_exit  = round(mean(as.numeric(mean_rct_mnths_to_exit), na.rm = TRUE),1),
    mean_rct_mnths_to_release  = round(mean(as.numeric(mean_rct_mnths_to_release), na.rm = TRUE),1),
    mean_rct_mnths_since_release  = round(mean(as.numeric(mean_rct_mnths_since_release), na.rm = TRUE),1),
    .groups="drop"
  ) %>%
  mutate(across(everything(), as.character))


# -- -- Bind rows and clean NaN for final treated table
tab_time_spent_treated_stratum <- bind_rows(time_spent_treated_stratum,
                                            total_time_spent_treated_stratum) %>%
  mutate(across(everything(), ~ ifelse(str_detect(as.character(.), "NaN"), "-", .))) 

# -- -- -- Table for control 
time_spent_control_stratum <- time_spent_mean_stratum %>% 
  filter(rct==0) %>% 
  select(-rct) %>%
  mutate(across(everything(), as.character)) #for binding

#remove 0 that must be error
# time_spent_control_stratum <- time_spent_control_stratum %>% mutate(mean_rct_mnths_to_exit = ifelse(str_detect(mean_rct_mnths_to_exit, "0"), "NaN", mean_rct_mnths_to_exit))

# -- -- -- -- Add total row for control 
total_time_spent_control_stratum <- time_spent_control_stratum %>% 
  summarise(
    rct_stratum = "Average",
    released = round(mean(as.numeric(released), na.rm = TRUE),1),
    mean_rct_mnths_to_exit  = round(mean(as.numeric(mean_rct_mnths_to_exit), na.rm = TRUE),1),
    mean_rct_mnths_to_release  = round(mean(as.numeric(mean_rct_mnths_to_release), na.rm = TRUE),1),
    mean_rct_mnths_since_release  = round(mean(as.numeric(mean_rct_mnths_since_release), na.rm = TRUE),1),
    .groups="drop"
  ) %>%
  mutate(across(everything(), as.character))

# bind rows and clean NaN
tab_time_spent_control_stratum <- bind_rows(time_spent_control_stratum,
                                    total_time_spent_control_stratum) %>%
  mutate(across(everything(), ~ ifelse(str_detect(as.character(.), "NaN"), "-", .))) %>%
  mutate(released = ifelse(rct_stratum   == "Average", "-", released)) %>%
  mutate(rct_stratum   = ifelse(rct_stratum   == "Average", "Average Time in Months", rct_stratum))

# -- --- Merge table tab_time_spent_treated and tab_time_spent_control 
time_spent_control_trimmed_stratum <- tab_time_spent_control_stratum %>% select(-rct_stratum)

tab_time_spent_stratum <- bind_cols(tab_time_spent_treated_stratum, 
                                    time_spent_control_trimmed_stratum)

# to title
names(tab_time_spent_stratum) <- str_to_title(gsub("_", " ", names(tab_time_spent_stratum)))

# -- -- Save table ####
# -- -- Create Latex table for time_spent 
file_name <- "tabx_treatment_duration_stratum"
tab_time_spent_stratum[,-7] %>%
  kbl(caption = "Treatment Duration in Months, by Statum and Treatment Status",
      align = c("lrrrrrrr"),
      label = file_name,
      format.args = list(big.mark = ","),
      booktabs = T,
      col.names = c("Cohort",
                    "Released", 
                    "Time to Unit Exit", 
                    "Time to Release",
                    "Time Since Release",
                    "Released", 
                    "Time to Release",
                    "Time Since Release"),
      linesep = "",
      row.names = F,
      format = "latex") %>%
  kable_styling(latex_options = "hold_position") %>%
  kable_styling(font_size = 7) %>%
  add_header_above(c(" " = 1, "Treated" = 4, "Control" = 3)) %>%
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>%
  row_spec(c(nrow(tab_time_spent_stratum)-1),
           hline_after=TRUE,
           extra_css = "border-bottom: 1px solid") %>%
  footnote(threeparttable = T,
           paste0("This table contains the average number of months between randomization and release, as well as the time between release and the date until data was available (", report_date, "). 
                  For treated individuals, the table also contains the time between randomization and exit from the Little Scandinavia Unit, as some individuals leave the unit prior to release. 
                  The table is split by treatment stratum and treatment group. Dashes inidicate no data.")) %>% 
  landscape() %>%
  save_kable(file = paste0("output/tables/", file_name, ".tex"),
             self_contained = T,density = 200) %>%
  save_kable(file = paste0("C:/Users/britt/Dropbox/Apps/Overleaf/SPP/tables_arnold_report/", file_name, ".tex"),
             self_contained = T,density = 200)

