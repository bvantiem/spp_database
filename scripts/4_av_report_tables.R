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
# Drop lifers and commuted death cases ####
randassign <- randassign %>%
  filter(rct_stratum %ni% c("lifer", "commuted death")) 

# Merge release dates into randassign ####
admission_rel_dates <- admission %>%
  filter(rct %in% c(0,1)) %>%
  select(research_id, rel_rct, rct_mnths_to_exit, rct_mnths_to_release, rct_mnths_since_release) %>%
  distinct() 

randassign <- left_join(randassign,
                        admission_rel_dates,
                        by = "research_id",
                        relationship = "one-to-one")

# Fix factors for stratum ####
randassign <- randassign %>%
  mutate(rct_stratum = factor(
    rct_stratum,
    levels = c("00_06_m", "06_12_m", "12_60_m", "60_pl_m"),
    labels = c(
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
  "This table summarizes the distribution of treatment compliance by cohort for individuals in the treated group.  Percentages are based on the full table total (n = 164).
  Individuals who were `Released from Unit' remained on the Little Scandinavia Unit from their date of randomziation until their release to the community.
  Individuals who were `Transferred' were transferred out of SCI Chester for reasons including ICE detainers, transfers to hospital or to other prisons for programming reasons. 
  Individuals who were `Removed' were removed from the unit for reasons that are generally related to repeated misconduct.
  Individuals who `Refused Treatment' refused to move to the Little Scandinavia Unit upon treatment notification and did not spend time on the unit.") %>% 
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
  row_spec(c(nrow(tab_combined_stratum)-1),
           hline_after=TRUE,
           extra_css = "border-bottom: 1px solid") %>%
  footnote(threeparttable = T,
           "This table summarizes the distribution of treatment compliance by stratum for individuals in the treated group.  Percentages are based on the full table total (n = 164).
  Individuals who were `Released from Unit' remained on the Little Scandinavia Unit from their date of randomziation until their release to the community.
  Individuals who were `Transferred' were transferred out of SCI Chester for reasons including ICE detainers, transfers to hospital or to other prisons for programming reasons. 
  Individuals who were `Removed' were removed from the unit for reasons that are generally related to repeated misconduct.
  Individuals who `Refused Treatment' refused to move to the Little Scandinavia Unit upon treatment notification and did not spend time on the unit.") %>% 
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
enrollment <- randassign 

# -- -- -- Prepare table A: Integrer rows, count enrolled per cohort and treatment group 
enrollment_count <- enrollment %>% 
  group_by(rct_treat_wave, rct) %>%
  summarise(
    enrolled = n()
  ) %>% 
  pivot_wider(names_from = "rct",
              values_from = "enrolled") %>% 
  rename(control = `0`, treatment = `1`) %>% # Bad practice!
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
# NEVER ROUND until the very end - This leads to funny results, like 2 released observations accounting for 0.00 percent..
# Change this here and elsewhere 
enrollment_released_pct <- enrollment_released_count %>%
  mutate(
    pct_treatment = round(sum(treatment, na.rm=T) / grand_total_released * 100, 2),
    pct_control   = round(sum(control, na.rm=T) / grand_total_released * 100, 2),
    pct_total     = round(sum(total, na.rm=T) / grand_total_released * 100, 2)
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
# -- Base Data  ####
# Released individuals only for these tables
time_spent <- randassign %>%
  mutate(released = ifelse(is.na(rel_rct), 0, 1)) %>% 
  filter(released == 1)

# -- By Cohort ####
# -- -- Average Time Spent 
time_spent_mean_by_cohort <- time_spent %>% 
  complete(rct_treat_wave, rct) %>%  # ensure waves with no released individuals do not get dropped 
  group_by(rct_treat_wave, rct) %>%
  mutate(released = sum(!is.na(research_id)), # Number of released individuals. Not using n() as this counts one row for wave 7, where no one was released
         mean_rct_mnths_to_exit = round(mean(rct_mnths_to_exit, na.rm=T),1),
         mean_rct_mnths_to_release = round(mean(rct_mnths_to_release, na.rm=T),1),
         mean_rct_mnths_since_release = round(mean(rct_mnths_since_release, na.rm=T),1), 
         .groups = "drop") %>%
  ungroup() %>%
  select(rct_treat_wave, rct, released, mean_rct_mnths_to_exit, mean_rct_mnths_to_release, mean_rct_mnths_since_release, rct_treat_dt) %>%
  distinct() 

time_spent_mean_overall <- time_spent %>% 
  group_by(rct) %>%
  mutate(released = n(), # Number of released individuals
         mean_rct_mnths_to_exit = round(mean(rct_mnths_to_exit, na.rm=T),1),
         mean_rct_mnths_to_release = round(mean(rct_mnths_to_release, na.rm=T),1),
         mean_rct_mnths_since_release = round(mean(rct_mnths_since_release, na.rm=T),1), 
         .groups = "drop") %>%
  ungroup() %>%
  mutate(rct_treat_wave = "Average") %>%
  select(rct_treat_wave, rct, released, mean_rct_mnths_to_exit, mean_rct_mnths_to_release, mean_rct_mnths_since_release, rct_treat_dt) %>%
  distinct(rct, released, .keep_all = TRUE) # We want to drop the distinct rct_treat_dates here 

tab_time_spent_cohort_and_overall <- rbind(time_spent_mean_by_cohort,
                                           time_spent_mean_overall)

# Turn this into a table with Treated and Control groups side by side 
tab_time_spent_side_by_side <- cbind(tab_time_spent_cohort_and_overall %>% 
                                       filter(rct==1) %>%
                                       select(-rct), # We don't need a column with treatment status 
                                     tab_time_spent_cohort_and_overall %>% 
                                       filter(rct==0) %>%
                                       select(-rct_treat_wave,-rct, -mean_rct_mnths_to_exit) %>% # We only need the rct_treat_wave column once, controls don't have months to exit
                                       rename_with(~ paste0(., "_control"), .cols = everything())) # To avoid duplicate names 

# Formatting 
# Purposfully doing this before we move dataframes side by side
tab_time_spent_formatted <- tab_time_spent_side_by_side %>%
  mutate(across(everything(), ~ ifelse(str_detect(as.character(.), "NaN"), "-", .))) %>%
  # We don't want an average of the number of people released
  mutate(released = ifelse(rct_treat_wave == "Average", "-", released)) %>% 
  mutate(released_control = ifelse(rct_treat_wave == "Average", "-", released_control)) %>% 
  # Correctly name cohort columns in format Cohort X (yyyy-mm-dd)
  mutate(rct_treat_dt = ifelse(is.na(rct_treat_dt), rct_treat_dt_control, rct_treat_dt)) %>% # To avoid NA dates in rows with zero observations
  mutate(rct_treat_wave = paste0("Cohort ", rct_treat_wave, " (", as.Date(rct_treat_dt), ")")) %>%
  mutate(rct_treat_wave = ifelse(grepl("average", rct_treat_wave, ignore.case = TRUE), "Average", rct_treat_wave)) %>%
  rename(cohort = rct_treat_wave) %>%
  select(-rct_treat_dt, -rct_treat_dt_control) 

# -- -- Table ####
file_name <- "tabx_treatment_duration"
tab_time_spent_formatted %>%
  kbl(caption = "Treatment Duration in Months, by Cohort",
      align = c("lrrrrrrr"),
      label = file_name,
      format.args = list(big.mark = ","),
      booktabs = T,
      col.names = c("Cohort",
                    "# Released", 
                    "Months to Unit Exit", 
                    "Months to Release",
                    "Months Since Release",
                    "# Released", 
                    "Months to Release",
                    "Months Since Release"),
      linesep = "",
      row.names = F,
      format = "latex") %>%
  kable_styling(latex_options = "hold_position") %>%
  kable_styling(font_size = 7) %>%
  add_header_above(c(" " = 1, "Treated" = 4, "Control" = 3)) %>%
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>%
  row_spec(c(nrow(tab_time_spent_formatted)-1),
           hline_after=TRUE,
           extra_css = "border-bottom: 1px solid") %>%
  footnote(threeparttable = T,
           paste0("This table contains the average number of months between randomization and release, as well as the time between release and the date until data was available (", report_date, "). 
                  For treated individuals, the table also contains the time between randomization and exit from the Little Scandinavia Unit, as some individuals leave the unit prior to release (see Treatment Compliance Table). 
                  The table is split by treatment cohort and treatment group. Dashes inidicate no data.")) %>% 
  landscape() %>%
  save_kable(file = paste0("output/tables/", file_name, ".tex"),
             self_contained = T,density = 200) %>%
  save_kable(file = paste0("C:/Users/britt/Dropbox/Apps/Overleaf/SPP/tables_arnold_report/", file_name, ".tex"),
             self_contained = T,density = 200)

# -- By Stratum ####
# -- -- Average Time Spent 
time_spent_mean_by_stratum <- time_spent %>% 
  complete(rct_stratum, rct) %>%  # ensure waves with no released individuals do not get dropped 
  group_by(rct_stratum, rct) %>%
  mutate(released = sum(!is.na(research_id)), # Number of released individuals. Not using n() as this counts one row even if there are no observations, because of complete()
         mean_rct_mnths_to_exit = round(mean(rct_mnths_to_exit, na.rm=T),1),
         mean_rct_mnths_to_release = round(mean(rct_mnths_to_release, na.rm=T),1),
         mean_rct_mnths_since_release = round(mean(rct_mnths_since_release, na.rm=T),1), 
         .groups = "drop") %>%
  ungroup() %>%
  select(rct_stratum, rct, released, mean_rct_mnths_to_exit, mean_rct_mnths_to_release, mean_rct_mnths_since_release, rct_treat_dt) %>%
  distinct(rct, released, .keep_all = TRUE) # We don't care about the distinct rct_treat_dates here 

time_spent_mean_by_stratum_overall <- time_spent %>% 
  group_by(rct) %>%
  mutate(released = n(), # Number of released individuals
         mean_rct_mnths_to_exit = round(mean(rct_mnths_to_exit, na.rm=T),1),
         mean_rct_mnths_to_release = round(mean(rct_mnths_to_release, na.rm=T),1),
         mean_rct_mnths_since_release = round(mean(rct_mnths_since_release, na.rm=T),1), 
         .groups = "drop") %>%
  ungroup() %>%
  mutate(rct_stratum = "Average") %>%
  select(rct_stratum, rct, released, mean_rct_mnths_to_exit, mean_rct_mnths_to_release, mean_rct_mnths_since_release, rct_treat_dt) %>%
  distinct(rct, released, .keep_all = TRUE) # We don't care about the distinct rct_treat_dates here 

tab_time_spent_stratum_and_overall <- rbind(time_spent_mean_by_stratum,
                                            time_spent_mean_by_stratum_overall)

# Turn this into a table with Treated and Control groups side by side 
tab_time_spent_side_by_side <- cbind(tab_time_spent_stratum_and_overall %>% 
                                       filter(rct==1) %>%
                                       select(-rct), # We don't need a column with treatment status 
                                     tab_time_spent_stratum_and_overall %>% 
                                       filter(rct==0) %>%
                                       select(-rct_stratum,-rct, -mean_rct_mnths_to_exit) %>% # We only need the rct_treat_wave column once, controls don't have months to exit
                                       rename_with(~ paste0(., "_control"), .cols = everything())) # To avoid duplicate names 


# Formatting 
# Purposfully doing this before we move dataframes side by side
tab_time_spent_formatted <- tab_time_spent_side_by_side %>%
  mutate(across(everything(), ~ ifelse(str_detect(as.character(.), "NaN"), "-", .))) %>% # FIX this and knock on rows - this sets rct_stratum to 1,2,3,4,5..
  mutate(rct_treat_dt = ifelse(is.na(rct_treat_dt), rct_treat_dt_control, rct_treat_dt)) %>% # To avoid NA dates in rows with zero observations
  # We don't want an average of the number of people released
  mutate(released = ifelse(rct_stratum == "5", "-", released)) %>% 
  mutate(released_control = ifelse(rct_stratum == "5", "-", released_control)) %>% 
  select(-rct_treat_dt, -rct_treat_dt_control) %>%
  # Jonas - this is a temporary fix. This shouldn't be happening.. I think a result of conversion to character above + your use of factor labels. 
  mutate(rct_stratum = case_when(
    rct_stratum == "1" ~ "0–6 Months",
    rct_stratum == "2" ~ "12–60 Months",
    rct_stratum == "3" ~ "12–60 Months",
    rct_stratum == "4" ~ "60+ Months",
    rct_stratum == "5" ~ "Average"))

# -- -- Table ####
file_name <- "tabx_treatment_duration_stratum"
tab_time_spent_formatted %>%
  kbl(caption = "Treatment Duration in Months, by Stratum",
      align = c("lrrrrrrr"),
      label = file_name,
      format.args = list(big.mark = ","),
      booktabs = T,
      col.names = c("Cohort",
                    "# Released", 
                    "Months to Unit Exit", 
                    "Months to Release",
                    "Months Since Release",
                    "# Released", 
                    "Months to Release",
                    "Months Since Release"),
      linesep = "",
      row.names = F,
      format = "latex") %>%
  kable_styling(latex_options = "hold_position") %>%
  kable_styling(font_size = 7) %>%
  add_header_above(c(" " = 1, "Treated" = 4, "Control" = 3)) %>%
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>%
  row_spec(c(nrow(tab_time_spent_formatted)-1),
           hline_after=TRUE,
           extra_css = "border-bottom: 1px solid") %>%
  footnote(threeparttable = T,
           paste0("This table contains the average number of months between randomization and release, as well as the time between release and the date until data was available (", report_date, "). 
                  For treated individuals, the table also contains the time between randomization and exit from the Little Scandinavia Unit, as some individuals leave the unit prior to release (see treatment compliance table). 
                  The table is split by stratum and treatment group. Dashes inidicate no data.")) %>% 
  landscape() %>%
  save_kable(file = paste0("output/tables/", file_name, ".tex"),
             self_contained = T,density = 200) %>%
  save_kable(file = paste0("C:/Users/britt/Dropbox/Apps/Overleaf/SPP/tables_arnold_report/", file_name, ".tex"),
             self_contained = T,density = 200)
