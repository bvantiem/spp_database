# ======================================================================= ####
# Notes to Script #### # didn't know what exactly to write in these places
# Notes to Script: ####
# -- Objective ####
# -- Readme ####
# -- To do ####
# ======================================================================= ####
# Set Up ####
# -- Prepare Environment ####
rm(list=ls())
# -- Functions #### 
`%ni%` = Negate(`%in%`) 
# -- Set Seed ####
set.seed(1962)
# -- Read in Data ####
unit_mapping <- read.csv("data/raw/2_data_keys/unit_mapping.csv")
pcq_lookup <- read_xlsx("data/raw/5_pcq_survey_questions/250605_pcq_survey_questions_PA.xlsx")
# ======================================================================= ####
# Unit Mapping ####
# -- INF is not included due to small sample of this unit
unit_mapping <- unit_mapping %>%
  mutate(across(starts_with("unit_type_wave"),
                ~ case_when(
                  . == "rhu" ~ "1. Restrictive Housing",
                  . == "gp" ~ "2. General Population",
                  . == "gp-tc" ~ "3. Therapeutic Community", 
                  . == "rec" ~ "4. Recovery Unit",
                  . == "hons" ~ "5. Honor Block",
                  . == "gp-epu" ~ "5. Enhanced Privilege Unit",
                  . == "gp-senior" ~ "5. Senior Unit",
                  . == "ls" ~ "6. Little Scandinavia",
                  TRUE ~ NA_character_
                ),
                .names = "unit_type_named_{col}" # Creating new columns with names like unit_type_named_wave1, etc.
  ))

unit_mapping$unit_type_nols <- with(unit_mapping, ifelse(unit %in% c("aa", "ab", "ac", "ad", "eb", "da", "db", "ca"), "gp",
                                                         ifelse(unit %in% c("bb", "bc", "bd"), "gp-tc",
                                                                ifelse(unit=="ba", "rec",
                                                                       ifelse(unit=="cb", "hons",
                                                                              ifelse(unit=="ea", "thu",
                                                                                     ifelse(unit %in% c("rhu","fa"), "rhu",
                                                                                            ifelse(unit %in% c("inf","ma"), "inf",NA))))))))

# ======================================================================= ####
# Pcq_lookup ####

pcq_lookup <- pcq_lookup %>%
  mutate(question_qno = paste0("q", question_no), .after = question_no)
pcq_lookup <- as.data.frame(pcq_lookup)

# -- For RCT Longitudinal paper ####
scale.key.longitudinal <- list(prisoners = c("q10", "q11", "q12", "q13", "q14") ,
                  staff = c("q15", "q16", "q17", "q18", "q19", "q20", "q21", "q22"), # includes procedure
                  safety = c("q26", "q29", "q30", "q31", "q32"),
                  visits = c("q139","q140", "q141", "q142", "q143", "q144"), # exclude  "q145", "q146"
                  contact = c("q135", "q136", "q137"),
                  sleep = c("q94", "q95", "q96"),
                  care = c("q111", "q112", "q119", "q120", "q121", "q122"),
                  shop = c("q99", "q100", "q101"),
                  complaints = c("q24", "q25"),
                  actsat = c("q62", "q63", "q64", "q65", "q66", "q67", "q68"),
                  actav = c("q69", "q70", "q71", "q72"),
                  reint = c("q73", "q74", "q75", "q76") ,
                  autonomy = c("q82", "q83", "q84", "q85"))

scale_names <- data.frame(scale_abbreviation = names(scale.key.longitudinal),
                          scale_theory_long = c("1. Prisoner relationships",
                                                "2. Staff-prisoner relationships and Procedural Justice",
                                                "3. Safety",
                                                "4. Satisfaction with Visits",
                                                "5. Satisfaction With Frequency of Contact",
                                                "6. Sleep Quality",
                                                "7. Quality of care",
                                                "8. Shop Quality",
                                                "9. Settlement of Complaints",
                                                "10. Satisfaction with Activities",
                                                "11. Availability of Meaningful Activities",
                                                "12. Reintegration",
                                                "13. Autonomy"))

experience.key <- list(actsat = c("q55", "q56", "q57", "q58", "q59", "q60", "q61"),
                       visits = c("q133", "q134", "q138"),
                       care = c("q115", "q116", "q117", "q118"),
                       safety = c("q43", "q44", "q45", "q46", "q47"),
                       shop = c("q107", "q108"),
                       discrimination = c("q147", "q148"))

service_use_qs <- c("q55","q56","q57","q58","q59","q61","q115","q116","q117","q118","q138" ,"q167", "q162", "q163")


# ======================================================================= ####
# Wave Dates ####
# -- dates each wave occurred to ensure standardization
wave1_date = lubridate::ymd(20220501)
wave2_date = lubridate::ymd(20221115)
wave3_date = lubridate::ymd(20230520)
wave4_date = lubridate::ymd(20231128)
wave5_date = lubridate::ymd(20240606)
wave6_date = lubridate::ymd(20241022)
# ======================================================================= #### 
# Prison Lookup Table ####
# -- table for transforming the abr facility to full facility name. 
prison_lookup <- tribble(
  ~pris_loc,     ~pris_loc_full,
  "ALB",          "Albion",
  "BEN",          "Benner Township",
  "CAM",          "Cambridge Springs",
  "CHS",          "Chester",
  "COA",          "Coal Township",
  "DAL",         "Dallas",
  "FRA",          "Frackville",
  "FYT",          "Fayette",
  "FRS",         "Forest",
  "GRN",          "Greene",
  "HOU",          "Houtzdale",
  "HUN",          "Huntingdon",
  "LAU",          "Laurel Highlands",
  "MAH",          "Mahanoy",
  "MER",          "Mercer",
  "MUN",          "Muncy",
  "PHX",          "Phoenix",
  "PIT",          "Pittsburgh",
  "QUE",          "Quehanna Boot Camp",
  "RET",          "Retreat",
  "ROC",          "Rockview",
  "SMI",          "Smithfield",
  "SMR",          "Somerset",
  "WAM",          "Waymart",
  "GRA",          "Graterford",
  "GRE",          "Greensburg",
  "CRE",          "Cresson",
  "102",          "Philadelphia CCC Center #2",
  "104",          "Philadelphia CCC Center #4",
  "201",          "Scranton Center",
  "319",          "Riverside Center"
) 
# ======================================================================= ####