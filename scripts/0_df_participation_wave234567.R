rm(list=ls())
library(openxlsx)
library(readxl)
library(dplyr)
library("excel.link") # For password protected files
`%ni%` = Negate(`%in%`)
source("scripts/0_id_masking_function.R")
source("scripts/0_utils.R")

############ Load data ####
# Callsheets wave 2 ####
aa1 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_aa_kw.xlsx", skip=0, sheet=1)
ab1 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_ab_kw.xlsx", skip=0, sheet=1)
ac1 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_ac_kw.xlsx", skip=0, sheet=1)
ad1 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_ad_kw.xlsx", skip=0, sheet=1)
ba1 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_ba_cm.xlsx", skip=0, sheet=1)
bb1 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_bb_kw.xlsx", skip=0, sheet=1)
bc1 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_bc_cm.xlsx", skip=0, sheet=1)
bd1 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_bd_cm.xlsx", skip=0, sheet=1)
ca1 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_ca_kw.xlsx", skip=0, sheet=1)
cb1 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_cb_cm.xlsx", skip=0, sheet=1)
da1 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_da_cm.xlsx", skip=0, sheet=1)
db1 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_db_cm.xlsx", skip=0, sheet=1)
ea1 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_ea_cm.xlsx", skip=0, sheet=1)
eb1 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_eb_kw.xlsx", skip=0, sheet=1)
inf1 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_inf_kw.xlsx", skip=0, sheet=1)
rhu1 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave2_participation/pcq_participation_wave2_rhu_kw.xlsx", skip=0, sheet=1)

wave2_callsheets <- rbind(aa1, ab1, ac1, ad1,
                    ba1, bb1, bc1, bd1,
                    ca1, cb1,
                    da1, db1,
                    ea1, eb1,
                    inf1, rhu1)

names(wave2_callsheets)[which(names(wave2_callsheets)=="inmate_id")] <- "original_id"
wave2_callsheets$original_id <- tolower(wave2_callsheets$original_id)
i <- unique(wave2_callsheets$original_id)
id.link <- mask_ids(i) # Generate masked IDs
wave2_callsheets <- left_join(wave2_callsheets, id.link, by="original_id") # Merge wave2_callsheets on original ID
wave2_callsheets <- wave2_callsheets[,-which(names(wave2_callsheets)=="original_id")] # delete original ID
wave2_callsheets <- wave2_callsheets[,c(ncol(wave2_callsheets),1:ncol(wave2_callsheets)-1)] # reorder columns

# save(wave2_callsheets, file = "data/processed/wave2_callsheets.Rda")

# Callsheets wave 3 ####
aa3 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_aa_kw.xlsx", skip=0, sheet=1)
ab3 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_ab_kw.xlsx", skip=0, sheet=1)
ac3 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_ac_kw.xlsx", skip=0, sheet=1)
ad3 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_ad_kw.xlsx", skip=0, sheet=1)
ba3 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_ba_kw.xlsx", skip=0, sheet=1)
bb3 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_bb_kw.xlsx", skip=0, sheet=1)
bc3 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_bc_kw.xlsx", skip=0, sheet=1)
bd3 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_bd_kw.xlsx", skip=0, sheet=1)
ca3 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_ca_kw.xlsx", skip=0, sheet=1)
cb3 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_cb_kw.xlsx", skip=0, sheet=1)
da3 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_da_kw.xlsx", skip=0, sheet=1)
db3 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_db_cm.xlsx", skip=0, sheet=1)
ea3 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_ea_cm.xlsx", skip=0, sheet=1)
eb3 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_eb_cm.xlsx", skip=0, sheet=1)
inf3 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_inf_cm.xlsx", skip=0, sheet=1)
rhu3 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_rhu_cm.xlsx", skip=0, sheet=1)

wave3_callsheets <- rbind(aa3, ab3, ac3, ad3,
                          ba3, bb3, bc3, bd3,
                          ca3, cb3,
                          da3, db3,
                          ea3, eb3,
                          inf3, rhu3)


names(wave3_callsheets)[which(names(wave3_callsheets)=="inmate_id")] <- "original_id"
wave3_callsheets$original_id <- tolower(wave3_callsheets$original_id)
i <- unique(wave3_callsheets$original_id)
id.link <- mask_ids(i) # Generate masked IDs
wave3_callsheets <- left_join(wave3_callsheets, id.link, by="original_id") # Merge wave3_callsheets on original ID
wave3_callsheets <- wave3_callsheets[,c(ncol(wave3_callsheets),1:ncol(wave3_callsheets)-1)] # reorder columns
names(wave3_callsheets)[which(names(wave3_callsheets)=="cell")] <- "location"
# wave3_callsheets <- wave3_callsheets[,c("research_id", "unit", "location")]
# save(wave3_callsheets, file = "data/processed/wave3_callsheets.Rda")

# Participation wave 3 ####
aa3p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_aa_kw.xlsx", skip=0, sheet=2)
aa3p$unit <- "aa"
aa3p <- aa3p[,-which(names(aa3p)=="notes")]
ab3p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_ab_kw.xlsx", skip=0, sheet=2)
ab3p$unit <- "ab"
ac3p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_ac_kw.xlsx", skip=0, sheet=2)
ac3p$unit <- "ac"
ad3p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_ad_kw.xlsx", skip=0, sheet=2)
ad3p$unit <- "ad"
ba3p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_ba_kw.xlsx", skip=0, sheet=2)
ba3p$unit <- "ba"
bb3p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_bb_kw.xlsx", skip=0, sheet=2)
bb3p$unit <- "bb"
bc3p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_bc_kw.xlsx", skip=0, sheet=2)
bc3p$unit <- "bc"
bc3p <- bc3p[,-which(names(bc3p)=="notes")]
bd3p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_bd_kw.xlsx", skip=0, sheet=2)
bd3p$unit <- "bd"
bd3p <- bd3p[,-which(names(bd3p)=="notes")]
ca3p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_ca_kw.xlsx", skip=0, sheet=2)
ca3p$unit <- "ca"
ca3p <- ca3p[,-which(names(ca3p)=="notes")]
cb3p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_cb_kw.xlsx", skip=0, sheet=2)
cb3p$unit <- "cb"
da3p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_da_kw.xlsx", skip=0, sheet=2)
da3p$unit <- "da"
da3p <- da3p[,-which(names(da3p)=="notes")]
db3p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_db_cm.xlsx", skip=0, sheet=2)
db3p$unit <- "db"
db3p <- db3p[,-which(names(db3p)=="notes")]
ea3p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_ea_cm.xlsx", skip=0, sheet=2)
ea3p$unit <- "ea"
ea3p <- ea3p[,-which(names(ea3p)=="notes")]
eb3p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_eb_cm.xlsx", skip=0, sheet=2)
eb3p$unit <- "eb"
eb3p <- eb3p[,-which(names(eb3p)=="notes")]
inf3p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_inf_cm.xlsx", skip=0, sheet=2)
inf3p$unit <- "inf"
rhu3p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave3_participation/pcq_participation_wave3_rhu_cm.xlsx", skip=0, sheet=2)
rhu3p$unit <- "rhu"

wave3_participation <- rbind(aa3p, ab3p, ac3p, ad3p,
                             ba3p, bb3p, bc3p, bd3p,
                             ca3p, cb3p,
                             da3p, db3p,
                             ea3p, eb3p,
                             inf3p, rhu3p)

names(wave3_participation)[which(names(wave3_participation)=="inmate_id")] <- "original_id"
wave3_participation$original_id <- tolower(wave3_participation$original_id)
i <- unique(wave3_participation$original_id)
id.link <- mask_ids(i) # Generate masked IDs
wave3_participation <- left_join(wave3_participation, id.link, by="original_id") # Merge wave3_participation on original ID
wave3_participation$completed_survey <- 2
wave3_payment <- wave3_participation %>% group_by(original_id) %>% summarise(to_pay = sum(completed_survey))

# wave3_participation <- wave3_participation[,c("research_id", "unit")]
# save(wave3_participation, file = "data/processed/wave3_participation.Rda")

# Participation wave 4 ####
aa4p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_aa_oc.xlsx", skip=0, sheet=2)
aa4p$unit <- "aa"
ab4p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_ab_sc.xlsx", skip=0, sheet=2)
ab4p$unit <- "ab"
ac4p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_ac_oc.xlsx", skip=0, sheet=2)
ac4p$unit <- "ac"
ad4p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_ad_sc.xlsx", skip=0, sheet=2)
ad4p$unit <- "ad"
ba4p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_ba_oc.xlsx", skip=0, sheet=2)
ba4p$unit <- "ba"
bb4p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_bb_oc.xlsx", skip=0, sheet=2)
bb4p$unit <- "bb"
bc4p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_bc_sjc.xlsx", skip=0, sheet=2)
bc4p$unit <- "bc"
bd4p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_bd_sjc.xlsx", skip=0, sheet=2)
bd4p$unit <- "bd"
ca4p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_ca_sjc.xlsx", skip=0, sheet=2)
ca4p$unit <- "ca"
cb4p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_cb_sjc.xlsx", skip=0, sheet=2)
cb4p$unit <- "cb"
da4p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_da_sjc.xlsx", skip=0, sheet=2)
da4p$unit <- "da"
db4p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_db_sjc.xlsx", skip=0, sheet=2)
db4p$unit <- "db"
ea4p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_ea_sjc.xlsx", skip=0, sheet=2)
ea4p$unit <- "ea"
eb4p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_eb_oc.xlsx", skip=0, sheet=2)
eb4p$unit <- "eb"
inf4p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_inf_oc.xlsx", skip=0, sheet=2)
inf4p$unit <- "inf"
rhu4p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_rhu_oc.xlsx", skip=0, sheet=2)
rhu4p$unit <- "rhu"
other4p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_other_oc.xlsx", skip=0, sheet=2)
other4p$unit <- "other"

wave4_participation <- rbind(aa4p, ab4p, ac4p, ad4p,
                             ba4p, bb4p, bc4p, bd4p,
                             ca4p, cb4p,
                             da4p, db4p,
                             ea4p, eb4p,
                             inf4p, rhu4p, other4p)

names(wave4_participation)[which(names(wave4_participation)=="inmate_id")] <- "original_id"
wave4_participation$original_id <- tolower(wave4_participation$original_id)
i <- unique(wave4_participation$original_id)
id.link <- mask_ids(i) # Generate masked IDs
wave4_participation <- left_join(wave4_participation, id.link, by="original_id") # Merge wave3_participation on original ID
wave4_participation$completed_survey <- 2
wave4_payment <- wave4_participation %>% group_by(original_id) %>% summarise(to_pay = sum(completed_survey))

wave4_participation <- wave4_participation[,c("research_id", "unit")]
save(wave4_participation, file = "data/processed/wave4_participation.Rda")

# Save payment file
write.xlsx(wave4_payment, file = "data/processed/wave4_payment.xlsx")

# Callsheets wave 4 ####
aa4 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_aa_oc.xlsx", skip=0, sheet=1)
ab4 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_ab_sc.xlsx", skip=0, sheet=1)
ac4 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_ac_oc.xlsx", skip=0, sheet=1)
ad4 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_ad_sc.xlsx", skip=0, sheet=1)
ba4 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_ba_oc.xlsx", skip=0, sheet=1)
bb4 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_bb_oc.xlsx", skip=0, sheet=1)
bc4 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_bc_sjc.xlsx", skip=0, sheet=1)
bd4 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_bd_sjc.xlsx", skip=0, sheet=1)
ca4 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_ca_sjc.xlsx", skip=0, sheet=1)
cb4 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_cb_sjc.xlsx", skip=0, sheet=1)
da4 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_da_sjc.xlsx", skip=0, sheet=1)
db4 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_db_sjc.xlsx", skip=0, sheet=1)
ea4 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_ea_sjc.xlsx", skip=0, sheet=1)
eb4 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_eb_oc.xlsx", skip=0, sheet=1)
inf4 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_inf_oc.xlsx", skip=0, sheet=1)
rhu4 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_rhu_oc.xlsx", skip=0, sheet=1)
other4 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave4_participation/pcq_participation_wave4_other_oc.xlsx", skip=0, sheet=1)


wave4_callsheets <- rbind(aa4, ab4, ac4, ad4,
                          ba4, bb4, bc4, bd4,
                          ca4, cb4,
                          da4, db4,
                          ea4, eb4,
                          inf4, rhu4, other4)

names(wave4_callsheets)[which(names(wave4_callsheets)=="inmate_id")] <- "original_id"
wave4_callsheets$original_id <- tolower(wave4_callsheets$original_id)
i <- unique(wave4_callsheets$original_id)
id.link <- mask_ids(i) # Generate masked IDs
wave4_callsheets <- left_join(wave4_callsheets, id.link, by="original_id") # Merge wave3_callsheets on original ID
wave4_callsheets <- wave4_callsheets[,c(ncol(wave4_callsheets),1:ncol(wave4_callsheets)-1)] # reorder columns
names(wave4_callsheets)[which(names(wave4_callsheets)=="cell")] <- "location"

wave4_callsheets <- wave4_callsheets[,c("research_id", "unit", "location")]
save(wave4_callsheets, file = "data/processed/wave4_callsheets.Rda")

# Participation by unit
wave4_callsheets$survey <- as.numeric(wave4_callsheets$research_id %in% wave4_participation$research_id)

wave4_callsheets %>% group_by(unit) %>%
  summarize(nunit = n(),
          nsurvey = sum(survey)) %>%
  mutate(rr = nsurvey/nunit) %>%
  select(unit, rr)

# Participation wave 5 ####
aa5p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_aa_kw.xlsx", skip=0, sheet=2)
aa5p$unit <- "aa"
ab5p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_ab_kw.xlsx", skip=0, sheet=2)
ab5p$unit <- "ab"
ac5p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_ac_kw.xlsx", skip=0, sheet=2)
ac5p$unit <- "ac"
ad5p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_ad_kw.xlsx", skip=0, sheet=2)
ad5p$unit <- "ad"
ba5p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_ba_kw.xlsx", skip=0, sheet=2)
ba5p$unit <- "ba"
bb5p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_bb_kw.xlsx", skip=0, sheet=2)
bb5p$unit <- "bb"
bc5p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_bc_kw.xlsx", skip=0, sheet=2)
bc5p$unit <- "bc"
bd5p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_bd_kw.xlsx", skip=0, sheet=2)
bd5p$unit <- "bd"
ca5p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_ca_kw.xlsx", skip=0, sheet=2)
ca5p$unit <- "ca"
cb5p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_cb_kw.xlsx", skip=0, sheet=2)
cb5p$unit <- "cb"
da5p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_da_kw.xlsx", skip=0, sheet=2)
da5p$unit <- "da"
db5p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_db_kw.xlsx", skip=0, sheet=2)
db5p$unit <- "db"
ea5p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_ea_kw.xlsx", skip=0, sheet=2)
ea5p$unit <- "ea"
eb5p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_eb_kw.xlsx", skip=0, sheet=2)
eb5p$unit <- "eb"
inf5p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_inf_kw.xlsx", skip=0, sheet=2)
inf5p$unit <- "inf"
rhu5p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_rhu_kw.xlsx", skip=0, sheet=2)
rhu5p$unit <- "rhu"

wave5_participation <- rbind(aa5p, ab5p, ac5p, ad5p,
                             ba5p, bb5p, bc5p, bd5p,
                             ca5p, cb5p,
                             da5p, db5p,
                             ea5p, eb5p,
                             inf5p, rhu5p)

names(wave5_participation)[which(names(wave5_participation)=="inmate_id")] <- "original_id"
wave5_participation$original_id <- tolower(wave5_participation$original_id)
i <- unique(wave5_participation$original_id)
id.link <- mask_ids(i) # Generate masked IDs
wave5_participation <- left_join(wave5_participation, id.link, by="original_id") # Merge wave3_participation on original ID
wave5_participation$completed_survey <- 2
wave5_payment <- wave5_participation %>% group_by(original_id) %>% summarise(to_pay = sum(completed_survey))

wave5_participation <- wave5_participation[,c("research_id", "unit")]
save(wave5_participation, file = "data/processed/wave5_participation.Rda")

# Save payment file
write.xlsx(wave5_payment, file = "data/processed/wave5_payment.xlsx")

# Callsheets wave 5 ####
aa5 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_aa_kw.xlsx", skip=0, sheet=1)
ab5 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_ab_kw.xlsx", skip=0, sheet=1)
ac5 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_ac_kw.xlsx", skip=0, sheet=1)
ad5 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_ad_kw.xlsx", skip=0, sheet=1)
ba5 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_ba_kw.xlsx", skip=0, sheet=1)
bb5 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_bb_kw.xlsx", skip=0, sheet=1)
bc5 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_bc_kw.xlsx", skip=0, sheet=1)
bd5 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_bd_kw.xlsx", skip=0, sheet=1)
ca5 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_ca_kw.xlsx", skip=0, sheet=1)
cb5 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_cb_kw.xlsx", skip=0, sheet=1)
da5 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_da_kw.xlsx", skip=0, sheet=1)
db5 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_db_kw.xlsx", skip=0, sheet=1)
ea5 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_ea_kw.xlsx", skip=0, sheet=1)
eb5 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_eb_kw.xlsx", skip=0, sheet=1)
inf5 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_inf_kw.xlsx", skip=0, sheet=1)
rhu5 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave5_participation/pcq_participation_wave5_rhu_kw.xlsx", skip=0, sheet=1)

wave5_callsheets <- rbind(aa5, ab5, ac5, ad5,
                          ba5, bb5, bc5, bd5,
                          ca5, cb5,
                          da5, db5,
                          ea5, eb5,
                          inf5, rhu5)

names(wave5_callsheets)[which(names(wave5_callsheets)=="inmate_id")] <- "original_id"
wave5_callsheets$original_id <- tolower(wave5_callsheets$original_id)
i <- unique(wave5_callsheets$original_id)
id.link <- mask_ids(i) # Generate masked IDs
wave5_callsheets <- left_join(wave5_callsheets, id.link, by="original_id") # Merge wave3_callsheets on original ID
wave5_callsheets <- wave5_callsheets[,c(ncol(wave5_callsheets),1:ncol(wave5_callsheets)-1)] # reorder columns
names(wave5_callsheets)[which(names(wave5_callsheets)=="cell")] <- "location"

wave5_callsheets <- wave5_callsheets[,c("research_id", "unit", "location")]
save(wave5_callsheets, file = "data/processed/wave5_callsheets.Rda")

# Participation by unit
wave5_callsheets$survey <- as.numeric(wave5_callsheets$research_id %in% wave5_participation$research_id)

wave5_callsheets %>% group_by(unit) %>%
  summarize(nunit = n(),
            nsurvey = sum(survey)) %>%
  mutate(rr = nsurvey/nunit) %>%
  select(unit, rr)



# Participation wave 6 ####
# Unit BC was closed for this wave
aa6p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_aa_bs.xlsx", skip=0, sheet=2)
aa6p$unit <- "aa"
ab6p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_ab_bs.xlsx", skip=0, sheet=2)
ab6p$unit <- "ab"
ac6p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_ac_gc.xlsx", skip=0, sheet=2)
ac6p$unit <- "ac"
ad6p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_ad_gc.xlsx", skip=0, sheet=2)
ad6p$unit <- "ad"
ba6p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_ba_gc.xlsx", skip=0, sheet=2)
ba6p$unit <- "ba"
bb6p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_bb_bs.xlsx", skip=0, sheet=2)
bb6p$unit <- "bb"
# Unit bc closed
bd6p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_bd_gc.xlsx", skip=0, sheet=2)
bd6p$unit <- "bd"
ca6p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_ca_bs.xlsx", skip=0, sheet=2)
ca6p$unit <- "ca"
cb6p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_cb_gc.xlsx", skip=0, sheet=2)
cb6p$unit <- "cb"
da6p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_da_bs.xlsx", skip=0, sheet=2)
da6p$unit <- "da"
db6p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_db_gc.xlsx", skip=0, sheet=2)
db6p$unit <- "db"
ea6p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_ea_bs.xlsx", skip=0, sheet=2)
ea6p$unit <- "ea"
eb6p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_eb_gc.xlsx", skip=0, sheet=2)
eb6p$unit <- "eb"
inf6p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_inf_gc.xlsx", skip=0, sheet=2)
inf6p$unit <- "inf"
rhu6p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_rhu_gc.xlsx", skip=0, sheet=2)
rhu6p$unit <- "rhu"

wave6_participation <- rbind(aa6p, ab6p, ac6p,
                             ad6p[,c("inmate_id", "unit")],
                             ba6p, bb6p, bd6p,
                             ca6p, cb6p,
                             da6p, db6p,
                             ea6p, eb6p,
                             inf6p, rhu6p) #bc closed

names(wave6_participation)[which(names(wave6_participation)=="inmate_id")] <- "original_id"
wave6_participation$original_id <- tolower(wave6_participation$original_id)
i <- unique(wave6_participation$original_id)
id.link <- mask_ids(i) # Generate masked IDs
wave6_participation <- left_join(wave6_participation, id.link, by="original_id") # Merge wave3_participation on original ID
wave6_participation$completed_survey <- 2
wave6_payment <- wave6_participation %>% group_by(original_id) %>% summarise(to_pay = sum(completed_survey))

wave6_participation <- wave6_participation[,c("research_id", "unit")]
save(wave6_participation, file = "data/processed/wave6_participation.Rda")

# Save payment file
write.xlsx(wave6_payment, file = "data/processed/wave6_payment.xlsx")

# Callsheets wave 6 ####
aa6 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_aa_bs.xlsx", skip=0, sheet=1)
ab6 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_ab_bs.xlsx", skip=0, sheet=1)
ac6 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_ac_gc.xlsx", skip=0, sheet=1)
ad6 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_ad_gc.xlsx", skip=0, sheet=1)
ba6 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_ba_gc.xlsx", skip=0, sheet=1)
bb6 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_bb_bs.xlsx", skip=0, sheet=1)
# unit bc closed
bd6 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_bd_gc.xlsx", skip=0, sheet=1)
ca6 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_ca_bs.xlsx", skip=0, sheet=1)
cb6 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_cb_gc.xlsx", skip=0, sheet=1)
da6 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_da_bs.xlsx", skip=0, sheet=1)
db6 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_db_gc.xlsx", skip=0, sheet=1)
ea6 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_ea_bs.xlsx", skip=0, sheet=1)
eb6 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_eb_gc.xlsx", skip=0, sheet=1)
inf6 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_inf_gc.xlsx", skip=0, sheet=1)
rhu6 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave6_participation/pcq_participation_wave6_rhu_gc.xlsx", skip=0, sheet=1)

wave6_callsheets <- rbind(aa6, ab6, ac6, ad6,
                          ba6, bb6, bd6, #bc closed
                          ca6, cb6,
                          da6, db6,
                          ea6, eb6,
                          inf6, rhu6)

names(wave6_callsheets)[which(names(wave6_callsheets)=="inmate_id")] <- "original_id"
wave6_callsheets$original_id <- tolower(wave6_callsheets$original_id)
i <- unique(wave6_callsheets$original_id)
id.link <- mask_ids(i) # Generate masked IDs
wave6_callsheets <- left_join(wave6_callsheets, id.link, by="original_id") # Merge wave3_callsheets on original ID
wave6_callsheets <- wave6_callsheets[,c(ncol(wave6_callsheets),1:ncol(wave6_callsheets)-1)] # reorder columns
names(wave6_callsheets)[which(names(wave6_callsheets)=="cell")] <- "location"

wave6_callsheets <- wave6_callsheets[,c("research_id", "unit", "location")]
save(wave6_callsheets, file = "data/processed/wave6_callsheets.Rda")

# Participation by unit
wave6_callsheets$survey <- as.numeric(wave6_callsheets$research_id %in% wave6_participation$research_id)

wave6_callsheets %>% group_by(unit) %>%
  summarize(nunit = n(),
            nsurvey = sum(survey)) %>%
  mutate(rr = nsurvey/nunit) %>%
  select(unit, rr)



# Participation wave 7 ####
aa7p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_aa_gc.xlsx", skip=0, sheet=2)
aa7p$unit <- "aa"
ab7p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_ab_gc.xlsx", skip=0, sheet=2)
ab7p$unit <- "ab"
ac7p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_ac_gc.xlsx", skip=0, sheet=2)
ac7p$unit <- "ac"
ad7p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_ad_gc.xlsx", skip=0, sheet=2)
ad7p$unit <- "ad"
ba7p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_ba_gc.xlsx", skip=0, sheet=2)
ba7p$unit <- "ba"
bb7p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_bb_gc.xlsx", skip=0, sheet=2)
bb7p$unit <- "bb"
bc7p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_bc_gc.xlsx", skip=0, sheet=2)
bc7p$unit <- "bc"
bd7p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_bd_gc.xlsx", skip=0, sheet=2)
bd7p$unit <- "bd"
ca7p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_ca_gc.xlsx", skip=0, sheet=2)
ca7p$unit <- "ca"
cb7p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_cb_gc.xlsx", skip=0, sheet=2)
cb7p$unit <- "cb"
da7p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_da_gc.xlsx", skip=0, sheet=2)
da7p$unit <- "da"
db7p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_db_gc.xlsx", skip=0, sheet=2)
db7p$unit <- "db"
ea7p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_ea_gc.xlsx", skip=0, sheet=2)
ea7p$unit <- "ea"
eb7p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_eb_gc.xlsx", skip=0, sheet=2)
eb7p$unit <- "eb"
inf7p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_inf_gc.xlsx", skip=0, sheet=2)
inf7p$unit <- "inf"
rhu7p <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_rhu_gc.xlsx", skip=0, sheet=2)
rhu7p$unit <- "rhu"

wave7_participation <- rbind(aa7p, ab7p, ac7p,
                             ad7p[,c("inmate_id", "unit")],
                             ba7p, bb7p, bc7p, bd7p,
                             ca7p, cb7p,
                             da7p, db7p,
                             ea7p, eb7p,
                             inf7p, rhu7p)

names(wave7_participation)[which(names(wave7_participation)=="inmate_id")] <- "original_id"
wave7_participation$original_id <- tolower(wave7_participation$original_id)
i <- unique(wave7_participation$original_id)
id.link <- mask_ids(i) # Generate masked IDs
wave7_participation <- left_join(wave7_participation, id.link, by="original_id") # Merge wave7_participation on original ID
wave7_participation$completed_survey <- 2
wave7_payment <- wave7_participation %>% group_by(original_id) %>% summarise(to_pay = sum(completed_survey))

wave7_participation <- wave7_participation[,c("research_id", "unit")]

save(wave7_participation, file = "wave7_participation.Rda")

# Save payment file
write.xlsx(wave7_payment, file = "wave7_payment.xlsx")

# Callsheets wave 7 ####
aa7 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_aa_gc.xlsx", skip=0, sheet=1)
ab7 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_ab_gc.xlsx", skip=0, sheet=1)
ac7 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_ac_gc.xlsx", skip=0, sheet=1)
ad7 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_ad_gc.xlsx", skip=0, sheet=1)
ba7 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_ba_gc.xlsx", skip=0, sheet=1)
bb7 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_bb_gc.xlsx", skip=0, sheet=1)
bc7 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_bc_gc.xlsx", skip=0, sheet=1)
bd7 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_bd_gc.xlsx", skip=0, sheet=1)
ca7 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_ca_gc.xlsx", skip=0, sheet=1)
cb7 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_cb_gc.xlsx", skip=0, sheet=1)
da7 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_da_gc.xlsx", skip=0, sheet=1)
db7 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_db_gc.xlsx", skip=0, sheet=1)
ea7 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_ea_gc.xlsx", skip=0, sheet=1)
eb7 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_eb_gc.xlsx", skip=0, sheet=1)
inf7 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_inf_gc.xlsx", skip=0, sheet=1)
rhu7 <- read_xlsx("data/raw/3_surveys/1_participation_lists/pcq_wave7_participation/pcq_participation_wave7_rhu_gc.xlsx", skip=0, sheet=1)

wave7_callsheets <- rbind(aa7, ab7, ac7, ad7,
                          ba7, bb7, bd7, bc7,
                          ca7, cb7,
                          da7, db7,
                          ea7, eb7,
                          inf7, rhu7)

names(wave7_callsheets)[which(names(wave7_callsheets)=="inmate_id")] <- "original_id"
wave7_callsheets$original_id <- tolower(wave7_callsheets$original_id)
i <- unique(wave7_callsheets$original_id)
id.link <- mask_ids(i) # Generate masked IDs
wave7_callsheets <- left_join(wave7_callsheets, id.link, by="original_id") # Merge wave3_callsheets on original ID
wave7_callsheets <- wave7_callsheets[,c(ncol(wave7_callsheets),1:ncol(wave7_callsheets)-1)] # reorder columns
names(wave7_callsheets)[which(names(wave7_callsheets)=="cell")] <- "location"

wave7_callsheets <- wave7_callsheets[,c("research_id", "unit", "location")]
save(wave7_callsheets, file = "wave7_callsheets.Rda")

# Participation by unit
wave7_callsheets$survey <- as.numeric(wave7_callsheets$research_id %in% wave7_participation$research_id)

wave7_callsheets %>% group_by(unit) %>%
  summarize(nunit = n(),
            nsurvey = sum(survey)) %>%
  mutate(rr = nsurvey/nunit) %>%
  select(unit, rr)

