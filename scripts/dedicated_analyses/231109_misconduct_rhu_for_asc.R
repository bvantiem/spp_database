# Data set-up ----
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")
source("scripts/0_id_masking_function.R")

# Data ####
misc <- xl.read.file("data/raw/z. misc/20231107_Misconducts - for bvt.xlsx", password = "LS2023", xl.sheet=1)
misc$inmate_number <- tolower(misc$inmate_number)
moves <- xl.read.file("data/raw/z. misc/20231108_LSParticipantBedMoves - for bvt.xlsx", password = "LS2023", xl.sheet=1)
randassign <- readRDS("data/processed/randassign.Rds")

randassign$original_id <- unmask_ids(randassign$research_id)$original_id
misc$in.randassign <- misc$inmate_number %in% randassign$original_id

moves$inmate_number <- tolower(moves$inmate_number)
moves$in.randassign <- moves$inmate_number %in% randassign$original_id

ids.misc <- misc[which(misc$in.randassign==FALSE), c("inmate_number", "LSStartDate", "RandomAssignment")] %>%
  distinct()
write.xlsx(ids.misc, "data/processed/dedicated_analyses/ids_not_treated_in_our_data_misc.xlsx")

ids.moves <- moves[which(moves$in.randassign==FALSE), c("inmate_number", "RandomAssignment")] %>%
  distinct()
write.xlsx(ids.moves, "data/processed/dedicated_analyses/ids_not_treated_in_our_data_moves.xlsx")

# Variables ####
misc$misconduct_date <- ymd(misc$misconduct_date)
misc$LSStartDate <- ymd(misc$LSStartDate)
misc$data.start.at.treatment <- misc$misconduct_date>=misc$LSStartDate
moves$DtIn <- as.Date(ymd(moves$DtIn))
moves$DtOut <- as.Date(ymd(moves$DtOut))

# For now, work only with the IDs that are in my data ####
misc <- misc[which(misc$inmate_number %ni% ids.misc$inmate_number),]
moves <- moves[which(moves$inmate_number %ni% ids.moves$inmate_number),]

# Standard code ####
moves <- moves %>% filter(housing_status=="RHU")
moves$DtOut[which(is.na(moves$DtOut))] <- max(moves$DtOut, na.rm=T)
moves$days.spent <- grep("[0-9].*", moves$DtOut-moves$DtIn)

misc <- as.data.frame(misc)
moves <- as.data.frame(moves)
misc$in.rhu <- 0

# Identify which misconducts happen in the RHU
for(i in 1:nrow(misc)){
  id <- misc$inmate_number[i]
  row.index <- which(moves$inmate_number==id)

  moves$check <- NA
  for(k in row.index){
    moves$check[k] <- ifelse((misc[i,"misconduct_date"] >= moves[k,"DtIn"]) &
                               (misc[i,"misconduct_date"] <= moves[k,"DtOut"]), TRUE, FALSE)
  }
  if(any(moves[row.index,"check"])==TRUE){
    misc$in.rhu[i] <- 1
  }
}

# Count misconduct incidences per person
misc <- misc %>% group_by(inmate_number) %>% mutate(n_misc = n()) %>% ungroup
misc$any.misconduct <- 1

misc.by.id <- misc %>% group_by(inmate_number) %>% summarize(n_misc = mean(n_misc))

# Identify how many days total spent in RHU
# Tricky because there is date overlap
# -- e.g. moves[moves$inmate_number=="qn6778",]
# -- Would need to remove instances of overlap

# Unique individuals, prevalence
misc.prevalence <- data.frame(inmate_number = randassign$original_id,
                              treated = randassign$treated,
                              treatment_wave = randassign$treatment_wave,
                              stratum = randassign$stratum,
                              any.misconduct = as.numeric(randassign$original_id %in% misc$inmate_number),
                              any.misconduct.outside.rhu = as.numeric(randassign$original_id %in%
                                             misc[which(misc$in.rhu==0),]$inmate_number),
                              any.rhu = as.numeric(randassign$original_id %in% moves$inmate_number))


misc.prevalence <- left_join(misc.prevalence, misc.by.id)
misc.prevalence$n_misc[which(is.na(misc.prevalence$n_misc))] <- 0

# Tabulations for Jordan ####
tab <- with(misc.prevalence, table(treated, useNA = "ifany"))
tab

write.xlsx(tab, "data/processed/dedicated_analyses/tab.count.treated.control.xlsx")

tab <- with(misc.prevalence, table(treated,treatment_wave, useNA = "ifany"))
tab

write.xlsx(tab, "data/processed/dedicated_analyses/tab.count.treated.by.treatment.wave.xlsx")

tab <- with(misc.prevalence, table(treated,stratum, useNA = "ifany"))
tab

write.xlsx(tab, "data/processed/dedicated_analyses/tab.count.treated.by.stratum.xlsx")

#
tab <- with(misc.prevalence, table(treated, n_misc, useNA = "ifany"))
tab
write.xlsx(tab, "data/processed/dedicated_analyses/tab.count.misconducts.xlsx")

#
tab <- with(misc.prevalence, table(treated, any.misconduct, useNA = "ifany"))
tab <- prop.table(tab, margin = 1)
tab
write.xlsx(tab, "data/processed/dedicated_analyses/tab.prop.any.misconduct.xlsx")

#
tab <- with(misc.prevalence, table(treated, any.rhu, useNA = "ifany"))
tab <- prop.table(tab, margin = 1)
tab
write.xlsx(tab, "data/processed/dedicated_analyses/tab.prop.any.rhu.xlsx")

#
tab <- with(misc.prevalence[which(misc.prevalence$any.misconduct==1),], table(treated, any.rhu, useNA = "ifany"))
tab <- prop.table(tab, margin = 1)
tab
write.xlsx(tab, "data/processed/dedicated_analyses/tab.prop.any.rhu.cond.on.any.misconduct.xlsx")
