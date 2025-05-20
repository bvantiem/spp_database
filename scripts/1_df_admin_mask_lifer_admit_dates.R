rm(list=ls())
library(readxl)
library(dplyr)
library(lubridate)
library("excel.link")
source("scripts/0_id_masking_function.R")
`%ni%` = Negate(`%in%`)

# Admin Data wave 1
lifers <- xl.read.file("data/raw/1_admin_data/other/230224_lifer_admit_dates.xlsx", xl.sheet=2, password="Jordan'sInmateAdmits")
lifers$admit_date <- ymd(lifers$admit_date)
length(unique(lifers$inmate_number)) #95 unique individuals

lifers$first_admit <- ymd(20990909)
for(i in unique(lifers$inmate_number)){
  lifers[lifers$inmate_number==i,"first_admit"] <- min(lifers[lifers$inmate_number==i,"admit_date"])
}
lifers <- lifers[which(lifers$first_admit==lifers$admit_date),]

# Mask IDs ####
names(lifers)[which(names(lifers)=="inmate_number")] <- "original_id"
lifers$original_id <- tolower(lifers$original_id)
i <- unique(lifers$original_id)
id.link <- mask_ids(i) # Generate masked IDs
lifers <- left_join(lifers, id.link, by="original_id") # Merge lifers on original ID
lifers <- lifers[,-which(names(lifers)=="original_id")] # delete original ID
lifers <- lifers[,c(ncol(lifers),1:ncol(lifers)-1)] # reorder columns

# Save ####
# saveRDS(lifers, file="data/processed/processing_layer_1/230224_lifer_admit_dates.Rds")
