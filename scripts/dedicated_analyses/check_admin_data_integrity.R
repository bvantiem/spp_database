# Wave 6
load("data/processed/wave6_callsheets.Rda")
wave6_callsheets_rid <- wave6_callsheets
basic <- readRDS("data/processed/processing_layer_1/basic.Rds")
pcq <- readRDS("data/processed/processing_layer_1/pcq.rds")

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

wave6_callsheets$inmate_id <- tolower(wave6_callsheets$inmate_id)

# Are all the IDs that we requested included?
temp <- mask_ids(wave6_callsheets$inmate_id)


callsheet_ids <- unique(wave6_callsheets_rid$research_id)
callsheet_ids[which(callsheet_ids %ni% basic$research_id)]
unmask_ids(callsheet_ids[which(callsheet_ids %ni% basic$research_id)])

 "qq2870"   "hd2017"   "qq1585"   "qc8480"  "ql6441"   "qq4428"   "qq0611" "cw8291"   "qq2686"   "qq1040"   "qd4273"
 "jc3798"   "dm3850"   "ql5350"



# Identify originals for mismatches
# Have not been able to trace this one back in pcq6
temp$original_id[which(temp$research_id == "rid_etNA397")] "lmf7604"
# Np
which(grepl("lm", pcq6$id_num))
which(grepl("lm", pcq6$id_num_2))
which(grepl("7604", pcq6$id_num))
which(grepl("7604", pcq6$id_num_2))

# Yes
pcq6$id_num[which(grepl("76", pcq6$id_num))]
pcq6$id_num[which(grepl("04", pcq6$id_num))]
pcq6$id_num[which(grepl("04", pcq6$id_num_2))]
pcq6$id_num[which(grepl("lf", pcq6$id_num))]


which(pcq6$id_num=="lf7604")
temp$original_id[which(temp$research_id == "rid_aqNA652")] # "qqo611", likely "qq0611"
which(pcq6$id_num=="qq0611")


# Is the date range what we'd expect?
# Up until the date of the datapull but not beyond.
range(house$date_in, na.rm=T)
range(house$date_out, na.rm=T) # Pulled on 20241104





# Are the number of observations what we'd expect given prior pulls?



