library("excel.link")
source("scripts/0_id_masking_function.R")
source("scripts/00_packages.R")
source("scripts/0_utils.R")
# Load my random assignment list for waves 1234
randassign <- readRDS(file="data/processed/randassign.Rds")
randassign <- cbind(randassign[,-which(names(randassign)=="research_id")], unmask_ids(randassign$research_id))
# Check assignment balance
prop.table(table(randassign$treatment_wave, randassign$treated), margin = 1)

# Who is on the unit now?
onunit <- randassign[which(is.na(randassign$release_type) & randassign$treated==1),]
writexl::write_xlsx(onunit, "temp_onunit.xlsx")

# Leaving out "qp2712" who is due to be released soon
onroster <- data.frame(original_id = c("cy9623",
  "bh6778",
  "as0160",
  "jq3455",
  "ce6457",
  "ma6659",
  "dl3766",
  "kf3947",
  "nm2924",
  "nb8976",
  "qg0327",
  "nm8559",
  "qg4037",
  "qn3680",
  "qg9258",
  "ns3392",
  "qk4233",
  "qn7893",
  "hg0083",
  "hg2050",
  "ql4946",
  "qf2595",
  "qn7876",
  "qp6175",
  "qj3332",
  "ql8849",
  "qp4011",
  "as2609",
  "dv1788",
  "qn6583",
  "bm7165",
  "ql2731",
  "qn7011",
  "cy7366",
  "qn9567",
  "qh0546",
  "ng2175",
  "fb1508",
  "na0081",
  "qh4632",
  "qp1702"))

# 34 matches
onunit$original_id[which(onunit$original_id %in% onroster$original_id)]

# 6 people on my list not on roster
onunit.notonroster <- onunit$original_id[which(onunit$original_id %ni% onroster$original_id)]

randassign[which(randassign$original_id %in% onunit.notonroster),]

# 7 people on roster not on my list
onroster.notonlist <- onroster$original_id[which(onroster$original_id %ni% onunit$original_id)]

## "dl3766" not on my random assignment list. Spoke to Jordan and he turns out to be a commuted lifer. Need to get controls for him.
onroster.notonlist[onroster.notonlist %ni% randassign$original_id]

## The remaining six people were assigned to the control group in my data. They were all treated in wave 4.
randassign[which(randassign$original_id %in% onroster.notonlist),"original_id"]

randassign[which(randassign$original_id %in% onroster.notonlist|randassign$original_id %in% onunit.notonroster),]


###############################################
# My data show there are currently 41 people on the unit (40 + 1 person about to be released.
nrow(randassign[which(is.na(randassign$release_type) & randassign$treated==1),])

# In reality there are 42 - 1 is a commuted lifer who is not yet in our database.
# So we need to fill 23 spots. 64-41

# Since the unit started, 66 individuals have left from the unit (including people who were moved to other prisons or units). Of those, 44 were in the 0-6m to min group, 13 in the 6-12m to min, 7 in the 12-60m to min, 1 in the 60+m to min, and 1 lifer.
temp.released <- randassign[which(!is.na(randassign$release_type) & randassign$treated==1),]
table(temp.released$stratum)

# Of the 23 individuals who were released last:
# 11 were from the 0-6 month stratum
# 6 were from the 6-12 month stratum
# 6 were from the 12-60 month stratum
# Therefore, we should randomly select:
# 33 individuals (11 treated, 23 controls) from the 0-6 month stratum
# 18 (6 treated, 12 controls) from the 6-12 month stratum
# 18 (6 treated, 12 controls) from the 12-60 months stratum

temp.released %>% filter(stratum!="lifer" & !is.na(release_type)) %>% arrange(desc(release_date)) %>% select(stratum) %>% head(n=23) %>% table()

# Sent email to Jordan on 5/28/24
