source("scripts/00_packages.R")
source("scripts/0_id_masking_function.R")
source("scripts/0_utils.R")
# library("excel.link")

# Emailed Jordan to resolve inconsistencies on 5/20/2025

# Load my random assignment list for waves 123456
randassign <- read_excel("data/raw/4_random_assignment/release_dates/release_dates_updated_prior_to_wave_7.xlsx")

# Check assignment balance
prop.table(table(randassign$treatment_wave, randassign$treated), margin = 1)

# Who is on the unit now? Treated and not released
onunit <- randassign[which(is.na(randassign$release_type) & randassign$treated==1),]
nrow(onunit)

onroster <- data.frame(id_num = c("bh6778",
                                       "as0160",
                                       "ce6457",
                                       "nm2924",
                                       "ks3571",
                                       "qp7389",
                                       "qd0544",
                                       "ma6659",
                                       "qg4037",
                                       "qn9943",
                                       "qp3766",
                                       "nw8946",
                                       "qg9258",
                                       "qg1311",
                                       "qn7893",
                                       "hg0083",
                                       "dl3766",
                                       "hg2050",
                                       "qq1481",
                                       "qp4329",
                                       "cy9623",
                                       "qp0179",
                                       "qp4998",
                                       "qj3332",
                                       "nq8517",
                                       "qp2723",
                                       "as2609",
                                       "dv1788",
                                       "qp5968",
                                       "bm7165",
                                       "qn7011",
                                       "qp6485",
                                       "qp6928",
                                       "cy7366",
                                       "qh0546",
                                       "qq1607",
                                       "ns3392",
                                       "qp8477",
                                       "qh4632",
                                       "qp1702"))
nrow(onroster)

# 39 out of 40 individuals who are on the roster appear as treated in our data
# dl3766 who is a commuted lifer. We still do not have controls for him.
onroster <- left_join(onroster, randassign)
table(onroster$treated, useNA = "always")
onroster$id_num[which(is.na(onroster$treated))]

# In our data, there are 39 individuals on the unit, instead of 40
# The missing individual is dl3766
nrow(onunit)

###############################################
# There are currently 40 people on the unit (39 + dl3766).
nrow(randassign[which(is.na(randassign$release_type) & randassign$treated==1),])

# We need to fill 23 open cells (1 cell is not in use)

# Since the unit started, 115 individuals have left from the unit (including people who were moved to other prisons or units). Of those, 71 were in the 0-6m to min group, 21 in the 6-12m to min, 13 in the 12-60m to min, 3 in the 60+m to min, and 1 lifer.
temp.released <- randassign[which(!is.na(randassign$release_type) & randassign$treated==1),]
table(temp.released$stratum)

# Of the 23 individuals who were released last:
# 14 were from the 0-6 month stratum
# 4 were from the 6-12 month stratum
# 3 were from the 12-60 month stratum
# 2 were from the 60+ month stratum
# Therefore, we should randomly select:
# 69 individuals (23 treated, 46 controls), of which
# 32 (14 treated, 28 controls) from the 0-6 month stratum
# 12 (4 treated, 8 controls) from the 6-12 month stratum
# 9 (3 treated, 6 controls) from the 12-60 months stratum
# 6 (2 treated, 4 controls) from the 60+ months stratum

temp.released %>% filter(stratum!="lifer" & !is.na(release_type)) %>% arrange(desc(release_date)) %>% select(stratum) %>% head(n=23) %>% table()


