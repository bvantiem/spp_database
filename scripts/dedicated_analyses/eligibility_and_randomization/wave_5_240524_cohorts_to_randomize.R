library("excel.link")
source("scripts/0_id_masking_function.R")
source("scripts/00_packages.R")
source("scripts/0_utils.R")

# Load my random assignment list for waves 1234
randassign <- readRDS(file="data/processed/randassign.Rds")

# Check assignment balance
prop.table(table(randassign$treatment_wave, randassign$treated), margin = 1)

# There are currently 40 people on the unit (Sarah said 45 in her notes last week!)
# This means that there are 64-40=24 open beds
nrow(randassign[which(is.na(randassign$release_type) & randassign$treated==1),])

# Since the unit started, 66 individuals have left from the unit (including people who were moved to other prisons or units). Of those, 44 were in the 0-6m to min group, 13 in the 6-12m to min, 7 in the 12-60m to min, 1 in the 60+m to min, and 1 lifer.
temp.released <- randassign[which(!is.na(randassign$release_type) & randassign$treated==1),]
table(temp.released$stratum)

# Of the 24 individuals who were released last:
# 12 were from the 0-6 month stratum
# 6 were from the 6-12 month stratum
# 6 were from the 12-60 month stratum
# Therefore, we should randomly select:
# 36 individuals (12 treated, 24 controls) from the 0-6 month stratum
# 18 (6 treated, 12 controls) from the 6-12 month stratum
# 18 (6 treated, 12 controls) from the 12-60 months stratum

temp.released %>% filter(stratum!="lifer" & !is.na(release_type)) %>% arrange(desc(release_date)) %>% select(stratum) %>% head(n=24) %>% table()

# Prior to proceeding, we should check: (1) that there are indeed 40 individuals on the unit (Sarah said there were 45 individuals in her notes last week), and that (2) we do not expect anyone to be release imminently (as we should include them and fill their spot)
# Note that 9 of the 24 individuals released last were released prior to the last randomization date of 2023-11-27. It is worth confirming that 9 spots were left unfilled last time.

# Sent email to Jordan on 24/5/24
