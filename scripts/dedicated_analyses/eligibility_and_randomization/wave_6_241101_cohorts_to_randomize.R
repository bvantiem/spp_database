library("excel.link")
source("scripts/0_id_masking_function.R")
source("scripts/00_packages.R")
source("scripts/0_utils.R")
# Load my random assignment list for waves 1234
randassign <- readRDS(file="data/processed/randassign.Rds")
randassign <- cbind(randassign[,-which(names(randassign)=="research_id")], unmask_ids(randassign$research_id))
# Check assignment balance
prop.table(table(randassign$treatment_wave, randassign$treated), margin = 1)

# Who is on the unit now? Treated and not released
onunit <- randassign[which(is.na(randassign$release_type) & randassign$treated==1),]
nrow(onunit)

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
                                       "qp2712",
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
                                       "qp1702",
                                       "qn7893"))

# All 43 individuals who are on the roster appear as treated in our data
# The one exception is dl3766 who is a commuted lifer. We still do not have controls for him.
onroster <- left_join(onroster, randassign)
onroster <- onroster[,-which(names(onroster) %in% c("notes", "research_id"))]

# In our data, there are 46 individuals on the unit, instead of 43
# This figure of 46 excludes some individuals who our data say are released but aren't yet!
nrow(onunit)

# Individuals who are on the unit according to our data (treated and not released) but are not on the roster
# All of these individuals were treated in wave 5
# None of these individuals were flagged as not on the unit in the release date list
onunit$original_id[which(onunit$original_id %in% onroster$original_id)]

# dl3766 is a commuted lifer. I do not have controls for him.
onroster$original_id[which(onroster$original_id %ni% randassign$original_id)]








