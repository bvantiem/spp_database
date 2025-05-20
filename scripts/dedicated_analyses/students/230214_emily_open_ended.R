load("data/processed/pcq.Rda")
load("data/processed/basic.Rda")

pcq_emily <- pcq[,c("research_id", "survey_wave","date_l","unit","unit_type","q19","q20","q21","q24","q25","q33","q81","q174", "text_answer")]
demographics <- basic[,c("research_id", "race", "age_on_20220501")]
save(pcq_emily, file="data/processed/dedicated_analyses/pcq_emily_open_ended_plus_230502.Rda")
save(demographics, file="data/processed/dedicated_analyses/pcq_emily_demographics.Rda")


