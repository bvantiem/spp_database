check <- basic[,c("research_id","sentence_class", "min_expir_date", "max_expir_date", "recomp_max_date", "min_sent_days","max_sent_days","min_cort_sent_yrs","min_cort_sent_mths")]
check$est_admit_date <- check$max_expir_date - 2*(days(check$max_expir_date-check$min_expir_date))
i <- which(check$max_expir_date == "9999-01-01")
check$est_admit_date[i] <- NA
i <- which(check$sentence_class=="DEFINITE/FLAT")
check$est_admit_date[i] <- check$min_expir_date[i]-check$min_sent_days[1]
i <- which(check$sentence_class=="INDETERMINATE" & (check$min_expir_date==check$max_expir_date))
check$est_admit_date[i] <- check$max_expir_date[i]-check$max_sent_days[i]
i <- which(check$research_id=="rid_am3704")
check$est_admit_date[i] <- ymd(20220430) # Manual override. Formula not 100% adequate
check$est_time_served <- ymd(20220501)-check$est_admit_date
check$est_time_served <- as.numeric(gsub(" days", "", check$est_time_served))

basic$est_admit_date <- basic$max_expir_date - 2*(days(basic$max_expir_date-basic$min_expir_date))
i <- which(basic$max_expir_date == "9999-01-01")
basic$est_admit_date[i] <- NA
i <- which(basic$sentence_class=="DEFINITE/FLAT")
basic$est_admit_date[i] <- basic$min_expir_date[i]-basic$min_sent_days[1]
i <- which(basic$sentence_class=="INDETERMINATE" & (basic$min_expir_date==basic$max_expir_date))
basic$est_admit_date[i] <- basic$max_expir_date[i]-basic$max_sent_days[i]
i <- which(basic$research_id=="rid_am3704")
basic$est_admit_date[i] <- ymd(20220430) # Manual override. Formula not 100% adequate
basic$est_time_served_on_20220501 <- ymd(20220501)-basic$est_admit_date
basic$est_time_served_on_20220501 <- as.numeric(gsub(" days", "", basic$est_time_served_on_20220501))

