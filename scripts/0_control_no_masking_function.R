# ================================================================= ####
# Notes to Script ####
# -- Objective ####
# Define function to mask and unmask control numbers
# -- Readme ####
# This script should be accessible only by those in charge of building the database.

# ================================================================= ####
# Set up ####
# -- Prepare environment ####
rm(list=ls())
source("scripts/00_packages.R")
source("scripts/0_utils.R")

# -- Functions ####
# -- -- Generate Reference Key ####
df.numbers <- data.frame(
  number_original = seq(0,9,1),
  number1_masked = c(9, 5, 8, 0, 2, 7, 6, 1, 4, 3),
  number2_masked = c(0, 8, 7, 1, 5, 2, 6, 3, 9, 4),
  number3_masked = c(4, 5, 8, 6, 3, 2, 9, 7, 0, 1),
  number4_masked = c(7, 2, 1, 5, 3, 9, 0, 6, 8, 4),
  number5_masked = c(5, 4, 8, 7, 3, 9, 6, 0, 2, 1),
  number6_masked = c(1, 3, 5, 7, 9, 0, 2, 4, 6, 8)
  )

# -- -- Define functions ####
mask_control_nos <- function(true_control_no){
  # Mask IDs by linking each letter/number to their corresponding value in df.letters and df.numbers
  temp <- data.frame(control_number = true_control_no)

  temp$number1_original <- NA
  temp$number2_original <- NA
  temp$number3_original <- NA
  temp$number4_original <- NA
  temp$number5_original <- NA
  temp$number6_original <- NA
  temp$number1_masked <- NA
  temp$number2_masked <- NA 
  temp$number3_masked <- NA
  temp$number4_masked <- NA
  temp$number5_masked <- NA
  temp$number6_masked <- NA
  temp$research_id <- NA

  k <- which(!is.na(temp$control_number)) # Only for IDs that are not NA.

  temp$number1_original[k] <- substr(temp$control_number[k],1,1)
  temp$number2_original[k] <- substr(temp$control_number[k],2,2)
  temp$number3_original[k] <- substr(temp$control_number[k],3,3)
  temp$number4_original[k] <- substr(temp$control_number[k],4,4)
  temp$number5_original[k] <- substr(temp$control_number[k],5,5)
  temp$number6_original[k] <- substr(temp$control_number[k],6,6)

  i <- match(temp$number1_original, df.numbers$number_original)
  temp$number1_masked <- df.numbers$number1_masked[i]

  i <- match(temp$number2_original, df.numbers$number_original)
  temp$number2_masked <- df.numbers$number2_masked[i]

  i <- match(temp$number3_original, df.numbers$number_original)
  temp$number3_masked <- df.numbers$number3_masked[i]

  i <- match(temp$number4_original, df.numbers$number_original)
  temp$number4_masked <- df.numbers$number4_masked[i]

  i <- match(temp$number5_original, df.numbers$number_original)
  temp$number5_masked <- df.numbers$number5_masked[i]

  i <- match(temp$number6_original, df.numbers$number_original)
  temp$number6_masked <- df.numbers$number6_masked[i]

  temp$research_id[k] <- with(temp, paste0("rid_",
                                           number1_masked[k],
                                           number2_masked[k],
                                           number3_masked[k],
                                           number4_masked[k],
                                           number5_masked[k],
                                           number6_masked[k]))
  matched_ids <- temp[,c("control_number","research_id")]
  return(matched_ids)
}
unmask_control_nos <- function(research_id){
  # Unmask IDs by linking each number to their corresponding value in df.numbers
  temp <- data.frame(research_id = gsub("rid_","",research_id))

  temp$number1_original <- NA
  temp$number2_original <- NA
  temp$number3_original <- NA
  temp$number4_original <- NA
  temp$number5_original <- NA
  temp$number6_original <- NA
  temp$number1_masked <- NA
  temp$number2_masked <- NA
  temp$number3_masked <- NA
  temp$number4_masked <- NA
  temp$number5_masked <- NA
  temp$number6_masked <- NA
  temp$control_number <- NA

  k <- which(!is.na(temp$research_id)) # Only for IDs that are not NA.

  temp$number1_masked[k] <- substr(temp$research_id[k],1,1)
  temp$number2_masked[k] <- substr(temp$research_id[k],2,2)
  temp$number3_masked[k] <- substr(temp$research_id[k],3,3)
  temp$number4_masked[k] <- substr(temp$research_id[k],4,4)
  temp$number5_masked[k] <- substr(temp$research_id[k],5,5)
  temp$number6_masked[k] <- substr(temp$research_id[k],6,6)

  i <- match(temp$number1_masked, df.numbers$number1_masked)
  temp$number1_original <- df.numbers$number_original[i]

  i <- match(temp$number2_masked, df.numbers$number2_masked)
  temp$number2_original <- df.numbers$number_original[i]

  i <- match(temp$number3_masked, df.numbers$number3_masked)
  temp$number3_original <- df.numbers$number_original[i]

  i <- match(temp$number4_masked, df.numbers$number4_masked)
  temp$number4_original <- df.numbers$number_original[i]

  i <- match(temp$number5_masked, df.numbers$number5_masked)
  temp$number5_original <- df.numbers$number_original[i]

  i <- match(temp$number6_masked, df.numbers$number6_masked)
  temp$number6_original <- df.numbers$number_original[i]

  temp$control_number[k] <- with(temp, paste0(number1_original[k],
                                           number2_original[k],
                                           number3_original[k],
                                           number4_original[k],
                                           number5_original[k],
                                           number6_original[k]))
  matched_ids <- temp[,c("control_number","research_id")]
  return(matched_ids)
}


# ================================================================= ####