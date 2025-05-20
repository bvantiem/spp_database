# This script backs up the function to mask IDs. Keep safe!

library(dplyr)
# Generate Reference Key
df.letters <- data.frame(letter_original = letters,
                         letter1_masked = c("h", "c", "z", "s", "d", "t", "w", "g", "q", "y", 
                                            "b", "e", "n", "j", "f", "i", "a", "r", "m", "x",
                                            "v", "l", "u", "o", "k", "p"),
                         letter2_masked = c("v", "e", "f", "r", "s", "w", "k", "p", "x", "g",
                                            "h", "d", "t", "m", "a", "n", "q", "z", "c", "i",
                                            "b", "u", "j", "l", "y", "o"))

df.numbers <- data.frame(
  number_original = seq(0,9,1),
  number1_masked = c(9, 5, 8, 0, 2, 7, 6, 1, 4, 3),
  number2_masked = c(0, 8, 7, 1, 5, 2, 6, 3, 9, 4),
  number3_masked = c(4, 5, 8, 6, 3, 2, 9, 7, 0, 1),
  number4_masked = c(7, 2, 1, 5, 3, 9, 0, 6, 8, 4))

mask_ids <- function(true_id){
# Mask IDs by linking each letter/number to their corresponding value in df.letters and df.numbers
temp <- data.frame(original_id = true_id)

temp$letter1_original <- NA
temp$letter2_original <- NA
temp$number1_original <- NA
temp$number2_original <- NA
temp$number3_original <- NA
temp$number4_original <- NA
temp$letter1_masked <- NA
temp$letter2_masked <- NA
temp$number1_masked <- NA
temp$number2_masked <- NA
temp$number3_masked <- NA
temp$number4_masked <- NA
temp$research_id <- NA

k <- which(!is.na(temp$original_id)) # Only for IDs that are not NA.

  temp$letter1_original[k] <- substr(temp$original_id[k],1,1)
  temp$letter2_original[k] <- substr(temp$original_id[k],2,2)
  temp$number1_original[k] <- substr(temp$original_id[k],3,3)
  temp$number2_original[k] <- substr(temp$original_id[k],4,4)
  temp$number3_original[k] <- substr(temp$original_id[k],5,5)
  temp$number4_original[k] <- substr(temp$original_id[k],6,6)
  
i <- match(temp$letter1_original, df.letters$letter_original)
temp$letter1_masked <- df.letters$letter1_masked[i]

i <- match(temp$letter2_original, df.letters$letter_original)
temp$letter2_masked <- df.letters$letter2_masked[i]

i <- match(temp$number1_original, df.numbers$number_original)
temp$number1_masked <- df.numbers$number1_masked[i]

i <- match(temp$number2_original, df.numbers$number_original)
temp$number2_masked <- df.numbers$number2_masked[i]

i <- match(temp$number3_original, df.numbers$number_original)
temp$number3_masked <- df.numbers$number3_masked[i]

i <- match(temp$number4_original, df.numbers$number_original)
temp$number4_masked <- df.numbers$number4_masked[i]

temp$research_id[k] <- with(temp, paste0("rid_",letter1_masked[k],
                           letter2_masked[k],
                           number1_masked[k],
                           number2_masked[k],
                           number3_masked[k],
                           number4_masked[k]))
matched_ids <- temp[,c("original_id","research_id")]
return(matched_ids)
}
unmask_ids <- function(research_id){
# Unmask IDs by linking each letter/number to their corresponding value in df.letters and df.numbers
  temp <- data.frame(research_id = gsub("rid_","",research_id))
  temp$letter1_original <- NA
  temp$letter2_original <- NA
  temp$number1_original <- NA
  temp$number2_original <- NA
  temp$number3_original <- NA
  temp$number4_original <- NA
  temp$letter1_masked <- NA
  temp$letter2_masked <- NA
  temp$number1_masked <- NA
  temp$number2_masked <- NA
  temp$number3_masked <- NA
  temp$number4_masked <- NA
  temp$original_id <- NA
  
  k <- which(!is.na(temp$research_id)) # Only for IDs that are not NA.
  
  temp$letter1_masked[k] <- substr(temp$research_id[k],1,1)
  temp$letter2_masked[k] <- substr(temp$research_id[k],2,2)
  temp$number1_masked[k] <- substr(temp$research_id[k],3,3)
  temp$number2_masked[k] <- substr(temp$research_id[k],4,4)
  temp$number3_masked[k] <- substr(temp$research_id[k],5,5)
  temp$number4_masked[k] <- substr(temp$research_id[k],6,6)
  
  i <- match(temp$letter1_masked, df.letters$letter1_masked)
  temp$letter1_original <- df.letters$letter_original[i]
  
  i <- match(temp$letter2_masked, df.letters$letter2_masked)
  temp$letter2_original <- df.letters$letter_original[i]
  
  i <- match(temp$number1_masked, df.numbers$number1_masked)
  temp$number1_original <- df.numbers$number_original[i]
  
  i <- match(temp$number2_masked, df.numbers$number2_masked)
  temp$number2_original <- df.numbers$number_original[i]
  
  i <- match(temp$number3_masked, df.numbers$number3_masked)
  temp$number3_original <- df.numbers$number_original[i]
  
  i <- match(temp$number4_masked, df.numbers$number4_masked)
  temp$number4_original <- df.numbers$number_original[i]
  
  temp$original_id[k] <- with(temp, paste0(letter1_original[k],
                                           letter2_original[k],
                                           number1_original[k],
                                           number2_original[k],
                                           number3_original[k],
                                           number4_original[k]))
  matched_ids <- temp[,c("original_id","research_id")]
  return(matched_ids)  
}
test <- c("ab1234", "dg5678", NA)
mask_ids(test)
unmask_ids(c("rid_sk7678","rid_he5763"))
