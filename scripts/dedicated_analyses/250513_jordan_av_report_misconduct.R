# Load Data ####
conduct <- readRDS("data/processed/processing_layer_1/conduct.Rds")
randassign <- readRDS("data/processed/randassign.Rds")

table(conduct$wave, conduct$date_datapull)
table(randassign$treatment_wave)

# Deduplicate house
conduct2 <- conduct |>
  # Create a group key based on all columns EXCEPT the three exceptions
  group_by(across(-c(wave, date_datapull, control_number_pull))) |>

  # Keep only the row with the earliest date_datapull in each group
  slice_min(order_by = date_datapull, with_ties = FALSE) |>

  ungroup()

temp <- left_join(conduct, randassign)

