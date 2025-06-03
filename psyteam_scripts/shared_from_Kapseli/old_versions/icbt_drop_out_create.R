# Creation of drop-out variables to iCBT data
# Original timing data created at W:/Sanna/dates_and_times2.R

library(dplyr)


# Load data
load("all_ses_times2.Rdata")

# Remove unnecessary helper columns
ses_times <- all_ses_times2[ , c("FID", "treatment_id", "treatment", "platform",
                                 "session", "ses_time_corrected")]

names(ses_times)[names(ses_times) == "ses_time_corrected"] <- "ses_time"


# Maximum session number for each patient
ses_times <- ses_times %>%
  group_by(treatment_id) %>%
  mutate(max_ses = max(session)) %>%
  ungroup()

# Change the follow-up session number into last treatment session in max_ses
ses_times <- ses_times %>%
  mutate(max_ses = case_when(treatment == "alc" & max_ses == 6 ~ 5L,
                             treatment == "bip" & max_ses == 15 ~ 14L,
                             treatment == "bul" & max_ses == 9 ~ 8L,
                             treatment == "dep" & max_ses == 8 ~ 7L,
                             treatment == "gad" & max_ses == 13 ~ 12L,
                             treatment == "ocd" & max_ses == 11 ~ 10L,
                             treatment == "pan" & max_ses == 11 & platform == "old" ~ 10L,
                             treatment == "pan" & max_ses == 9 & platform == "new" ~ 8L,
                             treatment == "pit_old" & max_ses == 7 ~ 6L,
                             treatment == "pit_new" & max_ses == 11 ~ 10L,
                             treatment == "soc" & max_ses == 8 ~ 7L,
                             treatment == "y-soc" & max_ses == 9 ~ 8L,
                             TRUE ~ max_ses))



# Drop-out variables

# Has the patient finished all treatment sessions?
ses_times <- ses_times %>%
  mutate(drop_out_100 = case_when(treatment == "alc" & max_ses < 5 ~ 1,
                             treatment == "bip" & max_ses < 14 ~ 1,
                             treatment == "bul" & max_ses < 8 ~ 1,
                             treatment == "dep" & max_ses < 7 ~ 1,
                             treatment == "gad" & max_ses < 12 ~ 1,
                             treatment == "ocd" & max_ses < 10 ~ 1,
                             treatment == "pan" & platform == "old" & max_ses < 10 ~ 1,
                             treatment == "pan" & platform == "new" & max_ses < 8 ~ 1,
                             treatment == "pit_old" & max_ses < 6 ~ 1,
                             treatment == "pit_new" & max_ses < 10 ~ 1,
                             treatment == "soc" & max_ses < 7 ~ 1,
                             treatment == "y-soc" & max_ses < 8 ~ 1,
                             TRUE ~ 0))

# Has the patient finished 70 % of the treatment sessions? # TÄHÄN <= 
ses_times <- ses_times %>%
  mutate(drop_out_70 = case_when(treatment == "alc" & max_ses < 3 ~ 1,
                                  treatment == "bip" & max_ses < 9 ~ 1,
                                  treatment == "bul" & max_ses < 5 ~ 1,
                                  treatment == "dep" & max_ses < 4 ~ 1,
                                  treatment == "gad" & max_ses < 8 ~ 1,
                                  treatment == "ocd" & max_ses < 7 ~ 1,
                                  treatment == "pan" & platform == "old" & max_ses < 7 ~ 1,
                                  treatment == "pan" & platform == "new" & max_ses < 5 ~ 1,
                                  treatment == "pit_old" & max_ses < 4 ~ 1,
                                  treatment == "pit_new" & max_ses < 4 ~ 1,
                                  treatment == "soc" & max_ses < 4 ~ 1,
                                  treatment == "y-soc" & max_ses < 5 ~ 1,
                                  TRUE ~ 0))


# For drop-out data, keep only one row per patient
# Drop unnecessary columns also
icbt_drop_out <- ses_times[ , c("FID", "treatment_id", "treatment", "platform",
                                "max_ses", "drop_out_100", "drop_out_70")]

icbt_drop_out <- distinct(icbt_drop_out)

# Compare with Tom's data
load("W:/R_scripts_and_preprocessed_data/all_therapies_harmonized_data.Rdata")

table(d$registry)

d <- d[d$registry %in% c("new iCBT", "old iCBT"), ]

length(unique(d$treatment_id))
length(unique(icbt_drop_out$treatment_id))

# Tom's data has more cases
# Are these patient's lacking baseline measure (never started the treatment)?
sum(is.na(d$BL)) # 3227, at least these should be missing from icbt_drop_outs

d <- d[d$treatment_id %in% icbt_drop_out$treatment_id, ]
icbt_temp <- icbt_drop_out[icbt_drop_out$treatment_id %in% d$treatment_id, ]

table(d$dropout)
table(icbt_temp$drop_out_100)
sum(is.na(d$dropout)) # 1876

# Calculate percentages of drop-outs based on tables
13812/(13812+14048) # 0.4957645
14783/(14783+14953) # 0.4971415 
# About the same


table(d$dropout2)
table(icbt_temp$drop_out_70)
sum(is.na(d$dropout2)) # 4844

# Calculate percentages of drop-outs based on tables
10401/(10401+14491) # 0.4178451
10516/(10516+19220) # 0.3536454 
# Bigger difference in this variable
# Could be to do with rounding (which session is counted in in the 70% threshold)
# Also the more accurate session timing could influence



# Look at depression patients only
d_dep <- d[d$treatment_type %in% c("Masennuksen nettiterapia", "masennus"), ]
t_dep <- icbt_temp[icbt_temp$treatment == "dep", ]

table(d_dep$dropout)
table(t_dep$drop_out_100)
sum(is.na(d_dep$dropout)) # 725

# Calculate percentages of drop-outs based on tables
4659/(4659+5627) # 0.4529458
5087/(5087+5924) # 0.4619926 
# Close


table(d_dep$dropout2)
table(t_dep$drop_out_70)
sum(is.na(d_dep$dropout2)) # 725

# Calculate percentages of drop-outs based on tables
3993/(3993+6293) # 0.3881976
3071/(3071+7940) # 0.2789029


# Save for now
save(icbt_drop_out, file = "icbt_drop_out.Rdata")





