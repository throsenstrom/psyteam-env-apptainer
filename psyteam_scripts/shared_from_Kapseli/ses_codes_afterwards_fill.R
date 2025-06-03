# Filling out missing values in the ses-variable
# 10.6.2024 / SM

library(dplyr)

################ Statistics Finland data ####################
# Tom's code

load("all_treatments_and_controls_with_hilmo.Rdata")
load("relatives_data.Rdata")

d_ses <- vroom::vroom("W:/data/Tilastokeskus/FD_4810_sose_findata_4810.csv")
d_ses2 <- vroom::vroom("W:/data/Data_THL_4810_14.02.00_2020_korjaus2_TK/TK_u1587_p_150523/FD_4810_2022_sose_u1587_p_150523.csv")
d_ses <- rbind(d_ses, d_ses2)
rm(d_ses2); gc()
names(d_ses)[names(d_ses) == "vuosi"] <- "ses_year"


library(foreach)
# SLOWER with parallel computation! (probably would need larger chunks)
# -> compute in separate session


t0 <- proc.time()
dtmp <- foreach(ii = 1:nrow(d), .combine = rbind) %do% {
  tmpFID <- d$FID[ii]
  ds <- d_ses[d_ses$FID == tmpFID, ]
  dr <- d_suku[d_suku$FID_kantahenkilo == tmpFID, ]
  ds_father <- d_ses[d_ses$FID == dr$FID_sukulainen[dr[,"Sukul-\nsuhde"] == "3i"][1], ]
  ds_mother <- d_ses[d_ses$FID == dr$FID_sukulainen[dr[,"Sukul-\nsuhde"] == "3a"][1], ]
  ind_index <- which.min(abs(lubridate::year(d$index_date[ii]) - 
                               ds$ses_year))[1]
  ind_last <- which.max(ds$ses_year)[1]
  ind_mother <- which.max(ds_mother$ses_year)[1]
  ind_father <- which.max(ds_father$ses_year)[1]
  data.frame(FID = tmpFID,
             ses = ds$sose[ind_index][1],
             ses_last = ds$sose[ind_last][1],
             pses = ds$psose[ind_index][1],
             pses_last = ds$psose[ind_last][1],
             ses_year = ds$ses_year[ind_index][1],
             ses_year_last = ds$ses_year[ind_last][1],
             ses_mother = ds_mother$sose[ind_mother][1],
             ses_father = ds_father$sose[ind_father][1],
             pses_mother = ds_mother$psose[ind_mother][1],
             pses_father = ds_father$psose[ind_father][1],
             ses_mother_year = ds_mother$ses_year[ind_mother][1],
             ses_father_year = ds_father$ses_year[ind_father][1]
  )
}
(runtime <- proc.time() - t0) # ii 1:1000 took 814.5 s w 1 core, 1052 s w 4 c
# Whole loop ~ 26 hours / SM
# stopCluster(cl)

# Check
sum(is.na(dtmp$ses)) # 1208

# Compare with the old version to ma
dtmp2 <- dtmp
load("W:/R_scripts_and_preprocessed_data/SES_codes_to_be_attached.Rdata")

sum(is.na(dtmp$pses))
sum(is.na(dtmp$pses))

sum(is.na(dtmp$ses_last))
sum(is.na(dtmp$ses_last))

sum(is.na(dtmp$ses_mother))
sum(is.na(dtmp$ses_mother))

sum(is.na(dtmp$ses_father))
sum(is.na(dtmp$ses_father))

# These appear to match

# Is ses the same as last_ses for everyone?
dtmp2 <- dtmp2 %>%
  mutate(same_ses = case_when(ses == ses_last ~ TRUE,
                              TRUE ~ FALSE))

table(dtmp2$same_ses, useNA = "always")
not_same <- dtmp2[dtmp2$same_ses == FALSE, ]

rm(not_same)

# 15970 have different value, rest have the same
# Apparently not many changes in ses between treatment time and last reported


# Save the new version
dtmp <- dtmp2
rm(dtmp2)
dtmp <- dtmp[, 1:13]

save(dtmp, runtime, file = "SES_codes_to_be_attached.Rdata")

#load("SES_codes_to_be_attached.Rdata")
names(dtmp)[names(dtmp) == "FID"] <- "double_check_FID"
d <- cbind(d, dtmp)
any(d$FID != d$double_check_FID) # Still no disagreements / SM
#save(d, file = "all_treatments_with_hilmo_and_ses_data.Rdata")



################ Combine new ses codes with the latest version of the "d" data frame ################
rm(d)

load("W:/R_scripts_and_preprocessed_data/all_treatments_and_controls_with_hilmo_ses_kela.Rdata")
load("SES_codes_to_be_attached.Rdata")

# Loop to the existing variable
dtmp2 <- distinct(dtmp)
length(unique(dtmp2$FID))
fids <- unique(d$FID)

for (i in 1:length(fids)) {
  id <- fids[i]
  d$ses[d$FID == id] <- dtmp2$ses[dtmp2$FID == id]
}

sum(is.na(d$ses)) # Same as before

# Save 
save(d, file = "all_treatments_and_controls_with_hilmo_ses_kela.Rdata")









