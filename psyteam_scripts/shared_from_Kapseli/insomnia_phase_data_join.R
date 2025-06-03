# insomnia_phase_data_join.R

datadir <- "W:/data/HUS/unettomuuden_nettiterapia/"
d_t <- vroom::vroom(paste0(datadir, "FD_4810_hf_uni_04_therapy.csv")) # Therapy data table
d_pi <- vroom::vroom(paste0(datadir, "FD_4810_hf_uni_03_patientidentification.csv")) # FIDs
d_tp <- vroom::vroom(paste0(datadir, "FD_4810_hf_uni_05_therapyphase.csv")) # TherapyPhase data table
# d_tpi <- vroom::vroom(paste0(datadir, "FD_4810_hf_uni_06_therapyphaseinquiry.csv")) # TherapyPhaseInquiry data table
# d_tpiq <- vroom::vroom(paste0(datadir, "FD_4810_hf_uni_07_therapyphaseinquiryquestion.csv")) # TherapyPhaseInquiryQuestion data table
# d_tptq <- vroom::vroom(paste0(datadir, "FD_4810_hf_uni_09_therapyphasetaskquestion.csv")) # TherapyPhaseTaskQuestion data table
# d_patient <- vroom::vroom(paste0(datadir, "FD_4810_hf_uni_02_patient.csv")) # Patient data table

d_t$FID <- as.character(sapply(d_t$PatientId, function(x) d_pi$FID[d_pi$PatientId == x]))

# load("all_therapies_with_hilmo_ses_kela.Rdata")
# d <- d[(d$treatment_type == "Insomnia")&(!is.na(d$treatment_type)), ]

library(magrittr)
library(dplyr)
dtmp <- d_t %>% left_join(., d_tp, by = "TherapyId")
# Remember to separate two therapies for the same patient!
d$truly_maxses <- sapply(d$FID, function(x) max(dtmp$Phase[dtmp$FID == x], na.rm = T))

sum(table(d_t$LatestPhase))
sum(table(a))
