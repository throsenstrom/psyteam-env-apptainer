# collect_wide_data_on_patients_and_controls.R
# Tom R, Sanna M, Aino P, Jaakko T / 29.4.2024

### TO-DO:
# 1) Treatment timings from 1st and last patient-created record (d_answers-taulu?)
# 2) Timings for 1st and last primary-measure response, as before
# 3) Last-observation-carried-forward follow-up measure
# 4) Not started -muuttuja
# 5) FPQR-muuttuja LaRe- lyhyt/pitka ja lasten/nuorten/aikuisten
# 6) Ammatillisen kuntoutuksen muuttuja?

############################################################################
######### First collect all FIDs and treatment ids (NA for non-patient) ####
############### (Later use these to collect treatment data)  ###############
############################################################################

#### new-HUS-iCBT patients ####
# load data
ddir1 <- "W:/data/Data_THL_1303_140600_2023_osa1/Data_THL_1303_140600_2023_osa1/"
# ddir2 <- "W:/data/Data_THL_1303_140600_2023_osa2/Data_THL_1303_140600_2023_osa2/" # NOT YET NEEDED
d_participants <- vroom::vroom(paste0(ddir1, "HUS/FD_1303_nettiterapia_osallistujat.csv"))
d <- d_participants[,c("FID", "hoidon_id")]
names(d) <- c("FID", "treatment_id")
d <- cbind(d, registry = "iCBT_new")
rm(ddir1, d_participants)

#### Insomnia iCBT ####
datadir <- "W:/data/HUS/unettomuuden_nettiterapia/"
d_t <- vroom::vroom(paste0(datadir, "FD_4810_hf_uni_04_therapy.csv")) # Therapy data table
patient_identification <- vroom::vroom(
  paste0(datadir, "FD_4810_hf_uni_03_patientidentification.csv")) # FIDs

dtmp <- dplyr::inner_join(patient_identification, d_t[,c("PatientId", "TherapyId")],
                         by = "PatientId")
dtmp <- dtmp[,c("FID", "TherapyId")]
names(dtmp)[2] <- "treatment_id"
d <- rbind(d, cbind(dtmp, registry = "iCBT_Insomnia"))
rm(dtmp, d_t, datadir, patient_identification)

d$treat_id_with_doubles <- d$treatment_id

#### Old iCBT ####

d_list <- list("W:/data/HUS/vanha_nettiterapia/masennus/FD_4810_masennus_Potilaat.csv",
               "W:/data/HUS/vanha_nettiterapia/gad/FD_4810_gad_Potilaat.csv",
               "W:/data/Data_THL_4810_140200_2020_osa4/HUS/vanha_nettiterapia/ocd/FD_4810_ocd_Potilaat.csv",
               "W:/data/Data_THL_4810_140200_2020_osa4/HUS/vanha_nettiterapia/alko/FD_4810_alko_Potilaat.csv",
               "W:/data/Data_THL_4810_140200_2020_osa4/HUS/vanha_nettiterapia/bipo/FD_4810_bipo_Potilaat.csv",
               "W:/data/Data_THL_4810_140200_2020_osa4/HUS/vanha_nettiterapia/paniikki/FD_4810_paniikki_Potilaat.csv",
               "W:/data/Data_THL_4810_140200_2020_osa4/HUS/vanha_nettiterapia/sosfob/FD_4810_sosfob_Potilaat.csv")
for (i in 1:length(d_list)){
  dtmp <- vroom::vroom(d_list[[i]])
  dtmp <- dtmp[,c("FID", "PatientUserName")]
  names(dtmp)[names(dtmp) == "PatientUserName"] <- "treat_id_with_doubles"
  dtmp$treatment_id <- dtmp$treat_id_with_doubles
  dtmp$treatment_id[duplicated(dtmp$treatment_id)] <- paste0(dtmp$treatment_id[duplicated(dtmp$treatment_id)],
                                                             "_B")
  dtmp$registry <- "iCBT_old"
  d <- rbind(d, dtmp[,c("FID","treatment_id", "registry", "treat_id_with_doubles")])
}
rm(d_list, dtmp)

################################################################################################
# HUOM 2.5.2024!!!
# Vanhan nettiterapian datassa joitain, joilla NA treatment_id muuttujassa
# Tarkista, puuttuuko näiltä treatment id myös alkuperäisissä datoissa (mutta kts. alin kohta)
# ongelmatapaukset: kts. Ainon tallentama data iCBT_old_treatment_id_NA samasta kansiosta
# (huomaa, että korjattuun treatment_id muuttujaan näille tulee arvoksi NA_B)
# Näistä ainakaan kaikki eivät myöskään näyttäisi olevan puuttuvia alkuperäisessä "d" -taulussa
################################################################################################


#### FPRQ ####

d_tre <- vroom::vroom("W:/data/HUS/bcb_psykoterapia_korjattuLaRe/FD_4810_bcb_treatment.csv", delim= ";") # treatment, patient, visit ja cohort kansiosta:
d_pat <- vroom::vroom("W:/data/HUS/bcb_psykoterapia_korjattuLaRe/FD_4810_bcb_patient.csv", delim= ";")  #  "Data_THL_4810_140200_2020_osa3"

nrow(dplyr::distinct(d_tre[,c("patient_id", "treatment_id")]))
sum(is.na(d_tre$treatment_id))

dtmp <- d_tre[,c("patient_id", "treatment_id")]
dtmp <- dplyr::inner_join(dtmp, d_pat[, c("FID", "patient_id")], by = "patient_id")
dtmp$registry <- "FPQR"
dtmp <- dtmp[,c("FID", "treatment_id", "registry")]
dtmp$treat_id_with_doubles <- dtmp$treatment_id
d <- rbind(d, dtmp)
rm(d_pat, d_tre)

# FPQR iCBT_Insomnia      iCBT_new      iCBT_old 
# 8844          4929         23625         10798 

length(unique(d$FID)) # 43 961 patients altogether

save(d, file = "all_treatments.Rdata")

######################################################
############# Combine controls #######################
######################################################

load("all_treatments.Rdata")
d$FID_index_patient <- d$FID

ctrl1 <- vroom::vroom("W:/data/DVV/FD_4810_Tulokset 2022-06-01 VERROKKI.csv")
ctrl2 <- vroom::vroom("W:/data/Data_THL_4810_140200_2020_korjaus1/DVV/FD_4810_Tulokset 2023-05-03 VERROKKI 6252.csv")
ctrl3 <- vroom::vroom("W:/data/Data_THL_4810_140200_2020_korjaus1/DVV/FD_4810_Tulokset 2023-05-03 VERROKKI 358.csv")
ctrl_rm <- vroom::vroom("W:/data/Data_THL_4810_140200_2020_korjaus1/DVV/FD_4810_Tulokset 2023-05-03 VERROKKI 6252_karsitut.csv")

ctrl1 <- ctrl1[!((ctrl1$FID_tutkimus_verrokkihenkilo %in% ctrl2$FID_tutkimus_verrokkihenkilo)|
                 (ctrl1$FID_tutkimus_verrokkihenkilo %in% ctrl3$FID_tutkimus_verrokkihenkilo)), ]

d_controls <- rbind(ctrl1, ctrl2, ctrl3)
# sum(d_controls$FID_tutkimushenkilo %in% ctrl_rm$FID_tutkimushenkilo) # 0
head(d_controls)
# d_controls <- d_controls[d_controls$`Ver-rokin nro`!=0, ]

# For all: Date of Birth, index date, sex, lang, municipality nro & name, marital status
d$index_date <- d$date_of_birth <- rep(as.Date(NA), nrow(d))
d$sex_dvv <- d$native_lang <- rep(as.character(NA), nrow(d))
d$control_no <- d$municipality_no <- rep(as.numeric(NA), nrow(d))
d$municipality <- rep(as.character(NA), nrow(d))

for (i in 1:nrow(d)){
  ii <- which(d_controls$FID_tutkimus_verrokkihenkilo == d$FID[i])
  if (length(ii)>0){
    d$index_date[i] <- lubridate::ymd(d_controls$Indeksipäivä[ii])
    d$date_of_birth[i] <- lubridate::ymd(d_controls$`Syntymä-päivä`[ii])
    d$sex_dvv[i] <- ifelse(d_controls$`Suku-
puoli`[ii]==1, "Male", "Female")
    d$native_lang[i] <- d_controls$`Äidin-
kieli`[ii]
    d$control_no[i] <- d_controls$`Ver-rokin nro`[ii]
    d$municipality_no[i] <- d_controls$`Ind.pv koti- kunta`[ii]
    d$municipality[i] <- d_controls$`Indeksipäivän
kotikunnan nimi`[ii]
  }
}

dtmp <- d_controls[d_controls$`Ver-rokin nro` != 0, ]
dtmp <- dtmp[, c("FID_tutkimus_verrokkihenkilo", "FID_tutkimushenkilo",
                 "Syntymä-päivä", "Indeksipäivä", "Äidin-
kieli", "Suku-
puoli", "Ind.pv koti- kunta", "Ver-rokin nro", "Indeksipäivän
kotikunnan nimi")]

names(dtmp) <- c("FID", "FID_index_patient", "date_of_birth", "index_date",
                 "native_lang", "sex_dvv", "municipality_no", "control_no", "municipality")

dtmp$treatment_id <- as.character(NA)
dtmp$treat_id_with_doubles <- as.character(NA)
dtmp$registry <- "control"
dtmp$sex_dvv <- plyr::mapvalues(dtmp$sex_dvv, from = c(1,2), to = c("Male","Female"))
dtmp$date_of_birth <- lubridate::ymd(dtmp$date_of_birth)
dtmp$index_date <- lubridate::ymd(dtmp$index_date)

d <- rbind(d, dtmp[,names(d)])
rm(dtmp, ctrl1, ctrl2, ctrl3, ctrl_rm, d_controls)
save(d, file = "all_treatments_and_controls.Rdata")

### REMOVE CONTROLS that are PATIENTS! ###
rids0 <- d$FID[d$registry=="control"]
rids0 <- sapply(rids0, function(x) any(d$registry[d$FID==x]!="control"))
rids0 <- d$FID[d$registry=="control"][rids0]
dtmp <- d[!((d$FID %in% rids0)&(d$registry=="control")), ]
d <- dtmp
save(d, file = "all_treatments_and_controls.Rdata")

### REMOVE PATIENTS NOT IN DVV-COLLECTION ###
d <- d[!is.na(d$control_no), ]
save(d, file = "all_treatments_and_controls.Rdata")

### REMOVE 33 with iCBT patient-treatments with control no 1 or 2 ###
afid <- d$FID[(d$registry!="control")&(d$control_no!=0)]
d <- d[!(d$FID %in% afid), ]
save(d, file = "all_treatments_and_controls.Rdata")

# Looks good!
table(d$control_no, d$registry, useNA = "always")

###################################################################
########## Add treatment registries - iCBTs #######################
###################################################################

load("all_treatments_and_controls.Rdata")
# Using Sanna's session-timed data: session_times_iCBT_create.R
load("session_times_iCBT.Rdata")
dst <- session_times_iCBT; rm(session_times_iCBT)

d$primary_score_bl <- d$primary_score_fu <- d$last_session <- 
  d$first_session <- as.numeric(NA)
d$treatment <- as.character(NA)
d$primary_measure <- as.character(NA)
d$uninitiated <- as.logical(NA)
d$first_session_time <- as.Date(NA)
d$last_session_time <- as.Date(NA)

# treatment id's to loop
tids <- unique(d$treatment_id[d$registry %in% c("iCBT_old","iCBT_new")])
tids <- na.omit(tids) # remove 1 missing treatment id
system.time({
  for (i in 1:length(tids)){
    rid <- which(d$treatment_id==tids[i])
    lid <- dst$treatment_id == tids[i]
    if (!any(lid)){
      d$uninitiated[rid] <- TRUE
    } else {
      d$uninitiated[rid] <- FALSE
      dtmp <- dst[lid, ]
      d$treatment[rid] <- dtmp$treatment[1]
      d$primary_measure[rid] <- dtmp$questionnaire[1]
      d$primary_score_bl[rid] <- dtmp$primary_sum[which.min(dtmp$session)]
      d$primary_score_fu[rid] <- dtmp$primary_sum[which.max(dtmp$session)]
      d$first_session[rid] <- min(dtmp$session)
      d$last_session[rid] <- max(dtmp$session)
      d$first_session_time[rid] <- dtmp$session_time_corrected[which.min(dtmp$session)]
      d$last_session_time[rid] <- dtmp$session_time_corrected[which.max(dtmp$session)]
    }
  }
}) # 17 min
# save(d, file = "all_treatments_and_controls_contd.Rdata")


###################################################################
########## Add treatment registries - FPQR ########################
###################################################################

load("all_treatments_and_controls_contd.Rdata")

#### Finnish Psychotherapy Quality Registry ####
# load data
d_for <- vroom::vroom("W:/data/HUS/bcb_psykoterapia_korjattuLaRe/FD_4810_bcb_form_data.csv", delim= ";") # "Data_THL_4810_140200_2020_osa4"    
d_tre <- vroom::vroom("W:/data/HUS/bcb_psykoterapia_korjattuLaRe/FD_4810_bcb_treatment.csv", delim= ";") # treatment, patient, visit ja cohort kansiosta:
d_pat <- vroom::vroom("W:/data/HUS/bcb_psykoterapia_korjattuLaRe/FD_4810_bcb_patient.csv", delim= ";")  #  "Data_THL_4810_140200_2020_osa3"
d_vis <- vroom::vroom("W:/data/HUS/bcb_psykoterapia_korjattuLaRe/FD_4810_bcb_visit.csv", delim= ";")

# all((d_full$treatment_id[d_full$registry=="FPQR"] %in% d_tre$treatment_id))
all((d$treatment_id[d$registry=="FPQR"] %in% d_tre$treatment_id))
# all((d_tre$treatment_id) %in% d_full$treatment_id[d_full$registry=="FPQR"])
all((d_tre$treatment_id) %in% d$treatment_id[d$registry=="FPQR"])
# all((d_tre$treatment_id %in% d$treatment_id))
all((d_tre$treatment_id %in% d_for$treatment_id))
all((d_for$treatment_id %in% d$treatment_id))
length(unique(d_for$treatment_id[!d_for$treatment_id %in% d_tre$treatment_id])) # 6 not in tre
length(unique(d$treatment_id[d$registry=="FPQR"][!d$treatment_id[d$registry=="FPQR"] %in% d_tre$treatment_id])) # all in tre

# Clean from d_for those not in d_tre
d_for <- d_for[d_for$treatment_id %in% d_tre$treatment_id, ]

# ADD NOT_STARTED variable for those with no data in d_for ()
nsid <- unique(d$treatment_id[d$registry=="FPQR"][!(d$treatment_id[d$registry=="FPQR"] %in% d_for$treatment_id)])
for (i in 1:length(nsid)) d$uninitiated[d$treatment_id == nsid[i]] <- TRUE
# save(d, file = "all_treatments_and_controls_contd.Rdata")
d <- d_for # next deal with this and later switch back to big d

#### FPQR ####

dataToBLFU_treatment <- function(dorig, items, dvis, septh=30, interrupt_ids=NULL, use_orig_lab=FALSE){
  # A sub-function to take first non-missing value
  FirstNonMissing <- function(x) ifelse(any(!is.na(x)),x[!is.na(x)][1],NA)
  
  # Take the item set only, plus unique TREATMENTS, not patients
  iset <- dorig[,c("patient_id",items,"treatment_id","visit_id","date_created")]
  if (use_orig_lab){
    # names(iset) <- c("id", items,"treatment_id","visit_id","cdate")
    names(iset) <- c("patient_id", items,"treatment_id","visit_id","cdate")
  } else {
    # names(iset) <- c("id", paste0("BL",1:length(items)),"treatment_id","visit_id","cdate")
    names(iset) <- c("patient_id", paste0("BL",1:length(items)),"treatment_id","visit_id","cdate")
  }
  iset$cdate <- lubridate::as_date(iset$cdate)
  uds <- unique(iset$treatment_id) # uds <- unique(iset$id) # unique patients -> treatments
  
  # # Find patients with multiple treatments and deal with them later
  # mts_ids <- sapply(uds, function(x) length(unique(dorig$treatment_id[dorig$patient_id == x])) > 1)
  # mts_ids <- uds[mts_ids]
  # uds <- setdiff(uds, mts_ids)
  
  # Take unique duplicated IDs
  # udis <- unique(setdiff(iset$id[duplicated(iset$id)], mts_ids))
  udis <- unique(iset$treatment_id[duplicated(iset$treatment_id)])
  
  # Patch together a data matrix with BL in columns preceeding FU
  ciset <- data.frame(matrix(NA,length(uds),ncol(iset)*2-1))
  if (use_orig_lab){
    names(ciset) <- c(names(iset),paste0(items, "FU"),"treatment_idFU","visit_idFU","cdateFU")
  } else {
    names(ciset) <- c(names(iset),paste0("FU",1:length(items)),"treatment_idFU","visit_idFU","cdateFU")
  }
  
  # set ciset column types a priori (R >= 4.0)
  for (i in 2:ncol(iset)) class(ciset[[i]]) <- class(ciset[[i+ncol(iset)-1]]) <- class(iset[[i]])
  class(ciset[[1]]) <- class(iset[[1]])
  # sapply(ciset, class)
  
  for (i in 1:length(uds)){
    dtmp <- iset[iset$treatment_id == uds[i],] #dtmp <- iset[iset$id == uds[i],]
    ciset$treatment_id[i] <- uds[i]
    ciset$patient_id[i] <- dtmp$patient_id[1] # ciset$id[i] <- uds[i]
    # E.g. audit items wind up to multiple rows: take first non-empty
    # A potential problem could be distinct treatment/visit ids for missing values!
    ciset[i,2:ncol(iset)] <- cbind(data.frame(lapply(dtmp[dtmp$cdate==min(dtmp$cdate),
                                                          2:(ncol(dtmp)-1)],FirstNonMissing)),
                                   cdate = min(dtmp$cdate)) # APPLY changed to LAPPLY
    if (uds[i] %in% udis){
      ciset[i,(ncol(iset)+1):ncol(ciset)] <- cbind(data.frame(lapply(
        dtmp[dtmp$cdate==max(dtmp$cdate),2:(ncol(dtmp)-1)],FirstNonMissing)),
        cdate = max(dtmp$cdate))
    }
  }
  
  # # Add patients with multiple treatments / SHOULD HAPPEN AUTOMATICALLY NOW
  # if (length(mts_ids) > 0) {
  #   for (i in 1:length(mts_ids)){
  #     treatments <- unique(dorig$treatment_id[dorig$patient_id == mts_ids[i]])
  #     ntreatments <- length(treatments)
  #     for (j in 1:ntreatments){
  #       ciset <- rbind(ciset,rep(NA,ncol(ciset)))
  #       dtmp <- iset[(iset$id == mts_ids[i])&(iset$treatment_id == treatments[j]),]
  #       ciset$id[nrow(ciset)] <- mts_ids[i]
  #       # E.g. audit items wind up to multiple rows: take first non-empty
  #       ciset[nrow(ciset),2:ncol(iset)] <-
  #         cbind(data.frame(lapply(dtmp[dtmp$cdate==min(dtmp$cdate),2:(ncol(dtmp)-1)],
  #                                 FirstNonMissing)), cdate = min(dtmp$cdate))
  #       if (length(unique(dtmp$cdate))>1){
  #         ciset[nrow(ciset), (ncol(iset)+1):ncol(ciset)] <-
  #           cbind(data.frame(lapply(dtmp[dtmp$cdate==max(dtmp$cdate),2:(ncol(dtmp)-1)],
  #                                   FirstNonMissing)), cdate = max(dtmp$cdate))
  #       }
  #     }
  #   }
  # }
  
  # Delete entries with non-matching visit data (i.e. initial visits recorded after midterm/final considered NA)
  for (i in 1:nrow(ciset)){
    vtypeBL <- dvis$visit_type_id[((!is.na(ciset$treatment_id[i])&(!is.na(ciset$visit_id[i]))))&
                                    (ciset$treatment_id[i]==dvis$treatment_id)&(ciset$visit_id[i] == dvis$visit_id)]
    vtypeFU <- dvis$visit_type_id[((!is.na(ciset$treatment_idFU[i])&(!is.na(ciset$visit_idFU[i]))))&
                                    (ciset$treatment_idFU[i]==dvis$treatment_id)&(ciset$visit_idFU[i] == dvis$visit_id)]
    if (length(vtypeBL)!=0){
      if (!is.na(vtypeBL)){
        if (vtypeBL != "initial"){ciset[i,2:ceiling(ncol(ciset)/2)] <- NA}
      }
    }
    if (length(vtypeFU)!=0){
      if ((!is.na(vtypeFU))&(!is.na(ciset$treatment_idFU[i]))){
        if (vtypeFU == "initial"){ciset[i,(ceiling(ncol(ciset)/2)+1):ncol(ciset)] <- NA}
      }
    }
  }
  
  # Delete follow-up entries separated by <= septh days (set as missing follow-up data)
  # ciset$cdate <- lubridate::as_date(ciset$cdate)
  # ciset$cdateFU <- lubridate::as_date(ciset$cdateFU)
  ind <- ((!is.na(ciset$cdateFU)) & (!is.na(ciset$cdate))) &(ciset$cdateFU - ciset$cdate <= septh)
  ciset[ind,(ncol(iset)+1):ncol(ciset)] <- NA
  
  # Delete follow-up entries of interrupted therapies (set as missing follow-up data)
  if (!is.null(interrupt_ids)){
    ind <- ciset$treatment_id %in% interrupt_ids
    ciset[ind,(ncol(iset)+1):ncol(ciset)] <- NA
  }
  
  # Replace working id label with original and return data
  # names(ciset)[names(ciset)=="id"] <- "patient_id"
  return(ciset)
}

build_FPQR_treatment_data <- function(d, d_tre, d_pat, d_vis){
  # Creates tidy FPQR data frame with scores from the first therapy only -> all treatments / Tom R. 3.8.2023 -> 16.5.2024
  # Defining sessions 1 & two for short and 1,2,3 for long therapies / Tom R. 16.5.2024
  library(tidyr)
  library(dplyr) # this needed separately. Why?
  tids <- unique(d$treatment_id) # unique treatment ids
  # (Remove) patients in KELA treatments
  pmuoto_tids <- unique(d$treatment_id[(d$string_answer == "kela") & (d$question_code == "palvelumuoto")])
  tids <- setdiff(tids, pmuoto_tids) # poimitaan vain ne jotka eivat ole kelaterapioissa, n= 7833
  
  ### Now DON'T Restrict to adults (6524) but make a variable for them ###
  # uids <- uids[uids %in% d$patient_id[(d$question_code == 
  #                                        "erikoisala") & (d$string_answer == "aikuisten")]]
  tids_adults <- tids[tids %in% d$treatment_id[(d$question_code == 
                                         "erikoisala") & (d$string_answer == "aikuisten")]]
  tids_kids <- tids[tids %in% d$treatment_id[(d$question_code == 
                                                "erikoisala") & (d$string_answer == "lasten")]]
  tids_adolescents <- tids[tids %in% d$treatment_id[(d$question_code == 
                                              "erikoisala") & (d$string_answer == "nuorten")]]
  
  # Uninitiated therapies, 2718
  tids_uninitiated <- tids[!(tids %in% dplyr::filter(d, (template_code == "psykoterapeutin_alkuarvio_nuoret") | 
                                                       (template_code == "psykoterapeutin_alkuarvio_aikuiset") | 
                                                       (template_code == "psykoterapeutin_alkuarvio_lapset"))$treatment_id)]
  
  # Interrupted therapies, 237 
  dtmp <- dplyr::filter(d, (question_code== "psykoterapia_paattyi") | (question_code== "therapy_interruption")) 
  tids_interrupted <- tids[(tids %in% dtmp$treatment_id[dtmp$string_answer %in% c("keskeytyi", "before_it_started", "without_final_termination")])]
  
  # # interrupted adults, 188
  # uids_interrupted <- uids_interrupted[uids_interrupted %in% dplyr::filter(d, template_code == "psykoterapeutin_alkuarvio_aikuiset")$patient_id]
  
  # completed psychotherapies, 4085
  tids_completed <- tids[(tids %in% dplyr::filter(d, (template_code == "psykoterapeutin_loppu_valiarvio_nuoret") |
                                                    (template_code == "psykoterapeutin_loppu_valiarvio_aikuispotilaat") |
                                                    (template_code == "psykoterapeutin_loppu_valiarvio_lapset"))$treatment_id)]
  # check how numbers change when requiring final/midterm stamp too
  tids_completed_stamped <- rep(F, length(tids_completed)) 
  tids_midterm_only_stamped <- rep(F, length(tids_completed))
  for (i in 1:length(tids_completed)) {
    trids <- unique(d$treatment_id[d$treatment_id == tids_completed[i]])
    tids_completed_stamped[i] <- any(d_vis$visit_type_id[d_vis$treatment_id %in% trids] %in% c("final", "midterm"))
    tids_midterm_only_stamped[i] <- any(d_vis$visit_type_id[d_vis$treatment_id %in% trids] == "midterm") &
      !any(d_vis$visit_type_id[d_vis$treatment_id %in% trids]== "final")
  }
  tids_midterm_only <- tids_completed[tids_midterm_only_stamped] # 160
  tids_completed <- tids_completed[tids_completed_stamped] # 4085-122 = 3963
  # remove interrupted
  tids_completed <- setdiff(tids_completed, tids_interrupted) # 4085 -> 3859
  # tids_midterm_only <- setdiff(tids_midterm_only, tids_interrupted) # 58
  
  # ######## Then DON'T restrict to adults with one outsourced non-KELA therapy ######
  # # Find patients with multiple treatments
  # mts_ids <- sapply(uids_completed, function(x) length(unique(d$treatment_id[d$patient_id == x])) > 1)
  # mts_ids <- uids_completed[mts_ids]
  # uids_completed <- setdiff(uids_completed, mts_ids) # 3203-131= 3072
  # uids_midterm_only <- setdiff(uids_midterm_only, mts_ids)
  
  # DON'T remove internal psychotherapies, but make a variable later
  # int_ids <- sapply(uids_completed, function(x) ("sisainen" %in% d$string_answer[(d$patient_id == x) & (d$question_code == "palvelumuoto")]))
  int_ids <- sapply(tids, function(x) ("sisainen" %in% d$string_answer[(d$treatment_id == x) & (d$question_code == "palvelumuoto")]))
  int_ids <- tids[int_ids] #int_ids <- uids_completed[int_ids]
  # uids_completed <- setdiff(uids_completed, int_ids) # 3072-87= 2985
  # uids_midterm_only <- setdiff(uids_midterm_only, int_ids)
  
  ################## DATA TABLES ###################
  
  # # A function to drop the second treatment per table
  # drop_second_treatments <- function(x){
  #   dids <- x$patient_id[duplicated(x$patient_id)]
  #   lxcol_treat <- grepl("treatment_id", names(x))&(!grepl("treatment_idFU", names(x)))
  #   lxcol_date <- grepl("cdate", names(x))&(!grepl("cdateFU", names(x)))
  #   if (length(dids) > 0){
  #     treatments_to_remove <- c()
  #     for (i in 1:length(dids)){
  #       tids <- x[x$patient_id == dids[i], lxcol_treat|lxcol_date]
  #       ltcol_date <- grepl("cdate", names(tids))&(!grepl("cdateFU", names(tids)))
  #       if (all(!is.na(tids[,ltcol_date]))){
  #         treatments_to_remove <- c(treatments_to_remove,
  #                                   tids[tids[,ltcol_date] != min(tids[,ltcol_date]),
  #                                        grepl("treatment_id", names(tids))])
  #       } else {
  #         treatments_to_remove <- c(treatments_to_remove,
  #                                   unique(tids[,grepl("treatment_id", names(tids))])[-1])
  #       }
  #     }
  #     x <- x[!(x[,lxcol_treat] %in% treatments_to_remove), ]
  #     return(x)
  #   }
  # }
  
  ### Background info (new version 5.5.2023) ###
  dbag <- d %>%  # string_answer
    dplyr::filter(template_code == "taustatiedot") %>% 
    # select(patient_id, date_created, template_code, question_code, string_answer, number_answer) %>%
    select(treatment_id, date_created, template_code, question_code, string_answer, number_answer) %>% 
    group_by(question_code) %>% 
    mutate(row = row_number()) %>%  
    ungroup() %>%
    pivot_wider(names_from = question_code, values_from = string_answer) %>% 
    select(-row)
  
  # unique dbag
  dbagu <- data.frame(matrix("", length(tids), ncol(dbag)))
  names(dbagu) <- names(dbag)
  FirstNonMissing <- function(x) ifelse(any(!is.na(x)),x[!is.na(x)][1],NA)
  for (i in 1:length(tids)){
    dbagu$treatment_id[i] <- tids[i]
    linds <- dbag$treatment_id == tids[i]
    for (j in 2:ncol(dbag)){
      dbagu[i,j] <- FirstNonMissing(dbag[linds, j])
    }
  }
  
  # dbagu <- cbind(dbagu, many_treatments = 
  #                  sapply(dbagu$patient_id, function(x) (length(unique(d$treatment_id[d$patient_id == x])) > 1) * 1))
  rm(dbag)
  
  ## Add all side diagnoses from all the treatments of a patient
  dbagu <- cbind(dbagu, sivudiagnoosit = as.character(rep(NA, nrow(dbagu))))
  for (i in 1:length(tids)){
    sdxs <- na.omit(d$list_answer[(d$treatment_id == dbagu$treatment_id[i])&
                                    (d$question_code=="sivudiagnoosit")])
    if (length(sdxs) == 1){
      dbagu$sivudiagnoosit[i] <- sdxs
    }
    if (length(sdxs) > 1){
      sdxs <- stringr::str_split(paste0(sdxs, collapse = ","), ",")[[1]]
      sdxs <- paste0(unique(sdxs), collapse = ",")
    }
    # NOTE: Let's rather take the below on a need to know basis due to complications
    # sds <- na.omit(d$date_answer[(d$patient_id == dbagu$patient_id[i])&
    #                                 (d$question_code=="terapian_alkamispaiva")])
    # eds <- na.omit(d$date_answer[(d$patient_id == dbagu$patient_id[i])&
    #                                (d$question_code=="terapian_paattymispaiva")])
    # if (length(sds) >= 1){dbagu$terapian_alkamispaiva[i] <- sds[which.min(sds)]}
    # if (length(eds) >= 1){dbagu$terapian_paattymispaiva[i] <- eds[which.min(eds)]}
  }
  
  ### Start collecting the output data ###
  d_fpqr <- subset(dbagu, select = c(treatment_id, date_created, paadiagnoosi,
                                     sukupuoli, terapiamuoto, terapiasuuntaus,
                                     terapeutti, terapeutti_harmonized, 
                                     terapian_pituus, mista_lahetetty, sivudiagnoosit,
                                     therapy_interruption)) #, many_treatments))
  names(d_fpqr)[names(d_fpqr) == "date_created"] <- "date_created_bag"
  
  ### SOFAS ###
  dsof <- d %>% 
    dplyr::filter(template_code == "sofas") %>% 
    select(patient_id, date_created, template_code, question_code, number_answer, treatment_id, visit_id) %>% 
    group_by(question_code) %>% 
    mutate(row = row_number()) %>% 
    ungroup() %>% 
    pivot_wider(names_from = question_code, values_from = number_answer) %>% 
    select(-row)
  
  dsof <- subset(dsof, select = c(patient_id, sofas_asteikolla, vapaa_aika,
                                  tyo_tai_opiskelu, perhe_elama_ja_ihmissuhteet, 
                                  itsesta_huolehtiminen, date_created, treatment_id, visit_id))
  
  names(dsof) <- c("patient_id", "sofas", "freetime", "work", "family", "self", "date_created", "treatment_id", "visit_id")
  
  dsof <- dataToBLFU_treatment(dsof, items = names(dsof)[c(2:6)], d_vis, 
                             interrupt_ids = tids_interrupted, use_orig_lab = T)
  dsof <- dsof[!is.na(dsof$treatment_id),] # remove 109 rows that miss treatment id
  # names(dsof)[-1] <- paste0("sofas_", names(dsof)[-1])
  names(dsof)[!(names(dsof) %in% c("patient_id", "treatment_id"))] <- 
    paste0("sofas_", names(dsof)[!(names(dsof) %in% c("patient_id", "treatment_id"))])
  
  d_fpqr <- merge(d_fpqr, 
                  dsof, #drop_second_treatments(dsof), 
                  by = "treatment_id", all = T) #by = "patient_id", all = T)
  
  ##### CORE-OM and -10 #####
  
  dcom <- d %>% dplyr::filter(template_code == "core_om") %>% 
    select(patient_id, date_created, template_code, question_name, number_answer, 
           treatment_id, visit_id) %>% 
    group_by(question_name) %>% 
    mutate(row = row_number()) %>% 
    ungroup() %>% 
    dplyr::filter(question_name != "") %>% # drops values 'remoteActivation' 'savedFromMyHealth' 'savedFromTherapistRemote' 
    pivot_wider(names_from = question_name, values_from = number_answer) %>% 
    select(-row) 
  
  # Sort the CORE-OM items
  ilabs <- names(dcom)[grepl(" ", substr(names(dcom), 1, 3), fixed = T)]
  x <- sort(as.numeric(substr(ilabs, 1, 2)), index.return = T)
  dcom <- dcom[, c("patient_id", "date_created", ilabs[x$ix], 
                   "treatment_id", "visit_id")]
  # CORE-OM to 1st and last
  dcom <- dataToBLFU_treatment(dcom, items = names(dcom)[3:36], d_vis, interrupt_ids = tids_interrupted)
  # dcom <- drop_second_treatments(dcom) # drops nothing currently
  # names(dcom)[-1] <- paste0("coreom_", names(dcom)[-1])
  
  dcom <- cbind(dcom,
                coreom = ifelse(rowMeans(is.na(dcom[,paste0("BL",1:34)]))<=0.1,
                                10*rowMeans(dcom[,paste0("BL",1:34)], na.rm = T), NA),
                coreomFU = ifelse(rowMeans(is.na(dcom[,paste0("FU",1:34)]))<=0.1,
                                  10*rowMeans(dcom[,paste0("FU",1:34)], na.rm = T), NA))
  
  blitems <- c("BL2", "BL3", "BL7", "BL10", "BL15", "BL16", "BL18", "BL23", "BL27", "BL28")
  fitems <- c("FU2", "FU3", "FU7", "FU10", "FU15", "FU16", "FU18", "FU23", "FU27", "FU28")
  dcom <- cbind(dcom,
                core10 = ifelse(rowMeans(is.na(dcom[,blitems]))<=0.1,
                                10*rowMeans(dcom[,blitems], na.rm = T), NA),
                core10FU = ifelse(rowMeans(is.na(dcom[,fitems]))<=0.1,
                                  10*rowMeans(dcom[,fitems], na.rm = T), NA))
  
  # Combine scores etc. only
  dtmp <- dcom[, c("patient_id", "treatment_id", "visit_id", "cdate", 
                   "coreom", "coreomFU","core10","core10FU", 
                   "treatment_idFU", "visit_idFU", "cdateFU")]
  # names(dtmp)[-1] <- paste0("coreom_",names(dtmp)[-1])
  names(dtmp)[!(names(dtmp) %in% c("patient_id", "treatment_id"))] <- 
    paste0("coreom_",names(dtmp)[!(names(dtmp) %in% c("patient_id", "treatment_id"))])
  dtmp <- dtmp[!is.na(dtmp$treatment_id),] # remove 93 missing treatment ids
  
  d_fpqr <- merge(d_fpqr, dtmp, by = c("patient_id", "treatment_id"), all = T)
  
  ##### PHQ-9 #####
  dphq <- d %>% 
    dplyr::filter(template_code == "PHQ9") %>% 
    select(patient_id, date_created, template_code, question_code, number_answer, treatment_id, visit_id) %>%
    group_by(question_code) %>% 
    mutate(row = row_number()) %>% 
    ungroup() %>% 
    pivot_wider(names_from = question_code,  values_from = number_answer) %>% 
    select(-row)
  
  dphq <- dataToBLFU_treatment(dphq, items = names(dphq)[6:14], d_vis, interrupt_ids = tids_interrupted)
  # dphq <- drop_second_treatments(dphq)
  
  # CHANGED TO rowSums INSTEAD OF rowMeans!!!
  dphq <- cbind(dphq,
                phq = ifelse(apply(dphq[,paste0("BL",1:9)],1,function(x) !any(is.na(x))),
                             rowSums(dphq[,paste0("BL",1:9)]), NA),
                phqFU = ifelse(apply(dphq[,paste0("FU",1:9)],1,function(x) !any(is.na(x))),
                               rowSums(dphq[,paste0("FU",1:9)]), NA))
  
  # Combine scores etc. only
  dtmp <- dphq[, c("patient_id", "treatment_id", "visit_id", "cdate", 
                   "phq", "phqFU", 
                   "treatment_idFU", "visit_idFU", "cdateFU")]
  # names(dtmp)[-1] <- paste0("phq_",names(dtmp)[-1])
  names(dtmp)[!(names(dtmp) %in% c("patient_id", "treatment_id"))] <- 
    paste0("phq_",names(dtmp)[!(names(dtmp) %in% c("patient_id", "treatment_id"))])
  # names(dtmp)[names(dtmp)=="phq_treatment_id"] <- "treatment_id"
  dtmp <- dtmp[!is.na(dtmp$treatment_id),] # remove 157 missing treatment ids
  d_fpqr <- merge(d_fpqr, dtmp, by = c("patient_id", "treatment_id"), all = T)
  
  ### OASIS ###
  doasis <- d %>% dplyr::filter(template_code == "oasis") %>% 
    select(patient_id, date_created, template_code, question_code, number_answer, treatment_id, visit_id) %>%
    group_by(question_code) %>% 
    mutate(row = row_number()) %>% 
    ungroup() %>% 
    pivot_wider(names_from = question_code, values_from = number_answer) %>%
    select(-row)
  
  doasis <- dataToBLFU_treatment(doasis, items = names(doasis)[6:10], d_vis, interrupt_ids = tids_interrupted)
  # doasis <- drop_second_treatments(doasis)
  
  doasis <- cbind(doasis,
                  oasis = ifelse(apply(doasis[,paste0("BL",1:5)],1,function(x) !any(is.na(x))),
                                 rowSums(doasis[,paste0("BL",1:5)]), NA),
                  oasisFU = ifelse(apply(doasis[,paste0("FU",1:5)],1,function(x) !any(is.na(x))),
                                   rowSums(doasis[,paste0("FU",1:5)]), NA))
  # Combine scores etc. only
  dtmp <- doasis[, c("patient_id", "treatment_id", "visit_id", "cdate", 
                     "oasis", "oasisFU", 
                     "treatment_idFU", "visit_idFU", "cdateFU")]
  names(dtmp)[!(names(dtmp) %in% c("patient_id", "treatment_id"))] <- 
    paste0("oasis_",names(dtmp)[!(names(dtmp) %in% c("patient_id", "treatment_id"))])
  # names(dtmp)[names(dtmp)=="oasis_treatment_id"] <- "treatment_id"
  dtmp <- dtmp[!is.na(dtmp$treatment_id),] # remove 88 missing treatment ids
  d_fpqr <- merge(d_fpqr, dtmp, by = c("patient_id", "treatment_id"), all = T)
  
  ### AUDIT-C ###
  daudit <- d %>% 
    dplyr::filter(template_code == "audit_c") %>% 
    select(patient_id, date_created, template_code, question_code, number_answer, treatment_id, visit_id) %>%
    group_by(question_code) %>%
    mutate(row = row_number()) %>% 
    ungroup() %>% 
    pivot_wider(names_from = question_code, values_from = number_answer) %>% 
    select(-row)
  
  daudit <- dataToBLFU_treatment(daudit, items = names(daudit)[2 + c(4, 5, 8)], d_vis, interrupt_ids = tids_interrupted)
  # daudit <- drop_second_treatments(daudit)
  daudit <- cbind(daudit,
                  audit = rowSums(daudit[,paste0("BL",1:3)], na.rm = T),
                  auditFU = rowSums(daudit[,paste0("FU",1:3)], na.rm = T))
  # Combine scores etc. only
  dtmp <- daudit[, c("patient_id", "treatment_id", "visit_id", "cdate", 
                     "audit", "auditFU", "treatment_idFU", "visit_idFU", "cdateFU")]
  names(dtmp)[!(names(dtmp) %in% c("patient_id", "treatment_id"))] <- 
    paste0("audit_",names(dtmp)[!(names(dtmp) %in% c("patient_id", "treatment_id"))])
  # names(dtmp)[names(dtmp)=="audit_treatment_id"] <- "treatment_id"
  dtmp <- dtmp[!is.na(dtmp$treatment_id),] # remove 100 missing treatment ids
  d_fpqr <- merge(d_fpqr, dtmp, by = c("patient_id", "treatment_id"), all = T)
  
  ##### Suoma's therapy-type definitions #####
  # A function to detect primary care units as a source of referral
  IsAdultPrimaryCare <- function(id) {
    x <- d[d$treatment_id== id, ]
    from_primary_care <- ("aikuisten" %in% x$string_answer[x$question_code== "erikoisala"]) &
      ("ostopalvelu" %in% x$string_answer[x$question_code== "palvelumuoto"]) &
      (any(x$string_answer[x$question_code== "terapiaan_lahettava_yksikko"] %in% c("muu", "1126002", "2126001")) |
         ((length(x$string_answer[x$question_code== "mista_lahetetty"]) >0) &
            ("perusterveydenhuollosta" %in% x$string_answer[x$question_code== "mista_lahetetty"])))
    return(from_primary_care)
  }
  
  # A function to detect short adult therapies (modified 3.8.2023 & 16.5.2024)
  IsShortTherapy <- function(id){
    # x <- d[d$patient_id== id, ]
    x <- d[d$treatment_id== id, ]
    # short_therapy <- ("aikuisten" %in% x$string_answer[x$question_code== "erikoisala"]) &
    short_therapy <- ("lyhyt" %in% x$string_answer[x$question_code== "terapian_pituus"])
    return(short_therapy)
  }
  IsChildTherapy <- function(id){
    x <- d[d$treatment_id== id, ]
    child_therapy <- ("lasten" %in% x$string_answer[x$question_code== "erikoisala"])
    return(child_therapy)
  }
  IsAdolescentTherapy <- function(id){
    x <- d[d$treatment_id== id, ]
    adolescent_therapy <- ("nuorten" %in% x$string_answer[x$question_code== "erikoisala"])
    return(adolescent_therapy)
  }
  
  # # A function to detect internally-/HUS-delivered therapies (modified 3.8.2023 & 16.5.2024)
  # IsInternalTherapy <- function(id){
  #   # x <- d[d$patient_id== id, ]
  #   x <- d[d$treatment_id== id, ]
  #   # internal_therapy <- ("aikuisten" %in% x$string_answer[x$question_code== "erikoisala"]) & 
  #   internal_therapy <- ("sisainen" %in% x$string_answer[x$question_code== "palvelumuoto"])
  #   return(internal_therapy)
  # }
  
  # Then add definitions
  d_fpqr <- cbind(d_fpqr, adult_primary_care = rep(NA,nrow(d_fpqr)),
                  short_therapy = rep(NA,nrow(d_fpqr)),
                  internal_therapy = rep(NA,nrow(d_fpqr)),
                  child_therapy = rep(NA,nrow(d_fpqr)),
                  adolescent_therapy = rep(NA,nrow(d_fpqr)))
  for (i in 1:nrow(d_fpqr)){
    if (!is.na(d_fpqr$treatment_id[i])){
      d_fpqr$adult_primary_care[i] <- IsAdultPrimaryCare(d_fpqr$treatment_id[i])
      d_fpqr$short_therapy[i] <- IsShortTherapy(d_fpqr$treatment_id[i])
      # d_fpqr$internal_therapy[i] <- IsInternalTherapy(d_fpqr$treatment_id[i])
      d_fpqr$internal_therapy[i] <- d_fpqr$treatment_id[i] %in% int_ids
      d_fpqr$child_therapy[i] <- IsChildTherapy(d_fpqr$treatment_id[i])
      d_fpqr$adolescent_therapy[i] <- IsAdolescentTherapy(d_fpqr$treatment_id[i])
    }
  } # ~11 min.
  # save(d_fpqr, file = "FPQR_treatments_data.Rdata")
  return(d_fpqr)
}

load("FPQR_treatments_data.Rdata")
# fill missing patient id's from d_tre and check existing ones
mids <- d_fpqr$treatment_id[is.na(d_fpqr$patient_id)]
all(mids %in% d_tre$treatment_id) # TRUE
sum(!d_fpqr$patient_id %in% d_tre$patient_id, na.rm = T)

### ASSUME d_tre has correct patient ids ###
d_fpqr$patient_id <- sapply(d_fpqr$treatment_id, function(x) d_tre$patient_id[d_tre$treatment_id==x][1])
# save(d_fpqr, file = "FPQR_treatments_data.Rdata")

load("all_treatments_and_controls_contd.Rdata")
# treatment id's to loop
tids <- unique(d$treatment_id[d$registry %in% c("FPQR")])
system.time({
  for (i in 1:length(tids)){
    rid <- which(d$treatment_id==tids[i])
    lid <- d_fpqr$treatment_id == tids[i]
    if (!any(lid)){
      d$uninitiated[rid] <- TRUE
    } else {
      d$uninitiated[rid] <- FALSE
      dtmp <- d_fpqr[lid, ]
      d$treatment[rid] <- paste0("FPQR: ", dtmp$terapiasuuntaus[1])
      d$primary_measure[rid] <- "CORE-10"
      d$primary_score_bl[rid] <- dtmp$coreom_core10[1]
      d$primary_score_fu[rid] <- dtmp$coreom_core10FU[1]
      d$first_session[rid] <- 1
      # d$last_session[rid] <- ifelse(dtmp$short_therapy, 2, 3)
      d$first_session_time[rid] <- dtmp$coreom_cdate[1]
      d$last_session_time[rid] <- dtmp$coreom_cdateFU[1]
    }
  }
}) # 2.8 min

# save(d, file = "all_treatments_and_controls_contd.Rdata")

###############################################################
#################### Add Insomnia iCBT ########################
###############################################################

load("all_treatments_and_controls_contd.Rdata")

datadir <- "W:/data/HUS/unettomuuden_nettiterapia/"
d_t <- vroom::vroom(paste0(datadir, "FD_4810_hf_uni_04_therapy.csv")) # Therapy data table
d_tp <- vroom::vroom(paste0(datadir, "FD_4810_hf_uni_05_therapyphase.csv")) # TherapyPhase data table
d_tpi <- vroom::vroom(paste0(datadir, "FD_4810_hf_uni_06_therapyphaseinquiry.csv")) # TherapyPhaseInquiry data table
d_tpiq <- vroom::vroom(paste0(datadir, "FD_4810_hf_uni_07_therapyphaseinquiryquestion.csv")) # TherapyPhaseInquiryQuestion data table
d_tptq <- vroom::vroom(paste0(datadir, "FD_4810_hf_uni_09_therapyphasetaskquestion.csv")) # TherapyPhaseTaskQuestion data table
d_patient <- vroom::vroom(paste0(datadir, "FD_4810_hf_uni_02_patient.csv")) # Patient data table
patient_identification <- vroom::vroom(
  paste0(datadir, "FD_4810_hf_uni_03_patientidentification.csv")) # FIDs


build_iCBTI <- function(d_patient, d_t, d_tp, d_tpi, d_tpiq, d_tptq, remove_formula_rows = TRUE){
  library(magrittr)
  library(dplyr)
  d <- d_tpiq %>% rename(., iqTitle = Title) %>% left_join(., 
                                                           select(d_tpi, !(DateDone)), by = "InquiryId") %>% 
    rename(., iTitle = Title) %>% left_join(., d_tp[, c("PhaseId", 
                                                        "TherapyId", "Phase", "Duration")], 
                                            by = "PhaseId") %>% left_join(., d_t, by = "TherapyId") %>% 
    select(!(Title)) %>% left_join(., d_patient, by = "PatientId")
  if (remove_formula_rows) {
    return(d[!grepl("formula", d$Type), ])
  }
  else {
    return(d)
  }
}

tidy_up_iCBTI <- function(d_iCBTI, patient_identification){
  utids <- unique(d_iCBTI$TherapyId)
  N <- length(utids)
  d_tidy <- data.frame(FID = rep("",N), 
                       treatment_id = rep("",N),
                       iCBT = rep("Insomnia",N),
                       inventory = rep("ISI",N),
                       inventory_name = rep("ISI - Insomnia Severity Index",N),
                       BL = as.numeric(rep(NA,N)),
                       FU = as.numeric(rep(NA,N)),
                       first_session = as.integer(rep(NA,N)),
                       first_session_time = as.Date(rep(NA,N)),
                       last_session = as.integer(rep(NA,N)),
                       last_session_time = as.Date(rep(NA,N)),
                       first_measurement_time = as.Date(rep(NA,N)),
                       last_measurement_time = as.Date(rep(NA,N)))
  for(i in 1:N){
    d_tidy$treatment_id[i] <- utids[i]
    dtmp <- d_iCBTI[d_iCBTI$TherapyId == utids[i], ]
    phases <- sort(unique(dtmp$Phase[dtmp$iTitle == "Unettomuuden arvio"]))
    phases_all <- sort(unique(dtmp$Phase))
    d_tidy$FID[i] <- patient_identification$FID[patient_identification$PatientId 
                                                == dtmp$PatientId[1]]
    if (length(phases) > 0){
      tmpscores <- dtmp$Value[(dtmp$iTitle == "Unettomuuden arvio")&(dtmp$Phase == 1)]
      d_tidy$BL[i] <- sum(tmpscores, na.rm = T)
      # d_tidy$nitemsBL[i] <- sum(!is.na(tmpscores))
      # NB!, DateCreated is for the therapy and DateDone for the answer!
      d_tidy$first_measurement_time[i] <- as.Date(min(dtmp$DateDone[(dtmp$iTitle == "Unettomuuden arvio")&
                                                        (dtmp$Phase == 1)], na.rm=T))
      d_tidy$first_session[i] <- min(phases, na.rm = T)
      d_tidy$first_session_time[i] <- as.Date(min(dtmp$DateDone[dtmp$Phase == 1], na.rm=T))
      if (length(phases_all) > 1){
        maxses <- max(phases, na.rm=T)
        maxses_all <- max(phases_all, na.rm=T)
        tmpscores <- dtmp$Value[(dtmp$iTitle == "Unettomuuden arvio")&
                                  (dtmp$Phase == maxses)]
        d_tidy$FU[i] <- sum(tmpscores, na.rm = T)
        # d_tidy$nitemsFU[i] <- sum(!is.na(tmpscores))
        # d_tidy$dropout[i] <- ifelse(maxses < 5, 1, 0)
        d_tidy$last_session[i] <- maxses_all
        d_tidy$last_measurement_time[i] <- as.Date(min(dtmp$DateCreated[(dtmp$iTitle == "Unettomuuden arvio")&
                                                            (dtmp$Phase == maxses)], na.rm=T))
        d_tidy$last_session_time[i] <- as.Date(min(dtmp$DateCreated[dtmp$Phase == maxses], na.rm=T)) #VIRHE, pitäisi olla DateDone eikä DateCreated
      } else {
        # d_tidy$last_session[i] <- 1
      }
      # d_tidy$age[i] <- lubridate::interval(dtmp$DateOfBirth[1], d_tidy$cdate[i])/
      #   lubridate::years(1)
      # d_tidy$zBL <- (d_tidy$BL - mean(d_tidy$BL, na.rm = T)) / sd(d_tidy$BL, na.rm = T)
      # d_tidy$zFU <- (d_tidy$FU - mean(d_tidy$BL, na.rm = T)) / sd(d_tidy$BL, na.rm = T)
    }
  }
  # d_tidy$cdate[(!is.na(d_tidy$cdate))&(d_tidy$cdate == Inf)] <- NA
  # d_tidy$cdateFU[(!is.na(d_tidy$cdateFU))&(d_tidy$cdateFU == Inf)] <- NA
  # d_tidy$age[(!is.na(d_tidy$age))&(abs(d_tidy$age) == Inf)] <- NA
  return(d_tidy)
}

d_icbti <- build_iCBTI(d_patient, d_t, d_tp, d_tpi, d_tpiq, d_tptq)
d_icbti <- tidy_up_iCBTI(d_icbti, patient_identification)

# change to NA if no non-missing in min (resulting in Inf)
for (i in 1:ncol(d_icbti)) print(sum(is.infinite(d_icbti[,i])))
d_icbti$first_measurement_time[is.infinite(d_icbti$first_measurement_time)] <- NA # 3 cases only
d_icbti$first_session_time[is.infinite(d_icbti$first_session_time)] <- NA # 2 cases only
# save(d_icbti, file = "insomnia_iCBT.Rdata")

# Remove 7 duplicated and 24 empty treatment ids (other data duplicated too)
sum(d$treatment_id=="NA_B", na.rm = T)
head(d[(duplicated(d$treatment_id)|duplicated(d$treatment_id, fromLast = T))& !is.na(d$treatment_id),])
lid <- (!is.na(d$treatment_id))&((d$treatment_id=="NA_B")|(duplicated(d$treatment_id)))
d <- d[!lid, ]

tids <- unique(na.omit(d_icbti$treatment_id))
d$uninitiated[(d$registry=="iCBT_Insomnia")&(!(d$treatment_id %in% tids))] <- TRUE
d$treatment[(d$registry=="iCBT_Insomnia")] <- "iCBT_Insomnia"
for (i in 1:length(tids)){
  if (tids[i] %in% d$treatment_id){ # Only 4307 out of 4332 are included
    rid_d <- which(d$treatment_id == tids[i])
    rid_i <- which(d_icbti$treatment_id == tids[i])
    d$first_session[rid_d] <- d_icbti$first_session[rid_i]
    d$first_session_time[rid_d] <- d_icbti$first_session_time[rid_i]
    d$last_session[rid_d] <- d_icbti$last_session[rid_i]
    d$last_session_time[rid_d] <- d_icbti$last_session_time[rid_i]
    d$primary_score_bl[rid_d] <- d_icbti$BL[rid_i]
    d$primary_score_fu[rid_d] <- d_icbti$FU[rid_i]
    d$primary_measure[rid_d] <- d_icbti$inventory_name[rid_i]
  }
}

# save(d, file = "all_treatments_and_controls_contd.Rdata")

####################################################################
############### Combine non-patient registries #####################
####################################################################

load("all_treatments_and_controls_contd.Rdata")

### Load relatives
# rltv1 <- vroom::vroom("W:/data/DVV/FD_4810_Tulokset 2022-06-01 SUKU.csv")
# rltv2 <- vroom::vroom("W:/data/Data_THL_4810_140200_2020_korjaus1/DVV/FD_4810_Tulokset 2023-05-03 SUKU 6252.csv")
# rltv3 <- vroom::vroom("W:/data/Data_THL_4810_140200_2020_korjaus1/DVV/FD_4810_Tulokset 2023-05-03 SUKU 358.csv")
# names(rltv1) <- names(rltv2)
# d_suku <- rbind(rltv1, rltv2, rltv3)
# rm(rltv1, rltv2, rltv3); gc()
# save(d_suku, file = "relatives_data.Rdata")
load("relatives_data.Rdata")

### Load Hilmo data
d_hilmo9621 <- vroom::vroom("W:/data/THL/FD_4810_THL2022_4810_hilmo9621.csv") 
d_hilmo9621_2nd <- vroom::vroom("W:/data/Data_THL_4810_140200_2020_korjaus1/THL/FD_4810_THL2020_4810_hilmo9621.csv")
# mean(d_hilmo9621$FID %in% d_hilmo9621_2nd$FID) # no overlap
d_hilmo9621 <- rbind(d_hilmo9621, d_hilmo9621_2nd[,names(d_hilmo9621)])
rm(d_hilmo9621_2nd)
d_hilmo9621 <- subset(d_hilmo9621, select = c(FID, HILMOID, TUPVA, EA))
gc()

d_diag <- vroom::vroom("W:/data/THL/FD_4810_THL2022_4810_icd10.csv")
d_diag_2nd <- vroom::vroom("W:/data/Data_THL_4810_140200_2020_korjaus1/THL/FD_4810_THL2020_4810_icd10.csv")
d_diag <- rbind(d_diag, d_diag_2nd); rm(d_diag_2nd); 
d_diag <- subset(d_diag, select = -c(N, KENTTA)); gc()

### def functions ###
dvv_to_date <- function(x){
  x <- as.character(x)
  x <- lubridate::make_date(year = substr(x,1,4),
                            month = substr(x,5,6),
                            day = substr(x,7,8))
  return(x)
}

### Combine Hilmo data using parallel processing ###
library(foreach)
library(doSNOW)
cl <- makeCluster(4)
registerDoSNOW(cl)
t0 <- proc.time()
dtmp <- foreach(ii = 1:nrow(d), .combine = rbind) %dopar% {
# dtmp <- foreach(ii = 1:2, .combine = rbind) %do% { # test it
  dd <- d_diag[d_diag$FID == d$FID[ii], ] # index person diagnoses
  dh <- d_hilmo9621[d_hilmo9621$FID == d$FID[ii], ] # index person Hilmo
  dh$TUPVA <- as.Date(dh$TUPVA, format = "%d.%m.%Y")
  # dated diagnoses
  dd <- merge(dd, dh[,c("HILMOID", "TUPVA")], by = "HILMOID")
  # rm(dh)
  # dated hilmo psychiatry visits
  dh <- dh[dh$EA %in% c("70","70F","70X","70Z","74","75","75X"),]
  if (!is.na(d$first_session_time[ii])){
    dd_post <- dd[dd$TUPVA > d$first_session_time[ii], ]
    dd <- dd[dd$TUPVA <= d$first_session_time[ii], ]
    uds <- unique(dd$KOODI)
    uds_post <- unique(dd_post$KOODI)
    dh_post <- dh[dh$TUPVA > d$first_session_time[ii], ]
    dh <- dh[dh$TUPVA <= d$first_session_time[ii], ]
  } else {
    uds <- unique(dd$KOODI) # unique diagnoses for index person
    uds_post <- as.character(c())
    dd_post <- data.frame(matrix(nrow = 0, ncol = ncol(dd)))
    names(dd_post) <- names(dd)
    dh_post <- data.frame(matrix(nrow = 0, ncol = ncol(dh)))
    names(dh_post) <- names(dh)
  }
  # # control persons of the index person (control itself if index) # NO LONGER NEEDED
  # dc <- d_controls[(d_controls$FID_tutkimushenkilo == d$FID[ii])|
  #                    (d_controls$FID_tutkimus_verrokkihenkilo == d$FID[ii]), ] 
  # relatives of the index person
  dr <- d_suku[d_suku$FID_kantahenkilo == d$FID[ii], ]
  names(dr)[names(dr) == "Sukul-\nsuhde"] <- "relation"
  dr <- dr[dr$relation != 0, ]
  dd_r <- d_diag[d_diag$FID %in% dr$FID_sukulainen, ]
  dh_r <- d_hilmo9621[d_hilmo9621$FID %in% dr$FID_sukulainen, ]
  ds_father <- dd_r$KOODI[dd_r$FID == dr$FID_sukulainen[dr$relation == "3i"][1]]
  ds_mother <- dd_r$KOODI[dd_r$FID == dr$FID_sukulainen[dr$relation == "3a"][1]]
  ds_sibling <- dd_r$KOODI[
    dd_r$FID %in% dr$FID_sukulainen[dr$relation %in% c("4a","4i")]]
  uds_father <- unique(ds_father)
  uds_mother <- unique(ds_mother)
  uds_sibling <- unique(ds_sibling)
  ### Hilmo variables to be added ###
  data.frame(FID = d$FID[ii], 
             # year_of_birth = lubridate::year(dvv_to_date(dc$`Syntymä-päivä`[1])),
             # sex_dvv = dc[["Suku-\npuoli"]][1],
             n_diag_F = sum(grepl("F", uds)),
             n_diag_nonF = sum(!grepl("F", uds)),
             n_diag_F_post = sum(grepl("F", uds_post)),
             n_diag_nonF_post = sum(!grepl("F", uds_post)),
             diag_1st_F = with(dd[grepl("F",dd$KOODI),],
                               paste0(KOODI[which.min(TUPVA)][1],
                                      collapse = ",")),
             diag_last_F = with(dd[grepl("F",dd$KOODI),],
                                paste0(KOODI[which.max(TUPVA)][1],
                                       collapse = ",")),
             diag_1st_F_post = ifelse(nrow(dd_post)>0,
                                      with(dd_post[grepl("F",dd$KOODI),],
                                           paste0(KOODI[which.min(TUPVA)][1],
                                                  collapse = ",")), NA),
             psych_visit_1st_pre_date = with(dh,TUPVA[which.min(TUPVA)][1]),
             psych_visit_1st_post_date = with(dh_post,TUPVA[which.min(TUPVA)][1]),
             psych_visit_last_pre_date = with(dh,TUPVA[which.max(TUPVA)][1]),
             psych_visit_last_post_date = with(dh_post,TUPVA[which.max(TUPVA)][1]),
             n_psych_visit_pre = nrow(dh),
             n_psych_visit_post = nrow(dh_post),
             diag_1st_F_date = with(dd[grepl("F",dd$KOODI),],
                                    TUPVA[which.min(TUPVA)][1]),
             diag_last_F_date = with(dd[grepl("F",dd$KOODI),],
                                     TUPVA[which.max(TUPVA)][1]),
             diag_1st_F_date_post = lubridate::as_date(ifelse(nrow(dd_post)>0,
                                                              with(dd_post[grepl("F",dd$KOODI),],
                                                                   TUPVA[which.min(TUPVA)][1]), NA)),
             diag_F_most_freq = ifelse(!is.null(names(which.max(table(dd$KOODI[grepl("F", dd$KOODI)])))[1]),
                                       names(which.max(table(dd$KOODI[grepl("F", dd$KOODI)])))[1], 
                                       as.character(NA)),
             diag_F_most_freq_post = ifelse(!is.null(names(which.max(table(dd_post$KOODI[grepl("F", dd_post$KOODI)])))[1]),
                                            names(which.max(table(dd_post$KOODI[grepl("F", dd_post$KOODI)])))[1],
                                            as.character(NA)),
             n_relatives = length(unique(dr$FID_sukulainen)),
             n_diag_F_mother = sum(grepl("F",uds_mother)),
             n_diag_F_father = sum(grepl("F",uds_father)),
             n_diag_F_per_sibling = sum(grepl("F",unique(dd_r$KOODI[
               dd_r$FID %in% dr$FID_sukulainen[dr$relation %in% c("4a","4i")]])))/
               sum(dr$relation %in% c("4a","4i")),
             # diag_most_freq_mother = which.max(table(uds_mother[grepl("F", uds_mother)]))[1],
             # diag_most_freq_father = which.max(table(uds_father[grepl("F", uds_father)]))[1],
             # diag_most_freq_sibling = which.max(table(uds_sibling[grepl("F", uds_sibling)]))[1],
             diag_F_most_freq_mother = ifelse(!is.null(names(which.max(table(ds_mother[grepl("F", ds_mother)])))[1]),
                                              names(which.max(table(ds_mother[grepl("F", ds_mother)])))[1],
                                              as.character(NA)),
             diag_F_most_freq_father = ifelse(!is.null(names(which.max(table(ds_father[grepl("F", ds_father)])))[1]),
                                              names(which.max(table(ds_father[grepl("F", ds_father)])))[1],
                                              as.character(NA)),
             diag_F_most_freq_sibling = ifelse(!is.null(names(which.max(table(ds_sibling[grepl("F", ds_sibling)])))[1]),
                                               names(which.max(table(ds_sibling[grepl("F", ds_sibling)])))[1],
                                               as.character(NA))
             # municipality_completed = ifelse(is.na(d$municipality[ii]),
             #                                 dc[["Indeksipäivän\nkotikunnan nimi"]][1],
             #                                 d$municipality[ii]),
             # municipality_indexdated = dc[["Indeksipäivän\nkotikunnan nimi"]][1],
             # index_date = dvv_to_date(dc[["Indeksipäivä"]][1])
  )
}
(runtime <- proc.time() - t0) # took 29.7 hours with 4 cores
stopCluster(cl)
names(dtmp)[names(dtmp) == "FID"] <- "double_check_FID"
save(dtmp, runtime, file = "corrected_hilmo_data.Rdata")
d <- cbind(d, dtmp)
all(d$FID == d$double_check_FID) # TRUE
d <- subset(d, select = -double_check_FID)
save(d, file = "all_treatments_and_controls_with_hilmo.Rdata")
gc()

################################################################
################ Add Statistics Finland data ####################
#################################################################

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
# stopCluster(cl)
save(dtmp, runtime, file = "SES_codes_to_be_attached.Rdata")

load("SES_codes_to_be_attached.Rdata")
names(dtmp)[names(dtmp) == "FID"] <- "double_check_FID"
d <- cbind(d, dtmp)
any(d$FID != d$double_check_FID) # no disagreements
save(d, file = "all_treatments_with_hilmo_and_ses_data.Rdata")

###################################################
################ Add Kela data ####################
###################################################

### previous patient and control data ###
load("all_treatments_with_hilmo_and_ses_data.Rdata")

### income support ###
d_isup <- vroom::vroom("W:/data/Kela/FD_4810_83_522_2021_TOIMEENTULOTUKI.csv")
d_isup2 <- vroom::vroom("W:/data/Data_THL_4810_14.02.00_2020_korjaus3_Kela/Kela/FD_4810_83_522_2021_TOIMEENTULOTUKI_MAKSUT_KORJAUS.csv")
d_isup <- subset(d_isup, select = c(FID, MAKSETTU_EUR, MAKSU_PV))
d_isup2 <- subset(d_isup2, select = c(FID, MAKSETTU_EUR, MAKSU_PV))
d_isup <- rbind(d_isup, d_isup2); rm(d_isup2); gc()
names(d_isup)[2:3] <- c("income_supp_eur", "income_supp_date")
# VERY skewed payments! Should be considered on a logarithmic scale?

### rehabilitation support, incl. kuntoutuspsykoterapia ###
d_rsup1 <- vroom::vroom("W:/data/Kela/FD_4810_83_522_2021_QMA_13_18.csv")
# sum(is.na(d_rsup1$EOSLOPV) & (d_rsup1$YHTE > 0)) # only 4 cases!
d_rsup1 <- subset(d_rsup1, select = c(FID, EOSALPV, EOSLOPV, KKL))
d_rsup1 <- dplyr::distinct(d_rsup1[!is.na(d_rsup1$EOSALPV), ])
names(d_rsup1)[2:4] <- c("rehab_start", "rehab_end", "rehab_ptherapy") 
d_rsup1$rehab_medical <- (d_rsup1$rehab_ptherapy == "2")*1# vaat. laak. kuntoutus
d_rsup1$rehab_occup <- (d_rsup1$rehab_ptherapy == "3")*1# occupational rehabilitation
d_rsup1$rehab_ptherapy <- (d_rsup1$rehab_ptherapy == "K")*1# code K is psychotherapy
d_rsup2 <- vroom::vroom("W:/data/Kela/FD_4810_83_522_2021_QMA_19_21.csv")
d_rsup2 <- subset(d_rsup2, select = c(FID, PERUSTEJAKSO_ALPV, PERUSTEJAKSO_LOPV, 
                                      LAKIPERUSTE_TILASTO))
d_rsup2 <- dplyr::distinct(d_rsup2[!is.na(d_rsup2$PERUSTEJAKSO_ALPV), ])
names(d_rsup2)[2:4] <- c("rehab_start", "rehab_end", "rehab_ptherapy")
d_rsup2$rehab_medical <- (d_rsup2$rehab_ptherapy == "2")*1# vaat. laak. kuntoutus
d_rsup2$rehab_occup <- (d_rsup2$rehab_ptherapy == "3")*1# occupational rehabilitation
d_rsup2$rehab_ptherapy <- (d_rsup2$rehab_ptherapy == "K")*1# code K is psychotherapy
d_rsup <- rbind(d_rsup1, d_rsup2)
# Then combine added data
d_rsup1 <- vroom::vroom("W:/data/Data_THL_4810_14.02.00_2020_korjaus3_Kela/Kela/FD_4810_83_522_2021_KUNTOUTUSPALVELUT_KORJAUS_2013_2018.csv")
d_rsup1 <- subset(d_rsup1, select = c(FID, EOSALPV, EOSLOPV, KKL))
d_rsup1 <- dplyr::distinct(d_rsup1[!is.na(d_rsup1$EOSALPV), ])
names(d_rsup1)[2:4] <- c("rehab_start", "rehab_end", "rehab_ptherapy") 
d_rsup1$rehab_medical <- (d_rsup1$rehab_ptherapy == "2")*1# vaat. laak. kuntoutus
d_rsup1$rehab_occup <- (d_rsup1$rehab_ptherapy == "3")*1# occupational rehabilitation
d_rsup1$rehab_ptherapy <- (d_rsup1$rehab_ptherapy == "K")*1# code K is psychotherapy
d_rsup2 <- vroom::vroom("W:/data/Data_THL_4810_14.02.00_2020_korjaus3_Kela/Kela/FD_4810_83_522_2021_KUNTOUTUSPALVELUT_KORJAUS_2019_2021.csv")
d_rsup2 <- subset(d_rsup2, select = c(FID, PERUSTEJAKSO_ALPV, PERUSTEJAKSO_LOPV, 
                                      LAKIPERUSTE_TILASTO))
d_rsup2 <- dplyr::distinct(d_rsup2[!is.na(d_rsup2$PERUSTEJAKSO_ALPV), ])
names(d_rsup2)[2:4] <- c("rehab_start", "rehab_end", "rehab_ptherapy")
d_rsup2$rehab_medical <- (d_rsup2$rehab_ptherapy == "2")*1# vaat. laak. kuntoutus
d_rsup2$rehab_occup <- (d_rsup2$rehab_ptherapy == "3")*1# occupational rehabilitation
d_rsup2$rehab_ptherapy <- (d_rsup2$rehab_ptherapy == "K")*1# code K is psychotherapy
d_rsup <- rbind(d_rsup, d_rsup1, d_rsup2)
rm(d_rsup1, d_rsup2); gc() # clean up
length(unique(d_rsup$FID)) # 59741 support/service receivers
length(unique(d_rsup$FID[d_rsup$rehab_ptherapy==1])) # 20796 Kela-therapy receivers

### sickness absence data ###
d_sabs <- vroom::vroom("W:/data/Kela/FD_4810_83_522_2021_SJKAUDET_1987_2021.csv")
d_sabs <- subset(d_sabs, select = c(FID, DIAGNOOSI_KOODI, TYOKYVYTTOMYYS_ALPV,
                                    MAKSETTUPAIVA_LKM))
d_sabs <- dplyr::distinct(d_sabs) # already was mainly
d_sabs2 <- vroom::vroom("W:/data/Data_THL_4810_14.02.00_2020_korjaus3_Kela/Kela/FD_4810_83_522_2021_SJKAUDET_KORJAUS_1987_2021.csv")
d_sabs2 <- subset(d_sabs2, select = c(FID, DIAGNOOSI_KOODI, TYOKYVYTTOMYYS_ALPV,
                                      MAKSETTUPAIVA_LKM))
d_sabs2 <- dplyr::distinct(d_sabs2) # already was mainly
d_sabs <- rbind(d_sabs, d_sabs2); rm(d_sabs2); gc()
d_sabs <- dplyr::distinct(d_sabs) # d_sabs and d_sabs2 had no overlap at all

### rehabilitation money data ###
d_rmon <- vroom::vroom("W:/data/Kela/FD_4810_83_522_2021_UMA_13_18.csv")
d_rmon <- subset(d_rmon, select = c(FID, SAIRKO, JAALPV, PALKM))
names(d_rmon)[2:4] <- c("rehab_money_code","rehab_money_start","rehab_money_days")
d_rmon2 <- vroom::vroom("W:/data/Kela/FD_4810_83_522_2021_UMA_19_21.csv")
d_rmon2 <- subset(d_rmon2, select = c(FID, SAIRAUSDIAGNOOSI1_KUNTOUTUSRAHA,
                                      JAKSO_ALPV, PAIVA_LKM))
names(d_rmon2)[2:4] <- c("rehab_money_code","rehab_money_start","rehab_money_days")
d_rmon <- rbind(d_rmon, d_rmon2); rm(d_rmon2); gc()
# then combine the data due to later-added patients
d_rmon1 <- vroom::vroom("W:/data/Data_THL_4810_14.02.00_2020_korjaus3_Kela/Kela/FD_4810_83_522_2021_KUNTOUTUSRAHA_KORJAUS_2013_2018.csv")
d_rmon1 <- subset(d_rmon1, select = c(FID, SAIRKO, JAALPV, PALKM))
names(d_rmon1)[2:4] <- c("rehab_money_code","rehab_money_start","rehab_money_days")
d_rmon2 <- vroom::vroom("W:/data/Data_THL_4810_14.02.00_2020_korjaus3_Kela/Kela/FD_4810_83_522_2021_KUNTOUTUSPALVELUT_KORJAUS_2019_2021.csv")
d_rmon2 <- subset(d_rmon2, select = c(FID, SAIRAUSKOODI1, # must be SAIRAUSDIAGNOOSI1_KUNTOUTUSRAHA?
                                      MAKSUJAKSO_ALPV, MAKSUJAKSO_LOPV))   # lacks PAIVA_LKM!
d_rmon2$MAKSUJAKSO_LOPV <- as.numeric(d_rmon2$MAKSUJAKSO_LOPV - d_rmon2$MAKSUJAKSO_ALPV)
names(d_rmon2)[2:4] <- c("rehab_money_code","rehab_money_start","rehab_money_days")
d_rmon <- rbind(d_rmon, d_rmon1, d_rmon2); rm(d_rmon1, d_rmon2); gc()

### drug purchases ###
d_meds <- vroom::vroom("W:/data/Kela/FD_4810_83_522_2021_Laakeostot_1995_2021.csv")
d_meds <- subset(d_meds, select = c(FID, TOIMITUS_PV, LAAKEMAARAYS_KIRJOITUS_PV,
                                    ATC5_KOODI))
d_meds2 <- vroom::vroom("W:/data/Data_THL_4810_14.02.00_2020_korjaus3_Kela/Kela/FD_4810_83_522_2021_LAAKEOSTOT_KORJAUS_1995_2021.csv")
d_meds2 <- subset(d_meds2, select = c(FID, TOIMITUS_PV, LAAKEMAARAYS_KIRJOITUS_PV,
                                      ATC5_KOODI))
d_meds <- rbind(d_meds, d_meds2); rm(d_meds2); gc()

# divide already / use only number of distinct codes, most common code, etc.
# d_meds_depr <- d_meds[grepl("N06A",d_meds$ATC5_KOODI)|grepl("N06CA",d_meds$ATC5_KOODI), ]
# d_meds_psyc <- d_meds[grepl("N05A",d_meds$ATC5_KOODI), ]
# d_meds_anx <- d_meds[grepl("N05B",d_meds$ATC5_KOODI), ]
# d_meds_sleep <- d_meds[grepl("N05C",d_meds$ATC5_KOODI), ]
# d_meds_opioid <- d_meds[grepl("N02A",d_meds$ATC5_KOODI), ]
# d_meds_depend <- d_meds[grepl("N07B",d_meds$ATC5_KOODI), ]
# d_meds_epilep <- d_meds[grepl("N03A",d_meds$ATC5_KOODI), ]
# d_meds_other <- d_meds[!(d_meds$ATC5_KOODI %in% c(unique(d_meds_depr$ATC5_KOODI),
#                                                   unique(d_meds_psyc$ATC5_KOODI),
#                                                   unique(d_meds_anx$ATC5_KOODI),
#                                                   unique(d_meds_sleep$ATC5_KOODI),
#                                                   unique(d_meds_opioid$ATC5_KOODI),
#                                                   unique(d_meds_depend$ATC5_KOODI),
#                                                   unique(d_meds_epilep$ATC5_KOODI),)), ]

# pryr::object_size(d_meds) # 533 MB
library(foreach)
# library(doSNOW)
# cl <- makeCluster(3)
# registerDoSNOW(cl)

t0 <- proc.time()
d_kela_tmp <- foreach(ii = 1:nrow(d), .combine = rbind) %do% {
  out <- tryCatch({ # difficult-to-debug error, so use tryCatch
    ipFID <- d$FID[ii] # index person FID
    if (!is.na(d$first_session_time[ii])){ 
      ipdate <- d$first_session_time[ii]
    } else {
      if (!is.na(d$index_date[ii])){
        ipdate <- d$index_date[ii]
      } else {
        ipdate <- NA
      }
    }
    drs <- d_rsup[d_rsup$FID == ipFID, ] # index person rehab support/services
    ds <- d_sabs[d_sabs$FID == ipFID, ]  # index person sickness absences
    ### Kela variables to be added ###
    # tegap <- lubridate::dmonths(6) # six-month post therapy events recorded as "post"
    if (!is.na(ipdate)){
      tmeds <- table(d_meds$ATC5_KOODI[(d_meds$FID==ipFID)&(d_meds$TOIMITUS_PV<=ipdate)])
      data.frame(FID = d$FID[ii], 
                 rehab_money_any_pre = any(ipdate >= d_rmon$rehab_money_start[d_rmon$FID==ipFID], na.rm = T),
                 rehab_money_F_pre = any(ipdate >= d_rmon$rehab_money_start[
                   (d_rmon$FID==ipFID)&grepl("F",d_rmon$rehab_money_code)], na.rm = T),
                 income_supp_pre = any(ipdate >= d_isup$income_supp_date[d_isup$FID==ipFID], na.rm = T),
                 rehab_supp_any_pre = any(ipdate >= drs$rehab_start, na.rm = T),
                 rehab_supp_psychoth_pre = any(ipdate >= drs$rehab_start[drs$rehab_ptherapy==1], na.rm = T),
                 rehab_supp_medical_pre = any(ipdate >= drs$rehab_start[drs$rehab_medical==1], na.rm = T),
                 rehab_supp_occup_pre = any(ipdate >= drs$rehab_start[drs$rehab_occup==1], na.rm = T),
                 n_meds_atcn_pre = length(tmeds),
                 n_meds_buys_pre = sum(tmeds),
                 most_freq_med_pre = paste0(names(tmeds)[which.max(tmeds)],
                                            collapse = ","),
                 d_sickabs_days_pre = sum(ds$MAKSETTUPAIVA_LKM[
                   ds$TYOKYVYTTOMYYS_ALPV<=ipdate], na.rm=T),
                 d_sickabs_days_F_pre = sum(ds$MAKSETTUPAIVA_LKM[
                   (ds$TYOKYVYTTOMYYS_ALPV<=ipdate)&
                     grepl("F", ds$DIAGNOOSI_KOODI)], na.rm=T)
      )
    } else {
      tmeds <- table(d_meds$ATC5_KOODI[d_meds$FID==ipFID])
      ipdate <- lubridate::as_date("2022-04-05") # last index date in the data
      data.frame(FID = d$FID[ii], 
                 rehab_money_any_pre = any(ipdate >= d_rmon$rehab_money_start[d_rmon$FID==ipFID], na.rm = T),
                 rehab_money_F_pre = any(ipdate >= d_rmon$rehab_money_start[
                   (d_rmon$FID==ipFID)&grepl("F",d_rmon$rehab_money_code)], na.rm = T),
                 income_supp_pre = any(ipdate >= d_isup$income_supp_date[d_isup$FID==ipFID], na.rm = T),
                 rehab_supp_any_pre = any(ipdate >= drs$rehab_start, na.rm = T),
                 rehab_supp_psychoth_pre = any(ipdate >= drs$rehab_start[drs$rehab_ptherapy==1], na.rm = T),
                 rehab_supp_medical_pre = any(ipdate >= drs$rehab_start[drs$rehab_medical==1], na.rm = T),
                 rehab_supp_occup_pre = any(ipdate >= drs$rehab_start[drs$rehab_occup==1], na.rm = T),
                 n_meds_atcn_pre = length(tmeds),
                 n_meds_buys_pre = sum(tmeds),
                 most_freq_med_pre = paste0(names(tmeds)[which.max(tmeds)],
                                            collapse = ","),
                 d_sickabs_days_pre = sum(ds$MAKSETTUPAIVA_LKM[
                   ds$TYOKYVYTTOMYYS_ALPV<=ipdate], na.rm=T),
                 d_sickabs_days_F_pre = sum(ds$MAKSETTUPAIVA_LKM[
                   (ds$TYOKYVYTTOMYYS_ALPV<=ipdate)&
                     grepl("F", ds$DIAGNOOSI_KOODI)], na.rm=T)
      )
    }
  }, error = function(e){
    # return NAs if the above resulted in an error
    ipFID <- d$FID[ii] # index person FID
    out <- data.frame(FID = ipFID, 
                      rehab_money_any_pre = NA,
                      rehab_money_F_pre = NA,
                      income_supp_pre = NA,
                      rehab_supp_any_pre = NA,
                      rehab_supp_psychoth_pre = NA,
                      rehab_supp_medical_pre = NA,
                      rehab_supp_occup_pre = NA,
                      n_meds_atcn_pre = as.numeric(NA),
                      n_meds_buys_pre = as.numeric(NA),
                      most_freq_med_pre = as.character(NA),
                      d_sickabs_days_pre = as.numeric(NA),
                      d_sickabs_days_F_pre = as.numeric(NA)
    )
    return(out) # output from tryCatch
  })
}
(runtime <- proc.time() - t0) # took 5.9 hours with 1 core
save(d_kela_tmp, runtime, file = "Kela_variables_to_be_attached.Rdata")

###### Combine them ######

load("all_treatments_with_hilmo_and_ses_data.Rdata")
d <- subset(d, select = -double_check_FID)
load("Kela_variables_to_be_attached.Rdata")
names(d_kela_tmp)[names(d_kela_tmp) == "FID"] <- "double_check_FID"
d <- cbind(d, d_kela_tmp)
all(d$FID == d$double_check_FID) # TRUE
d <- subset(d, select = -double_check_FID)
save(d, file = "all_treatments_and_controls_with_hilmo_ses_kela.Rdata")
rm(d_kela_tmp); gc()

# ADD last_measurement_time VARIABLE BECAUSE MAY NOT BE last_session_time!!!

### TO-DO:
# 1) Treatment timings from 1st and last patient-created record (d_answers-taulu?)
# 2) Timings for 1st and last primary-measure response, as before
# 3) Last-observation-carried-forward follow-up measure
# 4) Not started -muuttuja
# 5) FPQR-muuttuja LaRe- lyhyt/pitka ja lasten/nuorten/aikuisten
# 6) Ammatillisen kuntoutuksen muuttuja?