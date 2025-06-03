# combine_control_and_covariate_data.R
# This script has a very long runtime and takes plenty resources.
# Do not run the entire script at once. Precomputed datasets are in Kapseli.
# This script is for documentation and for making changes when needed.
# Tom R / 21.08.2023 and later.

#################################
### Load the therapy datasets ###
#################################

load("old_iCBT_primary_measure_data.Rdata")
load("new_iCBT_primary_measure_data.Rdata")
d_new_iCBT <- d; rm(d)
load("FPQR_data.Rdata")
load("insomnia_iCBT_data.Rdata")

patient_FIDs <- unique(c(d_old_iCBT$FID, d_new_iCBT$FID, d_iCBTI$FID, d_fpqr$FID))
length(patient_FIDs) # 41 011 patients

library(dplyr)
summarize(group_by(d_new_iCBT, by = iCBT),
          d = mean(zFU, na.rm=T),
          iCBT = first(inventory_name))
mean(d_iCBTI$zFU, na.rm = T)
# summarize(group_by(d_old_iCBT[d_old_iCBT$dropout==0,], by = treatment),
summarize(group_by(d_old_iCBT, by = treatment),
          d = mean(zFU, na.rm=T),
          iCBT = first(inventory))

############################################
############ Control persons ###############
############################################

ctrl1 <- vroom::vroom("W:/data/DVV/FD_4810_Tulokset 2022-06-01 VERROKKI.csv")
ctrl2 <- vroom::vroom("W:/data/Data_THL_4810_140200_2020_korjaus1/DVV/FD_4810_Tulokset 2023-05-03 VERROKKI 6252.csv")
ctrl3 <- vroom::vroom("W:/data/Data_THL_4810_140200_2020_korjaus1/DVV/FD_4810_Tulokset 2023-05-03 VERROKKI 358.csv")
ctrl_rm <- vroom::vroom("W:/data/Data_THL_4810_140200_2020_korjaus1/DVV/FD_4810_Tulokset 2023-05-03 VERROKKI 6252_karsitut.csv")

# control_FIDs <- c(ctrl1$FID_tutkimus_verrokkihenkilo[ctrl1$`Ver-rokin nro` !=0 ],
#                   ctrl2$FID_tutkimus_verrokkihenkilo[ctrl2$`Ver-rokin nro` !=0 ],
#                   ctrl3$FID_tutkimus_verrokkihenkilo[ctrl3$`Ver-rokin nro` !=0 ])
# # sum(control_FIDs %in% patient_FIDs) # 355
# control_FIDs <- setdiff(control_FIDs, patient_FIDs)
# d_controls <- data.frame(FID = control_FIDs, 
#                          registry = rep("controls", length(control_FIDs)))
d_controls <- rbind(ctrl1, ctrl2, ctrl3)
save(d_controls, file = "controls_FIDs.Rdata")


############################################
############### Relatives  #################
############################################

rltv1 <- vroom::vroom("W:/data/DVV/FD_4810_Tulokset 2022-06-01 SUKU.csv")
rltv2 <- vroom::vroom("W:/data/Data_THL_4810_140200_2020_korjaus1/DVV/FD_4810_Tulokset 2023-05-03 SUKU 6252.csv")
rltv3 <- vroom::vroom("W:/data/Data_THL_4810_140200_2020_korjaus1/DVV/FD_4810_Tulokset 2023-05-03 SUKU 358.csv")

# sum(duplicated(subset(rltv1, select = c(FID_valihenkilo, FID_sukulainen, FID_kantahenkilo)))) # 1
# sum(duplicated(rbind(subset(rltv1, select = c(FID_valihenkilo, FID_sukulainen, FID_kantahenkilo)),
#                      subset(rltv2, select = c(FID_valihenkilo, FID_sukulainen, FID_kantahenkilo))))) # 2412
# mean(rltv1$FID_sukulainen %in% rltv2$FID_sukulainen) # 2.8%

names(rltv1) <- names(rltv2)
d_suku <- rbind(rltv1, rltv2, rltv3)
rm(rltv1, rltv2, rltv3); gc()
save(d_suku, file = "relatives_data.Rdata")

# sum(control_FIDs %in% patient_FIDs) # 355
control_FIDs <- setdiff(control_FIDs, patient_FIDs)
d_controls <- data.frame(FID = control_FIDs, 
                         registry = rep("controls", length(control_FIDs)))
save(d_controls, file = "controls_FIDs.Rdata")


#######################################
####### Initialize the full data ######
#######################################

### Harmonize FPQR ###

klc <- function(x){ # knowable (non-missing) logical condition (includes NAs)
  return(ifelse(is.na(x), TRUE, x))
}
d <- subset(d_fpqr[klc(d_fpqr$short_therapy==1) &
                     klc(d_fpqr$terapiamuoto=="yksiloterapia") & 
                     klc(!d_fpqr$internal_therapy), ], 
            select = c(FID, coreom_core10, coreom_core10FU,
                       coreom_cdate, coreom_cdateFU, terapiasuuntaus, 
                       paadiagnoosi, many_treatments, treatment_id, sukupuoli))
d <- cbind(d, later_therapy = rep(0, nrow(d)), 
           later_therapy_date = rep(as.Date(NA), nrow(d)),
           registry = rep("FPQR", nrow(d)))
names(d)[2:7] <- c("BL", "FU", "cdate", "cdateFU", "treatment_type", "index_diag")

# Remove therapies missing an id because they all miss baseline values too
d <- d[!is.na(d$treatment_id), ] # removes 2395 therapies (38.5%)

# If multiple treatments...
tmpFIDs <- na.omit(d$FID[d$many_treatments==1])
for (i in 1:length(tmpFIDs)){
  dtmp <- d[d$FID == tmpFIDs[i], ]
  if (nrow(dtmp)>1){ # ...with dates -> take 1st treatment
    if (length(na.omit(unique(dtmp$cdate)))>1){
      t1st <- unique(dtmp$treatment_id[which.min(dtmp$cdate)])
      d <- d[!(d$treatment_id %in% setdiff(dtmp$treatment_id, t1st)), ]
      tmpind <- d$FID == tmpFIDs[i]
      d$later_therapy[tmpind] <- 1
      tmpdates <- dtmp$cdate[dtmp$treatment_id != t1st]
      if (!all(is.na(tmpdates))){
        d$later_therapy_date[tmpind] <- min(tmpdates, na.rm = T) 
      } else {
        d$later_therapy_date[tmpind] <- as.Date(NA)
      }
    } else { # without dates -> take non-missing values
      tmptids <- unique(dtmp$treatment_id)
      nnm <- sapply(tmptids, function(x) sum(c(!is.na(dtmp$BL[dtmp$treatment_id==x]),
                                               !is.na(dtmp$FU[dtmp$treatment_id==x]))))
      maxnnm <- max(nnm) # max number of non-missing observations
      if (length(unique(maxnnm))==1){
        d <- d[!(d$treatment_id %in% tmptids[-which.max(maxnnm)]), ]
      } else { # without dates or distinct missingness -> take the 1st mentioned
        d <- d[!(d$treatment_id %in% tmptids[-1]), ]
      }
    }
  }
}

names(d)[names(d) == "sukupuoli"] <- "sex"
d$sex[d$sex == "M"] <- "Male"; d$sex[d$sex == "F"] <- "Female"
d <- cbind(d, zBL = (d$BL - mean(d$BL, na.rm = T))/sd(d$BL, na.rm = T),
           zFU = (d$FU - mean(d$BL, na.rm = T))/sd(d$BL, na.rm = T))
d <- subset(d, select = c(FID:cdateFU, zBL, zFU, treatment_type, 
                          treatment_id, registry, sex))
d <- cbind(d, inventory = rep("CORE-OM", nrow(d)),
           age = as.numeric(rep(NA, nrow(d))),
           municipality = as.character(rep(NA, nrow(d))),
           dropout = is.na(d$FU)*1,
           dropout2 = rep(NA,nrow(d)))


### Harmonize and add new iCBTs ###

dtmp <- d_new_iCBT
names(dtmp)[names(dtmp) == "iCBT"] <- "treatment_type"
names(dtmp)[names(dtmp) == "dropout"] <- "dropout2" # 70% completed
dtmp <- cbind(dtmp, dropout = as.numeric(rep(NA,nrow(dtmp))))
# uts <- unique(dtmp$treatment_type) #unique treatments
uts <- c("Yleistyneen ahdistuneisuushäiriön nettiterapia",
         "Masennuksen nettiterapia",
         "Pakko-oireisen häiriön nettiterapia",
         "Sosiaalisten tilanteiden pelon nettiterapia",
         "Paniikkihäiriön nettiterapia",
         "Alkoholin liikakäytön nettiterapia",
         "Bulimian nettiterapia",
         "Kaksisuuntaisen mielialahäiriön nettiterapia",
         "Nuorten sosiaalisen ahdistu&shy:neisuuden nettiterapia",
         "pitkäaikaisten ja haittaavien kehollisten oireiden kuntoutusohjelma",
         "PITKO – Pitkäaikaisten ja haittaavien kehollisten oireiden kuntoutusohjelma")
maxses <- c(12,7,10,7,8,5,8,14,8,6,10)
for (i in 1:length(uts)){
  dtmp$dropout[dtmp$treatment_type == uts[i]] <- 1 - 
    (dtmp$last_session[dtmp$treatment_type == uts[i]] >= maxses[i])*1
}


dtmp <- cbind(dtmp, registry = rep("new iCBT", nrow(dtmp)))
d <- rbind(d, dtmp[,names(d)]); rm(dtmp)
names(d)

### Harmonize and add old iCBTs ###

dtmp <- d_old_iCBT
names(dtmp)[names(dtmp) == "treatment"] <- "treatment_type"
dtmp <- cbind(dtmp, 
              registry = rep("old iCBT", nrow(dtmp)),
              age = as.numeric(rep(NA, nrow(dtmp))),
              sex = as.character(rep(NA, nrow(dtmp))),
              municipality = as.character(rep(NA, nrow(dtmp))))
d <- rbind(d, dtmp[,names(d)]); rm(dtmp)
names(d)

### Harmonize and add insomnia iCBT ###

dtmp <- d_iCBTI
names(dtmp)[names(dtmp) == "iCBT"] <- "treatment_type"
dtmp <- cbind(dtmp, registry = rep("iCBT-I", nrow(dtmp)))
dtmp <- cbind(dtmp, dropout2 = dtmp$dropout)
d <- rbind(d, dtmp[,names(d)]); rm(dtmp)
names(d)
table(d$registry)

# save(d, file = "all_therapies_harmonized_data.Rdata")

# check how many went both types of treatments
sum(d$FID[d$registry != "FPQR"] %in% d$FID[d$registry == "FPQR"]) # 595
# check how many went face-to-face after an iCBT
ninfmin <- function(x, askmax = F){
  if (any(!is.na(x))){
    if (!askmax){ 
      return(min(x, na.rm = T))
    } else {
      return(max(x, na.rm = T))
    }
  } else {
    return(as.Date(NA))
  }
}
tmpFIDs <- d$FID[d$registry != "FPQR"][d$FID[d$registry != "FPQR"] %in% d$FID[d$registry == "FPQR"]]
a <- sapply(tmpFIDs, 
            function(x){ninfmin(d$cdate[(d$FID==x)&(d$registry=="FPQR")], askmax=T) - 
                ninfmin(d$cdate[(d$FID==x)&(d$registry!="FPQR")]) > 0})
sum(a, na.rm = T) # 480
sum(a, na.rm = T)/length(tmpFIDs) # 80.7%

###########################################################
###### Add data for patients and controls,plus Hilmo ######
###########################################################

### def functions ###
dvv_to_date <- function(x){
  x <- as.character(x)
  x <- lubridate::make_date(year = substr(x,1,4),
                            month = substr(x,5,6),
                            day = substr(x,7,8))
  return(x)
}

### load and combined data ###

load("all_therapies_harmonized_data.Rdata")
load("controls_FIDs.Rdata")
load("relatives_data.Rdata")

d_controls <- d_controls[!(d_controls$FID_tutkimus_verrokkihenkilo %in% d$FID), ]
# NAILLA SAA age, sex, municipality tietoa kontrolleille ja potilaille!
# dtmp <- subset(d_controls, select = c(FID_tutkimus_verrokkihenkilo,
#                                       `Syntymä-päivä`,`Suku-\npuoli`,
#                                       Indeksipäivä, 
#                                       `Indeksipäivän\nkotikunnan nimi`))
# names(dtmp) <- c("FID", "year_of_birth", "sex_dvv", "index_date_dvv", "municipality")
# dtmp$year_of_birth <- lubridate::year(dvv_to_date(dtmp$year_of_birth))
# dtmp$index_date_dvv <- dvv_to_date(dtmp$index_date_dvv)
dtmp <- subset(d_controls, select = c(FID_tutkimus_verrokkihenkilo))
names(dtmp) <- c("FID")
dtmp$registry <- rep("Controls", nrow(dtmp))

# combine d_controls to patient data d
for (i in 2:ncol(dtmp)){
  if (!(names(dtmp)[i] %in% c("registry"))){
    d <- cbind(d, rep(NA, nrow(d)))
    names(d)[ncol(d)] <- names(dtmp)[i]
    class(d[[ncol(d)]]) <- class(dtmp[[i]])
  }
}
for (i in 2:ncol(d)){
  if (!(names(d)[i] %in% names(dtmp))){
    dtmp <- cbind(dtmp, rep(NA, nrow(dtmp)))
    names(dtmp)[ncol(dtmp)] <- names(d)[i]
    class(dtmp[[ncol(dtmp)]]) <- class(d[[i]])
  }
}
d <- rbind(d, dtmp[,names(d)])
rm(dtmp)

### datasets containing the variables ###

# Hilmo
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

### Add variables using parallel computation ###
# library(future)
# options(future.globals.maxSize = 11000*1024^2)
# plan(multisession)
# f <- list()
library(foreach)
library(doSNOW)
cl <- makeCluster(4)
registerDoSNOW(cl)
t0 <- proc.time()
dtmp <- foreach(ii = 1:nrow(d), .combine = rbind) %dopar% {
  dd <- d_diag[d_diag$FID == d$FID[ii], ] # index person diagnoses
  dh <- d_hilmo9621[d_hilmo9621$FID == d$FID[ii], ] # index person Hilmo
  dh$TUPVA <- as.Date(dh$TUPVA, format = "%d.%m.%Y")
  # dated diagnoses
  dd <- merge(dd, dh[,c("HILMOID", "TUPVA")], by = "HILMOID")
  # rm(dh)
  # dated hilmo psychiatry visits
  dh <- dh[dh$EA %in% c("70","70F","70X","70Z","74","75","75X"),]
  if (!is.na(d$cdate[ii])){
    dd_post <- dd[dd$TUPVA > d$cdate[ii], ]
    dd <- dd[dd$TUPVA <= d$cdate[ii], ]
    uds <- unique(dd$KOODI)
    uds_post <- unique(dd_post$KOODI)
    dh_post <- dh[dh$TUPVA > d$cdate[ii], ]
    dh <- dh[dh$TUPVA <= d$cdate[ii], ]
  } else {
    uds <- unique(dd$KOODI) # unique diagnoses for index person
    uds_post <- as.character(c())
    dd_post <- data.frame(matrix(nrow = 0, ncol = ncol(dd)))
    names(dd_post) <- names(dd)
    dh_post <- data.frame(matrix(nrow = 0, ncol = ncol(dh)))
    names(dh_post) <- names(dh)
  }
  # control persons of the index person (control itself if index)
  dc <- d_controls[(d_controls$FID_tutkimushenkilo == d$FID[ii])|
                     (d_controls$FID_tutkimus_verrokkihenkilo == d$FID[ii]), ] 
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
             year_of_birth = lubridate::year(dvv_to_date(dc$`Syntymä-päivä`[1])),
             sex_dvv = dc[["Suku-\npuoli"]][1],
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
                                               as.character(NA)),
             municipality_completed = ifelse(is.na(d$municipality[ii]),
                                             dc[["Indeksipäivän\nkotikunnan nimi"]][1],
                                             d$municipality[ii]),
             municipality_indexdated = dc[["Indeksipäivän\nkotikunnan nimi"]][1],
             index_date_dvv = dvv_to_date(dc[["Indeksipäivä"]][1])
  )
}
(runtime <- proc.time() - t0) # took 26.4 hours with 5 cores
stopCluster(cl)
names(dtmp)[names(dtmp) == "FID"] <- "double_check_FID"
save(dtmp, runtime, file = "corrected_hilmo_data.Rdata")
# d <- cbind(d, dtmp)
# save(d, file = "all_therapies_with_hilmo_data.Rdata")
gc()

# all(d$FID == dtmp$double_check_FID) # TRUE
load("all_therapies_with_hilmo_ses_kela.Rdata")
load("corrected_hilmo_data.Rdata")
d <- subset(d, select = -c(diag_most_freq_mother, diag_most_freq_father, diag_most_freq_sibling))
d[,intersect(names(d),names(dtmp))] <- dtmp[,intersect(names(d),names(dtmp))]
d <- cbind(d, dtmp[,setdiff(names(dtmp),names(d))])
head(d)
save(d, file = "all_therapies_with_hilmo_ses_kela.Rdata")

# any(d$FID != d$double_check_FID) #was FALSE, is OK
d <- subset(d, select = -double_check_FID)
save(d, file = "all_therapies_with_hilmo_data.Rdata")

# mean((d$cdate - d$index_date_dvv)[!(d$registry %in% c("Controls"))] >= 0, na.rm = T)
# 98.5% has cdate greater or equal to index date. Good!
# mean(d$municipality == d$municipality_indexdated, na.rm = T) # 93.7% aggreement!

#################################################################
################ Add Statistics Finland data ####################
#################################################################

load("all_therapies_with_hilmo_data.Rdata")
load("relatives_data.Rdata")

# d_profession <- vroom::vroom("W:/data/Tilastokeskus/FD_4810_ammatti_findata_4810.csv")
# d_cod <- vroom::vroom("W:/data/Tilastokeskus/FD_4810_kuolemansyyt_findata_4810.csv")

d_ses <- vroom::vroom("W:/data/Tilastokeskus/FD_4810_sose_findata_4810.csv")
d_ses2 <- vroom::vroom("W:/data/Data_THL_4810_14.02.00_2020_korjaus2_TK/TK_u1587_p_150523/FD_4810_2022_sose_u1587_p_150523.csv")
d_ses <- rbind(d_ses, d_ses2)
rm(d_ses2); gc()
names(d_ses)[names(d_ses) == "vuosi"] <- "ses_year"


library(foreach)
# library(doSNOW)
# cl <- makeCluster(4)
# registerDoSNOW(cl)
# SLOWER with parallel computation! (probably would need larger chunks)
# -> compute in separate session

t0 <- proc.time()
dtmp <- foreach(ii = 1:nrow(d), .combine = rbind) %do% {
  tmpFID <- d$FID[ii]
  ds <- d_ses[d_ses$FID == tmpFID, ]
  dr <- d_suku[d_suku$FID_kantahenkilo == tmpFID, ]
  ds_father <- d_ses[d_ses$FID == dr$FID_sukulainen[dr[,"Sukul-\nsuhde"] == "3i"][1], ]
  ds_mother <- d_ses[d_ses$FID == dr$FID_sukulainen[dr[,"Sukul-\nsuhde"] == "3a"][1], ]
  ind_index <- which.min(abs(lubridate::year(d$index_date_dvv[ii]) - 
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

names(dtmp)[names(dtmp) == "FID"] <- "double_check_FID"
d <- cbind(d, dtmp)
any(d$FID != d$double_check_FID) # no disagreements
save(d, file = "all_therapies_with_hilmo_and_ses_data.Rdata")

###################################################
################ Add Kela data ####################
###################################################

### previous patient and control data ###
load("all_therapies_with_hilmo_data.Rdata")

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
d_rsup1$rehab_ptherapy <- (d_rsup1$rehab_ptherapy == "K")*1# code K is psychotherapy
d_rsup2 <- vroom::vroom("W:/data/Kela/FD_4810_83_522_2021_QMA_19_21.csv")
d_rsup2 <- subset(d_rsup2, select = c(FID, PERUSTEJAKSO_ALPV, PERUSTEJAKSO_LOPV, 
                                      LAKIPERUSTE_TILASTO))
d_rsup2 <- dplyr::distinct(d_rsup2[!is.na(d_rsup2$PERUSTEJAKSO_ALPV), ])
names(d_rsup2)[2:4] <- c("rehab_start", "rehab_end", "rehab_ptherapy")
d_rsup2$rehab_ptherapy <- (d_rsup2$rehab_ptherapy == "K")*1# code K is psychotherapy
d_rsup <- rbind(d_rsup1, d_rsup2)
# Then combine added data
d_rsup1 <- vroom::vroom("W:/data/Data_THL_4810_14.02.00_2020_korjaus3_Kela/Kela/FD_4810_83_522_2021_KUNTOUTUSPALVELUT_KORJAUS_2013_2018.csv")
d_rsup1 <- subset(d_rsup1, select = c(FID, EOSALPV, EOSLOPV, KKL))
d_rsup1 <- dplyr::distinct(d_rsup1[!is.na(d_rsup1$EOSALPV), ])
names(d_rsup1)[2:4] <- c("rehab_start", "rehab_end", "rehab_ptherapy") 
d_rsup1$rehab_ptherapy <- (d_rsup1$rehab_ptherapy == "K")*1# code K is psychotherapy
d_rsup2 <- vroom::vroom("W:/data/Data_THL_4810_14.02.00_2020_korjaus3_Kela/Kela/FD_4810_83_522_2021_KUNTOUTUSPALVELUT_KORJAUS_2019_2021.csv")
d_rsup2 <- subset(d_rsup2, select = c(FID, PERUSTEJAKSO_ALPV, PERUSTEJAKSO_LOPV, 
                                      LAKIPERUSTE_TILASTO))
d_rsup2 <- dplyr::distinct(d_rsup2[!is.na(d_rsup2$PERUSTEJAKSO_ALPV), ])
names(d_rsup2)[2:4] <- c("rehab_start", "rehab_end", "rehab_ptherapy")
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
    if (!is.na(d$cdate[ii])){ 
      ipdate <- d$cdate[ii]
    } else {
      if (d$treatment_id[ii] == d$treatment_id[d$FID==ipFID][1]){
        ipdate <- d$index_date_dvv[ii]
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
                      n_meds_atcn_pre = as.numeric(NA),
                      n_meds_buys_pre = as.numeric(NA),
                      most_freq_med_pre = as.character(NA),
                      d_sickabs_days_pre = as.numeric(NA),
                      d_sickabs_days_F_pre = as.numeric(NA)
    )
    return(out) # output from tryCatch
  })
}
(runtime <- proc.time() - t0) # took X hours with 1 core
save(d_kela_tmp, runtime, file = "Kela_variables_to_be_attached.Rdata")
# stopCluster(cl)

###### Combine them ######

load("all_therapies_with_hilmo_and_ses_data.Rdata")
d <- subset(d, select = -double_check_FID)
load("Kela_variables_to_be_attached.Rdata")
names(d_kela_tmp)[names(d_kela_tmp) == "FID"] <- "double_check_FID"
d <- cbind(d, d_kela_tmp)
all(d$FID == d$double_check_FID) # TRUE
d <- subset(d, select = -double_check_FID)
save(d, file = "all_therapies_with_hilmo_ses_kela.Rdata")
rm(d_kela_tmp); gc()
