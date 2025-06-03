# data_build_functions.R

#### Terveyskyla iCBT ####

build_new_iCBT_data <- function(d_patients, d_participants, d_treatments,
                                d_iCBTs, d_questionnaires, d_answers,
                                d_taskexec, d_tasks){
  
  # ORIGINAL TABLES:
  # d_patients = nettiterapia_potilaat
  # d_participants = nettiterapia_osallistujat
  # d_treatments = nettiterapia_hoidot
  # d_iCBTs = nettiterapia_nettiterapiat
  # d_questionnaires = nettiterapia_kyselyt
  # d_answers = nettiterapia_vastaukset
  # d_tasks = nettiterapia_tehtavat
  # d_taskexec = nettiterapia_tehtava_suoritukset
  
  library(magrittr) # need for piping downstream
  
  ### A function defining treatment-specific outcome ###
  toutcome <- function(dd){
    # Compute a mean score
    EDEQ_scoring <- function(ddd, scompl, slast, items){
      if ((nrow(ddd)==0)|(all(is.na(ddd$pisteet)))){
        return(data.frame(BL = NA, FU = NA, nitemsBL = NA, nitemsFU = NA, 
                          dropout = NA, last_session = NA, last_week = NA))
      }
      # A function to form the EDE-Q scale
      form_EDEQ_scale <- function(scores, qids){
        ntot <- 0
        # sub-scale 1 (syomisen rajoittaminen, 5 items: 1-5)
        lids <- qids %in% c(14034, 14033, 14032, 14031, 14030)
        nused <- ifelse(sum(lids, na.rm = T)>=3, sum(lids, na.rm = T), 0)
        ntot <- nused + ntot
        ss1 <- ifelse(sum(lids, na.rm = T)>=3, sum(scores[lids]/nused, na.rm = T)/ntot, NA)
        # sub-scale 2 (syomiseen liittyvat huolet, 5 items: 7, 9, 19, 20, 21)
        lids <- qids %in% c(14028, 14026, 14022, 14021, 14020)
        nused <- ifelse(sum(lids, na.rm = T)>=3, sum(lids, na.rm = T), 0)
        ntot <- nused + ntot
        ss2 <- ifelse(sum(lids, na.rm = T)>=3, sum(scores[lids]/nused, na.rm = T), NA)
        # sub-scale 3 (kehon muotoon liittyvat huolet, 8 items: 6, 8, 10, 11, 23, 26, 27, 28)
        lids <- qids %in% c(14029, 14027, 14035, 14025, 14018, 14015, 14024, 14036)
        nused <- ifelse(sum(lids, na.rm = T)>=5, sum(lids, na.rm = T), 0)
        ntot <- nused + ntot
        ss3 <- ifelse(sum(lids, na.rm = T)>=5, sum(scores[lids]/nused, na.rm = T), NA)
        # sub-scale 4 (kehon painoon liittyvat huolet, 5 items: 8, 12, 22, 24, 25)
        lids <- qids %in% c(14027, 14023, 14019, 14017, 14016)
        nused <- ifelse(sum(lids, na.rm = T)>=3, sum(lids, na.rm = T), 0)
        ntot <- nused + ntot
        ss4 <- ifelse(sum(lids, na.rm = T)>=3, sum(scores[lids]/nused, na.rm = T), NA)
        return(list(total_scores = (ss1+ss2+ss3+ss4)/sum(!is.na(c(ss1,ss2,ss3,ss4))),
                    nitems = ntot))
      }
      if (!any(is.na(ddd$istunto))){
        ddd <- ddd[!duplicated(ddd[,c("istunto","kysymys")]), ] # remove possible duplicates
        maxses <- min(max(ddd$istunto, na.rm = T), slast) # last proper session
        ### BL scores ###
        blscores <- ddd$pisteet[ddd$istunto==1]
        # blqs <- ddd$kysymys[ddd$istunto==1]
        blqids <- ddd$kysymyksen_id[ddd$istunto==1]
        blscores <- form_EDEQ_scale(blscores, blqids)
        if (maxses == 1){
          res <- data.frame(BL = blscores$total_scores,
                            FU = NA,
                            nitemsBL = blscores$nitems,
                            nitemsFU = 0,
                            dropout = 1*(maxses < scompl),
                            last_session = maxses,
                            last_week = NA)
        } else {
          ### FU scores ###
          fuscores <- ddd$pisteet[ddd$istunto==maxses]
          fuqids <- ddd$kysymyksen_id[ddd$istunto==maxses]
          fuscores <- form_EDEQ_scale(fuscores, fuqids)
          res <- data.frame(BL = blscores$total_scores,
                            FU = fuscores$total_scores,
                            nitemsBL = blscores$nitems,
                            nitemsFU = fuscores$nitems,
                            dropout = 1*(maxses < scompl),
                            last_session = maxses,
                            last_week = NA)
        }
      } else {
        sweeks <- sort(unique(ddd$hoitoviikko[!is.na(ddd$pisteet)]), decreasing = TRUE)
        if (length(sweeks)==0){
          return(data.frame(BL = NA, FU = NA, nitemsBL = NA, nitemsFU = NA, 
                            dropout = NA, last_session = NA, last_week = NA))
        }
        if ((sweeks[1] > slast+10)&(length(sweeks)>1)){
          maxweek <- sweeks[2] # last proper session  
        } else {
          maxweek <- sweeks[1]
        }
        minweek <- sweeks[length(sweeks)]
        ### BL scores ###
        ddd <- ddd[!duplicated(ddd[,c("hoitoviikko","kysymys")]), ] # remove possible duplicates
        blscores <- ddd$pisteet[ddd$hoitoviikko==minweek]
        blqids <- ddd$kysymyksen_id[ddd$hoitoviikko==minweek]
        blscores <- form_EDEQ_scale(blscores, blqids)
        if (minweek == maxweek){
          res <- data.frame(BL = blscores$total_scores,
                            FU = NA,
                            nitemsBL = blscores$nitems,
                            nitemsFU = 0,
                            dropout = NA,
                            last_session = NA,
                            last_week = maxweek)
        } else {
          ### FU scores ###
          fuscores <- ddd$pisteet[ddd$hoitoviikko==maxweek]
          fuqids <- ddd$kysymyksen_id[ddd$hoitoviikko==maxweek]
          fuscores <- form_EDEQ_scale(fuscores, fuqids)
          res <- data.frame(BL = blscores$total_scores,
                            FU = fuscores$total_scores,
                            nitemsBL = blscores$nitems,
                            nitemsFU = fuscores$nitems,
                            dropout = NA,
                            last_session = NA,
                            last_week = maxweek)
        }
      }
      return(res)
    }
    # A generic sum-score function
    first_last_session <- function(ddd, scompl, slast, items){
      if ((nrow(ddd)==0)|(all(is.na(ddd$pisteet)))){
        return(data.frame(BL = NA, FU = NA, nitemsBL = NA, nitemsFU = NA, 
                          dropout = NA, last_session = NA, last_week = NA))
      }
      ddd <- ddd[!(ddd$kysymys %in% c("Valitse tilannettasi parhaiten kuvaava vaihtoehto.")), ]
      if (!any(is.na(ddd$istunto))){
        ddd <- ddd[!duplicated(ddd[,c("istunto","kysymys")]), ] # remove possible duplicates
        maxses <- min(max(ddd$istunto, na.rm = T), slast) # last proper session
        blscores <- ddd$pisteet[ddd$istunto==1]
        if (maxses == 1){
          res <- data.frame(BL = sum(blscores, na.rm = T),
                            FU = NA,
                            nitemsBL = sum(!is.na(blscores)),
                            nitemsFU = 0,
                            dropout = 1*(maxses < scompl),
                            last_session = maxses,
                            last_week = NA)
        } else {
          fuscores <- ddd$pisteet[ddd$istunto==maxses]
          res <- data.frame(BL = sum(blscores, na.rm = T),
                            FU = sum(fuscores, na.rm = T),
                            nitemsBL = sum(!is.na(blscores)),
                            nitemsFU = sum(!is.na(fuscores)),
                            dropout = 1*(maxses < scompl),
                            last_session = maxses,
                            last_week = NA)
        }
      } else { # use weeks
        sweeks <- sort(unique(ddd$hoitoviikko[!is.na(ddd$pisteet)]), decreasing = TRUE)
        if (length(sweeks)==0){
          return(data.frame(BL = NA, FU = NA, nitemsBL = NA, nitemsFU = NA, 
                            dropout = NA, last_session = NA, last_week = NA))
        }
        if ((sweeks[1] > slast+10)&(length(sweeks)>1)){
          maxweek <- sweeks[2] # last proper session  
        } else {
          maxweek <- sweeks[1]
        }
        minweek <- sweeks[length(sweeks)]
        ddd <- ddd[!duplicated(ddd[,c("hoitoviikko","kysymys")]), ] # remove possible duplicates
        blscores <- ddd$pisteet[ddd$hoitoviikko==minweek]
        if (minweek == maxweek){
          res <- data.frame(BL = sum(blscores, na.rm = T),
                            FU = NA,
                            nitemsBL = sum(!is.na(blscores)),
                            nitemsFU = 0,
                            dropout = NA,
                            last_session = NA,
                            last_week = maxweek)
        } else {
          fuscores <- ddd$pisteet[ddd$hoitoviikko==maxweek]
          res <- data.frame(BL = sum(blscores, na.rm = T),
                            FU = sum(fuscores, na.rm = T),
                            nitemsBL = sum(!is.na(blscores)),
                            nitemsFU = sum(!is.na(fuscores)),
                            dropout = NA,
                            last_session = NA,
                            last_week = maxweek)
        }
      }
      return(res)
    }
    # Do it per therapy
    if (any(dd$nettiterapia == "Alkoholin liikakäytön nettiterapia", na.rm = T)){
      # inventory (measure) index (several indices for the same inventory)
      ii <- c(8318, 7563) # AUDIT, 1-5-S
      res <- cbind( data.frame(inventory = "AUDIT", 
                               inventory_name = tryCatch(dd$kysely[dd$kyselyn_id %in% ii][1], error=function(e) return(NA))),
                    first_last_session(dd[dd$kyselyn_id %in% ii, ],
                                       scompl = 5, slast = 5) ) # scompl/slast >= 0.7
    }
    if (any(dd$nettiterapia == "Bulimian nettiterapia", na.rm = T)){
      ii <- c(8312) # EDE-Q, 1-4-8-S
      res <- cbind( data.frame(inventory = "EDE-Q", 
                               inventory_name = tryCatch(dd$kysely[dd$kyselyn_id %in% ii][1], error=function(e) return(NA))),
                    EDEQ_scoring(dd[dd$kyselyn_id %in% ii, ],
                                 scompl = 8, slast = 8) ) # scompl/slast >= 0.7
    }
    if (any(dd$nettiterapia == "Kaksisuuntaisen mielialahäiriön nettiterapia", na.rm = T)){
      ii <- c(8339, 14745, 6971, 14746, 6404, 6854, 19767) # PHQ-9, 1-4-8-14-S
      res <- cbind( data.frame(inventory = "PHQ9", 
                               inventory_name = tryCatch(dd$kysely[dd$kyselyn_id %in% ii][1], error=function(e) return(NA))),
                    first_last_session(dd[dd$kyselyn_id %in% ii, ],
                                       scompl = 14, slast = 14) ) # scompl/slast >= 0.7
      
    }
    if (any(dd$nettiterapia == "Masennuksen nettiterapia", na.rm = T)){
      ii <- c(8339, 14745, 6971, 14746, 6404, 6854, 19767) # PHQ-9, 1-3-5-7-S
      res <- cbind( data.frame(inventory = "PHQ9", 
                               inventory_name = tryCatch(dd$kysely[dd$kyselyn_id %in% ii][1], error=function(e) return(NA))),
                    first_last_session(dd[dd$kyselyn_id %in% ii, ],
                                       scompl = 5, slast = 7) ) # scompl/slast >= 0.7
    }
    if (any(dd$nettiterapia == "Nuorten sosiaalisen ahdistu&shy:neisuuden nettiterapia", na.rm = T)){
      ii <- c(9625, 6483, 9480, 7378, 4226, 4226, 9461) # SPIN, 1-4-8-S # WRONG inventory
      res <- cbind( data.frame(inventory = "SPIN", 
                               inventory_name = tryCatch(dd$kysely[dd$kyselyn_id %in% ii][1], error=function(e) return(NA))),
                    first_last_session(dd[dd$kyselyn_id %in% ii, ],
                                       scompl = 8, slast = 8) ) # scompl/slast >= 0.7
    }
    if (any(dd$nettiterapia == "Pakko-oireisen häiriön nettiterapia", na.rm = T)){
      ii <- c(9631, 6963, 9478) # OCI-R, 1-4-7-10-S
      res <- cbind( data.frame(inventory = "OCI-R", 
                               inventory_name = tryCatch(dd$kysely[dd$kyselyn_id %in% ii][1], error=function(e) return(NA))),
                    first_last_session(dd[dd$kyselyn_id %in% ii, ],
                                       scompl = 7, slast = 10) ) # scompl/slast >= 0.7
    }
    if (any(dd$nettiterapia == "Paniikkihäiriön nettiterapia", na.rm = T)){
      ii <- c(9630, 6584, 9479) # PDSS-SR, 1-3-5-8-S
      res <- cbind( data.frame(inventory = "PDSS-SR", 
                               inventory_name = tryCatch(dd$kysely[dd$kyselyn_id %in% ii][1], error=function(e) return(NA))),
                    first_last_session(dd[dd$kyselyn_id %in% ii, ],
                                       scompl = 8, slast = 8) ) # scompl/slast >= 0.7
    }
    if (any((dd$nettiterapia == "pitkäaikaisten ja haittaavien kehollisten oireiden kuntoutusohjelma"), na.rm = T)){ # vanha PITKO
      ii <- c(9842, 12800) # BIBQ, 1-2-3-4-5-6-S
      res <- cbind( data.frame(inventory = "BIBQ_old", 
                               inventory_name = tryCatch(dd$kysely[dd$kyselyn_id %in% ii][1], error=function(e) return(NA))),
                    first_last_session(dd[dd$kyselyn_id %in% ii, ],
                                       scompl = 5, slast = 6) ) # scompl/slast >= 0.7
    }
    if (any((dd$nettiterapia == "PITKO – Pitkäaikaisten ja haittaavien kehollisten oireiden kuntoutusohjelma"), na.rm = T)){ # uusi PITKO
      ii <- c(9842, 12800) # BIBQ, 1-2-4-6-8-10-S
      res <- cbind( data.frame(inventory = "BIBQ_new", 
                               inventory_name = tryCatch(dd$kysely[dd$kyselyn_id %in% ii][1], error=function(e) return(NA))),
                    first_last_session(dd[dd$kyselyn_id %in% ii, ],
                                       scompl = 8, slast = 10) ) # scompl/slast >= 0.7
    }
    if (any(dd$nettiterapia == "Sosiaalisten tilanteiden pelon nettiterapia", na.rm = T)){
      ii <- c(9625, 6483, 9480, 7378, 4226, 4226, 9461) # SPIN, 1-3-5-7-S
      res <- cbind( data.frame(inventory = "SPIN", 
                               inventory_name = tryCatch(dd$kysely[dd$kyselyn_id %in% ii][1], error=function(e) return(NA))),
                    first_last_session(dd[dd$kyselyn_id %in% ii, ],
                                       scompl = 5, slast = 7) ) # scompl/slast >= 0.7
    }
    if (any(dd$nettiterapia == "Yleistyneen ahdistuneisuushäiriön nettiterapia", na.rm = T)){
      ii <- c(9624, 6579, 9477, 6291, 6401) # GAD, 1-2-3-4-5-6-7-8-9-10-11-12-S
      res <- cbind( data.frame(inventory = "GAD7", 
                               inventory_name = tryCatch(dd$kysely[dd$kyselyn_id %in% ii][1], error=function(e) return(NA))),
                    first_last_session(dd[dd$kyselyn_id %in% ii, ],
                                       scompl = 9, slast = 12) ) # scompl/slast >= 0.7
    }
    return(res)
  }
  
  # link patient data with treatment id and treatment-start and -end date
  names(d_patients)[names(d_patients)=="id"] <- "potilaan_id"
  d <- dplyr::full_join(d_patients, d_participants, by = c("potilaan_id","FID"))
  # remove less-used variables and values (these may be needed later by some)
  d <- subset(d, select = c(FID, potilaan_id, ika, sukupuoli, kunta, 
                            ensimmainen_omapolku_kirjautuminen, luontihetki, 
                            kieli, alkamispaiva, paattymispaiva, hoidon_id))
  d_treatments <- subset(d_treatments, 
                         select = c(FID, id, hoitopolun_id, keskeytynyt,
                                    seurantatila_alkanut, kayttanyt_oirepaivakirjaa,
                                    drop_outin_tila, nettiterapia_loppui,
                                    kontakteja_viikossa))
  names(d_treatments)[names(d_treatments) == "id"] <- "hoidon_id"
  tmpind <- d_questionnaires$kysely %in% c("AUDIT-C - Alkoholin ajankohtainen kulutus",
                                           "AUDIT-C - Alkoholin ajankohtainen kulutus (muokattu CE, ei sertifioitu)",
                                           "AUDIT-C - Alkoholin ajankohtainen kulutus CE",
                                           "AUDIT - Alkoholinkäytön kysely",
                                           "CIA - Syömishäiriön haittaavuus",
                                           "EDE-Q - Syömishäiriön oireet",
                                           "GAD-7", "GAD-7 - Ahdistuneisuuskysely",
                                           "GAD-7 - Ahdistuneisuuskysely CE",
                                           "HUS BIPQ", "HUS PHQ-4", 
                                           "ISI - Unettomuuskysely, HUS",
                                           "OASIS", "OASIS - Ahdistuneisuuden haitta",
                                           "OCI-R", "OCI-R - Pakko-oireiden kysely",
                                           "OCI-R - Pakko-oireiden kysely CE",
                                           "PDSS-SR - Paniikkioirekysely",
                                           "PDSS-SR - Paniikkioirekysely CE",
                                           "PHQ-9 - Masennusoireet",
                                           "SAFE - turvakäyttäytymiskysely",
                                           "Spin - Lasten ja nuorten sosiaalinen ahdistuneisuus",
                                           "SPIN - nuoret", "SPIN - Sosiaalisten tilanteiden pelko",
                                           "SPIN - Sosiaalisten tilanteiden pelko CE")
  d_questionnaires <- d_questionnaires[tmpind,]
  d_taskexec <- d_taskexec[!is.na(d_taskexec$liittyvan_vastauksen_id),]
  d_answers <- subset(d_answers, select = -c(ensimmainen_hoidon_vastaus,
                                             viimeinen_hoidon_vastaus))
  names(d_answers)[names(d_answers) == "id"] <- "vastauksen_id"
  names(d_taskexec)[names(d_taskexec) == "liittyvan_vastauksen_id"] <- "vastauksen_id"
  # link question information
  d_taskexec <- dplyr::left_join(d_taskexec, d_answers, 
                                 by = c("vastauksen_id","FID","hoidon_id"))
  d_taskexec <- dplyr::left_join(d_taskexec, d_question_answers, 
                                 by = c("vastauksen_id","FID"))
  names(d_questionnaires)[names(d_questionnaires) == "id"] <- "kyselyn_id"
  names(d_questions)[names(d_questions) == "id"] <- "kysymyksen_id"
  d_taskexec <- dplyr::left_join(d_taskexec, d_questionnaires, by = c("kyselyn_id"))
  d_taskexec <- dplyr::left_join(d_taskexec, d_questions, by = c("kysymyksen_id"))
  
  # link treatment information
  d_taskexec <- dplyr::full_join(d_taskexec, d_treatments, by = c("FID", "hoidon_id"))
  names(d_iCBTs)[names(d_iCBTs) == "id"] <- "hoitopolun_id"
  d_taskexec <- dplyr::left_join(d_taskexec, d_iCBTs[,c("hoitopolun_id","nettiterapia")], 
                                 by = "hoitopolun_id")
  d_taskexec <- dplyr::left_join(d_taskexec, d_tasks[,c("tehtavan_id","istunto")], 
                                 by = "tehtavan_id")
  
  # # Remove unexplainable duplicates
  # dupls <- d_taskexec %>%
  #   dplyr::group_by(FID, kysymys, hoidon_id) %>%
  #   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  #   dplyr::filter(n > 1L)
  
  # Remove repeated same therapies
  dupls <- d_taskexec[,c("FID", "hoidon_id", "nettiterapia")] %>%
    dplyr::distinct() %>%
    dplyr::group_by(FID, nettiterapia) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1L)
  # nrow(dupls) # 308 patients
  
  # dropt <- rep("", nrow(dupls)) # treatments to be removed
  dropt <- c() # treatments to be removed
  for (i in 1:nrow(dupls)){
    dtmp <- d_taskexec[(dupls$FID[i] == d_taskexec$FID)&
                         (dupls$nettiterapia[i] == d_taskexec$nettiterapia),
                       c("FID", "hoidon_id", "nettiterapia", "suorituspaiva")]
    if (all(is.na(dtmp$suorituspaiva))){ # if all therapies empty, just take 1
      dropt <- c(dropt, unique(dtmp$hoidon_id)[2:dupls$n[i]])
    } else {
      dropt <- c(dropt, setdiff(unique(dtmp$hoidon_id),
                                dtmp$hoidon_id[which(dtmp$suorituspaiva == min(dtmp$suorituspaiva, na.rm = T))[1]]))
    }
  }
  d_taskexec <- d_taskexec[!(d_taskexec$hoidon_id %in% dropt), ] # dropping 314 treatments
  
  # Compute outcome by treatment - initialize data frame
  utid <- setdiff(unique(d_treatments$hoidon_id),dropt) # unique treatment IDs
  d <- data.frame(matrix(NA, length(utid), 12))
  names(d) <- c("FID", "treatment_id", "iCBT", "inventory", "inventory_name",
                "BL", "FU", "nitemsBL", "nitemsFU", "dropout", "last_session", "lastweek")
  for (i in 1:5) class(d[[i]]) <- "character"
  for (i in 6:12) class(d[[i]]) <- "numeric"
  # Add dates and demographics
  d <- cbind(d, cdate = as.Date(rep(NA, nrow(d))), 
             cdateFU = as.Date(rep(NA, nrow(d))),
             age = as.numeric(rep(NA,nrow(d))), 
             sex = as.character(rep(NA, nrow(d))),
             municipality = as.character(rep(NA, nrow(d))))
  # Loop through the unique treatments
  t0 <- proc.time()
  for (i in 1:length(utid)){
    # ( i <- d_taskexec$hoidon_id[d_taskexec$nettiterapia == "Yleistyneen ahdistuneisuushäiriön nettiterapia"][1] )
    # dtmp <- d_taskexec[d_taskexec$hoidon_id == i, ]
    dtmp <- d_taskexec[d_taskexec$hoidon_id == utid[i], ]
    d$FID[i] <- unique(dtmp$FID) # throws an error if not unique
    d$treatment_id[i] <- utid[i]
    d$iCBT[i] <- unique(dtmp$nettiterapia)
    d[i,4:12] <- toutcome(dtmp)
    if (!is.na(d$last_session[i])){
      d$cdate[i] <- max(dtmp$suorituspaiva[dtmp$istunto==1], na.rm=T)
      d$cdateFU[i] <- max(dtmp$suorituspaiva[dtmp$istunto==d$last_session[i]], na.rm=T)
    } else {
      if (!is.na(d$lastweek[i])){
        d$cdate[i] <- max(dtmp$suorituspaiva[dtmp$hoitoviikko==1], na.rm=T)
        d$cdateFU[i] <- max(dtmp$suorituspaiva[dtmp$hoitoviikko==d$lastweek[i]], na.rm=T)
      } else {
        d$cdate[i] <- min(dtmp$suorituspaiva, na.rm=T)
      }
    }
    dtmp <- d_patients[d_patients$FID == d$FID[i], ]
    if (nrow(dtmp) > 0){
      d$sex[i] <- dtmp$sukupuoli[1]
      d$municipality[i] <- dtmp$kunta[1]
      if (!is.na(d$cdate[i])){
        d$age[i] <- dtmp$ika[1] + lubridate::interval(dtmp$luontihetki[1], d$cdate[i]) /
          lubridate::years(1)
      } else {
        d$age[i] <- dtmp$ika[1]
      }
    }
  }
  runtime <- proc.time() - t0 # Took 41.8 min
  # save(d, runtime, file = "new_iCBT_primary_measure_data.Rdata")
  # runtime/60 # 41.8 min
  
  # Add therapy-standardized outcome variables
  utid <- unique(d_taskexec$nettiterapia) # unique therapies
  d <- cbind(d, zBL = rep(0, nrow(d)), zFU = rep(0, nrow(d)))
  for (i in 1:length(utid)){
    linds <- d$iCBT == utid[i]
    blm <- mean(d$BL[linds], na.rm = T); blsd <- sd(d$BL[linds], na.rm = T)
    d$zBL[linds] <- (d$BL[linds] - blm)/blsd
    d$zFU[linds] <- (d$FU[linds] - blm)/blsd
  }
  d$sex[(!is.na(d$sex))&(d$sex == "mies")] <- "Male"
  d$sex[(!is.na(d$sex))&(d$sex == "nainen")] <- "Female"
  # save(d, runtime, file = "new_iCBT_primary_measure_data.Rdata")
  return(d)
}

#### FPQR ####

dataToBLFU_Kapseli <- function(dorig, items, dvis, septh=30, interrupt_ids=NULL, use_orig_lab=FALSE){
  # A sub-function to take first non-missing value
  FirstNonMissing <- function(x) ifelse(any(!is.na(x)),x[!is.na(x)][1],NA)
  
  # Take the item set only, plus unique patients
  iset <- dorig[,c("patient_id",items,"treatment_id","visit_id","date_created")]
  if (use_orig_lab){
    names(iset) <- c("id", items,"treatment_id","visit_id","cdate")
  } else {
    names(iset) <- c("id", paste0("BL",1:length(items)),"treatment_id","visit_id","cdate")
  }
  iset$cdate <- lubridate::as_date(iset$cdate)
  uds <- unique(iset$id) # unique patients
  
  # Find patients with multiple treatments and deal with them later
  mts_ids <- sapply(uds, function(x) length(unique(dorig$treatment_id[dorig$patient_id == x])) > 1)
  mts_ids <- uds[mts_ids]
  uds <- setdiff(uds, mts_ids)
  
  # Take unique duplicated IDs
  udis <- unique(setdiff(iset$id[duplicated(iset$id)], mts_ids))
  
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
    dtmp <- iset[iset$id == uds[i],]
    ciset$id[i] <- uds[i]
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
  
  # Add patients with multiple treatments
  if (length(mts_ids) > 0) {
    for (i in 1:length(mts_ids)){
      treatments <- unique(dorig$treatment_id[dorig$patient_id == mts_ids[i]])
      ntreatments <- length(treatments)
      for (j in 1:ntreatments){
        ciset <- rbind(ciset,rep(NA,ncol(ciset)))
        dtmp <- iset[(iset$id == mts_ids[i])&(iset$treatment_id == treatments[j]),]
        ciset$id[nrow(ciset)] <- mts_ids[i]
        # E.g. audit items wind up to multiple rows: take first non-empty
        ciset[nrow(ciset),2:ncol(iset)] <-
          cbind(data.frame(lapply(dtmp[dtmp$cdate==min(dtmp$cdate),2:(ncol(dtmp)-1)],
                                  FirstNonMissing)), cdate = min(dtmp$cdate))
        if (length(unique(dtmp$cdate))>1){
          ciset[nrow(ciset), (ncol(iset)+1):ncol(ciset)] <-
            cbind(data.frame(lapply(dtmp[dtmp$cdate==max(dtmp$cdate),2:(ncol(dtmp)-1)],
                                    FirstNonMissing)), cdate = max(dtmp$cdate))
        }
      }
    }
  }
  
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
    ind <- ciset$id %in% interrupt_ids
    ciset[ind,(ncol(iset)+1):ncol(ciset)] <- NA
  }
  
  # Replace working id label with original and return data
  names(ciset)[names(ciset)=="id"] <- "patient_id"
  return(ciset)
}

build_FPQR_data <- function(d, d_tre, d_pat, d_vis, short_therapies_only = T){
  # Creates tidy FPQR data frame with scores from the first therapy only / Tom R. 3.8.2023
  library(tidyr)
  library(dplyr) # this needed separately. Why?
  uids <- unique(d$patient_id) # unique ids
  # (Remove) patients in KELA treatments
  pmuoto_ids <- unique(d$patient_id[(d$string_answer == "kela") & (d$question_code == "palvelumuoto")])
  uids <- setdiff(uids, pmuoto_ids) # poimitaan vain ne jotka eivat ole kelaterapioissa, n= 7511
  
  # Restrict to adults, 6524
  uids <- uids[uids %in% d$patient_id[(d$question_code == 
                                         "erikoisala") & (d$string_answer == "aikuisten")]]
  
  # Uninitiated therapies, 2215                                                                                                                                 "psykoterapeutin_alkuarvio_lapset"))$patient_id)]
  uids_uninitiated <- uids[!(uids %in% dplyr::filter(d, (template_code == "psykoterapeutin_alkuarvio_nuoret") | 
                                                (template_code == "psykoterapeutin_alkuarvio_aikuiset") | 
                                                (template_code == "psykoterapeutin_alkuarvio_lapset"))$patient_id)]
  
  # Interrupted therapies, 195 
  dtmp <- dplyr::filter(d, (question_code== "psykoterapia_paattyi") | (question_code== "therapy_interruption")) 
  uids_interrupted <- uids[(uids %in% dtmp$patient_id[dtmp$string_answer %in% c("keskeytyi", "before_it_started", "without_final_termination")])]
  
  # interrupted adults, 188
  uids_interrupted <- uids_interrupted[uids_interrupted %in% dplyr::filter(d, template_code == "psykoterapeutin_alkuarvio_aikuiset")$patient_id]
  
  # completed psychotherapies, 3476
  uids_completed <- uids[(uids %in% dplyr::filter(d, (template_code == "psykoterapeutin_loppu_valiarvio_nuoret") |
                                             (template_code == "psykoterapeutin_loppu_valiarvio_aikuispotilaat") |
                                             (template_code == "psykoterapeutin_loppu_valiarvio_lapset"))$patient_id)]
  # check how numbers change when requiring final/midterm stamp too
  uids_completed_stamped <- rep(F, length(uids_completed)) # 97
  uids_midterm_only_stamped <- rep(F, length(uids_completed)) # 58
  for (i in 1:length(uids_completed)) {
    trids <- unique(d$treatment_id[d$patient_id == uids_completed[i]])
    uids_completed_stamped[i] <- any(d_vis$visit_type_id[d_vis$treatment_id %in% trids] %in% c("final", "midterm"))
    uids_midterm_only_stamped[i] <- any(d_vis$visit_type_id[d_vis$treatment_id %in% trids] == "midterm") &
      !any(d_vis$visit_type_id[d_vis$treatment_id %in% trids]== "final")
  }
  uids_midterm_only <- uids_completed[uids_midterm_only_stamped] # 51
  uids_completed <- uids_completed[uids_completed_stamped] # 3476-97 = 3379
  # remove interrupted
  uids_completed <- setdiff(uids_completed, uids_interrupted) # 3379 > 3203 (interrupted adults 188, 3379-188= 3191, 3203-3191=12, mitka 12?) 
  uids_midterm_only <- setdiff(uids_midterm_only, uids_interrupted) # 58
  
  ######## Then restrict to adults with one outsourced non-KELA therapy ######
  # Find patients with multiple treatments
  mts_ids <- sapply(uids_completed, function(x) length(unique(d$treatment_id[d$patient_id == x])) > 1)
  mts_ids <- uids_completed[mts_ids]
  uids_completed <- setdiff(uids_completed, mts_ids) # 3203-131= 3072
  uids_midterm_only <- setdiff(uids_midterm_only, mts_ids)
  
  # Remove internal psychotherapies, leave outsourced only
  int_ids <- sapply(uids_completed, function(x) ("sisainen" %in% d$string_answer[(d$patient_id == x) & (d$question_code == "palvelumuoto")]))
  int_ids <- uids_completed[int_ids]
  uids_completed <- setdiff(uids_completed, int_ids) # 3072-87= 2985
  uids_midterm_only <- setdiff(uids_midterm_only, int_ids)
  
  ################## DATA TABLES ###################
  
  # A function to drop the second treatment per table
  drop_second_treatments <- function(x){
    dids <- x$patient_id[duplicated(x$patient_id)]
    lxcol_treat <- grepl("treatment_id", names(x))&(!grepl("treatment_idFU", names(x)))
    lxcol_date <- grepl("cdate", names(x))&(!grepl("cdateFU", names(x)))
    if (length(dids) > 0){
      treatments_to_remove <- c()
      for (i in 1:length(dids)){
        tids <- x[x$patient_id == dids[i], lxcol_treat|lxcol_date]
        ltcol_date <- grepl("cdate", names(tids))&(!grepl("cdateFU", names(tids)))
        if (all(!is.na(tids[,ltcol_date]))){
          treatments_to_remove <- c(treatments_to_remove,
                                    tids[tids[,ltcol_date] != min(tids[,ltcol_date]),
                                         grepl("treatment_id", names(tids))])
        } else {
          treatments_to_remove <- c(treatments_to_remove,
                                    unique(tids[,grepl("treatment_id", names(tids))])[-1])
        }
      }
      x <- x[!(x[,lxcol_treat] %in% treatments_to_remove), ]
      return(x)
    }
  }
  
  ### Background info (new version 5.5.2023) ###
  dbag <- d %>%  # string_answer
    dplyr::filter(template_code == "taustatiedot") %>% 
    select(patient_id, date_created, template_code, question_code, string_answer, number_answer) %>% 
    group_by(question_code) %>% 
    mutate(row = row_number()) %>%  
    ungroup() %>%
    pivot_wider(names_from = question_code, values_from = string_answer) %>% 
    select(-row)
  
  # unique dbag
  dbagu <- data.frame(matrix("", length(uids), ncol(dbag)))
  names(dbagu) <- names(dbag)
  FirstNonMissing <- function(x) ifelse(any(!is.na(x)),x[!is.na(x)][1],NA)
  for (i in 1:length(uids)){
    dbagu$patient_id[i] <- uids[i]
    linds <- dbag$patient_id == uids[i]
    for (j in 2:ncol(dbag)){
      dbagu[i,j] <- FirstNonMissing(dbag[linds, j])
    }
  }

  dbagu <- cbind(dbagu, many_treatments = 
                  sapply(dbagu$patient_id, function(x) (length(unique(d$treatment_id[d$patient_id == x])) > 1) * 1))
  rm(dbag)
  
  ## Add all side diagnoses from all the treatments of a patient
  dbagu <- cbind(dbagu, sivudiagnoosit = as.character(rep(NA, nrow(dbagu))))
  for (i in 1:length(uids)){
    sdxs <- na.omit(d$list_answer[(d$patient_id == dbagu$patient_id[i])&
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
  d_fpqr <- subset(dbagu, select = c(patient_id, date_created, paadiagnoosi,
                                    sukupuoli, terapiamuoto, terapiasuuntaus,
                                    terapeutti, terapeutti_harmonized, 
                                    terapian_pituus, mista_lahetetty, sivudiagnoosit,
                                    therapy_interruption, many_treatments))
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
  
  dsof <- dataToBLFU_Kapseli(dsof, items = names(dsof)[c(2:6)], d_vis, 
                             interrupt_ids = uids_interrupted, use_orig_lab = T)
  names(dsof)[-1] <- paste0("sofas_", names(dsof)[-1])
  
  d_fpqr <- merge(d_fpqr, 
                  drop_second_treatments(dsof), 
                  by = "patient_id", all = T)
  
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
  dcom <- dataToBLFU_Kapseli(dcom, items = names(dcom)[3:36], d_vis, interrupt_ids = uids_interrupted)
  dcom <- drop_second_treatments(dcom) # drops nothing currently
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
  names(dtmp)[-1] <- paste0("coreom_",names(dtmp)[-1])
  
  names(d_fpqr)[names(d_fpqr)=="sofas_treatment_id"] <- "treatment_id"
  names(dtmp)[names(dtmp)=="coreom_treatment_id"] <- "treatment_id"
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
  
  dphq <- dataToBLFU_Kapseli(dphq, items = names(dphq)[6:14], d_vis, interrupt_ids = uids_interrupted)
  dphq <- drop_second_treatments(dphq)
  
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
  names(dtmp)[-1] <- paste0("phq_",names(dtmp)[-1])
  names(dtmp)[names(dtmp)=="phq_treatment_id"] <- "treatment_id"
  d_fpqr <- merge(d_fpqr, dtmp, by = c("patient_id", "treatment_id"), all = T)
  
  ### OASIS ###
  doasis <- d %>% dplyr::filter(template_code == "oasis") %>% 
    select(patient_id, date_created, template_code, question_code, number_answer, treatment_id, visit_id) %>%
    group_by(question_code) %>% 
    mutate(row = row_number()) %>% 
    ungroup() %>% 
    pivot_wider(names_from = question_code, values_from = number_answer) %>%
    select(-row)
  
  doasis <- dataToBLFU_Kapseli(doasis, items = names(doasis)[6:10], d_vis, interrupt_ids = uids_interrupted)
  doasis <- drop_second_treatments(doasis)
  
  doasis <- cbind(doasis,
                oasis = ifelse(apply(doasis[,paste0("BL",1:5)],1,function(x) !any(is.na(x))),
                             rowSums(doasis[,paste0("BL",1:5)]), NA),
                oasisFU = ifelse(apply(doasis[,paste0("FU",1:5)],1,function(x) !any(is.na(x))),
                               rowSums(doasis[,paste0("FU",1:5)]), NA))
  # Combine scores etc. only
  dtmp <- doasis[, c("patient_id", "treatment_id", "visit_id", "cdate", 
                   "oasis", "oasisFU", 
                   "treatment_idFU", "visit_idFU", "cdateFU")]
  names(dtmp)[-1] <- paste0("oasis_",names(dtmp)[-1])
  names(dtmp)[names(dtmp)=="oasis_treatment_id"] <- "treatment_id"
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
  
  daudit <- dataToBLFU_Kapseli(daudit, items = names(daudit)[2 + c(4, 5, 8)], d_vis, interrupt_ids = uids_interrupted)
  daudit <- drop_second_treatments(daudit)
  daudit <- cbind(daudit,
                  audit = rowSums(daudit[,paste0("BL",1:3)], na.rm = T),
                  auditFU = rowSums(daudit[,paste0("FU",1:3)], na.rm = T))
  # Combine scores etc. only
  dtmp <- daudit[, c("patient_id", "treatment_id", "visit_id", "cdate", 
                     "audit", "auditFU", "treatment_idFU", "visit_idFU", "cdateFU")]
  names(dtmp)[-1] <- paste0("audit_",names(dtmp)[-1])
  names(dtmp)[names(dtmp)=="audit_treatment_id"] <- "treatment_id"
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
  
  # A function to detect short adult therapies (modified 3.8.2023)
  IsShortTherapy <- function(id){
    # x <- d[d$patient_id== id, ]
    x <- d[d$treatment_id== id, ]
    short_therapy <- ("aikuisten" %in% x$string_answer[x$question_code== "erikoisala"]) &
      ("lyhyt" %in% x$string_answer[x$question_code== "terapian_pituus"])
    return(short_therapy)
  }
  
  # A function to detect internally-/HUS-delivered adult therapies (modified 3.8.2023)
  IsInternalTherapy <- function(id){
    # x <- d[d$patient_id== id, ]
    x <- d[d$treatment_id== id, ]
    internal_therapy <- ("aikuisten" %in% x$string_answer[x$question_code== "erikoisala"]) & 
      ("sisainen" %in% x$string_answer[x$question_code== "palvelumuoto"])
    return(internal_therapy)
  }
  
  # Then add definitions
  d_fpqr <- cbind(d_fpqr, adult_primary_care = rep(NA,nrow(d_fpqr)),
                  short_therapy = rep(NA,nrow(d_fpqr)),
                  internal_therapy = rep(NA,nrow(d_fpqr)))
  for (i in 1:nrow(d_fpqr)){
    if (!is.na(d_fpqr$treatment_id[i])){
      d_fpqr$adult_primary_care[i] <- IsAdultPrimaryCare(d_fpqr$treatment_id[i])
      d_fpqr$short_therapy[i] <- IsShortTherapy(d_fpqr$treatment_id[i])
      d_fpqr$internal_therapy[i] <- IsInternalTherapy(d_fpqr$treatment_id[i])
    }
  } # ~11 min.
  # save(d_fpqr, file = "FPQR_data.Rdata")
  # save(d_fpqr, file = "FPQR_data_sums_cores.Rdata")
  return(d_fpqr)
}

#### Old iCBT ####

build_old_iCBT_data <- function(){
  # dVT.Rdata produced by Enna Kujansuu and Sanna Myllari.
  library(dplyr)
  
  fp <- "W:/data/HUS/vanha_nettiterapia/" # Tarkista tuoreimman toimituksen polku
  # Files copied here from:
  # D:/Data_THL_4810_140200_2020_osa2/HUS/vanha_nettiterapia/ (GAD & masennus)
  # D:/Data_THL_4810_140200_2020_osa4/HUS/vanha_nettiterapia/ (others)
  
  # Load symptom data
  dd <- read.csv(paste0(fp, "masennus/FD_4810_masennus_Mittarit.csv"),
                 sep = ";", encoding = "UTF-8")
  dg <- read.csv(paste0(fp, "gad/FD_4810_gad_Mittarit.csv"),
                 sep = ";", encoding = "UTF-8")
  dp <- read.csv(paste0(fp, "paniikki/FD_4810_paniikki_Mittarit.csv"),
                 sep = ";", encoding = "UTF-8")
  da <- read.csv(paste0(fp, "alko/FD_4810_alko_Mittarit.csv"),
                 sep = ";", encoding = "UTF-8")
  db <- read.csv(paste0(fp, "bipo/FD_4810_bipo_Mittarit.csv"),
                 sep = ";", encoding = "UTF-8")
  ds <- read.csv(paste0(fp, "sosfob/FD_4810_sosfob_Mittarit.csv"),
                 sep = ";", encoding = "UTF-8")
  do <- read.csv(paste0(fp, "ocd/FD_4810_ocd_Mittarit.csv"),
                 sep = ";", encoding = "UTF-8")
  
  
  
  ###
  # Masennus (dd) ----
  
  colnames(dd)
  
  # filter relevant columns (BDI)
  dd <- dd %>%
    dplyr::filter(Title == "Mittari, BDI") %>%
    select(c("Title", "Author", "CreatedInSession", "Created_x0020_Date", starts_with("BDI")))
  n_distinct(dd$Author) # 3835 different patients
  # BDI22 on summamuuttujapistem??r?
  names(dd)[names(dd) == "BDI22"] <- "sum_score"
  
  # create variable for the last session each patient has data
  # max(dd$CreatedInSession) # 8
  # table(dd$CreatedInSession) # tietoa 1, 3 ja 7/8 kerroilta
  dd <- dd %>%
    group_by(Author) %>%
    mutate(num_sessions = max(CreatedInSession)) %>%
    mutate(num_sessions = case_when(num_sessions == 8 ~ as.integer(7),
                                    TRUE ~ num_sessions))
  
  # add new variables to indicate treatment and session maxses per therapy
  dd$session_max <- rep("7", nrow(dd))
  dd$treatment <- rep("masennus", nrow(dd))
  
  
  ###
  # GAD (dg) ----
  
  colnames(dg)
  
  # filter relevant columns (GAD-7 nimetty kryptisesti X_x0047_AD7)
  dg <- dg %>%
    dplyr::filter(Title == "Mittari, GAD-7") %>%
    select(c("Title", "Author", "CreatedInSession", "Created_x0020_Date", 
             starts_with("X_x0047_AD7")))
  n_distinct(dg$Author) # 2209
  # X_x0047_AD7 summamuuttujapistem??r?
  names(dg)[names(dg) == "X_x0047_AD7"] <- "sum_score"
  
  names(dg)[names(dg) == "X_x0047_AD701"] <- "GAD7_01"
  names(dg)[names(dg) == "X_x0047_AD702"] <- "GAD7_02"
  names(dg)[names(dg) == "X_x0047_AD703"] <- "GAD7_03"
  names(dg)[names(dg) == "X_x0047_AD704"] <- "GAD7_04"
  names(dg)[names(dg) == "X_x0047_AD705"] <- "GAD7_05"
  names(dg)[names(dg) == "X_x0047_AD706"] <- "GAD7_06"
  names(dg)[names(dg) == "X_x0047_AD707"] <- "GAD7_07"
  
  # create variable for the last session each patient has data
  # max(dg$CreatedInSession) # 13
  # table(dg$CreatedInSession) # tietoa kaikilta kerroilta 1-13
  dg <- dg %>%
    group_by(Author) %>%
    mutate(num_sessions = max(CreatedInSession)) %>%
    mutate(num_sessions = case_when(num_sessions == 13 ~ as.integer(12),
                                    TRUE ~ num_sessions))
  
  # add new variables to indicate treatment and session maxses per therapy
  dg$session_max <- rep("12", nrow(dg))
  dg$treatment <- rep("gad", nrow(dg))
  
  
  
  ###
  # Paniikki (dp) ----
  
  colnames(dp)
  
  # filter relevant columns (PDSS)
  dp <- dp %>%
    dplyr::filter(Title == "Mittari, PDSS") %>%
    select(c("Title", "Author", "CreatedInSession", "Created_x0020_Date",
             starts_with("PDSS")))
  n_distinct(dp$Author) # 1069
  # PDSS08 summamuuttujapistem??r??
  names(dp)[names(dp) == "PDSS08"] <- "sum_score"
  
  # create variable for the last session each patient has data
  # max(dp$CreatedInSession) # 11
  # table(dp$CreatedInSession) # tietoa 1 ja 10/11 kerroilta
  dp <- dp %>%
    group_by(Author) %>%
    mutate(num_sessions = max(CreatedInSession)) %>%
    mutate(num_sessions = case_when(num_sessions == 11 ~ as.integer(10),
                                    TRUE ~ num_sessions))
  
  # add new variables to indicate treatment and session maxses per therapy
  dp$session_max <- rep("10", nrow(dp))
  dp$treatment <- rep("paniikki", nrow(dp))
  
  ###
  # Alko (da) ----
  
  colnames(da)
  
  # filter relevant columns (Audit)
  da <- da %>%
    dplyr::filter(Title == "Mittari, Audit") %>%
    select(c("Title", "Author", "CreatedInSession", "Created_x0020_Date", 
             starts_with("Audit")))
  n_distinct(da$Author) # 204
  # Audit11 summamuuttujapistem??r?
  names(da)[names(da) == "Audit11"] <- "sum_score"
  
  # create variable for the last session each patient has data
  # max(da$CreatedInSession) # 6
  # table(da$CreatedInSession) # tietoa l?hinn? 1 ja 5/6 kerroilta
  da <- da %>%
    group_by(Author) %>%
    mutate(num_sessions = max(CreatedInSession)) %>%
    mutate(num_sessions = case_when(num_sessions == 6 ~ as.integer(5),
                                    TRUE ~ num_sessions))
  
  # add new variables to indicate treatment and session maxses per therapy
  da$session_max <- rep("5", nrow(da))
  da$treatment <- rep("alko", nrow(da))
  
  
  ###
  # Bipo (db) ----
  
  colnames(db)
  
  # filter relevant columns (BDI)
  db <- db %>%
    dplyr::filter(Title == "Mittari, BDI") %>%
    select(c("Title", "Author", "CreatedInSession", "Created_x0020_Date", 
             starts_with("BDI")))
  n_distinct(db$Author) # 329
  # BDI22 summamuuttujapistem??r?
  names(db)[names(db) == "BDI22"] <- "sum_score"
  
  # create variable for the last session each patient has data
  # max(db$CreatedInSession) # 15
  # table(db$CreatedInSession) # tietoja vain 1 ja 14/15 kerroilta
  db <- db %>%
    group_by(Author) %>%
    mutate(num_sessions = max(CreatedInSession)) %>%
    mutate(num_sessions = case_when(num_sessions == 15 ~ as.integer(14),
                                    TRUE ~ num_sessions))
  
  # add new variables to indicate treatment and session maxses per therapy
  db$session_max <- rep("14", nrow(db))
  db$treatment <- rep("bipo", nrow(db))
  
  
  
  ###
  # Sos.fob. (ds) ----
  
  colnames(ds)
  
  # filter relevant columns (SPIN-FIN)
  ds <- ds %>%
    dplyr::filter(Title == "Mittari, SPIN-FIN") %>%
    select(c("Title", "Author", "CreatedInSession", "Created_x0020_Date", 
             starts_with("SPINFIN")))
  n_distinct(ds$Author) # 759
  # SPINFIN18 summamuuttujapistem??r?
  names(ds)[names(ds) == "SPINFIN18"] <- "sum_score"
  
  # create variable for the last session each patient has data
  max(ds$CreatedInSession) # 8
  table(ds$CreatedInSession) # tietoa vain 1 ja 7/8 kerroilta
  ds <- ds %>%
    group_by(Author) %>%
    mutate(num_sessions = max(CreatedInSession)) %>%
    mutate(num_sessions = case_when(num_sessions == 8 ~ as.integer(7),
                                    TRUE ~ num_sessions))
  
  # add new variables to indicate treatment and session maxses per therapy
  ds$session_max <- rep("7", nrow(ds))
  ds$treatment <- rep("sosfob", nrow(ds))
  
  
  ###
  # OCD (do) ----
  
  colnames(do)
  
  # filter relevant columns (OCI-R)
  do <- do %>%
    dplyr::filter(Title == "Mittari, OCD") %>%
    select(c("Title", "Author", "CreatedInSession", "Created_x0020_Date", 
             starts_with("OCD")))
  n_distinct(do$Author) # 613
  # OCD19 summamuuttujapistem??r?
  names(do)[names(do) == "OCD19"] <- "sum_score"
  
  # mit? on OCD19-25?
  # poistetaan toistaiseksi
  do <- do %>%
    select(-c(OCD20, OCD21, OCD22, OCD23, OCD24, OCD25))
  
  # create variable for the last session each patient has data
  max(do$CreatedInSession) # 11
  table(do$CreatedInSession) # tietoa l?hinn? 1 ja 10/11 kerroilta
  do <- do %>%
    group_by(Author) %>%
    mutate(num_sessions = max(CreatedInSession)) %>%
    mutate(num_sessions = case_when(num_sessions == 11 ~ as.integer(10),
                                    TRUE ~ num_sessions))
  
  # add new variables to indicate treatment and session maxses per therapy
  do$session_max <- rep("10", nrow(do))
  do$treatment <- rep("ocd", nrow(do))
  
  
  
  ###
  # Check for duplicates ----
  checkids <- dd %>%
    group_by(Author, CreatedInSession) %>%
    summarise(n = n())
  ddoubles <- unique(checkids$Author[checkids$n > 1]) # 21

  checkids <- dg %>%
    group_by(Author, CreatedInSession) %>%
    summarise(n = n())
  gdoubles <- unique(checkids$Author[checkids$n > 1]) # 25

  checkids <- da %>%
    group_by(Author, CreatedInSession) %>%
    summarise(n = n())
  adoubles <- unique(checkids$Author[checkids$n > 1]) # 1

  checkids <- db %>%
    group_by(Author, CreatedInSession) %>%
    summarise(n = n())
  bdoubles <- unique(checkids$Author[checkids$n > 1]) # 0

  checkids <- do %>%
    group_by(Author, CreatedInSession) %>%
    summarise(n = n())
  odoubles <- unique(checkids$Author[checkids$n > 1]) # 3

  #checkids <- dp %>%
  # group_by(Author, CreatedInSession) %>%
  # summarise(n = n())
  # pdoubles <- unique(checkids$Author[checkids$n > 1]) # 0

  # checkids <- ds %>%
  # group_by(Author, CreatedInSession) %>%
  # summarise(n = n())
  # sdoubles <- unique(checkids$Author[checkids$n > 1]) # 0
  
  # suurin osa vaikuttaa tuplatallennuksilta -> poistetaan identtiset rivit 
  dd <- distinct(dd)
  dg <- distinct(dg)
  da <- distinct(da)
  do <- distinct(do)
  db <- distinct(db)
  
  # ei poistanut kaikkia, joten poistetaan potilaat
  dd <- dd[!(dd$Author %in% ddoubles), ]
  dg <- dg[!(dg$Author %in% gdoubles), ]
  da <- da[!(da$Author %in% adoubles), ]
  do <- do[!(do$Author %in% odoubles), ]
  db <- db[!(db$Author %in% bdoubles), ]
  
  
  
  ###
  # Function for new variables ----
  
  outcomes <- function(x) {
    if(x$treatment[1] == "masennus") {
      maxses = 7
    } else if (x$treatment[1] == "gad") {
      maxses = 12
    } else if (x$treatment[1] == "paniikki") {
      maxses = 10
    } else if (x$treatment[1] == "alko") {
      maxses = 5
    } else if (x$treatment[1] == "bipo") {
      maxses = 14
    } else if (x$treatment[1] == "sosfob") {
      maxses = 7
    } else if (x$treatment[1] == "ocd") {
      maxses = 10
    }
    
    new_df <- x %>%
      group_by(Author) %>%
      mutate(dropout = case_when(num_sessions < maxses ~ 1,
                                 TRUE ~ 0), # drop outin kriteerin? kaikkien hoitosessioiden k?yminen
             dropout2 = case_when(num_sessions <= 0.7*as.numeric(session_max) ~ 1,
                                  TRUE ~ 0),
             blscores = sum_score[CreatedInSession == 1],
             fuscores = sum_score[CreatedInSession == num_sessions],
             changescores = blscores - fuscores,
             cdate = as.Date(Created_x0020_Date[CreatedInSession == 1]),
             cdateFU = as.Date(Created_x0020_Date[CreatedInSession == num_sessions]))
    # 
    # x$dropout = case_when(x$num_sessions < maxses ~ 1,
    #                       TRUE ~ 0) # HUOM, t?ss? drop outin kriteerin? kaikkien hoitosessioiden k?yminen
    # x$dropout <- as.factor(x$dropout)
    # 
    # x$blscores <- x$sum_score[x$CreatedInSession == 1]
    # x$fuscores <- x$sum_score[x$CreatedInSession == x$num_sessions]
    # x$changescores <- x$blscores - x$fuscores
    x <- merge(x, new_df, all.x = TRUE)
    return(x)
    # x$dropout <- as.factor(x$dropout)
  }
  
  
  # case_when(num_sessions*0.7 & x$treatment == "gad" ~ 1,
  # x$treatment != "gad" ~ NA,
  # TRUE ~ 0), # dropoutin kriteerin? 70 % hoitosessioista 
  
  
  
  ###
  # Apply function ----
  df_list <- list(dd, dg, dp, da, db, ds, do)
  res_list <- lapply(df_list, outcomes)
  names(res_list) <- c("dd", "dg", "dp", "da", "db", "ds", "do")
  list2env(res_list, envir = .GlobalEnv)
  rm(res_list)
  
  
  # dropout2 <- "NA" all others but dg and dd # SM 27.11.2023: NÄMÄ RIVIT PITÄISI AINAKIN POISTAA
  library(naniar) # needed package
  dp <- dp %>% replace_with_na(replace = list(dropout2 = c(1,0))) 
  da <- da %>% replace_with_na(replace = list(dropout2 = c(1,0)))
  db <- db %>% replace_with_na(replace = list(dropout2 = c(1,0)))
  ds <- ds %>% replace_with_na(replace = list(dropout2 = c(1,0)))
  do <- do %>% replace_with_na(replace = list(dropout2 = c(1,0)))
  
  
  
  ###
  # New data VT (merge) ----
  
  # SM 27.11.2023: TÄSSÄ KOHDIN TULEE YLIMÄÄRÄISIÄ RIVEJÄ, JOS TURHIA MUUTTUJIA EI POISTETA ENSIN?
  VT_list <- list(dd, dg, dp, da, db, ds, do)
  dVT <- Reduce(function(x,y) merge(x,y, all=T), VT_list)
  names(dVT)[names(dVT) == "Title"] <- "inventory"
  names(dVT)[names(dVT) == "Author"] <- "treatment_id"
  names(dVT)[names(dVT) == "num_sessions"] <- "last_session"
  dVT$inventory[dVT$inventory == "Mittari, Audit"] <- "Audit"
  dVT$inventory[dVT$inventory == "Mittari, BDI"] <- "BDI"
  dVT$inventory[dVT$inventory == "Mittari, SPIN-FIN"] <- "SPIN-FIN"
  dVT$inventory[dVT$inventory == "Mittari, PDSS"] <- "PDSS"
  dVT$inventory[dVT$inventory == "Mittari, GAD-7"] <- "GAD-7"
  dVT$inventory[dVT$inventory == "Mittari, OCD"] <- "OCI-R"
  # dVT <- dVT[, c(2,7,1,6,5,8:12)]
  dVT <- subset(dVT, select = c(treatment_id,treatment,inventory,session_max,
                                sum_score, last_session, dropout:cdateFU,
                                Created_x0020_Date))
  
  dVT <- dVT %>% # SM 27.11.2023: Tarkista, tarvitaanko tätä
    group_by(treatment_id) %>%
    summarise(treatment = paste(unique(treatment)),
              inventory = paste(unique(inventory)),
              session_max = paste(unique(session_max)),
              last_session = paste(unique(last_session)),
              dropout = paste(unique(dropout)),
              dropout2 = paste(unique(dropout2)),
              BL = paste(unique(blscores)),
              FU = paste(unique(fuscores)),
              change = paste(unique(changescores)),
              cdate = paste(unique(cdate)),
              cdateFU = paste(unique(cdateFU)))
  dVT <- dVT %>% replace_with_na_at(.vars = "dropout2",
                                    condition = ~.x == "NA")
  # dVT <- dVT %>%
  #   group_by(treatment_id) %>%
  #   mutate(FU = if(last_session == 1) {
  #     print(NA)
  #   } else {
  #     paste(FU)
  #   }) %>%
  #   mutate(change = if(last_session == 1) {
  #     print(NA)
  #   } else {
  #     paste(change)
  #   })

  dVT <- dVT %>%
    group_by(treatment_id) %>%
    mutate(FU = ifelse(last_session == 1, NA, FU)) %>%
    mutate(change = ifelse(last_session == 1, NA, change)) %>%
    mutate(cdateFU = ifelse(last_session == 1, as.character(NA), cdateFU))
  dVT$cdateFU <- as.Date(dVT$cdateFU)
  dVT$cdate <- as.Date(dVT$cdate)
  
  ###
  # Add FID from Potilaat data ----
  
  library(vroom)
  d_netti_mas <- vroom::vroom("W:/data/HUS/vanha_nettiterapia/masennus/FD_4810_masennus_Potilaat.csv")
  d_netti_gad <- vroom::vroom("W:/data/HUS/vanha_nettiterapia/gad/FD_4810_gad_Potilaat.csv")
  d_netti_ocd <- vroom::vroom("W:/data/Data_THL_4810_140200_2020_osa4/HUS/vanha_nettiterapia/ocd/FD_4810_ocd_Potilaat.csv")
  d_netti_alko <- vroom::vroom("W:/data/Data_THL_4810_140200_2020_osa4/HUS/vanha_nettiterapia/alko/FD_4810_alko_Potilaat.csv")
  d_netti_bipo <- vroom::vroom("W:/data/Data_THL_4810_140200_2020_osa4/HUS/vanha_nettiterapia/bipo/FD_4810_bipo_Potilaat.csv")
  d_netti_paniikki <- vroom::vroom("W:/data/Data_THL_4810_140200_2020_osa4/HUS/vanha_nettiterapia/paniikki/FD_4810_paniikki_Potilaat.csv")
  d_netti_sosfob <- vroom::vroom("W:/data/Data_THL_4810_140200_2020_osa4/HUS/vanha_nettiterapia/sosfob/FD_4810_sosfob_Potilaat.csv")
  
  P_list <- list(d_netti_mas, d_netti_gad, d_netti_ocd, d_netti_alko, d_netti_bipo, d_netti_paniikki, d_netti_sosfob)
  dP <- Reduce(function(x,y) merge(x,y, all=T), P_list)
  dP <- dP %>%
    select(FID, PatientUserName, TherapyStarted, TherapyCompleted, TherapyAborted, TherapyStatus)
  list_doubles <- list(adoubles, bdoubles,ddoubles, gdoubles, odoubles)
  dP <- dP[!dP$PatientUserName %in% list_doubles, ] # remove doubles as is done with dVT
  dP <- dP %>%
    dplyr::filter(PatientUserName %in% dVT$treatment_id) # filter data to dVT treatment_ids
  n_distinct(dP$PatientUserName) # 8966
  n_distinct(dP$FID) # 8607 
  
  # merge datas
  dVT <- merge(dVT, dP, by.x = "treatment_id", by.y = "PatientUserName",  all.x = T)
  # dVT <- dVT[, c(11,1:10)]
  dVT <- subset(dVT, select = treatment_id:FID)
  dVT <- distinct(dVT) # remove identical rows (3, tuplatallentuneita)
  
  
  
  ###
  # Add zBL and zFU variables ----
  utid <- unique(dVT$treatment) # unique therapies
  dVT <- cbind(dVT, zBL = rep(0, nrow(dVT)), zFU = rep(0, nrow(dVT))) # add columns
  dVT <- dVT %>%
    transform(BL = as.numeric(BL),
              FU = as.numeric(FU)) # BL and FU as.numeric
  for (i in 1:length(utid)){
    linds <- dVT$treatment == utid[i]
    blm <- mean(dVT$BL[linds], na.rm = T); blsd <- sd(dVT$BL[linds], na.rm = T)
    dVT$zBL[linds] <- (dVT$BL[linds] - blm)/blsd
    dVT$zFU[linds] <- (dVT$FU[linds] - blm)/blsd
  } 
  
  return(dVT)
  # d_old_iCBT <- dVT; save(d_old_iCBT, file = "old_iCBT_primary_measure_data.Rdata")
}

#### Insomnia iCBT ####

tidy_up_prebuilt_insomnia_iCBT <- function(d_iCBTI, patient_identification){
  utids <- unique(d_iCBTI$TherapyId)
  N <- length(utids)
  d_tidy <- data.frame(FID = rep("",N), 
                     treatment_id = rep("",N),
                     iCBT = rep("Insomnia",N),
                     inventory = rep("ISI",N),
                     inventory_name = rep("ISI - Insomnia Severity Index",N),
                     BL = as.numeric(rep(NA,N)),
                     FU = as.numeric(rep(NA,N)),
                     nitemsBL = as.numeric(rep(NA,N)),
                     nitemsFU = as.numeric(rep(NA,N)),
                     dropout = as.integer(rep(NA,N)),
                     last_session = as.integer(rep(NA,N)),
                     lastweek = as.numeric(rep(NA,N)),
                     zBL = as.numeric(rep(NA,N)),
                     zFU = as.numeric(rep(NA,N)))
  # Add dates and demographics
  d_tidy <- cbind(d_tidy, cdate = as.Date(rep(NA, nrow(d_tidy))), 
                  cdateFU = as.Date(rep(NA, nrow(d_tidy))),
                  age = as.numeric(rep(NA,nrow(d_tidy))), 
                  sex = as.character(rep(NA, nrow(d_tidy))),
                  municipality = as.character(rep(NA, nrow(d_tidy))))
  for(i in 1:N){
    d_tidy$treatment_id[i] <- utids[i]
    dtmp <- d_iCBTI[d_iCBTI$TherapyId == utids[i], ]
    phases <- sort(unique(dtmp$Phase[dtmp$iTitle == "Unettomuuden arvio"]))
    d_tidy$FID[i] <- patient_identification$FID[patient_identification$PatientId 
                                                == dtmp$PatientId[1]]
    d_tidy$sex[i] <- dtmp$Gender[1]
    d_tidy$municipality[i] <- dtmp$City[1]
    if (length(phases) > 0){
      tmpscores <- dtmp$Value[(dtmp$iTitle == "Unettomuuden arvio")&(dtmp$Phase == 1)]
      d_tidy$BL[i] <- sum(tmpscores, na.rm = T)
      d_tidy$nitemsBL[i] <- sum(!is.na(tmpscores))
      d_tidy$cdate[i] <- as.Date(min(dtmp$DateCreated[(dtmp$iTitle == "Unettomuuden arvio")&
                                                (dtmp$Phase == 1)], na.rm=T))
      if (length(phases) > 1){
        maxses <- max(phases, na.rm=T)
        tmpscores <- dtmp$Value[(dtmp$iTitle == "Unettomuuden arvio")&
                                  (dtmp$Phase == maxses)]
        d_tidy$FU[i] <- sum(tmpscores, na.rm = T)
        d_tidy$nitemsFU[i] <- sum(!is.na(tmpscores))
        d_tidy$dropout[i] <- ifelse(maxses < 5, 1, 0)
        d_tidy$last_session[i] <- maxses
        d_tidy$cdateFU[i] <- as.Date(min(dtmp$DateCreated[(dtmp$iTitle == "Unettomuuden arvio")&
                                                            (dtmp$Phase == maxses)], na.rm=T))
      } else {
        d_tidy$last_session[i] <- 1
      }
      d_tidy$age[i] <- lubridate::interval(dtmp$DateOfBirth[1], d_tidy$cdate[i])/
        lubridate::years(1)
      d_tidy$zBL <- (d_tidy$BL - mean(d_tidy$BL, na.rm = T)) / sd(d_tidy$BL, na.rm = T)
      d_tidy$zFU <- (d_tidy$FU - mean(d_tidy$BL, na.rm = T)) / sd(d_tidy$BL, na.rm = T)
    }
  }
  d_tidy$cdate[(!is.na(d_tidy$cdate))&(d_tidy$cdate == Inf)] <- NA
  d_tidy$cdateFU[(!is.na(d_tidy$cdateFU))&(d_tidy$cdateFU == Inf)] <- NA
  d_tidy$age[(!is.na(d_tidy$age))&(abs(d_tidy$age) == Inf)] <- NA
  return(d_tidy)
}

####### Some data from all therapy rgistries at once #########

build_basic_outcome_datas_in_Kapseli <- function(){
  #### new-HUS-iCBT patients and time of 1st and last session ####
  # load data
  ddir1 <- "W:/data/Data_THL_1303_140600_2023_osa1/Data_THL_1303_140600_2023_osa1/"
  ddir2 <- "W:/data/Data_THL_1303_140600_2023_osa2/Data_THL_1303_140600_2023_osa2/"
  d_patients <- vroom::vroom(paste0(ddir1, "HUS/FD_1303_nettiterapia_potilaat.csv"))
  d_participants <- vroom::vroom(paste0(ddir1, "HUS/FD_1303_nettiterapia_osallistujat.csv"))
  d_treatments <- vroom::vroom(paste0(ddir1, "HUS/FD_1303_nettiterapia_hoidot.csv"))
  d_iCBTs <- vroom::vroom(paste0(ddir1, "HUS/FD_1303_nettiterapia_nettiterapiat.csv"))
  d_taskexec <- vroom::vroom(paste0(ddir1, "HUS/FD_1303_nettiterapia_tehtavasuoritukset.csv"))
  d_answers <- vroom::vroom(paste0(ddir1, "HUS/FD_1303_nettiterapia_vastaukset.csv"))
  d_question_answers <- vroom::vroom(paste0(ddir2, "HUS/FD_1303_nettiterapia_kysymysten_vastaukset.csv"))
  d_questions <- vroom::vroom(paste0(ddir1, "HUS/FD_1303_nettiterapia_kysymykset.csv"))
  d_questionnaires <- vroom::vroom(paste0(ddir1, "HUS/FD_1303_nettiterapia_kyselyt.csv"))
  d_tasks <- vroom::vroom(paste0(ddir1, "HUS/FD_1303_nettiterapia_tehtavat.csv"))
  
  # make tidy data
  d_iCBT_new <- build_new_iCBT_data(d_patients, d_participants, d_treatments,
                                    d_iCBTs, d_questionnaires, d_answers,
                                    d_taskexec, d_tasks)
  # clean up
  rm(d_patients, d_participants, d_treatments, d_iCBTs, d_taskexec, d_answers,
     d_question_answers, d_questions, d_questionnaires, d_tasks)
  
  #### Finnish Psychotherapy Quality Registry 1st-therapy outcomes ####
  # load data
  d <- vroom::vroom("W:/data/HUS/bcb_psykoterapia_korjattuLaRe/FD_4810_bcb_form_data.csv", delim= ";") # "Data_THL_4810_140200_2020_osa4"    
  d_tre <- vroom::vroom("W:/data/HUS/bcb_psykoterapia_korjattuLaRe/FD_4810_bcb_treatment.csv", delim= ";") # treatment, patient, visit ja cohort kansiosta:
  d_pat <- vroom::vroom("W:/data/HUS/bcb_psykoterapia_korjattuLaRe/FD_4810_bcb_patient.csv", delim= ";")  #  "Data_THL_4810_140200_2020_osa3"
  d_vis <- vroom::vroom("W:/data/HUS/bcb_psykoterapia_korjattuLaRe/FD_4810_bcb_visit.csv", delim= ";")
  # make tidy data
  d_fpqr <- build_FPQR_data(d, d_tre, d_pat, d_vis, short_therapies_only = T)
  # add FIDs
  d_fpqr <- cbind(d_fpqr, FID = as.character(rep(NA, nrow(d_fpqr))))
  for (i in 1:nrow(d_fpqr)){
    if (any(d_pat$patient_id == d_fpqr$patient_id[i])){
      d_fpqr$FID[i] <- d_pat$FID[d_pat$patient_id == d_fpqr$patient_id[i]][1]
    }
  }
  # clean up
  rm(d, d_tre, d_pat, d_vis)
  
  #### Old-system iCBTs #####
  
  #### iCBT for Insomnia ####
  
  datadir <- "W:/data/HUS/unettomuuden_nettiterapia/"
  d_t <- vroom::vroom(paste0(datadir, "FD_4810_hf_uni_04_therapy.csv")) # Therapy data table
  d_tp <- vroom::vroom(paste0(datadir, "FD_4810_hf_uni_05_therapyphase.csv")) # TherapyPhase data table
  d_tpi <- vroom::vroom(paste0(datadir, "FD_4810_hf_uni_06_therapyphaseinquiry.csv")) # TherapyPhaseInquiry data table
  d_tpiq <- vroom::vroom(paste0(datadir, "FD_4810_hf_uni_07_therapyphaseinquiryquestion.csv")) # TherapyPhaseInquiryQuestion data table
  d_tptq <- vroom::vroom(paste0(datadir, "FD_4810_hf_uni_09_therapyphasetaskquestion.csv")) # TherapyPhaseTaskQuestion data table
  d_patient <- vroom::vroom(paste0(datadir, "FD_4810_hf_uni_02_patient.csv")) # Patient data table
  
  d_iCBTI <- psyteamtools::build_insomnia_iCBT(
    d_patient = d_patient,
    d_t = d_t,
    d_tp = d_tp,
    d_tpi = d_tpi,
    d_tpiq = d_tpiq
  )
  
  patient_identification <- vroom::vroom(
    paste0(datadir, "FD_4810_hf_uni_03_patientidentification.csv")) # FIDs
  d_iCBTI <- tidy_up_prebuilt_insomnia_iCBT(d_iCBTI, patient_identification)
  # save(d_iCBTI, file = "insomnia_iCBT_data.Rdata")
  
  #### Return user data #####
  return(list(d_iCBT_new = d_iCBT_new, d_iCBT_old = d_iCBT_old, 
              d_iCBTI = d_iCBTI, d_fpqr = d_fpqr))
}
