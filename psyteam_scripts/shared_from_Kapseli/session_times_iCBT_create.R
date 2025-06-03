# Yhdistä tähän all_ses_times2_create ja icbt_drop_out_create olennaiset datanrakennusosat
# Tarkista, onko kaikki primaarioiremittarit saatu grepillä, voiko onko parempi käyttää mittarin id:tä
# Tarkista summapisteet: tuleeko EDEQ sama summapiste?
# Pyri tekemään funktioksi

# Uudet oirepistemuuttujat
# Session 1 primary measurement
# Viimeinen havaittu primary measurement
# Primary measurement, kun hoidosta käyty 70 % (lisäksi muuttuja, joka indikoi, mistä sessiosta tämä on)
# Ota tästä joku sessio, joka lähellä 70 % mutta ei ainakaan alle 60 %

# drop_out_100
# drop_out_70

# Lisää muuttuja, joka indikoi, mikä on hoito-ohjelman viimeinen sessio



# Function for extracting data from old iCBT treatments
# Use data from symptom measurement OR tasks (depending which information a session has)
get_old_data <- function(treatment, treatment_id, df_name) {
  fp1 <- paste0("W:/data/osadata5/HUS/vanha_nettiterapia/", treatment, "/FD_4810_", treatment, "_Terapiatehtävät.csv")
  d1 <- read.csv(fp1, sep = ";", encoding = "UTF-8")
  
  fp2 <- paste0("W:/data/HUS/vanha_nettiterapia/", treatment, "/FD_4810_", treatment, "_Mittarit.csv")
  d2 <- read.csv(fp2, sep = ";", encoding = "UTF-8")
  
  fp3 <- paste0("W:/data/HUS/vanha_nettiterapia/", treatment, "/FD_4810_", treatment, "_Potilaat.csv")
  d3 <- read.csv(fp3, sep = ";", encoding = "UTF-8")
  
  # Drop rows that include diary (i.e. between-sessions) data
  d1 <- d1[!grepl("päiväkirja", d1$Title), ]
  
  # Rename primary measurement sum score
  if (treatment == "alko") {
    names(d2)[names(d2) == "Audit11"] <- "primary_sum"
  } else if (treatment == "bipo") {
    names(d2)[names(d2) == "BDI22"] <- "primary_sum"
  } else if (treatment == "gad") {
    names(d2)[names(d2) == "X_x0047_AD7"] <- "primary_sum"
  } else if (treatment == "masennus") {
    names(d2)[names(d2) == "BDI22"] <- "primary_sum"
  } else if (treatment == "ocd") {
    names(d2)[names(d2) == "OCD19"] <- "primary_sum"
  } else if (treatment == "paniikki") {
    names(d2)[names(d2) == "PDSS08"] <- "primary_sum"
  } else if (treatment == "sosfob") {
    names(d2)[names(d2) == "SPINFIN18"] <- "primary_sum"
  }
  
  d1$primary_sum <- rep(NA, nrow(d1))
  
  # Merge and add FID
  d <- rbind(d2[, c("Author", "Title", "Created", "CreatedInSession", "primary_sum")],
             d1[, c("Author", "Title", "Created", "CreatedInSession", "primary_sum")])
  
  d <- merge(d, d3[, c("FID", "PatientUserName")], by.x = "Author", by.y = "PatientUserName", all.x = TRUE)
  
  # Add treatment indicator column
  d$treatment <- rep(treatment_id, nrow(d))
  
  assign(df_name, d, envir = .GlobalEnv)
  rm(d, d1, d2, d3)
}


# Function for extracting data from new iCBT treatments
get_new_data <- function(x) {
  library(dplyr)
  
  # Open relevant datas
  new_hoidot <- read.csv("W:/data/Data_THL_1303_140600_2023_osa1/Data_THL_1303_140600_2023_osa1/HUS/FD_1303_nettiterapia_hoidot.csv",
                         sep = ";", encoding = "UTF-8")
  new_ter <- read.csv("W:/data/Data_THL_1303_140600_2023_osa1/Data_THL_1303_140600_2023_osa1/HUS/FD_1303_nettiterapia_nettiterapiat.csv",
                      sep = ";", encoding = "UTF-8")
  new_teht <- read.csv("W:/data/Data_THL_1303_140600_2023_osa1/Data_THL_1303_140600_2023_osa1/HUS/FD_1303_nettiterapia_tehtavat.csv",
                       sep = ";", encoding = "UTF-8")
  new_suor <- read.csv("W:/data/Data_THL_1303_140600_2023_osa1/Data_THL_1303_140600_2023_osa1/HUS/FD_1303_nettiterapia_tehtavasuoritukset.csv",
                       sep = ";", encoding = "UTF-8")
  new_vast <- read.csv("W:/data/Data_THL_1303_140600_2023_osa1/Data_THL_1303_140600_2023_osa1/HUS/FD_1303_nettiterapia_vastaukset.csv",
                       sep = ";", encoding = "UTF-8")
  new_kys <- read.csv("W:/data/Data_THL_1303_140600_2023_osa1/Data_THL_1303_140600_2023_osa1/HUS/FD_1303_nettiterapia_kyselyt.csv",
                      sep = ";", encoding = "UTF-8")
  new_kys_vast <- read.csv("W:/data/Data_THL_1303_140600_2023_osa2/Data_THL_1303_140600_2023_osa2/HUS/FD_1303_nettiterapia_kysymysten_vastaukset.csv",
                      sep = ";", encoding = "UTF-8")
  
  # Combine
  new_data <- merge(new_vast[, c("id", "kyselyn_id", "vastausviikko", "vastauspaiva")],
                    new_kys, by.x = "kyselyn_id", by.y = "id", all.x = TRUE)
  names(new_data)[names(new_data) == "id"] <- "id_vastaukset"
  new_data <- merge(new_kys_vast[, c("vastauksen_id", "kysymyksen_id", "pisteet")],
                    new_data, by.x = "vastauksen_id", by.y = "id_vastaukset", all.y = TRUE)
  new_data <- merge(new_data, new_suor[, c("hoidon_id", "liittyvan_vastauksen_id", "suorituspaiva", "tehtavan_id")],
                    by.x = "vastauksen_id", by.y = "liittyvan_vastauksen_id", all.x = TRUE)
  new_data <- merge(new_data, new_teht[, c("tehtavan_id", "istunto")], by = "tehtavan_id", all.x = TRUE)
  new_data <- merge(new_data, new_hoidot[, c("FID", "id", "hoitopolun_id")],
                    by.x = "hoidon_id", by.y = "id", all.x = TRUE)
  new_data <- merge(new_data, new_ter, by.x = "hoitopolun_id", by.y = "id", all.x = TRUE)
  
  # use Tom's function to calculate EDE-Q scores
  # From data_build_functions_v2.R
  form_EDEQ_scale <- function(scores, qids){
    ntot <- 0
    # sub-scale 1 (syomisen rajoittaminen, 5 items: 1-5)
    lids <- qids %in% c(14034, 14033, 14032, 14031, 14030)
    nused <- ifelse(sum(lids, na.rm = T)>=3, sum(lids, na.rm = T), 0)
    ntot <- nused + ntot
    ss1 <- ifelse(sum(lids, na.rm = T)>=3, sum(scores[lids]/nused, na.rm = T)/ntot, NA) # Miksi tässä jaetaan ntotilla?
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
    return((ss1+ss2+ss3+ss4)/sum(!is.na(c(ss1,ss2,ss3,ss4))))
  }
  
  # Calculate EDE-Q sum scores
  new_data <- new_data %>%
    group_by(vastauksen_id) %>%
    mutate(sum_score = form_EDEQ_scale(scores = pisteet, qids = kysymyksen_id)) %>%
    ungroup()
  
  # Calculate sum scores for other relevant measurements
  # Note, these are not yet specified primary measurements for each treatment
  scales <- c("audit|phq-9|spin|oci-r|pdss-sr|bipq|gad-7")
  
  new_data <- new_data %>%
    group_by(vastauksen_id) %>%
    mutate(sum_score = ifelse(grepl(scales, kysely, ignore.case = TRUE), sum(pisteet, na.rm = TRUE), sum_score)) %>%
    distinct(vastauksen_id, .keep_all = TRUE) %>%
    ungroup()
  
  # Clean rows with empty value in "vastauspaiva" variable
  new_data <- new_data %>%
    mutate(vastauspaiva = (case_when(is.na(vastauspaiva) & suorituspaiva != "" ~ suorituspaiva,
                                     TRUE ~  vastauspaiva)))
  
  new_data <- new_data[!new_data$vastauspaiva == "", ]
  new_data <- new_data[!is.na(new_data$istunto), ]
  
  # Transform to date
  new_data$vastauspaiva <- lubridate::ymd(new_data$vastauspaiva)
  
  # Keep only the necessary columns
  new_data <- new_data[, c("hoidon_id", "kysely", "vastauspaiva", "istunto", "sum_score", "FID", "nettiterapia")]
  
  return(new_data)
}


####
# Function for building session time data
build_session_times <- function(x) {
  library(dplyr)
  
  ##
  # Gather old iCBT datas
  treatments <- c("alko", "bipo", "gad", "masennus", "ocd", "paniikki", "sosfob")
  treatment_ids <- c("alc", "bip", "gad", "dep", "ocd", "pan", "soc")
  df_names <- c("da", "db", "dg", "dd", "do", "dp", "ds")
  
  # Loop over treatments for combined time data
  for(i in 1:length(treatments)) {
    get_old_data(treatments[i], treatment_ids[i], df_names[i])
  }
  
  old_ses_times <- do.call("rbind", list(da, db, dd, dg, do, dp, ds))
  #rm(da, db, dd, dg, do, dp, ds)
  
  # Transform "Created" variable to date
  old_ses_times$Created <- lubridate::dmy_hms(old_ses_times$Created)
  old_ses_times$Created <- lubridate::date(old_ses_times$Created)
  
  
  ##
  # Gather new iCBT datas
  new_ses_times <- get_new_data()
  
  
  ##
  # Create initial session time variables
  # These are done for old and new treatments separately
  
  # Old
  old_ses_times <- old_ses_times %>%
    group_by(Author, CreatedInSession) %>%
    mutate(prim_mes = case_when(treatment == "alc" & Title == "Mittari, Audit" ~ TRUE,
                                treatment == "bip" & Title == "Mittari, BDI" ~ TRUE,
                                treatment == "gad" & Title == "Mittari, GAD-7" ~ TRUE,
                                treatment == "dep" & Title == "Mittari, BDI" ~ TRUE,
                                treatment == "ocd" & Title == "Mittari, OCD" ~ TRUE,
                                treatment == "pan" & Title == "Mittari, PDSS" ~ TRUE,
                                treatment == "soc" & Title == "Mittari, SPIN-FIN" ~ TRUE,
                                TRUE ~ FALSE)) %>%
    ungroup()
  
  # Session evaluation times
  old_eval_times <- old_ses_times[old_ses_times$Title == "Istunnon arviointi", ]
  names(old_eval_times)[names(old_eval_times) == "Created"] <- "end_eval_time"
  
  # Primary measurement times
  old_prim_times <- old_ses_times[old_ses_times$prim_mes, ]
  names(old_prim_times)[names(old_prim_times) == "Created"] <- "primary_measure_time"
  
  # Earliest times from task or primary measurement
  old_early_times <- old_ses_times %>%
    group_by(Author, CreatedInSession) %>% 
    slice_min(order_by = Created) %>%
    ungroup()
  
  old_early_times <- old_early_times %>%
    distinct(Author, CreatedInSession, .keep_all = TRUE)
  
  names(old_early_times)[names(old_early_times) == "Created"] <- "earliest_time"
  
  # Drop "primary_sum", "Title" and "prim_mes" columns so that the different names match the same session
  # keep titles and sum scores to restore later
  old_sums <- old_prim_times[, c("Author", "CreatedInSession", "Title", "primary_sum")]
  old_early_times <- old_early_times[, !(names(old_early_times) %in% c("Title", "prim_mes", "primary_sum"))]
  old_eval_times <- old_eval_times[, !(names(old_eval_times) %in% c("Title", "prim_mes", "primary_sum"))]
  old_prim_times <- old_prim_times[, !(names(old_prim_times) %in% c("Title", "prim_mes", "primary_sum"))]
  
  # Merge datas
  old_data <- merge(old_early_times, old_eval_times, all = TRUE)
  old_data <- merge(old_data, old_prim_times, all = TRUE)
  old_data <- merge(old_data, old_sums, by = c("Author", "CreatedInSession"), all.x = TRUE)
  
  
  # New
  # Primary symptom measurements
  new_ses_times <- new_ses_times %>%
    group_by(hoidon_id, istunto) %>%
    mutate(prim_mes = case_when(nettiterapia == "Alkoholin liikakäytön nettiterapia" & grepl("audit", kysely, ignore.case = TRUE) ~ TRUE,
                                nettiterapia == "Bulimian nettiterapia" & grepl("ede-q", kysely, ignore.case = TRUE) ~ TRUE,
                                nettiterapia == "Kaksisuuntaisen mielialahäiriön nettiterapia" & grepl("phq-9", kysely, ignore.case = TRUE) ~ TRUE,
                                nettiterapia == "Masennuksen nettiterapia" & grepl("phq-9", kysely, ignore.case = TRUE) ~ TRUE,
                                nettiterapia == "Nuorten sosiaalisen ahdistu&shy:neisuuden nettiterapia" & grepl("spin", kysely, ignore.case = TRUE) ~ TRUE,
                                nettiterapia == "Pakko-oireisen häiriön nettiterapia" & grepl("oci-r", kysely, ignore.case = TRUE) ~ TRUE,
                                nettiterapia == "Sosiaalisten tilanteiden pelon nettiterapia" & grepl("spin", kysely, ignore.case = TRUE) ~ TRUE,
                                nettiterapia == "Yleistyneen ahdistuneisuushäiriön nettiterapia" & grepl("gad-7", kysely, ignore.case = TRUE) ~ TRUE,
                                nettiterapia == "Paniikkihäiriön nettiterapia" & grepl("pdss-sr", kysely, ignore.case = TRUE) ~ TRUE,
                                grepl("kehollisten oireiden kuntoutusohjelma", nettiterapia) & grepl("bipq", kysely, ignore.case = TRUE) ~ TRUE,
                                TRUE ~ FALSE)) %>%
    ungroup()
  
  # Primary sum score variable
  new_ses_times <- new_ses_times %>%
    mutate(primary_sum = case_when(prim_mes == TRUE ~ sum_score,
                                   TRUE ~ NA_real_))
  
  # Session evaluation times
  new_eval_times <- new_ses_times[new_ses_times$kysely == "Istunnon arviointi", ]
  names(new_eval_times)[names(new_eval_times) == "vastauspaiva"] <- "end_eval_time"
  new_eval_times <- new_eval_times[rowSums(is.na(new_eval_times)) != ncol(new_eval_times), ]
  
  # Primary measurement times
  new_prim_times <- new_ses_times[new_ses_times$prim_mes, ]
  names(new_prim_times)[names(new_prim_times) == "vastauspaiva"] <- "primary_measure_time"
  
  # Earliest times from task or primary measurement
  new_early_times <- new_ses_times %>%
    group_by(hoidon_id, istunto) %>% 
    slice_min(order_by = vastauspaiva) %>%
    ungroup()
  
  new_early_times <- new_early_times %>%
    distinct(hoidon_id, istunto, .keep_all = TRUE)
  
  names(new_early_times)[names(new_early_times) == "vastauspaiva"] <- "earliest_time"
  
  # Drop "Title" and "prim_mes" columns so that the different names match the same session
  # keep titles and sum scores to retain later
  new_sums <- new_prim_times[, c("hoidon_id", "istunto", "kysely", "primary_sum")]
  new_early_times <- new_early_times[, !(names(new_early_times) %in% c("kysely", "sum_score", "primary_sum", "prim_mes"))]
  new_prim_times <- new_prim_times[, !(names(new_prim_times) %in% c("kysely", "sum_score", "primary_sum", "prim_mes"))]
  new_eval_times <- new_eval_times[, !(names(new_eval_times) %in% c("kysely", "sum_score", "primary_sum", "prim_mes"))]
  
  # Merge datas
  new_data <- merge(new_early_times, new_eval_times, all = TRUE)
  new_data <- merge(new_data, new_prim_times, all = TRUE)
  new_data <- merge(new_data, new_sums, by = c("hoidon_id", "istunto"), all.x = TRUE)
  
  
  ##
  # Harmonize and combine old and new data
  names(old_data)[names(old_data) == "Author"] <- "treatment_id"
  names(new_data)[names(new_data) == "hoidon_id"] <- "treatment_id"
  
  names(old_data)[names(old_data) == "CreatedInSession"] <- "session"
  names(new_data)[names(new_data) == "istunto"] <- "session"
  
  names(old_data)[names(old_data) == "Title"] <- "questionnaire"
  names(new_data)[names(new_data) == "kysely"] <- "questionnaire"
  
  names(new_data)[names(new_data) == "nettiterapia"] <- "treatment"
  
  # New iCBT treatment names
  new_data <- new_data %>%
    mutate(treatment = case_when(treatment == "Alkoholin liikakäytön nettiterapia" ~ "alc",
                                 treatment == "Bulimian nettiterapia" ~ "bul",
                                 treatment == "Kaksisuuntaisen mielialahäiriön nettiterapia" ~ "bip",
                                 treatment == "Masennuksen nettiterapia" ~ "dep",
                                 treatment == "Nuorten sosiaalisen ahdistu&shy:neisuuden nettiterapia" ~ "y-soc",
                                 treatment == "Pakko-oireisen häiriön nettiterapia" ~ "ocd",
                                 treatment == "Sosiaalisten tilanteiden pelon nettiterapia" ~ "soc",
                                 treatment == "Yleistyneen ahdistuneisuushäiriön nettiterapia" ~ "gad",
                                 treatment == "Paniikkihäiriön nettiterapia" ~ "pan",
                                 treatment == "pitkäaikaisten ja haittaavien kehollisten oireiden kuntoutusohjelma" ~ "pit_old",
                                 treatment == "PITKO – Pitkäaikaisten ja haittaavien kehollisten oireiden kuntoutusohjelma" ~ "pit_new",
                                 TRUE ~ treatment))
  
  # Add registry variable for old vs. new platform treatments
  old_data$registry <- rep("iCBT_old", nrow(old_data))
  new_data$registry <- rep("iCBT_new", nrow(new_data))
  
  # Rearrange so that variable order matches # TARKISTA!!!
  old_data <- old_data[, c("FID", "treatment_id", "registry", "treatment", "session", "earliest_time",
                           "primary_measure_time", "end_eval_time", "primary_sum", "questionnaire")]
  new_data <- new_data[, c("FID", "treatment_id", "registry", "treatment", "session", "earliest_time",
                           "primary_measure_time", "end_eval_time", "primary_sum", "questionnaire")]
  
  # Merge datasets
  all_data <- rbind(old_data, new_data)
  
  # Someone has session numbered 0 -> remove
  all_data <- all_data[all_data$session != 0, ]
  
  # Remove multiplicated rows
  all_data <- all_data %>% distinct()
  
  
  ##
  # Create session time variable
  # Primary measurement time if available
  # If not, then earliest task time
  # If none of the above available, use session evaluation (end of session)
  all_data$session_time <- rep(lubridate::ymd("1999-01-01"), nrow(all_data))
  all_data$session_time_from_primary <- rep(FALSE, nrow(all_data))
  
  for (i in 1:nrow(all_data)) {
    if (!is.na(all_data$primary_measure_time[i])) {
      all_data$session_time[i] <- all_data$primary_measure_time[i]
      all_data$session_time_from_primary[i] <- TRUE
    } else if (!is.na(all_data$earliest_time[i])) {
      all_data$session_time[i] <- all_data$earliest_time[i]
    } else if (!is.na(all_data$end_eval_time[i])) {
      all_data$session_time[i] <- all_data$end_eval_time[i]
    }
  }
  
  
  ##
  # There are some patients with multiple sessions under same date in "session_time"
  # Create a variable that corrects most of these
  all_data$session_time_corrected <- all_data$session_time
  all_ids <- unique(all_data$treatment_id)
  
  # MUUTA TÄHÄN MUUTTUJANIMET!!!
  all_data$tupla <- duplicated(all_data[c("treatment_id","session_time")]) | duplicated(all_data[c("treatment_id","session_time")], fromLast = TRUE)
  dup_ids <- unique(all_data$treatment_id[all_data$tupla])
  
  # Replace duplicated date with non-duplicated if some of the other date variables has one
  for (i in 1:length(all_ids)) {
    if (all_ids[i] %in% dup_ids) {
      temp <- all_data[all_data$treatment_id == all_ids[i], ]
      for (j in 1:nrow(temp)) {
        if (temp$tupla[j] == TRUE) {
          if ((!is.na(temp$primary_measure_time[j])) & temp$primary_measure_time[j] != temp$session_time[j]) {
            temp$session_time_corrected[j] <- temp$primary_measure_time[j]
          } else if ((!is.na(temp$earliest_time[j])) & temp$earliest_time[j] != temp$session_time[j]) {
            temp$session_time_corrected[j] <- temp$earliest_time[j]
          } else if ((!is.na(temp$end_eval_time[j])) & temp$end_eval_time[j] != temp$session_time[j]) {
            temp$session_time_corrected[j] <- temp$end_eval_time[j]
          }
        }
        all_data$session_time_corrected[(all_data$treatment_id == temp$treatment_id[j] & all_data$session == temp$session[j])] <- temp$session_time_corrected[j]
      }
    }
  }
  
  
  ## Return
  return(all_data)
}


# Function for building drop out and symptom score data
build_drop_our_data(df = NULL) {
  # Check if session time data is given as an argument
  # if not, build
  # Mieti tähän vielä järkevämpää ratkaisua! Esim. erilleen tästä joku main-funktio, jossa suoritetaan edellinen ja tämä
  if (is.null(df)) {
    df <- build_session_times()
  }
  
}





















