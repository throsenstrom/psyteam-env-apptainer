setwd("W:/Sanna")

library(lubridate)
library(stringr)
library(tidyr)
library(dplyr) # Aja tämä viimeisenä



#### OLD ####

# Function to extracting dates and sessions from different treatments
# Use data from symptom measurement OR tasks (depending which information a session has)
get_old_times <- function(treatment, treatment_id, df_name) {
  fp1 <- paste0("W:/data/osadata5/HUS/vanha_nettiterapia/", treatment, "/FD_4810_", treatment, "_Terapiatehtävät.csv")
  d1 <- read.csv(fp1, sep = ";", encoding = "UTF-8")
  
  fp2 <- paste0("W:/data/HUS/vanha_nettiterapia/", treatment, "/FD_4810_", treatment, "_Mittarit.csv")
  d2 <- read.csv(fp2, sep = ";", encoding = "UTF-8")
  
  fp3 <- paste0("W:/data/HUS/vanha_nettiterapia/", treatment, "/FD_4810_", treatment, "_Potilaat.csv")
  d3 <- read.csv(fp3, sep = ";", encoding = "UTF-8")
  
  # Drop rows that include diary (i.e. between-sessions) data
  d1 <- d1[!grepl("päiväkirja", d1$Title), ]
  
  # Merge and add FID
  d <- rbind(d2[, c("Author", "Title", "Created", "CreatedInSession")],
             d1[, c("Author", "Title", "Created", "CreatedInSession")])
  
  d <- merge(d, d3[, c("FID", "PatientUserName")], by.x = "Author", by.y = "PatientUserName", all.x = TRUE)
  
  # Add treatment indicator column
  d$treatment <- rep(treatment_id, nrow(d))
  
  assign(df_name, d, envir = .GlobalEnv)
  rm(d, d1, d2, d3)
}



# old_sessionLog <- read.csv("W:/data/HUS/vanha_nettiterapia/masennus/FD_4810_masennus_SessionLog.csv",
#                            sep = ";", encoding = "UTF-8")
# 
# old_teht <- read.csv("W:/data/osadata5/HUS/vanha_nettiterapia/masennus/FD_4810_masennus_Terapiatehtävät.csv",
#                      sep = ";", encoding = "UTF-8")
# 
# old_sympt <- read.csv("W:/data/HUS/vanha_nettiterapia/masennus/FD_4810_masennus_Mittarit.csv",
#                       sep = ";", encoding = "UTF-8")
# 
# old_pot <- read.csv("W:/data/HUS/vanha_nettiterapia/masennus/FD_4810_masennus_Potilaat.csv",
#                     sep = ";", encoding = "UTF-8")

# old_tapahtumat <- read.csv("W:/data/HUS/vanha_nettiterapia/masennus/FD_4810_masennus_Tapahtumat.csv",
#                            sep = ";", encoding = "UTF-8")
# rm(old_tapahtumat)


# sesLog_na <- old_sessionLog[old_sessionLog$SessionStart == "", ]
# table(sesLog_na$SessionNo)
# sesLog_na <- old_sessionLog[old_sessionLog$SessionEnd == "", ]
# table(sesLog_na$SessionNo)


# sessionLog sis. mm. kentät SessionStart, SessionEnd ja Created
# Näistä SessionStart sis. jonkin verran puuttuvia arvoja
# Suurin osa yo. puuttuvista liittyy sessioon 1
# Tällöin Created on ilmeisesti terapian luontipäivä (koska aikasempi kuin ses 1 lopetusaika)
# Jos SessionStart muussa kuin sessiossa 1 tyhjä, Created vastaa lopetusaikaa
# SessionEnd sis. myös tyhjiä 
# HUOM että tyhjät eivät aina ole potilaan vimeisessä sessiossa (eli ei johdu drop outista kesken session)
# Created ei sis. puuttuvia arvoja
# Created on session alkuhetki pl. havainnot, joilta alkuhetki puuttuu


treatments <- c("alko", "bipo", "gad", "masennus", "ocd", "paniikki", "sosfob")
treatment_ids <- c("alc", "bip", "gad", "dep", "ocd", "pan", "soc")
df_names <- c("da", "db", "dg", "dd", "do", "dp", "ds")

# Loop over treatments for combined time data
for(i in 1:length(treatments)) {
  get_old_times(treatments[i], treatment_ids[i], df_names[i])
}

old_ses_times <- do.call("rbind", list(da, db, dd, dg, do, dp, ds))

# Check
table(old_ses_times$treatment, useNA = "always")
length(unique(old_ses_times$FID)) # 8776
length(unique(old_ses_times$Author)) # 9147

rm(da, db, dd, dg, do, dp, ds)
gc()


# Transform to dates
sum(is.na(old_ses_times$Created))
length(old_ses_times$Created[old_ses_times$Created == ""])
old_ses_times$Created <- dmy_hms(old_ses_times$Created)
old_ses_times$Created <- date(old_ses_times$Created)

# If there is primary symptom measurement data, take date from that
# Otherwise keep only the earliest date for each session
# Group by Author (=PotilasID) -> that should be treatment specific


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

# Drop "Title" and "prim_mes" columns so that the different names match the same session
old_early_times <- old_early_times[, !(names(old_early_times) %in% c("Title", "prim_mes"))]
old_prim_times <- old_prim_times[, !(names(old_prim_times) %in% c("Title", "prim_mes"))]
old_eval_times <- old_eval_times[, !(names(old_eval_times) %in% c("Title", "prim_mes"))]

# Merge datas
old_ses_times2 <- merge(old_early_times, old_eval_times, all = TRUE)
old_ses_times2 <- merge(old_ses_times2, old_prim_times, all = TRUE)

# Add ses_time variable
# Primary measurement time if available
# If not, then earliest task time
# If none of the above available, use session evaluation (end of session)
old_ses_times2$ses_time <- rep(ymd("1999-01-01"), nrow(old_ses_times2))
old_ses_times2$ses_time_from_primary <- rep(FALSE, nrow(old_ses_times2))

for (i in 1:nrow(old_ses_times2)) {
  if (!is.na(old_ses_times2$primary_measure_time[i])) {
    old_ses_times2$ses_time[i] <- old_ses_times2$primary_measure_time[i]
    old_ses_times2$ses_time_from_primary[i] <- TRUE
  } else if (!is.na(old_ses_times2$earliest_time[i])) {
    old_ses_times2$ses_time[i] <- old_ses_times2$earliest_time[i]
  } else if (!is.na(old_ses_times2$end_eval_time[i])) {
    old_ses_times2$ses_time[i] <- old_ses_times2$end_eval_time[i]
  }
}

min(old_ses_times2$ses_time) # "2013-05-03"

length(unique(old_ses_times2$FID)) # 8776
length(unique(old_ses_times2$Author)) # 9147


rm(old_ses_times, old_early_times, old_eval_times, old_prim_times)
gc()

#### UUSI ####

new_hoidot <- read.csv("W:/data/Data_THL_1303_140600_2023_osa1/Data_THL_1303_140600_2023_osa1/HUS/FD_1303_nettiterapia_hoidot.csv",
                       sep = ";", encoding = "UTF-8")

new_ter <- read.csv("W:/data/Data_THL_1303_140600_2023_osa1/Data_THL_1303_140600_2023_osa1/HUS/FD_1303_nettiterapia_nettiterapiat.csv",
                    sep = ";", encoding = "UTF-8")

# new_siskirj <- read.csv("W:/data/Data_THL_1303_140600_2023_osa1/Data_THL_1303_140600_2023_osa1/HUS/FD_1303_nettiterapia_sisaankirjautumiset.csv",
#                        sep = ";", encoding = "UTF-8")

new_teht <- read.csv("W:/data/Data_THL_1303_140600_2023_osa1/Data_THL_1303_140600_2023_osa1/HUS/FD_1303_nettiterapia_tehtavat.csv",
                     sep = ";", encoding = "UTF-8")
new_suor <- read.csv("W:/data/Data_THL_1303_140600_2023_osa1/Data_THL_1303_140600_2023_osa1/HUS/FD_1303_nettiterapia_tehtavasuoritukset.csv",
                     sep = ";", encoding = "UTF-8")
# new_suor <- new_suor[1:500, ]

new_vast <- read.csv("W:/data/Data_THL_1303_140600_2023_osa1/Data_THL_1303_140600_2023_osa1/HUS/FD_1303_nettiterapia_vastaukset.csv",
                     sep = ";", encoding = "UTF-8")
#new_vast <- new_vast[1:500, ]

new_kys <- read.csv("W:/data/Data_THL_1303_140600_2023_osa1/Data_THL_1303_140600_2023_osa1/HUS/FD_1303_nettiterapia_kyselyt.csv",
                    sep = ";", encoding = "UTF-8")

new_hoidot <- merge(new_hoidot, new_ter, by.x = "hoitopolun_id", by.y = "id", all.x = TRUE)




sum(is.na(new_vast$vastauspaiva))
length(new_vast$vastauspaiva[new_vast$vastauspaiva == ""])
table(new_vast$vastausviikko)

length(new_suor$suorituspaiva[new_suor$suorituspaiva == ""]) # Tässä paljon tyhjiä

# Tehtavat: mihin sessioon mikin tehtävä liittyy
# tehtavasuoritukset: milloin tehtävä on suoritettu
# Huom! yo. kuitenkin paljon puuttuvia arvoja suorituspäivämäärässä!!!
# Vastaukset näyttäisi sisältävän vastauspäivä-muuttujan, jossa ei puuttuvia arvoja
# Lisäksi vastausviikko-muuttuja, joka ei kuitenkaan vastaa sessiota (saa arvoja väliltä -30;98)

names(new_suor) # Täällä FID (tämä tarvitaan tehtävin ja vastausten yhdistämiseen)
# Niille, joille ensimmäisen session vastauspäivä puuttuu, ajat suoritusajoista???
names(new_teht) # Täällä istunto
names(new_vast) # Täällä FID ja vastauspaiva

# table(new_kys$kysely)
# Päämittarit:
# Masennus, PHQ-9; GAD, GAD-7; Paniikki, PDSS-SR; sofo, SPIN; OCD, OCI-R
# HUOMAA, että pl. GAD, oireita ei mitattu joka sessiossa
# jos siis haluaa sessio-per-sessio-ajastuksen, pelkät oiremittarit ei toimi

new_vast <- merge(new_vast, new_kys, by.x = "kyselyn_id", by.y = "id")

new_ses_times <- merge(new_vast[, c("FID", "vastauspaiva", "id", "kysely")],
                       new_suor[, c("FID", "suorituspaiva", "liittyvan_vastauksen_id", "tehtavan_id", "hoidon_id")],
                       by.x = c("FID", "id"), by.y = c("FID", "liittyvan_vastauksen_id"),
                       all = TRUE)
new_ses_times <- merge(new_ses_times, new_teht[, c("istunto", "otsikko", "tehtavan_id")], 
                       by = "tehtavan_id", all.x = TRUE)

new_ses_times <- merge(new_ses_times, new_hoidot[, c("FID", "id", "nettiterapia")],
                       by.x = c("FID", "hoidon_id"), by.y = c("FID", "id"), all.x = TRUE)

rm(new_suor, new_teht, new_vast, new_hoidot, new_ter, new_kys)
gc()


# length(unique(new_ses_times$FID))
# length(unique(new_vast$FID))


sum(is.na(new_ses_times$vastauspaiva))
length(new_ses_times$vastauspaiva[new_ses_times$vastauspaiva == ""])

new_ses_times <- new_ses_times %>%
  mutate(vastauspaiva = (case_when(is.na(vastauspaiva) & suorituspaiva != "" ~ suorituspaiva,
                                   TRUE ~  vastauspaiva)))

new_ses_times <- new_ses_times[!new_ses_times$vastauspaiva == "", ]
new_ses_times <- new_ses_times[!is.na(new_ses_times$istunto), ]

#new_ses_times <- new_ses_times[, c("FID", "vastauspaiva", "istunto", "otsikko")]
new_ses_times$vastauspaiva <- ymd(new_ses_times$vastauspaiva)
new_ses_times$suorituspaiva <- ymd(new_ses_times$suorituspaiva)


# Detect primary symptom measurements
sort(unique(new_ses_times$kysely))
sort(unique(new_ses_times$kysely[grepl("kehollisten oireiden kuntoutusohjelma", new_ses_times$nettiterapia)]))

new_ses_times <- new_ses_times %>%
  group_by(hoidon_id, istunto) %>%
  mutate(prim_mes = case_when(nettiterapia == "Alkoholin liikakäytön nettiterapia" & grepl("AUDIT", kysely) ~ TRUE, # HUOM AUDIT-C VÄÄRÄ MITTARI
                              nettiterapia == "Bulimian nettiterapia" & grepl("EDE-Q", kysely) ~ TRUE,
                              nettiterapia == "Kaksisuuntaisen mielialahäiriön nettiterapia" & grepl("PHQ-9", kysely) ~ TRUE,
                              nettiterapia == "Masennuksen nettiterapia" & grepl("PHQ-9", kysely) ~ TRUE,
                              nettiterapia == "Nuorten sosiaalisen ahdistu&shy:neisuuden nettiterapia" & grepl("SPIN", kysely, ignore.case = TRUE) ~ TRUE,
                              nettiterapia == "Pakko-oireisen häiriön nettiterapia" & grepl("OCI-R", kysely) ~ TRUE,
                              nettiterapia == "Sosiaalisten tilanteiden pelon nettiterapia" & grepl("SPIN", kysely) ~ TRUE,
                              nettiterapia == "Yleistyneen ahdistuneisuushäiriön nettiterapia" & grepl("GAD-7", kysely) ~ TRUE,
                              nettiterapia == "Paniikkihäiriön nettiterapia" & grepl("PDSS-SR", kysely) ~ TRUE,
                              grepl("kehollisten oireiden kuntoutusohjelma", nettiterapia) & grepl("BIPQ", kysely) ~ TRUE,
                              TRUE ~ FALSE)) %>%
  ungroup()

# Check
prims <- new_ses_times[new_ses_times$prim_mes, ]
table(prims$kysely, prims$nettiterapia)
rm(prims)


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
new_early_times <- new_early_times[, !(names(new_early_times) %in% c("id", "tehtavan_id", "suorituspaiva", "kysely", "otsikko", "prim_mes"))]
new_prim_times <- new_prim_times[, !(names(new_prim_times) %in% c("id", "tehtavan_id", "suorituspaiva", "kysely", "otsikko", "prim_mes"))]
new_eval_times <- new_eval_times[, !(names(new_eval_times) %in% c("id", "tehtavan_id", "suorituspaiva", "kysely", "otsikko", "prim_mes"))]

# Merge datas
new_ses_times2 <- merge(new_early_times, new_eval_times, all = TRUE)
new_ses_times2 <- merge(new_ses_times2, new_prim_times, all = TRUE)


# Add ses_time variable
# Primary measurement time if available
# If not, then earliest task time
# If none of the above available, use session evaluation (end of session)
new_ses_times2$ses_time <- rep(ymd("1999-01-01"), nrow(new_ses_times2))
new_ses_times2$ses_time_from_primary <- rep(FALSE, nrow(new_ses_times2))

for (i in 1:nrow(new_ses_times2)) {
  if (!is.na(new_ses_times2$primary_measure_time[i])) {
    new_ses_times2$ses_time[i] <- new_ses_times2$primary_measure_time[i]
    new_ses_times2$ses_time_from_primary[i] <- TRUE
  } else if (!is.na(new_ses_times2$earliest_time[i])) {
    new_ses_times2$ses_time[i] <- new_ses_times2$earliest_time[i]
  } else if (!is.na(new_ses_times2$end_eval_time[i])) {
    new_ses_times2$ses_time[i] <- new_ses_times2$end_eval_time[i]
  }
}

min(new_ses_times2$ses_time) # "2018-12-12"

length(unique(new_ses_times2$FID)) # 20162
length(unique(new_ses_times2$hoidon_id)) # 20889


rm(new_ses_times, new_early_times, new_eval_times, new_prim_times)
gc()





#### Combine old and new ####

# Harmonize variable names first
names(old_ses_times2)
names(new_ses_times2)

names(old_ses_times2)[names(old_ses_times2) == "Author"] <- "treatment_id"
names(new_ses_times2)[names(new_ses_times2) == "hoidon_id"] <- "treatment_id"

names(old_ses_times2)[names(old_ses_times2) == "CreatedInSession"] <- "session"
names(new_ses_times2)[names(new_ses_times2) == "istunto"] <- "session"

names(new_ses_times2)[names(new_ses_times2) == "nettiterapia"] <- "treatment"

table(new_ses_times2$treatment, useNA = "always")

# New iCBT treatment names
new_ses_times2 <- new_ses_times2 %>%
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

table(new_ses_times2$treatment, useNA = "always")

# Add variable for old vs. new platform treatments
old_ses_times2$platform <- rep("old", nrow(old_ses_times2))
new_ses_times2$platform <- rep("new", nrow(new_ses_times2))

# Rearrange so that variable order matches
old_ses_times2 <- old_ses_times2 %>%
  relocate(FID, .before = treatment_id)

# Merge datasets
all_ses_times2 <- rbind(old_ses_times2, new_ses_times2)

table(all_ses_times2$treatment, useNA = "always")


# Someone has session numbered 0 -> remove
# length(all_ses_times2$session[all_ses_times2$session == 0])
# qf <- all_ses_times2$FID[all_ses_times2$session == 0]
# all_ses_times2[all_ses_times2$FID == qf, ]
all_ses_times2 <- all_ses_times2[all_ses_times2$session != 0, ]


# Remove multiplicated rows
# Also, filter those who have multiple dates for same session

# AJA NÄMÄ UUDESTAAN JA MUISTA SEIVATA!!!
all_ses_times2 <- all_ses_times2 %>% distinct()

all_ses_times2 <- all_ses_times2 %>%
  group_by(treatment_id, session) %>%
  filter((primary_measure_time == min(primary_measure_time, na.rm = TRUE)) %>%
           replace_na(TRUE)) %>%
  ungroup()

all_ses_times2 <- all_ses_times2 %>%
  group_by(treatment_id, session) %>%
  filter((end_eval_time == min(end_eval_time, na.rm = TRUE)) %>%
           replace_na(TRUE)) %>%
  ungroup()

# 196870 -> 196820

# Save
save(all_ses_times2, file = "all_ses_times2.Rdata")






#### checking ####

load("all_ses_times2.Rdata")
load("all_ses_times.Rdata")

length(unique(all_ses_times2$treatment_id)) # 30036
length(unique(all_ses_times$treatment_id)) # 30036

ses_times <- all_ses_times
ses_times$date <- as.character(ses_times$created)
ses_times$tupla <- duplicated(ses_times[c(2,7)]) | duplicated(ses_times[c(2,7)], fromLast = TRUE)

ses_times2 <- all_ses_times2
ses_times2$date <- as.character(ses_times2$ses_time)
ses_times2$tupla <- duplicated(ses_times2[c(2,11)]) | duplicated(ses_times2[c(2,11)], fromLast = TRUE)

length(unique(ses_times$treatment_id[ses_times$tupla])) # 588
length(unique(ses_times2$treatment_id[ses_times2$tupla])) # 590

tupla <- ses_times[ses_times$tupla, ]
tupla2 <- ses_times2[ses_times2$tupla, ]
  



# Check new iCBT gad patients against Jaakko's findings
gad_tuplat <- tupla2[tupla2$treatment == "gad", ]
table(gad_tuplat$platform) # all from new iCBT

new_gad_tuplat <- gad_tuplat[gad_tuplat$platform == "new", ]

load("W:/Jaakko/GAD_internet_therapy/GAD_sessions_in_one_day.RData")

length(unique(duplicate_obs$FID_poti)) # 344
length(unique(new_gad_tuplat$FID)) # 347
length(unique(new_gad_tuplat$treatment_id)) # 347

dup_fids <- unique(duplicate_obs$FID_poti)
ses_fids <- unique(new_gad_tuplat$FID)


common <- ses_fids[which(ses_fids %in% dup_fids)]

diff1 <- dup_fids[which(!dup_fids %in% ses_fids)] # 5 patients

diff2 <- ses_fids[which(!ses_fids %in% dup_fids)] # 8 patients


# ids with multiplicated dates for further use
dup_ids <- unique(tupla2$treatment_id)



#### Corrected date for duplicates ####
all_ses_times2$ses_time_corrected <- all_ses_times2$ses_time
all_ids <- unique(all_ses_times2$treatment_id)

all_ses_times2$tupla <- duplicated(all_ses_times2[c(2,8)]) | duplicated(all_ses_times2[c(2,8)], fromLast = TRUE)

# Replace duplicated date with non-duplicated if some of the other date variables has one
for (i in 1:length(all_ids)) {
  if (all_ids[i] %in% dup_ids) {
    temp <- all_ses_times2[all_ses_times2$treatment_id == all_ids[i], ]
    for (j in 1:nrow(temp)) {
      if (temp$tupla[j] == TRUE) {
        if ((!is.na(temp$primary_measure_time[j])) & temp$primary_measure_time[j] != temp$ses_time[j]) {
          temp$ses_time_corrected[j] <- temp$primary_measure_time[j]
        } else if ((!is.na(temp$earliest_time[j])) & temp$earliest_time[j] != temp$ses_time[j]) {
          temp$ses_time_corrected[j] <- temp$earliest_time[j]
        } else if ((!is.na(temp$end_eval_time[j])) & temp$end_eval_time[j] != temp$ses_time[j]) {
          temp$ses_time_corrected[j] <- temp$end_eval_time[j]
        }
      }
      all_ses_times2$ses_time_corrected[(all_ses_times2$treatment_id == temp$treatment_id[j] & all_ses_times2$session == temp$session[j])] <- temp$ses_time_corrected[j]
    }
  }
}

# Vars back to date
# all_ses_times2$earliest_time <- ymd(all_ses_times2$earliest_time)
# all_ses_times2$end_eval_time <- ymd(all_ses_times2$end_eval_time)
# all_ses_times2$primary_measure_time <- ymd(all_ses_times2$primary_measure_time)
# all_ses_times2$ses_time <- ymd(all_ses_times2$ses_time)
# all_ses_times2$ses_time_corrected <- ymd(all_ses_times2$ses_time_corrected)

# Check with old duplicate ids
tupla3 <- all_ses_times2[all_ses_times2$treatment_id %in% dup_ids, ]


save(all_ses_times2, file = "all_ses_times2.Rdata")




#### Re-checking ####
load("all_ses_times2.Rdata")
load("all_ses_times.Rdata")

length(unique(all_ses_times2$treatment_id)) # 30036
length(unique(all_ses_times$treatment_id)) # 30036

ses_times <- all_ses_times
ses_times$date <- as.character(ses_times$created)
ses_times$tupla <- duplicated(ses_times[c(2,7)]) | duplicated(ses_times[c(2,7)], fromLast = TRUE)

ses_times2 <- all_ses_times2
ses_times2$date <- as.character(ses_times2$ses_time)
ses_times2$tupla <- duplicated(ses_times2[c(2,11)]) | duplicated(ses_times2[c(2,11)], fromLast = TRUE)

length(unique(ses_times$treatment_id[ses_times$tupla])) # 588
length(unique(ses_times2$treatment_id[ses_times2$tupla])) # 457

tupla <- ses_times[ses_times$tupla, ]
tupla2 <- ses_times2[ses_times2$tupla, ]




# Check new iCBT gad patients against Jaakko's findings
gad_tuplat <- tupla2[tupla2$treatment == "gad", ]
table(gad_tuplat$platform) # all from new iCBT

new_gad_tuplat <- gad_tuplat[gad_tuplat$platform == "new", ]

load("W:/Jaakko/GAD_internet_therapy/GAD_sessions_in_one_day.RData")

length(unique(duplicate_obs$FID_poti)) # 344
length(unique(new_gad_tuplat$FID)) # 298
length(unique(new_gad_tuplat$treatment_id)) # 298

dup_fids <- unique(duplicate_obs$FID_poti)
ses_fids <- unique(new_gad_tuplat$FID)


common <- ses_fids[which(ses_fids %in% dup_fids)]

diff1 <- dup_fids[which(!dup_fids %in% ses_fids)] # 54 patients

diff2 <- ses_fids[which(!ses_fids %in% dup_fids)] # 8 patients

oudot_tuplat <- all_ses_times2[all_ses_times2$FID %in% diff2, ]
save(oudot_tuplat, file = "oudot_tuplat.Rdata")
# Mostly patients with NA in primary measure
# One strange patient with two dates for each of the 6 sessions 
# For this patient, duplicate has been replaced with another duplicate












