setwd("W:/Sanna/")


library(stringr)
library(dplyr)
library(tidyr)


# Load data

# df <- read.csv("W:/data/HUS/bcb_psykoterapia_korjattuLaRe/FD_4810_bcb_form_data.csv",
#                sep = ";") # Tämäkin toimii, mutta hitaammin
df <- vroom::vroom("W:/data/HUS/bcb_psykoterapia_korjattuLaRe/FD_4810_bcb_form_data.csv")



# table(df$template_code)
# table(df$template_name)
# table(df$question_code)
# table(df$question_name)


# Limit data to goalsetting at beginning of treatment
df2 <- df[grepl("tavoitteet", df$question_code), ]
df2 <- df2[grepl("psykoterapeutin_alkuarvio", df2$template_code), ]

table(df2$question_code)
table(df2$question_code, df2$template_code)

df3 <- df2[df2$question_code == "psykoterapian_tavoitteet", ] # For checking

# Which treatment ids have at least two sets of goals
d_ids <- df3$treatment_id[duplicated(df3$treatment_id)]
length(unique(d_ids)) # 316 patients
df_dups <- df3[df3$treatment_id %in% d_ids, ]



# Get classified goals
var_names <- df3$options[1]

var_names <- str_split_1(var_names, ",")
var_names <- gsub("\\^.*", "", var_names)
var_names



# New data to include classified goals and free form text fields for each treatment
d_g <- df2[, c("patient_id", "treatment_id", "form_id", "date_created")]
d_g <- d_g %>%
  distinct(form_id, .keep_all = TRUE)

length(unique(d_g$form_id))
form_ids <- d_g$form_id # Use form_id for looping here to avoid the issue of multiple forms per treatment id

# Include a variable to denote adult/youth/childern's treatments
d_g$age_group <- NA

# Goal variables
d_g[var_names] <- NA
d_g$tavoitteet_tarkenna <- NA
d_g$muu_mika <- NA

# Loop
for (i in 1:length(form_ids)) {
  id <- form_ids[i]
  temp <- df2[df2$form_id == id, ]
  goal <- temp$list_answer[temp$question_code == "psykoterapian_tavoitteet"]
  
  # Free form text fields
  if ("psykoterapian_tavoitteet_tarkenna" %in% temp$question_code) {
    d_g$tavoitteet_tarkenna[d_g$form_id == id] <- temp$string_answer[temp$question_code == "psykoterapian_tavoitteet_tarkenna"]
  }
  if ("psykoterapian_tavoitteet_muu_mika" %in% temp$question_code) {
    d_g$muu_mika[d_g$form_id == id] <- temp$string_answer[temp$question_code == "psykoterapian_tavoitteet_muu_mika"]
  }
  
  # Age group indicator
  if (grepl("aikuiset", temp$template_code[1])) {
    d_g$age_group[d_g$form_id == id] <- "aikuiset"
  } else if (grepl("nuoret", temp$template_code[1])) {
    d_g$age_group[d_g$form_id == id] <- "nuoret"
  } else if (grepl("lapset", temp$template_code[1])) {
    d_g$age_group[d_g$form_id == id] <- "lapset"
  }
  
  # Goal classes
  for (j in 1:length(var_names)) {
    v_n <- var_names[j]
    if (grepl(v_n, goal)) {
      d_g[d_g$form_id == id, v_n] <- 1
    } else {
      d_g[d_g$form_id == id, v_n] <- 0
    }
  }
}

sum(is.na(d_g$toimintakyvyn_parantuminen)) # 0
sum(is.na(d_g$tunnesaatelyn_keinojen_lisaantyminen)) # 0
sum(is.na(d_g$age_group)) # 0


# Transform to long form to check frequencies
d_g_l <- d_g %>%
  pivot_longer(cols = all_of(var_names), names_to = "list_answer")

d_g_l <- d_g_l[d_g_l$value == 1, ]

table(d_g_l$list_answer)
sorted <- sort(table(d_g_l$list_answer[d_g_l$age_group == "aikuiset"]))

# Visualize
library(ggplot2)

p1 <- d_g_l %>%
  filter(age_group == "aikuiset") %>% # Note, adults only plotted here
  ggplot(aes(x = list_answer)) +
  geom_bar() +
  scale_x_discrete(limits = names(sorted)) +
  theme_classic() +
  coord_flip()

p1


save(d_g, file = "lare_goals_wide.Rdata")
save(d_g_l, file = "lare_goals_long.Rdata")






