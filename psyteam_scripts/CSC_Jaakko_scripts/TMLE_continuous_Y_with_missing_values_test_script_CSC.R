library(sl3)
library(tmle3)
library(tmle3mopttx)
library(data.table)


washb_data <- fread(
  paste0(
    "https://raw.githubusercontent.com/tlverse/tlverse-data/master/",
    "wash-benefits/washb_data.csv"
  ),
  stringsAsFactors = TRUE
)

washb_data$complete<-complete.cases(washb_data)
table(washb_data$complete)
data<-subset(washb_data, complete==TRUE)
data$complete<-NULL
table(data$tr)

node_list <- list(
  W = c(
    "month", "aged", "sex", "momage", "momedu",
    "momheight", "hfiacat", "Nlt18", "Ncomp", "watmin",
    "elec", "floor", "walls", "roof", "asset_wardrobe",
    "asset_table", "asset_chair", "asset_khat",
    "asset_chouki", "asset_tv", "asset_refrig",
    "asset_bike", "asset_moto", "asset_sewmach",
    "asset_mobile"
  ),
  A = "tr",
  Y = "whz"
)

# recode treatment to numeric-coded factor 1..7
data$tr_num <- factor(as.integer(data$tr), levels = 1:7)

str(data$tr)
str(data$tr_num)

# update node_list
node_list2 <- node_list
node_list2$A <- "tr_num"

table(data$tr_num, useNA = "ifany")
anyNA(data$tr_num)

#add missingness
data1<-data
n <- nrow(data1)
idx_miss <- sample(seq_len(n), size = floor(0.20 * n))

data1$whz[idx_miss] <- NA
psych::describe(data1)

# Initialize few of the learners:
lrn_xgboost_50 <- Lrnr_xgboost$new(nrounds = 50)
lrn_xgboost_100 <- Lrnr_xgboost$new(nrounds = 100)
lrn_xgboost_500 <- Lrnr_xgboost$new(nrounds = 500)
lrn_mean <- Lrnr_mean$new()
lrn_glm <- Lrnr_glm_fast$new()

## Define the Q learner, which is just a regular learner:
Q_learner <- Lrnr_sl$new(
  learners = list(lrn_xgboost_100, lrn_mean, lrn_glm),
  metalearner = Lrnr_nnls$new()
)

# Define the g learner, which is a multinomial learner:
# specify the appropriate loss of the multinomial learner:
mn_metalearner <- make_learner(Lrnr_solnp,
                               eval_function = loss_loglik_multinomial,
                               learner_function = metalearner_linear_multinomial
)
g_learner <- make_learner(Lrnr_sl, list(lrn_xgboost_100, lrn_xgboost_500, lrn_mean), mn_metalearner)

# Define the Blip learner, which is a multivariate learner:
learners <- list(lrn_xgboost_50, lrn_xgboost_100, lrn_xgboost_500, lrn_mean, lrn_glm)
b_learner <- create_mv_learners(learners = learners)


#define delta learner
delta_learner <- Lrnr_sl$new(
  learners = list(lrn_mean, lrn_glm),
  metalearner = Lrnr_nnls$new()
)



tsm_spec_1 <- tmle_TSM_all()

#Without missigness using complete case data
# specify outcome and treatment regressions and create learner list for ATE estimation
learner_list1_without_missing <- list(Y = Q_learner, A = g_learner)
learner_list2_without_missing <- list(Y = Q_learner, A = g_learner, B = b_learner)

t0 <- proc.time()
set.seed(54197)
tmle_fit_without_missing <- tmle3(tsm_spec_1, data, node_list2, learner_list1_without_missing)
proc.time()-t0
tmle_fit_without_missing

tmle_spec_dynamic_no_missing <- tmle3_mopttx_blip_revere(
  V = c("month", "aged", "sex", "momage", "momedu",
        "momheight", "hfiacat", "Nlt18", "Ncomp", "watmin",
        "elec", "floor", "walls", "roof", "asset_wardrobe",
        "asset_table", "asset_chair", "asset_khat",
        "asset_chouki", "asset_tv", "asset_refrig",
        "asset_bike", "asset_moto", "asset_sewmach",
        "asset_mobile"), type = "blip2",
  learners = learner_list2_without_missing, maximize = TRUE, complex = TRUE,
  realistic = FALSE
)

t0 <- proc.time()
set.seed(54197)
tmle_fit_without_missing_dynamic <- tmle3(tmle_spec_dynamic_no_missing, data, node_list2, learner_list2_without_missing)
proc.time()-t0
tmle_fit_without_missing_dynamic


#WITH MISSING DATA THE SAME CODE DOES NOT WORK
table(is.na(data1$whz))


# specify outcome and treatment regressions and create learner list for ATE estimation
learner_list1 <- list(Y = Q_learner, A = g_learner, delta_Y=delta_learner)
learner_list2 <- list(Y = Q_learner, A = g_learner, B = b_learner, delta_Y=delta_learner)

t0 <- proc.time()
set.seed(54197)
tmle_fit_with_missing <- tmle3(tsm_spec_1, data1, node_list2, learner_list1)

tmle_spec_dynamic_missing <- tmle3_mopttx_blip_revere(
  V = c("month", "aged", "sex", "momage", "momedu",
        "momheight", "hfiacat", "Nlt18", "Ncomp", "watmin",
        "elec", "floor", "walls", "roof", "asset_wardrobe",
        "asset_table", "asset_chair", "asset_khat",
        "asset_chouki", "asset_tv", "asset_refrig",
        "asset_bike", "asset_moto", "asset_sewmach",
        "asset_mobile"), type = "blip2",
  learners = learner_list2, maximize = TRUE, complex = TRUE,
  realistic = FALSE
)

tmle_fit_missing_dynamic <- tmle3(tmle_spec_dynamic_missing, data1, node_list2, learner_list2)

sessionInfo()
