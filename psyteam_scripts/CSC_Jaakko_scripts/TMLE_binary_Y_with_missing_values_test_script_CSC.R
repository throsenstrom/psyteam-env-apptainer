library(data.table)
library(sl3)
library(tmle3)
library(tmle3mopttx)
library(devtools)

set.seed(111)

data("data_cat_realistic")

data_missing <- data_cat_realistic

#Add some random missingless:
rr <- sample(nrow(data_missing), 100, replace = FALSE)
data_missing[rr,"Y"]<-NA

node_list <- list(
  W = c("W1", "W2", "W3", "W4"),
  A = "A",
  Y = "Y"
)

# Initialize few of the learners:
lrn_xgboost_50 <- Lrnr_xgboost$new(nrounds = 50)
lrn_xgboost_100 <- Lrnr_xgboost$new(nrounds = 100)
lrn_xgboost_500 <- Lrnr_xgboost$new(nrounds = 500)
lrn_mean <- Lrnr_mean$new()
lrn_glm <- Lrnr_glm_fast$new()

## Define the Q learner, which is just a regular learner:
# specify the appropriate loss of the multinomial learner:
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


delta_learner <- Lrnr_sl$new(
  learners = list(lrn_mean, lrn_glm),
  metalearner = Lrnr_nnls$new()
)

# specify outcome and treatment regressions and create learner list
learner_list <- list(Y = Q_learner, A = g_learner, B = b_learner, delta_Y=delta_learner)


tmle_spec_cat_miss <- tmle3_mopttx_blip_revere(
  V = c("W1", "W2", "W3", "W4"), type = "blip2",
  learners = learner_list, maximize = TRUE, complex = TRUE,
  realistic = FALSE
)


fit_cat_miss <- tmle3(tmle_spec_cat_miss, data_missing, node_list, learner_list)
sessionInfo()

