library(data.table)
library(dplyr)
library(tmle3)
library(sl3)

###############################################################
### OITR for categorical treatment, binary Y, complete data ###
###############################################################
library(tmle3mopttx)

data("data_cat_realistic")

psych::describe(data_cat_realistic)

data <- na.omit(data_cat_realistic) # already complete 

# recode treatment to numeric-coded factor 1-3
data$A <- factor(data$A)

str(data$A)

node_list <- list(
  W = c("W1", "W2", "W3", "W4"),
  A = "A",
  Y = "Y"
)


## Constructing ootimal stacked regressions with sl3
sl3_list_learners(c("categorical"))
# Initialize few of the learners:
lrn_xgboost_50 <- Lrnr_xgboost$new(nrounds = 50)
lrn_xgboost_100 <- Lrnr_xgboost$new(nrounds = 100)
lrn_xgboost_500 <- Lrnr_xgboost$new(nrounds = 500)
lrn_mean <- Lrnr_mean$new()
lrn_glm <- Lrnr_glm_fast$new()

## Define the Q learner
Q_learner <- Lrnr_sl$new(
  learners = list(lrn_xgboost_100, lrn_mean, lrn_glm),
  metalearner = Lrnr_nnls$new()
)

# Define the g learner
# specify the appropriate loss of the multinomial learner:
mn_metalearner <- make_learner(Lrnr_solnp,
                               eval_function = loss_loglik_multinomial,
                               learner_function = metalearner_linear_multinomial
)

g_learner <- make_learner(Lrnr_sl, list(lrn_xgboost_100, lrn_xgboost_500, lrn_mean), mn_metalearner)


# Define the Blip learner
learners <- list(lrn_xgboost_50, lrn_xgboost_100, lrn_xgboost_500, lrn_mean, lrn_glm)
b_learner <- create_mv_learners(learners = learners)


# specify outcome and treatment regressions and create learner list
learner_list <- list(Y = Q_learner, A = g_learner, B = b_learner)


## Targeted estimation of the mean under the optimal individualized interventions effects
# initialize a tmle specification
tmle_spec_oitr <- tmle3_mopttx_blip_revere(
  V = c("W1", "W2", "W3", "W4"),
  type = "blip2",
  learners = learner_list, maximize = TRUE, complex = TRUE,
  realistic = FALSE
)

# fit the TML estimator
set.seed(556)
fit_oitr <- tmle3(tmle_spec_oitr, data, node_list, learner_list) 
fit_oitr

# How many individuals got assigned each treatment?
table(tmle_spec_oitr$return_rule) 
table(data$A)

indicated_treats <- tmle_spec_oitr$return_rule


### Blip preds ###
blip_oitr <- tmle_spec_oitr$make_tmle_task(data, node_list)
blip_preds_oitr <- tmle_spec_oitr$get_blip_pred(tmle_task = blip_oitr) 
head(blip_preds_oitr) # jokaiselle ID:lle kaikissa hoitovaihtoehdoissa

d_blips <- as.data.frame(matrix(data = unlist(blip_preds_oitr),
                                nrow = 1000, ncol = 3, byrow = T))
head(d_blips)

# max blip = blip of the optimal treatment 
# “What is the best possible improvement this person could get?”
max_blips <- c()
for(i in 1:nrow(d_blips)) {
  max_blips[i] = max(blip_preds_oitr[[i]][[1]])
}
max_blips[1:6] # ok

# Add max blips and indicated treatments into the same data
d_res <- cbind(data,
               data.frame(pred_treat = indicated_treats,
                          max_blip = max_blips))

head(d_res) 
table(d_res$A, d_res$pred_treat)

##################################
##### OITR SHAPS with ranger #####
##################################
# Which baseline features drive the optimal treatment decision?
library(fastshap)
library(shapviz)
library(ranger)

grid <- expand.grid(
  mtry = c(1:4), # number of vars to possibly split
  splitrule = "variance", # default for regression
  min.node.size = c(1,5,10)) # min node size to split at, default = 5 for regression 

ctrl <- trainControl(method = "cv", number = 10, savePredictions = "final")

set.seed(222)
ranger_train <- train(x = select(d_res, W1, W2, W3, W4), 
                      y= d_res$max_blip, 
                      method = "ranger", 
                      num.trees = 1000,
                      tuneGrid = grid,
                      importance= "permutation",
                      trControl= ctrl)
ranger_train$results
ranger_train$finalModel

set.seed(231)
ranger_mod <- ranger(max_blip ~ W1 + W2 + W3 + W4, 
                     mtry = 4, 
                     num.trees = 1000,
                     min.node.size = 1, 
                     importance = "permutation",
                     data = d_res) 
ranger_mod


# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

# Global explanations with fastshap 
set.seed(555)
shaps <- fastshap::explain(ranger_mod, X= select(d_res, W1, W2, W3, W4), pred_wrapper= pfun,
                           nsim= 100, adjust = T, shap_only = F)

head(shaps)

shaps_tib <- tibble::as_tibble(shaps$shapley_values)
head(shaps_tib)

# Additive feature of SHAPS
-0.0227 -0.00717 -0.00333  -0.00270
-0.0359 + 0.1293762# add the baseline
# Compare to the ranger prediction
predict(ranger_mod, d_res[1,])$predictions


### Visualize with shapviz
shv_global <- shapviz(shaps)
shv_global

# Beeswarm plot
shap_bee <- sv_importance(shv_global, kind = "bee")
shap_bee


###############################################################################
#### OITR with simulated data: continuous Y, categorical A, complete cases ####
################################################################################

# Simulate data
set.seed(123)
n <- 1000

# Covariates
W1 <- rnorm(n)
W2 <- rnorm(n)
W3 <- rbinom(n, 1, 0.5)
W4 <- runif(n, -1, 1)
W5 <- rnorm(n)

W <- data.frame(W1, W2, W3, W4, W5)

# 2. Multi-arm treatment (4 arms)
# Balanced randomization
A <- sample(1:4, size = n, replace = TRUE)

# 3. Treatment-specific conditional means
# Construct symmetric interactions so no treatment dominates globally
mu1 <-  0.5*W1 - 0.5*W2 + 0.3*W3
mu2 <- -0.5*W1 + 0.5*W2 + 0.3*W4
mu3 <-  0.5*W2 - 0.5*W4 + 0.3*W5
mu4 <- -0.5*W1 - 0.5*W3 + 0.3*W5

# Stack into matrix
MU <- cbind(mu1, mu2, mu3, mu4)

# 4. Continuous outcome
sigma <- 1
Y <- MU[cbind(1:n, A)] + rnorm(n, 0, sigma)

# 5. Create dataset
dat <- data.frame(W1, W2, W3, W4, W5,
                  A = factor(A),
                  Y = Y)
head(dat)
tapply(Y, dat$A, mean) # none of the treatment arms is 'the best'

str(dat$A) # is factor



node_list_oitr_sim <- list(
  W = c( "W1", "W2", "W3", "W4", "W5"),
  A = "A",
  Y = "Y"
)


## Constructing ootimal stacked regressions with sl3
sl3_list_learners(c("categorical"))
sl3_list_learners(c("continuous"))
# Initialize few of the learners:
lrn_xgboost_50 <- Lrnr_xgboost$new(nrounds = 50)
lrn_xgboost_100 <- Lrnr_xgboost$new(nrounds = 100)
lrn_xgboost_500 <- Lrnr_xgboost$new(nrounds = 500)
lrn_mean <- Lrnr_mean$new()
lrn_glm <- Lrnr_glm_fast$new()

## Define the Q learner (continuous Y)
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

g_learner <- make_learner(Lrnr_sl, 
                          list(lrn_xgboost_100, lrn_xgboost_500, lrn_mean), mn_metalearner)


# Define the Blip learner, which is a multivariate learner:
learners <- list(lrn_xgboost_50, lrn_xgboost_100, lrn_xgboost_500, lrn_mean)
b_learner <- create_mv_learners(learners = learners)

# specify outcome and treatment regressions and create learner list
learner_list_oitr_sim <- list(Y = Q_learner, A = g_learner, B = b_learner)

## Targeted estimation of the mean under the optimal individualized interventions effects
# initialize a tmle specification
tmle_spec_oitr_sim <- tmle3_mopttx_blip_revere(
  V = c("W1", "W2", "W3", "W4", "W5"), 
  type = "blip2",
  learners = learner_list_oitr_sim, 
  maximize = TRUE, complex = TRUE,
  realistic = FALSE
)

# fit the TML estimator
set.seed(222)
fit_oitr_sim <- tmle3(tmle_spec_oitr_sim, dat, 
                       node_list_oitr_sim, learner_list_oitr_sim) 

fit_oitr_sim # ok
# A tmle3_Fit that took 1 step(s)
# type         param     init_est   tmle_est     se      lower     upper   psi_transformed    lower_transformed   upper_transformed
# <char>        <char>     <num>     <num>      <num>     <num>     <num>           <num>             <num>             <num>
#  1:    TSM E[Y_{A=NULL}] 0.2029035 0.5739655 0.07163209 0.4335692 0.7143618       0.5739655         0.4335692         0.7143618

# How many individuals got assigned each treatment?
table(tmle_spec_oitr_sim$return_rule) 
# 1   2   3   4 
# 487 269 140 104 

indicated_treats_sim <- tmle_spec_oitr_sim$return_rule


### Blip preds ###
blip_oitr_sim <- tmle_spec_oitr_sim$make_tmle_task(dat, node_list_oitr_sim)
blip_preds_oitr_sim <- tmle_spec_oitr_sim$get_blip_pred(tmle_task = blip_oitr_sim) 
head(blip_preds_oitr_sim) # jokaiselle henkilölle kaikissa hoitovaihtoehdoissa

d_blips_sim <- as.data.frame(matrix(data = unlist(blip_preds_oitr_sim),
                                nrow = 1000, ncol = 4, byrow = T))
head(d_blips_sim)

# max blip = blip of the optimal treatment 
# “What is the best possible improvement this person could get?”
max_blips_sim <- c()
for(i in 1:nrow(d_blips_sim)) {
  max_blips_sim[i] = max(blip_preds_oitr_sim[[i]][[1]])
}
max_blips_sim[1:6] # ok

# Add max blips and indicated treatments into the same data
d_res_sim <- cbind(dat,
               data.frame(pred_treat = indicated_treats_sim,
                          max_blip = max_blips_sim))

head(d_res_sim) 
table(d_res_sim$A, d_res_sim$pred_treat)



########################################
##### OITR SHAPS with superlearner #####
########################################
library(fastshap)
library(shapviz)
library(ranger)

names(d_res_sim)
d_sl <- select(d_res_sim, c("W1", "W2", "W3", "W4", "W5", "max_blip"))
head(d_sl)
               
# Define the prediction task
task <- make_sl3_Task(
  data = d_sl,
  outcome = "max_blip",
  covariates = c("W1", "W2", "W3", "W4", "W5")
)


# Ennustetaan superlearnerilla max_blippejä
sl3_list_learners(properties = "continuous")

lrn_mean_Q <- Lrnr_mean$new()
lrn_glm_Q <- Lrnr_glm_fast$new()
lrn_gam_Q <- Lrnr_gam$new()
lrn_lasso_Q <- Lrnr_glmnet$new(alpha = 1)
lrn_enet.5_Q <- Lrnr_glmnet$new(alpha = 0.5)
lrn_ridge_Q <- Lrnr_glmnet$new(alpha = 0)
lrn_ranger_Q <-Lrnr_ranger$new()
lrn_earth_Q <-Lrnr_earth$new()
lrn_xgboost_Q <- Lrnr_xgboost$new()
lrn_svm_Q <- Lrnr_svm$new()

# Create stack
stack <- Stack$new(
    lrn_mean_Q,
    lrn_glm_Q,
    lrn_gam_Q,
    lrn_lasso_Q,
    lrn_enet.5_Q,
    lrn_ridge_Q,
    lrn_ranger_Q,
    lrn_earth_Q,
    lrn_xgboost_Q,
    lrn_svm_Q)

# Initiate SL
sl <- Lrnr_sl$new(learners = stack, metalearner = Lrnr_nnls$new())

# Fit the SL
set.seed(465)
sl_fit <- sl$train(task = task)
sl_fit

# SL predictions
sl_preds <- sl_fit$predict(task = task)
head(sl_preds)



# Prediction wrapper 
pfun_sl <- function(object, newdata) {
  # add dummy outcome (required by make_sl3_Task)
  newdata$max_blip <- 0
  # create new task with same structure
  new_task <- make_sl3_Task(
    data = data.frame(newdata),
    outcome = "max_blip",
    covariates = c("W1", "W2", "W3", "W4", "W5")
  )
  # return numeric predictions
  as.numeric(object$predict(new_task))
}

# Define feature matrix for fastshap
X <- d_sl[, c("W1", "W2", "W3", "W4", "W5")]

# Global explanations with fastshap  (uses Monte Carlo?)
set.seed(555)

shaps_sl <- fastshap::explain(
  object = sl_fit, 
  X= X, 
  pred_wrapper= pfun_sl,
  nsim= 100, adjust = T, shap_only = F)

head(shaps_sl)
# ...
# $baseline
# [1] 0.5738319

shaps_sl_tib <- tibble::as_tibble(shaps_sl$shapley_values)
head(shaps_sl_tib)

# Additive feature of SHAPS
-0.0371  + 0.0598 +  0.0309 -0.0321 +  0.0249 # = 0.0464
0.0464 +  0.5738319 # add the baseline = 0.6202319
# Compare to the SL prediction
sl_preds[1]


### Visualize with shapviz
shv_global_sl <- shapviz(shaps_sl)
shv_global_sl

# Beeswarm plot
shap_bee_sl <- sv_importance(shv_global_sl, kind = "bee")
shap_bee_sl
