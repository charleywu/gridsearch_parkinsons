#Simulation of models
# Anna Giron, 2021
# Björn Meder, 2025

#housekeeping
rm(list=ls())

#source of modeling code
source("models.R")

#packages
packages <- c('plyr', 'dplyr', 'jsonlite', 'lsr', 'BayesFactor', 'matrixcalc', 'future.apply', 'data.table', 'progressr')
lapply(packages, require, character.only = TRUE) #loads packages

# for parallel on local machine
# handlers(global = TRUE)
# plan(multisession, workers = parallel::detectCores() -1)  
set.seed(0511)
# RNGkind("L'Ecuyer-CMRG")                 


#extract environments
environments <- lapply(fromJSON("data/kernelRough.json"), FUN=function(x) matrix(as.numeric(unlist(x)), ncol=3, byrow=TRUE, dimnames=list(seq(1,64),  c('x2', 'y', 'x1'))))

env <- as.data.frame(environments[[1]])
for (i in 2:40){
  env<-rbind(env,as.data.frame(environments[[i]]))
}
env$en<-rep(1:40, each=64)

#Scaling reward range
env$y<-env$y*50

# simulation rounds for each parameter combination
replications = 100
# replications = 2


#############################################################################################################################
# Import fitted parameters
#############################################################################################################################
# get parameter estimates
modelFit = read.csv('modelResults/modelFit.csv')

modelFit = modelFit %>%
  mutate(kernel=factor(kernel, levels=c('RBF', 'BMT'), labels=c('GP', 'BMT'))) 

# only use GP-UCB model
modelFit = subset(modelFit, kernel=='GP' & acq=='UCB')


#############################################################################################################################
# Simulating performance for different parameter values
#############################################################################################################################
# Tukey's fence to compute upper and lower bound for each parameter
tukeysFence <- function(x, k = 1.5, na.rm = TRUE) {
  quar <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- diff(quar)
  
  lower = quar[1] - k * iqr
  upper = quar[2] + k * iqr
  
  return(c(lower, upper))
}

# Use log-ranges
lambda_range <- tukeysFence(log(modelFit$lambda))
beta_range   <- tukeysFence(log(modelFit$beta))
tau_range    <- tukeysFence(log(modelFit$tau)); tau_range[1] <- -5  # enforce lower bound


# fixed theoratical range
lambda_vals <- 1
beta_vals   <- exp(seq(log(0.001), log(50), length.out = 100))
tau_vals    <- exp(seq(log(0.001), log(20), length.out = 100))

# lambda_vals <- exp(seq(lambda_range[1], lambda_range[2], length.out = 5))
# beta_vals   <- exp(seq(beta_range[1],   beta_range[2],   length.out = 20))
# tau_vals    <- exp(seq(tau_range[1],    tau_range[2],    length.out = 20))

params <- expand.grid(lambda = lambda_vals, beta = beta_vals, tau = tau_vals)

# --- small-scale settings ---
#set.seed(1)
replications <- 100
env_ids <- sample(unique(env$en), size = min(5, length(unique(env$en))), replace = FALSE)

dparams <- data.frame(lambda = numeric(), beta = numeric(), tau = numeric(),
                      mu = numeric(), replication = integer(), stringsAsFactors = FALSE)

Xstar <- cbind(env$x1[1:64], env$x2[1:64])
k<-rbf

param_ids <- seq_len(nrow(params))

# with_progress({
#   p <- progressor(steps = length(param_ids))
#   
#   res_list <- future_lapply(param_ids, function(i) {
#     lambda <- params$lambda[i]
#     beta   <- params$beta[i]
#     tau    <- params$tau[i]
#     parVec <- c(lambda, lambda, 1, 1e-4)
#     
#     rows <- vector("list", replications)
#     for (rep_idx in seq_len(replications)) {
#       enselect <- sample(env_ids, 1)
#       environ  <- subset(env, en == enselect)
#       X <- cbind(environ$x1[sample(1:64, 1)], environ$x2[sample(1:64, 1)])
#       y <- as.matrix(environ$y[sample(1:64, 1)])
#       
#       for (trial in 1:25) {
#         out <- gpr(X.test = Xstar, theta = parVec, X = X, Y = (y - 25)/50, k = k)
#         util <- ucb(out, beta)
#         p_vec <- exp((util - max(util)) / tau)
#         p_vec <- p_vec / sum(p_vec)
#         p_vec <- pmax(pmin(p_vec, 0.99999), 1e-5)
#         
#         ind <- sample(1:64, 1, prob = p_vec)
#         X   <- rbind(X, cbind(environ$x1[ind], environ$x2[ind]))
#         y   <- rbind(y, as.matrix(environ$y[ind]))
#       }
#       y <- y[-1]
#       rows[[rep_idx]] <- list(lambda=lambda, beta=beta, tau=tau,
#                               mu=mean(y), replication=rep_idx)
#     }
#     
#     p()  # increment progress bar
#     data.table::rbindlist(rows)
#   })
# })
# 
# dparams <- data.table::rbindlist(res_list)

# not parallel
for (i in seq_len(nrow(params))) {

  lambda <- params$lambda[i]
  beta   <- params$beta[i]
  tau    <- params$tau[i]
  parVec <- c(lambda, lambda, 1, 1e-4)


  for (rep in seq_len(replications)) {

    message(paste(
      "Param set", i, "/", nrow(params),
      "lambda=", signif(lambda, 3),
      "beta=", signif(beta, 3),
      "tau=", signif(tau, 3),
      "rep", rep, "/", replications
    ))

    enselect <- sample(env_ids, 1)
    environ  <- subset(env, en == enselect)

    ind <- sample(1:64, 1)
    X   <- cbind(environ$x1[ind], environ$x2[ind])
    y   <- as.matrix(environ$y[ind])

    for (trial in 1:25) {
      out <- gpr(X.test = Xstar, theta = parVec, X = X, Y = (y - 25)/50, k = k)
      utilityVec <- ucb(out, beta)

      utilities <- utilityVec - max(utilityVec)
      p <- exp(utilities / tau)
      p <- p / colSums(p)
      p <- pmax(pmin(p, 0.99999), 1e-5)

      ind <- sample(1:64, 1, prob = p)
      X   <- rbind(X, cbind(environ$x1[ind], environ$x2[ind]))
      y   <- rbind(y, as.matrix(environ$y[ind]))
    }
    y <- y[-1]  # drop initial random observation
    dparams <- rbind(dparams, data.frame(lambda=lambda, beta=beta, tau=tau,
                                         mu=mean(y), replication=rep))
  }
}

# ensure output dir exists
outdir <- "modelResults/simulatedModels/local"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
outfile <- file.path(outdir, "simulatedModels_local.csv")
write.csv(dparams, outfile, row.names = FALSE)
message("Wrote: ", outfile)


# parameter range based on Tukey's fence
# lambda = tukeysFence(log(modelFit$lambda))
# beta = tukeysFence(log(modelFit$beta))
# tau = tukeysFence(log(modelFit$tau))
# 
# # lower bound for tau below range defined for model fitting - set to lower bound
# tau[1] = -5
# 
# # parameters to simulate
# params = expand.grid(lambda=exp(seq(lambda[1], lambda[2], len=100)), beta=exp(seq(beta[1], beta[2], len=100)),
#                      tau=exp(seq(tau[1], tau[2], len=100)))
# 
# #############################################################################################################################
# # Simulation
# #############################################################################################################################
# # Cluster id from qsub
# # per job, run 1000 simulations with different parameter combinations
# # ids 1 - 1000
# # clusterid <- sample(1:1000, 1) # sample random cluster id for testing
# clusterid = as.integer(commandArgs(TRUE)[1]) # Cluster id, corresponds to an integer used to indicate which combination of kernel and acquisition function to simulate
# 
# params$clusterid = rep(1:1000, each=1000)
# 
# # parameters for simulation based on cluster id
# lambdas = params[params$clusterid==clusterid,1]
# betas = params[params$clusterid==clusterid,2]
# taus = params[params$clusterid==clusterid,3]
# 
# dparams = data.frame(lambda=numeric(), beta=numeric(), tau=numeric(), mu=numeric(), replication=numeric())
# 
# Xstar<-cbind(env$x1[1:64], env$x2[1:64])
# k<-rbf
# 
# for (i in 1:length(lambdas)) {
# # for (i in 1:1) {
#   lambda = lambdas[i]
#   beta = betas[i]
#   tau = taus[i]
#   parVec <- c(lambda, lambda, 1, .0001) 
#   
#   mu = numeric()
#   for (round in 1:replications){
#     enselect<-sample(1:40, 1)
#     environ<-subset(env, en==enselect)
#     ind<-sample(1:64,1)
#     #X matrix
#     X<-cbind(environ$x1[ind], environ$x2[ind])
#     #y matrix
#     y<-as.matrix(environ$y[ind])
#     #loop through trials
#     for (trial in 1:25){
#       #output by GP with particular parameter settings
#       #don't forget mean centering and standardization
#       out<-gpr(X.test=Xstar, theta=parVec, X=X, Y=(y-25)/50, k=k)
#       #utility vector by UCB
#       utilityVec<-ucb(out, beta)
#       #avoid overflow
#       utilities <- utilityVec - max(utilityVec)
#       #softmaximization
#       p <- exp(utilities/tau)
#       #probabilities
#       p <- p/colSums(p)
#       #numerical overflow
#       p <- (pmax(p, 0.00001))
#       p <- (pmin(p, 0.99999))
#       #index is sampled proportionally to softmaxed utility vector
#       ind<-sample(1:64, 1, prob=p)
#       #bind X-observations
#       X<-rbind(X, cbind(environ$x1[ind], environ$x2[ind]))
#       #bind y-observations
#       y<-rbind(y, as.matrix(environ$y[ind]))
#     }
#     # remove first randomly revealed reward value
#     y = y[-1]
#     
#     cur = data.frame(lambda=lambda, beta=beta, tau=tau, mu=mean(y), replication=round)
#     dparams = rbind(dparams, cur)
#   }
# }
# 
# filename = paste0("modelResults/simulatedModels/batch1/simulatedModels_", clusterid, ".csv")
# write.csv(dparams, filename)
