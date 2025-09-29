#Simulation of models
# Anna Giron, 2021
# Björn Meder, 2025

#housekeeping
rm(list=ls())

#source of modeling code
source("models.R")

#packages
packages <- c('plyr', 'dplyr', 'jsonlite', 'lsr', 'BayesFactor', 'matrixcalc', 'furrr', 'data.table')
lapply(packages, require, character.only = TRUE) #loads packages



# 2. Multithreading DEAKTIVIEREN (NACH dem Laden der Pakete!)
Sys.setenv(
  "OMP_NUM_THREADS" = 1,      # OpenMP (z. B. für data.table, BayesFactor)
  "MKL_NUM_THREADS" = 1,      # Intel MKL (z. B. für matrixcalc)
  "OPENBLAS_NUM_THREADS" = 1, # OpenBLAS
  "GOTO_NUM_THREADS" = 1,      # GotoBLAS
  "VECLIB_MAXIMUM_THREADS" = 1 # macOS Accelerate Framework
)

# 3. Spezifische Paket-Optionen setzen (falls nötig)
# options(data.table.threads = 1)  # data.table explizit auf 1 Thread begrenzen


set.seed(0511)

##########################################################################################
# get environments --------------------------------------------------------
##########################################################################################

#extract environments
environments <- lapply(fromJSON("data/kernelRough.json"), FUN=function(x) matrix(as.numeric(unlist(x)), ncol=3, byrow=TRUE, dimnames=list(seq(1,64),  c('x2', 'y', 'x1'))))

# make df
env <- as.data.frame(environments[[1]])
for (i in 2:40){
  env<-rbind(env,as.data.frame(environments[[i]]))
}
env$en<-rep(1:40, each=64)

#Scaling reward range
env$y <- env$y*50

# simulation rounds for each parameter combination
replications <- 1000

#############################################################################################################################
# Import fitted parameters
#############################################################################################################################
# get corr-validated parameter estimates
#modelFit = read.csv('modelResults/modelFit.csv')

# modelFit = modelFit %>%
#   mutate(kernel=factor(kernel, levels=c('RBF', 'BMT'), labels=c('GP', 'BMT'))) 

# only use GP-UCB model
# modelFit = subset(modelFit, kernel=='GP' & acq=='UCB')


#############################################################################################################################
# Simulating performance for different parameter values
#############################################################################################################################

# fixed theoretical range
lambda_vals <- 0.5 # level of generalization (length-scale of RBF kernel), gfixed to 1.0 or 0.5

# bounds from parameter fitting
lower_bound <- exp(-5)  # ~0.0067
upper_bound <- exp(4)   # ~54.598

# parameter values for tau and beta
beta_vals <- exp(seq(log(lower_bound), log(upper_bound), length.out = 200))
tau_vals  <- exp(seq(log(lower_bound), log(upper_bound), length.out = 200))

# beta_vals   <- exp(seq(log(0.001), log(50), length.out = 100))
# tau_vals    <- exp(seq(log(0.001), log(20), length.out = 100))

params <- expand.grid(lambda = lambda_vals, beta = beta_vals, tau = tau_vals)

# make grid locations
Xstar <- cbind(env$x1[1:64], env$x2[1:64])
k<-rbf

###########################################################################
# parallelized version (via furrr package) --------------------------------
###########################################################################

# set up parallel plan
# plan(multisession, workers = parallel::detectCores() - 1)


# Anzahl der verfügbaren Cores ermitteln und  begrenzen
total_cores <- parallel::detectCores()

# maximal 50% der Cores,  nie mehr als 32
# max_cores <- min(floor(total_cores * 0.5), 32)

max_cores <- 90


# Parallelisierung
plan(multisession, workers = max_cores)


# run simulations in parallel
dparams <- future_pmap_dfr(params, function(lambda, beta, tau) {
  
  parVec <- c(lambda, lambda, 1, 1e-4)
  reps_out <- vector("list", replications)
  
  for (rep in seq_len(replications)) {
    enselect <- sample(1:40, 1)
    environ  <- subset(env, en == enselect)  # unchanged
    
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
    reps_out[[rep]] <- data.frame(lambda=lambda, beta=beta, tau=tau,
                                  mu=mean(y), replication=rep)
  }
  
  do.call(rbind, reps_out)
}, .progress = TRUE)  # progress bar


###########################################################################
# strictly sequential (loop) version --------------------------------------
###########################################################################
# 
# dparams <- data.frame(lambda = numeric(), beta = numeric(), tau = numeric(),
#                       mu = numeric(), replication = integer(), stringsAsFactors = FALSE)
# 
# for (i in seq_len(nrow(params))) {
# 
#   lambda <- params$lambda[i]
#   beta   <- params$beta[i]
#   tau    <- params$tau[i]
#   parVec <- c(lambda, lambda, 1, 1e-4)
# 
# 
#   for (rep in seq_len(replications)) {
#     enselect<-sample(1:40, 1)
#     environ  <- subset(env, en == enselect)
#     
#     message(paste(
#       "Param set", i, "/", nrow(params),
#       "lambda=", signif(lambda, 3),
#       "beta=", signif(beta, 3),
#       "tau=", signif(tau, 3),
#       "env=", enselect,
#       "rep", rep, "/", replications
#     ))
# 
#     ind <- sample(1:64, 1)
#     X   <- cbind(environ$x1[ind], environ$x2[ind])
#     y   <- as.matrix(environ$y[ind])
# 
#     for (trial in 1:25) {
#       out <- gpr(X.test = Xstar, theta = parVec, X = X, Y = (y - 25)/50, k = k)
#       utilityVec <- ucb(out, beta)
# 
#       utilities <- utilityVec - max(utilityVec)
#       p <- exp(utilities / tau)
#       p <- p / colSums(p)
#       p <- pmax(pmin(p, 0.99999), 1e-5)
# 
#       ind <- sample(1:64, 1, prob = p)
#       X   <- rbind(X, cbind(environ$x1[ind], environ$x2[ind]))
#       y   <- rbind(y, as.matrix(environ$y[ind]))
#     }
#     y <- y[-1]  # drop initial random observation
#     dparams <- rbind(dparams, data.frame(lambda=lambda, beta=beta, tau=tau,
#                                          mu=mean(y), replication=rep))
#   }
# }
# 


###########################################################################
# write results -----------------------------------------------------------
###########################################################################

if(lambda_vals == 1) {
  write.csv(dparams, "simulatedModels_local_lambda_1_0.csv", row.names = FALSE)
} else {
  write.csv(dparams, "simulatedModels_local_lambda_0_5.csv", row.names = FALSE)
}

##########################################################
# Original Code from Giron et al 2023 (optimized for cluster)
#########################################################


# parameter range based on Tukey's fence
# lambda = tukeysFence(log(modelFit$lambda))
# beta = tukeysFence(log(modelFit$beta))
# tau = tukeysFence(log(modelFit$tau))
# 

# Use log-ranges based on Tukey fence
# lambda_range <- tukeysFence(log(modelFit$lambda))
# beta_range   <- tukeysFence(log(modelFit$beta))
# tau_range    <- tukeysFence(log(modelFit$tau)); tau_range[1] <- -5  # enforce lower bound

# # lower bound for tau below range defined for model fitting - set to lower bound
# tau[1] = -5

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

