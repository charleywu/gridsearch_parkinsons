#data cleaning
#Björn Meder and Charley Wu 2025, based on a version by Simon Ciranka 2023

library('jsonlite')
library('plyr')
library('tidyverse')

# set working directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd("/Users/meder/Library/CloudStorage/OneDrive-HMUHealthandMedicalUniversityPotsdam/_Projekte/gridsearch Parkinson/analysis/")

#############################################################################################################################
# data import function to convert .json to .csv ---------------------------
#############################################################################################################################

dataImport_Park <- function(dataFile, writecsv = FALSE){
  #read in json
  myjson <- fromJSON(readLines(dataFile))
  all_opts = expand.grid(0:7, 0:7)
  # dat<-data.frame()
  
  envs = data.frame()
  subdata <- myjson
  #x-y-z
  x<-as.vector(t(subdata$searchHistory$xcollect))
  y<-as.vector(t(subdata$searchHistory$ycollect))
  z<-as.vector(t(subdata$searchHistory$zcollect))
  chosen <- apply(cbind(x,y),MARGIN=1, FUN=function(row) which(row[1]==all_opts$Var1 & row[2]==all_opts$Var2))
  zscaled <- as.vector(t(subdata$searchHistory$zcollectScaled))
  #Color value
  #time
  time<- as.vector(t(subdata$searchHistory$tscollect))
  #condition; no conditions
  #id
  id<-rep(as.numeric(subdata$participantId), length(x))
  #age
  session<-rep(as.numeric(subdata$session), length(x))
  #trial number
  trial<-rep(0:25, 10)
  #round number
  round<-rep(1:10, each=26)
  
  #dummy data frame
  dat<-data.frame(id=id, session=session, x=x, y=y, chosen=chosen, z=z, zscaled=zscaled, time=time, trial=trial, round=round)
  #bind them
  ############################################
  # save environment order
  id_env = rep(as.numeric(subdata$participantId), 10)
  round_env = 1:10
  envOrder = subdata$envOrder
  envs = data.frame(id=id_env, round=round_env, env=envOrder,session=subdata$session)
  
  
  
  ##########################################
  # compute distance between choices ------
  ##########################################
  dat <- dat %>% 
    arrange(id, round, trial) %>% # make sure rounds, trials etc are in the correct order
    mutate(distance = NA)
  
  # compute (Manhattan) distance between consecutive choices
  for(i in 1:(dim(dat)[1]-1)){
    dat$distance[i+1] <- dist(rbind(c(dat$x[i], dat$y[i]), c(dat$x[i+1], dat$y[i+1])), method = "manhattan")
  }
  
  # set distance for initial trial (=randomly revealed tile) to NA and classify distances
  dat <- dat %>% 
    mutate(distance = ifelse(trial == 0, NA, distance),
           type_choice = case_when(
             distance == 0 ~ "Repeat",
             distance == 1 ~ "Near",
             distance >1   ~ "Far"
             #is.na() ~ "nope",
           )) %>% 
    mutate(type_choice = factor(type_choice, levels = c('Repeat', 'Near', 'Far')))
  
  # distance as function of reward on previous trial ------------------------
  dat$previous_reward <- NA
  
  # add column with reward obtained on previous step
  for(i in 1:nrow(dat)){
    if(dat$trial[i] != 0) {
      dat$previous_reward[i] <-  dat$z[i-1]
    }
  }
  
  if (writecsv==TRUE){
    write.table(dat, file=paste0(dataFile, ".csv"), sep=",", row.names = F)
    write.table(envs, file=paste0(dataFile, ".Environments.csv"), sep=",", row.names = F)  
  }
  return(dat)
}


# get names of individual data files stored in .JSON format 
json_files <- list.files(path = "data/raw_data/all", pattern = "(?i)\\.json$", full.names = F )

# convert .JSON files in .csv files
# generates one file for behavioral data
list_of_dfs <- list()

for (i in seq_along(json_files)) {
  
  dataFile <- paste0("data/raw_data/all/", json_files[i])
  csv_file <- paste0(dataFile, ".csv")
  
  df <- dataImport_Park(dataFile) # get data
  
  list_of_dfs[[length(list_of_dfs) + 1]] <- df
  
  # check whether .csv file already exists
  if (!file.exists(csv_file)) {
    write.csv(df, csv_file, row.names = FALSE)
  }
}

results <- bind_rows(list_of_dfs)
write.csv(results, "data/data_gridsearch_Parkinson.csv", row.names = FALSE)



cohensd.ci <- function(d, n1, n2, ci = 0.95) {
  t <- d * sqrt((n1 * n2)/(n1 + n2))
  capture.output(
    fit <- compute.es::tes(t = t, n.1 = n1, n.2 = n2, level = 100 * ci),
    file = "NUL"
  )
  c(lower.ci = fit$l.d, upper.ci = fit$u.d)
}


#############################################################################################################################
# read bonus round data from json and write to .csv --------------------------------------------------
#############################################################################################################################

# create single json file
json_files <- list.files(path = "data/raw_data/all", pattern = "(?i)\\.json$", full.names = F )
# length(json_files)

# Create an empty list to hold the content of all JSON files
all_json_content <- list()

for (file in json_files) {
  json_content <- fromJSON(file.path("data/raw_data/all", file))
  all_json_content <- c(all_json_content, list(json_content))
}

# Write the combined content to a single JSON file as an array
write_json(all_json_content, "data/all_data_gridsearch_parkinson.json", pretty = TRUE)

# check number of participants
# extract number of participants
# all_ids <- sapply(all_json_content, function(x) x$participantId)
# length(unique(all_ids))

dataFile <- "data/all_data_gridsearch_parkinson.json"

#read in json
myjson    <-fromJSON(dataFile)

# get environments
roughEnvironments  <- lapply(fromJSON("data/kernelRough.json"), FUN=function(x) matrix(as.numeric(unlist(x)), ncol=3, byrow=TRUE, dimnames=list(seq(1,64),  c('y', 'z', 'x'))))

# df for bonus round data  
df_bonus_round <- data.frame()

#loop through json to create data frame
for (i in 1:nrow(myjson)){
  
  subd <- myjson[i,]
  
  # get subject data
  df_subject <- data.frame(id = as.numeric(rep(subd$participantId, 5)),
                           bonus_env_number = unlist(subd$envOrder)[length(unlist(subd$envOrder))]) %>% 
    mutate(bonus_environment = "Rough")
  
  #get bonus round environment of current subject
  subject_bonus_env_number <- df_subject$bonus_env_number[1]
  
  bonus_environment <- data.frame(eval(parse(text=paste0("roughEnvironments$`", subject_bonus_env_number, "`"))))
  
  #bonus_environment <- data.frame(Reduce(rbind, bonus_environment))
  # somehow the column labels get screwed up when extracting the matrix from the list
  # x1 should be x2; x2 should be y, y should be X2 (see original .json file)
  #colnames(bonus_environment) <- c('x2', 'y', 'x1') # this is the json file nomenclature
  #colnames(bonus_environment) <- c('y', 'z', 'x') # this is the "standard" nomenclature
  
  # compute true (observed) values on grid
  scale_factor        <- unlist(subd$scale)[length(unlist(subd$scale))]
  bonus_environment$z <- bonus_environment$z * scale_factor + 5
  
  # get bonus round data
  # reward estimates for five unseen tiles and subjective confidence of reward
  df_bonus          <- data.frame(subd$bonusLevel$bonusCells)
  
  # eventually chose tile
  df_bonus$chosen_x <- rep(unlist(subd$bonusLevel$finalChosenCell$x),5)
  df_bonus$chosen_y <- rep(unlist(subd$bonusLevel$finalChosenCell$y),5)
  
  # get true value of tiles in bonus round
  df_bonus$true_z <- NA
  for(i in 1:5){
    df_bonus$true_z[i] <- bonus_environment$z[bonus_environment$x == df_bonus$x[i] & bonus_environment$y == df_bonus$y[i]]
  }
  
  # bind
  df_current <- cbind(df_subject, df_bonus)
  
  # put together
  df_bonus_round <- rbind(df_bonus_round, df_current)
  
}

write.table(df_bonus_round, file="data/data_gridsearch_Parkinson_bonusround.csv", sep=",", row.names = F)

#############################################################################################################################
# Model Results
#############################################################################################################################
# imports and preprocesses model results 
importModelResults <- function(dataFolder, kernels, acqFuncs){
  
  # read data from individual .csv files
  # ("RBF", "BMT") \times (greedyMean, ucb, epsilonGreedy)
  # dataFolder <- "modelResults/batch2/"
  #kernels <- c("RBF", "BMT") # RBF = Radial Basis Function kernel, BMT= Bayesian Mean Tracker
  #acqFuncs <- c("GM", "UCB", "epsilonGreedy") # UCB = Upper Confidence Bound, GM=greedyMean, EG = epsilonGreedy
  # modelFit <- importModelResults(dataFolder, kernels, acqFuncs)
  # write.csv(modelFit[[1]], "modelResults/modelFit.csv", row.names = FALSE)
  # write.csv(modelFit[[2]], "modelResults/params.csv", row.names = FALSE)
  
  #Participant data
  #data<-dataImport()
  # import preprocessed data
  data = read.csv('data/data_gridsearch_Parkinson.csv')
  uids = unique(data$id)
  #length(uids)
  
  #initialize data frames
  modelFit <- data.frame() 
  paramEstimates <- data.frame()
  #loop through data
  for (k in kernels){
    for (a in acqFuncs){
      # for (i in 1:length(uids)){ #subjects
      for (i in uids){ #subjects
        filename <- paste0(dataFolder, k, a, i, ".csv") #read output file
        if (file.exists(filename)){
          dp<-read.csv(filename)
          #print(filename)
          #interpret parameters based on model combination
          if (k==""){#Heuristics
            colnames(dp) <- c("", "leaveoutindex", "nLL", "tau")
          }else if (k=="BMT" | k=="LBMT"){#Bayesian mean tracker
            ifelse(a=='UCB' | a=='Counts', colnames(dp)<- c("", "leaveoutindex", "nLL", "kError", "beta","tau"), colnames(dp)<- c("",  "leaveoutindex", "nLL","kError", "tau"))
          }else if (k=="LIN" | k=="LLIN"){ #linear kernel
            ifelse(a=='UCB', colnames(dp)<- c("",  "leaveoutindex", "nLL", "beta","tau"), colnames(dp)<- c("", "leaveoutindex", "nLL", "tau"))
          }else { #normal GP kernel
            if (a=='UCB' | a=='Counts') {
              colnames(dp)<- c("", "leaveoutindex", "nLL", "lambda", "beta","tau")
            } else if (a=='EG') {
              colnames(dp)<- c("", "leaveoutindex", "nLL", "lambda", "beta", "epsilon")
            } else {
              colnames(dp)<- c("", "leaveoutindex", "nLL", "lambda", "tau")
            }
          }
          
          rounds = length(dp$nLL)
          
          #demographics
          dummy <- subset(data, id==i) #subset participant in the dataframe
          #environment <- dummy$Condition[1]
          id <- dummy$id[1]  #replicate ID
          kernel <- k
          acq <- a
          #Total out of sample nLL
          nLL <- sum(dp$nLL)
          randomModelLL <- -log(1/64)*rounds*25
          R2 <- 1 - (nLL/randomModelLL)
          #blank median parameter estimates
          kErrorMed <- NA
          lambdaMed <- NA
          betaMed <- NA
          tauMed <- NA
          epsilonMed <- NA
          #blank mean parameter estimates
          kErrorMean <- NA
          lambdaMean <- NA
          betaMean <- NA
          tauMean <- NA
          epsilonMean <- NA
          #mean parameter estimates for UCB RBF
          if (a=="UCB"| a=='Counts' | a=='EG'){ #UCB has beta
            betaMed <- median(exp(dp$beta))
            betaMean <- mean(exp(dp$beta))
          }
          if (k=="RBF" | k=="LRBF"){
            lambdaMed <- median(exp(dp$lambda))
            lambdaMean <- mean(exp(dp$lambda))
          }
          if (k=="BMT" | k=="LBMT"){ #BMT
            kErrorMed <- median(exp(dp$kError))
            kErrorMean <- mean(exp(dp$kError))
          }
          if (a!='EG') {
            tauMed <- median(exp(dp$tau))
            tauMean <- mean(exp(dp$tau))
          }
          if (a=='EG') {
            epsilon <- 1/(1+exp(-(dp$epsilon)))
            epsilonMed <- median(epsilon)
            epsilonMean <- mean(epsilon)
          }
          #save modelFit
          dadd <- data.frame(id=id, nLL=nLL, kernel=kernel, acq=acq, R2=R2, kError=kErrorMean, lambda=lambdaMean, beta = betaMean, tau=tauMean, epsilon=epsilonMean)
          modelFit <-rbind(modelFit, dadd)
          #loop through leave out index to save all 9 parameter estimates for each subject
          for (loo in 2:(rounds+1)){
            subDP <- subset(dp, leaveoutindex == loo)
            roundnLL <- subDP$nLL
            #exponentiation of all parameters
            kError <- ifelse("kError" %in% colnames(subDP), exp(subDP$kError), NA)
            lambda <- ifelse("lambda" %in% colnames(subDP), exp(subDP$lambda), NA)
            beta <- ifelse("beta" %in% colnames(subDP), exp(subDP$beta),  NA)
            tau <- ifelse("tau" %in% colnames(subDP), exp(subDP$tau),  NA)
            epsilon <- ifelse("epsilon" %in% colnames(subDP), 1/(1+exp(-(dp$epsilon))), NA)
            dadd<-data.frame(id=id, leaveoutindex=loo, nLL=nLL, R2 =R2,  kernel=kernel, acq=acq, kError=kError, lambda=lambda, beta=beta, tau=tau, epsilon=epsilon, roundnLL=roundnLL)
            paramEstimates <-rbind(paramEstimates, dadd)
          }}}}}
  return(list(modelFit, paramEstimates))
}


