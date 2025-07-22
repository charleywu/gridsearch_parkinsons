# compute the mean reward for each subject, round
# assuming choosing randomly among the options on each time step (uniform distribution)

##########################################
# house keeping --------------------------
##########################################
# rm(list=ls())

#packages
# packages <- c('grid', 'gridExtra', 'BayesFactor', 'tidyverse', "RColorBrewer") 
# lapply(packages, require, character.only = TRUE)

library('jsonlite')
library('tidyverse')

getRandomPerformance <- function(){
  #subject data
  dataFile ="data/YKWG.json"
  
  #read in json
  myjson    <-fromJSON(dataFile)
  ageMonths <- read.csv('data/ageMonths.csv')
  
  # get environments
  roughEnvironments   <- lapply(fromJSON("data/YKWG kernels/kernelRough.json"), FUN=function(x) matrix(as.numeric(unlist(x)), ncol=3, byrow=TRUE, dimnames=list(seq(1,64), c('y', 'z', 'x'))))
  smoothEnvironments  <- lapply(fromJSON("data/YKWG kernels/kernelSmooth.json"), FUN=function(x) matrix(as.numeric(unlist(x)), ncol=3, byrow=TRUE, dimnames=list(seq(1,64),  c('y', 'z', 'x'))))
  
  
  # df for bonus round data  
  # see tester notes
  exclusionList  <- c('nocode', 4,12,39,49,58,59,60,69,74,75,77,79,102,104,108) 
  
  df_env <- data.frame()
  
  #loop through json to create data frame
  for (i in 1:nrow(myjson)){
    
    subd <- myjson[i,]
    
    if (!subd$participant$code %in% exclusionList){ #remove no code data
      print(paste0("id: ", subd$participant$code))
      
      # get subject data
      df_subject <- data.frame(id = as.numeric(rep(subd$participant$code, 6)),
                               age_years = as.numeric(rep(subd$data$age, 6)),
                               age_months = rep(ageMonths[ageMonths$id==subd$participant$code, 'age_months'], 6),
                               gender = rep(subd$data$gender, 6),
                               condition = rep(subd$data$condition, 6),
                               environment = rep(if_else(subd$data$condition==1, "Rough", "Smooth"), 6),
                               round = 1:6,
                               env_order = unlist(subd$data$envOrder),
                               scale_factor = unlist(subd$data$scale),
                               mean_zRaw = NA,
                               mean_z = NA)
      
      #loop over environments 
      for(j in 1:length(df_subject$env_order)){
        # get current environment 
        current_env_number <- df_subject$env_order[j]
        
        if(df_subject$environment[1] == "Rough"){
          current_env <- data.frame(eval(parse(text=paste0("roughEnvironments$`", df_subject$env_order[j], "`"))))
          
        }else{
          current_env <- data.frame(eval(parse(text=paste0("smoothEnvironments$`", df_subject$env_order[j], "`"))))
        }
        
        # compute average mean (unscaled) reward value on grid 
        df_subject$mean_zRaw[j] <- mean(current_env$z)
        
        # compute true (observed) values on grid via given scaling factor
        df_subject$mean_z[j]  <- mean(current_env$z * df_subject$scale_factor[j] + 5)
      }
      
      #put together
      if(nrow(df_env)==0 ){
        df_env <- df_subject
      }else{
        df_env <- rbind(df_env, df_subject)
      }
    }
  }
  
  #create age group
  df_env$agegroup <- factor(ifelse(df_env$age_months<84, "Younger", "Older"), levels = c("Younger", "Older"))
  
  #environment
  df_env$environment <- factor(ifelse(df_env$cond==1, "Rough", "Smooth"), levels = c('Rough', 'Smooth'))
  
  # df for random performance across all rounds, learning round (2-4, excluding practice and bonus round), and bonus round only
  df_rand_all <- df_env %>% 
    group_by(environment) %>% 
    summarise(zRaw_all_envs = mean(mean_zRaw),
              z_all_envs = mean(mean_z))
  
  # df for random performance in learning roudns (rounds 2-4)
  df_rand_learn <- df_env %>% 
    filter(round %in% 2:5) %>% 
    group_by(environment) %>% 
    summarise(zRaw_learn_envs = mean(mean_zRaw),
              z_learn_envs = mean(mean_z))
  
  # df for random performance in bonus roudns (round 6)
  df_rand_bonus <- df_env %>% 
    filter(round == 6) %>% 
    group_by(environment) %>% 
    summarise(zRaw_bonus_env = mean(mean_zRaw),
              z_bonus_env = mean(mean_z))
  
  df_rand_performance <- left_join(df_rand_all,df_rand_learn, by = 'environment') %>% left_join(df_rand_bonus, by = 'environment')
  
  #write_csv2(df_rand_performance, "data/rand_performance.csv")
  return(df_rand_performance)
  
}