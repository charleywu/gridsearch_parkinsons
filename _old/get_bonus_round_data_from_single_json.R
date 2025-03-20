# function for extracting bonus round data

getBonusRound <- function(){
  dataFile ="data/all_data_gridsearch_parkinson.json"
  
  #read in json
  myjson    <-fromJSON(dataFile)
  
  # get environments
  smoothEnvironments  <- lapply(fromJSON("data/kernelSmooth.json"), FUN=function(x) matrix(as.numeric(unlist(x)), ncol=3, byrow=TRUE, dimnames=list(seq(1,64),  c('y', 'z', 'x'))))
  
  
  
  # df for bonus round data  
  df_bonus_round <- data.frame()
  
  #loop through json to create data frame
  for (i in 1:nrow(myjson)){
    
    subd <- myjson[i,]
    
    # get subject data
    df_subject <- data.frame(id = as.numeric(rep(subd$participantId, 5)),
                             bonus_env_number = unlist(subd$envOrder)[length(unlist(subd$envOrder))]) %>% 
      mutate(bonus_environment = "Smooth")
    
    #get bonus round environment of current subject
    subject_bonus_env_number <- df_subject$bonus_env_number[1]
    
    bonus_environment <- data.frame(eval(parse(text=paste0("smoothEnvironments$`", subject_bonus_env_number, "`"))))
    
    #bonus_environment <- data.frame(Reduce(rbind, bonus_environment))
    # somehow the column labels get screwed up when extracting the matrix from the list
    # x1 should be x2; x2 should be y, y should be X2 (see original .json file)
    #colnames(bonus_environment) <- c('x2', 'y', 'x1') # this is the json file nomenclature
    #colnames(bonus_environment) <- c('y', 'z', 'x') # this is the "standard" nomenclature
    
    # compute true (observed) values on grid
    scale_factor        <- unlist(subd$scale)[length(unlist(subd$scale))]
    bonus_environment$z <- bonus_environment$z * scale_factor + 5
    #bonus_environment$z <- (bonus_environment$z + 0.5) * 50
    
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
return(df_bonus_round)
}








