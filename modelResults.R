# Plot model results
# Anna Giron, Simon Ciranka, Charley Wu, 2022

# house keeping
rm(list=ls())


# load packages
packages <- c('plyr', 'jsonlite', 'gridExtra', 'reshape2',  'cowplot', 'lme4', 'sjPlot',
              "grid", 'corrplot', 'ggbeeswarm', 'tidyverse', 'viridis', 'colorspace', 'ggrepel', 'tidybayes',
              'brms')
lapply(packages, require, character.only = TRUE)

source("dataProcessing_gridSearchParkinson.R")
source('statisticalTests.R')

dropLeadingZero <- function(l){
  str_replace(l, '0(?=.)', '')
}


modelPal <- c('black', '#6c7195', '#ea9d67', '#7ec3aa')
paramPal = c("#FFEE67", '#27AD88', "#D1495B")

data = read.csv('data/data_gridsearch_Parkinson.csv')
#############################################################################################################################
# Import model fit results comparison.
#############################################################################################################################
#compile  model results using function from dataProcessing_gridSearchParkinson.R
#modelResults <- importModelResults('modelResults/batch1/',c('RBF', 'BMT') , c("GM", "GV", 'UCB')) #Rerun this for new model results
#write.table(modelResults[[1]], 'data/modelFit.csv',  sep=",", row.names = F)
#write.table(modelResults[[2]], 'data/params.csv',  sep=",", row.names = F)

modelFits <- read.csv('data/modelFit.csv')
groupDF <- data %>% filter(round==1 & trial==0) #Get one row per participant
modelFits <- merge(modelFits, groupDF[,c('id', 'group', 'condition')], by = "id") #merge to add group and condition data

modelFits = modelFits %>%
  mutate(kernel=factor(kernel, levels=c('RBF', 'BMT'), labels=c('GP', 'BMT'))) %>%
  mutate(group=factor(group))
modelFits$ModelName = paste(modelFits$kernel, modelFits$acq, sep="-")
modelFits$ModelName = factor(modelFits$ModelName, levels = c('GP-UCB', 'BMT-UCB', 'GP-GM', 'BMT-GM', 'GP-GV', 'BMT-GV'))
modelFits$acq <- factor(modelFits$acq, levels = c('UCB', 'GM', 'GV'))

#Only include key comparisons
modelFits <- subset(modelFits, ModelName %in% c('GP-UCB', 'GP-GM', 'BMT-UCB'))

#Two line name for models
modelFits$shortname <- factor(modelFits$ModelName, levels = c('GP-UCB','BMT-UCB', 'GP-GM'))
levels(modelFits$shortname) <- c('GP\nUCB', 'lambda\nlesion', 'beta\nlesion')


p_R2_comp <- ggplot(modelFits, aes(x=shortname, y=R2, fill=NA,color=shortname)) +
  #geom_line(aes(group=id), color = 'grey', alpha  = 0.3)+
  geom_quasirandom( size = 0.5)+
  geom_boxplot(width = 0.4, color ='black', outlier.shape=NA, fill = NA)+
  stat_summary(fun.y = mean, geom='point', shape = 23, color = 'black', fill = 'white')+
  xlab('') +
  ylab(expression(R^2)) +
  #ylab(expression(italic(pxp))) +
  scale_color_manual(values=modelPal, name = 'Model', labels = expression('GP-UCB', lambda*' lesion', beta* ' lesion')) +
  scale_fill_manual(values=modelPal, name = 'Model', labels = expression('GP-UCB', lambda*' lesion', beta* ' lesion')) +
  facet_wrap(~group, nrow = 1)+
  ggtitle('Model fits') +
  theme_classic() +
  theme(strip.background=element_blank(),
        legend.position = 'none', legend.justification = c(1,0), axis.title.x=element_blank())

p_R2_comp


ggsave(filename = 'plots/modelFitsR2.pdf',p_R2_comp )


#############################################################################################################################
# Parameters
#############################################################################################################################

paramPlotDF <- modelFits %>% filter(kernel=='GP' & acq == 'UCB') %>% pivot_longer(c('lambda', 'beta', 'tau'), names_to = 'param', values_to = 'estimate')
paramPlotDF$param <- factor(paramPlotDF$param, levels = c('lambda', 'beta', 'tau'))

paramPlot <- ggplot(paramPlotDF, aes(x = group, y = estimate, color = group))+
  geom_quasirandom( size = 0.5)+
  geom_boxplot(width = 0.4, color ='black', outlier.shape=NA, fill = NA)+
  stat_summary(fun.y = mean, geom='point', shape = 23, color = 'black', fill = 'white')+
  scale_color_manual(values=modelPal, name = 'Model') +
  scale_fill_manual(values=modelPal, name = 'Model') +
  facet_wrap(~param, nrow = 1)+
  theme_classic() +
   scale_y_log10() +
  theme(strip.background=element_blank(),
        legend.position = 'none', legend.justification = c(1,0), axis.title.x=element_blank())

paramPlot 

#############################################################################################################################
# PXP #computed in PXP.ipynb
#############################################################################################################################
#Save model nLLs for running pxp
# #all data
# nLLs <- params %>% arrange(shortname) %>% pull(nLL) %>% matrix(ncol = 4)
# write.table(nLLs, file = 'modelResults/pxp/nLL.csv', sep=',', row.names=F,col.names = F)
# #each age group separately
# nLLs <- subset(params, agegroup == "5-6") %>% arrange(shortname) %>% pull(nLL) %>% matrix(ncol = 4)
# write.table(nLLs, file = 'modelResults/pxp/nLL5-6.csv', sep=',', row.names=F,col.names = F)
# 
# nLLs <- subset(params, agegroup == "7-8") %>% arrange(shortname) %>% pull(nLL) %>% matrix(ncol = 4)
# write.table(nLLs, file = 'modelResults/pxp/nLL7-8.csv', sep=',', row.names=F,col.names = F)
# 
# nLLs <- subset(params, agegroup == "9-10") %>% arrange(shortname) %>% pull(nLL) %>% matrix(ncol = 4)
# write.table(nLLs, file = 'modelResults/pxp/nLL9-10.csv', sep=',', row.names=F,col.names = F)
# 
# nLLs <- subset(params, agegroup == "11-13") %>% arrange(shortname) %>% pull(nLL) %>% matrix(ncol = 4)
# write.table(nLLs, file = 'modelResults/pxp/nLL11-13.csv', sep=',', row.names=F,col.names = F)
# 
# nLLs <- subset(params, agegroup == "14-17") %>% arrange(shortname) %>% pull(nLL) %>% matrix(ncol = 4)
# write.table(nLLs, file = 'modelResults/pxp/nLL14-17.csv', sep=',', row.names=F,col.names = F)
# 
# nLLs <- subset(params, agegroup == "18-24") %>% arrange(shortname) %>% pull(nLL) %>% matrix(ncol = 4)
# write.table(nLLs, file = 'modelResults/pxp/nLL18-24.csv', sep=',', row.names=F,col.names = F)
# 
# nLLs <- subset(params, agegroup == "25-55") %>% arrange(shortname) %>% pull(nLL) %>% matrix(ncol = 4)
# write.table(nLLs, file = 'modelResults/pxp/nLL25-55.csv', sep=',', row.names=F,col.names = F)


# protected exceedance probability 
pxpAll = read.csv('modelResults/pxp/PXP.csv', header=F)
pxp1 = read.csv('modelResults/pxp/PXP5-6.csv', header=F)
pxp2 = read.csv('modelResults/pxp/PXP7-8.csv', header=F)
pxp3 = read.csv('modelResults/pxp/PXP9-10.csv', header=F)
pxp4 = read.csv('modelResults/pxp/PXP11-13.csv', header=F)
pxp5 = read.csv('modelResults/pxp/PXP14-17.csv', header=F)
pxp6 = read.csv('modelResults/pxp/PXP18-24.csv', header=F)
pxp7 = read.csv('modelResults/pxp/PXP25-55.csv', header=F)

# combine
pxp = rbind(pxpAll, pxp1, pxp2, pxp3, pxp4, pxp5, pxp6, pxp7)
colnames(pxp) = c('GP-\nUCB', 'lambda\nlesion', 'beta\nlesion', 'tau\nlesion')
pxp$agegroup = rev(c('25-55', '18-24', '14-17', '11-13', '9-10', '7-8', '5-6', 'Overall'))
pxp$agegroup = factor(pxp$agegroup, levels=rev(c('Overall', '25-55', '18-24', '14-17', '11-13', '9-10', '7-8', '5-6')))
pxp = gather(pxp, ModelName, pxp, `GP-\nUCB`:`tau\nlesion`)
pxp$ModelName <- factor(pxp$ModelName, levels =  c('GP-\nUCB', 'lambda\nlesion', 'beta\nlesion', 'tau\nlesion') )

xlabels <- expression('GP-UCB', lambda * " lesion", beta * " lesion", tau * " lesion")
p_pxp_all = ggplot(pxp, aes(x=ModelName, y=pxp, fill=agegroup)) +
  geom_bar(stat='identity', position="dodge2", color = 'black') +
  xlab('') +
  ylab(expression(italic(pxp))) +
  scale_fill_manual(values=rev(c('black',"#440154FF", "#443A83FF", "#31688EFF" ,"#21908CFF", "#35B779FF", "#8FD744FF", "#FDE725FF")), name = '') +
  scale_x_discrete(labels =xlabels)+
  ggtitle("Bayesian Model Selection") +
  theme_classic() +
  theme(legend.position= 'right',strip.background=element_blank(), 
  )

p_pxp_all




#############################################################################################################################
# Learning curves
#############################################################################################################################

# Simulated learning curves
path = 'rationalModels/'
filenames = list.files(path=path, pattern='*.csv')
filenames = paste0(path, filenames)

rationalDF = ldply(filenames, read.csv)
rationalDF = rationalDF[,!names(rationalDF) %in% 'X']

# normalize reward
rationalDF$meanReward = rationalDF$meanReward / 50
rationalDF$meanSE = rationalDF$meanSE / 50

nagegroups = length(levels(params$agegroup))
ntrials = 26

#add human data
behavior = read.csv('data/behavioralData.csv')

# only smooth condition
behavior = subset(behavior, condition=='Smooth')

behavior = behavior %>%
  mutate(type_choice=factor(type_choice, levels=c('Repeat', 'Near', 'Far'))) %>%
  mutate(experiment=factor(experiment, levels=c('Meder (2021)', 'Schulz (2019)', 'Adolescent'))) %>%
  mutate(agegroup=factor(agegroup,
                         levels=rev(c('[25,55]', '[18,25)', '[14,18)', '[11,14)', '[9,11)', '[7,9)', '[5,7)')),
                         labels=rev(c('25-55', '18-24', '14-17', '11-13', '9-10', '7-8', '5-6'))))

# append mean reward to model results
meanRew = ddply(behavior, ~id, plyr::summarize, meanReward=mean(z))


# random model performance should be displayed in all facets
random = subset(rationalDF, model=='Random')

randomModel = data.frame(trial=rep(random$trial, times=nagegroups),
                         meanReward=rep(random$meanReward, times=nagegroups),
                         meanSE=rep(random$meanSE, times=nagegroups),
                         agegroup=rep(levels(behavior$agegroup), each=ntrials),
                         model=rep('Random', times=nagegroups*ntrials))

rationalDF = subset(rationalDF, model!='Random')


# include human data
# normalize reward
behavior$z = behavior$z / 50
dplot = ddply(behavior, ~agegroup+trial, plyr::summarize, meanReward=mean(z), meanSE=sd(z)/sqrt(length(z)))
dplot$model = 'Human'

# combine datasets
rationalDF = rbind(rationalDF, randomModel, dplot)

# reorder factor levels, random model has no agegroup
rationalDF$agegroup = factor(rationalDF$agegroup, levels=c('5-6', '7-8', '9-10', '11-13', '14-17', '18-24', '25-55'),
                             labels=c('5-6', '7-8', '9-10', '11-13', '14-17', '18-24', '25-55'))
rationalDF$model = factor(rationalDF$model, levels=c('Human', 'GP-UCB', 'BMT-UCB',"GP-EG","GP-GM", 'Random'))

# plot
LCmodelPal <- c('grey','black', '#6c7195', '#ea9d67', '#7ec3aa','red')


p_LearningCurves = ggplot(rationalDF, aes(x=trial, y=meanReward, color=model, fill=model)) +
  geom_line() +
  geom_ribbon(aes(ymin=meanReward-meanSE, ymax=meanReward+meanSE), alpha=.3, color=NA) +
  facet_wrap(~agegroup, ncol=4) +
  xlab('Trial')+
  ylab('Normalized Mean Reward ± SEM')+
  ggtitle('Learning Curves') +
  labs(color='') +
  labs(fill='') +
  scale_fill_manual(values = c("grey",modelPal,"red"), breaks=c('Human', 'GP-UCB', 'BMT-UCB',"GP-GM","GP-EG", 'Random'), name='', labels = expression('Human','GP-UCB', lambda*' lesion', beta* ' lesion', tau*' lesion','random')) +
  scale_color_manual(values =c("grey",modelPal,"red"), breaks=c('Human', 'GP-UCB', 'BMT-UCB',"GP-GM","GP-EG", 'Random'),name='', labels = expression('Human','GP-UCB', lambda*' lesion', beta* ' lesion', tau*' lesion','random')) +
  theme_classic() +
  theme( strip.background=element_blank(),legend.position=c(.87,.2))

p_LearningCurves



#########################################################
#combine plots
#########################################################
# plots = cowplot::plot_grid(p_r2_ucb, p_pxp_all, p_LearningCurves, ncol=3)
# ggsave(filename = "plots/modelResults.pdf", plot=plots, height=6, width=14, units = "in")


# insetted<-ggdraw(p_pxp_main+ theme(axis.text.x=element_text(angle=0, hjust=0.5)) + ggtitle('Model comparison')) +
#   draw_plot(p_R2_comp+
#               ggtitle('')+
#               theme(text = element_text(size = 10),
#                     plot.background = element_rect(fill = "transparent",colour = NA)), .35, .2, .65, .8)
# insetted

p_ab <- cowplot::plot_grid(p_pxp_all, p_r2_age,labels = c('b','c'), ncol = 1)
plotsTop = cowplot::plot_grid(p_ab,p_LearningCurves, ncol=2, rel_widths = c(1, 1.2), labels = c('', 'd'))
plotsTop





##########################################
#Parameter plots
##########################################

#Everything after this is only GP-UCB
params = subset(params, kernel=='GP' & acq=='UCB')%>%mutate(
  agegroup = fct_relevel(agegroup,rev(c('25-55', '18-24', '14-17', '11-13', '9-10', '7-8', '5-6')))
)

params_old<-params



#############################
#Create final plot
##########################

bottomRow <- cowplot::plot_grid(ParamPlot, insetSimilarity ,nrow = 1, rel_widths = c( 1,.7), labels=c('e','f'))

fullPlot <- cowplot::plot_grid(plotsTop, bottomRow, ncol = 1, rel_heights = c(1,.8))
fullPlot

ggsave('plots/models.pdf',fullPlot, width = 12, height = 7.5, units = 'in' )
