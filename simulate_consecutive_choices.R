# simulate consecutive search distances for learner choosing randomly on each step
# each learner chooses with uniform probability among the 8x8=64 options
# first tile (option) chosen randomly, then 25 random choices by the learner
# analyzed is the consecutive distance among choices

set.seed(0511)  

# function to simulate a random learner's path
simulate_learner <- function(n_trial = 26, grid_size = 8) {
  # random sequence of choices (positions on 8x8 grid)
  x <- sample(1:grid_size, n_trial, replace = TRUE)
  y <- sample(1:grid_size, n_trial, replace = TRUE)
  
  # trial index: 0 is randomly revelead trial, then 25 trials by learner
  trial <- 0:(n_trial - 1)
  
  # data frame
  path <- data.frame(trial = trial, x = x, y = y)
  
  # Manhattan distance between consecutive choices
  path$distance <- c(NA, abs(diff(path$x)) + abs(diff(path$y)))
  
  return(path)
}

# number of learners
n_learners <- 10^4
simulations <- lapply(1:n_learners, function(i) simulate_learner())

# bind results
simulations_df <- do.call(rbind, lapply(seq_along(simulations), function(i) {
  cbind(learner = i, simulations[[i]])
}))


# classify consecutive search decisions
simulations_df <- simulations_df %>% 
  filter(trial > 0) %>% # omit first trial
  mutate(
    type_choice = case_when(
      distance == 0 ~ "repeat",
      distance == 1 ~ "near",
      distance > 1  ~ "far",
      TRUE ~ NA_character_
    ),
    type_choice = factor(type_choice, levels = c("repeat", "near", "far"))
  )

# types of choices by 
df_types_choices_simulated <- 
  simulations_df %>% 
  filter(trial > 0) %>% 
  group_by(learner,type_choice) %>% 
  summarise(n = n()) %>% 
  complete(type_choice, fill = list(n = 0)) %>% # turn implicit missing values into explicit missing values
  group_by(learner) %>%
  mutate(prop = n / sum(n)) 

df_types_choices_simulated_overall <- df_types_choices_simulated %>% 
  group_by(type_choice) %>% 
  summarise(n = n(),
            mean_prop = mean(prop),
            SD_prop = sd(prop),
            se_prop = SD_prop / sqrt(n),
            lower_ci_prop = mean_prop - qt(1 - (0.05 / 2), n - 1) * se_prop,
            upper_ci_prop = mean_prop + qt(1 - (0.05 / 2), n - 1) * se_prop)

# proportion of search decision types
p_types_choices_simulated_overall <- 
ggplot(df_types_choices_simulated_overall) +
  geom_bar(aes(x = type_choice, y = mean_prop,  alpha = type_choice), fill = "steelblue", stat = "identity", colour = 'black', width = .9, position=position_dodge2(padding=0.1)) +
  scale_y_continuous("Average proportion", limits = c(0,1), breaks = seq(0,1,.2), expand = c(0, 0), labels =   scales::percent_format(accuracy=1)) +
  scale_alpha_discrete(range = c(0.2,1)) +
  scale_x_discrete("Type of search decision") +
  # scale_fill_manual(values=groupcolors) +
  # scale_color_manual(values=groupcolors) +
  ggtitle("Consecutive search decisions") +
  geom_text(aes(x = type_choice, y = mean_prop, label = scales::percent(mean_prop, accuracy = 1)), vjust = -0.5, size = 6) + 
  theme_classic() +
  theme(#aspect.ratio = 1,
    plot.title = element_text(size=24),
    legend.title = element_blank(),
    legend.position = 'none',
    legend.text =  element_text(colour="black"),
    text = element_text(colour = "black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 18),
    axis.text = element_text(colour="black", size=18),
    axis.title = element_text(colour="black", size=18),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())  

# exploration (consecutive distance > 0) vs. exploitation (consecutive distance == 0) decisions 
df_props_simulated <- 
simulations_df %>%
  filter(trial > 0) %>%
  group_by(distance) %>%
  summarise(count = n()) %>% 
  mutate(prop = count / sum(count)) %>%
  mutate(group = "Random learner") 

mean(simulations_df$distance, na.rm = T)

# p_distance_consecutive_simulated  <- 
  ggplot(df_props_simulated, aes(x = distance, y = prop)) +
    geom_col(
      width    = 1,
      fill   = "steelblue",
      colour = "black",
      alpha = 0.7
    ) +
    geom_col(
      data     = df_props_simulated %>% filter(distance == 0),
      aes(x     = distance, y = prop),
      fill   = "steelblue",
      color    = "black",
      width    = 1
    ) +
    scale_y_continuous(
      "Average proportion",
      limits = c(0, 0.7),
      breaks = seq(0, 0.6, 0.2),
      expand = c(0, 0),
      labels = percent_format(accuracy = 1)
    ) +
    scale_x_continuous("Distance") +
    # geom_vline(xintercept = 0.5, linetype = "dashed") +
    # geom_text(data = df_mean_distance, aes(x = Inf, y = Inf, label = paste0("Mean: ", round(mean_distance, 1))), # add mean distanecs
    #           hjust = 1.4, vjust = 1.5, size = 6, inherit.aes = FALSE) +
    ggtitle("Distance consecutive choices") +
    theme_classic() +
    theme(plot.title = element_text(size = 24),
          legend.title = element_blank(),
          legend.position = 'none',
          legend.text =  element_text(colour="black"),
          text = element_text(colour = "black"),
          strip.background =element_blank(),
          strip.text = element_text(colour="black", size=18),
          axis.text = element_text(colour="black", size=18),
          axis.title = element_text(colour="black", size=18),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(0.5, "cm"),
          plot.margin     = margin(r = 20))
  
  p_distance_consecutive
  
  
  