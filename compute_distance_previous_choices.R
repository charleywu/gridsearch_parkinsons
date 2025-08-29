
 # function to compute for each choice the (Manhattan) distance to all previous choices
 get_distances_loop <- function(df) {
   # Expect df to contain: id, round, group, x, y, z, trial, is_new_label
   df <- df %>% dplyr::arrange(id, round, trial)
   
   out <- vector("list", nrow(df))
   k <- 0L
   
   for (i in seq_len(nrow(df))) {
     current <- df[i, ]
     
     # restrict "previous" to the SAME subject and SAME round, earlier trials only
     prev <- df[
       df$id == current$id &
         df$round == current$round &
         df$trial < current$trial, ]
     
     if (nrow(prev) > 0L) {
       distances <- abs(current$x - prev$x) + abs(current$y - prev$y)
       
       tmp <- prev %>%
         dplyr::transmute(
           id,
           round,
           trial_prev = .data$trial,
           x_prev     = .data$x,
           y_prev     = .data$y,
           z_prev     = .data$z
         ) %>%
         dplyr::mutate(
           group         = current$group,
           trial         = current$trial,
           x             = current$x,
           y             = current$y,
           z             = current$z,
           is_new_label  = current$is_new_label,
           manhattan_dist = distances
         ) %>%
         dplyr::relocate(
           id, group, round, trial, x, y, z, is_new_label,
           trial_prev, x_prev, y_prev, z_prev, manhattan_dist
         )
       
       k <- k + 1L
       out[[k]] <- tmp
     }
   }
   
   if (k == 0L) dplyr::tibble() else dplyr::bind_rows(out[seq_len(k)])
 }

# 0) compute for each trial the Manhattan distance to all previous trials  
# yields n=trial rows per trial (e.g., for the 3. trial there are 3 distances, namely to trial 0, 1, and 2) 
closest_prev_exploration <- get_distances_loop(dat) 
 
filter(closest_prev_exploration, is.na(id))

# 1) Keep only minimum-distance previous trials (ties kept)
closest_prev_exploration_filtered <- closest_prev_exploration %>% 
  filter(is_new_label == "Exploration") %>% # only consider exploration trials
  group_by(id, group, round, trial) %>%
  slice_min(manhattan_dist, with_ties = TRUE, n = 1) %>%
  ungroup()

# 2) # average reward z *within the same closests tiles*, keeping onyl distinct tiles with same min distance
# yields one row per closest tile
minset_by_tile <- closest_prev_exploration_filtered %>%
  group_by(id, group, round, trial, x, y, is_new_label, x_prev, y_prev) %>%
  summarise(
    min_manhattan_dist = first(manhattan_dist),  # constant within min-distance set
    n_visits           = n(),                    # how often this tile was visited before
    z_prev_tile_mean   = mean(z_prev),           # mean reward over those visits
    tile_last_trial    = max(trial_prev),        # most recent visit to this tile
    .groups = "drop"
  )

# 3) aggregate across distinct tiles with same min distance (if multiple tiles tie)
# one row per (id, trial)
closest_prev_exploration_per_trial <- minset_by_tile %>%
  group_by(id, group, round, trial, x, y, is_new_label) %>%
  summarise(
    n_min_tiles             = n(),                          # number of tiles at min distance
    min_manhattan_dist      = first(min_manhattan_dist),    # still constant
    z_prev_mean_over_tiles  = mean(z_prev_tile_mean),       # tile-weighted mean
    # optional: keep details of the tied tiles
    tiles = list(tibble(
      x_prev, y_prev, n_visits, z_prev_tile_mean, tile_last_trial, min_manhattan_dist
    )),
    .groups = "drop"
  )

na_rows <- closest_prev_exploration_per_trial %>%
  filter(is.na(min_manhattan_dist))

filter(closest_prev_exploration, is.na(id))

lmer_distance_reward_closest <- lmer(min_manhattan_dist ~ z_prev_mean_over_tiles * group + (1 | id), 
                             data = closest_prev_exploration_per_trial)

#summary(lmer_distance_reward)
#emmeans(lmer_distance_reward, pairwise ~ previous_reward | group, pbkrtest.limit = 15000)

# p_lmer_distance_reward_closest <- 
  plot_model(lmer_distance_reward_closest, type = "pred", terms = c("z_prev_mean_over_tiles", "group")) +
  stat_summary(closest_prev_exploration_per_trial, mapping=aes(x=z_prev_mean_over_tiles, y=min_manhattan_dist, color=group, fill=group,shape = group), fun=mean, geom='point', alpha=0.7, size=1, na.rm = TRUE) +
  scale_x_continuous(
    name = "Normalized reward of closest previous choices",
    breaks = seq(0, 1, 0.1),
    labels = sprintf("%.1f", seq(0, 1, 0.1))) +
  scale_y_continuous(name = 'Distance to closest previous choice(s)', breaks = seq(0,5,1), labels = c(" 0"," 1"," 2"," 3"," 4"," 5")) +
  scale_fill_manual(values=groupcolors) +
  scale_color_manual(values=groupcolors) +
  coord_cartesian(ylim= c(0,5), xlim= c(-0.01,1)) +
  # ylab('Distance to Next Option')+
  # xlab('Distance to Next Option')+
  ggtitle('Distance to closest previous choice ~ reward of closest previous choice (lmer)') +
  theme_classic() +
    theme(legend.position = "inside", 
          legend.position.inside = c(0.85, 0.9),   
          legend.justification = c(1, 1),
          legend.title = element_blank(),
          legend.text = element_text(colour = "black", size = 18),
          plot.title = element_text(colour = "black", size = 24),
          plot.subtitle = element_text(colour = "black", size = 18),
          axis.text = element_text(colour = "black", size = 18),
          axis.title = element_text(colour = "black", size = 18)
    )        

#plot regression lines based on raw data
ggplot(closest_prev_exploration_per_trial, aes(y = min_manhattan_dist, x = z_prev_mean_over_tiles, color = group)) +
  # facet_wrap(~group) +
  # geom_jitter(alpha = 0.3, width = 0.05, height = 0.05) +
  # geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
  # scale_fill_manual(values=groupcolors) +
  # scale_color_manual(values=groupcolors) +
  # theme_minimal() +
  # xlab("Normalized reward of closest previous choice(s)") +
  # scale_y_continuous("Distance to closest previous choice", breaks = 1:15) +
  # theme_classic() +
  # theme(legend.position = "non", 
  #       plot.title = element_text(colour = "black", size = 24),
  #       plot.subtitle = element_text(colour = "black", size = 18),
  #       axis.text = element_text(colour = "black", size = 18),
  #       axis.title = element_text(colour = "black", size = 18)
  # )        






 