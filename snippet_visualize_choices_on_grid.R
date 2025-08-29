# plot all 25 choices on grid

id_to_plot <- unique(x$id)[1]

df_lab <- x %>%
  filter(id == id_to_plot) %>%
  arrange(trial) %>%
  group_by(x, y) %>%
  summarise(trials = paste(trial, collapse = ", "), .groups = "drop")

grid_8x8 <- expand.grid(x = 0:7, y = 0:7) %>%
  as_tibble() %>%
  left_join(df_lab, by = c("x","y"))

ggplot(grid_8x8, aes(x, y)) +
  geom_tile(fill = NA, color = "black") +
  geom_text(aes(label = trials), size = 3) +
  scale_x_continuous(breaks = 1:8, expand = c(0, 0)) +
  scale_y_continuous(breaks = 1:8, expand = c(0, 0)) +
  coord_equal() +
  labs(
    title = "Choices",
    subtitle = paste("ID:", id_to_plot),
    x = "x",
    y = "y"
  ) +
  theme_bw() +
  theme(panel.grid.major = element_blank())
