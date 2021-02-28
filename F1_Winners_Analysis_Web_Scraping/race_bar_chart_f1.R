

driversTable <- read_csv('cumulative_results.csv',col_names = TRUE)


gap <- driversTable %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = min_rank(-value) * 1,
         Value_rel = value/value[rank==1],
         Value_lbl = paste0(" ",value)) %>%
  filter(rank <=10) %>%
  arrange(value,winner) %>%
  ungroup() %>%
  group_by(year) %>%
  arrange(rank, winner) %>%
  mutate(rank = seq(1, n())) %>%
  ungroup() 


p <- ggplot(gap, aes(rank, group = winner, 
                     fill = as.factor(winner), color = as.factor(winner))) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(winner, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=value,label = Value_lbl, hjust=0),size = 7) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  
  labs(title='Total no of wins by F1 Drivers : {closest_state}', x = "", y = "Winners",
       caption = "Most No of wins by F1 Drivers 2015-2020") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,1,1,4, "cm")) +
  
  transition_states(year, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

animate(p, 200, fps = 15, duration = 13, width = 800, height = 600, renderer = gifski_renderer("gganim.gif"))


