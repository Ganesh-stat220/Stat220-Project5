schools <- read_csv("reference_schools.csv")
stores <- read_csv("school_nearby_liquor_stores.csv")

school_data <- schools %>%
  left_join(
    stores %>%
      group_by(latitude, longitude) %>%
      summarise(liquor_stores = n()),
    by = c("latitude", "longitude")
  ) %>%
  mutate(
    liquor_stores = replace_na(liquor_stores, 0),
    short_name = str_sub(org_name, 1, 40) 
  ) %>%
  select(short_name, urban_rural_indicator, liquor_stores)


ggplot(school_data, 
       aes(x = reorder(short_name, liquor_stores), 
           y = liquor_stores,
           fill = urban_rural_indicator)) +
  geom_col(width = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, max(school_data$liquor_stores), by = 2)) +
  labs(
    title = "Liquor Stores Near Schools",
    subtitle = "Grouped by urban/rural classification",
    x = "School (first 15 characters)",
    y = "Number of Liquor Stores Within 1km",
    fill = "Area Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    panel.background = element_rect(fill = "white"),          # Main plot area
    plot.background = element_rect(fill = "white", color = NA) 
  )

ggsave("my_viz.png", width = 10, height = 8, dpi = 300, bg = "white")  