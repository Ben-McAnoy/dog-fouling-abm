
source("main-loop.R") # runs the sim

# ---- Outputs ----

# Print summary
print(glimpse(daily_summary))
print(tail(daily_summary, 10))

# Final heatmap of visible faeces frequency & contamination
# Create dataframe for plotting frequency of visible faeces by cell
visible_counts <- faeces %>%
  group_by(x,y) %>%
  summarise(visible = n(), .groups = "drop")

# contamination layer to dataframe
cont_df <- as_tibble(which(contamination_layer, arr.ind = TRUE)) %>%
  rename(x = row, y = col) %>%
  mutate(contaminated = TRUE)

# for plotting a heatmap of historical contamination counts, compute how many times contaminated (binary so it's 1)
cont_plot_df <- expand.grid(x = 1:GRID_SIZE, y = 1:GRID_SIZE) %>%
  as_tibble() %>%
  left_join(cont_df, by = c("x","y")) %>%
  mutate(contaminated = if_else(is.na(contaminated), FALSE, contaminated))

# visible heatmap
if(plot_final_heatmaps) {
  p1 <- cont_plot_df %>%
    ggplot(aes(x = x, y = y, fill = contaminated)) +
    geom_raster() +
    scale_fill_manual(values = c("white", "red")) +
    coord_fixed() +
    labs(title = "Persistent contamination (cells that have ever received infected faeces)") +
    theme_minimal() +
    theme(axis.text = element_blank(), axis.title = element_blank())
  print(p1)
  
  if(nrow(visible_counts) > 0) {
    p2 <- visible_counts %>%
      ggplot(aes(x = x, y = y, fill = visible)) +
      geom_raster() +
      coord_fixed() +
      labs(title = "Visible faeces counts (present at simulation end)") +
      theme_minimal() +
      theme(axis.text = element_blank(), axis.title = element_blank())
    print(p2)
  } else {
    message("No visible faeces at end of simulation to plot.")
  }
}

# Example summary plots: daily counts
p3 <- daily_summary %>%
  pivot_longer(cols = c(total_deposited, total_removed, total_visible), names_to = "metric") %>%
  ggplot(aes(x = day, y = value, color = metric)) +
  geom_line() +
  labs(title = "Daily fouling/removal/visible counts", y = "count") +
  theme_minimal()
print(p3)

p4 <- daily_summary %>%
  ggplot(aes(x = day, y = contaminated_cells)) +
  geom_line() +
  labs(title = "Number of contaminated cells over time", y = "contaminated cells") +
  theme_minimal()
print(p4)

# Save results to list for later analysis
results <- list(
  params = list(GRID_SIZE = GRID_SIZE, SIM_DAYS = SIM_DAYS, N_daily = N_daily,
                prop_infected = prop_infected, degradation_time = degradation_time),
  faeces = faeces,
  contamination_layer = contamination_layer,
  daily_summary = daily_summary,
  park = list(entrances = entrances, paths = path_cells, bins = bins)
)

# Optionally save to disk
saveRDS(results, file = "dog_fouling_abm_results.rds")
message("Simulation complete. Results stored in `results` object.")

