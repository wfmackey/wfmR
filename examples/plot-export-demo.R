# Run with the current wfmR package installed.
# Files are saved relative to your working directory.
library(ggplot2)
library(dplyr)
library(wfmR)

output_dir <- "figures/wfmR-demo"

# Keep extra columns to show that only chart inputs reach the CSV.
cars <- mtcars |>
  mutate(cylinders = factor(cyl))

p <- ggplot(cars, aes(wt, mpg, colour = cylinders)) +
  geom_point(size = 2) +
  scale_colour_wfm() +
  labs(title = "Fuel economy", x = "Weight (1,000 lb)", y = "MPG") +
  theme_wfm()

# theme_wfm() uses Source Serif Pro, matching the bundled Quarto PDFs.
# The font must also be installed on the computer running this script.

# Save PDF, SVG and PNG with transparent backgrounds (background = NULL).
# Full width is 143.96 mm; half width is 71.98 mm.
full_files <- wfm_save_plot(
  p, path = file.path(output_dir, "cars-full"),
  width = "full", height = 90, units = "mm"
)

half_files <- wfm_save_plot(
  p, path = file.path(output_dir, "cars-half"),
  width = "half", height = 60, units = "mm", background = "yellow"
)

# Backgrounds accept HEX colours or wfmR palette names such as "yellow" above.
# A suffix selects one format.
# Numeric width and height work too, for other document layouts.
custom_files <- wfm_save_plot(
  p, path = file.path(output_dir, "cars-custom.png"),
  width = 110, height = 70, units = "mm", dpi = 320, background = "#FFFFFF"
)

# Export just the chart's source data: wt, mpg and cylinders.
csv_file <- wfm_export_plot_data(
  p, path = file.path(output_dir, "cars-data.csv")
)

chart_data <- read.csv(csv_file)
stopifnot(
  setequal(names(chart_data), c("wt", "mpg", "cylinders")),
  nrow(chart_data) == nrow(cars)
)

# Keep the plotting code alongside the CSV. Restore factor levels explicitly.
chart_data <- chart_data |>
  mutate(cylinders = factor(cylinders, levels = c(4, 6, 8)))

p_remade <- ggplot(chart_data, aes(wt, mpg, colour = cylinders)) +
  geom_point(size = 2) +
  scale_colour_wfm() +
  labs(title = "Fuel economy", x = "Weight (1,000 lb)", y = "MPG") +
  theme_wfm()

# Save all four formats with one call, using the chart remade from its CSV.
# save_all() ignores the supplied suffix: each file has the stem "cars-all".
all_files <- wfm_save_all(
  p_remade, path = file.path(output_dir, "cars-all.png"),
  width = "full", height = 90, units = "mm"
)

# Separate layer data are retained with layer and original row identifiers.
cylinder_means <- cars |>
  summarise(mean_mpg = mean(mpg), .by = cylinders)

p_layers <- ggplot(cars, aes(wt, mpg)) +
  geom_point(aes(colour = cylinders), size = 2, show.legend = FALSE) +
  geom_hline(
    data = cylinder_means, aes(yintercept = mean_mpg),
    linetype = "dashed", colour = "grey40"
  ) +
  facet_wrap(vars(cylinders), nrow = 1) +
  scale_colour_wfm() +
  labs(
    title = "Fuel economy by cylinders",
    subtitle = "Dashed lines show each group mean",
    x = "Weight (1,000 lb)", y = "MPG"
  ) +
  theme_wfm()

layer_files <- wfm_save_all(
  p_layers, path = file.path(output_dir, "cars-layers"),
  width = "full", height = 90, units = "mm"
)

layer_data <- read.csv(layer_files[["csv"]])
stopifnot(
  all(c(".wfm_layer", ".wfm_row") %in% names(layer_data)),
  sum(layer_data$.wfm_layer == 1) == nrow(cars),
  sum(layer_data$.wfm_layer == 2) == nrow(cylinder_means),
  !any(c("hp", "disp", "qsec") %in% names(layer_data))
)

# Layer 1 contains points; layer 2 contains the group means.
print(layer_data |> count(.wfm_layer))

exported_files <- c(full_files, half_files, custom_files, csv_file,
                    all_files, layer_files)
stopifnot(all(file.exists(exported_files)))
print(unname(exported_files))
