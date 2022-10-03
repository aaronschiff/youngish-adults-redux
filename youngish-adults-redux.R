# Visualise population changes for people aged 20-34 between 2001 and 2021
# Data source: Stats NZ subnational resident population estimates by SA2 area and 5-year age groups
# from NZ.Stat https://nzdotstat.stats.govt.nz/wbos/Index.aspx

# Map setup
map_centre <- c(174.76695, -36.84279)   # lon, lat
map_width <- 20000   # metres
map_height <- 28000  # metres
map_scale <- 10      # metres per pixel for output image

# Libraries
library(tidyverse)
library(here)
library(janitor)
library(sf)
library(ggthemes)
library(systemfonts)
library(ragg)
library(patchwork)

# Custom font setup
register_font(
  name = "National 2 Custom",
  plain = system_fonts() |>
    filter(family == "National 2", style == "Regular") |>
    pull(path),
  bold = system_fonts() |>
    filter(family == "National 2", style == "Bold") |>
    pull(path),
  italic = system_fonts() |>
    filter(family == "National 2", style == "Regular Italic") |>
    pull(path),
  bolditalic = system_fonts() |>
    filter(family == "National 2", style == "Bold Italic") |>
    pull(path),
  features = font_feature(
    ligatures = c("discretionary", "standard", "contextual"),
    numbers = c("lining", "proportional")
  )
)

# Raw data from NZ.Stat
# Population estimates by 5-year age groups and SA2 area
# For 2001, 2011 and 2021 (as at 30 June)
dat <- read_csv(
  file = here("data/TABLECODE7979_Data_77233290-f900-4dc6-ad99-ae2899b4d068.csv"),
  col_types = "ccciic"
) |>
  clean_names()

# Calculate changes in 'youngish adult' population
dat_ya_pop_change <- dat |>
  filter(age %in% c(
    "20-24 Years",
    "25-29 Years",
    "30-34 Years"
  )) |>
  group_by(area, year_at_30_june) |>
  summarise(pop = sum(value, na.rm = TRUE)) |>
  ungroup() |>
  pivot_wider(
    names_from = year_at_30_june,
    names_prefix = "pop_",
    values_from = pop
  ) |>
  mutate(
    pop_change_01_11 = pop_2011 - pop_2001,
    pop_change_11_21 = pop_2021 - pop_2011,
    pop_change_01_21 = pop_2021 - pop_2001
  ) |>
  select(area, starts_with("pop_change_")) |>
  pivot_longer(
    cols = starts_with("pop_change_"),
    names_to = "period",
    values_to = "value"
  ) |>
  mutate(sign = ifelse(value >= 0, "pos", "neg")) |>
  mutate(abs_value = abs(value))

# 2021 SA2 shapefile
dat_sa2_2021 <- read_sf(
  dsn = here("data/statistical-area-2-2021-clipped-generalised.shp")
) |>
  clean_names()

# 2021 geographic areas: regions and SA2
dat_areas_2021 <- read_csv(
  file = here("data/geographic-areas-table-2021.csv"),
  col_types = cols(.default = "character")
) |>
  clean_names() |>
  select(
    sa22021_code, sa22021_name_ascii,
    regc2021_name
  ) |>
  distinct()

# Join population and SA2 data for Auckland only
dat_combined <- dat_sa2_2021 |>
  right_join(
    y = dat_areas_2021 |> filter(regc2021_name == "Auckland Region"),
    by = c("sa22021_v1" = "sa22021_code", "sa22021_2" = "sa22021_name_ascii")
  ) |>
  select(sa22021_v1, sa22021_2) |>
  left_join(
    y = dat_ya_pop_change,
    by = c("sa22021_2" = "area")
  ) |>
  filter(!str_detect(string = sa22021_2, pattern = "Oceanic")) |>
  filter(!st_is_empty(geometry))

# Generate random dots inside SA2 areas for visualisation
dat_vis <- dat_combined |>
  nest_by(period, sign) |>
  mutate(dots = list(st_as_sf(st_sample(x = data, size = data$abs_value)))) |>
  ungroup() |>
  select(-data) |>
  unnest(cols = dots) |>
  st_as_sf()

# Set up map bounding box
map_centre_pt <- st_as_sf(
  tibble(p = list(st_point(x = map_centre, dim = "XY")))
) |>
  st_set_crs(4326) |>   # WGS84
  st_transform(2193) |> # NZGD2000
  st_coordinates() |>
  as_tibble()

map_box <- bind_rows(
  map_centre_pt |> mutate(X = X - map_width, Y = Y + map_height),  # Top left
  map_centre_pt |> mutate(X = X + map_width, Y = Y + map_height),  # Top right
  map_centre_pt |> mutate(X = X + map_width, Y = Y - map_height),  # Bottom right
  map_centre_pt |> mutate(X = X - map_width, Y = Y - map_height),  # Bottom left
) |>
  st_as_sf(
    coords = c("X", "Y"),
    crs = 2193
  ) |>
  st_bbox()

# Clip visualisation data to map_box
dat_vis_clipped <- st_crop(x = dat_vis, y = map_box)
dat_sa2_clipped <- st_crop(x = dat_sa2_2021, y = map_box) |> st_union()

# Visualise 2001 to 2011
vis_01_11 <- ggplot() +
  geom_sf(
    fill = grey(0.94),
    colour = grey(0.05),
    size = 0.15,
    data = dat_sa2_clipped
  ) +
  geom_sf(
    mapping = aes(colour = sign),
    size = 0.25,
    stroke = 0,
    data = dat_vis_clipped |>
      filter(period == "pop_change_01_11")
  ) +
  annotate(
    geom = "text",
    x = map_centre_pt[[1, "X"]] + 5000,
    y = map_centre_pt[[1, "Y"]] + 22000,
    label = "Age 20-34 resident population\nchanges: 2001 to 2011",
    hjust = 0,
    colour = "#491d8b",
    family = "National 2 Custom",
    fontface = "bold"
  ) +
  scale_x_continuous(expand = expansion(0, 0)) +
  scale_y_continuous(expand = expansion(0, 0)) +
  scale_colour_manual(
    values = c("pos" = "#003a6d", "neg" = "#750e13"),
    labels = c("One person increase", "One person decrease"),
    guide = guide_legend(
      title = NULL,
      ncol = 1,
      override.aes = list(size = 1.5)
    )
  ) +
  theme_map(
    base_family = "National 2 Custom"
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 0),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_blank(),
    plot.background = element_blank(),
    panel.background = element_rect(
      fill = NULL,
      colour = grey(0.05),
      size = 0.15
    ),
    legend.position = c(0.625, 0.785),
    legend.background = element_rect(
      fill = "white",
      colour = "black",
      size = 0.15
    ),
    legend.key = element_blank(),
    legend.margin = margin(1, 11, 5, 5, "pt"),
    legend.box.margin = margin(0, 0, 0, 0, "pt")
  )

# Visualise 2011 to 2021
vis_11_21 <- ggplot() +
  geom_sf(
    fill = grey(0.94),
    colour = grey(0.05),
    size = 0.15,
    data = dat_sa2_clipped
  ) +
  geom_sf(
    mapping = aes(colour = sign),
    size = 0.25,
    stroke = 0,
    data = dat_vis_clipped |>
      filter(period == "pop_change_11_21")
  ) +
  annotate(
    geom = "text",
    x = map_centre_pt[[1, "X"]] + 5000,
    y = map_centre_pt[[1, "Y"]] + 22000,
    label = "Age 20-34 resident population\nchanges: 2011 to 2021",
    hjust = 0,
    colour = "#491d8b",
    family = "National 2 Custom",
    fontface = "bold"
  ) +
  scale_x_continuous(expand = expansion(0, 0)) +
  scale_y_continuous(expand = expansion(0, 0)) +
  scale_colour_manual(
    values = c("pos" = "#003a6d", "neg" = "#750e13"),
    labels = c("One person increase", "One person decrease"),
    guide = guide_legend(
      title = NULL,
      ncol = 1,
      override.aes = list(size = 1.5)
    )
  ) +
  theme_map(
    base_family = "National 2 Custom"
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 0),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_blank(),
    plot.background = element_blank(),
    panel.background = element_rect(
      fill = NULL,
      colour = grey(0.05),
      size = 0.15
    ),
    legend.position = c(0.625, 0.785),
    legend.background = element_rect(
      fill = "white",
      colour = "black",
      size = 0.15
    ),
    legend.key = element_blank(),
    legend.margin = margin(1, 11, 5, 5, "pt"),
    legend.box.margin = margin(0, 0, 0, 0, "pt")
  )

# Combine
vis <- vis_01_11 + plot_spacer() + vis_11_21 +
  plot_layout(widths = c(15, 1, 15))

ggsave(
  filename = here("outputs/young-adult-pop-redux.png"),
  plot = vis,
  device = agg_png,
  width = 2 * round(map_width / map_scale) + 150,
  height = round(map_height / map_scale),
  units = "px",
  bg = "white"
)
