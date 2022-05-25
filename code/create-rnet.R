# This script creates the example dataset which is available in release 0.1

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(stplanr)

routes_kildare = readRDS("routes_combined_quietest.Rds")
# 404925

routes_kildare = routes_kildare %>%
  mutate(segment_id = row_number())
rnet_id = stplanr::overline(routes_kildare,
                            attrib = c("cyclists", "cyclists_10pc", "cyclists_30pc", "quietness", "gradient_smooth", "segment_id"),
                            fun = list(sum = sum, mean = mean, first = first)
)


quietness_breaks = c(0, 25, 50, 75, 100)
pal = colorspace::sequential_hcl(n = 4, h = c(141, 6), c = c(70, NA, 86), l = c(73, 52), power = c(0.7, 1.9), rev = TRUE)

rnet_selected = rnet_id %>%
  transmute(
    segment_id = segment_id_first,
    cyclists = round(cyclists_sum),
    cyclists_10pc = round(cyclists_10pc_sum),
    cyclists_30pc = round(cyclists_30pc_sum),
    gradient = round(gradient_smooth_mean * 100),
    quietness = round(quietness_mean),
    col = cut(quietness, quietness_breaks, labels = pal),
    lwd = case_when(cyclists < 20 ~ 20, TRUE ~ cyclists)
  ) %>%
  filter(cyclists > 10) # Remove any rnet segments with small numbers of cyclists
rnet_selected$length = units::drop_units(st_length(rnet_selected))

# Join the rnet with the street names -------------------------------------

kildare_streets = routes_kildare %>%
  st_drop_geometry() %>%
  select(name, segment_id)
rnet_join = inner_join(rnet_selected, kildare_streets, by = "segment_id")

saveRDS(rnet_join, "rnet_kildare_quietest.Rds")


# Not needed
# # Many of the route segments are being lost while others are being duplicated during the join process
# # This would be much more accurate if we could use an ID variable to identify the segments, but I haven't found a way to include a route segment ID in the rnet
# buff_dist = 10
# rnet_buff = geo_buffer(shp = rnet_kildare, dist = buff_dist)
#
# join_kildare = st_join(routes_kildare, rnet_buff, join = st_within)
# # 552387
# join_kildare_filtered = join_kildare %>%
#   filter(! is.na(quietness))
# # 355281
#
# kildare_simplified = join_kildare_filtered %>%
#   select(name, cyclists, cyclists_10pc, quietness, col, lwd)
# system.time({
#   kildare_unique = distinct(kildare_simplified)
# })

