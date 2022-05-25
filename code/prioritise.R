library(tidyverse)

# piggyback::pb_download("rnet_kildare_quietest.Rds", tag = "0.1")
rnet_quiet = readRDS("rnet_kildare_quietest.Rds")

r_linestrings_with_ref = rnet_kildare %>%
  filter(ref != "")


# Roads with a ref --------------------------------------------------------

gs = unique(r_linestrings_with_ref$ref)
# i = g[2]
i = "A4174"

# create per ref groups
rg_list = lapply(gs, FUN = function(i) {
  rg = r_linestrings_with_ref %>% filter(ref == i)
  # mapview::mapview(rg)
  r_lanes_all_buff = rg %>%
    st_transform(27700) %>%
    st_buffer(buff_dist_large) %>%
    st_transform(4326)
  touching_list = st_intersects(r_lanes_all_buff)
  g = igraph::graph.adjlist(touching_list)
  components = igraph::components(g)
  rg$ig = components$membership
  rg
})

rg_new = do.call(rbind, rg_list)
# mapview::mapview(rg_new)

#Create group IDs for the roads with a ref
rg_new$group2 = paste(rg_new$ig, rg_new$group, rg_new$ref)
rg_new$ig = NULL

# Only keep groups of sufficient width/lanes and cycling potential
rg_new2 = rg_new %>%
  group_by(group2) %>%
  mutate(
    group2_length = round(sum(length)),
    mean_cycling_potential = round(weighted.mean(cycling_potential, length, na.rm = TRUE)),
    mean_width = round(weighted.mean(width, length, na.rm = TRUE)),
    majority_spare_lane = sum(length[spare_lane]) > sum(length[!spare_lane])
  ) %>%
  filter(mean_width >= 10 | majority_spare_lane) %>%
  filter(mean_cycling_potential >= min_grouped_cycling_potential) %>%
  ungroup()
# mapview::mapview(rg_new2)

# Now rejoin the roads with no ref together with the roads with a ref
r_lanes = rbind(rg_new2, r_linestrings_without_ref2)
# mapview::mapview(r_lanes, zcol = "group2")

gs = unique(r_lanes$ref)
# i = g[2]
i = "A4174"

# create per ref groups
rg_list = lapply(gs, FUN = function(i) {
  rg = r_lanes %>% filter(ref == i)
  # mapview::mapview(rg)
  r_lanes_all_buff = rg %>%
    st_transform(27700) %>%
    st_buffer(buff_dist_large) %>%
    st_transform(4326)
  touching_list = st_intersects(r_lanes_all_buff)
  g = igraph::graph.adjlist(touching_list)
  components = igraph::components(g)
  rg$ig = components$membership
  rg
})

rg_new = do.call(rbind, rg_list)
# mapview::mapview(rg_new)

# Only keep groups of sufficient width and cycling potential
rg_new2 = rg_new %>%
  group_by(ig, group, ref) %>%
  mutate(
    mean_width = round(weighted.mean(width, length, na.rm = TRUE)),
    mean_cycling_potential = round(weighted.mean(cycling_potential, length, na.rm = TRUE)),
    majority_spare_lane = sum(length[spare_lane]) > sum(length[!spare_lane])
  ) %>%
  filter(mean_width >= 10 | majority_spare_lane) %>%
  filter(mean_cycling_potential >= min_grouped_cycling_potential) %>%
  ungroup()
# mapview::mapview(rg_new2, zcol = "mean_cycling_potential")

rg_buff = geo_buffer(shp = rg_new2, dist = buff_dist_large)
touching_list = st_intersects(rg_buff)
g = igraph::graph.adjlist(touching_list)
components = igraph::components(g)
rg_new2$lastgroup = components$membership

# Only keep segments which are part of a wider group (including roads with different refs/names) of >500m length (100m buffer)
rg_new3 = rg_new2 %>%
  group_by(lastgroup) %>%
  mutate(last_length = round(sum(length))) %>%
  filter(last_length >= min_grouped_length) %>%
  ungroup()

# mapview::mapview(rg_new3, zcol = "lastgroup")
# create a new group to capture long continuous sections with the same name
min_length_named_road = min_grouped_length
rg_new4 = rg_new3 %>%
  group_by(ref, group, ig, name) %>%
  mutate(long_named_section = case_when(
    sum(length) > min_length_named_road & name != "" ~ name,
    TRUE ~ "Other"
  )
  ) %>%
  ungroup()
# mapview::mapview(rg_new4, zcol = "long_named_section")
table(rg_new4$long_named_section)
# new approach

# Split into sections by road name, and split these into contiguous sections using buff_dist_large (100m).
lgs = unique(rg_new4$long_named_section)

i = "Melbourn bypass"

# create per name groups
long_list = lapply(lgs, FUN = function(i) {
  lg = rg_new4 %>% filter(long_named_section == i)
  # mapview::mapview(lg)
  l_buff = lg %>%
    st_transform(27700) %>%
    st_buffer(buff_dist_large) %>%
    st_transform(4326)
  touching_list = st_intersects(l_buff)
  g = igraph::graph.adjlist(touching_list)
  components = igraph::components(g)
  lg$long_named_group = components$membership
  lg
})

lg_new = do.call(rbind, long_list)

# other_roads = rg_new4[rg_new4$long_named_section == "Other", ]
# other_roads$long_named_group = NA
# rejoined = rbind(lg_new, other_roads)

# find group membership of top named roads
r_lanes_grouped2 = lg_new %>%
  group_by(ref, group, ig, long_named_section, long_named_group) %>%
  summarise(
    name = case_when(
      length(table(name)) > 4 ~ "Unnamed road",
      names(table(name))[which.max(table(name))] != "" ~
        names(table(name))[which.max(table(name))],
      names(table(ref))[which.max(table(ref))] != "" ~
        names(table(ref))[which.max(table(ref))],
      TRUE ~ "Unnamed road"
    ),
    group_length = round(sum(length)),
    mean_cycling_potential = round(weighted.mean(cycling_potential, length, na.rm = TRUE)),
    mean_width = round(weighted.mean(width, length, na.rm = TRUE)),
    majority_spare_lane = sum(length[spare_lane]) > sum(length[!spare_lane]),
    speed_limit = names(which.max(table(maxspeed)))
  ) %>%
  filter(mean_cycling_potential > min_grouped_cycling_potential | group_length > min_grouped_length) %>%
  ungroup() %>%
  mutate(group_id = 1:nrow(.))
# mapview::mapview(r_lanes_grouped2, zcol = "mean_cycling_potential")
# mapview::mapview(r_lanes_grouped2, zcol = "name")

# Generate lists of top segments ------------------------------------------------------------

cycleways = cycleways_en[region, ]
cycleways = cycleways %>% select(surface, name, lit, osm_id)
cycleway_buffer = stplanr::geo_buffer(cycleways, dist = pct_dist_within) %>% sf::st_union()

r_lanes_grouped_in_cycleway = st_intersection(r_lanes_grouped2, cycleway_buffer) %>%
  mutate(length_in_cycleway = round(as.numeric(st_length(.))))
# mapview::mapview(r_lanes_grouped_in_cycleway["length_in_cycleway"]) +
#   mapview::mapview(cycleways)
r_lanes_grouped_in_cycleway = r_lanes_grouped_in_cycleway %>%
  st_drop_geometry()

minp_exclude = 0.8
r_lanes_joined = left_join(r_lanes_grouped2, r_lanes_grouped_in_cycleway) %>%
  mutate(km_cycled = round(mean_cycling_potential * group_length / 1000))
r_lanes_joined$length_in_cycleway[is.na(r_lanes_joined$length_in_cycleway)] = 0
r_lanes_joined$proportion_on_cycleway = r_lanes_joined$length_in_cycleway / r_lanes_joined$group_length
summary(r_lanes_joined$proportion_on_cycleway) # all between 0 and 1
# mapview::mapview(r_lanes_joined["proportion_on_cycleway"])

# we need to add in all segments within the grey key roads, and usethe combined dataset to pick the top routes
r_lanes_top = r_lanes_joined %>%
  # filter(name != "Unnamed road" & ref != "") %>%
  filter(name != "Unnamed road") %>%
  # filter(!str_detect(string = name, pattern = "^A[1-9]")) %>%
  filter(group_length > min_grouped_length) %>%
  filter(mean_cycling_potential > min_grouped_cycling_potential) %>%
  filter(!grepl(pattern = regexclude, name, ignore.case = TRUE)) %>%
  filter(proportion_on_cycleway < minp_exclude) %>%
  mutate(
    length_up_to_1km = if_else(group_length > 1000, true = 1000, false = group_length),
    km_cycled_1km = length_up_to_1km * mean_cycling_potential,
    srn = name %in% srn_names_df$roa_number
  ) %>%
  arrange(desc(mean_cycling_potential)) %>%
  slice(1:n_top_roads)
nrow(r_lanes_top)

# classify roads to visualise
labels = c("Top ranked new cycleways", "Spare lane(s)", "Estimated width > 10m")
cycleways_name = "Existing cycleways"

r_lanes_final = r_lanes_joined %>%
  mutate(
    Status = case_when(
      group_id %in% r_lanes_top$group_id ~ labels[1],
      majority_spare_lane ~ labels[2],
      mean_width >= 10 ~ labels[3]
    ),
    `Estimated width` = case_when(
      mean_width < 10 ~ "<10 m",
      mean_width >= 10 & mean_width < 15 ~ "10-15 m",
      mean_width >= 15 ~ ">15 m"
    )
  ) %>%
  select(name, ref, Status, mean_cycling_potential, spare_lane = majority_spare_lane, `Estimated width`, `length (m)` = group_length, group_id, speed_limit)
r_lanes_final$Status = factor(r_lanes_final$Status, levels = c(labels[1], labels[2], labels[3]))

table(r_lanes_final$name)
table(r_lanes_final$Status)
summary(factor(r_lanes_final$Status))
