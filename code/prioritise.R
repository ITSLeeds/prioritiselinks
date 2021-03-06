# This script uses the example dataset created in code/create-rnet.R

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(stplanr)

# piggyback::pb_download("rnet_kildare_quietest.Rds", tag = "0.1")
rnet_quiet = readRDS("rnet_kildare_quietest.Rds")

min_grouped_cycling_potential = 30

# Identify road refs ------------------------------------------------------
# We separate the road refs from the names, and use them for grouping, as in the rapid tool

rnet_with_ref = rnet_quiet[which(grepl("L1|L2|L3|L4|L5|L6|L7|L8|L9|R1|R2|R3|R4|R5|R6|R7|R8|R9", rnet_quiet$name) & !(grepl("Link joining|Link between|Link with|Along the side of", rnet_quiet$name))),]

rnet_without_ref = rnet_quiet[which(!(rnet_quiet$name %in% rnet_with_ref$name)),]

# deal with "Un-named link", "Short un-named link"

rnet_with_ref = rnet_with_ref %>%
  mutate(ref = str_extract(rnet_with_ref$name, "\\D\\d+"))
rnet_without_ref = rnet_without_ref %>%
  mutate(ref = "")

# Roads with no ref -------------------------------------------------------

# # Remove segments with cycling potential below 30 (to prevent side street segments)
# min_cycling_potential_without_ref = 30
# r_linestrings_without_ref =  rnet_without_ref %>%
#   filter(cyclists >= min_cycling_potential_without_ref)

# Put into groups, using a 500m buffer (stricter than for roads with a ref, to prevent groups covering multiple streets)
r_linestrings_without_ref_buff = geo_buffer(shp = rnet_without_ref, dist = 500)
touching_list = st_intersects(r_linestrings_without_ref_buff)
g = igraph::graph.adjlist(touching_list)
components = igraph::components(g)
rnet_without_ref$group2 = components$membership

# mapview::mapview(r_linestrings_without_ref["group2"], lwd = 3)

# Remove groups with length <300m (20m buffer). (these are groups consisting purely of road segments with no ref) this is stricter than for roads with refs, because otherwise too many short segments are picked up.
# We could also remove groups without sufficient cycle potential (the same as for roads with a ref)
r_linestrings_without_ref = rnet_without_ref %>%
  group_by(group2) %>%
  mutate(
    group2_length = round(sum(length)),
    mean_cycling_potential = round(weighted.mean(cyclists, length, na.rm = TRUE))
  ) %>%
  filter(group2_length >= 300) %>%
  # filter(mean_cycling_potential > min_grouped_cycling_potential) %>%  # this varies by region
  ungroup()
# mapview::mapview(r_linestrings_without_ref, zcol = "mean_cycling_potential")


# Roads with a ref --------------------------------------------------------

# We should also group by cycle friendliness

buff_dist_large = 100
gs = unique(rnet_with_ref$ref)
# i = g[2]
i = "L4009"

# create per ref groups
rg_list = lapply(gs, FUN = function(i) {
  rg = rnet_with_ref %>% filter(ref == i)
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

# Only keep groups of sufficient length (and cycling potential)
rg_new2 = rg_new %>%
  group_by(group2) %>%
  mutate(
    group2_length = round(sum(length)),
    mean_cycling_potential = round(weighted.mean(cyclists, length, na.rm = TRUE))
  ) %>%
  # filter(mean_cycling_potential >= min_grouped_cycling_potential) %>%
  filter(group2_length >= 300) %>%
  ungroup()
# mapview::mapview(rg_new2)

summary(rg_new2$group2_length)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1     225     681    1537    1291   23340
# ranges from 1m to 23km










# Now rejoin the roads with no ref together with the roads with a ref
r_lanes = rbind(rg_new2, r_linestrings_without_ref)
# mapview::mapview(r_lanes, zcol = "group2")

gs = unique(r_lanes$name)
# i = g[2]
i = "A4174"

# create per name groups
rg_list = lapply(gs, FUN = function(i) {
  rg = r_lanes %>% filter(name == i)
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

# Only keep groups of sufficient cycling potential or length
rg_new2 = rg_new %>%
  group_by(ig, group2, name) %>%
  mutate(
    mean_cycling_potential = round(weighted.mean(cyclists, length, na.rm = TRUE)),
    length_name = round(sum(length))
  ) %>%
  # filter(mean_cycling_potential >= min_grouped_cycling_potential) %>%
  filter(mean_cycling_potential >= min_grouped_cycling_potential | length_name > 100) %>%
  ungroup()
# mapview::mapview(rg_new2, zcol = "mean_cycling_potential")



# probably not needed
rg_buff = geo_buffer(shp = rg_new2, dist = buff_dist_large)
touching_list = st_intersects(rg_buff)
g = igraph::graph.adjlist(touching_list)
components = igraph::components(g)
rg_new2$lastgroup = components$membership

# Only keep segments which are part of a wider group (including roads with different names/names) of >500m length (100m buffer)
min_grouped_length = 500
rg_new3 = rg_new2 %>%
  group_by(lastgroup) %>%
  mutate(last_length = round(sum(length))) %>%
  filter(last_length >= min_grouped_length) %>%
  ungroup()




# mapview::mapview(rg_new3, zcol = "lastgroup")
# create a new group to capture long continuous sections with the same name
min_length_named_road = min_grouped_length
rg_new4 = rg_new3 %>%
  group_by(ref, group2, ig, name) %>%
  mutate(long_named_section = case_when(
    sum(length) > min_length_named_road & name != "" ~ name,
    TRUE ~ "Other"
  )
  ) %>%
  ungroup()
# mapview::mapview(rg_new4, zcol = "long_named_section")
# table(rg_new4$long_named_section)
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

lg_new = lg_new %>%
  mutate(rowid = row_number())

# other_roads = rg_new4[rg_new4$long_named_section == "Other", ]
# other_roads$long_named_group = NA
# rejoined = rbind(lg_new, other_roads)

# find group membership of top named roads
r_lanes_grouped2 = lg_new %>%
  group_by(name, group2, ig, long_named_section, long_named_group, quietness) %>%
  summarise(
    name = case_when(
      length(table(name)) > 4 ~ "Unnamed road",
      names(table(name))[which.max(table(name))] != "" ~
        names(table(name))[which.max(table(name))],
      names(table(name))[which.max(table(name))] != "" ~
        names(table(name))[which.max(table(name))],
      TRUE ~ "Unnamed road"
    ),
    group_length = round(sum(length)),
    mean_cycling_potential = round(weighted.mean(cyclists, length, na.rm = TRUE))
  ) %>%
  filter(mean_cycling_potential > min_grouped_cycling_potential | group_length > min_grouped_length) %>%
  ungroup() %>%
  mutate(group_id = 1:nrow(.))

# mapview::mapview(r_lanes_grouped2, zcol = "mean_cycling_potential")
# mapview::mapview(r_lanes_grouped2, zcol = "name")
library(tmap)
tmap_mode("view")
# tm_shape(r_lanes_grouped2) + tm_lines("quietness")



# Split up any segments longer than 2km -----------------------------------

# too_long = r_lanes_grouped2 %>%
#   filter(group_length > 2000)
#
# too_long2 = lg_new %>%
#   group_by(name, group2, ig, long_named_section, long_named_group, quietness) %>%
#   summarise(
#     name = case_when(
#       length(table(name)) > 4 ~ "Unnamed road",
#       names(table(name))[which.max(table(name))] != "" ~
#         names(table(name))[which.max(table(name))],
#       names(table(name))[which.max(table(name))] != "" ~
#         names(table(name))[which.max(table(name))],
#       TRUE ~ "Unnamed road"
#     ),
#     group_length = round(sum(length)),
#     mean_cycling_potential = round(weighted.mean(cyclists, length, na.rm = TRUE))
#   ) %>%
#   filter(mean_cycling_potential > min_grouped_cycling_potential | group_length > min_grouped_length) %>%
#   ungroup() %>%
#   mutate(group_id = 1:nrow(.)) %>%
#   filter(group_length > 2000)

# Summary stats -----------------------------------------------------------

weighted.mean(r_lanes_grouped2$quietness, w = r_lanes_grouped2$group_length)
# [1] 53.79977


weighted.mean(r_lanes_grouped2$mean_cycling_potential, w = r_lanes_grouped2$group_length)
# [1] 39.82803

prioritise = r_lanes_grouped2 %>%
  mutate(quiet = case_when(r_lanes_grouped2$quietness > 50 ~ "yes", TRUE ~ "no"),
         high_cycling = case_when(r_lanes_grouped2$mean_cycling_potential > 40 ~ "yes", TRUE ~ "no")
  )

prioritise = prioritise %>%
  mutate(priority = case_when(prioritise$quiet == "no" & prioritise$high_cycling == "yes" ~ "1st",
                              prioritise$quiet == "yes" & prioritise$high_cycling == "yes" ~ "2nd",
                              prioritise$quiet == "yes" & prioritise$high_cycling == "no" ~ "3rd",
                              prioritise$quiet == "no" & prioritise$high_cycling == "no" ~ "4th"
  )
  )


pal = colorspace::sequential_hcl(n = 2, h = c(141, 6), c = c(70, NA, 86), l = c(73, 52), power = c(0.7, 1.9), rev = TRUE)
tm_shape(prioritise) + tm_lines("quiet", palette = pal)
tm_shape(prioritise) + tm_lines("high_cycling", palette = pal)

high_quiet = prioritise %>%
  filter(quiet == "yes", high_cycling == "yes")
sum(high_quiet$group_length)
# [1] 136038
dim(high_quiet)[1]
# [1] 215

high_busy = prioritise %>%
  filter(quiet == "no", high_cycling == "yes")
sum(high_busy$group_length)
# [1] 101434
dim(high_busy)[1]
# [1] 356

low_quiet = prioritise %>%
  filter(quiet == "yes", high_cycling == "no")
sum(low_quiet$group_length)
# [1] 251034
dim(low_quiet)[1]
# [1] 204

low_busy = prioritise %>%
  filter(quiet == "no", high_cycling == "no")
sum(low_busy$group_length)
# [1] 211562
dim(low_busy)[1]
# [1] 272

prioritise = prioritise %>%
  mutate(ordered = sqrt(mean_cycling_potential) / sqrt(quietness))

g1 = prioritise %>%
  ggplot(aes(mean_cycling_potential, quietness)) +
  geom_point(aes(colour = priority, size = group_length), alpha = 0.3) +
  scale_color_manual(values = c("red", "yellow", "pink", "blue")) +
  scale_size(range = c(1, 9)) +
  geom_hline(yintercept = 50) +
  geom_vline(xintercept = 40) +
  labs(x = "Mean cycling potential", y = "Quietness", colour = "Priority", size = "Length (m)") +
  guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
# g1
g1 + scale_x_continuous(trans = "log10")

# Generate lists of top segments ------------------------------------------------------------



summary(prioritise)












# cycleways = cycleways_en[region, ]
# cycleways = cycleways %>% select(surface, name, lit, osm_id)
# cycleway_buffer = stplanr::geo_buffer(cycleways, dist = pct_dist_within) %>% sf::st_union()
#
# r_lanes_grouped_in_cycleway = st_intersection(prioritise, cycleway_buffer) %>%
#   mutate(length_in_cycleway = round(as.numeric(st_length(.))))
# # mapview::mapview(r_lanes_grouped_in_cycleway["length_in_cycleway"]) +
# #   mapview::mapview(cycleways)
# r_lanes_grouped_in_cycleway = r_lanes_grouped_in_cycleway %>%
#   st_drop_geometry()
#
# minp_exclude = 0.8
# r_lanes_joined = left_join(prioritise, r_lanes_grouped_in_cycleway) %>%
#   mutate(km_cycled = round(mean_cycling_potential * group_length / 1000))
# r_lanes_joined$length_in_cycleway[is.na(r_lanes_joined$length_in_cycleway)] = 0
# r_lanes_joined$proportion_on_cycleway = r_lanes_joined$length_in_cycleway / r_lanes_joined$group_length
# summary(r_lanes_joined$proportion_on_cycleway) # all between 0 and 1
# # mapview::mapview(r_lanes_joined["proportion_on_cycleway"])
#
# # we need to add in all segments within the grey key roads, and usethe combined dataset to pick the top routes
# r_lanes_top = r_lanes_joined %>%
#   # filter(name != "Unnamed road" & name != "") %>%
#   filter(name != "Unnamed road") %>%
#   # filter(!str_detect(string = name, pattern = "^A[1-9]")) %>%
#   filter(group_length > min_grouped_length) %>%
#   filter(mean_cycling_potential > min_grouped_cycling_potential) %>%
#   filter(!grepl(pattern = regexclude, name, ignore.case = TRUE)) %>%
#   filter(proportion_on_cycleway < minp_exclude) %>%
#   mutate(
#     length_up_to_1km = if_else(group_length > 1000, true = 1000, false = group_length),
#     km_cycled_1km = length_up_to_1km * mean_cycling_potential,
#     srn = name %in% srn_names_df$roa_number
#   ) %>%
#   arrange(desc(mean_cycling_potential)) %>%
#   slice(1:n_top_roads)
# nrow(r_lanes_top)
#
# # classify roads to visualise
# labels = c("Top ranked new cycleways", "Spare lane(s)", "Estimated width > 10m")
# cycleways_name = "Existing cycleways"
#
# r_lanes_final = r_lanes_joined %>%
#   mutate(
#     Status = case_when(
#       group_id %in% r_lanes_top$group_id ~ labels[1],
#       majority_spare_lane ~ labels[2],
#       mean_width >= 10 ~ labels[3]
#     ),
#     `Estimated width` = case_when(
#       mean_width < 10 ~ "<10 m",
#       mean_width >= 10 & mean_width < 15 ~ "10-15 m",
#       mean_width >= 15 ~ ">15 m"
#     )
#   ) %>%
#   select(name, name, Status, mean_cycling_potential, spare_lane = majority_spare_lane, `Estimated width`, `length (m)` = group_length, group_id, speed_limit)
# r_lanes_final$Status = factor(r_lanes_final$Status, levels = c(labels[1], labels[2], labels[3]))
#
# table(r_lanes_final$name)
# table(r_lanes_final$Status)
# summary(factor(r_lanes_final$Status))
