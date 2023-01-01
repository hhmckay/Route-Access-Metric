### Route Accessibility
options(java.parameters = "-Xmx10G")
library(gtfstools)
library(r5r)
library(tigris)
library(lehdr)
library(dplyr)
library(ggplot2)
library(sf)

gtfs <- read_gtfs("C:/Users/henrymckay/Desktop/RouteAccessibility/SacRT_GTFS.zip")
routes <- gtfs$routes
route_ids <- routes$route_id

data_path = "/Users/henrymckay/Desktop/RouteAccessibility/Network"
r5r_core <- setup_r5(data_path, verbose = FALSE, temp_dir = TRUE, overwrite = TRUE)

shapes <- block_groups(
  state = "CA",
  county = "Sacramento",
  year = 2019
)

jobs <- grab_lodes(state = "ca", year = 2019, lodes_type = "wac", job_type = "JT00", 
                   segment = "S000", state_part = "main", agg_geo = "bg")

jobs <- jobs %>%
  filter(substring(jobs$w_bg, 3, 5) == "067")

workers <- grab_lodes(state = "ca", year = 2018, lodes_type = "rac", job_type = "JT00", 
                   segment = "S000", state_part = "main", agg_geo = "bg")

workers <- workers %>%
  filter(substring(workers$h_bg, 3, 5) == "067")

jobs <- merge(shapes,
              jobs,
              by.x = "GEOID",
              by.y = "w_bg",
              all.x = T)

# Convert census geometry centroid lat/lon from chr to num
jobs$INTPTLAT <- as.numeric(jobs$INTPTLAT)
jobs$INTPTLON <- as.numeric(jobs$INTPTLON)

# Change census geometry col names for use in r5
names(jobs)[names(jobs) == 'INTPTLAT'] <- 'lat'
names(jobs)[names(jobs) == 'INTPTLON'] <- 'lon'
names(jobs)[names(jobs) == 'GEOID'] <- 'id'

# Convert Census geometry to data frame
jobs <- as.data.frame(jobs)

### BASE ACCESS ###
range <- seq(0, 120, by = 5)

base_access_df = NULL

departure_datetime <- as.POSIXct("08-30-2022 07:00:00",
                                 format = "%m-%d-%Y %H:%M:%S",
                                 tz = "America/Los_Angeles")

for (i in range) {
  
  base_access <- accessibility(r5r_core = r5r_core,
                               origins = jobs,
                               destinations = jobs,
                               opportunities_colname = "C000",
                               mode = "TRANSIT",
                               mode_egress = "WALK",
                               departure_datetime <- departure_datetime,
                               decay_function = "exponential",
                               cutoffs = 60,
                               max_trip_duration = 60,
                               walk_speed = 4.82803,
                               max_rides = 4,
                               max_walk_dist = 3218.69,
                               verbose = FALSE,
                               progress = FALSE)
  
  print(departure_datetime)
  
  departure_datetime <- departure_datetime + 300
  
  base_access_df <- rbind(base_access_df, base_access)
}

base_access <- base_access_df %>%
  group_by(from_id) %>%
  summarise(accessibility = median(accessibility))

base_access <- merge(base_access,
                     workers,
                     by.x = "from_id",
                     by.y = "h_bg",
                     all.x = T)
base_access[is.na(base_access)] = 0

base_access <- base_access %>%
  mutate(weighted_access = accessibility * C000)

tot_jobs <- sum(base_access$accessibility)
tot_jobs_weighted  <- sum(base_access$weighted_access)
avg_jobs <- mean(base_access$accessibility)
avg_jobs_weighted <- weighted.mean(base_access$accessibility, base_access$C000)

### Modified Access ###
summary_stats = NULL
for (i in route_ids) {

  filtered_gtfs <- filter_by_route_id(gtfs, i, keep = FALSE)
  url <- "C:/Users/henrymckay/Desktop/RouteAccessibility/Network/SacRT_GTFS.zip"
  write_gtfs(filtered_gtfs, url)

  r5r_core <- setup_r5(data_path, verbose = FALSE, overwrite = TRUE, temp_dir = TRUE)
  
  range <- seq(0, 120, by = 5)
  
  modified_access_df = NULL
  
  departure_datetime <- as.POSIXct("08-30-2022 07:00:00",
                                   format = "%m-%d-%Y %H:%M:%S",
                                   tz = "America/Los_Angeles")
  
  for (h in range) {
    
    modified_access <- accessibility(r5r_core = r5r_core,
                                     origins = jobs,
                                     destinations = jobs,
                                     opportunities_colname = "C000",
                                     mode = "TRANSIT",
                                     mode_egress = "WALK",
                                     departure_datetime <- departure_datetime,
                                     decay_function = "exponential",
                                     cutoffs = 60,
                                     max_trip_duration = 60,
                                     walk_speed = 4.82803,
                                     max_rides = 4,
                                     max_walk_dist = 3218.69,
                                     verbose = FALSE,
                                     progress = FALSE)
    
    print(departure_datetime)
    
    departure_datetime <- departure_datetime + 300
    
    modified_access_df <- rbind(modified_access_df, modified_access)
  }
  
  modified_access <- modified_access_df %>%
    group_by(from_id) %>%
    summarise(accessibility = median(accessibility))
  
  modified_access <- merge(modified_access,
                       workers,
                       by.x = "from_id",
                       by.y = "h_bg",
                       all.x = T)
  modified_access[is.na(modified_access)] = 0
  
  modified_access <- modified_access %>%
    mutate(weighted_access = accessibility * C000)
  
  tot_jobs_modified <- sum(modified_access$accessibility)
  tot_weighted_jobs_modified <- sum(modified_access$weighted_access)
  avg_jobs_modified <- mean(modified_access$accessibility)
  avg_weighted_jobs_modified <- weighted.mean(modified_access$accessibility, modified_access$C000)
  
  pct_change <- (tot_jobs_modified - tot_jobs) / tot_jobs
  pct_change_weighted <- (tot_weighted_jobs_modified - tot_jobs_weighted) / tot_jobs_weighted
  avg_change_diff <- avg_jobs_modified - avg_jobs
  avg_weighted_change_diff <- avg_weighted_jobs_modified - avg_jobs_weighted
  
  Route <- i
  df <- data.frame(Route, pct_change, pct_change_weighted, avg_change_diff, avg_weighted_change_diff)
  
  summary_stats <- rbind(summary_stats, df)
  
  print(i)
  
  write.csv(summary_stats, "C:/Users/henrymckay/Desktop/RouteAccessibility/Summary.csv")
}

stop_r5(r5r_core)

rJava::.jgc(R.gc = TRUE)

summary_stats <- read.csv("C:/Users/henrymckay/Desktop/RouteAccessibility/Summary.csv")

network <- convert_shapes_to_sf(gtfs)

trips <- gtfs$trips 
trips <- merge(trips,
               summary_stats,
               by.x = "route_id",
               by.y = "Route",
               all.x = T)
trips <- merge(trips,
               network,
               by = "shape_id",
               all.x = T)

trips <- st_as_sf(trips)

map <- ggplot(trips) +
  geom_sf(size = (trips$pct_change_weighted * -35), color = "#204484") +
  labs(title = "Worker-Weighted Job Access by Route",
       subtitle = "Line Thickness = Higher Contributor to Job Access",
       caption = "Created using SacRT GTFS data and r5r") +
  theme_void()

map <- map + theme(
  plot.title = element_text(color = "grey23", size = 25, face = "bold"),
  plot.subtitle = element_text(color = "grey23", size = 15),
  plot.caption = element_text(color = "grey23", size = 15, face = "italic", hjust = 0),
  legend.title=element_text(color = "grey23", size=15, face = "bold")
)

# Set filepath to map output
filepath = paste0("/Users/henrymckay/Downloads/RouteMap.png")

# Save map as png
ggsave(filepath, 
       plot = map,
       dpi = 600,
       height = 8,
       width = 12,
       bg = 'white')


            