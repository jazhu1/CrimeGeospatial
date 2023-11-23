# Libraries needed
library(sf)
library(dplyr)
library(tmap)

# Read in shapefiles
neighborhoods <- st_read("Neighborhoods.shp")
zipcode <- st_read("zip_codes.shp")
grocery <- st_read("Grocery_Stores_in_City_of_Detroit_Public_View.shp")
crime <- st_read("RMS_Crime_Incidents.shp")

# Task 1.1

# Filter the crime incidents that occurred in 2021
crime_data <- crime %>% filter(year == 2021)

# Reproject the neighborhood and crime data
neighborhoods_data <- st_transform(neighborhoods, crs = 5623)
crime_data <- st_transform(crime_data, crs = 5623)

# Group the neighborhoods within the crime shapefile to obtain the counts for crime in each neighborhood
crime_counts <- crime_data %>%
  group_by(neighborho) %>%
  summarise(count = n())

# Join the crime counts to the neighborhood data
new_neighborhood_data <- st_join(neighborhoods_data, crime_counts, 
                                 join = st_intersects, left = FALSE)

# Color the neighborhoods that have less than 100 crime incidents in 2021 with ‘green’,
# and neighborhoods that have more than 1200 crime incidents in 2021 with ‘red’
new_neighborhood_data$color <- ifelse(new_neighborhood_data$count < 100, "green", 
                                      ifelse(new_neighborhood_data$count > 1200, "red", "gray"))

# Create the map
tm_shape(new_neighborhood_data) +
  tm_polygons(col = "color") +
  tm_layout(frame = FALSE) +
  tm_legend(outside = TRUE) +
  tm_text("nhood_name", size = 0.5)

# Task 1.2

# group crime incidents by zip code
crime_counts2 <- crime_data %>%
  group_by(zip_code) %>%
  summarise(count = n())

# join crime counts to zip code data
zipcode_p <- st_transform(zipcode, crs = 5623)
new_zipcode_data <- st_join(zipcode_p, crime_counts2, by = c("zipcode" = "zip_code"))


# add a color column based on the count of crime incidents
new_zipcode_data$color <- ifelse(new_zipcode_data$count < 1000, "green", 
                                 ifelse(new_zipcode_data$count > 5000, "red", "gray"))

# plot the map
tm_shape(new_zipcode_data) +
  tm_polygons(col = "color") +
  tm_layout(frame = FALSE) +
  tm_legend(outside = TRUE) +
  tm_text("zipcode", size = 0.5)

# task 2.1

# create 1 mile, 2 mile, and 3 mile buffers
buffer1 <- st_buffer(grocery_p, dist = 1609)  # 1 mile = 1609.34 meters
buffer2 <- st_buffer(grocery_p, dist = 3218)  # 2 miles = 3218.69 meters
buffer3 <- st_buffer(grocery_p, dist = 4828)  # 3 miles = 4828.03 meters

# using opaqueness to color the buffer zones

tm_shape(neighborhoods_p) + tm_polygons(alpha = 0.9) +
  tm_shape(grocery_p) + tm_dots(col = "blue", size = 0.1) +
  tm_shape(buffer1) + tm_borders(col = "red") + tm_fill(col = "red", alpha = 0.8) +
  tm_shape(buffer2) + tm_borders(col = 'red') + tm_fill(col = 'red', alpha = 0.5) +
  tm_shape(buffer3) + tm_borders(col = 'red') + tm_fill(col = 'red', alpha = 0.2)


#task 2.2

# filtering for "Robbery" type crime within column offense_de in 2021   
crime_data2 <- crime_data %>% filter(year == 2021, `offense_de` == "ROBBERY")

# reprojecting data
crime_data2 <- st_transform(crime_data, crs = 5623)

# viewing robbery in relation to buffered zones of grocery stores
tm_shape(neighborhoods_p) + tm_polygons(alpha = 0.9) +
  tm_shape(grocery_p) + tm_dots(col = "blue", size = 0.1) +
  tm_shape(buffer1) + tm_borders(col = "red") + tm_fill(col = "red", alpha = 0.8) +
  tm_shape(buffer2) + tm_borders(col = 'red') + tm_fill(col = 'red', alpha = 0.5) +
  tm_shape(buffer3) + tm_borders(col = 'red') + tm_fill(col = 'red', alpha = 0.2) +
  tm_shape(crime_data2) + tm_dots(col = 'green')


# getting number of robbery within each radius of grocery store
robbery_within_1m <- st_within(crime_data2, buffer1, sparse = FALSE) # Sparse geometry binary predicate (sgbp)
robbery_within_2m <- st_within(crime_data2, buffer2, sparse = FALSE)
robbery_within_3m <- st_within(crime_data2, buffer3, sparse = FALSE)


# number of robbery within 1,2,3 miles of grocery store
robbery_within_1m <- apply(X = robbery_within_1m, MARGIN=2,FUN=sum)
robbery_within_2m <- apply(X = robbery_within_2m, MARGIN=2,FUN=sum)
robbery_within_3m <- apply(X = robbery_within_3m, MARGIN=2,FUN=sum)


# bind grocery store data to robbery location
grocery_robbery <- cbind(grocery_p, robbery_within_1m, robbery_within_2m, robbery_within_3m)

colnames(grocery_robbery)

# selecting the 5 columns for new df
grocery_robbery1 <- grocery_robbery %>% 
  select(Store_Name, Address, robbery_within_1m, robbery_within_2m, robbery_within_3m)


# dropping the geometry column
grocery_robbery1 <- st_drop_geometry(grocery_robbery1)

# descriptive statistics 
summary(grocery_robbery1)

# save data
save(grocery_robbery1, file = "detroit grocery stores and robbery.RData")

library(writexl)
write_xlsx(grocery_robbery1, 'detroit grocery stores and robbery.xlsx')
