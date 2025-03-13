

################################################################################
# About


################################################################################

rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate, 
                 splitstackshape, geosphere, sf, leaflet)
gs4_auth()

#set dir
datdir <- "/Volumes/seaotterdb$/kelp_recovery/data/MBA_kelp_forest_database"

#read site table
site_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1EnLOhGma-IBsMr4nLl159BfLarCeHO25k2yhLnDsTow/edit?gid=134064181#gid=134064181",
                       sheet = 1) %>% clean_names()



################################################################################
#Create polygons surrounding each point

# Function to create a rectangle polygon given a center (lon, lat) and a heading.
create_rectangle <- function(lon, lat, heading, half_length = 30, half_width = 2) {
  # Define local coordinates for corners (x: along heading, y: perpendicular to right)
  # Order the corners to make a closed polygon:
  # bottom left, bottom right, top right, top left, then back to bottom left.
  local_corners <- matrix(c(
    -half_length, -half_width,
    half_length, -half_width,
    half_length,  half_width,
    -half_length,  half_width,
    -half_length, -half_width  # closing the polygon
  ), ncol = 2, byrow = TRUE)
  
  # Convert heading from degrees to radians for computation
  heading_rad <- heading * pi/180
  
  # For each local offset (x, y), compute the displacement in the east and north directions.
  # Given that in our coordinate system:
  #   - The unit vector along the heading is: (sin(heading_rad), cos(heading_rad))
  #   - The perpendicular (to the right) is: (cos(heading_rad), -sin(heading_rad))
  # The global displacement (in meters) will be:
  #   dx = x*sin(heading_rad) + y*cos(heading_rad)
  #   dy = x*cos(heading_rad) - y*sin(heading_rad)
  # Then, we can compute the bearing (from north, clockwise) and the distance from the center.
  
  corners <- t(apply(local_corners, 1, function(pt) {
    x <- pt[1]
    y <- pt[2]
    # Calculate east and north offsets
    dx <- x * sin(heading_rad) + y * cos(heading_rad)
    dy <- x * cos(heading_rad) - y * sin(heading_rad)
    # Distance is just the Euclidean norm (which is valid for these small distances)
    distance <- sqrt(dx^2 + dy^2)
    # Bearing: note that atan2 expects (x, y) to compute the angle from north.
    bearing <- (atan2(dx, dy) * 180/pi) %% 360
    # Use geosphere's destPoint() to get the destination coordinates
    geosphere::destPoint(c(lon, lat), b = bearing, d = distance)
  }))
  
  # Create and return an sf polygon from the computed coordinates.
  poly <- st_polygon(list(corners))
  return(poly)
}


site_raw <- site_raw %>%
  mutate(latitude = as.numeric(unlist(latitude)))

# Convert the data frame to an sf object (EPSG:4326)
site_sf <- st_as_sf(site_raw, coords = c("longitude", "latitude"), crs = 4326)

# Create rectangle polygons for each point using the DC heading
# The mapply function applies the function to each row of coordinate and heading
site_sf$rectangle <- mapply(create_rectangle,
                            lon = st_coordinates(site_sf)[,1],
                            lat = st_coordinates(site_sf)[,2],
                            heading = site_sf$dc_heading,
                            SIMPLIFY = FALSE)


site_sf$rectangle_geom <- st_sfc(site_sf$rectangle, crs = 4326)

# Now, site_sf has a geometry column 'rectangle_geom' that you can plot or use in spatial operations.
print(site_sf)




################################################################################
#Visualize

# Set the active geometry to the rectangle polygons
site_polys <- st_set_geometry(site_sf, site_sf$rectangle_geom)

leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addPolygons(data = site_polys,
              color = "red",
              weight = 2,
              fillOpacity = 0.3,
              popup = ~site_long) %>% 
  addCircleMarkers(data = site_sf,
                   radius = 5,
                   color = "indianred",
                   stroke = TRUE,
                   fillOpacity = 1,
                   popup = ~site_long)







################################################################################
#Export

# Specify the fields to export (adjust as needed)
fields_to_keep <- c("site_long", "survey_type", "region", "site_name", 
                    "site_type", "zone", "target_depth_meters", 
                    "uc_heading", "dc_heading", "feature_type")

# Select and clean the attributes (convert any list columns to character)
combined_sf_clean2 <- combined_sf %>%
  select(all_of(fields_to_keep), geometry) %>%
  # If any column is still a list, collapse its elements into a single string
  mutate(across(-geometry, ~ if (is.list(.)) sapply(., function(x) paste(x, collapse = ",")) else .)) %>%
  # Convert all attributes (except geometry) to character
  mutate(across(-geometry, as.character))



# Create a new sf object with custom Name and Description fields.
combined_sf_for_kml <- combined_sf_clean2 %>%
  mutate(
    # Set Name from site_long (or whichever field you want as the placemark title)
    Name = site_long,
    # Create an HTML formatted Description that includes all desired fields.
    Description = paste0(
      "<strong>Survey Type:</strong> ", survey_type, "<br>",
      "<strong>Region:</strong> ", region, "<br>",
      "<strong>Site Name:</strong> ", site_name, "<br>",
      "<strong>Site Type:</strong> ", site_type, "<br>",
      "<strong>Zone:</strong> ", zone, "<br>",
      "<strong>Target Depth (m):</strong> ", target_depth_meters, "<br>",
      "<strong>UC Heading:</strong> ", uc_heading, "<br>",
      "<strong>DC Heading:</strong> ", dc_heading, "<br>"
    )
  ) %>%
  # Keep only the fields we want to export to KML
  select(Name, Description, geometry)

# Export the KML file using the Name field for placemarks.
st_write(combined_sf_for_kml,
         here::here(datdir, "processed/recovery/recovery_polygons.kml"),
         driver = "KML",
         dataset_options = "NameField=Name",
         delete_dsn = TRUE)





