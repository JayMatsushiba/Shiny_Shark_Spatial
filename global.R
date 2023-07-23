library(dplyr)
library(sf)

# hexagon grid cell import
# hexagons <- st_read("data/03_HexagonTessellation/CylindricalEqualArea_Hexagons.shp")
# land <- st_read("data/04_NaturalEarthLand/ne_10m_land.shp")
hexagons <- readRDS("data/rli_richness_hex.rds") 

areas <- hexagons

# lot(hexagons$geometry)
# st_transform_proj(crs = "+proj=cea +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs")
allzips <- readRDS("data/superzip.rds")
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$college <- allzips$college * 100
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
row.names(allzips) <- allzips$zipcode

cleantable <- allzips %>%
  select(
    City = city.x,
    State = state.x,
    Zipcode = zipcode,
    Rank = rank,
    Score = centile,
    Superzip = superzip,
    Population = adultpop,
    College = college,
    Income = income,
    Lat = latitude,
    Long = longitude
  )
