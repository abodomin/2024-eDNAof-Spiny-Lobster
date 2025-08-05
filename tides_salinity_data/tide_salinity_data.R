#set wd
setwd("/Users/aiko.abo.dominguez/Desktop/PAV1_eDNA_project/metadata")

#import csv with buoy data
buoy_data <- read.csv("/Users/aiko.abo.dominguez/Desktop/PAV1_eDNA_project/metadata/tides_salinity_data/FLBay_combined_dataframe_HH.csv")
buoy_data=buoy_data %>% rename(
  long=lon,
  site_id=site
)

head(buoy_data)

#import NOAA tide metadata
tide_meta <- read.csv("/Users/aiko.abo.dominguez/Desktop/PAV1_eDNA_project/metadata/tides_salinity_data/NOAAtide_meta.csv")
tide_meta$type=rep("tide",length(unique(tide_meta$site_id)))
head(tide_meta)

#import site metadata
site_meta <- read.csv("/Users/aiko.abo.dominguez/Desktop/PAV1_eDNA_project/metadata/site_metadata_PAV1_eDNA.csv")
head(site_meta)
site_meta$type <- rep("sample",length(unique(site_meta$site_id)))

#find unique latitudes and longitudes
buoy_meta <- data.frame(site_id= unique(buoy_data$site_id),
                        long=unique(buoy_data$long),
                        lat=unique(buoy_data$lat),
                        type=rep("buoy",length(unique(buoy_data$lat))))
buoy_meta

#combine aall metadata
combined_meta <- rbind(subset(site_meta,select=c(site_id,lat,long,type)),
  subset(tide_meta,select=c(site_id,lat,long,type)), 
                       subset(buoy_meta,select=c(site_id,lat,long,type)))
head(combined_meta)


#map all 
m <- leaflet(data = combined_meta) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    lng = ~long, lat = ~lat,
    radius = 4,
    color = "#2074ac", stroke = FALSE, fillOpacity = 0.7
  ) %>%
  addLabelOnlyMarkers(
    lng = ~long, lat = ~lat,
    label = ~site_id,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "top",   
      offset = c(0, 0), 
      textOnly = TRUE,
      style = list(
        "color" = "black",
        "font-size" = "14px",
        "font-family"="Arial"
      )
    )
  )
options(chromote.chrome = "/Applications/Google Chrome.app")
m

#select buoys closest to sample sites
install.packages("geosphere")
library(geosphere)
library(dplyr)

library(geosphere)
library(dplyr)

# Ensure site_meta and tide_meta contain columns: lat, long, site_id

sample_tides <- site_meta %>%
  rowwise() %>%
  mutate(
    closest_tide = {
      # Compute distances from this site to all tide_meta stations
      distances <- distHaversine(
        c(long, lat),
        matrix(c(tide_meta$long, tide_meta$lat), ncol = 2)
      )
      closest_index <- which.min(distances)
      # Return the actual site_id of the closest tide station
      tide_meta$site_id[closest_index]
    }
  ) %>%
  ungroup()


head(sample_tides)

#verify that the tide sites look close to eDNA sample sites 
test= tide_meta %>% 
  filter (site_id%in%sample_tides$closest_tide) 

m <- leaflet(data = test) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    lng = ~long, lat = ~lat,
    radius = 4,
    color = "#2074ac", stroke = FALSE, fillOpacity = 0.7
  ) %>%
  addLabelOnlyMarkers(
    lng = ~long, lat = ~lat,
    label = ~site_id,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "top",   
      offset = c(0, 0), 
      textOnly = TRUE,
      style = list(
        "color" = "black",
        "font-size" = "14px",
        "font-family"="Arial"
      )
    )
  )

m


###GETTING TIDE INFO
# Load required packages
if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("lubridate")) install.packages("lubridate")
if (!require("dplyr")) install.packages("dplyr")

library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)



# NOAA tide station info
#8723921
#8723970 heidi's query
station_id <- "8723970"  # Marathon, Boot Key Harbor
begin_date <- "20240624"
end_date   <- "20240627"
product   <- "predictions"
datum     <- "MLLW"
units     <- "metric"
#interval  <- "h"  # hourly predictions
#interval  <- "15"  # every 6 minutes
interval  <- "high/low"  # only high and low tides
time_zone <- "gmt"

# Build API request URL
base_url <- "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"

response <- GET(base_url, query = list(
  product = product,
  application = "r_script",
  begin_date = begin_date,
  end_date = end_date,
  station = station_id,
  time_zone = time_zone,
  units = units,
  interval = interval,
  format = "json",
  datum = datum
))

response

# Parse and clean data
tides_json <- content(response, as = "text")
#tides_json
tides_data <- fromJSON(tides_json)$predictions

tide_df <- tides_data %>%
  mutate(
    datetime_utc = ymd_hm(t),
    datetime_local = with_tz(datetime_utc, "America/New_York"),
    water_level_m = as.numeric(v)
  ) %>%
  select(datetime_local, water_level_m)

# View or export
print(head(tide_df, 10))

# Save to CSV
# write.csv(tide_df, "tides_marathon_June2024.csv", row.names = FALSE)


##do this all in for loop

#collect tide station IDs and format sample time
tide_data = sample_tides %>%
  mutate(
    station_id = as.character(closest_tide),
    begin_date = sampling_date %>%
      parse_date_time(orders = "m_d_Y", tz = "UTC") %>%
      format("%Y%m%d"),
    end_date = begin_date,
    date_parsed    = parse_date_time(sampling_date, orders = "m_d_Y", tz = "gmt"),
    hour_min       = hm(sample_time),
    datetime_local = as.character(date_parsed + hour_min),
    product = "predictions",
    datum   = "MLLW",
    units  = "metric",
    interval = "highlow",
    time_zone = "gmt"
  )

tide_data
write.csv(tide_data, "tides_PAV1_June2024.csv", row.names = FALSE)


#get tide database
base_url <- "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"

#storage df
tide_df <- data.frame(
  datetime_local= Date(length=length(sample_tides$site_id)), 
  water_level_m= numeric(length=length(sample_tides$site_id))
)

#for loop
for (i in 1:length(tide_data$site_id)) {
  response <- GET(base_url, query = list(
    product = tide_data$product[i],
    application = "r_script",
    begin_date = tide_data$begin_date[i],
    end_date = tide_data$end_date[i],
    station = tide_data$station_id[i],
    time_zone = tide_data$time_zone[i],
    units = tide_data$units[i],
    interval = tide_data$interval[i],
    format = "json",
    datum = tide_data$datum[i]
    ))
    
    
    # Parse and clean data
    tides_json <- content(response, as = "text")
    #tides_json
    tides_data <- fromJSON(tides_json)$predictions
    
    tide_df <- tides_data %>%
      mutate(
        datetime_utc = ymd_hm(t),
        datetime_local = with_tz(datetime_utc, "America/New_York"),
        water_level_m = as.numeric(v)
      ) %>%
      select(datetime_local, water_level_m)
  
  
}

# View or export
print(head(tide_df, 10))

# Save to CSV
# write.csv(tide_df, "tides_marathon_June2024.csv", row.names = FALSE)

