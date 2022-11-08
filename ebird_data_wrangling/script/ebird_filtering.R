#### 2022-09-21 eBird data filtering for birds and carbon hotspots project ####

setwd("E:/city_bird_hotspots/ebird_data_wrangling")

## EBird data and sampling event data were first filtered to wanted columns only using bash to reduce file size

# $ awk 'BEGIN {FS="\t";OFS=FS} {print $6,$7,$8,$9,$11,$16,$17,$18,$19,$29,$30,$31,$32,$33,$34,$35,$38,$39,$40,$41,$42,$43}' ebd_CA_relAug-2022.txt > ebird_data.txt

# awk 'BEGIN {FS="\t";OFS=FS} {print $2,$3,$4,$5,$15,$16,$17,$18,$19,$20,$21,$24,$25,$26,$27,$28,$29}' ebd_sampling_relAug-2022.txt > ebird_sampling.txt

## https://cornelllabofornithology.github.io/ebird-best-practices/ebird.html ##

#### Filtering the EBD using auk ####

install.packages("remotes")
install.packages("auk")
remotes::install_github("mstrimas/ebppackages")

# Set ebd path
auk::auk_set_ebd_path("E:/city_bird_hotspots/ebird_data_wrangling", overwrite = TRUE)

library(auk)
library(lubridate)
library(sf)
library(tidyverse)

# Resolve namespace conflicts
select <- dplyr::select

# Setup data directory

ebd <- auk_ebd("data/ebird_data.txt", 
               file_sampling = "data/ebird_sampling.txt")


#https://www150.statcan.gc.ca/n1/en/catalogue/92-160-X
# Cartographic files depict the geographical areas using only the major land mass of Canada and its coastal islands. The files provide a framework for mapping and spatial analysis using commercially available geographic information systems (GIS) or other mapping software.The Boundary Files are portrayed in Lambert conformal conic projection and are based on the North American Datum of 1983 (NAD83). A reference guide is available (92-160-G).
# 2021 Census Boundary files: https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21

## Census metropolitan areas and census agglomerations selected ##

CMAs <- st_read("E:/city_bird_hotspots/ebird_data_wrangling/CMAs.shp") # Projected CRS: NAD83 / Statistics Canada Lambert

#### EBD filters https://cornelllabofornithology.github.io/auk/reference/index.html#section-filter ####

# Years to filter to
years <- c(2016:2021)

# Check eBird protocols
valid_protocols

# define filters to be used 
ebd_filters <- ebd %>% 
  #only choose observations from Canada
  auk_country("Canada") %>% 
  #only choose observations within census metropolitan areas
  auk_bbox(bbox = CMAs) %>%
  # get data from above decided years
  auk_year(year = years) %>% 
  # restrict to the standard traveling and stationary count protocols
  auk_protocol(protocol = c("Stationary", "Traveling", "Area","Traveling - Property Specific","Random")) %>% 
  # only keep checklists that are less than 1 km long (note that stationary checklists (i.e. point counts) have no    distance associated with them, however, since these checklists can be assumed to have 0 distance they will be kept   if 0 is in the range defined by distance)
  auk_distance(distance = c(0, 1)) %>% 
  # only keep checklists that are between 5 min and 240 min long
  auk_duration(duration = c(5, 240)) %>%
  #keep complete checklists only
  auk_complete()

ebd_filters

#### use auk_filter() to compile the filters into an AWK script and run it to produce two output files: one for the EBD and one for the SED ####

dir.create("data", showWarnings = FALSE)
auk_set_awk_path("C:/cygwin64/bin/gawk.exe/bin/gawk.exe", overwrite=TRUE) 
#https://github.com/CornellLabofOrnithology/auk/issues/30 and restart

# output files
data_dir <- "data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}
f_ebd <- file.path(data_dir, "output/ebd_filtered.txt")
f_sampling <- file.path(data_dir, "output/ebd_checklists_filtered.txt")

# only run if the files don't already exist
if (!file.exists(f_ebd)) {
  auk_filter(ebd_filters, file = f_ebd, file_sampling = f_sampling, keep = c("common name", "scientific name", "observation count", "state","latitude", "longitude","observation date","observer id","sampling event identifier", "duration minutes", "effort distance km", "effort area ha", "all species reported", "number observers", "group identifier"))
}

# The results were output to a file, which you can read in with read_ebd()

ebd_filt <- read_ebd("output/ebd_filtered.txt")

## https://www.r-bloggers.com/2020/04/extracting-ebird-data-from-a-polygon/ ##

#### split ebird dataset into two nearly equal parts on bash ####

# cd E:/city_bird_hotspots/ebird_data_wrangling/output
# split -b 1375245.5k ebd_filtered.txt uniq_dfiltered.txt

#### subset both the EBD and sampling event data files separately to points within the polygon, then combine them together and zero-fill with auk_zerofill() ####

## Keep only observations of breeding passerines in urban areas across the US ##

# spatial join of previously compiled species trait data to urban area CMA shape files provided by the US Census Bureau 

library(sp)
library(rgdal)

#### read in first half of bird data (uniq_dfiltered.txt), and turn it into a SpatialPointsDataFrame ####

ebd_filt1 <- read_ebd("output/uniq_dfiltered.txt")

coordinates(ebd_filt1) <- c("longitude", "latitude")
proj4string(ebd_filt1) <- CRS("+proj=longlat +datum=WGS84")

# read in  polygons --> this is the urban area shape file 

# CMAs <- st_read("E:/city_bird_hotspots/ebird_data_wrangling/CMAs.shp") # Projected CRS: NAD83 / Statistics Canada Lambert
CMAs  <- readOGR("E:/city_bird_hotspots/ebird_data_wrangling/CMAs.shp")# Projected CRS: NAD83 / Statistics Canada Lambert

proj4string(CMAs)

# To transform from one CRS to another so that coordinates are in the same lat/lon reference system
species1 <- spTransform(ebd_filt1, CRS("+proj=lcc +lat_0=63.390675 +lon_0=-91.8666666666667 +lat_1=49 +lat_2=77 +x_0=6200000 +y_0=3000000 +datum=NAD83 +units=m +no_defs"))
proj4string(species1)

#plot(CMAs) #plot CMAs
#plot(species1, add=TRUE, col="red") #overlay the species points
memory.limit()
gc()

#spatial join of species over CMAs
Species2 <- over(species1,CMAs) #18849534 obs.

#store the city name as an attribute of the birds data
Species3<-cbind(species1@data,Species2$CMANAME)
#keep species latitude and longitude also
Species4<-cbind(species1@coords,Species3)
df <- as.data.frame(Species4) #9947705 obs
summary(df)
str(df)
#remove birds not in CMAs
subs1 <- df[complete.cases(df$`Species2$CMANAME`), ] #4854449 obs Return a logical vector indicating which cases are complete, i.e., have no missing values.

write.csv(subs1, file="data/birds_in_CMAs1.csv")

summary(subs1)
# subset data frame
ebd_in_CMAs <- ebd_filt[in_CMAs[, 1], ]
# ebd_in_CMAs_df <- as.data.frame(ebd_in_CMAs)
# write.csv(ebd_in_CMAs_df, "ebd_in_CMAs_df.csv")


# clear R environment at this point #
#{ echo -e "COMMON NAME,SCIENTIFIC NAME,OBSERVATION COUNT,STATE,LATITUDE,LONGITUDE,OBSERVATION DATE,OBSERVER ID,SAMPLING EVENT IDENTIFIER,DURATION MINUTES,EFFORT DISTANCE KM,EFFORT AREA HA,NUMBER OBSERVERS,ALL SPECIES REPORTED,GROUP IDENTIFIER"; cat uniq_dfiltered2.txt; } >dfiltered.txt

#cat dfiltered.txt | tr "\\t" "," > filtered1.txt

#### read in 2nd half of bird data (filtered1.txt), and turn it into a SpatialPointsDataFrame ####

ebd_filt1 <- read_ebd("data/filtered1.txt", sep = ",", unique = TRUE, rollup = TRUE) #10052653 obs

coordinates(ebd_filt1) <- c("longitude", "latitude")
proj4string(ebd_filt1) <- CRS("+proj=longlat +datum=WGS84")

# read in  polygons --> this is the CMAs shape file 

CMAs  <- readOGR("E:/city_bird_hotspots/ebird_data_wrangling/CMAs.shp")

proj4string(CMAs)

#To transform from one CRS to another so that coordinates are in the same lat/lon reference system

species1 <- spTransform(ebd_filt1, CRS("+proj=lcc +lat_0=63.390675 +lon_0=-91.8666666666667 +lat_1=49 +lat_2=77 +x_0=6200000 +y_0=3000000 +datum=NAD83 +units=m +no_defs"))
proj4string(species1)

#plot(CMAs) #plot CMAs

memory.limit(100000000000000)
gc()

#spatial join of species over CMAs
Species2 <- over(species1,CMAs) 

#store the city name as an attribute of the birds data
Species3<-cbind(species1@data,Species2$CMANAME)
#keep species latitude and longitude also
Species4<-cbind(species1@coords,Species3)
df <- as.data.frame(Species4) 
summary(df)
str(df)
#remove birds not in CMAs
subs1 <- df[complete.cases(df$`Species2$CMANAME`), ] #5188735 obs Return a logical vector indicating which cases are complete, i.e., have no missing values.

write.csv(subs1, file="data/birds_in_CMAs3.csv") #NAD83


#### convert sampling data to sf and identify points within polygons ####

# convert sampling data to sf object

sampling <- read_sampling("data/ebd_checklists_filtered.txt", unique = TRUE) #1935412 obs.

coordinates(sampling) <- c("longitude", "latitude")
proj4string(sampling) <- CRS("+proj=longlat +datum=WGS84")
proj4string(CMAs)

#To transform from one CRS to another so that coordinates are in the same lat/lon reference system
sampling1 <- spTransform(sampling, CRS("+proj=lcc +lat_0=63.390675 +lon_0=-91.8666666666667 +lat_1=49 +lat_2=77 +x_0=6200000 +y_0=3000000 +datum=NAD83 +units=m +no_defs"))
proj4string(sampling1)

#spatial join of checklists over CMAs
sampling2 <- over(sampling1,CMAs) # obs.
#store the city name as an attribute of the birds data
sampling3 <- cbind(sampling1@data,sampling2$CMANAME)
#keep checklists latitude and longitude also
sampling4 <- cbind(sampling1@coords,sampling3)
df <- as.data.frame(sampling4) 
summary(df)
str(df)
#remove checklists not in CMAs
subs1 <- df[complete.cases(df$`sampling2$CMANAME`), ] #990607 obs Return a logical vector indicating which cases are complete, i.e., have no missing values.

write.csv(subs1, file="data/checklists_in_CMAs.csv") #NAD83

#write.csv(df, file="data/checklists_in_CMAs1.csv") #NAD83
# use auk_zerofill() to read these two files into R and combine them together to produce zero-filled, detection/non-detection data (also called presence/absence data. by default auk_zerofill() returns a compact representation of the data, consisting of a list of two data frames, one with checklist data and the other with observation data; the use of collapse = TRUE combines these into a single data frame, which will be easier to work with
library(readr)
birds1 <- read_csv("data/birds_in_CMAs1.csv", 
                   col_types = cols(...1 = col_skip(), effort_area_ha = col_double()), 
                   na = "empty") #4854449 obs.

birds2 <- read_csv("data/birds_in_CMAs3.csv", 
                   col_types = cols(...1 = col_skip(), effort_area_ha = col_double()), 
                   na = "empty")
sampling <- read_csv("data/checklists_in_CMAs1") #1935412 obs.
sampling <- read_sampling("data/ebd_checklists_filtered.txt") #1935412 obs.

dir.create("output", showWarnings = FALSE)

# output files
data_dir <- "output"

if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}

ebd_zf_1 <- file.path(data_dir, "ebd_zf_1.csv")
ebd_zf_2 <- file.path(data_dir, "ebd_zf_2.csv")

# only run if the files don't already exist
ebd_zf_1<- if (!file.exists(ebd_zf_1)) {
  auk_zerofill(birds1, checklists_in_CMAs1, rollup = FALSE, collapse = TRUE)
}
write.csv(ebd_zf_1, file="ebd_zf_1.csv")

if (!file.exists(ebd_zf_2)) {
  auk_zerofill(birds2, sampling, rollup = FALSE, collapse = TRUE)
}

#ebd_zf_1 <- auk_zerofill(birds1, sampling, rollup = FALSE, collapse = TRUE)

# ebd_zf_df <- as.data.frame(ebd_zf)

# write.csv(ebd_zf_df, file="data/ebird_zf.csv")

summary(ebd_zf)

# transform some of the variables to a more useful form for modelling.eBirders have the option of entering an âXâ rather than a count for a species, to indicate that the species was present, but they didnât keep track of how many individuals were observed. During the modeling stage, weâll want the observation_count variable stored as an integer and weâll convert âXâ to NA to allow for this.

# clean up variables
ebd_zf <- ebd_zf %>%     #4755025 obs.
  mutate(
    # convert X to NA
    observation_count = if_else(observation_count == "X", 
                                NA_character_, observation_count),
    observation_count = as.integer(observation_count),
    # split date into year and day of year
    year = year(observation_date),
    day_of_year = yday(observation_date)
  )
summary(ebd_zf)

# additional filtering
ebd_zf_filtered <- ebd_zf %>%     #4733025 obs.
  filter(
    # 10 or fewer observers
    number_observers <= 10)

summary(ebd_zf_filtered)

# Call renv::snapshot() to save the state of your project library if your attempts to update R packages were successful 
renv::snapshot()


