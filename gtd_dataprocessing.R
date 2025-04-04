# Set working directory
setwd("C:/Users/Lukas/iCloudDrive/HfP_TUM Bachelor+Master/Master/0 Master Thesis/R/gtd_dataprocessing")
#setwd("/Users/lukas/Library/Mobile Documents/com~apple~CloudDocs/HfP_TUM Bachelor+Master/Master/0 Master Thesis/R/gtd_visualization")

#### LOAD DATA AND BASIC PROCESSING ####

# Load packages under the condition that they have not been loaded before
packages <- c("dplyr", "readxl", "leaflet", "sf", "h3jsr")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

library(dplyr) # For data manipulation using dplyr
library(readxl) # For reading in gtd_germany_extremism_categorization
library(sf) # For storing and analyzing spatial data
library(h3jsr) # For working with the H3 geospatial indexing system (polygons)
library(leaflet) # For visualizing geospatial data

# Load data
gtd_full <- read.csv("gtd_0522.csv", sep = ";")

# List unique country names in alphabetic order (check name for West & East Germany pre 1990)
sort(unique(gtd_full$country_txt)) # East Germany (GDR), West Germany (FRG), Germany

# Create subset for Germany - filter column country_txt by string "Germany"
gtd_germany <- gtd_full %>% filter(grepl("Germany", country_txt))

# List all variables in dataset
colnames(gtd_germany) # 135 variables

# Replace missing values with NA
gtd_germany[gtd_germany == ""] <- NA

# Remove gtd_full from environment
rm(gtd_full)





#### FORMAT DATES ####

# Check for how many incidents the exact month is unclear (imonth == 0 - see gtd codebook for approxdate)
sum(gtd_germany$imonth == 0) # 0 incidents

# Check for how many incidents the exact day is unclear (iday == 0 - see gtd codebook for approxdate)
sum(gtd_germany$iday == 0) # 7 incidents

# Create subset with incidents where the exact date is unknown 
day_unclear <- gtd_germany %>% filter(iday == 0)

# Check if approximate dates are available for those incidents
day_unclear$approxdate # only one entry in approxdate, which is "December 2009" and therefore of no help identifying the exact date

# Check if there are sources for those incidents
day_unclear$scite1 # only source for one incident available

# Decision: create additional column "dayunclear" (0/1) to mark that the exact day is unknown for entries with iday == 0, then set iday to "1" for each entry to enable visualization
gtd_germany <- gtd_germany %>%
  mutate(
    dayunclear = ifelse(iday == 0, 1, 0),
    iday = ifelse(iday == 0, 1, iday)
  )

# Drop column "approxdate"
gtd_germany <- gtd_germany %>%
  select(-approxdate)

# Remove day_unclear from environment
rm(day_unclear)

# Create new column "date" and format iyear, imonth, iday as date, then remove iyear, imonth, iday
gtd_germany <- gtd_germany %>%
  mutate(date = as.Date(paste(iyear, imonth, iday, sep = "-"), format = "%Y-%m-%d")) %>%
  select(-iyear, -imonth, -iday)





#### ENSURE AVAILABILITY OF LATITUDE AND LONGITUDE FOR ALL INCIDENTS ####

# Check for how many incidents the latitude or longitude is missing
sum(is.na(gtd_germany$latitude)) # 35
sum(is.na(gtd_germany$longitude)) # 35

# Create a dataframe with rows where latitude and/or or longitude are missing
lat_long_NA <- gtd_germany %>%
  filter(is.na(latitude) | is.na(longitude))

# After research, the exact location of the incidents could not be identified.
# Therefore, add a column "location_unclear" (values 0 or 1).
gtd_germany <- gtd_germany %>%
  mutate(locationunclear = ifelse(is.na(latitude) & is.na(longitude), 1, 0)) # if lat/long missing, location_unclear is 1, otherwise 0.

# If location_unclear is 0, add coordinates for the North Sea to ensure display in shiny app
gtd_germany <- gtd_germany %>%
  mutate(
    latitude = ifelse(locationunclear == 1, "54,193719", latitude),
    longitude = ifelse(locationunclear == 1, "7,582290", longitude)
  )

# Check again for how many incidents the latitude or longitude is missing
sum(is.na(gtd_germany$latitude)) # 0 --> updating coordinates worked
sum(is.na(gtd_germany$longitude)) # 0 --> updating coordinates worked

# Convert latitude and longitude into numeric values
gtd_germany$latitude <- as.numeric(gsub(",", ".", gtd_germany$latitude))
gtd_germany$longitude <- as.numeric(gsub(",", ".", gtd_germany$longitude))

# Remove "lat_long_NA"
rm(lat_long_NA)





#### CATEGORIZE ALL GROUPS IN VARIABLE gname ACCORDING TO PHENOMENON AREAS (RIGHT-WING EXTREMISM, LEFT-WING EXTREMISM, ISLAMISM, FOREIGN EXTREMISM, OTHER/UNCATEGORIZED) ####

# Check for availability of sources from GTD:
sources <- gtd_germany %>%
  select(gname, scite1) %>%
  arrange(gname)
View(sources)

# If sources are unavailable, check for the following variables in order to search in Google:
# eventid, iday, imonth, iyear, city, attacktype1_txt, targtype1_txt, targsubtype1_txt, target1, corp1, weaptype1_txt, weapsubtype1_txt, motive

# Create list with all individual values of gname
gname_list <- sort(unique(gtd_germany$gname))

# Iteration across all values in gname_list
for (name in gname_list) {
  result <- gtd_germany %>%
    filter(gname == name) %>%
    select(gname, eventid, date, city, attacktype1_txt, targtype1_txt, targsubtype1_txt, 
           target1, corp1, weaptype1_txt, weapsubtype1_txt, motive)
  
  if (nrow(result) > 0) {
    cat("\n=========================\n")
    cat("Ergebnisse für:", name, "\n")
    cat("=========================\n")
    print(result)
  }
}

# Remove sources and result from environment
rm(sources, result)





#### MERGE CATEGORIZATION DATA WITH GTD_GERMANY ####

# Load data for extremism categorization
gtd_germany_extremism_categorization <- read.csv2("gtd_germany_extremism_categorization.csv")

# Merge based on "gname"
gtd_germany <- gtd_germany %>%
  left_join(gtd_germany_extremism_categorization %>% select(gname, phenomenon_area), by = "gname")

# Remove gtd_germany_extremism_categorization from environment
rm(gtd_germany_extremism_categorization)





#### DELETE COLUMNS OF NO INTEREST ####

# Cat I - "GTD ID and Date":

# keep:
# - eventid

# delete:
# - iyear (date previously formatted, not required anymore)
# - imonth (date previously formatted, not required anymore)
# - iday (date previously formatted, not required anymore)
# - approxdate (dayunclear column was added)
# - extended (for events longer than 1 day - lose because "end time" would be difficult to visualize)
# - resolution (only applies if "extended" is "Yes"/1)


# Cat II - "Incident Information":

# keep:
# - doubtterr (only systematically available after 1997. But still 149 incidents with uncertainty. Could be important for qualitative analysis)
# - alternative_txt (category for why doubtterr is positive)

# delete:
# - crit1 (POLITICAL, ECONOMIC, RELIGIOUS, OR SOCIAL GOAL) (not necessary for this project)
# - crit2 (INTENTION TO COERCE, INTIMIDATE OR PUBLICIZE TO LARGER AUDIENCE(S)) (not necessary for this project)
# - crit3 (OUTSIDE INTERNATIONAL HUMANITARIAN LAW) (not necessary for this project)
# - summary (only available for entries after 1997, majority of incidents is pre 1997)
# - alternative (same as alternative_text, but with values ranging from 1-5)
# - multiple (when multiple incidents are connected) (not necessary for this project)
# - related (comma-separated list of related incidents) (not necessary for this project)


# Cat III - "Incident Location"

# keep:
# - city
# - latitude
# - longitude

# delete:
# - country (already filtered for only Germany)
# - country_txt (already filtered for only Germany)
# - region (already filtered for only Germany)
# - region_txt (already filtered for only Germany)
# - provstate (not necessary for this project)
# - vicinity (covered by lat/long variables)
# - location (covered by lat/long variables)
# - specificity  (not necessary for this project)


# Cat IV - "Attack Information"

# keep:
# - attacktype1_txt
# - attacktype2_txt (only 4 incidents)
# - success
# - suicide

# delete:
# - attacktype1 (attacktype1_txt is sufficient)
# - attacktype2 (attacktype2_txt is sufficient)
# - attacktype3 (all NA)
# - attacktype3_txt (all NA)


# Cat V - "Weapon Information"

# keep:
# - weaptype1_txt
# - weapsubtype1_txt
# - weaptype2_txt
# - weapsubtype2_txt
# - weaptype3_txt (only 5 entries)
# - weapsubtype3_txt
# - weapdetail (for qualitative analysis)

# delete:
# - weaptype1 (weaptype1_txt is sufficient)
# - weapsubtype1 (weapsubtype1_txt is sufficient)
# - weaptype2 (weaptype2_txt is sufficient)
# - weapsubtype2 (weapsubtype2_txt is sufficient)
# - weaptype3 (weaptype3_txt is sufficient)
# - weapsubtype3 (weapsubtype3_txt is sufficient)
# - weaptype4 (all NA)
# - weapsubtype4 (all NA)


# Cat VI - "Target/Victim Information"

# keep:
# - corp1 (name of entity)
# - target1 (specific target/victim)
# - targtype1_txt
# - targtype2_txt
# - corp2 (name of entity)
# - target2 (specific target/victim)
# - targtype3_txt
# - corp3 (name of entity)
# - target3 (specific target/victim)

# delete:
# - targtype1 (targtype1_txt is sufficient)
# - targsubtype1 (not necessary for this project)
# - targsubtype1_txt (not necessary for this project)
# - natlty1 (not necessary for this project)
# - natlty1_txt (not necessary for this project)
# - targtype2 (targtype2_txt is sufficient)
# - targsubtype2 (not necessary for this project)
# - targsubtype2_txt (not necessary for this project)
# - natlty2 (not necessary for this project)
# - natlty2_txt (not necessary for this project)
# - targtype3 (targtype3_txt is sufficient)
# - targsubtype3 (not necessary for this project)
# - targsubtype3_txt (not necessary for this project)
# - natlty3 (not necessary for this project)
# - natlty3_txt (not necessary for this project)


# Cat VII - "Perpetrator Information"

# keep:
# - gname (perpetrator group name)
# - individual (carried out by individual(s) not affiliated with a group)

# delete:
# - gsubname (after qualitative analysis decided it is not necessary - mainly RAF)
# - gname2 (only 4 entries, after qualitative analysis decided it does not add valuable information)
# - gsubname2 (not included)
# - gname3 (all NA)
# - gsubname3 (all NA)
# - guncertain1 (if the claims about perpetrators are dubious/uncertain) (not necessary for this project)
# - guncertain2 (not included)
# - guncertain3 (all NA)
# - claimmode (way in which responsibility was claimed - does not add valuable information)
# - clammode_txt (not included)
# - compclaim (all unknown or NA)
# - claim2 (all NA)
# - claimmode2 (all NA)
# - claim3 (all NA)
# - claimmode3 (all NA)
# - nperps (number of perpetrators) (not necessary for this project, since the number is misleading - 700 Neo Nazi extremists - demonstration...?)
# - nperpcap (number of perpetrators captured) (not necessary for this project)
# - claimed (if responsibility was claimed) (not necessary for this project)
# - motive (not necessary for this project)


# Cat VIII - "Casualties and Consequences"

# keep:
# - nkill (total number of fatalities)
# - nkillter (number of perpetrator fatalities)
# - nwound (number of wounded)
# - nwoundte (number of perpetrators wounded)

# delete:
# - nkillus (number of U.S. citizens killed - not important in this research)
# - nwoundus (number of U.S. citizens wounded - not important in this research)
# - propextent (propextent_txt is sufficient)
# - propvalue (propextent_txt is sufficient)
# - propcomment (propextent_txt is sufficient)
# - nhostkidus (number of U.S. citizens kidnapped - not important in this research)
# - ransompaidus (Ransom paid by U.S. sources - not important in this research)
# - ransomnote (all NA)
# - hostkidoutcome (hostkidoutcome_txt is sufficient)
# - property (1/0/-9 if property damage) (not necessary for this project)
# - propextent_txt (extent of property damage) (not necessary for this project)
# - ishostkid (hostage or kidnapping victims - yes/no) (not necessary for this project)
# - nhostkid (number of hostage/kidnapping victims) (not necessary for this project)
# - nhours (hours for hostage/kidnapping) (not necessary for this project)
# - ndays (days for hostage/kidnapping) (not necessary for this project)
# - divert (country that hijackers diverted to) (not necessary for this project)
# - kidhijcountry (country of kidnapping/hijacking resolution) (not necessary for this project)
# - ransom (if ransom was demanded - yes/no) (not necessary for this project)
# - ransomamt (amount of ransom demanded) (not necessary for this project)
# - ransompaid (amount of ransom paid) (not necessary for this project)
# - hostkidoutcome_txt (outcome of hostage situation) (not necessary for this project)
# - nreleased (number of hostages released/escaped/rescued) (not necessary for this project)


# Cat IX - "Additional Information and Sources"

# keep:

# delete:
# - addnotes (few entries. additional relevant details) (not necessary anymore, qualitative research to identify attacks has been done previosly)
# - scite1 (source 1 about incident - only after 1997) (not necessary anymore, qualitative research to identify attacks has been done previosly)
# - scite2 (source 2 about incident - only after 1997) (not necessary anymore, qualitative research to identify attacks has been done previosly)
# - scite3 (source 3 about incident - only after 1997) (not necessary anymore, qualitative research to identify attacks has been done previosly)
# - INT_LOG (if a border was crossed to carry out the attack - can be taken from context of the attack/other data)
# - INT_IDEO (if an attack was ideologically international - can be taken from context of the attack/other data)
# - INT_MISC (if an attack was miscellaneous international - can be taken from context of the attack/other data)
# - INT_ANY (not included)
# - dbsource (data collection effort for GTD - cite GTD codebook, not necessary for application)



# Variables to keep
keep_vars <- c(
  "eventid", "success", "date", "gname", "individual", "phenomenon_area", "city", "latitude", "longitude",
  "dayunclear", "locationunclear", "doubtterr", "alternative_txt",
  "nkill", "nwound", "nkillter", "nwoundte",
  "attacktype1_txt", "attacktype2_txt",
  "targtype1_txt", "targtype2_txt", "targtype3_txt",
  "target1", "target2", "target3",
  "corp1", "corp2", "corp3",
  "weaptype1_txt", "weapsubtype1_txt", "weaptype2_txt", "weapsubtype2_txt", "weaptype3_txt", "weapsubtype3_txt", "weapdetail", "suicide"
)

# List all variable names in the dataset
all_vars <- names(gtd_germany)

# List variables that are not in the keep list
exclude_vars <- setdiff(all_vars, keep_vars)

# Remove all variables in exclude_vars from gtd_germany
gtd_germany <- gtd_germany %>%
  select(-all_of(exclude_vars))





#### OTHER MODIFICATIONS ####

# Rename weaptype "Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)" to "vehicle"
gtd_germany <- gtd_germany %>%
  mutate(weaptype1_txt = ifelse(weaptype1_txt == "Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)", "Vehicle", weaptype1_txt),
         weaptype2_txt = ifelse(weaptype2_txt == "Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)", "Vehicle", weaptype2_txt),
         weaptype3_txt = ifelse(weaptype3_txt == "Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)", "Vehicle", weaptype3_txt))

# Replace false values in corp1/corp2/corp3 with NA
replace_values <- c("unknown", "Unknown", "Unkonwn", "Uknown", "unk", "Unk", "*", "Not Applicable", "Not applicable")

gtd_germany <- gtd_germany %>%
  mutate(across(c(corp1, corp2, corp3), ~ ifelse(. %in% replace_values, NA, .)))

# Set nkill, nkillter, nwound, nwoundte to NA instead of 0
gtd_germany <- gtd_germany %>%
  mutate(across(c(nkill, nkillter, nwound, nwoundte), ~ ifelse(. == 0, NA, .)))

# Set doubtterr as numeric
gtd_germany$doubtterr <- as.numeric(gtd_germany$doubtterr)

# Change order of variables/columns
gtd_germany <- gtd_germany %>%
  select(
    eventid, success, date, gname, individual, phenomenon_area, city, latitude, longitude,
    dayunclear, locationunclear, doubtterr, alternative_txt,
    nkill, nwound, nkillter, nwoundte,
    attacktype1_txt, attacktype2_txt,
    targtype1_txt, targtype2_txt, targtype3_txt,
    target1, target2, target3,
    corp1, corp2, corp3,
    weaptype1_txt, weapsubtype1_txt, weaptype2_txt, weapsubtype2_txt, weaptype3_txt, weapsubtype3_txt, weapdetail, suicide
  )

# Arrange gtd_germany according to date
gtd_germany <- gtd_germany %>%
  arrange(date)





#### POPULATION DENSITY MAP ####

# Load population densitiy data (https://www.demografie-portal.de/DE/Fakten/bevoelkerungsdichte.html)
pop_density <- read.csv("bevoelkerungsdichte.csv", sep = ";", fileEncoding = "Latin1") # Latin1 encoding because of German "Umlaute"

# Remove the first 5 rows
pop_density <- pop_density %>% 
  slice(-c(1:5))

# Extract the new header names from the first row
new_header <- as.character(unlist(pop_density[1, ]))

# Assign the new header names and remove the first row
pop_density <- pop_density %>%
  slice(-1)
colnames(pop_density) <- new_header

# Reset row numbers
rownames(pop_density) <- NULL

# Rename columns
names(pop_density)[names(pop_density) == "Amtlicher Gemeindeschlüssel"] <- "AGS"
names(pop_density)[names(pop_density) == "Einwohner je Quadratkilometer"] <- "pop_dn"

# Remove column "Gemeindename"
pop_density <- pop_density %>%
  select(-Gemeindename)

# Convert AGS column to character to manipulate lengths
pop_density$AGS <- as.character(pop_density$AGS)

# Insert "0" at the beginning of every AGS value that is only 7 digits long (because it "disappeared" in csv)
pop_density$AGS <- ifelse(nchar(pop_density$AGS) == 7,
                          paste0("0", pop_density$AGS),
                          pop_density$AGS)



# Load shapefile with administrative borders (https://gdz.bkg.bund.de/index.php/default/digitale-geodaten/verwaltungsgebiete/verwaltungsgebiete-1-5-000-000-stand-01-01-vg5000-01-01.html)
shapefile_administrative_districts <- st_read("VG5000_GEM.shp")

# Check the Coordinate Reference System (CRS) of the shapefile
st_crs(shapefile_administrative_districts) # 3-degree Gauss-Kruger zone 3

# CRS is not EPSG:4326, needs to be converted for leaflet to be able to display it
if (st_crs(shapefile_administrative_districts)$epsg != 4326) {
  shapefile_administrative_districts <- st_transform(shapefile_administrative_districts, crs = 4326)
}

# Compare amount of values in both datasets
length(pop_density$AGS) # 10993
length(shapefile_administrative_districts$AGS) # 10957

# Get unique values from each dataset
ags_shapefile <- unique(shapefile_administrative_districts$AGS)
ags_pop_density <- unique(pop_density$AGS)

# Find values in shapefile_administrative_districts but not in pop_density
setdiff(ags_shapefile, ags_pop_density) # 3

# 14522275 (Jahnatal): 67,1 (https://www.statistik.sachsen.de/download/regional/statistik-sachsen_sbe_gemeinde_jahnatal-14522275.pdf)
# 16061119 (Uder): 88 (https://www.lg-uder.de/fileadmin/Dateiablage/PDF/Hauptamt/Broschuere_VG_Uder.pdf)
# 16076094 (Berga-Wünschendorf, Stadt): 81,1 (https://www.stadt-berga.de/zahlen-und-fakten/)

# Convert existing pop_dn column to numeric, handling commas as decimal separators
pop_density$pop_dn <- as.numeric(gsub(",", ".", pop_density$pop_dn))

# Create a dataframe with the new values, ensuring consistent data types
new_data <- data.frame(
  AGS = as.character(c(14522275, 16061119, 16076094)),  # AGS as character
  pop_dn = c(67, 88, 81)  # pop_dn as numeric
)

# Add the new rows to the existing pop_density dataframe
pop_density <- bind_rows(pop_density, new_data)

# Check length of pop_density$AGS again to ensure the missing values were added correctly
length(pop_density$AGS) # 10996 --> 3 more than previously

# Remove new_data
rm(new_data)

# Ensure the AGS columns are in the same format
pop_density$AGS <- as.character(pop_density$AGS)
shapefile_administrative_districts$AGS <- as.character(shapefile_administrative_districts$AGS)

# Merge the data
pop_density_germany <- shapefile_administrative_districts %>%
  left_join(pop_density %>% select(AGS, pop_dn), by = "AGS")

# Validate the merging was correct
nrow(pop_density_germany) # 10957 - same as in shapefile_administrative_districts, therefore merging was correct

# Force conversion of pop_dn back to numeric and check for warnings
pop_density_germany$pop_dn <- as.numeric(as.character(pop_density_germany$pop_dn)) # No warnings - all values numeric

# Check for NA values after conversion
sum(is.na(pop_density_germany$pop_dn)) # No NA values

# Convert the merged dataframe back to sf object
pop_density_germany <- st_as_sf(pop_density_germany)

# Remove unnecessary files from environment
rm(pop_density, shapefile_administrative_districts)





#### SAVE DATA ####

# Save gtd_germany
write.csv2(gtd_germany, "C:/Users/Lukas/iCloudDrive/HfP_TUM Bachelor+Master/Master/0 Master Thesis/R/gtd_dataprocessing/gtd_0522_germany.csv", sep = ";", row.names = FALSE)

write.csv2(gtd_germany, "C:/Users/Lukas/iCloudDrive/HfP_TUM Bachelor+Master/Master/0 Master Thesis/R/gtd_visualization/gtd_0522_germany.csv", sep = ";", row.names = FALSE)

# Save sf file with overwrite option
st_write(pop_density_germany, 
         "C:/Users/Lukas/iCloudDrive/HfP_TUM Bachelor+Master/Master/0 Master Thesis/R/gtd_dataprocessing/pop_density_germany.shp", 
         delete_layer = TRUE)

st_write(pop_density_germany, 
         "C:/Users/Lukas/iCloudDrive/HfP_TUM Bachelor+Master/Master/0 Master Thesis/R/gtd_visualization/pop_density_germany.shp", 
         delete_layer = TRUE)




