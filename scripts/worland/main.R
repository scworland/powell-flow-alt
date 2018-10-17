
library(dataRetrieval)
library(tidyverse)
library(readr)
library(sf)
library(lubridate)
library(feather)
library(rgdal)
library(rgeos)

source("scripts/utils.R")

# read in shapefiles
HUC_upper_CO <- readOGR('data/shapefiles/Sud_GIS_files','HUC_upper_CO')
HUC_lower_CO <- readOGR('data/shapefiles/Sud_GIS_files','HUC_lower_CO') 
HUC_CO <- readOGR('data/shapefiles/Sud_GIS_files','HUC_2_CO')

# build site list
source("scripts/build_sitelist.R")
site_list <- build_sitelist(HUC_upper_CO,HUC_lower_CO,HUC_CO)

# load main gage/comid relational shapefile
gage_locs <- st_read("data/shapefiles/GageLoc", stringsAsFactors = F)  %>% 
  select(site_no=SOURCE_FEA,comid=FLComID) %>%
  st_set_geometry(NULL)

# load updated gage/comid relational shapefile (may not apply)
updated_locs <- st_read("data/shapefiles/CATCHMENT_gageloc_v1", stringsAsFactors = F) %>%
  st_set_geometry(NULL)

# add streamflow data
source("scripts/add_streamflow.R")
dv <- add_streamflow(site_list)

# add comid, drainage area, and coordinates
source("scripts/add_gage_info.R")
site_info <- add_gage_info(site_list, gage_locs, updated_locs)
  
# prepare streamflow for modeling 
source("scripts/prepare_streamflow.R")
dv_clean <- prepare_streamflow(dv,site_info)

#write_feather(site_info,"data/CO_site_info.feather")
#write_feather(dv_clean,"data/CO_daily_streamflow.feather")

# dam information
dams <- read_csv('data/NID_ul_CO.csv')

# add covariates
source("scripts/add_covariates.R")
historic_class_link <- read_csv("data/basinchars/historic_lulc_classes.csv")
X <- add_covariates(site_info) # take > 30 minutes
write_feather(X,"data/MO_annual_covariates.feather")

# combine covariates and streamflow
source("scripts/combine_data.R")
data <- combine_data(dv_clean,X)


ggplot(dv_clean) +
  geom_line(aes(date,Q)) +
  facet_wrap(~site_no, scales="free_y", ncol=1)
