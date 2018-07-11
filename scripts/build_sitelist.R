
build_sitelist <- function(HUC_upper_CO,HUC_lower_CO,HUC_CO) {
  
  sites_AZ <- whatNWISsites(stateCd  = 'AZ',siteType = 'ST',parameterCd = "00060", hasDataTypeCd = 'dv')
  sites_CO <- whatNWISsites(stateCd  = 'CO',siteType = 'ST',parameterCd = "00060", hasDataTypeCd = 'dv')
  sites_UT <- whatNWISsites(stateCd  = 'UT',siteType = 'ST',parameterCd = "00060", hasDataTypeCd = 'dv')
  sites_NV <- whatNWISsites(stateCd  = 'NV',siteType = 'ST',parameterCd = "00060", hasDataTypeCd = 'dv')
  sites_NM <- whatNWISsites(stateCd  = 'NM',siteType = 'ST',parameterCd = "00060", hasDataTypeCd = 'dv')
  sites_WY <- whatNWISsites(stateCd  = 'WY',siteType = 'ST',parameterCd = "00060", hasDataTypeCd = 'dv')
  
  sites_all <- bind_rows(sites_AZ,sites_CO,sites_UT,sites_NV,sites_NM,sites_WY)
  
  # make a spatial object 
  sites <- sites_all
  coordinates(sites) <- c("dec_long_va","dec_lat_va")
  proj4string(sites) <- proj4string(HUC_CO)
  in.upper.CO <- !is.na(over(sites, as(HUC_upper_CO, "SpatialPolygons")))
  in.lower.CO <- !is.na(over(sites, as(HUC_lower_CO, "SpatialPolygons")))
  
  sites_all$basin <- NA
  sites_all$basin[in.lower.CO] <- 'lower'
  sites_all$basin[in.upper.CO] <- 'upper'
  sites_basin <- na.omit(sites_all)
  
  atr_base <- readNWISrating(siteNumber=sites_basin$site_no)
  
  attr_df <- attr(atr_base,"siteInfo")
  
  site_list <- full_join(sites_basin,attr_df,by="site_no")
  
  return(site_list)
}

