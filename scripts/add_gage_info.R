

add_gage_info <- function(site_list, gage_locs, updated_locs){
  
  # join to site list
  sites_comids <- site_list %>%
    data.frame() %>%
    left_join(gage_locs,by="site_no") %>%
    left_join(updated_locs,by=c("site_no"="Gage_no")) %>%
    mutate(COMID = ifelse(COMID=="-9999",NA,COMID),
           comid = ifelse(!is.na(COMID),COMID,comid)) %>%
    select(site_no,comid,basin) %>%
    mutate(comid=as.character(comid)) 
  
  # use NWIS to get more site information
  gage_list_comid <- readNWISsite(siteNumber=site_list$site_no) %>%
    select(site_no,name=station_nm,lon=dec_long_va,lat=dec_lat_va, 
           drainage_area=drain_area_va, elev=alt_va) %>%
    left_join(sites_comids, by="site_no") %>%
    arrange(elev) %>%
    select(site_no,name,comid,lon,lat,basin,drainage_area,elev)
  
  return(gage_list_comid)
  
}
  