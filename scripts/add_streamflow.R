
add_streamflow <- function(site_list) {
  dv_streamflow <- NULL
  for(i in 1:length(site_list$site_no)){
    print(paste0("Downloading data for site ", i, " out of ",length(site_list$site_no)))
    site <- site_list$site_no[i]
    nwisdata <- readNWISdv(siteNumbers=site,
                             parameterCd="00060",
                             statCd="00003") 
    
    if(length(nwisdata)>0){
    streamflow <- nwisdata %>%
      select(2:4) %>%
      set_names(c("site_no","date","Q")) %>%
      mutate(year=year(date),
             month=month(date))
    }
    
    dv_streamflow=rbind(dv_streamflow,streamflow)
  }
  
  return(dv_streamflow)
}

