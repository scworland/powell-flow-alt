
prepare_streamflow <- function(dv,site_info){
  
  # dv_clean <- dv %>%
  #   mutate(Q=ifelse(Q==-999999,NA,Q)) %>%
  #   group_by(site_no,year) %>%
  #   summarize(Q_mean=mean(Q),
  #             Q_min=min(Q),
  #             Q_max=max(Q)) %>%
  #   left_join(site_comid, by="site_no") %>%
  #   ungroup() %>%
  #   group_by(site_no) %>%
  #   mutate(start_year = min(year),
  #          end_year = max(year)) %>%
  #   ungroup() %>%
  #   select(comid,site_no,year,everything())
  
  dv_clean <- dv %>%
    filter(Q != -999999) %>%
    ungroup() %>%
    #filter(year >= 1955 & year <= 2015) %>%
    group_by(site_no) %>%
    #mutate(dt = round(c(NA,diff(date)), 1)) %>%
    #filter(dt>1)
    spread(site_no,Q) 
  
  return(dv_clean)
}