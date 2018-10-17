
prepare_streamflow <- function(dv,site_info){
  
  basin <- site_info %>%
    select(site_no,basin)
  
  dv_clean <- dv %>%
    filter(Q != -999999) %>%
    filter(year >= 1948 & year <= 2017) %>%
    group_by(site_no,year) %>%
    add_count(year) %>%
    filter(n>=365) %>%
    ungroup() %>%
    select(-n) %>%
    left_join(basin,by="site_no") %>%
    distinct() %>%
    spread(site_no,Q) 
  
  return(dv_clean)
}

