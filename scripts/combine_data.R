

combine_data <- function(dv_clean,X){
  all_data <- X %>%
    left_join(dv_clean, by=c("site_no","comid","year")) %>%
    select(site_no,comid,lon,lat,drainage_area,elev,year,
           decade,Q_mean,Q_min,Q_max,precip,temp,acc_major:woody_wetland)
}