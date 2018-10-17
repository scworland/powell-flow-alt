
add_covariates <- function(site_info){
  
  # prepare sites 
  sites <- site_info %>%
    select(site_no,comid)
  
  # add PRISM precip information
  precip <- sw_sb_extract("57bf5c07e4b0f2f0ceb75b1b",sites=sites)
  
  precip_gage_df <- precip %>%
    select(-matches("cat|tot")) %>%
    left_join(sites, by="comid") %>%
    distinct(site_no,.keep_all = T) %>%
    gather(variable,precip,-comid,-site_no) %>%
    mutate(year=parse_number(variable),
           decade=as.numeric(sub("\\d$", "", year))*10) %>%
    select(-variable) %>%
    arrange(site_no,year) 
  
  # add PRSIM temp data
  temp <- sw_sb_extract("5787ea72e4b0d27deb377b6d",sites=sites)
  
  temp_gage_df <- temp %>%
    select(-matches("cat|tot")) %>%
    left_join(sites, by="comid") %>%
    distinct(site_no,.keep_all = T) %>%
    gather(variable,temp,-comid,-site_no) %>%
    mutate(year=parse_number(variable),
           decade=as.numeric(sub("\\d$", "", year))*10) %>%
    select(-variable) %>%
    arrange(site_no,year)  
  
  climate_gage_df <- temp_gage_df %>%
    left_join(precip_gage_df, by=c("comid","site_no","year","decade"))
  
  # add NID dam information
  dams <- sw_sb_extract("58c301f2e4b0f37a93ed915a",sites)
  
  dam_gage_df <- dams %>%
    select(-matches("cat|tot|dens|.y|.x|2013")) %>%
    left_join(sites, by="comid") %>%
    distinct(site_no,.keep_all = T) %>%
    select(comid,site_no,everything()) %>%
    gather(variable,value,-comid,-site_no) %>%
    mutate(decade=parse_number(variable),
           variable=gsub("\\d", "", variable)) %>%
    spread(variable,value) 
  
  # add LULC data
  historic_lulc <- sw_sb_extract("58cbeef2e4b0849ce97dcd61",sites=sites)
  
  recent_lulc <- sw_sb_extract("5a5406bee4b01e7be2308855?groupId=JQUERY-FILE-UPLOAD-ac3590ad-84f6-47f6-86cb-0ad6779fbdbb",sites=sites)
  
  historic_class_link <- historic_class_link %>%
    mutate(variable=sub('.*_', '', variable))
  
  clean_historic_lulc <- historic_lulc %>%
    select(-matches("nodata")) %>%
    left_join(sites, by="comid") %>%
    distinct(site_no,.keep_all = T) %>%
    select(comid,site_no,everything()) %>%
    gather(variable,value,-comid,-site_no) %>%
    mutate(decade = sub('.*acc_',"",variable) %>%
             sub('_.*',"",.) %>%
             parse_number(.) %>%
             paste0("19",.),
           variable=sub('.*_', '', variable)) %>%
    left_join(historic_class_link, by="variable") %>%
    filter(lulc_class !="Intentionally left blank",
           lulc_class !="Mining") %>%
    select(-variable) %>%
    distinct(site_no,lulc_class,decade,.keep_all = T) %>%
    select(comid,site_no,decade,variable=lulc_class,value) %>%
    spread(variable,value)
  
  clean_recent_lulc <- recent_lulc %>%
    select(-matches("nodata")) %>%
    left_join(sites, by="comid") %>%
    distinct(site_no,.keep_all = T) %>%
    select(comid,site_no,everything()) %>%
    gather(variable,value,-comid,-site_no) %>%
    mutate(decade = sub('.*acc_',"",variable) %>%
             sub('_.*',"",.) %>%
             parse_number(.),
           decade = ifelse(nchar(decade)==2, "1990","2000"),
           variable=sub('.*_', '', variable)) %>%
    left_join(historic_class_link, by="variable") %>%
    filter(lulc_class !="Intentionally left blank",
           lulc_class !="Mining") %>%
    select(-variable) %>%
    distinct(site_no,lulc_class,decade,.keep_all = T) %>%
    select(comid,site_no,decade,variable=lulc_class,value) %>%
    group_by(comid,site_no,decade,variable) %>%
    summarize(value=mean(value)) %>%
    spread(variable,value)
  
  lulc_gage_df <- bind_rows(clean_historic_lulc,clean_recent_lulc) %>%
    gather(variable,value,-comid,-site_no,-decade) %>%
    group_by(comid,site_no,decade,variable) %>%
    summarize(value=mean(value)) %>%
    spread(variable,value) %>%
    ungroup() %>%
    mutate(decade=as.numeric(decade))
    
  X <- list(climate_gage_df,dam_gage_df,lulc_gage_df) %>%
    reduce(left_join, by = c("comid","site_no","decade")) %>%
    select(comid,site_no,year,decade,everything()) %>%
    filter(year >= 1955 & year <= 2015)
  
  
}