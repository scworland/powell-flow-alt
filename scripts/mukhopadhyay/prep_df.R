prep_df <- function(df,sitelist){
  
  name. <- names(df)
  check <- grepl("[^a-zA-Z]",name.) # keep nothing but numbers 
  sites.list <- name.[check]
  ncol <- length(sites.list) 
  
  
  df.info <- sitelist[match(sites.list,sitelist$site_no),]
  i_start <- match(sites.list[1], name.)
  i_end   <- match(sites.list[ncol],name.)
  
  df. <- df[,i_start:i_end]
  
  # look for NAs : lapply() applies the function to each column and returns a 
  # list whose i-th element is a vector containing the indices of the elements 
  # which have missing values in column i
  checkNA <- lapply (df., function (x) which (is.na (x))) 
  
  checkVal <- lapply (df., function (x) which (!is.na (x))) # index of non NA values 
  checkVlen <- unlist(lapply(checkVal, function(x) length(x))) # length of non NA values 
  checkfirstNA <- unlist(lapply(checkVal, function(x) min(x))) #fist non NA value
  checklastNA <- unlist(lapply(checkVal, function(x) max(x))) # last non NA value
  
  df.info <- mutate(df.info, first_rec=df$date[checkfirstNA],
                    first_rec_yr=df$year[checkfirstNA],
                    first_rec_month= df$month[checkfirstNA], 
                    last_rec=df$date[checklastNA],
                    last_rec_yr=df$year[checklastNA],
                    last_rec_month = df$month[checklastNA],
                    len_rec_days= as.Date(last_rec,format="%Y-%m-%d") - 
                      as.Date(first_rec,format="%Y-%m-%d"),
                    len_rec_yrs=len_rec_days/365,
                    len_nonNA = checkVlen)
  
  
  prepdf <- select(df.info,site_no,basin,
                   dec_long_va.x,dec_lat_va.x,contains("first"),contains("last"),contains("len"))
  
  
  return(prepdf)
}