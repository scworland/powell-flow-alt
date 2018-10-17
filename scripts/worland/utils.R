
# download and unzip files from sciencebase
#' @example sw_sb_extract("5835cad4e4b0d9329c801b22")
sw_sb_extract <- function(item,type="ACC",path="data/basinchars/nhd_sb",sites){
  
  library(sbtools)
  
  fnames <- item_list_files(item)$fname
  
  if(any(grepl(type,fnames))) {
    files <- item_list_files(item) %>%
      filter(grepl(type,fname))
  } else {
    files <- item_list_files(item) 
  }
  
  files <- filter(files,!grepl("xml",fname))
  
  gages <- select(sites,comid)
  for (i in 1:nrow(files)){
    
    pth <- file.path(path,files$fname[i])
    
    if(!file.exists(pth)){
      dat <- item_file_download(item, 
                                names=files$fname[i], 
                                destinations=pth,
                                overwrite_file = T)
    }else{
      dat <- pth
    }
    
      
      d <- read_delim(unzip(dat,exdir=path,overwrite=T),",",guess_max = 20000) %>%
        rename_all(tolower) %>%
        mutate(comid = as.character(comid)) %>%
        mutate_at(vars(-comid), funs(as.numeric))
    
    gages <- left_join(gages,d,by="comid")
    
  }
  
  return(gages)
}

