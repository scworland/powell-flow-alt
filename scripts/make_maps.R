make_maps <- function(n,df,HUC_upper_CO,HUC_lower_CO,CO_river_trans){
  
  sites <- filter(df,len_rec_yrs >= n) 
  ns <- dim(sites)[1]
  titletext <- paste0('Record length > ',n,' years ')
  subtext <- paste0('No. of gages = ',ns)
  figname <- paste0('CO_gages_',n,'.jpeg')
  
  
  coordinates(sites) <- c("dec_long_va.x","dec_lat_va.x")
  proj4string(sites) <- proj4string(HUC_CO)
  in.upper.CO <- !is.na(over(sites, as(HUC_upper_CO, "SpatialPolygons")))
  in.lower.CO <- !is.na(over(sites, as(HUC_lower_CO, "SpatialPolygons")))
  
  jpeg(figname,height = 6,width=5, units = 'in',res = 300)
  
  plot(HUC_upper_CO,axes=TRUE,border="black",lwd = 1.5,xlim = c(-116,-104),ylim  = c(30,44))
  plot(HUC_lower_CO,axes=TRUE,border="brown",lwd = 1.5,add = TRUE)
  lines(CO_river_trans, col="blue", lwd=1.2)
  
  points(sites[in.upper.CO, ], pch=20,col="red",cex=0.5)
  points(sites[in.lower.CO, ], pch=20, col="blue",cex=0.5)
  
  legend("bottomright", cex=0.85,inset = c(0.05,0.05),
         c("upper gages","lower gages",'CO River'),
         pch=c(20,20,NA), lty=c(NA,NA,1),lwd = c(NA,NA,1.2),
         col=c("red", "blue", "blue"), bty="n")
  
  legend("topright",subtext,bty="n")
  title(main = titletext)
  
  dev.off()
}
