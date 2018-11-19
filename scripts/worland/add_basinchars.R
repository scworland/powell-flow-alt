library(dataRetrieval)
library(tidyverse)
library(readr)
library(cowplot)
library(forcats)
library(sf)
library(lubridate)
library(feather)

source("scripts/worland/sb_utils.R")

# load nid information with comids
sites <- read_csv("data/nid_characteristics.csv") %>%
  rename(site_no=NID_ID.y,comid=nrst_l_) 

# add basin characteristics
bc <- read_delim("data/basinchars/BASIN_CHAR_ACC_CONUS.TXT",delim=",",guess_max = 20000) %>%
  set_names(gsub(x = names(.), pattern = "ACC_", replacement = "")) %>%
  filter(COMID %in% sites$comid) %>%
  rename_all(tolower)

# month order
month_order <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")

# add precipitation
path <- "data/basinchars/precip"
ppt_03_15 <- sb_climate(item="5730f062e4b0dae0d5db1fbe", years=2003:2015, path, sites)
ppt_16_17 <- sb_climate(item="5b7322e2e4b0f5d5787c5ef8", years=2016:2017, path, sites)
ppt <- bind_rows(ppt_03_15, ppt_16_17) %>%
  filter(month!="") %>%
  rename(ppt=value)

# add temp
path <- "data/basinchars/temp"
temp_03_15 <- sb_climate(item="574f238fe4b0ee97d51a8916", years=2003:2015, path, sites)
temp_16_17 <- sb_climate(item="5b73267ce4b0f5d5787c5f6e", years=2016:2017, path, sites)
temp <- bind_rows(temp_03_15, temp_16_17) %>%
  filter(month!="") %>%
  rename(temp=value)

# combine climate
climate <- temp %>%
  left_join(ppt, by=c("comid","year","month")) %>%
  select(-month,-year) %>%
  group_by(comid) %>%
  summarize_all(funs(median))

# add lulc
lulc_link <- read_csv("data/basinchars/lulc/lulc_name_links.csv")
path <- "data/basinchars/lulc"
lulc <- sb_lulc(item="5761bad4e4b04f417c2d30c5", path, sites, lulc_link) 

# combine everything
covariates <- list(bc,climate,lulc) %>%
  reduce(left_join, by="comid") %>%
  select(comid,ppt,temp,everything())

# plots
p1 <- ggplot(mutate(ppt,month=fct_relevel(as.factor(month),month_order))) + 
  geom_line(aes(year,ppt,group=comid),alpha=0.3) + 
  scale_x_continuous(breaks=2003:2017,labels=substr(as.character(2003:2017),3,4)) +
  labs(y="mm") +
  facet_wrap(~month,nrow=4) +
  theme_bw()

p2 <- ggplot(mutate(temp,month=fct_relevel(as.factor(month),month_order))) + 
  geom_line(aes(year,temp,group=comid),alpha=0.3) + 
  scale_x_continuous(breaks=2003:2017,labels=substr(as.character(2003:2017),3,4)) +
  labs(y="°C") +
  facet_wrap(~month,nrow=4) +
  theme_bw()

ggdraw() +
  draw_plot(p1, x = 0, y = 0.5, width = 1, height = 0.45) +
  draw_plot(p2, x = 0, y = 0.0, width = 1, height = 0.45) +
  draw_plot_label(label = c("Precipitation", "Temperature"), size = 12,
                  x = c(0, 0), y = c(1, 0.5))

