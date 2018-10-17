
library(forcats)

# plot period of record
yrs <- dv %>%
  group_by(site_no,year) %>%
  count(year) %>%
  ungroup() %>%
  spread(site_no,n) %>%
  gather(site_no,n,-year) %>%
  mutate(flow = ifelse(n>150,"yes","no flow")) %>%
  group_by(site_no) %>%
  mutate(start=min(year[!is.na(n)]),
         end=max(year[!is.na(n)]),
         length=end-start) %>%
  left_join(select(site_info,site_no,basin), by='site_no') %>%
  mutate(flow=ifelse(flow=='yes',basin,flow)) %>%
  arrange(start,length) %>%
  ungroup() %>%
  mutate(site_no = fct_reorder(site_no,start,.desc=T)) 

ggplot(yrs) +
  geom_tile(aes(x=year, y=site_no, fill = flow),color="white", size=0.01) +
  # scale_fill_viridis_d(option="C",na.value="white") +
  scale_fill_manual(values=c("orange","white","dodgerblue")) +
  labs(x="years",y="Individual gages", fill = "flow") +
  #geom_vline(xintercept=1950,linetype="dashed",size=0.8) +
  theme_bw() +
  scale_x_continuous(expand=c(0,0)) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = c(.2, .25))

# plot years dams were built
dams2 <- dams %>%
  filter(Dam_Height > 49 | Max_Storage > 2432)

ggplot(dams2) +
  geom_histogram(aes(x=Year_Completed,fill=basin), color='white',alpha=0.8) +
  facet_wrap(~Primary_Purpose)
