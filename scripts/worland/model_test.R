
library(feather)
library(tidyverse)
library(rethinking)

select <- dplyr::select

# start with actual streamflow and "build" downstream sites
d <- read_feather("data/MO_daily_streamflow.feather") %>%
  select(date,Q1="06052500") %>%
  filter(date > "2005-01-01") %>%
  mutate(Q1=log(Q1),
         Q2=(0 + (1.1*Q1)) + rnorm(nrow(.),0,0.1),
         Q3=(0 + (1.5*Q2)) + rnorm(nrow(.),0,0.3),
         Q4=rnorm(nrow(.),10,3)) 

# plot streamflow
ggplot(gather(d,site,Q,-date)) + 
  geom_line(aes(date,Q)) + 
  facet_wrap(~site,scales="free_y",ncol=1) +
  labs(y="log(Q)")

# build flow-covariates
X <- d %>%
  mutate(x1 = 0,
         x2 = Q1,
         x3 = Q2,
         x4 = Q3) %>%
  select(x1,x2,x3,x4) %>%
  gather() 

# create model data frame
model_data <- d %>%
  gather(site,Q,-date) %>%
  mutate(x = X$value) %>%
  data.frame()

m1.0 <- map2stan(
  alist(
    Q ~ dnorm(mu, sigma),
    mu <- a[site] + b[site]*x,
    a[site] ~ dnorm(0,3),
    b[site] ~ dnorm(0,3),
    sigma ~ dcauchy(0,1)
  ),
  data=model_data, iter=2)

m1 <- resample(m1.0, warmup=1000, iter=3000, chains=2, cores=2)

post <- data.frame(extract.samples(m1,n=1000)) %>%
  select(b.2,b.3,b.4) %>%
  gather(parameter,value) %>% 
  mutate(site=paste0("Q",as.numeric(sub(".*\\.", "",parameter)))) %>%
  group_by(site) %>%
  summarize(mu=mean(value),
            low = quantile(value,0.1),
            high = quantile(value,0.9),
            lowest = quantile(value,0.01),
            highest = quantile(value,0.99)) %>%
  ungroup() %>%
  mutate(obs = c(1.1,1.5,0))

ggplot(post) +
  geom_linerange(aes(x=site,ymin=low,ymax=high),size=1,alpha=0.9) +
  geom_linerange(aes(x=site,ymin=lowest,ymax=highest),size=0.5,alpha=0.7) +
  geom_point(aes(site,mu),color="black", shape=23, size = 2) +
  geom_point(aes(site,mu),color="black", fill="white", shape=23, size = 2) +
  #geom_point(aes(site,obs),color="red") +
  coord_flip() + 
  theme_bw() +
  labs(y="value",x=expression(beta)) +
  geom_hline(yintercept=c(0,1.1,1.5), linetype="dashed") 
  
  
            

  
  
  
  
