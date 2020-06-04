data_dir <- "./data"
legend_name <- "ts_guide.csv"
ts_name <- "ts_rec.csv"



raw_rec_data <- combine_spreadsheets(time_series_name = ts_name, legend_name = legend_name)

require(ggplot2)
raw_rec_data$org <- sapply(strsplit(as.character(raw_rec_data$assessid),"-"), first)
orgs <- unique(raw_rec_data$org)

num_ts <- raw_rec_data %>%
  filter(!is.na(tsvalue)) %>%
  group_by(assessid) %>%
  summarise(num=n()) %>%
  filter(num>30)
  

for(i in 1:length(orgs)){

p <- ggplot(filter(raw_rec_data, org==orgs[i], assessid %in% num_ts$assessid), aes(x=tsyear, y=scale(tsvalue), colour=stockid)) +
  geom_point() +
  ggtitle(orgs[i])

ggsave(paste("figs/",orgs[i],".png",sep=""))
}

write.csv(raw_rec_data, "./inst/extdata/spliced_data.csv")


afsc_stocks<- filter(raw_rec_data, org=="AFSC") %>% select(assessid) %>% unique()

for(i in 1:length(afsc_stocks)){
  
  p <- ggplot(filter(raw_rec_data, assessid== afsc_stocks[i]), aes(x=tsyear, y=scale(tsvalue), colour=stockid)) +
    geom_point() +
    ggtitle(afsc_stocks[i])
  
  ggsave(paste("figs",afsc_stocks[i],".png",sep=""))
}

write.csv(raw_rec_data, "./inst/extdata/spliced_data.csv")