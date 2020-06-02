data_dir <- "./data"
legend_name <- "ts_guide.csv"
ts_name <- "ts_rec.csv"



raw_rec_data <- combine_spreadsheets(time_series_name = ts_name, legend_name = legend_name)

require(ggplot2)
raw_rec_data$org <- sapply(strsplit(as.character(raw_rec_data$assessid),"-"), first)
orgs <- unique(raw_rec_data$org)

for(i in 1:length(orgs)){

p <- ggplot(filter(raw_rec_data, org==orgs[i]), aes(x=tsyear, y=scale(tsvalue), colour=stockid)) +
  geom_point() +
  ggtitle(orgs[i])

ggsave(paste(orgs[i],".png",sep=""))
}

write.csv(raw_rec_data, "./inst/extdata/spliced_data.csv")
