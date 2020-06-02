#'@param time_series_name string denoting the .csv that the
#'@param legend_name string denoting the .csv that the legend for the time series is stored in
combine_spreadsheets <- function(data_dir="./data", time_series_name, legend_name){
  #Read in the legend .csv
  lege <- read.csv(file.path(data_dir,legend_name))
  dat <- read.csv(file.path(data_dir, time_series_name))

  lege_withR <- lege %>%
    select(stockid, R) %>%
    filter(R!="")

  data__ <- dat %>%
    filter(stockid %in% lege_withR$stockid & tsid %in% lege_withR$R)

  return(data__)
}
