coronavirus = read.csv("data/coronavirus1.csv", stringsAsFactors = F) [,-1]

coronavirus = coronavirus %>% 
  dplyr::mutate(country = dplyr::if_else(countryName == "United States", "United States of America", countryName)) %>% 
  select(-countryName)


names(coronavirus) = c("date","countryCode","region","lat","lon",
                       "Confirmed","Recovered","Deaths","countryName")

#### New data
# 
# df1 <- read.csv(file = "https://raw.githubusercontent.com/ulklc/covid19-timeseries/master/countryReport/raw/rawReport.csv", stringsAsFactors = F)
# #
# write.csv(df1,"data/coronavirus1.csv")


