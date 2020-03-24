coronavirus = read.csv("data/coronavirus1.csv", stringsAsFactors = F) [,-1]

names(coronavirus) = c("date","countryCode","countryName","region","lat","lon",
"Confirmed","Recovered","Deaths")

#### Old data
# 
# df1 <- read.csv(file = "https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/coronavirus_dataset.csv", stringsAsFactors = F)
# #
# write.csv(df1,"data/coronavirus.csv")



#### New data

# df1 <- read.csv(file = "https://raw.githubusercontent.com/ulklc/covid19-timeseries/master/report/raw/rawReport.csv", stringsAsFactors = F)
# #
# write.csv(df1,"data/coronavirus1.csv")
