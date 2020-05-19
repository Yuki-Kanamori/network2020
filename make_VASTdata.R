require(tidyverse)

setwd("/Users/Yuki/Dropbox/TokyoBay_CPUE/nominalCPUE")
old = read.csv("new_chiba3.csv")
new = read.csv("tent_data.csv", fileEncoding = "CP932")

head(old)
head(new)

old2 = old %>% select(Y,M,Lon,Lat,FISH,CATCH,NUM,CPUE,GEAR)
mode(old2$M)
new2 = new %>% select(year,month,lon,lat,回数,CPUE,全銘柄,species,gear) %>% rename(Y = year, Lon = lon, Lat = lat, NUM = 回数, CATCH = 全銘柄, FISH = species, GEAR= gear)
month =  data.frame(month = unique(new2$month))
month
month$M = c(9,10,7,8,6,12,11,4,5,2,3,1)
new2 = left_join(new2, month, by = "month") %>% dplyr::select(-month)

data = rbind(old2, new2)

sp_list = data.frame(FISH = unique(data$FISH))
sp_list
sp_list$FISH2 = ifelse(sp_list$FISH == "akakamasu", "kamasu spp.", as.character(sp_list$FISH))
sp_list$Trend = c("dec", "dec", "inc", "inc", "dec", "inc", "dec", "fumei", "inc", "inc", "dec", "inc")

data = left_join(data, sp_list, by = "FISH")

setwd("/Users/Yuki/Dropbox/Network2020")
write.csv(data, "VASTdata.csv")
