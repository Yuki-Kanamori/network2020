require(tidyverse)

setwd("/Users/Yuki/Dropbox/TokyoBay_CPUE/nominalCPUE")
y2019 = read.csv("after2019_isimoti.csv", fileEncoding = "CP932")
colnames(y2019)
y2019 = y2019 %>% mutate(lon = 経度 %/% 100 + 経度 %% 100/60, lat = 緯度 %/% 100 + 緯度 %% 100/60, test = lubridate::dmy(年月日)) %>% mutate(year = as.numeric(str_sub(test, 1, 4)), month = as.numeric(str_sub(test, 6, 7)), day = as.numeric(str_sub(test, 9, 10))) %>% dplyr::rename(wt = 水温) %>% select(year, month, day, lon, lat, 回数, CPUE, 全銘柄, 込, species, gear, wt)
write.csv(y2019, "latest_data.csv", fileEncoding = "CP932")

# check the lon&lat ---------------------------------------------
require(maps)
require(mapdata)
library(ggrepel)

p <- ggplot() + coord_fixed() +
  xlab("Longitude") + ylab("Latitude")
world_map <- map_data("world")
jap <- subset(world_map, world_map$region == "Japan")
jap_cog <- jap[jap$lat > 35 & jap$lat < 38 & jap$long > 139 & jap$long < 141, ]
t2 <- p + geom_polygon(data = jap_cog, aes(x=long, y=lat, group=group), colour="gray 50", fill="gray 50")+ coord_map(xlim = c(139.5, 140.3), ylim = c(35, 35.75))
t2 + geom_point(data = y2019, aes(x = lon, y = lat), shape = 16, size = 1)
summary(new)