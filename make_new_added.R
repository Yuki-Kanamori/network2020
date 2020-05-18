require(tidyverse)

setwd("/Users/Yuki/Dropbox/TokyoBay_CPUE/nominalCPUE")
old = read.csv("new_chiba3.csv")
new = read.csv("added_data.csv", fileEncoding = "CP932")
summary(new)

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
t2 + geom_point(data = new, aes(x = lon, y = lat), shape = 16, size = 1)


new2 = new %>% mutate(lonlat = paste(lon, lat, sep = "_")) %>% distinct(lonlat)
lab = data.frame(lonlat = unique(new2$lonlat)) %>% mutate(lab = rep(1:length(unique(new2$lonlat))))
new2 = merge(new2, lab, by = "lonlat")
head(new2)
g = ggplot(new2, aes(x = lon, y = lat, label = lab))
p = geom_point()
g+p+geom_text_repel()

t2 + geom_point(data = new2, aes(x = lon, y = lat), shape = 16, size = 1) + geom_label_repel(aes(label = lab))
