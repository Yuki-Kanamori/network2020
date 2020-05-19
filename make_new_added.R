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
summary(new)

# remove the data
new2 = new %>% filter(lat > 35.4) %>% mutate(lonlat = paste(lon, lat, sep = "_"))
lab = data.frame(lonlat = unique(new2$lonlat)) %>%distinct(lonlat) %>%  mutate(lab = rep(1:length(unique(new2$lonlat))))
new2 = merge(new2, lab, by = "lonlat")
new2 = new2 %>% distinct(new2$lab, .keep_all = T)
head(new2)

g = ggplot(new2, aes(x = lon, y = lat, label = lab))
p = geom_point()
# g+p+geom_text_repel()
g+p+geom_text()
# 151, 150

new3 = new2 %>% filter(lab != 150 & lab != 151)
g = ggplot(new3, aes(x = lon, y = lat, label = lab))
p = geom_point()
# g+p+geom_text_repel()
g+p+geom_text()
#180, 157, 169, 155, 156

new4 = new3 %>% filter(lab != 180, lab != 157, lab != 169, lab != 155, lab != 156)
p <- ggplot() + coord_fixed() +
  xlab("Longitude") + ylab("Latitude")
world_map <- map_data("world")
jap <- subset(world_map, world_map$region == "Japan")
jap_cog <- jap[jap$lat > 35 & jap$lat < 38 & jap$long > 139 & jap$long < 141, ]
t2 <- p + geom_polygon(data = jap_cog, aes(x=long, y=lat, group=group), colour="gray 50", fill="gray 50")+ coord_map(xlim = c(139.5, 140.3), ylim = c(35, 35.75))
t2 + geom_point(data = new4, aes(x = lon, y = lat), shape = 16, size = 1)

# list for remove
remove = c("150"," 151", "180", "157", "169", "155", "156")
r_lab1 = lab %>% filter(lab %in% remove) #==だと引っかけられない，%in%でも151が引っかかってこない
r_lab2 = lab %>% filter(lab %in% 151)
r_lab = rbind(r_lab1, r_lab2)

r_data = new %>% mutate(lonlat = paste(lon, lat, sep = "_"))
mode(r_data$lonlat) #character
mode(r_lab$lonlat) #numeric
r_lab$lonlat = as.character(as.factor(r_lab$lonlat))
# r_data = left_join(r_data, r_lab, by = "lonlat", keep = F)
r_data = merge(r_data, r_lab) %>% select(-lonlat, -lab, -X)
write.csv(r_data, "ReviseList.csv")

#tentative data
sita = new %>% filter(lat <= 35.4) %>% select(-X) #4099

ue = new %>% filter(lat > 35.4) %>% mutate(lonlat = paste(lon, lat, sep = "_")) #5793
ue2 = merge(ue, r_lab, by = "lonlat", all = TRUE)
ue2 = subset(ue2, is.na(ue2$lab)) %>% select(-lonlat, -X, -lab)

head(sita)
head(ue2)
tent_data = rbind(ue2, sita)
write.csv(tent_data, "tent_data.csv")
