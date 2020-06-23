require(ggvast)
require(rgdal)
setwd("/Users/Yuki/Dropbox/Network2020/suzuki_lognorm0")

# 0.1 set the directory ---------------------------------------------
vast_output_dirname = "/Users/Yuki/Dropbox/Network2020/suzuki_lognorm0"
fig_output_dirname = vast_output_dirname
setwd(dir = vast_output_dirname)

# 0.2 load the data -------------------------------------------------
load("Save.RData")
DG = read.csv("Data_Geostat.csv")

# 1. plot index ------------------------------------------------
# vast output
setwd(dir = vast_output_dirname)
vast_index = read.csv("Table_for_SS3.csv") %>% mutate(type = "Standardized", Category = 1)

# nominal
levels(DG$spp) #単一種の時はNULLと出る
category_name = c("kouika") #カテゴリーの名前（魚種名や銘柄など）

# make a figure
# nominalにはerror barが無いため，geom_errorbarのwarningが出るが問題ない
ggvast::plot_index(vast_index = vast_index,
                   DG = DG,
                   category_name = category_name,
                   fig_output_dirname = fig_output_dirname)

# 5. map COG ----------------------------------------------------
data_type = c("VAST", "nominal")[1]

#unique(map_data("world")$region)
region = "Japan" #作図する地域を選ぶ
ncol = 2 #横にいくつ図を並べるか（最大数 = カテゴリー数）
shape = 16 #16はclosed dot
size = 1.9 #shapeの大きさ
zoom_in_lon = 10 #mapの拡大・縮小（1がデフォルト，数字が大きくなるほど拡大する．1以下で縮小する）
zoom_in_lat = 10 #mapの拡大・縮小（1がデフォルト，数字が大きくなるほど拡大する．1以下で縮小する）
use_biascorr = TRUE

# make figures
ggvast::map_cog(data_type = data_type,
                category_name = category_name,
                region = region,
                ncol = ncol,
                shape = shape,
                size = size,
                zoom_out_lon,
                zoom_out_lat,
                fig_output_dirname = fig_output_dirname)


# make COG Table
### this code is from plot_range_index() in FishStatsUtils ###
Sdreport = Save[["Opt"]][["SD"]]
if("ln_Index_cyl" %in% rownames(TMB::summary.sdreport(Sdreport))){
  # VAST Version >= 2.0.0
  CogName = "mean_Z_cym"
  EffectiveName = "effective_area_cyl"
  Save[["TmbData"]][["n_t"]] = nrow(Save[["TmbData"]][["t_yz"]])
}else{
  message("not available because this function does not match your VAST version (VAST Version >= 2.0.0 is needed)")
}

Year_Set = 1:Save$TmbData$n_t
Years2Include = 1:Save$TmbData$n_t
strata_names = 1:Save$TmbData$n_l
category_names = 1:Save$TmbData$n_c
Return = list( "Year_Set"=Year_Set )

SD = TMB::summary.sdreport(Sdreport)
SD_mean_Z_ctm = array( NA, dim=c(unlist(Save$TmbData[c('n_c','n_t','n_m')]),2), dimnames=list(NULL,NULL,NULL,c('Estimate','Std. Error')) )
#use_biascorr = TRUE
if( use_biascorr==TRUE && "unbiased"%in%names(Sdreport) ){
  SD_mean_Z_ctm[] = SD[which(rownames(SD)==CogName),c('Est. (bias.correct)','Std. Error')]
}
if( !any(is.na(SD_mean_Z_ctm)) ){
  message("Using bias-corrected estimates for center of gravity...")
}else{
  message("Not using bias-corrected estimates for center of gravity...")
  SD_mean_Z_ctm[] = SD[which(rownames(SD)==CogName),c('Estimate','Std. Error')]
}

COG_Table = NULL
for( cI in 1:Save$TmbData$n_c ){
  for( mI in 1:dim(SD_mean_Z_ctm)[[3]]){
    Tmp = cbind("m"=mI, "Year"=Year_Set, "COG_hat"=SD_mean_Z_ctm[cI,,mI,'Estimate'], "SE"=SD_mean_Z_ctm[cI,,mI,'Std. Error'])
    if( Save$TmbData$n_c>1 ) Tmp = cbind( "Category"=category_names[cI], Tmp)
    COG_Table = rbind(COG_Table, Tmp)
  }}
### end the code form plot_range_index() in FishStatsUtils ###

#UTM to longitude and latitude
#year_set = DG %>% select(Year) %>% distinct(Year, .keep_all = T)
#cog = read.csv("COG_Table.csv")
cog = COG_Table
nyear_set = seq(min(DG$Year), max(DG$Year))
tag = data.frame(Year = rep(1:length(unique(DG$Year))), Year2 = rep(min(DG$Year):max(DG$Year), each = length(category_name)), Category = rep(category_name))

if(length(unique(category_name)) == 1){
  cog = cog %>% data.frame() %>% mutate(Category = category_name)
  cog = merge(cog, tag, by = c("Category", "Year")) %>% arrange(Year)
}else{
  cog = cog %>% data.frame()
  tag2 = data.frame(ncate = unique(cog$Category), Category = category_name)
  cog = cog %>% rename(ncate = Category)
  cog = merge(cog, tag2, by = "ncate")
  cog = merge(cog, tag, by = c("Category", "Year")) %>% arrange(Year)
}



lat = cog[cog$m == 1, ]
lon = cog[cog$m == 2, ]
x = lat$COG_hat*1000
y = lon$COG_hat*1000
xy = cbind(x,y)
zone = unique(DG$zone)
lonlat = data.frame(project(xy, paste0("+proj=utm +zone=", zone, " ellps=WGS84"), inv = TRUE))
colnames(lonlat) = c("lon", "lat")

lonlat = cbind(lonlat, lat[, c("Year", "Category")])
lonlat = merge(lonlat, tag, by = c("Category", "Year"))


#make COG maps
setwd(dir = fig_output_dirname)
map = ggplot() + coord_fixed() + xlab("Longitude") + ylab("Latitude")
world_map = map_data("world")
region2 = subset(world_map, world_map$region == region)
local_map = map + geom_polygon(data = region2, aes(x = long, y = lat, group = group), colour = "black", fill = "white") + coord_map(xlim = c(min(lonlat$lon)-0.1, max(lonlat$lon)+0.1), ylim = c(min(lonlat$lat)-0.1, max(lonlat$lat)+0.1))
# local_map = map + geom_polygon(data = region2, aes(x = long, y = lat, group = group), colour = "black", fill = "white") + coord_map(xlim = c(min(lonlat$lon)-1*zoom_in_lon, max(lonlat$lon)+1*zoom_in_lon), ylim = c(min(lonlat$lat)-1*zoom_in_lat, max(lonlat$lat)+1*zoom_in_lat))

th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.5)),
           axis.text.y = element_text(size = rel(1.5)),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_text(size = 13))
p = geom_point(data = lonlat, aes(x = lon, y = lat, colour = Year2), shape = shape, size = size)
f = facet_wrap( ~ Category, ncol = ncol)
c = scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
labs = labs(x = "Longitude", y = "Latitude", colour = "Year")

if(length(unique(category_name)) == 1){
  fig = local_map+theme_bw()+th+p+c+labs
}else{
  fig = local_map+theme_bw()+th+p+f+c+labs
}
ggsave(filename = "map_cog.pdf", plot = fig, units = "in", width = 11.69, height = 8.27)



# time series of lonlat -----------------------------------------
summary(lonlat)
lonlat2 = lonlat %>% dplyr::rename(Latitude = lat, Longitude = lon) %>% gather(key = lonlat, value = n_lonlat, 3:4)
g = ggplot(data = lonlat2, aes(x = Year2, y = n_lonlat))
p = geom_point()
l = geom_line()
f = facet_wrap(~ lonlat, ncol = 1, scales = "free")
lab = labs(x = "Year", y = "Geographic position", title = "")
g+p+l+f+lab+theme_bw()
