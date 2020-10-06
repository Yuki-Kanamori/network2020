require(tidyverse)
require(openxlsx)

setwd("/Users/Yuki/Dropbox/TokyoBay_CPUE/nominalCPUE/追加データ2020年3月まで")
df = read.csv("added_data2019.csv", fileEncoding = "CP932")
# df = openxlsx::read.xlsx("added_data2019.xlsx")
head(df)
df = df %>% dplyr::rename(LAT = 緯度, LON = 経度, effort = 回数, catch = 全銘柄) %>% mutate(year = as.numeric(str_sub(年月日, -2, -1)), month = str_sub(年月日, 4, 6), day = as.numeric(str_sub(年月日, 1, 2)))
summary(df)
# 2967-366 = 2601
df1 = df[!is.na(df$day), ] #dayにNAが入っていないもの
df2 = df[is.na(df$day), ] #dayにNAが入っているもの（日付が0始まりでなくて，うまく列が作れなかったもの）
df2 = df2 %>% mutate(day = as.numeric(str_sub(年月日, 1, 1)), month = str_sub(年月日, 3, 5))
head(df2)
df = rbind(df1, df2)  %>% mutate(lat = LAT %/% 100 + LAT %% 100/60, lon = LON %/% 100 + LON %% 100/60) %>% select(-年月日, -水温, -込, -LAT, -LON)
write.csv(df, "added_data2019_2.csv", fileEncoding = "CP932")