library(ggmap)
library(ggplot2)
library(raster)
library(viridis)
library(data.table)
Sys.setlocale("LC_ALL", "korean")

# Enter GCP api key:
gg.api <- "your gcp api key here"
register_google(gg.api)

# korea map
names <- c("서울", "안양", "안산", "시흥", "일산",
           "과천", "성남", "부산", "대구", "안동", 
           "포항", "원주", "강릉", "춘천", "장흥", 
           "해남", "전주", "온양", "제주", "인천")
freq <- c(10, 10, 10, 1, 1, 
          5, 10, 1, 1, 10, 
          10, 10, 1, 1, 1, 
          1, 1, 2, 10, 5)
gc <- geocode(enc2utf8(names))
loc <- data.frame(names, gc, freq)
loc

visits <- c(1, 1, 2, 1, 0, 6, 0, 10, 5, 0, 5, 9, 3, 1, 0, 10, 0)
korea.visits <- data.frame(Region=korea@data$NAME_1, freq=visits)
korea.visits

korea <- shapefile("./gadm36_KOR_shp/gadm36_KOR_1.shp")
ggplot() +
  geom_polygon(data=korea, 
               aes(x=long, y=lat, group=group), 
               fill="white", color="black")

korea.df <- fortify(korea)
head(korea.df)

korea@data$id <- rownames(korea@data)
korea@data$id

korea.df <- merge(korea.df, korea@data[, c("id", "NAME_1")], by="id", all.x=T)
korea.df

new.korea <- merge(korea.df, korea.visits, by.x="NAME_1", by.y="Region")
new.korea

theme_set(theme_grey(base_family="AppleGothic"))
p <- ggplot() + 
  geom_polygon(data=new.korea, aes(x=long, y=lat, group=group, fill=freq))
p + scale_fill_gradient(low="yellow", high="red") +
  geom_point(data=loc, aes(x=lon, y=lat)) +
  geom_text(data=loc, aes(label=names, x=lon, y=lat), vjust=-1)
