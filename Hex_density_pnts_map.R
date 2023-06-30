library(ggplot2)
library(maptools)

ghgh<- read.csv("D:/Erod_for_globe/erod_SE_ksat_28_02.csv")

colnames(ghgh)

land <- readShapeLines('C:/Users/surya/Downloads/global_soil_erosion/land.shp')
land <- fortify(land, region='piece')
colnames(land)[1] <- c('longitude_decimal_degrees')
colnames(land)[2] <- c('latitude_decimal_degrees')
head(land)

p  <- ggplot(data=ghgh, aes(x=longitude_decimal_degrees,y=latitude_decimal_degrees))
p + geom_point()

p + geom_point(alpha=0.25)


p + stat_binhex(bins=10, binwidth=c(3,3)) +
  geom_path(data=land,aes(group=group), size=0.25) +
  scale_fill_gradientn(colours=c('yellow','blue'),name='Frequency',na.value=NA) +
  coord_equal() +
  ylim(-60,90) +
  labs(x=NULL, y=NULL) +
  theme_bw() +
  theme(legend.position=c(0.075,0.28))