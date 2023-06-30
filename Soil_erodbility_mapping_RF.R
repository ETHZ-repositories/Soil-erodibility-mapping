grid <- list.files("D:/Global_maps_1km/Maps1km" , pattern = "*.tif$")
All_cov <- raster::stack(paste0("D:/Global_maps_1km/Maps1km/", grid))
Soil_erod<- read.csv("D:/Erod_for_globe/erod_SE_ksat_28_02.csv")

table(Soil_erod$source_db)
colnames(Soil_erod)

pol.100km = readOGR("D:/Downloads_Surya/tiles_ll_100km_mask.shp")
sp.pnts = Soil_erod[,c("longitude_decimal_degrees", "latitude_decimal_degrees")]
ov.ID = sp::over(SpatialPoints(sp.pnts, proj4string = CRS(proj4string(pol.100km))), pol.100km["ID"])
summary(is.na(ov.ID$ID))
Soil_erod$ID = ov.ID$ID
data<- terra::extract(All_cov,sp.pnts)
SE_dataset<- cbind(Soil_erod,data)

I.vars = make.names(unique(unlist(sapply(c("ID","oc_","clay_","clm","sand_","dtm","latitude", "longitude","lcv","veg", "smectite", "kaoli"), function(i){names(SE_dataset)[grep(i, names(SE_dataset))]}))))

colnames(SE_dataset)

t.vars = c("soil_erodibility1")
sel.n <- c(t.vars,I.vars)
sel.r <- complete.cases(SE_dataset[,sel.n])
PTF_temp2 <- SE_dataset[sel.r,sel.n]

table(PTF_temp2$source_db)

set.seed(11)
chosen <- sample(unique(PTF_temp2$ID),51)

ff<-subset(PTF_temp2, ID %in% chosen)

final<-PTF_temp2[!(PTF_temp2$ID %in% ff$ID),]

library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf() +
  geom_point(data = ff, aes(x = longitude_decimal_degrees, y = latitude_decimal_degrees), size = 3, 
             shape = 21, fill = "darkred")

set.seed(24)
chosen <- sample(unique(final$ID),58)

ff1<-subset(final, ID %in% chosen)

final1<-final[!(final$ID %in% ff1$ID),]

ggplot(data = world) +
  geom_sf() +
  geom_point(data = ff1, aes(x = longitude_decimal_degrees, y = latitude_decimal_degrees), size = 3, 
             shape = 21, fill = "darkred")

set.seed(3288)
chosen <- sample(unique(final1$ID),33)

ff2<-subset(final1, ID %in% chosen)

ggplot(data = world) +
  geom_sf() +
  geom_point(data = ff2, aes(x = longitude_decimal_degrees, y = latitude_decimal_degrees), size = 3, 
             shape = 21, fill = "darkred")

final2<-final1[!(final1$ID %in% ff2$ID),]

set.seed(331)
chosen <- sample(unique(final2$ID),21)

ff3<-subset(final2, ID %in% chosen)

ggplot(data = world) +
  geom_sf() +
  geom_point(data = ff3, aes(x = longitude_decimal_degrees, y = latitude_decimal_degrees), size = 3, 
             shape = 21, fill = "darkred")

final3<-final2[!(final2$ID %in% ff3$ID),]


df1<-ff
df2<-ff1
df3<-ff2
df4<-ff3
df5<-final3


Train1<- rbind(ff, ff1, ff2, ff3)

Train2<- rbind (ff1, ff2, ff3,final3)

Train3<- rbind(ff2, ff3,final3, ff)

Train4<- rbind(ff3,final3, ff,ff1)

Train5<- rbind(final3, ff,ff1, ff2)


set.seed(2) 
fm.ksat <- as.formula(paste("soil_erodibility1~ ",paste(names(All_cov), collapse = "+")))
fm.ksat
# view(Train1)
# # #
# # #
set.seed(2)
m.ksat <- Train1[complete.cases(Train1[,all.vars(fm.ksat)]),]
m.ksat <- ranger(fm.ksat, m.ksat,importance="impurity", num.trees=500, mtry=15, quantreg = TRUE)
m.ksat

df5$prediction<- predict(m.ksat,df5)$predictions


RMSE(df5$prediction, df5$soil_erodibility1)

## Ist_part is computed

m.ksat1 <- Train2[complete.cases(Train2[,all.vars(fm.ksat)]),]
m.ksat1 <- ranger(fm.ksat, m.ksat1, num.trees=500, mtry=15, quantreg = TRUE)
m.ksat1

df1$prediction<- predict(m.ksat1,df1)$predictions

RMSE(df1$prediction, df1$soil_erodibility1)

## 2nd_part is computed
m.ksat2 <- Train3[complete.cases(Train3[,all.vars(fm.ksat)]),]
m.ksat2 <- ranger(fm.ksat, m.ksat2, num.trees=500, mtry=15, quantreg = TRUE)
m.ksat2

df2$prediction<- predict(m.ksat2,df2)$predictions


RMSE(df2$prediction, df2$soil_erodibility1)

## 3rd_part is computed

m.ksat3 <- Train4[complete.cases(Train4[,all.vars(fm.ksat)]),]
m.ksat3 <- ranger(fm.ksat, m.ksat3, num.trees=500, mtry=15, quantreg = TRUE)
m.ksat3
df3$prediction<- predict(m.ksat3,df3)$predictions

RMSE(df3$prediction, df3$soil_erodibility1)

## 4th_part is computed
m.ksat4 <- Train5[complete.cases(Train5[,all.vars(fm.ksat)]),]
m.ksat4 <- ranger(fm.ksat, m.ksat4, num.trees=500, mtry=15, quantreg = TRUE)
m.ksat4

df4$prediction<- predict(m.ksat4,df4)$predictions
RMSE(df4$prediction, df4$soil_erodibility1)


Final_data<- rbind(df1,df2,df3,df4,df5)

colnames(Final_data)

rss <- sum((Final_data$prediction - Final_data$soil_erodibility2_ST) ^ 2)  ## residual sum of squares
tss <- sum((Final_data$soil_erodibility2_ST - mean(Final_data$soil_erodibility2_ST)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss

hh<- lm(Final_data$prediction~Final_data$soil_erodibility2_ST)
summary(hh)

plot(Final_data$prediction,Final_data$soil_erodibility2_ST)

RMSE(Final_data$prediction,Final_data$soil_erodibility2_ST)

bias(Final_data$prediction,Final_data$soil_erodibility2_ST)

ccc = DescTools::CCC(Final_data$prediction,Final_data$soil_erodibility2_ST, ci = "z-transform", conf.level = 0.95, na.rm=TRUE)$rho.c
ccc

hexbinplot(Final_data$soil_erodibility1~ Final_data$prediction, 
           panel = function(x, y, ...){
             panel.hexbinplot(x, y, ...)
             panel.loess(x, y,span = 2/3, col.line = "blue",type="l", lty=2, lwd = 4)
             panel.abline(c(0, 1),lwd = 2)
           },
           data = Final_data,xlab = "Predicted Soil Erod [t ha h ha−1 MJ−1 mm−1]", ylab = "Measured Soil Erod [t ha h ha−1 MJ−1 mm−1]",cex.axis = 4, aspect="1", xbins=30, colramp = function(n) {viridis (8,  alpha = 1, begin = 0, end = 1, direction = -1,option = "C")},xlim=c(0,0.07), ylim=c(0,0.07),
           # scales=list(
           #   x = list(log = 10, equispaced.log = FALSE),
           #   y = list(log = 10, equispaced.log = FALSE)
           # ),
           font.lab= 6, cex.labels = 1.2,font.axis = 2,colorcut=c(0,0.01,0.03,0.07,0.15,0.25,0.5,0.75,1) )


##Final model

set.seed(2)
m.ksat <- PTF_temp2[complete.cases(PTF_temp2[,all.vars(fm.ksat)]),]
m.ksat <- ranger(fm.ksat, m.ksat,importance="impurity", num.trees=500, mtry=15, quantreg = TRUE)
m.ksat

p2 = predict(All_cov,m.ksat, progress='window',type = "response",fun = function(model, ...) predict(model, ...)$predictions)

writeRaster(p2, "D:/Global_maps_1km/Maps1km/Erod_maps/SE_ksat_map.tif")

xl <- as.list(ranger::importance(m.ksat))

par(mfrow=c(1,1),oma=c(0.7,2,0,1), mar=c(4,3.5,1,0))

plot(vv <- t(data.frame(xl[order(unlist(xl), decreasing=TRUE)[10:1]])), 1:10,
     type = "n", ylab = "", yaxt = "n", xlab = "Variable Importance (Node Impurity)",
     cex.axis = .7, cex.lab = .7)
write.csv(vv, "D:/Global_maps_1km/Maps1km/Erod_maps/erod_ksat_imp_cov.csv")