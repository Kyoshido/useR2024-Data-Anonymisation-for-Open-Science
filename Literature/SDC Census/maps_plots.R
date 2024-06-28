################################
# maps graphics
#


source("~/learn4sdg/R/FramePredictions/00_helpers.R")
source("~/learn4sdg/R/Validation/compareSILCTables_helpers.R")

library(raster)
library(leaflet)
library(rgdal)
library(dplyr)
library(data.table)

mountSTAT::mountMeth()

# pol bezirke
area_shape <- load_shapefile(path="https://data.statistik.gv.at/data/OGDEXT_POLBEZ_1_STATISTIK_AUSTRIA_20200101.zip",
                             layer="STATISTIK_AUSTRIA_POLBEZ_20200101Polygon")
df.polygon <- spTransform(area_shape, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 
     +no_defs"))
Shape.plot <- data.table(ggplot2::fortify(df.polygon))
Shape.plot[,POL:=as.character(df.polygon$id[as.numeric(id)+1])]

# nuts2 regionen
area_shape <- load_shapefile(path="https://data.statistik.gv.at/data/OGDEXT_NUTS_1_STATISTIK_AUSTRIA_NUTS2_20160101.zip",
                             layer="STATISTIK_AUSTRIA_NUTS2_20160101")
df.polygon <- spTransform(area_shape, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 
     +no_defs"))
nuts2.plot <- data.table(ggplot2::fortify(df.polygon))
nuts2.plot[,NUTS2:=as.character(df.polygon$ID[as.numeric(id)+1])]


# marker set
set.seed(123456)
Shape.plot[,r1:=diff(range(long))/4,by=.(POL)]
Shape.plot[,r2:=diff(range(lat))/4,by=.(POL)]
Shape.plot[substr(POL,1,1)==9,r1:=diff(range(long))/4]
Shape.plot[substr(POL,1,1)==9,r2:=diff(range(lat))/4]

setnames(Shape.plot,"group","group2")
data_border_pol <- as.data.frame(Shape.plot[substr(POL,1,1) %in% c("3","9")])

data_border_nuts2 <- as.data.frame(nuts2.plot)

# first plot
vienna <- Shape.plot[substr(POL,1,1)==9,.(long=mean(long) + runif(1,min=-r1[1],max=r1[1]),lat=mean(lat) + runif(1,min=-r2[1],max=r2[1]))]
vienna[,POL:=9]
lowerost <- Shape.plot[substr(POL,1,1)==3,.(long=16.65,lat=48.3)]
data_image <- rbindlist(list(vienna,lowerost),use.names=TRUE,fill=TRUE)
data_image$image <- file.path(mountSTAT::mountMeth(),"Projekte/ESTP-SDC/Census_SDC/icons8-huette-96_grey_V2.png")
data_image <- as.data.frame(data_image)

data_arrow <- copy(data_image)
setDT(data_arrow)
data_arrow$Index <- 1
data_arrow$Index2 <- 1:nrow(data_arrow)
data_arrow <- dcast(data_arrow,Index~Index2,value.var=c("long","lat"))

p1 <- ggplot(data_border_nuts2, aes(long, lat)) +
  geom_polygon(aes(group=group),fill = "white", colour = "black",size=2) + 
  geom_polygon(data=data_border_pol,aes(long,lat,group=group2),fill = "white", colour = "grey50",linetype="dashed",alpha=0)+
  coord_cartesian(xlim=c(16.2,16.7),ylim=c(48.1,48.35))+
  geom_image(data=data_image,aes(long,lat,image=image), size=.05)+
  geom_curve(data=data_arrow,
             aes(x = long_1+0.01, y = lat_1-0.01, xend = long_2-0.01, yend = lat_2-0.01),
             arrow = arrow(length = unit(0.03, "npc")),
             colour = "#EC7014",
             size = 1.2
  )+
  geom_curve(data=data_arrow,
             aes(x = long_2-0.01, y = lat_2+0.01, xend = long_1+0.01, yend = lat_1+0.01),
             arrow = arrow(length = unit(0.03, "npc")),
             colour = "#EC7014",
             size = 1.2
  )+
  theme_void()
plot(p1)
png(file="SDC Census/switch1.png")
plot(p1)
dev.off()


# plot with ggplo2
# make houses
set.seed(2021)
vienna <- Shape.plot[substr(POL,1,1)==9,.(long=mean(long) + runif(1,min=-r1[1],max=r1[1]),lat=mean(lat) + runif(1,min=-r2[1],max=r2[1]))]
vienna[,POL:=9]
lowerost <- Shape.plot[substr(POL,1,1)==3,.(long=mean(long) + runif(1,min=-r1[1],max=r1[1]),lat=mean(lat) + runif(1,min=-r2[1],max=r2[1])),by=.(POL)]

data_image <- rbindlist(list(vienna,lowerost),use.names=TRUE,fill=TRUE)
data_image$image <- file.path(mountSTAT::mountMeth(),"Projekte/ESTP-SDC/Census_SDC/icons8-huette-96_blue_V2.png")

data_image[,risk:=round(runif(.N,0.01,0.3),2)]
data_image[,risk:=paste("Risk=",risk)]
data_image[POL==9,risk:="Risk= 0.5!"]

p1 <- ggplot(data_border_nuts2, aes(long, lat)) +
  geom_polygon(aes(group=group),fill = "white", colour = "black",size=2) + 
  geom_polygon(data=data_border_pol,aes(long,lat,group=group2),fill = "white", colour = "grey50",linetype="dashed",alpha=0)+
  coord_cartesian(xlim=c(14.5,17),ylim=c(47.5,49))+
  geom_image(data=data_image,aes(long,lat,image=image), size=.05)+
  geom_label(data=data_image[POL!=9],aes(long,lat,label=risk),size=3,hjust = 0, nudge_y=-0.05,fontface = "bold", color="black")+
  geom_label(data=data_image[POL==9],aes(long,lat,label=risk),size=3,hjust = 0, nudge_y=-0.05,fontface = "bold", color="red")+
  theme_void()
plot(p1)

png(file="SDC Census/Swap_demo1.png",width = 750, height = 525)
plot(p1)
dev.off()

data_image[POL==9,image:=file.path(mountSTAT::mountMeth(),"Projekte/ESTP-SDC/Census_SDC/icons8-huette-96_green_V2.png")]
data_text <- data_image[POL==9]
data_text[,label:="high risk\nhousehold"]

p2 <- ggplot(data_border_nuts2, aes(long, lat)) +
  geom_polygon(aes(group=group),fill = "white", colour = "black",size=2) + 
  geom_polygon(data=data_border_pol,aes(long,lat,group=group2),fill = "white", colour = "grey50",linetype="dashed",alpha=0)+
  coord_cartesian(xlim=c(14.5,17),ylim=c(47.5,49))+
  geom_image(data=data_image,aes(long,lat,image=image), size=.05)+
  geom_label(data=data_text,aes(label=label),size=5,hjust = 0, nudge_x = 0.05,nudge_y=0.05,fontface = "bold", color="darkgreen")+
  theme_void()
plot(p2)

png(file="SDC Census/Swap_demo2.png",width = 750, height = 525)
plot(p2)
dev.off()

data_image[POL %in% 325,image:=file.path(mountSTAT::mountMeth(),"Projekte/ESTP-SDC/Census_SDC/icons8-huette-96_green_V2.png")]

data_text <- data_image[image %like% "96_green_V2"]
data_text[POL==9,label:="high risk\nhousehold"]
data_text[POL!=9,label:="donor household"]

p3 <- ggplot(data_border_nuts2, aes(long, lat)) +
  geom_polygon(aes(group=group),fill = "white", colour = "black",size=2) + 
  geom_polygon(data=data_border_pol,aes(long,lat,group=group2),fill = "white", colour = "grey50",linetype="dashed",alpha=0)+
  coord_cartesian(xlim=c(14.5,17),ylim=c(47.5,49))+
  geom_image(data=data_image,aes(long,lat,image=image), size=.05)+
  geom_label(data=data_text,aes(label=label),size=5,hjust = 0, nudge_x = 0.05,nudge_y=0.05,fontface = "bold", color="darkgreen")+
  theme_void()
plot(p3)

png(file="SDC Census/Swap_demo3.png",width = 750, height = 525)
plot(p3)
dev.off()


data_arrow <- copy(data_image[image %like% "96_green_V2"])
data_arrow$Index <- 1
data_arrow$Index2 <- 1:nrow(data_arrow)
data_arrow <- dcast(data_arrow,Index~Index2,value.var=c("long","lat"))
shift <- 0.05
p4 <- ggplot(data_border_nuts2, aes(long, lat)) +
  geom_polygon(aes(group=group),fill = "white", colour = "black",size=2) + 
  geom_polygon(data=data_border_pol,aes(long,lat,group=group2),fill = "white", colour = "grey50",linetype="dashed",alpha=0)+
  coord_cartesian(xlim=c(14.5,17),ylim=c(47.5,49))+
  geom_image(data=data_image,aes(long,lat,image=image), size=.05)+
  geom_label(data=data_text,aes(label=label),size=5,hjust = 0, nudge_x = 0.05,nudge_y=0.05,fontface = "bold", color="darkgreen")+
  geom_curve(data=data_arrow,
             aes(x = long_1, y = lat_1+shift, xend = long_2+shift, yend = lat_2),
             arrow = arrow(length = unit(0.03, "npc")),
             colour = "#EC7014",
             size = 1.2
  )+
  geom_curve(data=data_arrow,
             aes(x = long_2, y = lat_2-shift, xend = long_1-shift, yend = lat_1),
             arrow = arrow(length = unit(0.03, "npc")),
             colour = "#EC7014",
             size = 1.2
  )+
  theme_void()
plot(p4)

png(file="SDC Census/Swap_demo4.png",width = 750, height = 525)
plot(p4)
dev.off()

data_image[POL %in% data_image[!POL %in% c(9,311,325),sample(POL,5)],image:=file.path(mountSTAT::mountMeth(),"Projekte/ESTP-SDC/Census_SDC/icons8-huette-96_red_V2.png")]
data_image[POL %in% 325,image:=file.path(mountSTAT::mountMeth(),"Projekte/ESTP-SDC/Census_SDC/icons8-huette-96_red_V2.png")]
data_image[POL %in% 311,image:=file.path(mountSTAT::mountMeth(),"Projekte/ESTP-SDC/Census_SDC/icons8-huette-96_green_V2.png")]

data_text <- data_image[!image %like% "96_blue_V2"]
data_text[POL==9,label:="high risk\nhousehold"]
data_text[POL %in% 311,label:="donor household"]
data_text[!POL %in% c(311,9),label:="not similar"]

data_arrow <- copy(data_image[image %like% "96_green_V2"])
data_arrow$Index <- 1
data_arrow$Index2 <- 1:nrow(data_arrow)
data_arrow <- dcast(data_arrow,Index~Index2,value.var=c("long","lat"))
shift <- 0.05
p5 <- ggplot(data_border_nuts2, aes(long, lat)) +
  geom_polygon(aes(group=group),fill = "white", colour = "black",size=2) + 
  geom_polygon(data=data_border_pol,aes(long,lat,group=group2),fill = "white", colour = "grey50",linetype="dashed",alpha=0)+
  coord_cartesian(xlim=c(14.5,17),ylim=c(47.5,49))+
  geom_image(data=data_image,aes(long,lat,image=image), size=.05)+
  geom_label(data=data_text[POL %in% c(311,9)],aes(label=label),size=5,hjust = 0, nudge_x = 0.05,nudge_y=0.05,fontface = "bold", color="darkgreen")+
  geom_label(data=data_text[!POL %in% c(311,9)],aes(label=label),size=5,hjust = 0, nudge_x = 0.05,nudge_y=0.05,fontface = "bold", color="darkred")+
  geom_curve(data=data_arrow,
             aes(x = long_1, y = lat_1+shift, xend = long_2+shift, yend = lat_2),
             arrow = arrow(length = unit(0.03, "npc")),
             colour = "#EC7014",
             size = 1.2
  )+
  geom_curve(data=data_arrow,
             aes(x = long_2, y = lat_2-shift, xend = long_1-shift, yend = lat_1),
             arrow = arrow(length = unit(0.03, "npc")),
             colour = "#EC7014",
             size = 1.2
  )+
  theme_void()
plot(p5)

png(file="SDC Census/Swap_similar.png",width = 750, height = 525)
plot(p5)
dev.off()

# create awesome icons
my_icons <- awesomeIcons(icon = data_all$icon,
                         markerColor = data_all$color,
                         library = "glyphicon")
data_all %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolylines(data = data_border_pol, lng = ~long, lat = ~lat, group=~POL, color = "black",opacity = .7,stroke = "dashed") %>%
  addPolylines(data = data_border_nuts2, lng = ~long, lat = ~lat, group=~NUTS2, color = "black",opacity = .7)%>%
  addAwesomeMarkers(lng = ~ long, lat = ~ lat, icon = ~ my_icons[Group]) 

data_all[5,]$color<-"green"
data_all[11,]$color<-"blue"

my_icons <- awesomeIcons(icon = data_all$icon,
                         markerColor = data_all$color,
                         library = "glyphicon")
data_all %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addAwesomeMarkers(lng = ~ long, lat = ~ lat, icon = ~ my_icons[Group])



library("ggimage")

# create a df

set.seed(2017-02-21)
d <- data.frame(x = rnorm(10),
                y = rnorm(10),
                image = sample(c("~/mnt/mdrive/Projekte/ESTP-SDC/Census_SDC/icons8-huette-96_red_V2.png",
                                 "~/mnt/mdrive/Projekte/ESTP-SDC/Census_SDC/icons8-huette-96_blue_V2.png",
                                 "~/mnt/mdrive/Projekte/ESTP-SDC/Census_SDC/icons8-huette-96_green_V2.png"),
                               size=10, replace = TRUE)
)
# plot2
ggplot(d, aes(x, y)) + geom_image(aes(image=image), size=.05)




data_all %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addAwesomeMarkers(lng = ~ long, lat = ~ lat, icon = ~ my_icons[Group])



lat1= 36+runif(n=5,min=-1,max=1)
lon1 =-115+runif(n=5,min=-1,max=1)

lat2= 35+runif(n=5,min=-0.5,max=0.5)
lon2 =-110+runif(n=5,min=-0.5,max=0.5)

lat3= 34+runif(n=5,min=-0.5,max=0.5)
lon3 =-112+runif(n=5,min=-0.5,max=0.5)

data_all=rbind(data.frame(Longitude=lon1,Latitude=lat1,Group=1),
               data.frame(Longitude=lon2,Latitude=lat2,Group=2),
               data.frame(Longitude=lon3,Latitude=lat3,Group=3))
data_all$color <- rep(c("red", "green", "gray"), 5)

# add icon label column
data_all <- data_all %>%
  mutate(icon = case_when(
    Group == 1 ~ "home",
    Group == 2 ~ "cog",
    Group == 3 ~ "camera"))

# create awesome icons
my_icons <- awesomeIcons(icon = data_all$icon,
                         markerColor = data_all$color,
                         library = "glyphicon")

# leaflet using AwesomeMarkers
data_all %>% 
  leaflet() %>% 
  addTiles() %>% 
  addAwesomeMarkers(lng = ~ Longitude, lat = ~ Latitude, icon = ~ my_icons[Group])


