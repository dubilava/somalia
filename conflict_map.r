library(data.table)
library(sf)
library(ggrepel)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

rm(list=ls())
gc()

world <- ne_countries(scale="large",returnclass="sf")
horn <- subset(map_data("world"),region %in% c("Kenya","Tanzania","Sudan","Ethiopia","Eritrea","Djibouti"))
somalia <- subset(map_data("world"),region %in% c("Somalia"))

load("dataset.RData")

# create the buffer around the trade routes
trade_routes_bf <- st_buffer(trade_routes_sf,dist=20000)

# conflict data (selecting the three categories used in the analysis)
incidents_dt <- conflict_dt[Event %in% c("Battles","Explosions/Remote violence","Violence against civilians")]

# coordinates of the reported incidents
point_coordinates <- incidents_dt[,.(Longitude,Latitude)]

# save these in a sf format with the same crs as trade routes for plotting
incidents_sf <- st_as_sf(point_coordinates,coords=c("Longitude","Latitude"),crs = st_crs(trade_routes_bf))

# identify incidents that fall within the buffer and those that don't
mat <- st_intersects(incidents_sf,trade_routes_bf,sparse = F)
incidents_dt$OnRoad <- 0
incidents_dt$OnRoad[apply(mat,1,any)] <- 1

## aggregate geo-coordinates to four decimal points
incidents_aggregate_dt <- incidents_dt[,.(Incidents=.N),by=.(Longitude=round(Longitude,4),Latitude=round(Latitude,4),OnRoad)]

## points in sf format for mapping
pts_onroad <- cbind(as.numeric(incidents_aggregate_dt[OnRoad==1]$Longitude),as.numeric(incidents_aggregate_dt[OnRoad==1]$Latitude))
pts_onroad_sf <- st_cast(st_sfc(st_multipoint(pts_onroad),crs=4326),"POINT")
pts_onroad_sf <- st_sf(data.frame(Incidents=incidents_aggregate_dt[OnRoad==1]$Incidents),geometry=pts_onroad_sf)

pts_offroad <- cbind(as.numeric(incidents_aggregate_dt[OnRoad==0]$Longitude),as.numeric(incidents_aggregate_dt[OnRoad==0]$Latitude))
pts_offroad_sf <- st_cast(st_sfc(st_multipoint(pts_offroad),crs=4326),"POINT")
pts_offroad_sf <- st_sf(data.frame(Incidents=incidents_aggregate_dt[OnRoad==0]$Incidents),geometry=pts_offroad_sf)

gg_map <- ggplot() +
  geom_polygon(data=horn,aes(x=long,y=lat,group=group),color="gray70",fill="gray95",linetype=3) +
  geom_polygon(data=somalia,aes(x=long,y=lat,group=group),color="gray30",fill="ivory") +
  geom_sf(data=trade_routes_sf,size=1) + 
  geom_sf(data=pts_onroad_sf,aes(size=Incidents),shape=21,fill="indianred",alpha=.5) + 
  geom_sf(data=pts_offroad_sf,aes(size=Incidents),shape=21,fill=NA,alpha=.5,show.legend=F) + 
  scale_size(range=c(1,10),breaks=c(1,10,50,200,500)) +
  coord_sf(xlim=c(38,53),ylim=c(-3,13),expand=F) +
  geom_point(data=cities_dt,aes(x=Longitude,y=Latitude),color="gray30",fill="gray70",size=1,shape=1) +
  geom_text_repel(data=cities_dt[Capital==0],aes(x=Longitude,y=Latitude,label=Market),size=5,nudge_x=c(2,-3,0,-3,-4,-3,-4,-2,-3,-3),nudge_y=c(0,0,-1,0,0,0,0,0,0,0))+
  geom_text_repel(data=cities_dt[Capital==1],aes(x=Longitude,y=Latitude,label=Market),size=6,nudge_x=c(2),nudge_y=c(-1))+
  theme(panel.background=element_blank(),axis.title=element_blank(),legend.background=element_blank(),legend.key=element_blank(),legend.position=c(1,0),legend.justification=c(1,0),legend.title=element_text(size=14),legend.text=element_text(size=12,hjust=1),panel.spacing=unit(0,"lines"))

## Figure 1
ggsave("incidents.png",gg_map,width=6.5,height=6.5)


