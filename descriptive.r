library(data.table)
library(ggplot2)
library(cowplot)

'%!in%' <- Negate('%in%')

mean_adj <- function(x){
  x <- (x)/mean(x)
  return(x)
}

# load data
load("dataset.RData") 


############
## TABLES ##
############

### conflict incidents
summary_combined_dt <- combined_dt[,.(Incidents=sum(z0)),by=.(Crop,Dyad)]
summary_combined_dt <- summary_combined_dt[,.(Dyad,Incidents)]
summary_combined_dt <- unique(summary_combined_dt)

geo_conflict_merge <- merge(distance_dt,summary_combined_dt,by="Dyad")

## Table 1 (note: slight discrepancy due to the omitted 2009 here)
geo_conflict_tab <- geo_conflict_merge[order(-Incidents)]


### prices
summary_dt <- prices_dt[,.(p_mean=round(mean(PRICE_USD_Complete),2),p_sd=round(sd(PRICE_USD_Complete),2),p_min=min(PRICE_USD_Complete),p_max=max(PRICE_USD_Complete)),by=.(Market,Commodity)]

summary_prices_dt <- summary_dt[order(Commodity,Market)]
summary_prices_dt <- summary_prices_dt[Market %in% cities_dt$Market]

## Table 2
average_prices_tab <- dcast(summary_prices_dt,Market~Commodity,value.var="p_mean")


#############
## FIGURES ##
#############

prices_dt$Commodity <- sub(" .+","",prices_dt$Commodity)
prices_dt$Commodity <- factor(prices_dt$Commodity,levels=unique(prices_dt$Commodity)[c(2,1,3)])

gg_prices <- ggplot(prices_dt[Market %!in% c("Hargeisa","Borama","Qorioley")],aes(x=Date,y=PRICE_USD,color=Market,linetype=Market))+
  geom_line(size=.3)+
  facet_grid(. ~ Commodity)+
  labs(x="Year",y="Price (USD/kg)")+
  theme_classic()+
  theme(axis.title = element_text(size=10), axis.text = element_text(size=8),legend.title = element_blank(),legend.text = element_text(size=8),legend.position = "bottom",legend.background = element_rect("transparent"),strip.background = element_rect(fill="transparent",size = NA))

## Figure 2
ggsave("prices.png",gg_prices,width=6.5,height=3.5)


combined_dt$Dyad_long <- paste(combined_dt$Market1,"-",combined_dt$Market2,sep="")

gg_incidents <- ggplot(combined_dt,aes(x=reorder(Dyad_long,-z0),y=z0))+
  geom_boxplot(color="indianred")+
  labs(x="Dyad",y="Monthly Incidents")+
  theme_classic()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8),axis.text.x=element_text(angle=45,hjust=1),axis.text.y=element_text(angle=90,hjust=.5))

## Figure A1.a
ggsave("incidents.png",gg_incidents,width=6.5,height=3.5)


combined_dt[,Normalized := mean_adj(z0),by=c("Dyad")]

gg_normalized <- ggplot(combined_dt,aes(x=reorder(Dyad_long,-z0),y=Normalized))+
  geom_boxplot(color="indianred")+
  labs(x="Dyad",y="Monthly Incidents (Normalized)")+
  theme_classic()+
  theme(axis.title=element_text(size=10),axis.text=element_text(size=8),axis.text.x=element_text(angle=45,hjust=1),axis.text.y=element_text(angle=90,hjust=.5))

## Figure A1.b
ggsave("normalized.png",gg_normalized,width=6.5,height=3.5)
