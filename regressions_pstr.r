library(data.table)
library(ggplot2)
library(cowplot)
library(fixest)
library(lubridate)
library(dummies)
library(devtools)
library(tibble)
library(backports)
# install_github("yukai-yang/PSTR")
library(PSTR)

rm(list=ls())
gc()

source("myscript.r")

# the 'not in' function
'%!in%' <- Negate('%in%')

# the 'normalizing' function
mean_adj <- function(x){
  x <- x/mean(x)
  return(x)
}

# load the price and conflict data
load("dataset.RData")

# control variables
combined_dt[,w1 := x*Within_Reg]
combined_dt[,w2 := x*Distance_km/100]
combined_dt[,w3 := x*Travel_hr]

rice.fe <- feols(y~x+w1+w2 | Dyad+Month, combined_dt[Crop=="Rice"])
r.base <- summary(rice.fe)

maize.fe <- feols(y~x+w1+w2  | Dyad+Month, combined_dt[Crop=="Maize"])
m.base <- summary(maize.fe)

sorghum.fe <- feols(y~x+w1+w2  | Dyad+Month, combined_dt[Crop=="Sorghum"])
s.base <- summary(sorghum.fe)

## Table 4.a
linear_tab <- etable(r.base,m.base,s.base,drop=c("w1","w2"))

##-- Baseline, Dyad FE
rice.fe <- feols(y~x:I(1-s1bar)+x:s1bar+w1+w2 | Dyad+Month, combined_dt[Crop=="Rice"])
r.inter <- summary(rice.fe)

maize.fe <- feols(y~x:I(1-s1bar)+x:s1bar+w1+w2  | Dyad+Month, combined_dt[Crop=="Maize"])
m.inter <- summary(maize.fe)

sorghum.fe <- feols(y~x:I(1-s1bar)+x:s1bar+w1+w2  | Dyad+Month, combined_dt[Crop=="Sorghum"])
s.inter <- summary(sorghum.fe)

##-------------------------------------------------------------------------
## to obtain statistical significance in the difference between the regimes
rice.fe <- feols(y~x+x:s1bar+w1+w2 | Dyad+Month, combined_dt[Crop=="Rice"])
r.inter.d <- summary(rice.fe)

maize.fe <- feols(y~x+x:s1bar+w1+w2  | Dyad+Month, combined_dt[Crop=="Maize"])
m.inter.d <- summary(maize.fe)

sorghum.fe <- feols(y~x+x:s1bar+w1+w2  | Dyad+Month, combined_dt[Crop=="Sorghum"])
s.inter.d <- summary(sorghum.fe)
##-------------------------------------------------------------------------

## Table 4.c
interacted_tab <- etable(r.inter,m.inter,s.inter,drop=c("w1","w2"))


crop <- "Rice"
crop_dt <- combined_dt[Crop==crop]

# save as tibble to use the homogeneity test from PSTR the package 
crop_tb <- as_tibble(crop_dt)

# candidate transition variables:
tvec <- c('s1bar','s3bar','s6bar','s12bar')

pstr <- NewPSTR(crop_tb,dep='y',indep=c("x"),indep_k=c("x"),tvars=tvec[1],im=2,iT=length(unique(crop_dt$Date)))


## the next few lines contribute to Table 3
# homogeneity test: F-distribution
options(warn=0)
LinTest(pstr)

# the bootstrap version (takes a little bit of time)
wb_test <- WCB_LinTest(pstr,iB=999)

wb_test$wcb_test
wb_test$wcb_sqtest

# define the transition variable and include in the dataset
tvr <- 's1bar'
combined_dt$tv <- combined_dt[,..tvr]

# estimte the PSTR
pstr_coef <- my_pstr(crop_dt,w=c("w1","w2","Month_01","Month_02","Month_03","Month_04","Month_05","Month_06","Month_07","Month_08","Month_09","Month_10","Month_11"),tv=tvr,g_start=0.5,c_start=.5,tf=1,max_it=100)

options(warn=0)

crop_dt$tv <- crop_dt[,..tvr]
if(length(pstr_coef)==2){
  crop_dt$G_tv <- ((1+exp(-pstr_coef["g1"]*(crop_dt$tv-pstr_coef["c1"])/sd(crop_dt$tv)))^(-1))
}else{
  crop_dt$G_tv <- ((1+exp(-pstr_coef["g1"]*(crop_dt$tv-pstr_coef["c11"])*(crop_dt$tv-pstr_coef["c12"])/sd(crop_dt$tv)^2))^(-1))
}

# re-estimate the PSTR by fixing the estimated smoothness and centrality
# parameters and fitting the regression as a fixed effects model with
# monthly dummy variables
crop.fe <- feols(y~x:I(1-G_tv)+x:G_tv+w1+w2 | Dyad+Month, crop_dt)
r.pstr <- summary(crop.fe)

##-------------------------------------------------------------------------
## to obtain statistical significance in the difference between the regimes
rice.fe <- feols(y~x+x:G_tv+w1+w2 | Dyad+Month, crop_dt)
r.pstr.d <- summary(rice.fe)
##-------------------------------------------------------------------------

## Table 4.b
pstr_tab <- etable(r.pstr,drop=c("w1","w2"))

# plot the estimated transition function
gg_tf <- ggplot(crop_dt,aes(x=tv,y=G_tv))+
  geom_point(size=1,color="indianred")+
  labs(y=bquote(G({c^"*"}[ijt-1],gamma,kappa)),x=bquote({c^"*"}[ijt-1]))+
  theme_classic()+
  theme(axis.title = element_text(size=12), axis.text = element_text(size=10),legend.title = element_blank(),legend.text = element_text(size=8),legend.position = "none")

## Figure A2
ggsave("transition.png",gg_tf,width=6.5,height=3.5)


# # un-comment from here forward and run this loop to generate dyad-specific
# # figures of transition variables and transition functions over time
# # (not included in the paper)
# for(i in 1:length(unique(crop_dt$Dyad))){
# 
#   dt <- crop_dt[Dyad==unique(Dyad)[i]]
#   dt <- dt[,.(Dyad,Market1,Market2,Date,x,tv,G_tv,z=z0)]
# 
#   dt$aa <- NA
#   dt$bb <- NA
#   for(s in 2:(nrow(dt))){
#     if(dt$G_tv[s] >= mean(dt$G_tv,na.rm=T) & dt$G_tv[s-1] < mean(dt$G_tv,na.rm=T)){
#       dt$aa[s] <- as.character(dt$Date[s])
#     }else if(dt$G_tv[s] < mean(dt$G_tv,na.rm=T) & dt$G_tv[s-1] >= mean(dt$G_tv,na.rm=T)){
#       dt$bb[s] <- as.character(dt$Date[s])
#     }
#   }
# 
#   if(dt$G_tv[1] >= mean(dt$G_tv,na.rm=T)){
#     dt$aa[1] <- as.character(dt$Date[1])
#   }
#   if(dt$G_tv[nrow(dt)] >= mean(dt$G_tv,na.rm=T)){
#     dt$bb[nrow(dt)] <- as.character(dt$Date[nrow(dt)])
#   }
# 
#   dt$aa <- as.Date(dt$aa)
#   dt$bb <- as.Date(dt$bb)
# 
#   dt$Date1 <- shift(dt$Date,type="lead")
# 
#   gg_dyad <- ggplot(dt,aes(x=decimal_date(Date)))+
#     geom_line(aes(y=z),color="indianred",size=.8,na.rm=T)+
#     geom_rect(aes(xmin=decimal_date(dt$Date),xmax=decimal_date(dt$Date1),ymin=-Inf,ymax=Inf),fill=alpha("indianred",dt$G_tv*.25),na.rm=T)+
#     scale_x_continuous(breaks=c(2009,2012,2015,2018))+
#     labs(title=paste(dt$Market1[1],"\u2013",dt$Market2[1],sep=""),y=expression(c[ijt]),x="Year")+
#     theme_classic()+
#     theme(axis.title = element_text(size=12), axis.text = element_text(size=10),legend.title = element_blank(),legend.text = element_text(size=8),legend.position = "none")
# 
#   ggsave(paste(substr(dt$Market1[1],1,3),"_",substr(dt$Market2[1],1,3),".png",sep=""),gg_dyad,width=6.5,height=3.5)
# 
# }
