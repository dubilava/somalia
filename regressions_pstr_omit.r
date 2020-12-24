library(data.table)
library(ggplot2)
library(cowplot)
library(fixest)
library(lubridate)
library(dummies)

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

# crop
crop <- "Rice"
crop_dt <- combined_dt[Crop==crop]

# transition variable
tvr <- "s1bar"
crop_dt$tv <- crop_dt[,..tvr]

# control variables (interacted with lagged price differential)
crop_dt[,w1 := x*Within_Reg]
crop_dt[,w2 := x*Distance_km/100]
crop_dt[,w3 := x*Travel_hr]


## set of regressions

### baseline (reg+dist+month)
pstr_coef <- my_pstr(crop_dt,w=c("w1","w2","Month_01","Month_02","Month_03","Month_04","Month_05","Month_06","Month_07","Month_08","Month_09","Month_10","Month_11"),tv=tvr,g_start=.5,c_start=.5,tf=1,max_it=100)
crop_dt$G_tv <- ((1+exp(-pstr_coef["g1"]*(crop_dt$tv-pstr_coef["c1"])/sd(crop_dt$tv)))^(-1))
crop.fe <- feols(y~x:I(1-G_tv)+x:G_tv+w1+w2 | Dyad+Month, crop_dt)
r.rdm <- summary(crop.fe)

### reg+time+month
pstr_coef <- my_pstr(crop_dt,w=c("w1","w3","Month_01","Month_02","Month_03","Month_04","Month_05","Month_06","Month_07","Month_08","Month_09","Month_10","Month_11"),tv=tvr,g_start=.5,c_start=.5,tf=1,max_it=100)
crop_dt$G_tv <- ((1+exp(-pstr_coef["g1"]*(crop_dt$tv-pstr_coef["c1"])/sd(crop_dt$tv)))^(-1))
crop.fe <- feols(y~x:I(1-G_tv)+x:G_tv+w1+w3 | Dyad + Month, crop_dt)
r.rtm <- summary(crop.fe)

### reg+month
pstr_coef <- my_pstr(crop_dt,w=c("w1","Month_01","Month_02","Month_03","Month_04","Month_05","Month_06","Month_07","Month_08","Month_09","Month_10","Month_11"),tv=tvr,g_start=.5,c_start=.5,tf=1,max_it=100)
crop_dt$G_tv <- ((1+exp(-pstr_coef["g1"]*(crop_dt$tv-pstr_coef["c1"])/sd(crop_dt$tv)))^(-1))
crop.fe <- feols(y~x:I(1-G_tv)+x:G_tv+w1 | Dyad + Month, crop_dt)
r.regm <- summary(crop.fe)

### dist+month
pstr_coef <- my_pstr(crop_dt,w=c("w2","Month_01","Month_02","Month_03","Month_04","Month_05","Month_06","Month_07","Month_08","Month_09","Month_10","Month_11"),tv=tvr,g_start=.5,c_start=.5,tf=1,max_it=100)
crop_dt$G_tv <- ((1+exp(-pstr_coef["g1"]*(crop_dt$tv-pstr_coef["c1"])/sd(crop_dt$tv)))^(-1))
crop.fe <- feols(y~x:I(1-G_tv)+x:G_tv+w2 | Dyad + Month, crop_dt)
r.distm <- summary(crop.fe)

### time+month
pstr_coef <- my_pstr(crop_dt,w=c("w3","Month_01","Month_02","Month_03","Month_04","Month_05","Month_06","Month_07","Month_08","Month_09","Month_10","Month_11"),tv=tvr,g_start=.5,c_start=.5,tf=1,max_it=100)
crop_dt$G_tv <- ((1+exp(-pstr_coef["g1"]*(crop_dt$tv-pstr_coef["c1"])/sd(crop_dt$tv)))^(-1))
crop.fe <- feols(y~x:I(1-G_tv)+x:G_tv+w3 | Dyad + Month, crop_dt)
r.timem <- summary(crop.fe)

### reg
pstr_coef <- my_pstr(crop_dt,w=c("w1"),tv=tvr,g_start=.5,c_start=.5,tf=1,max_it=100)
crop_dt$G_tv <- ((1+exp(-pstr_coef["g1"]*(crop_dt$tv-pstr_coef["c1"])/sd(crop_dt$tv)))^(-1))
crop.fe <- feols(y~x:I(1-G_tv)+x:G_tv+w1 | Dyad, crop_dt)
r.reg <- summary(crop.fe)

### dist
pstr_coef <- my_pstr(crop_dt,w=c("w2"),tv=tvr,g_start=.5,c_start=.5,tf=1,max_it=100)
crop_dt$G_tv <- ((1+exp(-pstr_coef["g1"]*(crop_dt$tv-pstr_coef["c1"])/sd(crop_dt$tv)))^(-1))
crop.fe <- feols(y~x:I(1-G_tv)+x:G_tv+w2 | Dyad, crop_dt)
r.dist <- summary(crop.fe)

### time
pstr_coef <- my_pstr(crop_dt,w=c("w3"),tv=tvr,g_start=.5,c_start=.5,tf=1,max_it=100)
crop_dt$G_tv <- ((1+exp(-pstr_coef["g1"]*(crop_dt$tv-pstr_coef["c1"])/sd(crop_dt$tv)))^(-1))
crop.fe <- feols(y~x:I(1-G_tv)+x:G_tv+w3 | Dyad, crop_dt)
r.time <- summary(crop.fe)

## unique list of markets
market <- unique(c(crop_dt$Market1,crop_dt$Market2))
market <- market[order(market)]

### omit 1
omit_dt <- crop_dt[(Market1!=market[1] & Market2!=market[1])]

pstr_coef <- my_pstr(omit_dt,w=c("w1","w2","Month_01","Month_02","Month_03","Month_04","Month_05","Month_06","Month_07","Month_08","Month_09","Month_10","Month_11"),tv=tvr,g_start=.5,c_start=.5,tf=1,max_it=100)
omit_dt$G_tv <- ((1+exp(-pstr_coef["g1"]*(omit_dt$tv-pstr_coef["c1"])/sd(omit_dt$tv)))^(-1))
crop.fe <- feols(y~x:I(1-G_tv)+x:G_tv+w1+w2 | Dyad + Month, omit_dt)
r.omit1 <- summary(crop.fe)

### omit 2
omit_dt <- crop_dt[(Market1!=market[2] & Market2!=market[2])]

pstr_coef <- my_pstr(omit_dt,w=c("w1","w2","Month_01","Month_02","Month_03","Month_04","Month_05","Month_06","Month_07","Month_08","Month_09","Month_10","Month_11"),tv=tvr,g_start=.5,c_start=.5,tf=1,max_it=100)
omit_dt$G_tv <- ((1+exp(-pstr_coef["g1"]*(omit_dt$tv-pstr_coef["c1"])/sd(omit_dt$tv)))^(-1))
crop.fe <- feols(y~x:I(1-G_tv)+x:G_tv+w1+w2 | Dyad + Month, omit_dt)
r.omit2 <- summary(crop.fe)

### omit 3
omit_dt <- crop_dt[(Market1!=market[3] & Market2!=market[3])]

pstr_coef <- my_pstr(omit_dt,w=c("w1","w2","Month_01","Month_02","Month_03","Month_04","Month_05","Month_06","Month_07","Month_08","Month_09","Month_10","Month_11"),tv=tvr,g_start=.5,c_start=.5,tf=1,max_it=100)
omit_dt$G_tv <- ((1+exp(-pstr_coef["g1"]*(omit_dt$tv-pstr_coef["c1"])/sd(omit_dt$tv)))^(-1))
crop.fe <- feols(y~x:I(1-G_tv)+x:G_tv+w1+w2 | Dyad + Month, omit_dt)
r.omit3 <- summary(crop.fe)

### omit 4
omit_dt <- crop_dt[(Market1!=market[4] & Market2!=market[4])]

pstr_coef <- my_pstr(omit_dt,w=c("w1","w2","Month_01","Month_02","Month_03","Month_04","Month_05","Month_06","Month_07","Month_08","Month_09","Month_10","Month_11"),tv=tvr,g_start=.5,c_start=.5,tf=1,max_it=100)
omit_dt$G_tv <- ((1+exp(-pstr_coef["g1"]*(omit_dt$tv-pstr_coef["c1"])/sd(omit_dt$tv)))^(-1))
crop.fe <- feols(y~x:I(1-G_tv)+x:G_tv+w1+w2 | Dyad + Month, omit_dt)
r.omit4 <- summary(crop.fe)

### omit 5
omit_dt <- crop_dt[(Market1!=market[5] & Market2!=market[5])]

pstr_coef <- my_pstr(omit_dt,w=c("w1","w2","Month_01","Month_02","Month_03","Month_04","Month_05","Month_06","Month_07","Month_08","Month_09","Month_10","Month_11"),tv=tvr,g_start=.5,c_start=.5,tf=1,max_it=100)
omit_dt$G_tv <- ((1+exp(-pstr_coef["g1"]*(omit_dt$tv-pstr_coef["c1"])/sd(omit_dt$tv)))^(-1))
crop.fe <- feols(y~x:I(1-G_tv)+x:G_tv+w1+w2 | Dyad + Month, omit_dt)
r.omit5 <- summary(crop.fe)

### omit 6
omit_dt <- crop_dt[(Market1!=market[6] & Market2!=market[6])]

pstr_coef <- my_pstr(omit_dt,w=c("w1","w2","Month_01","Month_02","Month_03","Month_04","Month_05","Month_06","Month_07","Month_08","Month_09","Month_10","Month_11"),tv=tvr,g_start=.5,c_start=.5,tf=1,max_it=100)
omit_dt$G_tv <- ((1+exp(-pstr_coef["g1"]*(omit_dt$tv-pstr_coef["c1"])/sd(omit_dt$tv)))^(-1))
crop.fe <- feols(y~x:I(1-G_tv)+x:G_tv+w1+w2 | Dyad + Month, omit_dt)
r.omit6 <- summary(crop.fe)

### omit 7
omit_dt <- crop_dt[(Market1!=market[7] & Market2!=market[7])]

pstr_coef <- my_pstr(omit_dt,w=c("w1","w2","Month_01","Month_02","Month_03","Month_04","Month_05","Month_06","Month_07","Month_08","Month_09","Month_10","Month_11"),tv=tvr,g_start=.5,c_start=.5,tf=1,max_it=100)
omit_dt$G_tv <- ((1+exp(-pstr_coef["g1"]*(omit_dt$tv-pstr_coef["c1"])/sd(omit_dt$tv)))^(-1))
crop.fe <- feols(y~x:I(1-G_tv)+x:G_tv+w1+w2 | Dyad + Month, omit_dt)
r.omit7 <- summary(crop.fe)

### omit 8
omit_dt <- crop_dt[(Market1!=market[8] & Market2!=market[8])]

pstr_coef <- my_pstr(omit_dt,w=c("w1","w2","Month_01","Month_02","Month_03","Month_04","Month_05","Month_06","Month_07","Month_08","Month_09","Month_10","Month_11"),tv=tvr,g_start=.5,c_start=.5,tf=1,max_it=100)
omit_dt$G_tv <- ((1+exp(-pstr_coef["g1"]*(omit_dt$tv-pstr_coef["c1"])/sd(omit_dt$tv)))^(-1))
crop.fe <- feols(y~x:I(1-G_tv)+x:G_tv+w1+w2 | Dyad + Month, omit_dt)
r.omit8 <- summary(crop.fe)

### omit 9
omit_dt <- crop_dt[(Market1!=market[9] & Market2!=market[9])]

pstr_coef <- my_pstr(omit_dt,w=c("w1","w2","Month_01","Month_02","Month_03","Month_04","Month_05","Month_06","Month_07","Month_08","Month_09","Month_10","Month_11"),tv=tvr,g_start=.5,c_start=.5,tf=1,max_it=100)
omit_dt$G_tv <- ((1+exp(-pstr_coef["g1"]*(omit_dt$tv-pstr_coef["c1"])/sd(omit_dt$tv)))^(-1))
crop.fe <- feols(y~x:I(1-G_tv)+x:G_tv+w1+w2 | Dyad + Month, omit_dt)
r.omit9 <- summary(crop.fe)

### omit 10
omit_dt <- crop_dt[(Market1!=market[10] & Market2!=market[10])]

pstr_coef <- my_pstr(omit_dt,w=c("w1","w2","Month_01","Month_02","Month_03","Month_04","Month_05","Month_06","Month_07","Month_08","Month_09","Month_10","Month_11"),tv=tvr,g_start=.5,c_start=.5,tf=1,max_it=100)
omit_dt$G_tv <- ((1+exp(-pstr_coef["g1"]*(omit_dt$tv-pstr_coef["c1"])/sd(omit_dt$tv)))^(-1))
crop.fe <- feols(y~x:I(1-G_tv)+x:G_tv+w1+w2 | Dyad + Month, omit_dt)
r.omit10 <- summary(crop.fe)



coef_names <- names(r.rdm$coefficients)[3:4]

panel_list <- list(r.rdm,r.rtm,r.regm,r.distm,r.timem,r.reg,r.dist,r.time,r.omit1,r.omit2,r.omit3,r.omit4,r.omit5,r.omit6,r.omit7,r.omit8,r.omit9,r.omit10)

coef_vec <- c(1:length(coef_names))

coef_list <- list()
se_list <- list()

for(s in 1:length(coef_vec)){
  
  coef_num <- coef_vec[s]
  
  coef_mat <- matrix(nrow=length(panel_list),ncol=1)
  se_mat <- matrix(nrow=length(panel_list),ncol=1)
  for(i in 1:length(panel_list)){
    coef_mat[i,1] <- panel_list[[i]]$coeftable[coef_names,][coef_num,1]
    se_mat[i,1] <- panel_list[[i]]$coeftable[coef_names,][coef_num,2]
  }
  
  colnames(coef_mat) <- paste(c("coef"),s,sep="")
  colnames(se_mat) <- paste(c("se"),s,sep="")
  coef_mat <- as.data.table(coef_mat)
  se_mat <- as.data.table(se_mat)
  
  coef_list[[s]] <- coef_mat
  se_list[[s]] <- se_mat
}

coef_mat <- Reduce(cbind,coef_list)
se_mat <- Reduce(cbind,se_list)

coef_mat$pos <- rownames(coef_mat)
se_mat$pos <- rownames(se_mat)

colnames(coef_mat) <- c("Low Conflict Intensity","High Conflict Intensity","pos")
colnames(se_mat) <- c("Low Conflict Intensity","High Conflict Intensity","pos")

coef_long <- melt(coef_mat,id.vars="pos")
coef_long$pos <- factor(coef_long$pos,levels=unique(coef_long$pos))
colnames(coef_long) <- c("pos","var","coef")

se_long <- melt(se_mat,id.vars="pos")
se_long$pos <- factor(se_long$pos,levels=unique(se_long$pos))
colnames(se_long) <- c("pos","var","se")

se_long$pos <- NULL
se_long$var <- NULL

dt_long <- cbind(coef_long,se_long)


## control identifiers
controls <- c("Dyad","Month","Same region","Distance (km)","Travel time (hr)",paste(market[1],sep=""),paste(market[2],sep=""),paste(market[3],sep=""),paste(market[4],sep=""),paste(market[5],sep=""),paste(market[6],sep=""),paste(market[7],sep=""),paste(market[8],sep=""),paste(market[9],sep=""),paste(market[10],sep=""))
control_mat <- matrix(nrow=length(panel_list),ncol=length(controls))
control_mat[1,c(1,2,3,4)] <- 1
control_mat[2,c(1,2,3,5)] <- 1
control_mat[3,c(1,2,3)] <- 1
control_mat[4,c(1,2,4)] <- 1
control_mat[5,c(1,2,5)] <- 1
control_mat[6,c(1,3)] <- 1
control_mat[7,c(1,4)] <- 1
control_mat[8,c(1,5)] <- 1
control_mat[9,c(1,2,3,4,6)] <- 1
control_mat[10,c(1,2,3,4,7)] <- 1
control_mat[11,c(1,2,3,4,8)] <- 1
control_mat[12,c(1,2,3,4,9)] <- 1
control_mat[13,c(1,2,3,4,10)] <- 1
control_mat[14,c(1,2,3,4,11)] <- 1
control_mat[15,c(1,2,3,4,12)] <- 1
control_mat[16,c(1,2,3,4,13)] <- 1
control_mat[17,c(1,2,3,4,14)] <- 1
control_mat[18,c(1,2,3,4,15)] <- 1

colnames(control_mat) <- controls
control_mat <- as.data.table(control_mat)

control_mat$pos <- rownames(control_mat)
control_mat$pos <- factor(control_mat$pos,levels=unique(control_mat$pos))

cfixed <- c(controls[1:5],"pos")
cmarkt <- c(controls[6:length(controls)],"pos")

control_fixed <- control_mat[,..cfixed]
control_markt <- control_mat[,..cmarkt]

fixed_long <- melt(control_fixed,id.vars="pos")
fixed_long$pos <- factor(fixed_long$pos,levels=unique(fixed_long$pos))
colnames(fixed_long) <- c("pos","var","fe")

markt_long <- melt(control_markt,id.vars="pos")
markt_long$pos <- factor(markt_long$pos,levels=unique(markt_long$pos))
colnames(markt_long) <- c("pos","var","fe")


# plots
gg1 <- ggplot(dt_long,aes(x=pos,color=var,shape=var)) + 
  geom_hline(yintercept=0,size=1,linetype=3,color="gray50",alpha=.8)+
  geom_errorbar(aes(ymin=coef-1.96*se,ymax=coef+1.96*se,group=var),position=position_dodge(.5),size=1,width=NA,alpha=.5) +
  geom_point(aes(y=coef),position=position_dodge(.5),size=3)+
  labs(x="Specification",y="")+
  scale_color_manual(values=c("steelblue","indianred"))+
  scale_shape_manual(values=c(16,18))+
  theme_classic()+
  theme(legend.position="top", legend.title = element_blank(), axis.line.x=element_blank(), axis.text.y = element_text(size=8),axis.ticks.x=element_blank())

gc1 <- ggplot(fixed_long,aes(x=pos,y=var)) + 
  geom_tile(na.rm=T,color=NA,fill=NA) + 
  ylim(rev(levels(fixed_long$var))) + 
  geom_point(aes(size=fe),na.rm=T,shape=15,color="gray50") +
  labs(title="Fixed effects and interaction terms",x="",y="") +
  theme_classic() +
  theme(legend.position="none",title=element_text(size=8),axis.line=element_blank(), axis.text.y.left = element_text(hjust=0,size=8), axis.ticks=element_blank(),axis.text.x=element_blank())

gc2 <- ggplot(markt_long,aes(x=pos,y=var)) + 
  geom_tile(na.rm=T,color=NA,fill=NA) + 
  ylim(rev(levels(markt_long$var))) + 
  geom_point(aes(size=fe),na.rm=T,shape=15,color="gray50") +
  labs(title="Excluded market",x="",y="") +
  theme_classic() +
  theme(legend.position="none",title=element_text(size=8),axis.line=element_blank(), axis.text.y.left = element_text(hjust=0,size=8), axis.ticks=element_blank(),axis.text.x=element_blank())

gg_comb <- plot_grid(gg1,gc1,gc2,align="v",ncol=1,rel_heights=c(12,6,10))

gg_comb

# ggsave("sensitivity.png",ggcomb,width=6.5,height=6.5)

ggsave(paste("Figures/Omit/robust_pstr_",tvr,"_",crop,"_b.png",sep=""),gg_comb,width=6.5,height=6.5)


