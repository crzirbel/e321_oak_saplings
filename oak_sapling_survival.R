#Oak Sapling Survival
#Chad Zirbel
#5/14/2019

## Lets get started-----------------------

##To Do
#add continuous woody variable
#clean and combine survival data
#proportion analysis
#cox survival
#fire temp at 50cm
#fire severity
#make a new model


#load packages
library(ggplot2)
library(lme4)
library(car)
library(tidyverse)
library(glmmTMB)

#Load data
setwd("C:/Users/zirbe032/Documents/Bison/e321_oak_saplings") #remove this once added to DB
fire.temp<-read.csv("e321_fire_temp_2019.csv")
site.data<-read.csv("e321_site_data.csv")
oak.survival<-read.csv("oak_seedling_survival_2019.csv")
setwd("C:/Users/zirbe032/Documents/Bison/e321_consumption") #remove this once added to DB
sp_biomass<-read.csv("sp_biomass_data_2018.csv")
sp_cover<-read.csv("e321_sp_cover_2018_final.csv")

## Data cleaning and merging ------------------------------

#merge fire and site data
data<-merge(site.data, fire.temp, by="plot", all=T)
fire.data<-merge(site.data, fire.temp, by="plot", all=T)

## Clean biomass data -----------------------
##Create woody, grass, litter, and forb:grass variables##
biomass.vars<-sp_biomass%>%
  group_by(plot, type)%>%
  summarise(mass=sum(mass))%>%
  spread(type, value=mass, fill=0)

#rename variables
names(biomass.vars)<- c("plot", "forb.biomass", "grass.biomass", "litter.biomass", "woody.biomass")
#forb:grass
biomass.vars$forb.grass.ratio<- biomass.vars$forb.biomass/biomass.vars$grass.biomass

biomass.vars[is.na(biomass.vars)]<-0

#Do the same for the cover data (cover not relative cover)
cover.vars<- sp_cover %>%
  group_by(plot, type) %>%
  summarise(cover=sum(cover)) %>%
  spread(type, value=cover, fill=0)

#rename plot variable levels
cover.vars$plot<- gsub("plot_", "", cover.vars$plot)

#rename variables
names(cover.vars)<- c("plot", "bare.cover","forb.cover", "grass.cover", "litter.cover", "woody.cover")
#forb:grass
cover.vars$forb.grass.ratio.cover<- cover.vars$forb.cover/cover.vars$grass.cover

#merge biomass and cover data frames
biomass.cover.vars<-merge(biomass.vars, cover.vars, by="plot")

#aggregate biomass data
#plot_mass_ag<-aggregate(mass~plot,data=sp_biomass,sum)

#merge with site and fire data
data<-merge(data, biomass.cover.vars, by="plot", all=T)

#format survival data
oak.survival$prim.stem.alive<-ifelse(oak.survival$alive=="y" & 
                                     oak.survival$resprout== "n", 1, 0)

oak.survival$alive.binom<-ifelse(oak.survival$alive== "y", 1, 0)

#merge data
data<-merge(data, oak.survival, by=c("plot", "group"),all=T)

#binary fire variable
data$fire.binary<-ifelse(data$max_temp>0, "Yes", "No")

#drop the forest plots
data.drop<-data[!data$cover.type%in%"forest",]

#drop snapped trees
data.snap.drop<-data[!data$drop%in%"drop",]
data.snap.drop<-data.snap.drop[data.snap.drop$height%in%"0m",]
data.snap.drop<-data.snap.drop[!is.na(data.snap.drop$alive.binom),]

#drop forest plots
data.snap.drop2<-data.snap.drop[!data.snap.drop$cover.type%in%"forest",]
mean(data.snap.drop2[data.snap.drop2$grazed%in%"yes",]$prim.stem.alive)
mean(data.snap.drop2[data.snap.drop2$grazed%in%"no",]$prim.stem.alive)

data.snap.drop3<-data.snap.drop2[data.snap.drop2$height%in%"0m",]
mean(data.snap.drop3[(data.snap.drop3$grazed%in%"yes" & data.snap.drop3$cover.type%in%"grass"),]$max_temp)
mean(data.snap.drop3[(data.snap.drop3$grazed%in%"no" & data.snap.drop3$cover.type%in%"grass"),]$max_temp)

mean(data.snap.drop3[(data.snap.drop3$grazed%in%"yes" & data.snap.drop3$cover.type%in%"shrub"),]$max_temp)
mean(data.snap.drop3[(data.snap.drop3$grazed%in%"no" & data.snap.drop3$cover.type%in%"shrub"),]$max_temp)

nrow(data.snap.drop3[(data.snap.drop3$alive%in%"y" & data.snap.drop3$resprout%in%"y"),])
nrow(data.snap.drop3[(data.snap.drop3$alive%in%"n" & data.snap.drop3$resprout%in%"n"),])
nrow(data.snap.drop3[(data.snap.drop3$alive%in%"y" & data.snap.drop3$resprout%in%"n"),])
nrow(data.snap.drop3[(data.snap.drop3$prim.stem.alive==0),])

##Create fire temp data
fire.data<-fire.data[!(fire.data$cover.type%in%"forest"),]
fire.data<-merge(fire.data, biomass.cover.vars, by="plot")

#drop 50cm plots
fire.data.ground<-fire.data[!(fire.data$height%in%"50m"),]
fire.data.ground$total.biomass<-colSums(fire.data.ground[,15:18])

## Models -------------------------------------------------
##TRY GLMMTMB HERE##
mod.survival.binom.graze<-glmmTMB(prim.stem.alive~grazed*grass.biomass+grazed*woody.cover+
                                  +(1|plot/group),data=data.snap.drop2,family=binomial)
  
  mod.eff<-glmer(prim.stem.alive~grazed*grass.biomass+grazed*woody.cover+
                          +(1|plot/group),data=data.snap.drop2,family=binomial)
summary(mod.survival.binom.graze)
Anova(mod.survival.binom.graze, type=3)

mod.survival.binom.cover<-glmmTMB(alive.binom~cover.type+
                                    +(1|plot),data=data.snap.drop2[data.snap.drop2$grazed%in%"no",],family=binomial)

mod.survival.binom.fire<-glmmTMB(prim.stem.alive~scale(max_temp)
                                    +(1|plot/group),data=data.snap.drop,family=binomial)
summary(mod.survival.binom.fire)
Anova(mod.survival.binom.fire, type=3)

mod.survival.binom<-glmer(alive.binom~scale(max_temp)
                 +(1|plot/group),data=data.snap.drop2,family=binomial)

summary(mod.survival.binom)

mod.survival.binom2<-lmer(alive.binom~fire.binary+(1|plot/group),data=data.snap.drop)

summary(mod.survival.binom2)
Anova(mod.survival.binom2, type=3)

mod.stem.binom<-glmer(prim.stem.alive~scale(max_temp)
                          +(1|plot/group),data=data.snap.drop,family=binomial)
summary(mod.stem.binom)

mod.stem.binom2<-lmer(prim.stem.alive~fire.binary+(1|plot/group),data=data.snap.drop)

summary(mod.stem.binom2)
Anova(mod.stem.binom2, type=3)

#only resprout dead primary stems
data.prim.dead<-data.snap.drop2[data.snap.drop2$prim.stem.alive==0,]
sum(data.prim.dead$alive.binom)
nrow(data.prim.dead)

mod.stem.binom3<-glmer(alive.binom~grazed*cover.type+(1|plot/group),
                       data=data.prim.dead, family=binomial)
summary(mod.stem.binom3)
Anova(mod.stem.binom3)

#fire temp
mod.fire<-lmer(max_temp~grazed*grass.biomass+grazed*woody.cover+(1|plot),
               data=fire.data.ground)
summary(mod.fire)
Anova(mod.fire, type=3)

## Figures ------------------------------------------------
var.label<-c("0m"="0", "50m"="50")
var.label2<-c("yes"="Grazed", "no"="Ungrazed")

cover.labs <- c("Grass dominated plots", "Shrub dominated plots")
names(cover.labs) <- c("grass", "shrub")

data.sum<-data.snap.drop2 %>% 
  group_by(cover.type, grazed) %>% 
  summarise(mean=mean(prim.stem.alive),sd=sd(prim.stem.alive),
            se=(sd(prim.stem.alive)/sqrt(length(prim.stem.alive))),
            ci=(sd(prim.stem.alive)/sqrt(length(prim.stem.alive))) * qt((0.95/2 +0.5), (length(prim.stem.alive)-1)))
data.sum$grazed<-factor(data.sum$grazed, levels= c("yes", "no"))

tiff("survival_graze_cover.tiff",res=300,height=4,width=6, units= "in")
ggplot(data.sum, aes(grazed, mean, color=grazed))+
  geom_point(size=2.5)+
  facet_grid(~cover.type, labeller = labeller(cover.type = cover.labs))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1, width=0.1)+
  labs(x="Bison grazing?", y=bquote('Oak stem survival'))+
  scale_x_discrete(labels=var.label)+
  scale_color_manual(values=c("#e66101","#5e3c99"))+
  theme(text = element_text(size=18),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = "none",
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))
dev.off()

##
data.sum2<-data.snap.drop2 %>% 
  group_by(cover.type, grazed) %>% 
  summarise(mean=mean(alive.binom),sd=sd(alive.binom),
            se=(sd(alive.binom)/sqrt(length(alive.binom))),
            ci=(sd(alive.binom)/sqrt(length(alive.binom))) * qt((0.95/2 +0.5), (length(alive.binom)-1)))
data.sum2$grazed<-factor(data.sum2$grazed, levels= c("yes", "no"))

#tiff("survival_graze_cover.tiff",res=300,height=4,width=6, units= "in")
ggplot(data.sum2, aes(grazed, mean, color=grazed))+
  geom_point(size=2.5)+
  facet_grid(~cover.type, labeller = labeller(cover.type = cover.labs))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1, width=0.1)+
  labs(x="Bison grazing?", y=bquote('Oak stem survival'))+
  scale_x_discrete(labels=var.label)+
  scale_color_manual(values=c("#e66101","#5e3c99"))+
  theme(text = element_text(size=18),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = "none",
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))
#dev.off()

#cover type by grazing
ggplot(data.sum, aes(grazed, mean, color=cover.type))+
  geom_point(size=2.5)+
  facet_grid(~cover.type)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1, width=0.1)+
  labs(x="Bison grazing?", y=bquote('Oak stem survival'))+
  scale_x_discrete(labels=var.label)+
  scale_color_manual(values=c("#DC143C","#00BFFF"))+
  theme(text = element_text(size=18),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = "none",
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

data.sum2<-data.prim.dead %>% 
  group_by(cover.type, grazed) %>% 
  summarise(mean=mean(alive.binom),sd=sd(alive.binom),
            se=(sd(alive.binom)/sqrt(length(alive.binom))),
            ci=(sd(alive.binom)/sqrt(length(alive.binom))) * qt((0.95/2 +0.5), (length(alive.binom)-1)))

#cover type by grazing
ggplot(data.sum2, aes(grazed, mean, color=cover.type))+
  geom_point(size=2.5)+
  facet_grid(~cover.type)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1, width=0.1)+
  labs(x="Bison grazing?", y=bquote('Oak stem resprout'))+
  scale_x_discrete(labels=var.label)+
  scale_color_manual(values=c("#DC143C","#00BFFF"))+
  theme(text = element_text(size=18),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = "none",
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))


#temp boxplot (height by grazing)
ggplot(data.drop, aes(grazed, max_temp, fill=height))+
  geom_boxplot(lwd=0.75)+
  facet_grid(~cover.type)+
  scale_fill_manual(name = "Height (cm)",labels = var.label,values=c("#DC143C","#00BFFF"))+
  labs(x="Bison grazing?", y="Fire temperature (C)")+
  theme(text = element_text(size=20),axis.text=element_text(colour="black"),
        panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line = element_line(size=.7, color="black"))

#temp vs biomass
tiff("biomass_temp.tiff",res=300,height=4,width=6, units= "in")
ggplot(fire.data.ground, aes(grass.biomass, max_temp, color=grazed))+
  geom_point()+
  geom_smooth(method="lm",size=1.5, se=F)+
  #facet_grid(~cover.type, labeller = labeller(cover.type = cover.labs))+
  labs(x=bquote('Grass biomass (g/ '*m^2~')'), y="Fire temperature (C)")+
  scale_color_manual(name = "",labels = var.label2,values=c("#DC143C","#00BFFF"))+
  theme(text = element_text(size=20),axis.text=element_text(colour="black"),
        panel.background=element_blank(),strip.background = element_rect(
          color="white", fill="white", size=1.5, linetype="solid"),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line = element_line(size=.7, color="black"), legend.position="bottom")
dev.off()

ggplot(fire.data.ground, aes(woody.cover, max_temp))+
  geom_point()+
  geom_smooth(method="lm",size=1.5, se=F)+
  #facet_grid(~cover.type, labeller = labeller(cover.type = cover.labs))+
  labs(x="Woody cover (%)", y="Fire temperature (C)")+
  #scale_color_manual(name = "",labels = var.label2,values=c("#DC143C","#00BFFF"))+
  theme(text = element_text(size=20),axis.text=element_text(colour="black"),
        panel.background=element_blank(),strip.background = element_rect(
          color="white", fill="white", size=1.5, linetype="solid"),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line = element_line(size=.7, color="black"), legend.position="bottom")

ggplot(fire.data.ground, aes(grazed, max_temp, fill=grazed))+
  geom_boxplot(lwd=0.75)+
  #facet_grid(~cover.type, labeller = labeller(cover.type = cover.labs))+
  labs(x="Grazed by bison?", y="Fire temperature (C)")+
  scale_fill_manual(name = "",labels = var.label2,values=c("#DC143C","#00BFFF"))+
  theme(text = element_text(size=20),axis.text=element_text(colour="black"),
        panel.background=element_blank(),strip.background = element_rect(
          color="white", fill="white", size=1.5, linetype="solid"),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line = element_line(size=.7, color="black"), legend.position="none")

data.cut<-data[which(data$cover.type%in%"grass" & data$height%in%"0m"),]

tiff("graze_temp.tiff",res=300,height=4,width=6, units= "in")
ggplot(data.cut, aes(grazed, max_temp, fill=grazed))+
  geom_boxplot(lwd=0.75)+
  labs(x="", y="Fire temperature (°C)")+
  scale_x_discrete(labels=var.label2, limits = rev(levels(data.cut$grazed)))+
  scale_fill_manual(name = "",labels = var.label2,values=c("#00BFFF", "#DC143C"))+
  theme(text = element_text(size=20),axis.text=element_text(colour="black"),
        panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line = element_line(size=.7, color="black"), legend.position = "none")
dev.off()

data.temp<-fire.data.ground %>% 
  group_by(grazed, cover.type) %>% 
  summarise(mean=mean(max_temp),sd=sd(max_temp),
            se=(sd(max_temp)/sqrt(length(max_temp))),
            ci=(sd(max_temp)/sqrt(length(max_temp))) * qt((0.95/2 +0.5), (length(max_temp)-1)))

tiff("graze_temp.tiff",res=300,height=4,width=6, units= "in")
ggplot(data.temp, aes(grazed, mean, color=grazed))+
  geom_point(size=2.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1, width=0.1)+
  facet_grid(~cover.type, labeller = labeller(cover.type = cover.labs))+
  labs(x="Grazed by bison?", y="Fire temperature (C)")+
  labs(x="", y="Fire temperature (°C)")+
  scale_x_discrete(labels=var.label2, limits = rev(levels(data.temp$grazed)))+
  scale_color_manual(name = "",labels = var.label2,values=c("#5e3c99","#e66101"))+
  theme(text = element_text(size=20),axis.text=element_text(colour="black"),
        panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line = element_line(size=.7, color="black"), legend.position = "none")
dev.off()


mean(data.cut$max_temp[data.cut$grazed%in%"yes"])
mean(data.cut$max_temp[data.cut$grazed%in%"no"])

#binary
ggplot(data.snap.drop, aes(x=as.factor(fire.binary), y=prim.stem.alive))+
  stat_summary(fun.y="mean", geom="point")+
  labs(x="Fire", y="Survival")+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

#temp by survival
logistic = function(x) exp(x)/(1+exp(x))
mod.coefs <- fixef(mod.survival.binom.graze)

ggplot(data.snap.drop2, aes(grass.biomass, alive.binom, color=grazed))+
  geom_point(position = position_jitter(width = 0.05, height = 0.05))+
  stat_function(fun=function(x)logistic(mod.coefs[1]+mod.coefs[2]*x),size=1.5,color="blue")+
  labs(x="Fire temperature (C)", y="Oak seedling stem survival")+
  theme(text = element_text(size=20),axis.text=element_text(colour="black"),
        panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line = element_line(size=.7, color="black"))

data.snap.drop2<-data.snap.drop2 %>%
  mutate(pred.prob= predict(mod.survival.binom.graze, type="response"))

ggplot(data.snap.drop2, aes(max_temp, alive.binom, color=grazed))+
  geom_point(position = position_jitter(width = 0.05, height = 0.05))+
  geom_smooth(method = "glm", aes(x = max_temp, y= pred.prob), method.args = list(family = "binomial"), se = FALSE)+
  #stat_function(fun=function(x)logistic(mod.coefs[1]+mod.coefs[2]*x),size=1.5,color="blue")+
  #labs(x="Grass cover (%)", y="Oak seedling stem survival")+
  theme(text = element_text(size=20),axis.text=element_text(colour="black"),
        panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line = element_line(size=.7, color="black"))

#####
eff<-effect("scale(max_temp):woody.cover",mod.survival.binom.fire,xlevels=2)
eff_df <- data.frame(eff)

ggplot(eff_df)+
  geom_line(aes(max_temp,fit,linetype=factor(woody.cover)),size=1.25)+
  xlab("Maximum fire temperature")+ylab("Oak seedling survival")+
  scale_linetype_discrete(name="Woody cover (%)",labels = c("Low", "High")) +
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),panel.background=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"),legend.key = element_blank(),legend.position=c(.8, .8))

##
#define  4	bins,	using	a	sequence	of	5	breaks
breaks	= with(data.snap.drop,	seq(min(max_temp),	max(max_temp),	length	= 5))

#make	a	binning	factor	from	x1
cut.x	= cut(data.snap.drop$max_temp,	breaks	= breaks)

#calculate	the	proportion	of	presence	by	bin
means	= with(data.snap.drop,	tapply(prim.stem.alive,	cut.x,	mean))

#function	for	the	standard	error	of	a	binary	variable
binomial.SE	= function(x)	sqrt((mean(x)*(1-mean(x)))/length(x))

#calculate	the	standard	error	for	each	bin
ses	= with(data.snap.drop,	tapply(prim.stem.alive,	cut.x,	binomial.SE))

#plot	the	bin	means
plot(means	~ breaks[1:4],	pch	= 19,	ylim	= c(0,1),	xlim	=
       range(data.snap.drop$max_temp),	xlab	= 'X1',	ylab	= 'Proportion', col	= 'red')

#plot	the	bin	SEs
segments(breaks[1:4],	means+ses,	breaks[1:4],	means-ses)

#plot	the	raw	data
#with(data,	points(proportion~ x1)) #show proportion data
with(data.snap.drop,	points(prim.stem.alive~ max_temp)) #show occurance (0,1) data

#plot curve
logistic = function(x) exp(x)/(1+exp(x))
mod.coefs <- fixef(mod.survival.binom)
curve(logistic(mod.coefs[1]+mod.coefs[2]*x), add = T, col = 'blue', lwd = 3)

##
nrow(data.snap.drop[which(data.snap.drop$prim.stem.alive==1 & data.snap.drop$fire.binary%in%"No"),])
nrow(data.snap.drop[which(data.snap.drop$prim.stem.alive==1 & data.snap.drop$fire.binary%in%"Yes"),])

nrow(data.snap.drop[which(data.snap.drop$prim.stem.alive==0 & data.snap.drop$fire.binary%in%"No"),])
nrow(data.snap.drop[which(data.snap.drop$prim.stem.alive==0 & data.snap.drop$fire.binary%in%"Yes"),])

#
nrow(data.snap.drop[data.snap.drop$alive.binom==0,])
nrow(data.snap.drop[data.snap.drop$alive.binom==1,])
##
nrow(data.snap.drop[which(data.snap.drop$prim.stem.alive==0 & data.snap.drop$resprout%in%"n"),])
nrow(data.snap.drop[which(data.snap.drop$prim.stem.alive==0 & data.snap.drop$resprout%in%"y"),])

##temp bins
data.snap.drop$temp.bin<-cut(data.snap.drop$max_temp, breaks=c(0,1,100,200,300,400,500,600,700,800),right=F)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df<-data_summary(data.snap.drop, varname = "alive.binom", groupnames = "temp.bin")

ggplot(data.snap.drop, aes(as.factor(temp.bin), alive.binom))+
  stat_summary(fun.y="mean", geom="point")+
  labs(x="Fire temperature (C)", y="Oak seedling stem survival")+
  theme(text = element_text(size=20),axis.text=element_text(colour="black"),
        panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line = element_line(size=.7, color="black"))

ggplot(df, aes(temp.bin, alive.binom))+
  geom_pointrange(aes(ymin=alive.binom-sd, ymax=alive.binom+sd),size=1)+
  labs(x="Fire temperature (C)", y="Oak seedling stem survival")+
  theme(text = element_text(size=20),axis.text=element_text(colour="black"),
        panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line = element_line(size=.7, color="black"))

df2<-data_summary(data.snap.drop, varname = "prim.stem.alive", groupnames = "fire.binary")

ggplot(df2, aes(fire.binary, prim.stem.alive))+
  geom_pointrange(aes(ymin=prim.stem.alive-sd, ymax=prim.stem.alive+sd),size=1)+
  labs(x="Did the plot burn?", y="Primary stem survival")+
  theme(text = element_text(size=20),axis.text=element_text(colour="black"),
        panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line = element_line(size=.7, color="black"))

data.snap.drop$stem.con<-rep(NA, nrow(data.snap.drop))
data.snap.drop$stem.con[data.snap.drop$prim.stem.alive==1]<-"alive"
data.snap.drop$stem.con[data.snap.drop$resprout%in%"y"]<-"resprout"
data.snap.drop$stem.con[data.snap.drop$alive.binom==0]<-"dead"

##
#herbivory-Leaf N interaction
eff.graze<-effect("grazed:grass.biomass",mod.eff,xlevels=2)
eff_df.graze <- data.frame(eff.graze)

#plot
p.herb <-ggplot(eff_df.herb, aes(herbivory,fit,color=factor(NITROGEN.)))+
  geom_line(size=1.25)+
  xlab("Herbivore pressure (% tissue missing)")+ylab(bquote('Seedling count (Indivduals/ 25'~m^2*')'))+
  scale_color_manual(values=c("#2c7bb6","#d7191c"),name="Leaf nitrogen",breaks=waiver(),labels = c("Low", "High"))+
  geom_ribbon(aes(ymin=fit-se,ymax=fit+se,fill=factor(NITROGEN.)),alpha=0.3, colour= NA)+
  scale_fill_manual(values=c("#2c7bb6","#d7191c"),guide=F)+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  annotate("text",label="p=0.02",x=-0.5,y=3.5,size=5)+
  annotate("text", label="b", x=-0.775, y=3.7, size=5)+
  theme(text = element_text(size=15),axis.text=element_text(colour="black"),panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"),
        legend.key = element_blank(),legend.position=c("right"))
