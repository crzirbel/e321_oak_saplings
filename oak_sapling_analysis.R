####Oak tree data analysis####
#11/2/2018
#by: Chad Zirbel

##load packages
library(lme4)
library(car)
library(data.table)
library(ggplot2)
library(ggridges)

##load data
source("oak_sapling_data_clean.r") #load in cleaned oak sapling data
#oak.data is the cleaned file with height and root colar diameter for each sapling

#brun frequency, grazing, vegetation class data for each plot
site.data<-read.csv("e321_site_data.csv")
site.data$plot<-as.factor(site.data$plot)

#calcualte oak growth (dropping leaves for now to avoid duplicating rows)
oak.data<-data.table::dcast(setDT(oak.data), plot+subplot+tree.id+species ~ season+year,
                value.var = c("height", "diameter")) 
#height
oak.data$height_f_sp_18<-oak.data$height_fall_2018-oak.data$height_spring_2018
#diameter
oak.data$diameter_f_sp_18<-oak.data$diameter_fall_2018-oak.data$diameter_spring_2018

#create dataframe
full.data<-merge(oak.data,site.data,by="plot", all.x=T)

#drop instances where trees were not planted   ##already done in cleaning script##
#full.data<-full.data[full.data$tree.planted=="yes",]
#full.data$plot<-as.factor(full.data$plot)

####build models####
#change in height
mod.height<-lmer(height_f_sp_18~fire.frequency*fence_type+fence_type*cover.type
                 +(1|plot/subplot)+(1|species),data=full.data)
summary(mod.height)
Anova(mod.height,type=3)

#change in diameter
mod.diameter<-lmer(diameter_f_sp_18~fire.frequency*fence_type+fence_type*cover.type
                   +(1|plot/subplot)+(1|species),data=full.data)
summary(mod.diameter)
Anova(mod.diameter,type=3)

####Figures####
##EDA

#histograms
ggplot(full.data, aes(x = height_f_sp_18, y = species,fill=species)) + 
  geom_density_ridges(show.legend = F,rel_min_height=0.01,scale=10,size=1,alpha=0.5) +
  ylab("Species")+ xlab("Change in height")+
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0))+
  geom_vline(xintercept=0,size=1)+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=1, color="black"))

ggplot(full.data, aes(x = diameter_f_sp_18, y = species,fill=species)) + 
  geom_density_ridges(show.legend = F,rel_min_height=0.01,scale=10,size=1,alpha=0.5) +
  ylab("Species")+ xlab("Change in root collar diameter")+
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0))+
  geom_vline(xintercept=0,size=1)+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=1, color="black"))

#boxplots#
ggplot(full.data, aes(x=as.factor(fire.frequency), y=diameter_f_sp_18, fill=fence_type))+
  geom_boxplot()+
  labs(x="Fire frequency", y="Change in root collar diameter (mm)")+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

ggplot(full.data, aes(x=as.factor(fire.frequency), y=height_f_sp_18, fill=fence_type))+
  geom_boxplot()+
  labs(x="Fire frequency", y="Change in height (cm)")+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))


#Calculate variance of the mean for change in height in each plot given the number of trees
df <- full.data[order(full.data$plot),]

pdf(file="height_meanvar_curves.pdf", bg="white", width=10, height=10,pointsize=8)

par(mfrow=c(6,5))

for(i in as.numeric(levels(full.data$plot))){
  tempdf <- df[which(df$plot==i),]
  
  samples <- matrix(nrow=0,ncol=2)
  
  for(j in 1:18){
    
    x <- replicate(100,{
      m <- mean(sample(tempdf$height_f_sp_18, j, replace = FALSE),na.rm=T)
    })
    
    out <- c(j,var(x))
    samples <- rbind(samples, out)
    colnames(samples)<-c("NumSubsample","VarianceOfMeans")
    row.names(samples)<-NULL
    print(j)
    print(var(x))
    
  }

  sampledf <- as.data.frame(samples)
  assign(paste("Tree_height.Plot",i,sep="."),sampledf)
  theplot <-plot(VarianceOfMeans~NumSubsample,sampledf,main=paste("Tree height Plot",i,sep=" "))
  theplot
  assign(paste("plot",i,sep="."),theplot)
  
}
dev.off()

#Calculate variance of the mean for change in diameter in each plot given the number of trees
pdf(file="diameter_meanvar_curves.pdf", bg="white", width=10, height=10,pointsize=8)

par(mfrow=c(6,5))

for(i in as.numeric(levels(full.data$plot))){
  tempdf <- df[which(df$plot==i),]
  
  samples <- matrix(nrow=0,ncol=2)
  
  for(j in 1:20){
    
    x <- replicate(100,{
      m <- mean(sample(tempdf$diameter_f_sp_18, j, replace = FALSE),na.rm=T)
    })
    
    out <- c(j,var(x))
    samples <- rbind(samples, out)
    colnames(samples)<-c("NumSubsample","VarianceOfMeans")
    row.names(samples)<-NULL
    print(j)
    print(var(x))
    
  }
  
  sampledf <- as.data.frame(samples)
  assign(paste("Tree_diameter.Plot",i,sep="."),sampledf)
  theplot <-plot(VarianceOfMeans~NumSubsample,sampledf,main=paste("Tree diameter Plot",i,sep=" "))
  theplot
  assign(paste("plot",i,sep="."),theplot)
  
}
dev.off()
