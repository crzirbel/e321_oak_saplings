#Oak tree data cleaning script
#11/1/2018
#by: Chad Zirbel

#load packages
library(reshape2)

#load data
planting<-read.csv("oak_sp_planting_list.csv")
sapling.data<-read.csv("oak_seedling_data.csv",na.strings = "NA")

#make planting data long
planting.melt<-melt(planting,id.vars="plot")
names(planting.melt)<-c("plot","group","type")

#Just use spring 2018 data to create list of trees
sapling.data.cut<-sapling.data[which(sapling.data$year=="2018" & sapling.data$season=="spring"),]

#cut sapling data to just have id info
sapling.data.cut<-sapling.data.cut[1:4]

#merge data frames
sp.id.saplings<-merge(sapling.data.cut,planting.melt,by=c("plot","group"),all=T)

#Add a type for center trees
sp.id.saplings[sp.id.saplings$group=="C",]$type<-"m.mono" #make them m.mono because they are all Q. macrocarpa

#drop all uplanted plots
sp.id.saplings<-sp.id.saplings[!is.na(sp.id.saplings$tree),]

#create a species column based on the planting type
sp.id.saplings$species<-with(sp.id.saplings,ifelse(
  type=="m.mono","quemac", ifelse(
  type=="e.mono","queeli", ifelse(
  type=="m.mix" & tree!="5","queeli",ifelse(
  type=="m.mix" & tree=="5","quemac",ifelse(
  type=="e.mix" & tree!="5","quemac","queeli"))))))

#drop missing/broken trees
sapling.data<-sapling.data[sapling.data$drop!="drop",]

#rebuild dataframe
oak.data<-merge(sp.id.saplings,sapling.data,by=c("plot","group","tree","subplot"),all.y=T)

#keep only certain columns
oak.data<-oak.data[,c("plot","species","subplot","tree.id","height","diameter","leaves","year","season")]

#change variable formats
oak.data$plot<-as.factor(oak.data$plot)
oak.data$year<-as.factor(oak.data$year)
