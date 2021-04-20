####Oak tree data analysis####
#11/2/2018
#by: Chad Zirbel

##load packages
library(lme4)
library(car)
library(data.table)
library(ggplot2)
library(ggridges)
library(tidyverse)

##load data
setwd("C:/Users/zirbe032/Documents/Bison/e321_oak_saplings") #remove this once added to DB
source("oak_sapling_data_clean.r") #load in cleaned oak sapling data
#oak.data is the cleaned file with height and root colar diameter for each sapling
#oak seedling survival after 2019 spring fires
oak.survival <- read.csv("oak_seedling_survival_2019.csv")

#brun frequency, grazing, vegetation class data for each plot
site.data <- read.csv("e321_site_data.csv")
site.data$plot <- as.factor(site.data$plot)

#calcualte oak growth (dropping leaves for now to avoid duplicating rows)
oak.data <-
  data.table::dcast(
    setDT(oak.data),
    plot + subplot + tree.id + species ~ season + year,
    value.var = c("height", "diameter")
  )
#height
oak.data$height_f_sp_18 <-
  oak.data$height_fall_2018 - oak.data$height_spring_2018
#diameter
oak.data$diameter_f_sp_18 <-
  oak.data$diameter_fall_2018 - oak.data$diameter_spring_2018

#create dataframe
full.data <- merge(oak.data, site.data, by = "plot", all.x = T)

#add survival data
oak.survival <- oak.survival[, c("tree.id", "alive", "resprout")]
full.data <- merge(full.data, oak.survival, by = "tree.id", all.x = T)

#create data frame droping center trees
full.data.cut <- full.data[!grepl("C", full.data$tree.id), ]

##caluclate tree growth from fall '18 to fall '19

#height
#growth in a year from resprout or previous stem
full.data.cut$height_18_19 <-
  ifelse(
    full.data.cut$alive == "y" &
      full.data.cut$resprout == "n",
    (
      full.data.cut$height_fall_2019 - full.data.cut$height_fall_2018
    ),
    ifelse(
      full.data.cut$resprout == "y",
      full.data.cut$height_fall_2019,
      NA
    )
  )
#possible negative growth if resprout
full.data.cut$height_18_19_alt <-
  ifelse(
    full.data.cut$alive == "y",
    (
      full.data.cut$height_fall_2019 - full.data.cut$height_fall_2018
    ),
    NA
  )

#root collar diameter
#growth in a year from resprout or previous stem
full.data.cut$diameter_18_19 <-
  ifelse(
    full.data.cut$alive == "y" &
      full.data.cut$resprout == "n",
    (
      full.data.cut$diameter_fall_2019 - full.data.cut$diameter_fall_2018
    ),
    ifelse(
      full.data.cut$resprout == "y",
      full.data.cut$diameter_fall_2019,
      NA
    )
  )
#possible negative growth if resprout
full.data.cut$diameter_18_19_alt <-
  ifelse(
    full.data.cut$alive == "y",
    (
      full.data.cut$diameter_fall_2019 - full.data.cut$diameter_fall_2018
    ),
    NA
  )

#drop forest plots
full.data.cut <- full.data.cut[!full.data.cut$cover.type %in% "forest", ]

#drop instances where trees were not planted   ##already done in cleaning script##
#full.data<-full.data[full.data$tree.planted=="yes",]
#full.data$plot<-as.factor(full.data$plot)

####build models####
#change in height
mod.height <-
  lmer(
    height_f_sp_18 ~ fire.frequency * fence_type + fence_type * cover.type
    + (1|plot/subplot) + (1|species),data = full.data)
summary(mod.height)
Anova(mod.height, type = 3)

#change in diameter
mod.diameter <-
  lmer(diameter_f_sp_18 ~ fire.frequency * fence_type + fence_type * cover.type
    + (1 | plot / subplot) + (1 |species),
    data = full.data
  )
summary(mod.diameter)
Anova(mod.diameter, type = 3)

##
mod.height.19 <- lmer(height_18_19 ~ grazed * cover.type
                      + (1 |plot / subplot) + (1 | species),
                      data = full.data.cut2)
summary(mod.height.19)
Anova(mod.height.19, type = 3)

mod.height.19.alt <- lmer(height_18_19_alt ~ grazed * cover.type
                          + (1 |plot / subplot) + (1 | species), data = full.data.cut)
summary(mod.height.19.alt)
Anova(mod.height.19.alt, type = 3)

###
mod.diameter.19 <- lmer(diameter_18_19 ~ grazed * cover.type
                        + (1 |plot / subplot) + (1 | species), data = full.data.cut)
summary(mod.diameter.19)
Anova(mod.diameter.19, type = 3)

mod.diameter.19.alt <- lmer(diameter_18_19_alt ~ grazed * cover.type
                            + (1 | plot / subplot) + (1 | species), data = full.data.cut)
summary(mod.diameter.19.alt)
Anova(mod.diameter.19.alt, type = 3)

full.data.cut2<-full.data.cut[full.data.cut$resprout%in%"y",]
full.data.cut3<-full.data.cut[full.data.cut$resprout%in%"n",]

####Figures####
##EDA

#histograms
ggplot(full.data, aes(x = height_f_sp_18, y = species, fill = species)) +
  geom_density_ridges(
    show.legend = F,
    rel_min_height = 0.01,
    scale = 10,
    size = 1,
    alpha = 0.5
  ) +
  ylab("Species") + xlab("Change in height") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  geom_vline(xintercept = 0, size = 1) +
  theme(
    text = element_text(size = 24),
    axis.text = element_text(colour = "black"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 1, color = "black")
  )

ggplot(full.data, aes(x = diameter_f_sp_18, y = species, fill = species)) +
  geom_density_ridges(
    show.legend = F,
    rel_min_height = 0.01,
    scale = 10,
    size = 1,
    alpha = 0.5
  ) +
  ylab("Species") + xlab("Change in root collar diameter") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  geom_vline(xintercept = 0, size = 1) +
  theme(
    text = element_text(size = 24),
    axis.text = element_text(colour = "black"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 1, color = "black")
  )

#boxplots#
ggplot(full.data,
       aes(
         x = as.factor(fire.frequency),
         y = diameter_f_sp_18,
         fill = fence_type
       )) +
  geom_boxplot() +
  labs(x = "Fire frequency", y = "Change in root collar diameter (mm)") +
  theme(
    text = element_text(size = 24),
    axis.text = element_text(colour = "black"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = .7, color = "black")
  )

ggplot(full.data,
       aes(
         x = as.factor(fire.frequency),
         y = height_f_sp_18,
         fill = fence_type
       )) +
  geom_boxplot() +
  labs(x = "Fire frequency", y = "Change in height (cm)") +
  theme(
    text = element_text(size = 24),
    axis.text = element_text(colour = "black"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = .7, color = "black")
  )

##mean+SE plots
data.sum.diam<-full.data.cut2 %>% 
  group_by(grazed, cover.type) %>% 
  summarise(mean=mean(diameter_18_19, na.rm=T),sd=sd(diameter_18_19, na.rm=T),
            se=(sd(diameter_18_19, na.rm=T)/sqrt(length(diameter_18_19))),
            ci=(sd(diameter_18_19, na.rm=T)/sqrt(length(diameter_18_19))) * qt((0.95/2 +0.5), (length(diameter_18_19)-1)))
data.sum.diam$grazed<-factor(data.sum.diam$grazed, levels= c("yes", "no"))

cover.labs <- c("Grass dominated plots", "Shrub dominated plots")
names(cover.labs) <- c("grass", "shrub")

tiff("root_diam_graze.tiff",res=300,height=4,width=6, units= "in")
ggplot(data.sum.diam, aes(grazed, mean, color=grazed))+
  geom_point(size=2.5)+
  facet_grid(~cover.type, labeller = labeller(cover.type = cover.labs))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1, width=0.1)+
  labs(x="Bison grazing?", y=bquote('Root collar diameter (mm)'))+
  scale_color_manual(values=c("#e66101","#5e3c99"))+
  theme(text = element_text(size=18),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),
        legend.position = "none",panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line = element_line(size=.7, color="black"))
dev.off()

#Calculate variance of the mean for change in height
#in each plot given the number of trees
df <- full.data[order(full.data$plot), ]

pdf(
  file = "height_meanvar_curves.pdf",
  bg = "white",
  width = 10,
  height = 10,
  pointsize = 8
)

par(mfrow = c(6, 5))

for (i in as.numeric(levels(full.data$plot))) {
  tempdf <- df[which(df$plot == i), ]
  
  samples <- matrix(nrow = 0, ncol = 2)
  
  for (j in 1:18) {
    x <- replicate(100, {
      m <- mean(sample(tempdf$height_f_sp_18, j, replace = FALSE), na.rm = T)
    })
    
    out <- c(j, var(x))
    samples <- rbind(samples, out)
    colnames(samples) <- c("NumSubsample", "VarianceOfMeans")
    row.names(samples) <- NULL
    print(j)
    print(var(x))
    
  }
  
  sampledf <- as.data.frame(samples)
  assign(paste("Tree_height.Plot", i, sep = "."), sampledf)
  theplot <-
    plot(VarianceOfMeans ~ NumSubsample,
         sampledf,
         main = paste("Tree height Plot", i, sep = " "))
  theplot
  assign(paste("plot", i, sep = "."), theplot)
  
}
dev.off()

#Calculate variance of the mean for change in diameter
#in each plot given the number of trees
pdf(
  file = "diameter_meanvar_curves.pdf",
  bg = "white",
  width = 10,
  height = 10,
  pointsize = 8
)

par(mfrow = c(6, 5))

for (i in as.numeric(levels(full.data$plot))) {
  tempdf <- df[which(df$plot == i), ]
  
  samples <- matrix(nrow = 0, ncol = 2)
  
  for (j in 1:20) {
    x <- replicate(100, {
      m <-
        mean(sample(tempdf$diameter_f_sp_18, j, replace = FALSE),
             na.rm = T)
    })
    
    out <- c(j, var(x))
    samples <- rbind(samples, out)
    colnames(samples) <- c("NumSubsample", "VarianceOfMeans")
    row.names(samples) <- NULL
    print(j)
    print(var(x))
    
  }
  
  sampledf <- as.data.frame(samples)
  assign(paste("Tree_diameter.Plot", i, sep = "."), sampledf)
  theplot <-
    plot(VarianceOfMeans ~ NumSubsample,
         sampledf,
         main = paste("Tree diameter Plot", i, sep = " "))
  theplot
  assign(paste("plot", i, sep = "."), theplot)
  
}
dev.off()
