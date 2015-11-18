# 2015-11-7 Written by The Superpowerful Cristy and Kaitlin! 
# Complementarity and Selection effect in Biocon in 2014

# Loading required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(nlme)

# Reading in data megafile 
data <- read.csv("BioCON Master Harvest_150625_USE.csv")

# Subset the variables we want
colnames(data[,c(1:11,16:17,23:39)])
w.data <- data[,c(1:11,16:17,23:39)]

# We need to take out H20 and Temp Treatments: CPR_KK_Nov.7.2015
#w.data.water <- subset(w.data, w.data$Water.trt=="0")
#w.data.temp <- subset(w.data.water, w.data.water$Temp.trt=="0")
#w.data.year <- subset(w.data.temp,w.data.temp$year==2014)

w.data.year <- subset(w.data, w.data$Season=="August" & w.data$Water.trt=="0" & w.data$Temp.trt=="0" & w.data$CountOfSpecies!="0" & w.data$year==2004)
w.data.year <- w.data.year[,c(1:7,12:30)]
colnames(w.data.year)

#####################
## To Calculate Yi ##
#####################

# Yi = observed biomass of ith species in a given plot

# Convert to long data.frame using dyplr and tidyr see ?gather
# Yi is a new column with the g/m2 for each species and each plot
l.data.z.year  <- w.data.year  %>% gather(Species, Yi, Achillea.millefolium..g.m.2.:Sorghastrum.nutans..g.m.2.)

#####################
## To Calculate Mi ##
#####################

# Mi = average monoculture biomass for ith species
# When CountofSpecies = 1, but we need to group by treatments (CO2, N)

# Adding a column with the combined CO2 and N Treatments
l.data.z.year$Treat <- paste(l.data.z.year$CO2.Treatment, l.data.z.year$Nitrogen.Treatment, sep=".")
# Make a data frame with the Mi values
Mi.year <- l.data.z.year %>% group_by(Species,Treat) %>% filter(CountOfSpecies=="1"& Yi !="NA") %>% summarise(Mi = mean(Yi))
# Create a column for Mi
l.data.z.year$Trt.Mono.Biomass<-Mi.year$Mi[match(paste(l.data.z.year$Species, l.data.z.year$Treat), paste(Mi.year$Species, Mi.year$Treat))]
# Add in anemone data from a few years back 
l.data.z.year$Trt.Mono.Biomass[l.data.z.year$Species=="Anemone.cylindrica..g.m.2."]<-5.9
# Will still give species a Monoculture average if there are not in the plot
l.data.z.year$Trt.Mono.Biomass[is.na(l.data.z.year$Yi)]<-NA

##########################
## To Calculate YidivMi ##
##########################

l.data.z.year$YidivMi <- l.data.z.year$Yi/l.data.z.year$Trt.Mono.Biomass

#calculate 1/D or the inverse of species
l.data.z.year$d.t <- 1/l.data.z.year$CountOfSpecies


# dRY = Yi/Mi - 1/D  ,"observed - expected" 
l.data.z.year$RY <- l.data.z.year$YidivMi-l.data.z.year$d.t
#bring Mi back into original data.frame. should have used match

##########################
## Questionable         ##
##########################


remove(Mi.year)


# Remove all Mis if species has no biomass in that plot


#### Hand Check later! ####
Rbar.year <- l.data.z.year %>% group_by(Plot) %>% filter(Yi != "NA") %>% summarise(RYbar = mean(RY))
Rbar.year <- as.data.frame(Rbar.year)
#### Hand Check later! ####


# We think there should be different Mbar's for each treatment and SR level
# 
# Calculate Mbar

#### Hand Check later! ####
Mbar.year <- l.data.z.year %>% group_by(Plot) %>% filter(Yi != "NA") %>% summarise(Mbar = mean(Trt.Mono.Biomass))
Mbar.year <- as.data.frame(Mbar.year)
#### Hand Check later! ####

#Match RBar values per plot back to longer data frame with multiple values for plot
l.data.z.year$RYbar <- Rbar.year$RYbar[match(l.data.z.year$Plot,Rbar.year$Plot)]
l.data.z.year$Mbar.year <- Mbar.year$Mbar[match(l.data.z.year$Plot,Mbar.year$Plot)]
remove(Rbar.year)
remove(Mbar.year)


#calculate complementarity effect Mbar * SR * RYBar
# This should be fine! :)
l.data.z.year$Comp <- l.data.z.year$Mbar*l.data.z.year$CountOfSpecies*l.data.z.year$RYbar

#create a subset of only the variables we want
tmp.year <- subset(l.data.z.year, select=c("Ring","Plot","Treat","CountOfSpecies","Comp", "Nitrogen.Treatment", "CO2.Treatment"))
#reduce down to only plot values
final.year <- tmp.year %>% group_by(Ring, Plot, Treat, CountOfSpecies, Nitrogen.Treatment, CO2.Treatment) %>% summarise(Comp = mean(Comp))
remove(tmp.year)
#final.year <- final.year %>% replace(is.na(.), 0)# remove NAs from control plots to make them zero
# we took this out because it was reducing the average complementarity effect for all the plots : CPR_KK_Nov.7.2015



######
######
# Checking Stuff
count.YidivMi <- length(subset(l.data.z.year,l.data.z.year$YidivMi=="NA"))
count.RY <- length(subset(l.data.z.year,l.data.z.year$RY=="NA"))
count.RYbar <- length(subset(l.data.z.year,l.data.z.year$RYbar=="NA"))


xyz <- subset(l.data.z.year,l.data.z.year$YidivMi=="NA")
l.data.z.year
xyz
######
######

comp.data <- subset(final.year, final.year$Comp!= "NA" & final.year$Comp!= "NaN" & final.year$CountOfSpecies!="1" & final.year$CountOfSpecies!="0")

#data exploration
p1.year <- ggplot(comp.data, aes(x=CountOfSpecies, y=Comp))+geom_point()+
  theme_bw()+
  theme(axis.text = element_text(size = 20, face = "bold", color="Black"),
        axis.title = element_text(size = 17, face = "bold", color="Black"))+
  xlab("Species Richness")+ylab("Complementarity Effect")+ggtitle("Biocon Comp effect 2014")+
  scale_color_discrete(label = "Fitted Mean", name="")
p1.year

#with C02
p2.year <- ggplot(comp.data, aes(x=CountOfSpecies, y=Comp, col=Treat))+geom_point()+geom_smooth(method="lm")+
  theme_bw()+
  theme(axis.text = element_text(size = 20, face = "bold", color="Black"),
        axis.title = element_text(size = 17, face = "bold", color="Black"))+
  xlab("Species Richness")+ylab("Complementarity Effect")+ggtitle("Biocon Comp effect 2014")
p2.year
##########################
mod1.year <- lm(Comp~CountOfSpecies*Treat, data=comp.data)
anova(mod1.year)
par(mfrow=c(2,2))
plot(mod1.year)
#
m.year <- summary(mod1.year) #extract out model summary
coef.year <- as.data.frame(m.year$coefficients) # make a data frame of coefficients
#
par(mfrow=c(1,1))
interaction.plot(comp.data$CountOfSpecies, comp.data$Treat, comp.data$Comp)
interaction.plot(comp.data$Treat, comp.data$CountOfSpecies,  comp.data$Comp)
#
p4.year <- ggplot(comp.data, aes(x=CountOfSpecies, y=Comp))+geom_point()+geom_abline(aes(intercept=coef.year[1,1],slope=coef.year[2,1],colour="Fitted Line"),data=coef.year)+
  theme(axis.text = element_text(size = 20, face = "bold", color="Black"),
        axis.title = element_text(size = 17, face = "bold", color="Black"))+
  annotate("text", x = 2, y = 1500, label = paste("y =",round(coef.year[1,1],3),"+",round(coef.year[2,1],3))) +
  annotate("text", x = 2, y = 1000, label = paste("R^2==",m.year$r.squared), parse=TRUE) +
  xlab("Species Richness")+ylab("Complementarity Effect")+ggtitle("Biocon Comp effect 2014")
p4.year  


################################################
###  Trying new models: : CPR_KK_Nov.9.2015  ###
################################################

#comp.data <- subset(final.year, final.year$Comp!= "NA" & final.year$Comp!= "NaN" & final.year$CountOfSpecies!="1" & final.year$CountOfSpecies!="0")
#comp.data <- subset(comp.data, )
#comp.data <- subset(comp.data, comp.data$CountOfSpecies!="1")
#comp.data <- subset(comp.data, comp.data$CountOfSpecies!="0")
mod2 <- lme(Comp~Nitrogen.Treatment*CO2.Treatment, random = ~1|CO2.Treatment/Ring, data=comp.data)

# DF for CO2 treatment are zero, this might be an artifact of our error structure! : CPR_KK_Nov.9.2015

summary(mod2)

################################################
###    We need to think more about this!     ###
################################################




#interaction plots
#par(mfrow=c(1,1))
#interaction.plot(final.year$CO2.Treatment,final.year$CountOfSpecies, fitted(mod2))
###############
#########
##########
l.data.z.year$Mi.2 <- l.data.z.year$Trt.Mono.Biomass-l.data.z.year$Mbar
l.data.z.year$Ry.2 <- l.data.z.year$RY-l.data.z.year$RYbar
l.data.z.year$var <- l.data.z.year$Mi.2*l.data.z.year$Ry.2 
#
s1 <- ggplot(l.data.z.year, aes(x=CountOfSpecies, y=var, col=Treat))+geom_point()+geom_smooth(method="lm")+
  theme_bw()+
  theme(axis.text = element_text(size = 20, face = "bold", color="Black"),
        axis.title = element_text(size = 17, face = "bold", color="Black"))+
  xlab("Species Richness")+ylab("Selection Effect")+ggtitle("Biocon Comp effect 2014")
s1
######
pdf("graphs.pdf")
p1.year
p2.year
p4.year
s1
dev.off()
