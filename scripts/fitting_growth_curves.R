#####################################################################
### FITTING GROWTH CURVES TO OD600 DATA AND STATISTICAL MODELLING ###
#####################################################################

# Load dependencies

library(tidyverse)
library(growthrates)
library(tibble)
library(emmeans)
library(plyr)

# 1.0 Load and prepare data ####

load(file = "datafiles/plate1tidy.Rdata")
load(file = "datafiles/plate2tidy.Rdata")
load(file = "datafiles/plate3tidy.Rdata")

data <- rbind(plate1tidy,plate2tidy,plate3tidy)

data$OD600 <- as.numeric(data$OD600)

# 1.1 fit easylinear model to all growth curves ####

fits <- all_easylinear(OD600 ~ timepoint | strain + well + plate,
            data = data, h = 20)

plot(fits,log = "y")

grates <- as.data.frame(coef(fits))

grates <- tibble::rownames_to_column(grates, "curveID")

# 1.2 Testing if there is a difference between lysogens in max growth rate ####

grates <- grates %>%
  mutate(strain = str_split_i(curveID,":",1)) %>%
  mutate(well = str_split_i(curveID,":",2)) %>%
  mutate(plate = str_split_i(curveID,":",3))

grates$strain <- as.factor(grates$strain)
grates$well <- as.factor(grates$well)
grates$plate <- as.factor(grates$plate)

# 1.3 Exploratory plotting ####

hist(grates$mumax[grates$strain!="S.aureus"],breaks = 50)
hist(log(grates$mumax[grates$strain!="S.aureus"]),breaks = 50)

hist(grates$mumax,breaks = 50)
hist(log(grates$mumax),breaks = 50)


p <- ggplot(grates, aes(x=strain, y=mumax)) + 
  geom_boxplot() +
  ylim(0,0.1)
p

# 1.4 Averaging across technical replicates ####

grates$logmumax <- log(grates$mumax)

grates$curveID <- NULL

grates <- grates %>%
  mutate(curve = paste(strain,plate,sep="_"))

grates$curve <- as.factor(grates$curve)

grates_means <- grates %>%
  select(curve,strain) %>%
  unique()

means <- ddply(grates, .(curve), summarize, logmumax=mean(logmumax),mumax=mean(mumax),lag=mean(lag),y0=mean(y0),y0_lm=mean(y0_lm))

grates_means <- left_join(grates_means,means)

p2 <- ggplot(grates_means, aes(x=strain, y=mumax)) + 
  geom_boxplot() +
  ylim(0,0.1) +
  theme_bw()
p2


# 1.5 Testing if there is a difference between lysogens in max growth rate ####

m1 <- glm(logmumax~strain,data = grates_means)
m2 <- glm(logmumax~1,data = grates_means)

anova(m1,m2,test="Chisq")

summary(m1)

plot(m1)

emm_m1 <- emmeans(m1, specs = pairwise ~ strain)
emm_m1$contrasts

contra <- as.data.frame(emm_m1$contrasts)

contra <- contra %>%
  filter(p.value < 0.1)


contra_cor <- contra %>%
  mutate(cor.p = p.value*91) %>%
  filter(cor.p < 0.05)



# 1.6 removing staph from the model ####

gratesnostaf <- grates_means %>%
  filter(strain!="S.aureus")

m1 <- glm(logmumax~strain,data = gratesnostaf )
m2 <- glm(logmumax~1,data = gratesnostaf )

anova(m1,m2,test="Chisq")

summary(m1)

plot(m1)

emm_m1 <- emmeans(m1, specs = pairwise ~ strain)
emm_m1$contrasts

contra <- as.data.frame(emm_m1$contrasts)

contra <- contra %>%
  filter(p.value < 0.05)
