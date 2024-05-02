library(tidyverse)

# plate1 #####

plate1 <- read.csv("datafiles/plate1.csv")

colnames(plate1) <- NULL

plate1 <- as.data.frame(as.matrix(plate1)) # this sets the names to V1 etc rather than them displaying V1 but having a null value

plate1tidy <- plate1 %>%
  pivot_longer(3:147)

plate1tidy$name <- sub('.', '', plate1tidy$name)

plate1tidy$name <- as.integer(plate1tidy$name)
plate1tidy$name <- plate1tidy$name-2

colnames(plate1tidy)[1]<- "well"
colnames(plate1tidy)[2]<- "strain"
colnames(plate1tidy)[3]<- "timepoint"
colnames(plate1tidy)[4]<- "OD600"

plate1tidy$strain <- gsub('\\s+', '', plate1tidy$strain)

plate1tidy <- plate1tidy %>% 
  filter(strain !="LBblank") %>%
  mutate(plate=1) %>%
  mutate(curve = paste(well,strain,plate,sep = "_"))


plate1tidy$curve <- as.factor(plate1tidy$curve)

save(plate1tidy,file = "datafiles/plate1tidy.Rdata")

# plate2 #####

plate2 <- read.csv("datafiles/plate2.csv")

colnames(plate2) <- NULL

plate2 <- as.data.frame(as.matrix(plate2)) # this sets the names to V1 etc rather than them displaying V1 but having a null value

plate2tidy <- plate2 %>%
  pivot_longer(3:147)

plate2tidy$name <- sub('.', '', plate2tidy$name)

plate2tidy$name <- as.integer(plate2tidy$name)
plate2tidy$name <- plate2tidy$name-2

colnames(plate2tidy)[1]<- "well"
colnames(plate2tidy)[2]<- "strain"
colnames(plate2tidy)[3]<- "timepoint"
colnames(plate2tidy)[4]<- "OD600"

plate2tidy$strain <- gsub('\\s+', '', plate2tidy$strain)

plate2tidy <- plate2tidy %>% 
  filter(strain !="LBblank") %>%
  mutate(plate=2) %>%
  mutate(curve = paste(well,strain,plate,sep = "_"))


plate2tidy$curve <- as.factor(plate2tidy$curve)

save(plate2tidy,file = "datafiles/plate2tidy.Rdata")

# plate3 #####

plate3 <- read.csv("datafiles/plate3.csv")

colnames(plate3) <- NULL

plate3 <- as.data.frame(as.matrix(plate3)) # this sets the names to V1 etc rather than them displaying V1 but having a null value

plate3tidy <- plate3 %>%
  pivot_longer(3:147)

plate3tidy$name <- sub('.', '', plate3tidy$name)

plate3tidy$name <- as.integer(plate3tidy$name)
plate3tidy$name <- plate3tidy$name-2

colnames(plate3tidy)[1]<- "well"
colnames(plate3tidy)[2]<- "strain"
colnames(plate3tidy)[3]<- "timepoint"
colnames(plate3tidy)[4]<- "OD600"

plate3tidy$strain <- gsub('\\s+', '', plate3tidy$strain)

plate3tidy <- plate3tidy %>% 
  filter(strain !="LBblank") %>%
  mutate(plate=3) %>%
  mutate(curve = paste(well,strain,plate,sep = "_"))


plate3tidy$curve <- as.factor(plate3tidy$curve)

save(plate3tidy,file = "datafiles/plate3tidy.Rdata")


