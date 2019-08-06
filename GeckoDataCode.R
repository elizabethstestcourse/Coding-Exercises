# Paul data:
# Lines 11 - 77 are with the 2018 raw data so i could make sure i knew what i was doing and could check my math.
# Analysis/plots for both years starts on line 78.

# Libraries:----
library(Rmisc)
library(ggplot2)
library(dplyr)
library(tidyr)

# Read in data:----

raw <- read.csv("Gecko_2018.csv", header = TRUE)
head(raw)

# Reformat data:----

id.info <- raw[,c(1:9)]
head(id.info)

dat.info <- raw[,c(1,10:29)]
head(dat.info)

dat.long <- gather(dat.info, Insect.Genus, Count, 2:21, factor_key = TRUE)
head(dat.long)

# replace NA with 0

dat.long[is.na(dat.long)] <- 0

# Put the data back together:

gecko18 <- full_join(id.info, dat.long, by = "Field..")
head(gecko18)

####

# Calculate the total number of insects per gecko per location:----

gecko18.sum <- aggregate(Count ~ Insect.Genus + Genus + Location, data = gecko18, sum)
head(gecko18.sum)

gecko18.totals <- aggregate(Count ~ Genus + Location, data = gecko18, sum)

gecko18.final <- full_join(gecko18.sum, gecko18.totals, by = c("Genus", "Location"))
head(gecko18.final)

gecko18.final$ProportionalUtilization <- gecko18.final$Count.x/gecko18.final$Count.y
head(gecko18.final)

####

# Plot the 2018 data:----
ggplot(gecko18.final, aes(Insect.Genus, ProportionalUtilization, fill = Genus)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  facet_grid(~Location) +
  theme_bw() +
  scale_fill_manual(values = c("black","white")) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
  theme(panel.grid = element_blank()) +
  ylab("Proportional Utilization") +
  xlab("")


ggplot(gecko18.final, aes(Insect.Genus, ProportionalUtilization, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  facet_grid(~Genus) +
  theme_bw() +
  scale_fill_manual(values = c("black","white")) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
  theme(panel.grid = element_blank()) +
  ylab("Proportional Utilization") +
  xlab("")


####

# All Data:----

geckoAll <- read.csv("Gecko_competition.csv", header = TRUE)
head(geckoAll)

# Reorganize data:----
geckoAll$ID <- c(1:nrow(geckoAll))
head(geckoAll)

id.dat <- geckoAll[,c(24,1:3)]
head(id.dat)

dat.wide <- geckoAll[,c(24,4:23)]
head(dat.wide)

all.long <- gather(dat.wide, Insect.Genus, Count, 2:21, factor_key = TRUE)
head(all.long)


# Put the data back together:

alldata <- full_join(id.dat, all.long, by = "ID")
head(alldata)

####

# Calculate the total number of insects per gecko per location:----

alldata.totals <- aggregate(Count ~ Species + Overlap + Year, data = alldata, sum)

gecko.final <- full_join(alldata, alldata.totals, by = c("Species", "Overlap","Year"))
head(gecko.final)

gecko.final$ProportionalUtilization <- gecko.final$Count.x/gecko.final$Count.y
head(gecko.final)

####

# Plot data:----
ggplot(gecko.final, aes(Insect.Genus, ProportionalUtilization, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  facet_grid(Year~Overlap) +
  theme_bw() +
  scale_fill_manual(values = c("black","white")) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
  theme(panel.grid = element_blank()) +
  ylab("Proportional Utilization") +
  xlab("")


ggplot(gecko.final, aes(Insect.Genus, ProportionalUtilization, fill = Overlap)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  facet_grid(Year~Species) +
  theme_bw() +
  scale_fill_manual(values = c("black","white")) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
  theme(panel.grid = element_blank()) +
  ylab("Proportional Utilization") +
  xlab("")

####

# Plot number of Prey Items:----

prey.sum <- aggregate(Count.x ~ Year + Insect.Genus, data = gecko.final, sum)
head(prey.sum)

ggplot(prey.sum, aes(Insect.Genus, Count.x, fill = as.factor(Year))) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  theme_bw() +
  scale_fill_manual(values = c("black","white"), name = "Year") +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
  theme(panel.grid = element_blank()) +
  ylab("Number of Prey Items") +
  xlab("")
