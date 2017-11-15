# ==================================================================
# STECF_2016_tac_topups.r
# 
# Calculations on tac topups
#
# Martin Pastoors
#
# 05/07/2016 initial coding
# 25/10/2016 updated data included
# 15/11/2017 checked for still functioning with new data
# ==================================================================

# Reset lists
rm(list=ls())

# Libraries
require(readxl)
require(lubridate, quietly=TRUE)     # data handling
require(pander, quietly=TRUE)        # tables
require(reshape2, quietly=TRUE)      # reshaping data; e.g. cast
require(stringr, quietly=TRUE)       # string manipulation
library(tidyverse)

# getwd()
# setwd("C:/DATA/STECF/2016_PLEN03/tor6.1")

# read fdi data (2014-2015)
csv.list <- list.files(path="FDI", recursive=T,pattern='*.csv',full.names=TRUE)
catch.list <- grep(pattern='landings', csv.list, value = TRUE)
effort.list <- grep(pattern='effort', csv.list, value = TRUE)
nvessels.list <- grep(pattern='nvessels', csv.list, value = TRUE)

# read catch
for (i in 1:length(catch.list)){
  t        <- read.csv(catch.list[i])
  names(t) <- c("country","var","regarea","reggear","species","specon",
                "vessellength", "year","value")
  t$annex  <- word(catch.list[i],1)
  t$annex  <- unlist(strsplit(t$annex, split='/', fixed=TRUE))[2]
  t$unit   <- "tonnes"
  t        <- mutate_each(t, funs(as.character), 
                   c(country, var, regarea, reggear, species, specon,
                     vessellength))
  t        <- filter(t, !is.na(value))
  print(i)
  if (i==1) catch <-t else catch<-rbind(catch,t)
}

# read effort
for (i in 1:length(effort.list)){
  t        <- read.csv(effort.list[i])
  if (i==6) {
    t <- select(t, -Measure.Names)
    t$specon <- NA
    t <- select( t, 1, 2, 3, 4, 8, 5, 6, 7)
  }
  # print(names(t))
  names(t) <- c("annex", "country","regarea","reggear","specon",
                "vessellength", "year","value")
  t$var    <- "effort"
  t$unit   <- "kwdays"
  t$species<- NA
  t        <- mutate_each(t, funs(as.character),
                          c(annex, country, regarea, reggear, specon,
                            vessellength))
  t        <- filter(t, !is.na(value))
  print(i)
  if (i==1) effort <-t else effort<-rbind(effort,t)
}

# read.csv(nvessels.list[1])[1:10,]

# read nvessels
for (i in 1:length(nvessels.list)){
  t        <- read.csv(nvessels.list[i])
  names(t) <- c("annex", "country","regarea","reggear","specon",
                "vessellength", "year","value")
  t$var    <- "nvessels"
  t$unit   <- "count"
  t$species<- NA
  t        <- mutate_each(t, funs(as.character),
                          c(annex, country, regarea, reggear, specon,
                            vessellength))
  t        <- filter(t, !is.na(value))
  print(i)
  if (i==1) nvessels <-t else nvessels<-rbind(nvessels,t)
}

# combine dataset
fdi <- rbind(catch,effort,nvessels)

# unique(fdi$regarea)
# unique(fdi$reggear)
# unique(fdi$annex)

# View(filter(fdi, reggear == "3A"))

save(fdi,file="rdata/FDI.RData")

# read MS data (2014-2015)
ms <-
  read_excel(path="MS/STECF 20161025 MS topup data.xlsx", sheet="Data") %>% 
  # read_excel(path="Excel/NWW 20161026 MS topup data fleet coverage.xlsx", sheet="combined") %>% 
  filter(!is.na(Value)) %>% 
  mutate(Value = as.numeric(Value))
save(ms,file="rdata/MS.RData")


# load("rdata/FDI.RData")
# load("rdata/MS.RData")

# Calculate proportions per Fisheries and Variable
a <- 
  ms %>% 
  group_by(Basin, Fisheries, Variable) %>% 
  summarise(TValue = sum(Value)) 

a2 <-
  ms %>% 
  group_by(Basin, Fisheries, Variable, Rule) %>% 
  summarise(Value = sum(Value)) %>% 
  mutate(Score = ifelse(substr(Rule,1,1) %in% c("0","<"),"below","above")) %>% 
  filter(Value > 0, Basin == "NWW") %>%
  left_join(a, by = c("Basin", "Fisheries", "Variable")) %>% 
  mutate(prop = Value / TValue, 
         Country = "ALL")


# Calculate proportions per Fisheries, Country and Variable
t <- 
  ms %>% 
  group_by(Basin, Fisheries, Country, Variable) %>% 
  summarise(TValue = sum(Value)) 

t2 <-
  ms %>% 
  group_by(Basin, Fisheries, Country, Variable, Rule) %>% 
  summarise(Value = sum(Value)) %>% 
  mutate(Score = ifelse(substr(Rule,1,1) %in% c("0","<"),"below","above")) %>% 
  filter(Value > 0, Basin == "NWW") %>%
  left_join(t, by = c("Basin", "Fisheries", "Country", "Variable")) %>% 
  mutate(prop = Value / TValue) %>% 
  rbind(a2) %>% 
  group_by() %>% 
  mutate(Fisheries = str_wrap(Fisheries, width = 12) ) %>% 
  arrange(desc(Score))

# plotting
ggplot(t2, aes(Variable, prop)) +
geom_bar(aes(fill=Score), stat="identity") +
facet_grid(Country ~ Fisheries , scales = "free" , drop = FALSE) +
theme(axis.text.x  = element_text(angle = 90, hjust = 1, vjust=0),
      strip.text.x = element_text())



# read the correspondence tables
# to be done
unique(fdi$reggear)
