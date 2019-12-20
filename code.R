

# Preparation -------------------------------------------------------------

# Set working directory, load packages and files

setwd("~/Git/2019_spbsampling")

library(tidyverse)
library(lingtypology)

## WALS data

wals_lang <- read_tsv("data/walscodes.tsv") # tsv file was downloaded at wals.info
colnames(wals_lang)[1] <- "id"

sample100 <- wals_lang[(wals_lang$`sample 100` == TRUE),]
sample200 <- wals_lang[(wals_lang$`sample 200` == TRUE),]

wals_ev <- wals.feature(c("78a", "77a"), na.rm = FALSE)
colnames(wals_ev)[1] <- "id"

## My data

lang <- read_tsv("data/languages.csv") # languages, affiliation and glottocodes
col <- read_tsv("data/colors.csv") # color schemes
glot <- read_tsv("data/glottolog_points.csv") # language coordinates from Glottolog

sys <- read_tsv("data/system.csv") # EC evidential systems per language
sys <- merge(sys, lang, by = "lang") 
sys <- merge(sys, glot, by = "lang")

part <- read_tsv("data/particles.csv") # particles with evidential(ish) meanings

vill <- read_tsv("data/villages.csv") # villages, coordinates and languages
vill_nona <- vill[complete.cases(vill$lat),] # remove villages without coordinates
vill_gltc <- merge(vill_nona, lang, by = "lang")
vill_col <- merge(vill_gltc, col, by = "lang")



# WALS map 78A Coding of Evidentiality ------------------------------------

# Reproduce WALS map 78A "Coding of Evidentiality"

map.feature(lang.gltc(wals_ev$glottocode),
            latitude = wals_ev$latitude,
            longitude = wals_ev$longitude,
            features = wals_ev$`78a`,
            title = "Coding of Evidentiality",
            rectangle.lng = c(30, 60), # draw a rectangle around the Caucasus
            rectangle.lat = c(30, 50),
            rectangle.color = "forestgreen", 
            color = c("snow3", "yellow", "white", "red3", "blue", "orchid2"),
            tile = c("Esri.WorldGrayCanvas"))



# All villages by affiliation ---------------------------------------------

# Draw a map with all villages of the eastern Caucasus, and color them by branch (non-East Caucasian languages are white)

map.feature(lang.gltc(vill_col$gltc),
            latitude = vill_col$lat,
            longitude = vill_col$lon,
            features = vill_col$group.x,
            title = "Group",
            color = vill_col$groupcol,
            width = 4,
            minimap = T, # add a minimap for perspective
            minimap.position = "bottomleft", 
            tile = c("Esri.WorldGrayCanvas"))



# Sample proportions ------------------------------------------------------

# Merge the WALS data on evidentiality with other metadata from WALS

wals_ev_area <- merge(wals_ev, wals_lang, by = "id")
wals_ev_area$sample <- "evidentiality maps" # add sample name

sample100$sample <- "100 sample" # add sample name

# select the necessary variables from the evidential and the core 100 dataframe

ev <- wals_ev_area %>%
  select(sample, macroarea)

hundred <- sample100 %>%
  select(sample, macroarea)

# stick them together

proportions <- rbind(ev, hundred)

# draw a stacked barchart showing the proportions of each macroarea in the respective samples

ggplot(proportions, aes(x = sample, fill = macroarea)) + 
  geom_bar(position = "fill")+
  scale_fill_brewer(palette="RdPu")+
  theme_classic()



# WALS maps Caucasus ------------------------------------------------------

wals <- sys[complete.cases(sys$coding),] # filter languages for which WALS has data

map.feature(lang.gltc(wals$gltc),
            latitude = wals$lat,
            longitude = wals$lon,
            features = wals$coding,
            color = wals$col,
            title = "Coding of Evidentiality",
            width = 8,
            zoom.level = 6,
            label = wals$lang,
            popup = wals$ref)


# plot the WALS sample with corrections

map.feature(lang.gltc(wals$gltc),
            latitude = wals$lat,
            longitude = wals$lon,
            features = wals$corr,
            color = wals$corr_col,
            title = "Coding of Evidentiality",
            width = 8,
            zoom.level = 6,
            label = wals$lang,
            popup = wals$ref)

# Maps from my data -------------------------------------------------------

# Systems of evidential coding a la WALS

map.feature(lang.gltc(sys$gltc),
            latitude = sys$lat,
            longitude = sys$lon,
            features = sys$corr,
            title = "Coding of Evidentiality",
            color = sys$corr_col,
            width = 8)

# Tense vs. particles


## Create a column for y/n evidentiality as part of tense

sys <- sys %>%
  mutate(tense = case_when(
    corr == "Part of the tense system" ~ "yes", 
    corr == "Mixed" ~"yes",
    TRUE ~ "no"))

## Filter out only evidential particles

partev <- part %>%
  filter(complete.cases(evidential))

## Re-order the elements in the legends

partev$type <- factor(partev$type, levels = c("verb particle", "free particle", "verb form", "no particle"))
sys$tense <- factor(sys$tense, levels = c("yes", "no"))

map.feature(lang.gltc(sys$gltc),
            features = sys$tense,
            legend.position = "bottomright",
            title = "Part of tense",
            color = c("grey", "white"),
            width = 9)%>%
  map.feature(partev$lang, # add a map on top of the other map
              features = partev$type,
              shape = c("●", "◍", "○", "◌"),
              shape.size = 14,
              title = "Evidential clitic type",
              legend.position = "topright",
              tile = c("Esri.WorldGrayCanvas"),
              pipe.data = .)


## Perfect instead of tense

sys$perfect <- factor(sys$perfect, levels = c("yes", "no"))

map.feature(lang.gltc(sys$gltc),
            features = sys$perfect,
            legend.position = "bottomright",
            title = "Evidential perfect",
            color = c("grey", "white"),
            width = 9)%>%
  map.feature(partev$lang,
              features = partev$type,
              shape = c("●", "◍", "○", "◌"),
              shape.size = 14,
              title = "Evidential clitic type",
              legend.position = "topright",
              tile = c("Esri.WorldGrayCanvas"),
              pipe.data = .)


# My data vs. villages ----------------------------------------------------

# Now plot the WALS style map on top of the villages map

vill_col$lang <- factor(vill_col$lang, levels =c(
  "Dargwa", "Lak", "Bats", "Ingush", "Chechen", "Khinalug", "Archi", "Tsakhur", "Rutul", "Kryz", "Budukh", "Udi", "Lezgian", "Agul", "Tabasaran", "Avar", "Andi", "Botlikh", "Godoberi", "Chamalal", "Bagvalal", "Tindi", "Karata", "Akhvakh", "Tsez", "Hinuq", "Bezhta", "Hunzib", "Khwarshi", "Armenian", "Kumyk", "Nogai", "Azerbaijani"))

map.feature(lang.gltc(vill_col$gltc),
            latitude = vill_col$lat,
            longitude = vill_col$lon,
            features = vill_col$lang,
            title = "Language",
            width = 3,
            color = vill_col$gencol) %>%
  map.feature(lang.gltc(sys$gltc),
              features = sys$corr,
              color = sys$corr_col,
              width = 5,
              legend.position = "topleft",
              title = "Coding of Evidentiality",
              tile = c("Esri.WorldGrayCanvas"),
              pipe.data = .)
