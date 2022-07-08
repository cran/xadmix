## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.retina = 2,
  fig.width = 480/72, 
  fig.asp = 0.7, 
  dev.args = list(type = "cairo-png")
)

## ----setup--------------------------------------------------------------------
library(xadmix)

## -----------------------------------------------------------------------------
data("xadmixture")
str(xadmixture)

# number of observations per country
table(xadmixture$country)

# number of observations per species
table(xadmixture$species)

## -----------------------------------------------------------------------------
# keep only observations with K1 > 0.15 and K2 > 0.01
subset1 <- admix_subset(xadmixture, 
                        anc = c("K1", "K2"), 
                        pct = c(0.15, 0.01))

# keep only observations with K2 < 0.1 and K3 < 0.1
subset2 <- admix_subset(xadmixture, 
                        anc = c("K2", "K3"), 
                        pct = c(0.1, 0.1), 
                        comparison = "less")

# filtering for countries and species
subset3 <- admix_subset(xadmixture, 
                        country = c("GBR", "FRA"), 
                        species = c("lorem", "dolor"))

## -----------------------------------------------------------------------------
library(magrittr)
# keep only observations with K1 > 0.1 and K4 < 0.3,
# without printing subset progress
subset4 <- admix_subset(xadmixture, 
                        anc = "K1", 
                        pct = 0.1, 
                        quiet = TRUE) %>% 
admix_subset(anc = "K1", 
             pct = 0.3, 
             comparison = "less", 
             quiet = TRUE)

# print number of observations for comparison
nrow(xadmixture)
nrow(subset4)

## -----------------------------------------------------------------------------
# ancestries (K) are in the fourth to last column,
# and plotted without showing bar labels
admix_barplot(xadmixture,
   K = 4:ncol(xadmixture),
   names = FALSE
)

## -----------------------------------------------------------------------------
# grouping data by column "country",
# and sorting each group by ancestry column "K1"
admix_barplot(subset1,
   K = 4:ncol(xadmixture),
   grouping = "country",
   sortkey = "K1"
)

# changing color palette to "turbo" from package 'viridis',
admix_barplot(subset2,
   K = 4:ncol(xadmixture),
   palette = "turbo",
   grouping = "species",
   sortkey = "K4"
)

## -----------------------------------------------------------------------------
# removing title and changing axis labels text
admix_barplot(subset3,
   K = 4:ncol(xadmixture),
   main = "",
   xlab = "Accessions",
   ylab = "Ancestry [%]", 
   palette = "alternating",
   sortkey = "K1",
   names = FALSE
)

## -----------------------------------------------------------------------------
# directly output grouped plot with clipping removed from elements
# (useful if there are groups with a low number of observations)
subset5 <- admix_subset(xadmixture,
                        anc = c("K3", "K4"), 
                        pct = c(0.3, 0.2), 
                        quiet = TRUE)


## -----------------------------------------------------------------------------
# noclip set to "TRUE"
admix_barplot(subset5, 
            K = 4:ncol(xadmixture),          
            sortkey = "K5",
            grouping = "country", 
            palette = "viridis", 
            names = FALSE, 
            main = "Noclip on",
            noclip = TRUE)
# noclip set to "FALSE"
admix_barplot(subset5, 
            K = 4:ncol(xadmixture),          
            sortkey = "K5",
            grouping = "country", 
            palette = "viridis", 
            names = FALSE, 
            main = "Noclip off",
            noclip = FALSE)

