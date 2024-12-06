# Tackett Data in networks
# https://www.mdpi.com/article/10.3390/d14030179/s1. 
# Dataviz ideas: https://github.com/manlius/muxViz/blob/master/gui-old/theory/README.md

require(here)
library(dplyr)
library(ggraph)
require(networkD3)
require(echarts4r)
devtools::install_github('Ecological-Complexity-Lab/emln', force=T) # new package - explore maybe
library(emln)
# install.packages("ggalluvial")
library(ggalluvial)
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(xlsx)) install.packages('xlsx')
if (!require(igraph)) install.packages('igraph')

setwd(here())
setwd('data')

df <- read.xlsx('Supplementary Materials-Table S3.xlsx', sheetIndex = 1, startRow=2)

colnames(df)

# Layers to explore 
df[c('species', 'body.part', 'cures'  )]

unique(df$IUCN.Region)
unique(df$species)
unique(df$body.part)
unique(df$cures)

# Prep

df <- df %>%
  filter(species != "unknown")

df <- df %>%
  mutate(across(everything(), ~ gsub("^unknown\\s*$", "unknown", .)))

df <- df %>%
  mutate(across(everything(), ~ gsub("^unknown\\s*$", "unknown", .)))

links <- df %>%
  count(species, body.part, cures) %>%
  rename(value = n) %>%
  pivot_longer(cols = c(body.part, cures), names_to = "target_type", values_to = "target") %>%
  select(source = species, target, value)

links

nodes <- unique(c(links$source, links$target))
nodes <- data.frame(name = nodes)

head(nodes)
head(links)

# Plot a cool sankey graph 

links %>%
  e_charts() %>%
  e_sankey(links, source = source, target = target, value = value) %>%
  e_theme("macarons")


# Multilayer attempts -----------------

ggplot(data = df,
       aes(axis1 = species, axis2 = body.part, axis3 = cures)) +
  geom_alluvium(aes(fill = species), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("species", "body.part", "cures"), expand = c(0.15, 0.15)) +
  labs(title = "Alluvial Plot of Species, Body Parts, and Cures",
       y = "Frequency") +
  theme_minimal()


#-------------------------------------------------------------------------------------------------------------