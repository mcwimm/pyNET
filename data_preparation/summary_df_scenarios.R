########################################
# Combine all scenarios in one file ####
########################################

library(tidyverse)
library(data.table)
library(spatstat)

fs = list.files("../data", recursive = F) %>%
    str_subset(pattern = "SEED")

trees_50 = data.frame()
stands_50 = data.frame()
groups_50 = data.frame()
links_50 = data.frame()
for (ff in fs) {
    print(ff)
    t50 = fread(file = paste("../data", ff, 
                             "trees_50.txt", sep = "/"))
    
    trees_50 = bind_rows(trees_50, t50)
    
    s50 = fread(file = paste("../data", ff, 
                             "stands_50.txt", sep = "/"))
    
    stands_50 = bind_rows(stands_50, s50)
    
    
    g50 = fread(file = paste("../data", ff, 
                             "groups_50.txt", sep = "/"))
    
    groups_50 = bind_rows(groups_50, g50)
    
    
    l50 = fread(file = paste("../data", ff, 
                             "links_50.txt", sep = "/"))
    
    links_50 = bind_rows(links_50, l50)
    
}

stands_50 = stands_50 %>% 
    mutate(ROOTING = SPE,
           GRAFTING = GRAFT,
           RESOURCES = ENV)
trees_50 = trees_50 %>% 
    mutate(ROOTING = SPE,
           GRAFTING = GRAFT,
           RESOURCES = ENV)

# Add nearest neighbor to trees
trees_50 = trees_50 %>% 
    group_by(setup, year) %>% 
    mutate(dist = nndist(x, y)) %>% 
    ungroup()


fwrite(trees_50, file = "trees_50.txt")
fwrite(stands_50, file = "stands_50.txt")
fwrite(groups_50, file = "groups_50.txt")
fwrite(links_50, file = "links_50.txt")
