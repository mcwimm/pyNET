#######################################################
# Create dataframes with all trees from pyNET output  #
# Extract every 50th time step                        #
#######################################################

library(tidyverse)
library(data.table)

source("helpers.R")
print("TREES ----")


fs = list.files("../data", recursive = F) %>%
    str_subset(pattern = "SEED")

for (ff in fs) {
    fff = list.files(paste("../data", ff, sep = "/")) %>%
        str_subset(pattern = "setup_")
    trees = data.frame()
    print(paste("\t", ff))
    
    for (f in fff){
        s = strsplit(f, "_")[[1]]
        
        # all trees 0 - 600 ----
        # Removed all entries with is.na(ag_factor) to ignore trees
        # that immediately died for further analyses
        t = fread(paste("../data", ff, f, "Population.csv", sep = "/")) %>%
            mutate(year = time / 3600 / 24 / 365.25,
                ENV = s[3],
                GRAFT = s[5],
                SPE = s[7],
                SEED = s[9],
                setup = f,
            ) %>%
            group_by(plant) %>%
            mutate(age = age / 3600 / 24 / 365.25) %>% 
            data.frame(.)
        
        t = t %>% 
            filter(!is.na(ag_factor))
        
        trees = rbind(trees, t)
    }
    
    trees = trees %>%
        mutate(salinity = as.numeric(psi_osmo) / -85000) %>% 
        rename("gIDpy" = "groupID") 
    
    fwrite(trees, file = paste(paste("../data", ff, "trees.txt", sep = "/")))
    
    # trees 400 - 600, every 50. years ----
    trees_50 = data.frame()
    trees_400 = trees %>% 
        filter(year >= 400)
    
    all_years = unique(trees_400$year)
    yoi = all_years[seq(1, length(all_years), 50)]
    
    for (y in yoi){
        t = subsetData(df = trees_400, yearOfInterest = y)
        trees_50 = bind_rows(trees_50, t)
    }
    
    
    fwrite(trees_50, 
           file = paste(paste("../data", ff, "trees_50.txt", sep = "/")))
}

