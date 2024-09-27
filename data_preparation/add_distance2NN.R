#######################################################
# Add distance to nearest neighbor (NN) to all trees  #
# from pyNET output                                   #
#######################################################

library(tidyverse)
library(data.table)

# Load data
trees_old = fread(file = "../data/trees_50.txt") %>%
    data.frame(.) %>% 
    unite("setup2", setup, year, remove = F)


trees = data.frame()
for (i in 1:length(unique(trees_old$setup2))){
    # Select setup and time step
    tt = trees_old %>% 
        filter(setup2 %in% unique(trees_old$setup2)[i])

    # Create distance matrix
    dd = as.matrix(dist(tt[, c("x", "y")])) 
    # Replace distance to itself with 99999
    idx = which(dd == 0)
    dd[idx] = 99999
    
    # Find minimum for each tree
    tt$dist = apply(dd, 2, min)
    
    trees = bind_rows(trees, tt)
}

fwrite(trees, 
       file = "../data/trees_50.txt")
