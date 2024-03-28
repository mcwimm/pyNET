############################################################
# Create dataframes with all links (every 50th time step), #
# require make_trees.R                                     # 
############################################################

library(tidyverse)
library(data.table)
source("helpers.R")
print("LINKS ----")

fs = list.files("../data", recursive = F) %>%
    str_subset(pattern = "SEED")


for (ff in fs) {
    print(paste("\t", ff))
    trees_50 = fread(file = paste("../data", ff, "trees_50.txt", sep = "/"))
    links_50 = data.frame()
    no_links = data.frame()
    for (sss in unique(trees_50$setup)){
        for (y in unique(trees_50$year)){
            t = trees_50 %>% 
                filter(setup == sss) %>% 
                filter(partner != "[]")
            t = subsetData(df = t, yearOfInterest = y)
            
            tt = splitID(t)
            ll = linksFromTrees(trees = tt)
            if (nrow(ll) != 0){
                ll = ll %>% 
                    mutate(dist = sqrt((x1-x2)^2 + (y1-y2)^2))
                
                ttt = tt %>% 
                    select(plant, r_stem:age, gIDpy)
                ll = ll %>% 
                    merge(., ttt, by.x = "ID1", 
                          by.y = "plant") %>% 
                    merge(., ttt %>% select(-gIDpy), by.x = "ID2", 
                          by.y = "plant", 
                          suffix = c("1", "2"))
                
                links_50 = bind_rows(links_50,
                                     ll %>% 
                                         mutate(setup = sss))
            } else {
                no_links = rbind(no_links,
                                 data.frame(setup = sss,
                                            year = y))
            }
        }
    }

    fwrite(links_50, 
           file = paste(paste("../data", ff, "links_50.txt", sep = "/")))
    fwrite(no_links, 
           file = paste(paste("../data", ff, "no_links.txt", sep = "/")))
}



