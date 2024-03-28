library(tidyverse)
library(data.table)

fs = list.files("pyNET_example", recursive = T) %>% 
  str_subset(pattern = ".csv") 


### Morris ----
pyOut = data.frame()
for (f in fs) {
    s = strsplit(f, "/")[[1]][1]
    print(s)
    
    tt = fread(paste("ModelOutput", f, sep = "/")) %>% 
        mutate(year = time / 3600 / 24 / 365.25) 
    
    t = tt %>%
        filter(!is.na(ag_factor)) %>% 
        group_by(year) %>% 
        reframe(no_trees = n(),
                dbh_mean = mean(200*r_stem, na.rm=T),
                root_mean = mean(r_root, na.rm=T),
                grafted = ifelse(partner == "[]", 0, 1),
                fgraft = sum(grafted) / no_trees * 100) %>% 
        select(-grafted) %>% distinct(.)
    
    g = tt %>% 
        filter(partner != "[]") %>% 
        group_by(year, groupID) %>% 
        reframe(group_size = n()) %>% 
        group_by(year) %>% 
        reframe(gsx = max(group_size, na.rm = T),
                gsm = mean(group_size, na.rm = T),
                no_groups = n())

    t = merge(t, g, by = "year", all = T) %>% 
        mutate(setup = s,
               i = strsplit(s, "_")[[1]][2]) 
    t = t %>% replace_na(list(gsx = 0,
                              gsm = 0,
                              no_groups = 0)) 
    pyOut = rbind(pyOut, t)
}

fwrite(pyOut, file = "Transfer/pyOut.txt")

ys = unique(pyOut$year)
for (y in ys){
    print(y)
    pyOut %>% 
        group_by(year) %>% 
        arrange(i) %>% 
        select(-year, -setup) %>% 
        fwrite(., file = paste("Transfer/pyOut_", 
                               round(y), ".txt",
                               sep = ""),
               sep = "\t",
               col.names = F)
    
}

### Exploration ----
trees = data.frame()
for (f in fs) {
    s = strsplit(strsplit(f, "/")[[1]][1], "_")[[1]][2]
    print(s)
    
    tt = fread(paste("pyNET_example", f, sep = "/")) %>% 
        mutate(year = time / 3600 / 24 / 365.25) 
    
    t = tt %>%
        filter(!is.na(ag_factor)) %>% 
        group_by(year) %>% 
        mutate(no_trees = n(),
                dbh_mean = mean(200*r_stem, na.rm=T),
                root_mean = mean(r_root, na.rm=T),
                grafted = ifelse(partner == "[]", 0, 1),
                fgraft = sum(grafted) / no_trees * 100,
               i = s) 

    trees = rbind(trees, t)
}

fwrite(trees, file = "Transfer/trees.txt")
