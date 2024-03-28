################################################
# Create dataframes with all graft formations, #
# require make_links.R                         # 
################################################


library(tidyverse)
library(data.table)
theme_set(theme_bw())
source("helpers.R")
print("RGF ----")


fs = list.files("../data", recursive = F) %>%
    str_subset(pattern = "SEED")
length(fs)
fs[1]


rgf = data.frame()
rg_func = data.frame()
for (ff in fs) {
    print(ff)
    # Read complete trees output (all years)
    ttt = fread(file = paste("../data", ff, "trees.txt", sep = "/")) %>% 
        filter(ENV %in% c(0:2))
    ttt = splitID(ttt)
    
    # Get rgf stats for GRAFTING with costs
    trees23 = ttt %>% 
        filter(GRAFT %in% c(2, 3)) %>% 
        group_by(SPE, GRAFT, ENV, plant) %>% 
        mutate(age_max = max(age),
               death = max(year)) %>% 
        filter(potential_partner != "[]") %>% 
        group_by(SPE, GRAFT, ENV, plant,
                 potential_partner) %>% 
        mutate(rgf_s = min(year),
               rgf_e = max(year),
               rgf_d = rgf_e - rgf_s, 
               r_stem_s = min(r_stem),
               potential_partner, x, y) %>% 
        filter(year > 400) %>% 
        ungroup() %>% 
        select(SPE, GRAFT, ENV, plant, potential_partner, 
               rgf_s, rgf_e, rgf_d, r_stem_s, 
               age_max, death, SEED, x, y) %>%
        filter(rgf_e < max(ttt$year)) %>%
        distinct() 
    
    trees23 = trees23 %>% 
        left_join(., ttt %>% 
                      filter(year > 400) %>% 
                      select(plant, x, y, ENV:SPE) %>% distinct(.),
                  by = c('potential_partner'='plant', "ENV",
                         "SPE", "GRAFT"))
    
    trees23 = trees23 %>% 
        mutate(dist = sqrt((x.x - x.y)^2 + (y.x - y.y)^2))
    
    # Get rgf stats for GRAFTING without costs
    trees01 = ttt %>% 
        filter(GRAFT %in% c(0, 1)) %>% 
        group_by(SPE, GRAFT, ENV, plant) %>% 
        mutate(age_max = max(age),
               death = max(year)) %>% 
        group_by(SPE, GRAFT, ENV, plant,
                 partner) %>%
        mutate(rgf_age_grafted = min(age), 
               rgf_s = rgf_age_grafted, rgf_e = rgf_age_grafted, 
               rgf_d = rgf_e - rgf_s, 
               r_stem_s = min(r_stem),
               age_max, death, SEED) %>%
        filter(year > 400) %>% ungroup() %>% 
        distinct(SPE, GRAFT, ENV, plant,
                 rgf_s, rgf_e, rgf_d, r_stem_s, age_max, 
                 death, SEED) 
    
    # Combine stats.
    rgf = bind_rows(rgf, trees01, trees23)
    
    
    # Get age when first graft was formed
    min_rgf_age = ttt %>% 
        filter(partner != "[]") %>%
        group_by(SPE, GRAFT, ENV, plant) %>% 
        mutate(rgf_age_grafted = min(age)) %>% 
        filter(year > 400) %>% 
        select(SPE, GRAFT, ENV, plant, rgf_age_grafted) %>% 
        distinct()
    
    rg_func = rbind(rg_func, min_rgf_age)
    
}

rgf = rgf %>% data.frame(.) %>% 
    rename("ROOTING" = "SPE",
           "GRAFTING" = "GRAFT",
           "RESOURCES" = "ENV") 
rgf = rgf %>% 
    mutate(died_rgf = ifelse(rgf_e == death, 1, 0))
fwrite(rgf, file = "rgf.txt")

rg_func = rg_func %>% data.frame(.) %>% 
    rename("ROOTING" = "SPE",
           "GRAFTING" = "GRAFT",
           "RESOURCES" = "ENV") 
fwrite(rg_func, file = "rg_func.txt")
