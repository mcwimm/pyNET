#############################################################
# Create dataframes with a summary of stand characteristics #
# (every 50th time step),                                   #
# require make_trees.R and make_groups.R                    # 
#############################################################

library(tidyverse)
library(data.table)
source("helpers.R")
print("STANDS ----")

fs = list.files("../data", recursive = F) %>%
    str_subset(pattern = "SEED")


for (ff in fs) {
    print(paste("\t", ff))
    
    trees_50 = fread(file = paste("../data", ff, "trees_50.txt", sep = "/"))
    groups_50 = fread(file = paste("../data", ff, "groups_50.txt", sep = "/"))
    
    
    stands_50 = trees_50 %>%
        mutate(grafted = ifelse(partner == "[]", 0, 1)) %>%
        group_by(SEED, GRAFT, ENV, SPE, setup, year) %>%
        reframe(
            salinity = mean(salinity, na.rm = T),
            bg_mean = mean(bg_factor, na.rm = T),
            ag_mean = mean(ag_factor, na.rm = T),
            root_mean = mean(r_root, na.rm = T),
            age_mean = mean(age, na.rm = T),
            age_q95 = quantile(age, 0.95),
            age_max = max(age, na.rm = T),
            no_trees = n(),
            tdens = no_trees / (30*30/10000),
            basal_area = sum(pi * (r_stem * 200 / 2) ^ 2 / (30 * 30 / 10000)),
            dbh_mean = mean(r_stem * 200, na.rm = T),
            height_mean = mean((h_stem + 2 * r_crown), na.rm = T),
            gain = ifelse(water_exchanged > 0, 1, 0),
            no_gain = sum(gain)
        )
    
    stands_50_group_sum = groups_50 %>%
      select(-gIDpy) %>% distinct(.) %>% 
       mutate(shape = noLinks / (groupSize - 1),
              non_redundant = ifelse(shape == 1, 1, 0),
              gs2 = ifelse(groupSize == 2, 0, 1),
              non_red_2 = ifelse(non_redundant+gs2 == 2, 1, 0)) %>% 
       group_by(setup, year) %>% 
       reframe(
          no_groups = n(),
          no_groups_bigger_2 = sum(gs2),
          gdens = no_groups / (30*30/10000),
          no_grafted = sum(groupSize),
          gs_mean = mean(groupSize),
          gs_max = max(groupSize),
          node_degree_mean = mean(2 * noLinks / groupSize),
          shape_mean = mean(noLinks / (groupSize - 1)),
          p_non_red = sum(non_redundant) / no_groups * 100,
          p_non_red_no2 = sum(non_red_2) / no_groups_bigger_2 * 100
       ) %>% 
        mutate(p_non_red_no2 = ifelse(gs_max == 2, 0, p_non_red_no2))
    
    
    stands_50 = left_join(stands_50, stands_50_group_sum,
                          by = c("setup", "year"))
    
    stands_50 = stands_50 %>% 
        mutate(fgraft = no_grafted / no_trees * 100,
               fgain = no_gain / no_grafted *100)
    
    fwrite(stands_50, 
           file = paste(paste("../data", ff, "stands_50.txt", sep = "/")))
}



  