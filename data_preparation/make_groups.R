#############################################################
# Create dataframes with all groups (every 50th time step), #
# require make_links.R                                      # 
#############################################################


library(tidyverse)
library(data.table)
source("helpers.R")
print("GROUPS ----")

fs = list.files("../data", recursive = F) %>%
    str_subset(pattern = "SEED")


for (ff in fs) {
    print(paste("\t", ff))
    links_50 = fread(file = paste("../data", ff, "links_50.txt", sep = "/"))

    groups_50 = links_50 %>%
        group_by(setup, year, groupSize, noLinks, groupID)  %>% 
        reframe(dist_mean = mean(dist),
                dist_max = max(dist),
                shape = noLinks / (groupSize - 1),
                gIDpy = gIDpy1) %>%
        distinct(.)

    fwrite(groups_50, 
           file = paste(paste("../data", ff, "groups_50.txt", sep = "/")))
}
