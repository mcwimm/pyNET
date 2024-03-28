library('vivid')
library("randomForest")
library("tidyverse")
library(data.table)


outs = c("gs", "nlinks", "shape")

# Prepare data ----
# i.e., combine group variables in one table
# Create dataframe with all grafted trees based on links table
links_50 = fread(file = "../data/links_50.txt") 

ll1 = links_50 %>% 
    select(ID1, year, groupID:shape, dist,
           x1, y1, r_stem1:age1, gIDpy1,
           setup:GRAFTING)

ll2 = links_50 %>% 
    select(ID2, year, groupID:shape, dist,
           x2, y2, r_stem2:age2, gIDpy2,
           setup:GRAFTING)

colnames(ll1) = c("ID", "year", "groupID", "gs", "nlinks", "shape",
               "dist", "x", "y", 
               "r_stem", "h_stem", "r_crown", "r_root",
               "ag_factor", "bg_factor", "age", "gIDpy",
               "setup", "SEED", "ROOTING", "RESOURCES", "GRAFTING")
colnames(ll2) = colnames(ll1)

links = bind_rows(ll1, ll2)

# Create groups data frame ----
# ggg = links %>%
#     group_by(year, setup, groupID) %>%
#     reframe(
#         dbh_m = mean(r_stem * 200),
#         height_m = mean(h_stem  + 2*r_crown),
#         bg_m = mean(bg_factor),
#         ag_m = mean(ag_factor),
#         age_m = mean(age),
#         age_q95 = quantile(age, 0.95),
#         root_m = mean(r_root),
#         dist_m = mean(dist),
#         dist_x = max(dist),
#         gs, nlinks, shape, setup, SEED,
#         ROOTING, RESOURCES, GRAFTING
#     ) %>% distinct(.)
# 
# fwrite(ggg, file = "../data_preparation/VAR_IMP/GROUP/groups_ext.txt")

ggg = fread(file = "../data_preparation/VAR_IMP/GROUP/groups_ext.txt")

# Run random forest models for all output variables ----
for (out in outs){
    print(out)
    # Load train and test data
    group_data = ggg %>% 
        select(dbh_m:dist_x, GRAFTING, all_of(out)) %>% 
        rename("y" = out)
    set.seed(123)
    train_test_split <- sample(c(1:nrow(group_data)),
                               0.8*nrow(group_data), replace=F) 
    data_train <- group_data[train_test_split,]
    data_test  <- setdiff(group_data, data_train)
    
    saveRDS(data_train, file = paste("../data_preparation/VAR_IMP/GROUP/", "data_train_", out, ".rds", sep = ""))
    saveRDS(data_test, file = paste("../data_preparation/VAR_IMP/GROUP/", "data_test_", out, ".rds", sep = ""))
    
    # rf
    set.seed(123)
    rf <- randomForest(y ~ ., data = data_train, importance = T)
    
    # vivid
    set.seed(123)
    vi <- vivi(data = data_train, fit = rf, response = 'y', normalized = T)
    
    saveRDS(rf, file = paste("../data_preparation/VAR_IMP/GROUP/rf_", out, ".rds", sep = ""))
    saveRDS(vi, file = paste("../data_preparation/VAR_IMP/GROUP/vi_", out, ".rds", sep = ""))
}


# Plot variable importance maps ----
i = 1
for (out in outs){
    rf = readRDS(file = paste("../data_preparation/VAR_IMP/GROUP/rf_", out, ".rds", sep = ""))
    vi = readRDS(file = paste("../data_preparation/VAR_IMP/GROUP/vi_", out, ".rds", sep = ""))
    rsq = round(100 * rf$rsq[length(rf$rsq)], digits = 2)
    t = data.frame(V1 = diag(vi)) %>% 
        mutate(r = rank(-V1),
               Variable_1 = row.names(.),
               Variable_2 = row.names(.))
    
    p = viviHeatmap(mat = vi, angle = 45, border = T) +
        ggtitle(paste(out, ": ", rsq, " %", sep = "")) +
        geom_text(t, mapping=aes(label = r, alpha = -r), show.legend = F) +
        geom_tile(t, mapping=aes(size = -r), col = "black", fill = NA, show.legend = F) +
        scale_size_continuous(range = c(0.1, 1)) 
    # p
    saveRDS(p, file = paste("../data_preparation/VAR_IMP/GROUP/viviHeatmap_", out, ".rds", sep = ""))
    
    nam <- paste("p", i, sep = "")
    assign(nam, p)
    i = i + 1

}


ggarrange(p1, p2, p3, ncol = 3,
          common.legend = T,
          legend = "bottom",
          labels = c("(a)", "(b)", "(c)"),
          align = "hv")

ggsave("../data_preparation/VAR_IMP/groups.jpg",
       width = 10, height = 4)


