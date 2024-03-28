library('vivid')
library("randomForest")
library("tidyverse")
library(data.table)
library(ggpubr)

# Prepare data ----

outs = c("fgraft", "gdens", "gsm", "gsx", "fred", "fgain")
stands = fread(file = "../data/stands_50.txt") %>%
    data.frame(.) %>%
    mutate(ROOTING = factor(ROOTING),
           RESOURCES = factor(RESOURCES),
           GRAFTING = factor(GRAFTING)) %>% 
    filter(RESOURCES %in% c(0:2)) %>%
    filter(GRAFTING %in% c(0:3)) %>%
    filter(ROOTING %in% c(0:2)) %>% 
    rename("gsm" = "gs_mean",
           "gsx" = "gs_max",
           "fred" = "p_non_red_no2",
           "degree" = "node_degree_mean",
           "shape" = "shape_mean")

names(stands)


# Run random forest models for all output variables ----
for (out in outs[6]){
    print(out)
    # Load train and test data
    stand_data = stands %>% 
        select(GRAFTING, bg_mean:tdens, dbh_mean, height_mean, all_of(out)) %>% 
        select(-no_trees, -age_max) %>% 
        rename("y" = out)
    set.seed(123)
    train_test_split <- sample(c(1:nrow(stand_data)),
                               0.8*nrow(stand_data), replace=F) 
    data_train <- stand_data[train_test_split,]
    data_test  <- setdiff(stand_data, data_train)
    
    saveRDS(data_train, file = paste("../data_preparation/STAND/", "data_train_", out, ".rds", sep = ""))
    saveRDS(data_test, file = paste("../data_preparation/STAND/", "data_test_", out, ".rds", sep = ""))
    
    # rf
    set.seed(123)
    rf <- randomForest(y ~ ., data = data_train, importance = T)
    
    # vivid
    set.seed(123)
    vi <- vivi(data = data_train, fit = rf, response = 'y', normalized = T)
    
    saveRDS(rf, file = paste("../data_preparation/STAND/rf_", out, ".rds", sep = ""))
    saveRDS(vi, file = paste("../data_preparation/STAND/vi_", out, ".rds", sep = ""))
}


# Plot variable importance maps ----
i = 1
for (out in outs){
    rf = readRDS(file = paste("../data_preparation/STAND/rf_", out, ".rds", sep = ""))
    vi = readRDS(file = paste("../data_preparation/STAND/vi_", out, ".rds", sep = ""))
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
    saveRDS(p, file = paste("../data_preparation/STAND/viviHeatmap_", out, ".rds", sep = ""))
    
    nam <- paste("p", i, sep = "")
    assign(nam, p)
    i = i + 1

    
}

x11()
ggarrange(rename.axis(p1), rename.axis(p2), rename.axis(p3),
          rename.axis(p4), rename.axis(p5), rename.axis(p6),
          common.legend = T,
          legend = "bottom",
          labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
          align = "hv")

ggsave("../data_preparation/stands.jpg", 
       width = 9.5, height = 6.5)



rename.axis = function(p){
    return(
        p +
            scale_x_discrete(labels=c("root_mean" = "root_m",
                                      "ag_mean" = "ag_m",
                                      "bg_mean" = "bg_m",
                                      "dbh_mean" = "dbh_m",
                                      "height_mean" = "height_m",
                                      "age_mean" = "age_m"),
                             position = "top") +
            scale_y_discrete(labels=c("root_mean" = "root_m",
                                      "ag_mean" = "ag_m",
                                      "bg_mean" = "bg_m",
                                      "dbh_mean" = "dbh_m",
                                      "height_mean" = "height_m",
                                      "age_mean" = "age_m"))
    )
}
