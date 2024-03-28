library('vivid')
library("randomForest")
library("tidyverse")
library(data.table)
library(ggpubr)

# Prepare data ----
ROOTINGS = factor(c(0:2))

trees_50 = fread(file = "../data/trees_50.txt") %>% 
  mutate(grafted = as.factor(ifelse(partner == "[]", "no", "yes")))
names(trees_50)


# Run random forest models for all output variables ----
for (root in ROOTINGS){
  # Load train and test data
  tree_data = trees_50 %>% 
    filter(ROOTING == root) %>% 
    select(r_stem:age, dist, GRAFTING, grafted) 
  set.seed(123)
  train_test_split <- sample(c(1:nrow(tree_data)),
                             0.8*nrow(tree_data), replace=F) 
  data_train <- tree_data[train_test_split,]
  data_test  <- setdiff(tree_data, data_train)
  
  saveRDS(data_train, file = paste("../data_preparation/VAR_IMP/TREE/", "data_train_ROOTING_", root, ".rds", sep = ""))
  saveRDS(data_test, file = paste("../data_preparation/VAR_IMP/TREE/", "data_test_ROOTING_", root, ".rds", sep = ""))
  
  # rf
  set.seed(123)
  rf <- randomForest(grafted ~ ., data = data_train, importance = T)
  
  # vivid
  set.seed(123)
  vi <- vivi(data = data_train, fit = rf, response = 'grafted', normalized = T)
  
  saveRDS(rf, file = paste("../data_preparation/VAR_IMP/TREE/rf_ROOTING_", root, ".rds", sep = ""))
  saveRDS(vi, file = paste("../data_preparation/VAR_IMP/TREE/vi_ROOTING_", root, ".rds", sep = ""))
}



# Plot variable importance maps ----
i = 1
for (root in ROOTINGS){
  rf = readRDS(file = paste("../data_preparation/VAR_IMP/TREE/rf_ROOTING_", root, ".rds", sep = ""))
  vi = readRDS(file = paste("../data_preparation/VAR_IMP/TREE/vi_ROOTING_", root, ".rds", sep = ""))
  rsq = data.frame(pred = rf$predicted, obs = rf$y) %>% 
    mutate(same = ifelse(pred == obs, 1, 0)) %>% 
    reframe(r = sum(same)/n())
  t = data.frame(V1 = diag(vi)) %>% 
    mutate(r = rank(-V1),
           Variable_1 = row.names(.),
           Variable_2 = row.names(.))
  
  p = viviHeatmap(mat = vi, angle = 45, border = T) +
    ggtitle(paste("ROOTING ", root, ": ", round(rsq[[1]]*100, 2), " %", sep = "")) +
    geom_text(t, mapping=aes(label = r, alpha = -r), show.legend = F) +
    geom_tile(t, mapping=aes(size = -r), col = "black", fill = NA, show.legend = F) +
    scale_size_continuous(range = c(0.1, 1))
  # p
  saveRDS(p, file = paste("../data_preparation/VAR_IMP/TREE/viviHeatmap_ROOTING_", root, ".rds", sep = ""))
  
  nam <- paste("p", i, sep = "")
  assign(nam, p)
  i = i + 1
  
}

ggarrange(p1, p2, p3, ncol = 3,
          common.legend = T,
          legend = "bottom",
          labels = c("(a)", "(b)", "(c)"),
          align = "hv")
# p1
ggsave("../data_preparation/VAR_IMP/trees_ROOTING.jpg", 
       width = 10, height = 3.7)


p3  +
    theme(legend.position = "bottom")
ggsave("../data_preparation/VAR_IMP/trees_ROOTING_legend.jpg", 
       width = 10, height = 3.7)

