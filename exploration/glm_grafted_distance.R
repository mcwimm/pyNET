library(tidyverse)
library(data.table)

# Logistic regression - tree level
# graft status ~ distance to nearest neighbor
# Load tree data
trees = fread(file = "../data/trees_50.txt") %>%
    data.frame(.) %>%
    mutate_at(vars(ROOTING, RESOURCES, GRAFTING), as.factor) %>% 
    mutate(grafted = factor(ifelse(partner == "[]", 0, 1)))


# grouped by scenario and year 
trees = trees %>% 
    mutate(yr = round(year)) %>% 
    unite("set2", setup, yr, remove = F)

mods = data.frame()
for (i in unique(trees$set2)){
    print(i)
    tt = trees %>% 
        filter(set2 %in% i)
    
    mod = glm(factor(grafted) ~ dist, data = tt, family = "binomial")
    nullmod <- glm(factor(grafted) ~ 1, data = tt, family="binomial")
    
    an = anova(mod, nullmod)

    mod_sum = data.frame(COEFF = summary(mod)$coefficients[2, 1],
                         PVAL = summary(mod)$coefficients[2, 1],
                         DEV = mod$deviance,
                         DEV_NULL = mod$null.deviance,
                         PANO = an$`Pr(>Chi)`[2]) %>% 
        bind_cols(., tt %>% select(ROOTING:RESOURCES, SEED, year) %>% distinct())
    mods = bind_rows(mods, mod_sum)

}

# Visualization of log-odds estimates ----
mods %>% 
    filter(abs(COEFF) < 11) %>% 
    distinct() %>% 
    mutate(PSIGN = ifelse(PVAL <= 0.05, "SIG", "NS")) %>% 
    ggplot(., aes(x = ROOTING, fill = GRAFTING,
                  y = COEFF)) +
    geom_hline(yintercept = 0) +
    geom_boxplot(alpha = 0.4) +
    # facet_grid(.~RESOURCES, labeller = label_both) +
    labs(y = "log-odds \n(glm: grafted ~ dist2NN)") +
    theme(legend.position = "bottom")

# ggsave("glm_grafted_distance.jpg", width = 4.5, height = 3)
ggsave("glm_grafted_distance.jpg", width = 6, height = 3.74)

mods %>% 
    mutate(SIGano = ifelse(PANO >= 0.05, "NS", "SIG (< 0.05)")) %>% 
    ggplot(., aes(x = SIGano)) +
    geom_bar()

mods %>% 
    mutate(SIGano = ifelse(PANO >= 0.05, "NS", "SIG (< 0.05)")) %>% 
    group_by(SIGano) %>% 
    reframe(n())


mods %>% 
    mutate(DIR = ifelse(COEFF >= 0, "positive", "negative")) %>% 
    group_by(DIR) %>% 
    reframe(n())

# Visualization tdens ~ distance 2 NN ----
trees %>% 
    group_by(set2) %>% 
    reframe(tdens = n() / (30*30/10000),
            distm = mean(dist),
            disti = min(dist),
            distx = max(dist)) %>% 
    ggplot(., aes(x = tdens, y = distm)) +
    geom_point() +
    geom_errorbar(aes(ymin = disti, ymax = distx))
