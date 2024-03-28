#############################################################
# Helper functions to prepare pyNET raw output for analysis #
#############################################################

library(igraph)
library(data.table)

# Subset data frame to get all entries from a certain time step
subsetData = function(df, yearOfInterest){
    # get year closest to yearOfInterest
    yoi = getYearOfInterest(df, yearOfInterest)
    d = df %>%
        filter(year == yoi) %>% na.omit %>% 
        select(everything())
    return(d)
}

# Get year from data frame closest to a given value
getYearOfInterest = function(df, yearOfInterest){
    return(df %>% ungroup() %>% 
               slice(which.min(abs(year - yearOfInterest))) %>% 
               distinct(year) %>% unlist(.))
}

# Remove 0s from tree ID
splitID = function(trees){
    trees = trees %>%
        data.frame(.) %>% 
        rowwise() %>% 
        mutate(plant = paste(str_replace(strsplit(as.character(plant), "_")[[1]],
                                         "^0+", ""), collapse = "")) %>% 
        mutate(ID = as.numeric(str_match(plant, '\\d+$')[[1]])) 
    return(trees)
}

# Get data frame with all pairs of grafted trees
linksFromTrees = function(trees){
    links = data.frame()
    
    if (length(unique(trees$partner)) > 1){
        # create links data frame
        links = makeLinksFromTrees(trees)
        # add group attributes
        links = links %>% 
            rowwise() %>% 
            mutate(graftName = paste(ID1, ID2, sep = "-"))
        links = links[!duplicated(links$graftName),] 
        
        net = graph_from_data_frame(d = links, vertices = trees, directed = F)
        comps = components(net)
        trees$groupID = paste("gID_", comps$membership, sep = "")
        trees = trees %>% 
            group_by(groupID) %>% 
            mutate(groupSize = n())
        
        links = links %>% merge(., trees[, c("plant", "groupID", "groupSize")],
                                by.x = "ID1", by.y = "plant", all = F) %>% 
            group_by(groupID) %>% 
            mutate(noLinks = n(), shape = noLinks / (groupSize - 1))
        
        
        links = links %>% 
            merge(., trees[, c(1:4)], by.x = c("ID1", "time"), 
                  by.y = c("plant", "time")) %>% 
            merge(., trees[, c(1:4)], by.x = c("ID2", "time"), 
                  by.y = c("plant", "time"), 
                  suffix = c("1", "2"))
    } else {
        print("No partners recognized in trees data frame.")
    }
    return(links)
}


# !!! NOTE
# if input incl. all trees (grafted and non-grafted) it returns also 
# non-functional links
makeLinksFromTrees = function(trees_one_ts){
    trees_one_ts$water_exchanged = as.numeric(trees_one_ts$water_exchanged)
    links = data.frame()
    for (i in 1:nrow(trees_one_ts)){
        partners = str_remove_all(trees_one_ts$partner[i], "[[:punct:]]")
        partners = strsplit(partners, " ")[[1]]
        for (partner in partners){
            if (partner %in% trees_one_ts$plant){
                n1 = strsplit(as.character(trees_one_ts[i, "plant"]), "_")[[1]]
                n1 = as.numeric(gsub("[^\\d]+", "", n1, perl=TRUE))
                
                n2 = as.numeric(str_match(partner, '\\d+$')[[1]])
                
                if (as.numeric(n1) < n2){
                    ID1 = trees_one_ts[i, "plant"][[1]]
                    ID2 = partner
                    wrg = trees_one_ts[i, "water_exchanged"][[1]]
                } else {
                    ID1 = partner
                    ID2 = trees_one_ts[i, "plant"][[1]]
                    wrg = -trees_one_ts[i, "water_exchanged"][[1]]
                    
                }
                
                l = data.frame(ID1 = ID1, ID2 = ID2, wrg = wrg,
                               time = trees_one_ts[i, "time"], 
                               year = trees_one_ts[i, "year"])
                links = rbind(links, l)
            } else {
                # print(paste("Dead link occured between ",
                #             trees_one_ts[i, "plant"][[1]],
                #             " and its partner (dead tree) ",
                #             partner, ", groupID: ", trees_one_ts[i, "groupID"][[1]],
                #             sep = ""))
            }
            
        }
    }
    return(links)
}

# Calculate tree volume based on BETTINA approach and
# add it to trees df
add_volume = function(trees, h_crown = 0.004, h_root = 0.004){
    return(
        trees %>% 
            mutate(volume = h_crown * pi * r_crown^2 +
                       2*r_crown*pi*r_stem^2 + 
                       pi*r_stem^2*h_stem +
                       pi * sqrt(0.5)*r_stem^2*r_root + 
                       h_root * pi * r_root^2)
    )
}


range01 <- function(x){(x-min(x))/(max(x)-min(x))}
