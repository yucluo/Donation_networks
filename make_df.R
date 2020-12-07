library(dplyr)
library(igraph)

# load network stats
network_stats = read.csv("network_stats.csv")

# load edge lists
ak_2005 = read.csv("A")




network_stats %>% filter(leg == "House") %>% select(state.ab, start.year, stop.year,All.Liberal.EV.1,All.Liberal.CL.1, All.Liberal.DEG.1, All.Cons.EV.1, All.Cons.CL.1, All.Cons.DEG.1, All.Dem.EV.1, All.Dem.CL.1, All.Dem.DEG.1, All.Rep.EV.1, All.Rep.CL.1, All.Rep.DEG.1)


# read all edge lists
file_names <- list.files(pattern="*House.csv")


# net <- read.csv("CO-2013-2014-House.csv")
# meta_df <- read.csv("/Users/yuchenluo/Desktop/causal_inference/Metadata/CO-2013-2014-House.csv")
# net <- net[net$edge>=3,]
# net <- as.matrix(net[,1:2])
# net <- graph_from_data_frame(net, vertices=meta_df, directed=F)
# #network_stats <- read_csv("Full_Cycle_Networks/network_stats.csv")

# eigen_col1314 <- eigen_centrality(net)

df = data.frame()
for (file in file_names){
  net = read.csv(file)
  path = getwd()
  meta_file = paste(path, "Metadata", file, sep="/")
  meta = read.csv(meta_file)
  meta$EID = as.character(meta$EID)
  net <- net[net$edge>=3,]
  net <- as.matrix(net[,1:2])
  net <- graph_from_data_frame(net, vertices=meta, directed=F)
  eigen_col <- as.data.frame(eigen_centrality(net)$vector)
  eigen_col$donor = as.character(row.names(eigen_col))
  eigen_col$year = mean(as.numeric(scan(text=file, sep="-", what="", quiet=TRUE)[2]), as.numeric(scan(text=file, sep="-", what="", quiet=TRUE)[3]))# select year as middle year
  eigen_col$state = scan(text=file, sep="-", what="", quiet=TRUE)[1]
  eigen_col = left_join(eigen_col,meta, by = c('donor' = 'EID'))
  colnames(eigen_col)[1] = "EV"
  eigen_col = eigen_col%>% select(EV, donor, year, state,PerDem, PerRep)
  eigen_col$party = ifelse(eigen_col$PerDem >0.6, "Dem", ifelse(eigen_col$PerRep > 0.6, "Rep", "None"))
  df = rbind(df, eigen_col)

}
write.csv(df, file = "RDD_dat.csv")

plot(df$year, df$EV)
