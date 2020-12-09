library(dplyr)
library(igraph)
library(ggplot2)
# load network stats
network_stats = read.csv("network_stats.csv")

# load edge lists
ak_2005 = read.csv("A")




network_stats %>% filter(leg == "House") %>% select(state.ab, start.year, stop.year,All.Liberal.EV.1,All.Liberal.CL.1, All.Liberal.DEG.1, All.Cons.EV.1, All.Cons.CL.1, All.Cons.DEG.1, All.Dem.EV.1, All.Dem.CL.1, All.Dem.DEG.1, All.Rep.EV.1, All.Rep.CL.1, All.Rep.DEG.1)


# read all edge lists
file_names <- list.files(path ="/Users/yuchenluo/Desktop/causal_inference/final/edge_list", pattern="*House.csv")


# net <- read.csv("CO-2013-2014-House.csv")
# meta_df <- read.csv("/Users/yuchenluo/Desktop/causal_inference/Metadata/CO-2013-2014-House.csv")
# net <- net[net$edge>=3,]
# net <- as.matrix(net[,1:2])
# net <- graph_from_data_frame(net, vertices=meta_df, directed=F)
# #network_stats <- read_csv("Full_Cycle_Networks/network_stats.csv")

# eigen_col1314 <- eigen_centrality(net)
path = getwd()

df = data.frame()

for (file in file_names){
  net = read.csv(paste(path, "edge_list", file, sep="/"))
  meta_file = paste(path, "Metadata", file, sep="/")
  meta = read.csv(meta_file)
  meta$EID = as.character(meta$EID)
  net <- net[net$edge>=3,]
  net <- as.matrix(net[,1:2])
  net <- graph_from_data_frame(net, vertices=meta, directed=F)
  eigen_col <- as.data.frame(eigen_centrality(net)$vector)
  eigen_col$donor = as.character(row.names(eigen_col))
  eigen_col$year = scan(text=file, sep="-", what="", quiet=TRUE)[2]# select year as start year
  eigen_col$state = scan(text=file, sep="-", what="", quiet=TRUE)[1]
  eigen_col = left_join(eigen_col,meta, by = c('donor' = 'EID'))
  colnames(eigen_col)[1] = "EV"
  eigen_col = eigen_col%>% select(EV, donor, year, state,PerDem, PerRep)
  eigen_col$party = ifelse(eigen_col$PerDem >0.6, "Dem", ifelse(eigen_col$PerRep > 0.6, "Rep", "None"))
  df = rbind(df, eigen_col)

}
write.csv(df, file = "RDD_dat.csv")

df = read.csv('RDD_dat.csv')



df$cycle =
plot(df$year, df$EV)

ggplot(data = df, aes(x = year, y = EV)) +
  geom_jitter()


# bandwidth 2009-2011
# restrict df to 2009 - 2011
df_res = df[df$year<2012 & df$year >2008, ]
df$trans_yr = as.numeric(df$year)-2010
df$eligible = ifelse(df$trans_yr<0, 0, 1)

lin_res = lm(EV ~ trans_yr+eligible, data = df)
summary(lin_res)


# DID estimate of E[Y(1)-Y(0) | Z=0]

df$time = ifelse(df$year >= 2010, 1, 0)
df$treated = ifelse(df$state == "VT", 0, 1)
df$did = df$time * df$treated

didreg = lm(EV ~ treated + time + did, data = df)
summary(didreg)

didreg1 = lm(EV ~ treated*time, data = df)
summary(didreg1)
