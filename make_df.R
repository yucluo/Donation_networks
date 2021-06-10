library(dplyr)
library(igraph)
library(ggplot2)
library(data.table)
library(birankr)
# # load network stats
# network_stats = read.csv("network_stats.csv")

# read all edge lists
file_names <- list.files(path ="/Users/yuchenluo/Desktop/causal_inference/final/edge_list", pattern="*House.csv")


# # influence via pagerank
# ca_1314_edges <- read_csv("/Users/yuchenluo/Desktop/causal_inference/final/edge_list/CA-2013-2014-House.csv")
#
# setDT(ca_1314_edges)
#
# ca1314_pr <- pagerank(ca_1314_edges, is_bipartite = TRUE, sender_name = "node_1", receiver_name = "node_2", weight_name = "edge")
#

path = getwd()

df = data.frame()

for (file in file_names){
  net = read.csv(paste(path, "edge_list", file, sep="/"))
  meta_file = paste(path, "Metadata", file, sep="/")
  meta = read.csv(meta_file)
  meta$EID = as.character(meta$EID)
  p_rank <- pagerank(net, is_bipartite = TRUE, sender_name = "node_1", receiver_name = "node_2", weight_name = "edge")
  colnames(p_rank)[1] = 'donor'
  p_rank$year = scan(text=file, sep="-", what="", quiet=TRUE)[2]# select year as start year
  p_rank$state = scan(text=file, sep="-", what="", quiet=TRUE)[1]
  p_rank = left_join(p_rank,meta, by = c('donor' = 'EID'))
  p_rank = p_rank%>% select(donor, rank, year, state,PerDem, PerRep, Total)
  df = rbind(df, p_rank)
}

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
  df = rbind(df, eigen_col)

}
setDT(df)
# only keep positive donations
df = df[Total >0, ]
write.csv(df, file = "RDD_dat.csv")

df = read.csv('RDD_dat.csv')


plot(df$year, df$EV)

ggplot(data = df, aes(x = year, y = rank, color = as.factor(eligible))) +
  geom_jitter()


# bandwidth 2009-2011

# # mean donation pre 2010
# mean_donation =  df %>% group_by(donor) %>% filter (year <2010) %>% summarise(mean(Total))
# summary(mean_donation$`mean(Total)`)
# df = left_join(df, mean_donation, by = 'donor')

# # create mean PR before 2010
# mean_pr = df %>% group_by(donor) %>% filter (year <2009) %>% summarise(mean(rank))
# colnames(mean_pr)[2] = "avg_pr"
# df= left_join(df, mean_pr, by = 'donor')


# mean percent of dem donation pre 2010
mean_party = df %>% group_by(donor) %>% filter (year <2010) %>% summarise(mean(PerDem))
colnames(mean_party)[2] = "avg_PerDem"
df = left_join(df, mean_party, by = 'donor')
df$party = ifelse(df$avg_PerDem >0.6, "Dem", ifelse(df$avg_PerDem < 0.4, "Rep", "None"))

df$eligible = ifelse(df$year<2010, 0, 1)



# restrict df to 2009 - 2011
df_res = df[df$year<2012 & df$year >2008, ]
df_res$trans_yr = as.numeric(df_res$year)-2010
df_res$eligible = ifelse(df_res$trans_yr<0, 0, 1)


# # see what is going on high rank donors
# df_pr_high = na.omit(df_res[df_res$avg_pr > 0.005, ])
#
# summary(lm(rank ~ trans_yr+eligible, data = df_pr_high))
#
#
#
# # check high versus low donors
# df_high_don = df_res[df_res$avg_total> 5000, ]
# df_low_don = df_res[df_res$avg_total<750, ]
# high_don = lm(rank ~ trans_yr+eligible, data = df_high_don)
# low_don = lm(rank ~ trans_yr+eligible, data = df_low_don)
# summary(high_don)
# summary(low_don)
#
# #high_ev = lm(rank ~ trans_yr+eligible, data = df_high)
# #summary(high_ev)
#
#
# #low_res = lm(rank ~ trans_yr+eligible, data = df_low)
# #summary(low_res)

### test partisanship
setDT(df_res)
df_rep = df_res[party == 'Rep',]
df_dem = df_res[party == 'Dem',]

lin_rep = lm(rank ~ trans_yr+eligible, data = df_rep)
lin_dem = lm(rank ~ trans_yr+eligible, data = df_dem)
summary(lin_rep)
summary(lin_dem)


# DID estimate of E[Y(1)-Y(0) | Z=0]

df$time = ifelse(df$year >= 2010, 1, 0)
df$treated = ifelse(df$state == "VT", 0, 1)
df$did = df$time * df$treated

didreg = lm(rank ~ treated + time + did, data = df)
summary(didreg)

didreg1 = lm(rank~ treated*time, data = df)
summary(didreg1)


