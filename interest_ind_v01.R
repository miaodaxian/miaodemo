
if(!require("fredr")){ install.packages("fredr")} 
if(!require("bit64")){ install.packages("bit64")} 
library("fredr")
library("bit64")
library(data.table)
library(doParallel)
library(rlang)
library(tidyverse)
library(igraph)
library(amap)
library(lubridate)
#######################
#input
#########################
tree_size<-60
weight1<-1
weight2<-0
weight3<-0.1

#############################
###############################
#read from FRED
fredr_set_key("780eae23dab705bd9ca6f0fa652b38b5")
dgs1_data<-fredr(
  series_id = "DGS1",
  observation_start = as.Date("1994-01-01"),
  frequency = "d"
)

dgs2_data<-fredr(
  series_id = "DGS2",
  observation_start = as.Date("1994-01-01"),
  frequency = "d"
)

dgs3_data<-fredr(
  series_id = "DGS3",
  observation_start = as.Date("1994-01-01"),
  frequency = "d"
)

dgs5_data<-fredr(
  series_id = "DGS5",
  observation_start = as.Date("1994-01-01"),
  frequency = "d"
)

dgs7_data<-fredr(
  series_id = "DGS7",
  observation_start = as.Date("1994-01-01"),
  frequency = "d"
)

dgs10_data<-fredr(
  series_id = "DGS10",
  observation_start = as.Date("1994-01-01"),
  frequency = "d"
)

dgs20_data<-fredr(
  series_id = "DGS20",
  observation_start = as.Date("1994-01-01"),
  frequency = "d"
)

dgs_df<-data.frame(Date=dgs1_data$date,DGS1=dgs1_data$value,DGS2=dgs2_data$value,DGS3=dgs3_data$value,
           DGS5=dgs5_data$value,DGS7=dgs7_data$value, DGS10=dgs10_data$value,DGS20=dgs20_data$value)
dgs_df_clean<-dgs_df[!is.na(dgs_df$DGS1),]

#dgs_df_clean<-dgs_df_clean[1:6558,]

#Read from tsy
tsy_5yr<-fread("~/Desktop/ShinyApps/int_ind/FVX.csv")
tsy_10yr<-fread("~/Desktop/ShinyApps/int_ind/TNX.csv")
tsy_30yr<-fread("~/Desktop/ShinyApps/int_ind/TYX.csv")

tsy_5yr$Date<-as.Date(paste(substr(tsy_5yr$V1,0,4),substr(tsy_5yr$V1,5,6),substr(tsy_5yr$V1,7,8),sep="-"))
tsy_10yr$Date<-as.Date(paste(substr(tsy_10yr$V1,0,4),substr(tsy_10yr$V1,5,6),substr(tsy_10yr$V1,7,8),sep="-"))
tsy_30yr$Date<-as.Date(paste(substr(tsy_30yr$V1,0,4),substr(tsy_30yr$V1,5,6),substr(tsy_30yr$V1,7,8),sep="-"))

#check whether there are more updates from tsy
if(max(tsy_5yr$Date)>max(dgs_df_clean$Date))
{
  tsy_5yr_clean<-tsy_5yr[tsy_5yr$Date>=max(dgs_df_clean$Date),]
  tsy_5yr_clean<-tsy_5yr_clean[order(tsy_5yr_clean$Date),]
  tsy_10yr_clean<-tsy_10yr[tsy_10yr$Date>=max(dgs_df_clean$Date),]
  tsy_10yr_clean<-tsy_10yr_clean[order(tsy_10yr_clean$Date),]  
  tsy_30yr_clean<-tsy_30yr[tsy_30yr$Date>=max(dgs_df_clean$Date),]
  tsy_30yr_clean<-tsy_30yr_clean[order(tsy_30yr_clean$Date),]
  
  last_update_dgs<-as.vector(dgs_df_clean[dgs_df_clean$Date==max(dgs_df_clean$Date),c("DGS1", "DGS2", "DGS3", "DGS5", "DGS7", "DGS10", "DGS20")])
  spread_tsy_5yr<-last_update_dgs-rep(tsy_5yr_clean$V6[1],length(last_update_dgs))
  spread_tsy_10yr<-last_update_dgs-rep(tsy_10yr_clean$V6[1],length(last_update_dgs))
  spread_tsy_30yr<-last_update_dgs-rep(tsy_30yr_clean$V6[1],length(last_update_dgs))
  
  for(i in (nrow(tsy_5yr_clean)-1)){
    upd_with_5yr<-spread_tsy_5yr+rep(tsy_5yr_clean$V6[1+i],length(last_update_dgs))
    upd_with_10yr<-spread_tsy_10yr+rep(tsy_10yr_clean$V6[1+i],length(last_update_dgs))
    upd_with_30yr<-spread_tsy_30yr+rep(tsy_30yr_clean$V6[1+i],length(last_update_dgs))
    avg_update<-(upd_with_5yr+upd_with_10yr+upd_with_30yr)/3
    dgs_df_clean<-rbind(dgs_df_clean,data.frame(Date=tsy_5yr_clean$Date[1+i],avg_update))
  }
}

#end:data clean

#run mst and mst ratio
usty_df<-dgs_df_clean
usty_df<-usty_df %>%
  mutate(spread_DGS1_2=DGS2-DGS1,
         spread_DGS1_3=DGS3-DGS1,
         spread_DGS1_5=DGS5-DGS1,
         spread_DGS1_7=DGS7-DGS1,
         spread_DGS1_10=DGS10-DGS1,
         spread_DGS1_20=DGS20-DGS1,
         spread_DGS2_3=DGS3-DGS2,
         spread_DGS2_5=DGS5-DGS2,
         spread_DGS2_7=DGS7-DGS2,
         spread_DGS2_10=DGS10-DGS2,
         spread_DGS2_20=DGS20-DGS2,
         spread_DGS3_5=DGS5-DGS3,
         spread_DGS3_7=DGS7-DGS3,
         spread_DGS3_10=DGS10-DGS3,
         spread_DGS3_20=DGS20-DGS3,
         spread_DGS5_7=DGS7-DGS5,
         spread_DGS5_10=DGS10-DGS5,
         spread_DGS5_20=DGS20-DGS5,
         spread_DGS7_10=DGS10-DGS7,
         spread_DGS7_20=DGS20-DGS7,
         spread_DGS10_20=DGS20-DGS10)



##########################################
#Kendall Tau Matrix for Rates
###########################################
inversionNumber <- function(x){
  mergeSort <- function(x){
    if(length(x) == 1){
      inv <- 0
      #printind(' base case')
    } else {
      n <- length(x)
      n1 <- ceiling(n/2)
      n2 <- n-n1
      y1 <- mergeSort(x[1:n1])
      y2 <- mergeSort(x[n1+1:n2])
      inv <- y1$inversions + y2$inversions
      x1 <- y1$sortedVector
      x2 <- y2$sortedVector
      i1 <- 1
      i2 <- 1
      while(i1+i2 <= n1+n2+1){
        if(i2 > n2 || (i1 <= n1 && x1[i1] <= x2[i2])){ # ***
          x[i1+i2-1] <- x1[i1]
          i1 <- i1 + 1
        } else {
          inv <- inv + n1 + 1 - i1
          x[i1+i2-1] <- x2[i2]
          i2 <- i2 + 1
        }
      }
    }
    return (list(inversions=inv,sortedVector=x))
  }
  r <- mergeSort(x)
  return (r$inversions)
}

kendallTauDistance <- function(x,y){
  return(inversionNumber(order(x)[rank(y)]))
}


#######################################
print("building trees")
#mst_list<-list()
no_cores <- 5
cl <- makeCluster(no_cores)
registerDoParallel(cl)

mst_list<-foreach(.packages = c("igraph","amap"),i = 1:(nrow(usty_df)-tree_size+1)) %dopar% {
  usty_temp1<-usty_df[i:(i+tree_size-1),]
  usty_mx1<-as.matrix(usty_temp1[,2:8])
  usty_dist1<-Dist(usty_mx1,method="kendall",diag=T,upper=T)
  usty_mx2<-as.matrix(usty_temp1[,9:ncol(usty_df)])
  usty_dist2<-Dist(usty_mx2,method="kendall",diag=T,upper=T)
  cor_df<-cor(t(usty_temp1[,2:ncol(usty_df)]),method = "kendall")
  distM<-sqrt(2*(1-cor_df))
  dist_mx<-weight1*usty_dist1+weight2*usty_dist2+weight3*distM
  
  tmp_weightedGraph<- graph_from_adjacency_matrix(dist_mx, weighted=TRUE, diag=FALSE, mode="undirected");
  tmp_weightedGraph_MST<- mst(tmp_weightedGraph, algorithm = "prim")
  tmp_weightedGraph_MST
}#2min
stopCluster(cl)
#sum(E(tmp_weightedGraph_MST)$weight)
####################################################
#MST Tree
####################################################
print("tree length")
sum_mst_len<-function(mst_graph){
  sum(E(mst_graph)$weight)
}
mst_length<-unlist(lapply(mst_list, sum_mst_len))
###############################################
##################################################
#multistep survival ratio
##################################################
multi_steps<-4
multi_step_size<-1
num_edge<-tree_size-1
multi_survival<-c()
print("tree structure change")
for(i in 1:(length(mst_list)-(multi_steps-1)*multi_step_size)){
  weightedGraph_MST1<-mst_list[[i]]
  for(j in 1:(multi_steps-1)){
    weightedGraph_MST2 <- mst_list[[i+j*multi_step_size]]
    weightedGraph_MST1<-intersection(weightedGraph_MST1,weightedGraph_MST2)
  }
  
  multi_step_survival_count<-length(E(weightedGraph_MST1))
  multi_survival<-c(multi_survival,multi_step_survival_count/num_edge)
} #1-2min

str_change_ratio_multi<-1-multi_survival
#########################################################
#store the output & visualization
result_df<-usty_df
result_df["mst_length"]<-c(rep(NA,nrow(usty_df)-length(mst_length)),mst_length)
result_df["mst_str_change_ratio"]<-c(rep(NA,nrow(usty_df)-length(str_change_ratio_multi)),str_change_ratio_multi)
result_df_clean<-na.omit(result_df)
write.csv(result_df,"~/Desktop/ShinyApps/int_ind/output.csv")

plot(mst_length)
plot(str_change_ratio_multi)
##########################################################
