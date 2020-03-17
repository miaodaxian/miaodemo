#optimization draft
#specific rate, specific period
#input parameter(matrix weights,matrix size)
#objective(all the revese &trend labels vs indicator values -->kendall tau is small for both indicators)
opt_obj<-function(input_param){
  
  tree_size<-input_param[1]
  step_size<-input_param[2]
  weight1<-input_param[3]
  weight2<-input_param[4]
  weight3<-input_param[5]
  tree_size<-as.integer(tree_size)
  step_size<-as.integer(step_size)
  target_name<-"DGS1"
  target_range<-120
  ##########################################
  # Parameters
  ########################################
  # tree_size<-60
  # step_size<-30
  # weight1<-0.5
  # weight2<-0.5
  # weight3<-0.1
  #########################################
  ##########################################
  
  
  
  library(data.table)
  library(doParallel)
  library(dplyr)
  library(igraph)
  library(amap)
  library(lubridate)
  usty_df<-as.data.frame(fread("/home/k2uxam/Data/usty_rates.csv"))
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
    usty_dist2<-Dist(usty_mx1,method="kendall",diag=T,upper=T)
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
  #mst_length_df<-as.data.frame(mst_length)
  
  ##################################################
  #multistep survival ratio
  ##################################################
  multi_steps<-4
  multi_step_size<-1
  
  num_edge<-tree_size-1
  multi_survival<-c()
  print("tree structure change")
  for(i in 1:(length(mst_list)-tree_size-(multi_steps-1)*multi_step_size+1)){
    weightedGraph_MST1<-mst_list[[i]]
    for(j in 1:(multi_steps-1)){
      weightedGraph_MST2 <- mst_list[[i+j*multi_step_size]]
      weightedGraph_MST1<-intersection(weightedGraph_MST1,weightedGraph_MST2)
    }
    
    multi_step_survival_count<-length(E(weightedGraph_MST1))
    multi_survival<-c(multi_survival,multi_step_survival_count/num_edge)
  } #1-2min
  
  str_change_ratio_multi<-1-multi_survival
  ############################################
  #Two indicators
  #############################################
  #sum_mst_len #mst_df
  #str_change_ratio_multi #surv_ratio_df
  ###################################################
  ##################################################
  row_num<-min(nrow(usty_df),length(mst_length),length(str_change_ratio_multi))
  ind_df<-cbind(usty_df[1:row_num,],mst_length[1:row_num],str_change_ratio_multi[1:row_num])
  ind_df$DATES<-ymd(ind_df$date)
  target_df<-usty_df[,c("date",target_name)]
  target_df$target_start<-as.numeric(c(rep(NA,target_range),target_df[,target_name][1:(nrow(target_df)-target_range)]))
  target_df$target_end<-c(target_df[,target_name][(1+target_range):nrow(target_df)],rep(NA,target_range))
  target_df_clean<-na.omit(target_df)
  
  final_df<-as.data.frame(merge(ind_df,target_df_clean,by=c("date",target_name),all=FALSE))
  final_df$delta_change<-abs(final_df$target_end-final_df[,eval(target_name)])
  var_val<-var(final_df$delta_change)
  final_df$label1<- ifelse(final_df$delta_change>2*var_val,1.0,0.0)
  final_df$label2<-ifelse((final_df[,target_name]-final_df$target_start)*(final_df$target_end-final_df[,target_name])<0,1.0,0.0)
  final_df$label<-ifelse(final_df$label1+final_df$label2>=1,1.0,0.0)
  
  
  normFactor<-0.5*nrow(final_df)*(nrow(final_df)-1)
  mst_len_kdt<-kendallTauDistance(final_df$label,final_df$`mst_length[1:row_num]`)/normFactor
  mst_ratio_kdt<-kendallTauDistance(final_df$label,final_df$`str_change_ratio_multi[1:row_num]`)/normFactor
  c(mst_len_kdt,mst_ratio_kdt)
}
#setup the optimizations
library(nsga2R)
print("Start")
Sys.time()
opt_sol<-nsga2(opt_obj,5,2,generations = 20,popsize=60,
               lower.bounds=c(30,30,0,0,0),upper.bounds = c(200,200,1,1,1)) 
print("run ends")
saveRDS(opt_sol,file="/home/k2uxam/TDA/rateInd_v3/sol_dgs1_120.rds")
Sys.time()


# tree_size<-60
# step_size<-30
# weight1<-0.5
# weight2<-0.5
# weight3<-0.1