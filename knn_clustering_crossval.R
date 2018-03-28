### K-Fold CV for KNN (for 1 number of K)
#     > completely non-parametric (i.e. no assumptions about shape of decision boundary)
#     > will work better than LDA & logistic when decision boundary is highly non-linear
#     > disadvantage: does not tell us which predictors are important

knn_cv = function(dat, dv, ivs, kn, k, seed = 123){
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load("dplyr", "magrittr", "MASS", "FNN")
  set.seed(seed)
  dat = dat %>% mutate(k_iter = sample(head(rep(1:k, ceiling(nrow(dat) / k)), nrow(dat))))
  dat[,ivs] = scale(dat[,ivs])
  knn_list = list()
  for(i in 1:k){
    train_x = dat[dat$k_iter != i,ivs] ; train_y = dat[dat$k_iter != i,dv]
    test_x = dat[dat$k_iter == i,ivs]  ; test_y = dat[dat$k_iter == i,dv]
    knn_list[i] = mean(knn(train_x, test_x, train_y, k = kn) != test_y)
  }
  return(data.frame(Num_Neighb = kn, Mn_Error = mean(unlist(knn_list))))
}
knn_bestk = function(dat, dv, ivs, max_neigh, k, seed = 123){
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load("dplyr", "magrittr", "MASS", "FNN")
  set.seed(seed);   temp_list = list()
  for(i in 1:max_neigh){
    temp_list[[i]] = knn_cv(dat, dv, ivs, i, k, seed)
  }
  return(do.call(rbind.data.frame, temp_list))
}
