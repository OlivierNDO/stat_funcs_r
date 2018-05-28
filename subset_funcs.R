# Load Packages
#######################################################################################################
req_pkgs = c("boot", "doParallel", "FNN", "ISLR", "MASS", "tidyverse")
lapply(req_pkgs, require, character.only = TRUE)

# Parallel Processing
#######################################################################################################
cl = makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)

# Data
#######################################################################################################
college = College %>% na.omit()

# 1. Identify all possible combinations of predictors (p) & fit models, capturing goodness of fit
#######################################################################################################
get_combs = function(df, dv){
  ivs = df[,colnames(df) != dv]
  temp_list = list()
  for(i in 1:ncol(ivs)){
    temp_list[[i]] = combn(colnames(ivs),i, simplify = FALSE) 
  }
  return(temp_list %>% unique() %>% flatten())
}

# Kfold Subsetting Function
#######################################################################################################
tidy_kfold = function(dat, k, seed = 12345){
  set.seed(seed)
  nr = nrow(dat)
  dat = dat %>% mutate(kf_num = sample(rep(1:k, ceiling(nr/k)), nr))
  tmplist = list()
  for(i in 1:k){
    tmplist[[i]] = dat %>% mutate(trn_tst = ifelse(kf_num == i, "test", "train"))
  }
  kfold_subset_list = do.call(rbind.data.frame, tmplist)
  return(tmplist)
}

initialsubsets = tidy_kfold(college, 20)
initial_len = length(initialsubsets)
mysubsets = initialsubsets %>% rep(length(myfits))
myfits = get_combs(college, "Grad.Rate") %>% rep(initial_len)
rm(initialsubsets)

subdata1 = mysubsets[[1]]
subcalls1 = myfits[[1]]

# 2. Fit models for every combination, capturing goodness of fit measures (AIC, BIC, R2, MSE, CV_MSE)
#######################################################################################################
fit_comb = function(df, dv, iv_list, seed = 12345){
  set.seed(seed)
  train = df %>% filter(trn_tst == "train") %>% dplyr::select(-trn_tst)
  test = df %>% filter(trn_tst == "test") %>% dplyr::select(-trn_tst)
  call = paste(dv, "~", paste(iv_list %>% unlist(), collapse=" + "))
  fit = lm(call, data = train)
  pred = predict(fit, newdata = test)
  mse = mean((pred - test[,dv])^2)
  return(mse)
}

tryit = fit_comb(subdata1, "Grad.Rate", subcalls1)

tryit = mapply(fit_comb, df = subdata1, dv = "Grad.Rate", iv_list = subcalls1)

output_combs = fit_combs(auto, "mpg", .3)
