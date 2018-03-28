##### Classification Methods - LDA vs. QDA - Evaluate in Cross Validatiopn #####

# Bayes' Theorem: Theoretical Best Classif. Prob. Estimate
#       - p(X) = Pr(Y=k | X=k) = (piK * Fk(x)) /  SUMMATION(piK * Fk(x))
#       - where > piK = prior prob. of comming from class k
#               > Fk(X) = density function for X that X is obs. from class K


# LDA: D = v1*X1 + V2*X2 ... Vi*Xi + e
#       - where X ~ indep. var, v is coef, D is discr. function
#       - v's are set to maximize mean difference in D between classes
#       - assumptions: obs. are random sample, IVs are normally distributed, All Ks have some var/covar
#       - why LDA?: > logistic unstable when classes well separated,
#                   > when n small & IVs are normal, LDA more stable than logistic
#                   > LDA more popular when num. response classes > 2

# QDA: same as LDA except estimates separate var/covar for each K
#       - works better if var. between classes are very different
#       - not as good if n is too small to acc. estimate variances

kfold_lqda = function(my_data, my_dv, my_ivs, k, seed = 123){
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load("dplyr", "magrittr", "MASS")
  set.seed(seed)
  k_vec = head(sample(rep(1:k, ceiling(nrow(my_data) / k))), nrow(my_data))
  new_data = my_data[,c(my_dv, my_ivs)] %>% mutate(k_iter = k_vec)
  lda_temp_list = list() ; qda_temp_list = list()
  
  for(i in 1:k){
    train = new_data[new_data$k_iter != i,] # Train & Test Subsetting by Fold
    test = new_data[new_data$k_iter == i,]
    
    train_x = model.matrix(reformulate(".", my_dv), train) %>%
      data.frame() %>%
      dplyr::select(-X.Intercept.)
    
    train_y = data.frame(y = train[[my_dv]])
    train_df = cbind.data.frame(train_y, train_x)
    
    test_x = model.matrix(reformulate(".", my_dv), test) %>%
      data.frame() %>%
      dplyr::select(-X.Intercept.)
    
    test_y = data.frame(y = test[[my_dv]])
    test_df = cbind.data.frame(test_y, test_x)
    
    # Fit and Predict LDA Model
    lda_fit = lda(y ~. , data = train_df)
    lda_pred = predict(lda_fit, newdata = test_x)
    lda_acc = mean(lda_pred$class == test_y$y)
    lda_temp_list[[i]] = lda_acc
    
    # Fit and Predict QDA Model
    qda_fit = qda(y ~. , data = train_df)
    qda_pred = predict(qda_fit, newdata = test_x)
    qda_acc = mean(qda_pred$class == test_y$y)
    qda_temp_list[[i]] = qda_acc
  }
  lda_accuracy = do.call(rbind, lda_temp_list)
  qda_accuracy = do.call(rbind, qda_temp_list)
  cat(paste0("LDA Mean Accuracy: ", mean(lda_accuracy), "   |   ",
             "QDA Mean Accuracy: ", mean(qda_accuracy)))
}