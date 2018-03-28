
# Divide a Dataframe into Cross Validation Subsets
trn_tst = function(df, test_perc, seed=123){
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load("dplyr", "magrittr", "lubridate")
  set.seed(seed)
  n = sort(sample(nrow(df), nrow(df) * test_perc))
  df = rbind.data.frame(df[-n,] %>% mutate(subset = "train"),
                        df[n,] %>% mutate(subset = "test"))
}

