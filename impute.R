impute <- function(df, df1){
  num <- nrow(df1)
  for (i in 1:num){
    val <- round(rnorm(1, df1$avg[i], df1$stdev[i]))
    if (val < 0){
      val <- 0
    } 
    df$steps[which(df$interval == df1$interval[i])] <- val
  }
  df
}