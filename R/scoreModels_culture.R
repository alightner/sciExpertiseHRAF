require(dplyr)
model_support <- function(test_vars, value, df){
  support <- by(df[,test_vars], df$culture_id, FUN=function(x) sum(x==value, na.rm=TRUE))
  mod_df <- data.frame(culture_id=names(support), instances=as.numeric(support))
  return(mod_df)
}
scoreModels_culture <- function(test_vars, df, culture_key){
  sdf <- culture_key %>% 
    left_join(model_support(test_vars, value=1, df), by='culture_id') %>% 
    left_join(model_support(test_vars, value=0, df), by='culture_id') %>% 
    left_join(model_support(test_vars, value=-1, df), by='culture_id')
  colnames(sdf) <- c('culture', 'culture_id', 'for', 'absence', 'against')
  return(sdf)
}
