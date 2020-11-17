require(dplyr)
scoreModels_culture <- function(test_vars, df, culture_key){
  sdf <- culture_key %>% 
    left_join(model_support(test_vars, value=1, df), by='culture_id') %>% 
    left_join(model_support(test_vars, value=0, df), by='culture_id') %>% 
    left_join(model_support(test_vars, value=-1, df), by='culture_id')
  colnames(sdf) <- c('culture', 'culture_id', 'for', 'absence', 'against')
  return(sdf)
}
