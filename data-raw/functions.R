# function script for project ---------------------------------------------

stripMultiples <- function(x)
{
  y <- NULL
  for(i in 1:length(x)){
    tmp <- unlist(strsplit(x[i], ', '))
    y <- c(y,tmp)
  }
  return(y)
}

reshapeDF <- function(x, df)
{
  # x='colname', df=df
  x1 <- df[[x]]
  colvec <- stripMultiples(x1)
  idvec <- NULL
  for(i in 1:nrow(df)){
    y <- unlist(strsplit(df[[x]][i], ', '))
    idvec <- c(idvec, rep(df$textid[i], length(y)))
  }
  outab <- data.frame(idvec, colvec, stringsAsFactors=FALSE)
  colnames(outab) <- c('id', x)
  return(outab)
}

model_support <- function(test_vars, value, df){
  support <- by(df[,test_vars], df$culture_id, FUN=function(x) sum(x==value, na.rm=TRUE))
  mod_df <- data.frame(culture_id=names(support), instances=as.numeric(support))
  return(mod_df)
}

model_totals <- function(test_vars, df, culture_key){
  sdf <- culture_key %>% 
    left_join(model_support(test_vars, value=1, df), by='culture_id') %>% 
    left_join(model_support(test_vars, value=0, df), by='culture_id') %>% 
    left_join(model_support(test_vars, value=-1, df), by='culture_id')
  colnames(sdf) <- c('culture', 'culture_id', 'for', 'absence', 'against')
  return(sdf)
}

sccs_pull <- function(sccs_var, var_name, scdf, culture_codes)
{
  d2 <- as_tibble(scdf[scdf$var_id==sccs_var,]) 
  d2 <- d2[d2$soc_id %in% culture_codes$sccs_id,] %>% 
    dplyr::select(
      sccs_id=soc_id,
      #sccs_year=year,
      code    # if a function is written, be sure colnames are accounted for
    ) %>% 
    left_join(culture_codes, by='sccs_id')
  colnames(d2) <- c('sccs_id', var_name, 'culture_id')
  d2$sccs_id <- NULL
  return(d2)
  #culture_df <- culture_df %>% left_join(d2, by='culture_id')
}
