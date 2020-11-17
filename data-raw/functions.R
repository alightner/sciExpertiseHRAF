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

