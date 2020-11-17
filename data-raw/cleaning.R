library(tidyverse)
library(readxl)
library(devtools)
rm(list=ls())
source('functions.R')

df <- read_csv('AL-coded-dataset.csv')
text_data <- read_delim('raw-text-data.tsv', delim=" ")

culture_df <- read_csv('culture_table.csv')
colnames(culture_df) <- c(
  'cultural_complexity',
  'culture_id',
  'latitude',
  'longitude',
  'culture',
  'region',
  'subregion',
  'subsistence_id',
  'subsistence_type'
)
culture_df <- culture_df %>% 
  dplyr::select(
    culture_id,
    everything()
  )
culture_df$culture[culture_df$culture=='Taiwain Hokkien'] <- 'Taiwan Hokkien'

cite_df <- read_csv('citation_data.csv')
years <- rep(NA, length(cite_df$citation))
for(i in 1:length(years)){
  years[i] <- as.numeric(unlist(str_extract_all(cite_df$citation[i], regex('[:digit:]{4}')))[1])
}
decades <- rep(NA, length(years))
for(i in 1:length(years)){
  if(!is.na(years[i])){
    decades[i] <- as.numeric(paste0(substr(as.character(years[i]),1,3),'0'))
  }
}
cite_df$year <- years
cite_df$decade <- decades


# splitting up the main df into shapes ------------------------------------



# overwrite with usethis --------------------------------------------------

# age_data.Rdata')
# cite_data.Rdata')
# culture_data.Rdata')
# data.Rdata')
# domain_data.Rdata')
# learning_data.Rdata')
# status_data.Rdata')
# patron_data.Rdata')
# text_data.Rdata')

data <- df
cite_data <- cite_df
culture_data <- culture_df

usethis::use_data(cite_data, overwrite=TRUE)
usethis::use_data(culture_data, overwrite=TRUE)
usethis::use_data(data, overwrite=TRUE)
usethis::use_data(text_data, overwrite=TRUE)


usethis::use_data(age_data)
usethis::use_data(domain_data)
usethis::use_data(learning_data)
usethis::use_data(status_data)
usethis::use_data(patron_data)

