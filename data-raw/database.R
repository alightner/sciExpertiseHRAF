library(data.table)
#textDB <- data.table(read_csv('raw-text-data.csv'))
# textDB$ocm <- NULL
# save(textDB, file='textDB.RData')
text_data <- read_delim('raw-text-data.tsv', delim=" ")
