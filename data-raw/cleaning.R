library(tidyverse)
library(tidytext)
library(stringr)
library(textstem)
library(readxl)
library(devtools)
rm(list=ls())
source('functions.R')

# reading main data -------------------------------------------------------
df <- read_csv('coded-dataset.csv')
text_data <- read_delim('raw-text-data.tsv', delim=" ")

# culture data ------------------------------------------------------------

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

# citation data -----------------------------------------------------------

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

# authorship --------------------------------------------------------------

authsplit <- stringr::str_split(cite_df$citation, regex('[:digit:]{4}'))
authnames <- unlist(lapply(authsplit, function(x) x[1]))
authnames[240] <- "LeBar, Frank M."
authnames[206] <- "Marshall, Donald Stanley. "
authnames[187] <- "Textor, Robert B."
authnames[181] <- "Hauck, Hazel Marie"
authnames[163] <- "Ames, David W."
authnames[158] <- "Smith, M. G. (Michael Garfield)"
authnames[76] <- "Marshall, Donald Stanley. "
authnames[74] <- "Kemp, P. "
authnames[58] <- "Kemp, Jeremy"
authnames[33] <- "Akiga, Rupert East"
cite_df$authors <- authnames

cite_df$author_id <- NA
ind <- 1
for(i in unique(cite_df$authors)){
  cite_df$author_id[cite_df$authors==i] <- paste0(
    'a', paste0(rep('0',4-nchar(ind)), collapse=''), as.character(ind))
  ind <- ind+1
}

# splitting up the main df into shapes ------------------------------------
# reshaping multiple character vectors into unique data tables

domain_data <- reshapeDF('domain', df)
age_data <- reshapeDF('age', df)
learning_data <- reshapeDF('how_learned', df)
patron_data <- reshapeDF('patronage_based_cat', df)
status_data <- reshapeDF('how_gained_status', df)
prescribed_data <- reshapeDF('prescribed_behavior_cat', df)

# main numeric vars in main dataframe -------------------------------------
data <- df %>%
  dplyr::select(
    -domain,
    -age,
    -how_learned,
    -patronage_based_cat,
    -how_gained_status,
    -prescribed_behavior_cat
  )

# adding vars to culture df -----------------------------------------------

vars <- read_csv('sccs/variables.csv')
codes <- read_csv('sccs/codes.csv')
socdf <- read_csv('sccs/societies.csv')
scdf <- read.csv('sccs/data.csv',
               colClasses=c(
                 'character','character','numeric','character','numeric', rep('NULL',4)),
               header=TRUE)
culture_codes <- read.delim("Culture codes.txt", stringsAsFactors=FALSE)
culture_codes <- culture_codes[complete.cases(culture_codes),]
culture_codes$SCCS <- paste0('SCCS', as.character(culture_codes$SCCS))
colnames(culture_codes) <- c('culture_id', 'sccs_id')

## society key
# sccs_key <- as_tibble(data.frame(culture=socdf$pref_name_for_society,
#                                  sccs_id=socdf$id,
#                                  stringsAsFactors=FALSE))
cdvar <-
  vars %>%
  dplyr::select(
    var_id=id,
    category,
    title,
    type
  )
scdf <- scdf %>% left_join(cdvar, by='var_id')

culture_df <- culture_df %>% 
  left_join(
    sccs_pull(sccs_var='SCCS872', var_name='repro_skew', scdf, culture_codes),
    by='culture_id'
  )

# iterating through added SCCS vars ---------------------------------------

var_list <- read_csv('SCCS-addons.csv')
var_list <- var_list$codes

var_name_list <- c(
  'pathogen_stress',
  'intercommunity_trade_food',
  'subsistence_change',
  'writing_records',
  'fixity_residence',
  'urbanization',
  'technology_specialization',
  'pop_density',
  'political_integration',
  'social_stratification',
  'mechanical_transport',
  'water_transport',
  'roads_highways',
  'change_education',
  'foreign_med_practices',
  'foreign_disease_concepts',
  'change_traditional_religion',
  'foreign_religion',
  'change_burial_rituals',
  'food_storage',
  'food_storage_surplus',
  'inheritance_rule_land',
  'inheritance_rule_movable',
  'deep_islam_christian',
  'log10_popsize',
  'avg_popsize',
  'evil_eye_belief',
  'evil_eye_rating',
  'fear_ghosts',
  'fear_ghosts_rating',
  'christian_influence',
  'world_religion',
  'societal_rigidity',
  'high_gods',
  'ritual_war',
  'leadership_wealth',
  'theory_infection',
  'theory_stress',
  'theory_deterioration',
  'theory_accident',
  'theory_fate',
  'theory_ominous_feel',
  'theory_contagion',
  'theory_mystic_retribution',
  'theory_soul_loss',
  'theory_spirit_aggression',
  'theory_sorcery',
  'theory_witchcraft',
  'shaman',
  'shaman_healer',
  'healer',
  'medium',
  'sorcerer_witch',
  'priest'
)

for(i in 1:length(var_list)){
  culture_df <- culture_df %>% 
    left_join(
      sccs_pull(sccs_var=var_list[i], var_name=var_name_list[i], scdf, culture_codes),
      by='culture_id'
    )
}

# computing model scores for culture level --------------------------------
## For details on model definitions, see the Lightner et al. (2021) paper 
## in the alightner/conceptualExpertsHRAF repo

culture_key <- data.frame(culture=culture_df$culture,
                          culture_id=culture_df$culture_id,
                          stringsAsFactors=FALSE)

# document term matrix ----------------------------------------------------

expert_words <- 
  text_data %>% 
  dplyr::select(textid, paragraph) %>% 
  mutate(
    paragraph=iconv(paragraph, "", "UTF-8"),
    paragraph=str_replace(paragraph, "’", "'")
  ) %>% 
  unnest_tokens(word, paragraph) %>% 
  mutate(
    word=str_to_lower(word),
    word=str_replace(word, "'s", "")
  ) %>% 
  dplyr::filter(!str_detect(word, "\\d+")) %>% 
  dplyr::filter(!str_detect(word, "page")) %>% 
  #dplyr::filter(!str_detect(word, "AL")) %>% 
  dplyr::filter(!str_detect(word, "unknown")) %>% 
  dplyr::filter(!str_detect(word, "unavailable")) %>% 
  anti_join(stop_words) %>% 
  dplyr::filter(str_count(word) >2) %>% 
  mutate(lemma=lemmatize_words(word))

# two-letter word combinations removed
# twowords <- expert_words %>% filter(str_count(word) <= 2)  
# sort(table(twowords$word))
# ax bk br da dà ed en ff fo ib il iv ke ku ky le mj ni oo pi po pó 
# 1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 
# se sp ss tb tc tu wb wi wu xi xv xx ah ai ca ch fa fu gu hu kw mu 
# 1  1  1  1  1  1  1  1  1  1  1  1  2  2  2  2  2  2  2  2  2  2 
# nt pl pt su vi yi de ge ha ph st ti ya zä ad dr lu ne wa ba bi dï 
# 2  2  2  2  2  2  3  3  3  3  3  3  3  3  4  4  4  4  4  5  5  5 
# ms na  á hû mi cf ia ma si ta te la mo al ii sh pp li ki pa sa 
# 5  5  6  6  6  7  7  7  7  7  7  8  9 10 10 10 11 12 14 15 22 

texrec_length <- expert_words %>% 
  group_by(textid) %>% 
  summarise(
    numwords=length(word)
  )

docterm <-
  expert_words %>%
  dplyr::select(textid, lemma) %>% 
  group_by(textid, lemma) %>% 
  summarise(freq=n()) %>% 
  spread(lemma, freq, fill=0) %>% 
  ungroup %>% 
  dplyr::select(
    -(`10`:a.n)
  )

# split main df to numeric here -------------------------------------------

coded_data <- df
df$male <- 0
df$male[df$sex=='male' | df$sex=='either/both'] <- 1
df$female <- 0
df$female[df$sex=='female' | df$sex=='either/both'] <- 1

df$case <- 0
df$model <- 0
df$case[df$case_model=='case'] <- 1
df$model[df$case_model=='model'] <- 1

# starting iterations on cats to numeric ----------------------------------

# Age variables to numeric cols
agelist <- names(table(age_data$age))
names(agelist) <- agelist
names(agelist)[names(agelist)=='older/middle-aged'] <- 'middleaged'
names(agelist) <- paste0("age_",names(agelist))

# Patronage basis to numeric cols
patronlist <- names(table(patron_data$patronage_based_cat))
names(patronlist) <- patronlist
names(patronlist) <- unlist(lapply(strsplit(names(patronlist), split=' '), 
                                   function(x) paste0(x, collapse='_')))
names(patronlist) <- paste0('patron_', names(patronlist))

# Prescribes behavior
prescribelist <- names(table(prescribed_data$prescribed_behavior_cat))
names(prescribelist) <- prescribelist
names(prescribelist) <- unlist(lapply(strsplit(names(prescribelist), split=c(' ')), 
                                      function(x) paste0(x, collapse='_')))
names(prescribelist) <- unlist(lapply(strsplit(names(prescribelist), split=c('/')), 
                                      function(x) paste0(x, collapse='_')))
names(prescribelist)[names(prescribelist)=='auspicious_behavior'] <- 'auspicious'
names(prescribelist)[names(prescribelist)=='sharing_behavior'] <- 'sharing'
names(prescribelist) <- paste0('prescribe_', names(prescribelist))

# How learned domain of expertise
learnlist <- names(table(learning_data$how_learned))
names(learnlist) <- unlist(lapply(
  strsplit(learnlist, split=' '), function(x){
    if("(misc.)" %in% x) x <- x[1]
    paste0(x, collapse='_')
  }
))
names(learnlist) <- unlist(lapply(
  strsplit(names(learnlist), split='/'), function(x) paste0(x, collapse='_')
))
names(learnlist) <- paste0('learned_', names(learnlist))

# How experts gained their status as expert
statlist <- names(table(status_data$how_gained_status))
names(statlist) <- unlist(lapply(
  strsplit(statlist, split=' '), function(x) paste0(x, collapse='_')
))
names(statlist) <- unlist(lapply(
  strsplit(names(statlist), split='/'), function(x) paste0(x, collapse='_')
))
names(statlist) <- paste0('status_', names(statlist))
# prescribed, patronage basis, learned, status

# Age iteration
tmp2 <- data.frame(matrix(NA, nrow=nrow(df), ncol=length(agelist)))
for(i in agelist){
  tmp2[which(agelist==i)] <- convertCatNumeric(cat_name=i, cat_var='age', cat_data=age_data, df)
}
colnames(tmp2) <- names(agelist)
df <- cbind(df, tmp2)


# Patronage basis to numeric cols
tmp2 <- data.frame(matrix(NA, nrow=nrow(df), ncol=length(patronlist)))
for(i in patronlist){
  tmp2[which(patronlist==i)] <- convertCatNumeric(cat_name=i, cat_var='patronage_based_cat', cat_data=patron_data, df)
}
colnames(tmp2) <- names(patronlist)
df <- cbind(df, tmp2)


# Prescribes behavior
tmp2 <- data.frame(matrix(NA, nrow=nrow(df), ncol=length(prescribelist)))
for(i in prescribelist){
  tmp2[which(prescribelist==i)] <- convertCatNumeric(cat_name=i, cat_var='prescribed_behavior_cat', cat_data=prescribed_data, df)
}
colnames(tmp2) <- names(prescribelist)
df <- cbind(df, tmp2)


# How learned domain of expertise
tmp2 <- data.frame(matrix(NA, nrow=nrow(df), ncol=length(learnlist)))
for(i in learnlist){
  tmp2[which(learnlist==i)] <- convertCatNumeric(cat_name=i, cat_var='how_learned', cat_data=learning_data, df)
}
colnames(tmp2) <- names(learnlist)
df <- cbind(df, tmp2)


# How experts gained their status as expert
tmp2 <- data.frame(matrix(NA, nrow=nrow(df), ncol=length(statlist)))
for(i in statlist){
  tmp2[which(statlist==i)] <- convertCatNumeric(cat_name=i, cat_var='how_gained_status', cat_data=status_data, df)
}
colnames(tmp2) <- names(statlist)
df <- cbind(df, tmp2)

# Converting disability categories to numeric
df$disabled_blind <- 0
df$disabled_crippled <- 0
df$disabled_blind[!is.na(df$disability)] <- 1
df$disabled_crippled[df$disability=='blind, crippled'] <- 1

# constructing key dataframe ----------------------------------------------

central_key <- data.frame(
  textid=df$textid,
  culture_id=df$culture_id,
  citation_id=df$citation_id
)

central_key <- central_key %>% left_join(culture_key, by='culture_id')
ac_key <- cite_df %>% dplyr::select(citation_id, author_id)
central_key <- central_key %>% left_join(ac_key, by='citation_id')

# cleaning up usethis objects ---------------------------------------------

coded_data <- coded_data %>% 
  dplyr::select(
    textid,
    acculturation:signs_mental_illness
  )

coded_categorical <- df %>% 
  dplyr::select(
    textid,
    male:disabled_crippled
  )

text_data$culture_id <- NULL
text_data$citation_id <- NULL

cite_df$author_id <- NULL

# overwrite with usethis --------------------------------------------------

cite_data <- cite_df
culture_data <- culture_df

usethis::use_data(central_key, overwrite=TRUE)
usethis::use_data(coded_data, overwrite=TRUE)
usethis::use_data(coded_categorical, overwrite=TRUE)
usethis::use_data(cite_data, overwrite=TRUE)
usethis::use_data(culture_data, overwrite=TRUE)
usethis::use_data(docterm, overwrite=TRUE)
usethis::use_data(domain_data, overwrite=TRUE)
usethis::use_data(expert_words, overwrite=TRUE)
usethis::use_data(texrec_length, overwrite=TRUE)
usethis::use_data(text_data, overwrite=TRUE)

