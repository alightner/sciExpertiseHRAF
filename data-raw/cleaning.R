library(tidyverse)
library(tidytext)
library(stringr)
library(textstem)
library(readxl)
library(devtools)
# setwd('data-raw')
rm(list=ls())
# source('database.R')
source('functions.R')

# reading main data -------------------------------------------------------
df <- read_csv('AL-coded-dataset.csv')
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
# stringr::str_split(authsplit, '-/')
authnames <- unlist(lapply(authsplit, function(x) x[1]))
authnames[240] <- "LeBar, Frank M."
authnames[206] <- "Marshall, Donald Stanley."
authnames[187] <- "Textor, Robert B."
authnames[181] <- "Hauck, Hazel Marie"
authnames[163] <- "Ames, David W."
authnames[158] <- "Smith, M. G. (Michael Garfield)"
authnames[76] <- "Marshall, Donald Stanley."
authnames[58] <- "Kemp, Jeremy"
authnames[33] <- "Akiga, Rupert East"

cite_df$authors <- authnames
  
# you left off here
stringr::str_split(authnames, ' and ')

# # add to citedata?
# first_author_lastname <- strsplit(cite_df$citation, split=',')
# #substr(cite_df$citation, start=1, stop=which regex('[:digit:]{4}'))
# strsplit(cite_df$citation, split='')

# splitting up the main df into shapes ------------------------------------
# reshaping multiple character vectors into unique data tables
# colnames(df)[sapply(df, function(x) !is.numeric(x))]
# [1] "textid"                  "culture_id"             
# [3] "citation_id"             "case_model"             
# [5] "sex"                     "disability"             
# [7] "domain"                  "age"                    
# [9] "how_learned"             "how_gained_status"      
# [11] "patronage_based_cat"     "prescribed_behavior_cat"

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

## reproductive skew (from dplace.org):
# Percentage of Married Women Polygynously Married 
# (Share Husband with One or More Co-wives) [SCCS872]
# White, D. R. (1986). Forms and frequencies of polygyny: Standard Sample codes. World Cultures 2(2).

# NOTE: extensible/revisions can be made with sub char vector for 872 var
# BUT a function would need to iterate lines below unless code is restructured
# 11/17 UPDATE: wrote the function, but testing it out. might still need to iterate over sccs vars

# function(sccs_var, var_name, scdf, culture_codes)
culture_df <- culture_df %>% 
  left_join(
    sccs_pull(sccs_var='SCCS872', var_name='repro_skew', scdf, culture_codes),
    by='culture_id'
  )

# d2 <- as_tibble(scdf[scdf$var_id=='SCCS872',]) 
# d2 <- d2[d2$soc_id %in% culture_codes$sccs_id,] %>% 
#   dplyr::select(
#     sccs_id=soc_id,
#     #sccs_year=year,
#     'repro_skew'=code    # if a function is written, be sure colnames are accounted for
#   ) %>% 
#   left_join(culture_codes, by='sccs_id')
# 
# culture_df <- culture_df %>% left_join(d2, by='culture_id')
# out of curiosity: culture_df %>% select(culture, repro_skew)

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

# names: names(var_list)
# keys: as.character(var_list)
# function(sccs_var, var_name, scdf, culture_codes)

for(i in 1:length(var_list)){
  culture_df <- culture_df %>% 
    left_join(
      sccs_pull(sccs_var=var_list[i], var_name=var_name_list[i], scdf, culture_codes),
      by='culture_id'
    )
}

# adding vars to culture df (alt approach) --------------------------------
# sccs$SOCNAME[sccs.nums]
# # NOTE: migrate after finishing in dplace data directory
# load('sccs.RData')
# sccs <- as.data.frame(sccs, stringsAsFactors=FALSE)
# # ID's of SCCS cultures that correspond to HRAF probability sample cultures
# sccs_nums <- c(19,37,79,172,28,7,140,76,121,109,124,24,87,12,69,181,26,51,149,85,112,125,16,94,138,116,158,57,102,4,34,182,13,127,52,62,165,48,42,36,113,152,100,16,132,98,167,154,21,120)
# ## adding variables SCCS variables
# sccs_vars <- c('SOCNAME','V61','V63', 'V64', 'V69','V70', 'V73', 'V76', 'V77',  'V79',
#                'V80', 'V81', 'V83', 'V84', 'V85', 'V93', 'V94', 'V156', 'V158', 'V158.1',
#                'V207', 'V208', 'V210', 'V234', 'V235', 'V236', 'V247', 'V270', 'V276',
#                'V573', 'V574', 'V575', 'V587', 'V666', 'V669', 'V670', 'V679',
#                'V756', 'V758', 'V759', 'V760', 'V761', 'V762', 'V763', 'V764', 'V765', 'V766', 'V767',
#                'V768', 'V769', 'V770', 'V773', 'V774', 'V775', 'V777', 'V778', 'V780', 'V785', 'V793', 'V794',
#                'V795', 'V796', 'V835', 'V836', 'V860', 'V866', 'V867', 'V868', 'V869', 'V872', 'V902', 'V903', 'V905',
#                'V907', 'V910', 'V1133', 'V1134', 'V1648', 'V1683', 'V1684', 'V1685')
# sccs2 <- sccs[sccs_nums, sccs_vars]
# culture_codes <- read.delim("Culture codes.txt", stringsAsFactors=FALSE)
# sccs2 <- merge(sccs2, culture_codes, by.x='row.names', by.y='SCCS')
# 
# # cultures <- read.delim("data-raw/cultures.txt")
# # cultures = merge(cultures, sccs.2, by='Culture.code', all=T)
# # rm(sccs.2)
# 
# ##d uses "c_culture_code", cultures uses "Culture.code" -- only two differences, e.g. two groups in PSF not in SCCS
# cultures$c_culture_code<-cultures$Culture.code
# 
# 
# ## Isolating variables for MA focus
# ## Prestige/Dominance variables, VV variables, Neel variables (*d3 is main extract level data frame
# ##ORIGINAL CODE#d3 = d[,c(1:12, 27:35, 51:54, 61:67, 79:80)]
# 
# #d3<-d[,c(1:12,27:35,68,69,66,67,74:80,57,58)]
# 
# 
# # Rows that are all zero on study variables == 1
# # a=0
# # for (i in 1:nrow(d3)){a[i] = 1*(sum(abs(d3[i,14:34]))==0)}
# # d3$all.zero = a
# # d4 = d3[d3$all.zero!=1,]
# 
# d$c_cultural_complexity <-as.numeric(d$c_cultural_complexity)
# 
# ## Recode 0 to -1, and -1 to 0
# tmp = as.matrix(d[,14:37])
# tmp[tmp==0] = -2
# tmp[tmp==-1] = 0
# tmp[tmp==-2] = -1
# d[,14:37] = tmp
# rm(tmp)
# 
# 
# ##Culture level data
# 
# # Try to replicate d.ct in R
# # PSF has Bahia Brazilians, but no leadership extracts from that culture, so delete that row
# 
# d.ct <-
#   read.csv('data-raw/culture_fmpro2.csv', stringsAsFactors=F) %>%
#   filter(c_name != 'Bahia Brazilians ') %>%  # Not in d
#   left_join(as.data.frame(table(d$c_name), stringsAsFactors = F), by=c('c_name'='Var1')) %>%
#   rename(extract_count = Freq)
# 
# 
# 

# computing model scores for culture level --------------------------------

culture_key <- data.frame(culture=culture_df$culture,
                          culture_id=culture_df$culture_id,
                          stringsAsFactors=FALSE)

# example use:
# test_vars <- c('confers_benefits',
#                'confers_benefit_kin',
#                'opaque',
#                'secretive_knowledge',
#                'uncommon_serious',
#                'patronage_based_efficacy',
#                'reputation_efficacy',
#                'experts_compete')
# value <- 1
# model_support(test_vars, value, df)  # single model
# model_totals <- function(test_vars, df, culture_key)   # full evidence for, absence, and against
# model_totals(test_vars, df, culture_key)

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
# avg_texrec <- mean(texrec_length$numwords)
# sd_texrec <- sd(texrec_length$numwords)
# median_texrec <- median(texrec_length$numwords)

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

# expanding numeric to explicitly code certain categoricals ---------------
# colnames(df)[sapply(df, is.character)]
## sex, age, etc.
df$male <- 0
df$male[df$sex=='male' | df$sex=='either/both'] <- 1
df$female <- 0
df$female[df$sex=='female' | df$sex=='either/both'] <- 1

df$case <- 0
df$model <- 0
df$case[df$case_model=='case'] <- 1
df$model[df$case_model=='model'] <- 1


# split main df to numeric here -------------------------------------------

# num_df <- df   # NOTE: split df to numeric version here?
# still undecided if this is the best approach
data <- df

# starting iterations on cats to numeric ----------------------------------

## First: setting up names to iterate over

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

## Second: iterating over those name lists

# Age iteration
#df$child <- convertCatNumeric(cat_name='child', cat_var='age', cat_data=age_data, df)
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


# overwrite with usethis --------------------------------------------------

long_data <- df
cite_data <- cite_df
culture_data <- culture_df

usethis::use_data(data, overwrite=TRUE)
usethis::use_data(long_data, overwrite=TRUE)
usethis::use_data(cite_data, overwrite=TRUE)
usethis::use_data(culture_data, overwrite=TRUE)
usethis::use_data(text_data, overwrite=TRUE)
usethis::use_data(age_data, overwrite=TRUE)
usethis::use_data(domain_data, overwrite=TRUE)
usethis::use_data(learning_data, overwrite=TRUE)
usethis::use_data(status_data, overwrite=TRUE)
usethis::use_data(patron_data, overwrite=TRUE)
usethis::use_data(prescribed_data, overwrite=TRUE)
usethis::use_data(culture_key, overwrite=TRUE)
usethis::use_data(docterm, overwrite=TRUE)
usethis::use_data(texrec_length, overwrite=TRUE)
usethis::use_data(expert_words, overwrite=TRUE)

