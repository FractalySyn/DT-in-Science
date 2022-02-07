rm(list = ls())
required_libs = c('tidyverse')
load_pack = function(lib = required_libs)
{
   if(!require(lib, character.only = TRUE)) install.packages(lib)
   library(lib, character.only = TRUE)
}
lapply(required_libs, load_pack); rm(required_libs)


# *** --------------------------------------------------------------------------------------

# sorted authors
science_f = list.files('data/counts/author', pattern = "papers_.*.csv")
sf = str_match(science_f, "_AU_\\d+_(\\w+).csv")[,2] %>%
   str_replace_all("_", " ")

l = list()
for(i in 1:length(science_f)){
   l[[sf[i]]] = read_csv(paste0('data/counts/author/', science_f[i]))
}
authors = sapply(l, function(d) d$citations %>% sum) %>% 
   sort(decreasing = T)  %>% names()
paste0("'", paste(authors, collapse = "', '"), "'")


# sorted inventors
industry_f = list.files('data/counts/author', pattern = "patents_.*.csv")
pf = str_match(industry_f, "_inventor_\\d+_(\\w+).csv")[,2] %>%
   str_replace_all("_", " ")

l = list()
for(i in 1:length(industry_f)){
   l[[pf[i]]] = read_csv(paste0('data/counts/author/', industry_f[i]))
}
inventors = sapply(l, function(d) d$count %>% sum) %>% 
   sort(decreasing = T)  %>% names()
paste0("'", paste(inventors, collapse = "', '"), "'")



# sorted publishers
science_f = list.files('data/counts/publisher', pattern = "papers_.*.csv")
sf = str_match(science_f, "_publisher_\\d+_(\\w+).csv")[,2] %>%
   str_replace_all("_", " ")

l = list()
for(i in 1:length(science_f)){
   l[[sf[i]]] = read_csv(paste0('data/counts/publisher/', science_f[i]))
}
publishers = sapply(l, function(d) d$citations %>% sum) %>% 
   sort(decreasing = T)  %>% names()
paste0("'", paste(publishers, collapse = "', '"), "'")



# sorted topics
data = read_csv("data/counts/papers_RA.csv") %>%
   filter(year <= 2019) %>%
   mutate(year = as.integer(year))

dtopics = data %>% group_by(Topic) %>% 
   summarise(n = sum(citations)) %>%
   arrange(desc(n)) %>%
   head(60) %>%
   pull(Topic)
paste0("'", paste(dtopics, collapse = "', '"), "'")

stopics = data %>% group_by(ScienceT) %>% 
   summarise(n = sum(citations)) %>%
   arrange(desc(n)) %>%
   head(60) %>%
   pull(ScienceT)
paste0("'", paste(stopics, collapse = "', '"), "'")












