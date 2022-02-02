rm(list = ls())
required_libs = c('tidyverse', 'reticulate')
load_pack = function(lib = required_libs)
{
   if(!require(lib, character.only = TRUE)) install.packages(lib)
   library(lib, character.only = TRUE)
}
lapply(required_libs, load_pack); rm(required_libs)




# papers --------------------------------------------------------------------------------------


load("data_wos_updated.RData")
papers = data_wos_updated %>% as_tibble()
rm(data_wos_updated)

View(papers[1:30,])

# Keep relevant variables
papers = papers[, c('PY', 'AU', 'C1', 'TI', 'AB', 'SO', 'DE', 'ID', 'WC', 'SC', 'FU', 'NR', 'TC')] %>%
   # AU := authors count
   mutate(n_authors = str_count(AU, ";") + 1) %>%
   # C1 := affil count
   mutate(n_affils = str_count(C1, "\\[")) %>%
   # FU := funding count %>%
   mutate(n_grants = str_count(FU, ";")) %>%
   # reshape
   rename(year=PY, title=TI, abstract=AB, publisher=SO,  
          keywords_plus=ID, wos_categ=WC, research_areas=SC, 
          n_refs=NR, n_citations=TC, author_keywords=DE) %>%
   select(-C1, -FU)

# Deal with years
papers$year %>% is.na() %>% sum
papers = papers %>% mutate(year = as.numeric(year)) %>%
   filter(!(year %>% is.na()) & (year > 1900))
papers$year %>% hist

# NAs
papers %>% sapply(function(x){ is.na(x) %>% sum })
# numeric columns := NAs become 0s
papers = papers %>% mutate(n_refs = ifelse(is.na(n_refs), 0, n_refs)) %>% 
   mutate(n_citations = ifelse(is.na(n_citations), 0, n_citations)) %>%
   mutate(n_authors = ifelse(is.na(n_authors), 0, n_authors)) %>%
   mutate(n_affils = ifelse(is.na(n_affils), 0, n_affils)) %>%
   mutate(n_grants = ifelse(is.na(n_grants), 0, n_grants))
# For the rest, we want to remove papers with a few text info, let's consider 4+ NAs in a row as a criterion
papers = papers[!(papers %>% apply(1, function(x){ sum(is.na(x)) > 3 })),]


write.csv(papers, "papers.csv")
rm(papers)



# patents -------------------------------------------------------------------------------------

load('PATSTAT_EPO_digital_patents.RData')
patents = digital_patents_EPO %>% as_tibble()
rm(digital_patents_EPO)

patents[1:30,] %>% View

# relevant data
patents = patents %>% select(year, date, title, abstract, inventor, applicant, country, keywords, categories)

# years
patents$year %>% hist
patents = patents %>% filter(year >=2000)

# NAs
patents %>% sapply(function(x){ is.na(x) %>% sum })
patents = patents %>% drop_na()

write.csv(patents, 'patents.csv')

















