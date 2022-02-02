rm(list = ls())
required_libs = c('tidyverse', 'plotly')
load_pack = function(lib = required_libs)
{
   if(!require(lib, character.only = TRUE)) install.packages(lib)
   library(lib, character.only = TRUE)
}
lapply(required_libs, load_pack); rm(required_libs)


# By Author --------------------------------------------------------------------------------------

science_f = list.files('data/counts/author', pattern = "papers_.*.csv")
industry_f = list.files('data/counts/author', pattern = "patents_.*.csv")

sf = str_match(science_f, "_AU_\\d+_(\\w+).csv")[,2] %>%
   str_replace_all("_", " ")
for(i in 1:length(science_f)){
   d = read_csv(paste0("data/counts/author/", science_f[i])) %>%
      filter(count > 10)
   p = d %>%
      ggplot(aes(topic %>% reorder(count), count, fill=topic %>% reorder(count))) +
      geom_bar(stat='identity') +
      coord_flip() + ylab("Contributions") + xlab('') +
      theme_minimal() +
      ggtitle(sf[i]) +
      theme(plot.title = element_text(hjust=.5, face='bold', size=16),
            legend.position = 'none')
   p %>% ggplotly(width=1200, height=700) %>%
      as_widget() %>%
      htmlwidgets::saveWidget(paste0('data/plots/contrib/author_', sf[i], '.html'), 
                              selfcontained = FALSE, libdir = "lib")
}
p %>% ggplotly()

for(i in 1:length(science_f)){
   d = read_csv(paste0("data/counts/author/", science_f[i])) %>%
      filter(count > 10)
   p = d %>%
      ggplot(aes(topic %>% reorder(citations), citations, fill=topic %>% reorder(citations))) +
      geom_bar(stat='identity') +
      coord_flip() + ylab("Citations") + xlab('') +
      theme_minimal() +
      ggtitle(sf[i]) +
      theme(plot.title = element_text(hjust=.5, face='bold', size=16),
            legend.position = 'none')
   p %>% ggplotly(width=1200, height=700) %>%
      as_widget() %>%
      htmlwidgets::saveWidget(paste0('data/plots/contrib/author_', sf[i], '_citations.html'), 
                              selfcontained = FALSE, libdir = "lib")
}
p %>% ggplotly()


pf = str_match(industry_f, "_inventor_\\d+_(\\w+).csv")[,2] %>%
   str_replace_all("_", " ")
for(i in 1:length(industry_f)){
   d = read_csv(paste0("data/counts/author/", industry_f[i])) %>%
      filter(count > 10)
   p = d %>%
      ggplot(aes(topic %>% reorder(count), count, fill=topic %>% reorder(count))) +
      geom_bar(stat='identity') +
      coord_flip() + ylab("Contributions") + xlab('') +
      theme_minimal() +
      ggtitle(pf[i]) +
      theme(plot.title = element_text(hjust=.5, face='bold', size=16),
            legend.position = 'none')
   p %>% ggplotly(width=1200, height=700) %>%
      as_widget() %>%
      htmlwidgets::saveWidget(paste0('data/plots/contrib/inventor_', pf[i], '.html'), 
                              selfcontained = FALSE, libdir = "lib")
}



# By publisher --------------------------------------------------------------------------------

pub_f = list.files('data/counts/publisher', pattern = "papers_.*.csv")

sf = str_match(pub_f, "_publisher_\\d+_(\\w+).csv")[,2] %>%
   str_replace_all("_", " ")
for(i in 1:length(science_f)){
   d = read_csv(paste0("data/counts/publisher/", pub_f[i])) %>%
      filter(count > 30)
   p = d %>%
      ggplot(aes(topic %>% reorder(count), count, fill=topic %>% reorder(count))) +
      geom_bar(stat='identity') +
      coord_flip() + ylab("Contributions") + xlab('') +
      theme_minimal() +
      ggtitle(sf[i]) +
      theme(plot.title = element_text(hjust=.5, face='bold', size=16),
            legend.position = 'none')
   p %>% ggplotly(width=1200, height=700) %>%
      as_widget() %>%
      htmlwidgets::saveWidget(paste0('data/plots/contrib/publisher_', sf[i], '.html'), 
                              selfcontained = FALSE, libdir = "lib")
}
p %>% ggplotly()

for(i in 1:length(science_f)){
   d = read_csv(paste0("data/counts/publisher/", pub_f[i])) %>%
      filter(count > 30)
   p = d %>%
      ggplot(aes(topic %>% reorder(citations), citations, fill=topic %>% reorder(citations))) +
      geom_bar(stat='identity') +
      coord_flip() + ylab("Citations") + xlab('') +
      theme_minimal() +
      ggtitle(sf[i]) +
      theme(plot.title = element_text(hjust=.5, face='bold', size=16),
            legend.position = 'none')
   p %>% ggplotly(width=1200, height=700) %>%
      as_widget() %>%
      htmlwidgets::saveWidget(paste0('data/plots/contrib/publisher_', sf[i], '_citations.html'), 
                              selfcontained = FALSE, libdir = "lib")
}
p %>% ggplotly()




# By DT - science -----------------------------------------------------------------------------

science_f = paste0('data/counts/author/', list.files('data/counts/author', pattern = "papers_.*.csv"))
dfs = lapply(science_f, function(x){
   d = read_csv(x) %>%
      mutate(author = sub(".*_AU_\\d+_", "", x) %>% 
                substr(start=1, stop=nchar(.)-4) %>% 
                gsub(pattern = "_", replacement = " ", .))
   return(d)
})
data = do.call(rbind, dfs)   
rm(dfs)

# authors
data = data %>% group_by(topic) %>%
   arrange(desc(count)) %>%
   slice_head(n=15) %>%
   mutate(sum = sum(count)) %>%
   ungroup() %>%
   filter(sum > 50) %>%
   arrange(desc(sum))

topics = unique(data$topic)
for(i in length(topics)){
   t = topics[i]
   p = data %>% filter(topic == t) %>%
      ggplot(aes(author %>% reorder(count), count, fill=author %>% reorder(count))) +
      geom_bar(stat='identity', width = 0.5) +
      coord_flip() + ylab("Contributions") + xlab('') +
      theme_minimal() +
      ggtitle(t) +
      theme(plot.title = element_text(hjust=.5, face='bold', size=16),
            legend.position = 'none')
   p %>% ggplotly(width=1200, height=700) %>%
      as_widget() %>%
      htmlwidgets::saveWidget(paste0('data/plots/contrib/science_topic_', t, '.html'), 
                              selfcontained = FALSE, libdir = "lib")
}
p

# authors citations
for(i in length(topics)){
   t = topics[i]
   p = data %>% filter(topic == t) %>%
      ggplot(aes(author %>% reorder(citations), citations, fill=author %>% reorder(citations))) +
      geom_bar(stat='identity', width = 0.5) +
      coord_flip() + ylab("Citations") + xlab('') +
      theme_minimal() +
      ggtitle(t) +
      theme(plot.title = element_text(hjust=.5, face='bold', size=16),
            legend.position = 'none')
   p %>% ggplotly(width=1200, height=700) %>%
      as_widget() %>%
      htmlwidgets::saveWidget(paste0('data/plots/contrib/science_topic_', t, '_citations.html'), 
                              selfcontained = FALSE, libdir = "lib")
}
p

# publishers
pub_f = paste0('data/counts/publisher/', list.files('data/counts/publisher', pattern = "papers_.*.csv"))
dfs = lapply(pub_f, function(x){
   d = read_csv(x) %>%
      mutate(publisher = sub(".*_publisher_\\d+_", "", x) %>% 
                substr(start=1, stop=nchar(.)-4) %>% 
                substr(1, 30) %>%
                gsub(pattern = "_", replacement = " ", .))
   return(d)
})
data = do.call(rbind, dfs)   
rm(dfs)

data = data %>% group_by(topic) %>%
   arrange(desc(count)) %>%
   slice_head(n=15) %>%
   mutate(sum = sum(count)) %>%
   ungroup() %>%
   filter(sum > 50) %>%
   arrange(desc(sum))

topics = unique(data$topic)
for(i in length(topics)){
   t = topics[i]
   p = data %>% filter(topic == t) %>%
      ggplot(aes(publisher %>% reorder(count), count, fill=publisher %>% reorder(count))) +
      geom_bar(stat='identity', width = 0.5) +
      coord_flip() + ylab("Contributions") + xlab('') +
      theme_minimal() +
      ggtitle(t) +
      theme(plot.title = element_text(hjust=.5, face='bold', size=16),
            legend.position = 'none')
   p %>% ggplotly(width=1200, height=700) %>%
      as_widget() %>%
      htmlwidgets::saveWidget(paste0('data/plots/contrib/science_topic_', t, '_bypub.html'), 
                              selfcontained = FALSE, libdir = "lib")
}

# publishers citations
for(i in length(topics)){
   t = topics[i]
   p = data %>% filter(topic == t) %>%
      ggplot(aes(publisher %>% reorder(citations), citations, fill=publisher %>% reorder(citations))) +
      geom_bar(stat='identity', width = 0.5) +
      coord_flip() + ylab("Citations") + xlab('') +
      theme_minimal() +
      ggtitle(t) +
      theme(plot.title = element_text(hjust=.5, face='bold', size=16),
            legend.position = 'none')
   p %>% ggplotly(width=1200, height=700) %>%
      as_widget() %>%
      htmlwidgets::saveWidget(paste0('data/plots/contrib/science_topic_', t, '_bypub_citations.html'), 
                              selfcontained = FALSE, libdir = "lib")
}
p




