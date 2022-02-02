rm(list = ls())
required_libs = c('tidyverse', 'plotly')
load_pack = function(lib = required_libs)
{
   if(!require(lib, character.only = TRUE)) install.packages(lib)
   library(lib, character.only = TRUE)
}
lapply(required_libs, load_pack); rm(required_libs)


# Trials --------------------------------------------------------------------------------------

data = read_csv("data/counts/papers_RA.csv") %>%
   filter(ScienceT == "Construction", year <= 2020) %>%
   mutate(year = as.integer(year))

(data %>% group_by(Topic) %>%
   ggplot(aes(year, count, color=Topic)) +
   geom_line() +
   ggtitle("Digital Topics in Construction") + ylab("Occurences") + xlab("") +
   guides(color = guide_legend(title="")) +
   theme_minimal() +
   theme(plot.title = element_text(hjust=.5, face='bold', size=16))) %>%
   ggplotly()

data = read_csv("data/counts/papers_RA.csv") %>%
   filter(Topic == "AI", year <= 2020) %>%
   mutate(year = as.integer(year))

(data %>% group_by(ScienceT) %>%
      ggplot(aes(year, count, color=ScienceT)) +
      geom_line() +
      ggtitle("AI in Science") + ylab("Occurences") + xlab("") +
      guides(color = guide_legend(title="")) +
      theme_minimal() + 
      theme(plot.title = element_text(hjust=.5, face='bold', size=16))) %>%
   ggplotly() 






# Produce plots - counts -------------------------------------------------------------------------------

data = read_csv("data/counts/papers_RA.csv") %>%
   filter(year <= 2019) %>%
   mutate(year = as.integer(year))


dtopics = data %>% group_by(Topic) %>% summarise(n = sum(count)) %>%
   arrange(desc(n))
stopics = data %>% group_by(ScienceT) %>% summarise(n = sum(count)) %>%
   arrange(desc(n))

# All DTs in one science topic
data1 = data %>% filter(Topic %in% dtopics$Topic[1:15])
data2 = data %>% filter(Topic %in% dtopics$Topic[16:30])
data3 = data %>% filter(Topic %in% dtopics$Topic[31:45])
data4 = data %>% filter(Topic %in% dtopics$Topic[46:60])

plot_st = function(data, t, suffix){
   p = data %>%
      filter(ScienceT == t) %>%
      ggplot(aes(year, count, color=Topic)) +
      geom_line() + geom_point() +
      ggtitle(paste0("Digital Topics in ", t, " - ", suffix)) + 
      ylab("Occurences") + xlab("") +
      guides(color = guide_legend(title="")) +
      theme_minimal() +
      theme(plot.title = element_text(hjust=.5, face='bold', size=16))
   p %>% ggplotly(width=1200, height=700) %>%
      as_widget() %>%
      htmlwidgets::saveWidget(paste0('data/plots/evo/dt_in_', t, '_', suffix, '.html'), 
                              selfcontained = FALSE, libdir = "lib")
   # return(p %>% ggplotly)
}

for(i in 1:60){
   plot_st(data1, stopics$ScienceT[i], "most popular")
   plot_st(data2, stopics$ScienceT[i], "moderately popular")
   plot_st(data3, stopics$ScienceT[i], "little popular")
   plot_st(data4, stopics$ScienceT[i], "less popular")
}

# All STs in one digital topic
data1 = data %>% filter(ScienceT %in% stopics$ScienceT[1:15])
data2 = data %>% filter(ScienceT %in% stopics$ScienceT[16:30])
data3 = data %>% filter(ScienceT %in% stopics$ScienceT[31:45])
data4 = data %>% filter(ScienceT %in% stopics$ScienceT[46:60])

plot_dt = function(data, t, suffix){
   p = data %>%
      filter(Topic == t) %>%
      ggplot(aes(year, count, color=ScienceT)) +
      geom_line() + geom_point() +
      ggtitle(paste0(t, " in Science - ", suffix)) + 
      ylab("Occurences") + xlab("") +
      guides(color = guide_legend(title="")) +
      theme_minimal() +
      theme(plot.title = element_text(hjust=.5, face='bold', size=16))
   p %>% ggplotly(width=1200, height=700) %>%
      as_widget() %>%
      htmlwidgets::saveWidget(paste0('data/plots/evo/', t, "_in_science_", suffix, '.html'), 
                              selfcontained = FALSE, libdir = "lib")
   # return(p %>% ggplotly)
}

for(i in 1:60){
   plot_dt(data1, dtopics$Topic[i], "most popular")
   plot_dt(data2, dtopics$Topic[i], "moderately popular")
   plot_dt(data3, dtopics$Topic[i], "little popular")
   plot_dt(data4, dtopics$Topic[i], "less popular")
}








# Produce plots - counts -------------------------------------------------------------------------------

data = read_csv("data/counts/papers_RA.csv") %>%
   filter(year <= 2019) %>%
   mutate(year = as.integer(year))


dtopics = data %>% group_by(Topic) %>% summarise(n = sum(citations)) %>%
   arrange(desc(n))
stopics = data %>% group_by(ScienceT) %>% summarise(n = sum(citations)) %>%
   arrange(desc(n))


# All DTs in one science topic
data1 = data %>% filter(Topic %in% dtopics$Topic[1:15])
data2 = data %>% filter(Topic %in% dtopics$Topic[16:30])
data3 = data %>% filter(Topic %in% dtopics$Topic[31:45])
data4 = data %>% filter(Topic %in% dtopics$Topic[46:60])

plot_st = function(data, t, suffix){
   p = data %>%
      filter(ScienceT == t) %>%
      ggplot(aes(year, citations, color=Topic)) +
      geom_line() + geom_point() +
      ggtitle(paste0("Digital Topics in ", t, " - ", suffix)) + 
      ylab("Citations") + xlab("") +
      guides(color = guide_legend(title="")) +
      theme_minimal() +
      theme(plot.title = element_text(hjust=.5, face='bold', size=16))
   p %>% ggplotly(width=1200, height=700) %>%
      as_widget() %>%
      htmlwidgets::saveWidget(paste0('data/plots/evo/dt_in_', t, '_', suffix, '.html'), 
                              selfcontained = FALSE, libdir = "lib")
   # return(p %>% ggplotly)
}

for(i in 1:60){
   plot_st(data1, stopics$ScienceT[i], "most popular")
   plot_st(data2, stopics$ScienceT[i], "moderately popular")
   plot_st(data3, stopics$ScienceT[i], "little popular")
   plot_st(data4, stopics$ScienceT[i], "less popular")
}



# All STs in one digital topic
data1 = data %>% filter(ScienceT %in% stopics$ScienceT[1:15])
data2 = data %>% filter(ScienceT %in% stopics$ScienceT[16:30])
data3 = data %>% filter(ScienceT %in% stopics$ScienceT[31:45])
data4 = data %>% filter(ScienceT %in% stopics$ScienceT[46:60])

plot_dt = function(data, t, suffix){
   p = data %>%
      filter(Topic == t) %>%
      ggplot(aes(year, citations, color=ScienceT)) +
      geom_line() + geom_point() +
      ggtitle(paste0(t, " in Science - ", suffix)) + 
      ylab("Citations") + xlab("") +
      guides(color = guide_legend(title="")) +
      theme_minimal() +
      theme(plot.title = element_text(hjust=.5, face='bold', size=16))
   p %>% ggplotly(width=1200, height=700) %>%
      as_widget() %>%
      htmlwidgets::saveWidget(paste0('data/plots/evo/', t, "_in_science_", suffix, '.html'), 
                              selfcontained = FALSE, libdir = "lib")
   # return(p %>% ggplotly)
}

for(i in 1:60){
   plot_dt(data1, dtopics$Topic[i], "most popular")
   plot_dt(data2, dtopics$Topic[i], "moderately popular")
   plot_dt(data3, dtopics$Topic[i], "little popular")
   plot_dt(data4, dtopics$Topic[i], "less popular")
}





















