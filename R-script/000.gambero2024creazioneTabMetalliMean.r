library(openxlsx)
metalli <- read.xlsx('input/metalli.xlsx')
head(metalli)

library(rstatix)
library(lubridate)
library(tidyr)

metalli <- metalli %>% 
    filter(codice!='M7-0718-M') %>% 
    mutate(date.tmp=gsub(".*-(.*)-.*", "\\1", codice)) %>% 
        mutate(month=substring(date.tmp,1,2),
           year=substring(date.tmp,3,4)) %>% 
    mutate(date=as_date(paste0(year,'-',month,'-01')))


metalli %>% 
    group_by(date,sesso,tessuto) %>% 
    get_summary_stats(Cr:Hg,type = 'mean') %>% 
    pivot_wider(names_from = variable,
                values_from = mean) 
# %>% 
#     write.xlsx('input/metalliMean.xlsx')
