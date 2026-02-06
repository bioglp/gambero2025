# -------------------------------------
# Project:
# Author:
# Date:
# Notes:
# -------------------------------------

library(tidyverse)
library(patchwork)
library(skimr)
library(glmm)
theme_set(theme_linedraw())

m <- readxl::read_excel('input/Gamberi Maschi 18-19 morf biochim.xls')
f <- readxl::read_excel('input/Gamberi Femm 18-19 morf biochim.xls')

m$month <- str_sub(m$data,1,2)
m$year <- str_sub(m$data,4,5)
m$date <- str_c(m$year,'.',m$month)

f$month <- str_sub(f$data,1,2)
f$year <- str_sub(f$data,4,5)
f$date <- str_c(f$year,'.',f$month)

#
skim(m)
skim(f)

nf <- names(f)
nm <- names(m)
clark <- rbind(m,f)
clark <- clark %>% filter(!is.na(sesso))

library(mosaic)
favstats(`lung tot (mm)` ~ sesso, data=clark)

# tabella statistiche
clark %>%
    group_by(sesso) %>%
    summarise(across(c(`lung tot (mm)`,
                       `lung.chela (mm)`,
                       `peso tot (g)`,
                       `peso addome`,
                       `peso epato`,
                       `peso muscolo`),
                     list(mean=mean,sd=sd, min=min,max=max), .names = "{col}_{fn}")) %>%
    write_csv('stat.csv')

m %>% arrange(`lung tot (mm)`)


# tabella sintesi - data, sesso, variabile

clark %>%
    group_by(m,sesso) %>%
    select(ym,sesso,`lung tot (mm)`,`peso tot (g)`) %>%
    summarise_all(list(mean,sd,min,max))

library(gtsummary)
clark %>%
    select(sesso,`peso tot (g)`,`peso addome`,`peso epato`) %>%
    tbl_summary(by = c(sesso))

# peso totale
meanW <- clark %>%
    group_by(sesso) %>%
    summarise(mean=mean(`peso tot (g)`,na.rm=T))
clark %>%
    ggplot(aes(`peso tot (g)`))+
    geom_histogram(aes(fill=sesso), col='black')+
    labs(y='N')+
    facet_grid(~sesso)+
    geom_vline(data=meanW,aes(xintercept = mean), linetype='dashed')+
    theme(legend.position = 'none')
ggsave('plots/pesoTotale.pdf',width = 10, height = 5)

# lunghezza carapace
meanLC <- clark %>%
    group_by(sesso) %>%
    summarise(mean=mean(`lt carapace (mm)`,na.rm=T))
clark %>%
    ggplot(aes(`lt carapace (mm)`))+
    geom_histogram(aes(fill=sesso), col='black')+
    labs(y='N')+
    facet_grid(~sesso)+
    geom_vline(data=meanLC,aes(xintercept = mean), linetype='dashed')+
    theme(legend.position = 'none')
ggsave('plots/lunghezzaCarapace.pdf',width = 10, height = 5)

# largh carapace
meanlC <- clark %>%
    group_by(sesso) %>%
    summarise(mean=mean(`largh carapace (mm)`,na.rm=T))
clark %>%
    ggplot(aes(`largh carapace (mm)`))+
    geom_histogram(aes(fill=sesso), col='black')+
    labs(y='N')+
    facet_grid(~sesso)+
    geom_vline(data=meanlC,aes(xintercept = mean), linetype='dashed')+
    theme(legend.position = 'none')
ggsave('plots/larghezzaCarapace.pdf',width = 10, height = 5)

# lung.chela (mm)
meanlc <- clark %>%
    group_by(sesso) %>%
    summarise(mean=mean(`lung.chela (mm)`,na.rm=T))
clark %>%
    ggplot(aes(`lung.chela (mm)`))+
    geom_histogram(aes(fill=sesso), col='black')+
    labs(y='N', x='lung chela (mm)')+
    facet_grid(~sesso)+
    geom_vline(data=meanlc,aes(xintercept = mean), linetype='dashed')+
    theme(legend.position = 'none')
ggsave('plots/lunghezzaChela.pdf',width = 10, height = 5)

require(ggpubr)
# regressione lung - peso
ggplot(clark,aes(`lung tot (mm)`,`peso tot (g)`, col=sesso))+
    geom_smooth(method = 'lm') +
    geom_point(alpha=0.7,size=3) +
    geom_point(col='black',size=.3) +
    ggtitle('Regressione lunghezza tot vs peso tot') +
    xlab('lung tot (mm)') + ylab('peso tot (g)') +
    stat_cor(label.x = 60, label.y = 61) +
    stat_regline_equation(label.x = 60, label.y = 58) +
    facet_grid(.~sesso) +
    theme(legend.position = "none")
ggsave('plots/regressioneLW.pdf',width = 10, height = 5)

# numero mute nel tempo by sex ----
# non ci sono i numeri per fare un'elaborazione
clark.moult <-clark %>%
    filter(muta=='SI')%>%
    group_by(date,sesso, muta) %>%
    count()

#livelli medi mensili e gonadi ---- fig. 30
clark.gonad <- filter(clark, sesso=='F', `maturità gonadi`=="MATURE") %>%
    group_by(date) %>%
    count()

# livelli lago
liv2018 <- read.csv('./input/S_Savino-idrometro2018.csv')
liv2019 <- read.csv('./input/S_Savino-idrometro2019.csv')
livelli <- as_tibble(rbind(liv2018,liv2019))
livelli$date <- as.Date(livelli$date,format='%d/%m/%Y')
library(lubridate)
livelli$year <- year(livelli$date)
livelli$month <- month(livelli$date)
meanLev <- mean(livelli$avg)
livelliM <- livelli %>%
    group_by(year,month) %>%
    filter(date>'2018-06-30', date<'2019-07-01') %>%
    summarise(avg=mean(avg),
              min=mean(min),
              max=mean(max),
              .groups = 'drop')
livelliM$date=clark.gonad$date

# gonadi
require(zoo)
clark.gonad2 <- left_join(clark.gonad,livelliM,"date")
clark.gonad2$ym <- as.yearmon(str_c(clark.gonad2$year,'-',clark.gonad2$month))

ggplot(clark.gonad2, aes(x=ym,y=n, group=1)) +
    geom_line(col='#EF9A71', alpha=0.9, lwd=1.3) +
    geom_point(col='#EF9A71',size=4) +
    labs(title = 'Maturazione gonadi e livelli lago',
         x='') +
    geom_point(aes(y=(avg+1)*100), size=4) +
    geom_line(aes(y=(avg+1)*100), lwd=1.3, alpha=0.3) +
    scale_y_continuous('N femmine',
                       sec.axis = sec_axis(~ ./100-1,
                                           name = "livello idrometrico (m)")) +
    scale_color_brewer(palette = "Set2")
ggsave('plots/maturazioneGonadi.pdf',width = 10, height = 5)

clark$gonadBin <- 0
clark$gonadBin[clark$`maturità gonadi`=='MATURE'] <- 1

clark$ym <- as.yearmon(str_c(as.numeric(clark$year)+2000,'-',clark$month))
clark.Pgonads2 <- clark %>%
    filter(sesso=='F') %>%
    group_by(ym, gonadBin) %>%
    select(ym, gonadBin) %>%
    count()
ggplot(clark.Pgonads2,aes(x=ym,y=n))+
    geom_bar(stat = 'identity', position = 'fill',
             aes(fill=as.factor(gonadBin)))+ ylab('') + xlab('')+
    scale_fill_manual('',labels=c('Immature','Mature'),
                          values = c("#FAEADC", "#D4722D"))+
    scale_y_continuous(labels = scales::percent_format())+
    theme(legend.position = 'top')
ggsave('plots/maturazioneGonadiPercentuale.pdf',width = 10, height = 5)

# Maturità sessuale maschi ---- fig. 32
clark.sexM <- clark %>%
    filter(sesso=='M') %>%
    group_by(ym, SA) %>%
    select(ym, SA) %>%
    count()

ggplot(clark.sexM,aes(x=ym,y=n))+
    geom_bar(stat = 'identity', position = 'fill',
             aes(fill=SA))+ ylab('') +xlab('') +
    scale_fill_manual('', labels=c('Immature','Mature'),
                      values=c("#DEBBB9","#E93323"))+
    scale_y_continuous(labels = scales::percent_format())+
    theme(legend.position = 'top')
ggsave('plots/maturazioneMaschiPercentuale.pdf',width = 10, height = 5)

# TwB
clark %>%
    group_by(ym,sesso) %>%
    summarise(meanTwB=mean(TwB),
              minTwB=min(TwB),
              maxTwB=max(TwB)) %>%
    ggplot(aes(ym,meanTwB, col=sesso))+
    geom_pointrange(aes(ymin=minTwB, ymax=maxTwB),
                    position = position_dodge(width = 0.01))+
    geom_line(aes(group=sesso))+labs(y='TwB', x='')+
    theme(legend.position = 'top')
ggsave('plots/TwB.pdf',width = 10, height = 5)


# peso addome (g)
meanAdd <- clark %>%
    group_by(sesso) %>%
    summarise(mean=mean(`peso addome`,na.rm=T))
clark %>%
    ggplot(aes(`peso addome`))+
    geom_histogram(aes(fill=sesso), col='black')+
    labs(y='N')+
    facet_grid(~sesso)+
    geom_vline(data=meanAdd,aes(xintercept = mean), linetype='dashed')+
    theme(legend.position = 'none')
ggsave('plots/pesoAddome.pdf',width = 10, height = 5)

# Hiw
clark %>%
    group_by(ym,sesso) %>%
    summarise(meanHiw=mean(Hiw),
              minHiw=min(Hiw),
              maxHiw=max(Hiw),
              .groups = 'drop') %>%
    ggplot(aes(ym,meanHiw, col=sesso))+
    geom_pointrange(aes(ymin=minHiw, ymax=maxHiw),
                    position = position_dodge(width = 0.01))+
    geom_line(aes(group=sesso))+ labs(y='HIw', x='')+
    theme(legend.position = 'top')
ggsave('plots/Hiw.pdf',width = 10, height = 5)

# colore hepato ---- riclassificare i colori????
clark.colEpato <- clark %>%
    group_by(ym, `colepatocod`) %>%
    count()

ggplot(clark.colEpato, aes(x=ym,y=n)) +
    geom_bar(stat="identity",
             aes(fill=as.factor(colepatocod)),
             position = position_stack(reverse = TRUE)) +
    labs(y='N', x='')+
    scale_fill_manual('colore ',
                      values=c("#F8AA32", "#B29D62", "#BECB9A"), )
ggsave('plots/coloreHepato.pdf',width = 10, height = 5)


# peso hepatopancreas (g)
meanEpa <- clark %>%
    group_by(sesso) %>%
    summarise(mean=mean(`peso epato`,na.rm=T))
clark %>%
    ggplot(aes(`peso epato`))+
    geom_histogram(aes(fill=sesso), col='black')+
    labs(y='N')+
    facet_grid(~sesso)+
    geom_vline(data=meanEpa,aes(xintercept = mean), linetype='dashed')+
    theme(legend.position = 'none')
ggsave('plots/pesoHepato.pdf',width = 10, height = 5)

# GSI and HIw ----
clark.indexes <- filter(clark, sesso=='F') %>%
    select(ym,Ptot=`peso tot (g)`,Pgonads=`peso gonadi`,Phepato=`peso epato`) %>%
    mutate(GSI=Pgonads/(Ptot-Pgonads)*100,
           HIw=Phepato/Ptot*100) %>%
    group_by(ym) %>%
    summarise(GSI=mean(GSI, na.rm = T),
              HIw=mean(HIw, na.rm = T))

ggplot(clark.indexes, aes(ym,GSI, group = 1)) +
    geom_point(size=4, col='#F79A43') +
    geom_path(lwd=1.3, col='#F79A43') +
    geom_path(aes(ym,HIw),lwd=1.3, col='#E76BF3') +
    geom_point(aes(ym,HIw),size=4, col='#E76BF3') +
    scale_y_continuous(sec.axis = sec_axis(~.*1, name = "HIw")) +
    geom_text(x=2019.45, y=7.9, label="GSI", col='#F79A43')+
    geom_text(x=2019.45, y=6.5, label="HIw", col='#E76BF3')+
    labs(x='')
ggsave('plots/GSI.pdf',width = 10, height = 5)

# relazioni con dimensioni
skim(clark)

# MTE
clark %>%
    ggplot(aes(`lung tot (mm)`,MTE))+
    geom_point(aes(col=sesso))

clark %>%
    ggplot(aes(`peso tot (g)`,MTE))+
    geom_point(aes(col=sesso))

clark %>%
    ggplot(aes(`peso epato`,MTE))+
    geom_point(aes(col=sesso))

clark %>%
    ggplot(aes(`peso addome`,MTE))+
    geom_point(aes(col=sesso))

clark %>%
    ggplot(aes(`peso muscolo`,MTE))+
    geom_point(aes(col=sesso))

# CATE
clark %>%
    ggplot(aes(`lung tot (mm)`,CATE))+
    geom_point(aes(col=sesso))

clark %>%
    ggplot(aes(`peso tot (g)`,CATE))+
    geom_point(aes(col=sesso))

clark %>%
    ggplot(aes(`peso epato`,CATE))+
    geom_point(aes(col=sesso))

clark %>%
    ggplot(aes(`peso addome`,CATE))+
    geom_point(aes(col=sesso))

clark %>%
    ggplot(aes(`peso muscolo`,CATE))+
    geom_point(aes(col=sesso))

# GPxE
clark %>%
    ggplot(aes(`lung tot (mm)`,GPxE))+
    geom_point(aes(col=sesso))

clark %>%
    ggplot(aes(`peso tot (g)`,GPxE))+
    geom_point(aes(col=sesso))

clark %>%
    ggplot(aes(`peso epato`,GPxE))+
    geom_point(aes(col=sesso))

clark %>%
    ggplot(aes(`peso addome`,GPxE))+
    geom_point(aes(col=sesso))

clark %>%
    ggplot(aes(`peso muscolo`,GPxE))+
    geom_point(aes(col=sesso))

# MTE
clark %>%
    ggplot(aes(`lung tot (mm)`,MTE))+
    geom_point(aes(col=sesso))

clark %>%
    ggplot(aes(`peso tot (g)`,MTE))+
    geom_point(aes(col=sesso))

clark %>%
    ggplot(aes(`peso epato`,MTE))+
    geom_point(aes(col=sesso))

clark %>%
    ggplot(aes(`peso addome`,MTE))+
    geom_point(aes(col=sesso))

clark %>%
    ggplot(aes(`peso muscolo`,MTE))+
    geom_point(aes(col=sesso))


# parametri ambientali
library(zoo)

chimLudo <- read.csv('input/chimLudo.csv')
chimLudo <- as_tibble(chimLudo)

chimLudo$DOfondo[1] <- NA


chimLudo$date <- as.yearmon(str_c(chimLudo$year+2000,'-',chimLudo$month))

chimLudo
clark$date <- as.yearmon(str_c(as.numeric(substr(clark$data,4,5))+2000,'-',
                         as.numeric(substr(clark$data,1,2))))


clarkChim <- left_join(clark,chimLudo, by='date')
#View(clarkChim)

# tabella di sintesi correlazioni
# devtools::install_github("laresbernardo/lares")
library(lares)
library(extrafont)

# Hepathopancreas
write_excel_csv2(clarkChim,'clarkChim.xlsx')

fem <- clarkChim %>%
    filter(sesso=='F') %>%
    select(MTE,prof:AlcalTCaCO3) %>%
    corr_F(MTE,top = 20, max_pvalue = 0.05)
mal <- clarkChim %>%
    filter(sesso=='M') %>%
    select(MTE,prof:AlcalTCaCO3) %>%
    corr_M(MTE,top = 20, max_pvalue = 0.05)
fem+mal
ggsave('plots/corrMTE.pdf',width = 10, height = 6,device = cairo_pdf)

fem <- clarkChim %>%
    filter(sesso=='F') %>%
    select(GPxE,prof:AlcalTCaCO3) %>%
    corr_F(GPxE,top = 20, max_pvalue = 0.05)
mal <- clarkChim %>%
    filter(sesso=='M') %>%
    select(GPxE,prof:AlcalTCaCO3) %>%
    corr_M(GPxE,top = 20, max_pvalue = 0.05)
fem+mal
ggsave('plots/corrGPxE.pdf',width = 10, height = 6,device = cairo_pdf)

fem <- clarkChim %>%
    filter(sesso=='F') %>%
    select(CATE,prof:AlcalTCaCO3) %>%
    corr_F(CATE,top = 20, max_pvalue = 0.05)
mal <- clarkChim %>%
    filter(sesso=='M') %>%
    select(CATE,prof:AlcalTCaCO3) %>%
    corr_M(CATE,top = 20, max_pvalue = 0.05)
fem+mal
ggsave('plots/corrCATE.pdf',width = 10, height = 6,device = cairo_pdf)

fem <- clarkChim %>%
    filter(sesso=='F') %>%
    select(SODE,prof:AlcalTCaCO3) %>%
    corr_F(SODE,top = 20, max_pvalue = 0.05)
mal <- clarkChim %>%
    filter(sesso=='M') %>%
    select(SODE,prof:AlcalTCaCO3) %>%
    corr_M(SODE,top = 20, max_pvalue = 0.05)
fem+mal
ggsave('plots/corrSODE.pdf',width = 10, height = 6,device = cairo_pdf)

# Muscolo
fem <- clarkChim %>%
    filter(sesso=='F') %>%
    select(MTM,prof:AlcalTCaCO3) %>%
    corr_F(MTM,top = 20, max_pvalue = 0.05)
mal <- clarkChim %>%
    filter(sesso=='M') %>%
    select(MTM,prof:AlcalTCaCO3) %>%
    corr_M(MTM,top = 20, max_pvalue = 0.05)
fem+mal
ggsave('plots/corrMTM.pdf',width = 10, height = 6,device = cairo_pdf)

fem <- clarkChim %>%
    filter(sesso=='F') %>%
    select(GPxM,prof:AlcalTCaCO3) %>%
    corr_F(GPxM,top = 20, max_pvalue = 0.05)
mal <- clarkChim %>%
    filter(sesso=='M') %>%
    select(GPxM,prof:AlcalTCaCO3) %>%
    corr_M(GPxM,top = 20, max_pvalue = 0.05)
fem+mal
ggsave('plots/corrGPxM.pdf',width = 10, height = 6,device = cairo_pdf)

fem <- clarkChim %>%
    filter(sesso=='F') %>%
    select(CATM,prof:AlcalTCaCO3) %>%
    corr_F(CATM,top = 20, max_pvalue = 0.05)
mal <- clarkChim %>%
    filter(sesso=='M') %>%
    select(CATM,prof:AlcalTCaCO3) %>%
    corr_M(CATM,top = 20, max_pvalue = 0.05)
fem+mal
ggsave('plots/corrCATM.pdf',width = 10, height = 6,device = cairo_pdf)

fem <- clarkChim %>%
    filter(sesso=='F') %>%
    select(SODM,prof:AlcalTCaCO3) %>%
    corr_F(SODM,top = 20, max_pvalue = 0.05)
mal <- clarkChim %>%
    filter(sesso=='M') %>%
    select(SODM,prof:AlcalTCaCO3) %>%
    corr_M(SODM,top = 20, max_pvalue = 0.05)
fem+mal
ggsave('plots/corrSODM.pdf',width = 10, height = 6,device = cairo_pdf)


# Plot ----------------------------------------------

clarkChim %>%
    ggplot(aes(AlcalTfondoCaCO3,MTE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

clarkChim %>%
    ggplot(aes(secchi,MTE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

clarkChim %>%
    ggplot(aes(conduSup25,MTE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

clarkChim %>%
    ggplot(aes(Tfondo,MTE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso), alpha=0.7)+
    facet_grid(~sesso)


# Temperatura

clarkChim %>%
    ggplot(aes(Tfondo,MTE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

clarkChim %>%
    ggplot(aes(Tfondo,GPxE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

clarkChim %>%
    ggplot(aes(Tfondo,CATE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

clarkChim %>%
    ggplot(aes(Tfondo,SODE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

# pHfondo
clarkChim %>%
    ggplot(aes(pHfondo,GPxE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

clarkChim %>%
    ggplot(aes(pHfondo,CATE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

clarkChim %>%
    ggplot(aes(pHfondo,SODE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

clarkChim %>%
    ggplot(aes(pHfondo,MTE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

# livello
clarkChim %>%
    ggplot(aes(prof,MTE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

clarkChim %>%
    ggplot(aes(prof,GPxE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

clarkChim %>%
    ggplot(aes(prof,CATE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

clarkChim %>%
    ggplot(aes(prof,SODE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

# DO
o1 <- clarkChim %>%
    ggplot(aes(date,DOsup))+
    geom_point()+
    geom_line() + ylim(6,13)

o2 <- clarkChim %>%
    ggplot(aes(date,DOfondo))+
    geom_point()+
    geom_line() + ylim(6,13)

library(patchwork)
o1+o2

# pH
pH1 <- clarkChim %>%
    ggplot(aes(date,pHsup))+
    geom_point()+
    geom_line()+ylim(8.2,9)

pH2 <- clarkChim %>%
    ggplot(aes(date,pHfondo))+
    geom_point()+
    geom_line()

library(patchwork)+ylim(8.2,9)
pH1+pH2


do1 <- clarkChim %>%
    ggplot(aes(DOsup,MTE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

do2 <- clarkChim %>%
    ggplot(aes(DOfondo,MTE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso), pch=16)+
    facet_grid(~sesso)

do1 + do2 + plot_layout(guides = 'collect')

clarkChim %>%
    filter(DOsup>12.3, sesso=='M') %>%
    select(sesso, date, DOsup, DOfondo, MTE)


clarkChim %>%
    ggplot(aes(DOsup,GPxE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

clarkChim %>%
    ggplot(aes(DOsup,CATE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

clarkChim %>%
    ggplot(aes(DOsup,SODE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

# DO media

clarkChim %>%
    ggplot(aes(Domedia,MTE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

clarkChim %>%
    ggplot(aes(Domedia,GPxE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

clarkChim %>%
    ggplot(aes(Domedia,CATE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

clarkChim %>%
    ggplot(aes(Domedia,SODE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)


# DO media
dom <- clarkChim %>%
    group_by(data,codice) %>%
    select(DOsup,DOfondo,Domedia) %>%
    mutate(media=(DOsup+DOfondo)/2)




clarkChim %>%
    ggplot(aes(condMediau,MTE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

clarkChim %>%
    ggplot(aes(condMediau,GPxE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

clarkChim %>%
    ggplot(aes(condMediau,CATE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)

clarkChim %>%
    ggplot(aes(condMediau,SODE))+
    geom_smooth(method = 'lm', aes(col=sesso))+
    geom_point(size=4,col='white')+
    geom_point(aes(col=sesso))+
    facet_grid(~sesso)





# testing ----------------------------------

skim(clark)

mod.Hiw <- glm(Hiw~SODM+CATM+GPxM+MTM,
    data=clark, family = 'gaussian')
summary(mod.Hiw)

library(lme4)
mod.Hiw <- lmer(Hiw~SODE+CATE+GPxE+MTE+(1|sesso),
               data=clark)
summary(mod.Hiw)


library(corrr)

clark %>%
    select(SODM,SODE,CATM,CATE,GPxM,GPxE,MTM,MTE) %>%
    correlate()







