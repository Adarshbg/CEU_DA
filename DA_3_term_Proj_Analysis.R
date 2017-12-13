rm(list=ls())

library(data.table)
library(ggplot2)

setwd('C:/Users/S1BQ8U/Desktop/Business Analytics/DA 3/Term proj')

# LOAD  DATA
df_all <- read.csv("bisnode_all.csv", na.strings = "#N/A")

dt_all <- data.table(df_all)

pct <- function(x) {
  round((x-lag(x))*100/(lag(x)+0.1),2)} 

dt_15 <- dt_all[year %in% c('2014', '2015')]

dt_15[, ceo_Age := year-birth_year]

dt_15 <- dt_15[!is.na(birth_year)]

dt_15[, ceo_Age := year-birth_year]

##Considering CEOs of atleast 18 yrs only
dt_15 <- dt_15[birth_year<1998]

summary(dt_15$ceo_Age)

dt_15$tot_assets <- dt_15$curr_assets + dt_15$fixed_assets +dt_15$intang_assets
summary(dt_15$tot_assets)

## Dropping highest 1percentile  Total Assets
dt_15 <- dt_15[tot_assets<=quantile(dt_15$tot_assets, 0.99, na.rm=TRUE)]
dt_15 <- dt_15[sales<=quantile(dt_15$sales, 0.99, na.rm=TRUE)]


summary(dt_15$tot_assets)

dt_15[tot_assets <= 21270, comp_size := 'small']
dt_15[tot_assets > 21270 & tot_assets<= 86538, comp_size := 'medium']
dt_15[tot_assets > 86538 , comp_size := 'large']

ggplot(dt_15, aes(x=comp_size)) + geom_bar()

ggplot(dt_15[,list(sales= sum(sales, na.rm=TRUE)),by=c('year','comp_size')]) +
  geom_bar(aes(x=comp_size,y=sales, fill=factor(year)), stat="identity", position="dodge") 

##Calculating % growth
df_15 <- dt_15 %>% group_by(comp_id) %>% mutate_each(funs(pct),
                                                     sales_percent = c(sales),
                                                     tot_assets_percent = c(tot_assets),
                                                     profit_percent = c(profit_loss_year))

dt_15 <- data.table(df_15)


##Adding CEO_AGE_CAT catagorical variable
dt_15[, ceo_age_cat := cut(ceo_Age, c(0,40,Inf), c('young','old'))]

dt_15[,list(count=.N),by=c('comp_size','ceo_age_cat')]

ggplot(dt_15[,list(count=.N),by=c('comp_size','ceo_age_cat')]) + 
  geom_bar(aes(x=comp_size, y=count, fill=ceo_age_cat), stat="identity",position="dodge")

##Creating analysis table
dt_15_an <- dt_15[,list(sales= sum(sales, na.rm=TRUE),
                        profit_loss_year= sum(profit_loss_year, na.rm=TRUE),
                        tot_assets= sum(tot_assets, na.rm=TRUE)),
                  by=c('year','comp_size','ceo_age_cat')]

ggplot(dt_15_an[ceo_age_cat=='young']) +
  geom_bar(aes(x=comp_size,y=sales, fill=factor(year)), stat="identity", position="dodge")

ggplot(dt_15_an[ceo_age_cat=='young']) +
  geom_bar(aes(x=comp_size,y=sales, fill=factor(year)), stat="identity", position="dodge")+
  geom_text(aes(label=round(sales/1000000,1),x=comp_size,y=sales))

ggplot(dt_15_an) +
  geom_bar(aes(x=comp_size,y=sales, fill=factor(year)), stat="identity", position="dodge")+
  facet_wrap(~ceo_age_cat)

ggplot(dt_15_an) +
  geom_bar(aes(x=comp_size,y=profit_loss_year, fill=factor(year)), stat="identity", position="dodge")+
  facet_wrap(~ceo_age_cat)

ggplot(dt_15_an) +
  geom_bar(aes(x=comp_size,y=tot_assets, fill=factor(year)), stat="identity", position="dodge")+
  facet_wrap(~ceo_age_cat)

##Keeping only 2015 data
dt_15 <- dt_15[year==2015]

dt_15_an2 <- dt_15[, list(count=.N, 
                          avg_sales_gr=mean(sales_percent, na.rm=TRUE),
                          avg_assets_gr=mean(tot_assets_percent, na.rm=TRUE),
                          avg_profit_gr=mean(profit_percent, na.rm=TRUE)), 
                   by=c('comp_size','ceo_age_cat')]

ggplot(dt_15_an2) +
  geom_bar(aes(x=comp_size, y=avg_sales_gr, fill=ceo_age_cat), stat="identity", position="dodge")

ggplot(dt_15_an2) +
  geom_bar(aes(x=comp_size, y=avg_assets_gr, fill=ceo_age_cat), stat="identity", position="dodge")

ggplot(dt_15_an2) +
  geom_bar(aes(x=comp_size, y=avg_profit_gr, fill=ceo_age_cat), stat="identity", position="dodge")

##Creating analysis table with Industry
dt_15_an3 <- dt_15[, list(count=.N, 
                          avg_sales_gr=mean(sales_percent, na.rm=TRUE),
                          avg_assets_gr=mean(tot_assets_percent, na.rm=TRUE),
                          avg_profit_gr=mean(profit_percent, na.rm=TRUE)), 
                   by=c('comp_size','ceo_age_cat','ind')]

ggplot(dt_15_an3) +
  geom_bar(aes(x=comp_size, y=avg_sales_gr, fill=ceo_age_cat), stat="identity", position="dodge")+
  facet_wrap(~ind)

ggplot(dt_15_an3) +
  geom_bar(aes(x=comp_size, y=avg_assets_gr, fill=ceo_age_cat), stat="identity", position="dodge")+
  facet_wrap(~ind)

ggplot(dt_15_an3) +
  geom_bar(aes(x=comp_size, y=avg_profit_gr, fill=ceo_age_cat), stat="identity", position="dodge")+
  facet_wrap(~ind)

