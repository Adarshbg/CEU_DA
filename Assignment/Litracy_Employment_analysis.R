library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)  # to combine graphs
#install.packages("plm")
library(plm)  # for panels
library(data.table)
library(lmtest)
library(sandwich)
library(stargazer)
library(tidyverse)
library(tidyr)

#getwd()
setwd("D:/Data World/Business Analytics/DA 5/Assignment")

?fread

data <- fread("Education_Employment_new.csv")

base_data <- data

class(data)

for(i in colnames(data)){
  sum_na <- sum(is.na(dt[,i,]))
  if (sum_na){
    print(c(i, sum_na,"NA-s"))
  }              
}

original_names <- names(data)

names(data) <- c(
  'country_name', 'country_code', 'series_code', 
  'yr1995','yr1996','yr1997','yr1998','yr1999','yr2000','yr2001','yr2002','yr2003','yr2004','yr2005',
  'yr2006','yr2007',
  'yr2008','yr2009','yr2010','yr2011','yr2012','yr2013','yr2014','yr2015','yr2016','yr2017'
)

nrow(data[is.na(yr1995)])

for(i in colnames(data)){
  sum_na <- nrow(data[is.na(i)])
      print(c(i, sum_na,"NA-s"))
}

#?spread

data_long <- gather(data, key="year", value="values", yr1995:yr2017, factor_key=TRUE)

data_long <- data.table(data_long)

data_long_base <- data_long

data_spread <- spread(data_long, key="series_code", value="values", fill=NA, convert=FALSE)



data_long[is.na(values),list(count=.N),by=list(country_name, series_code)][order(-count)]

data_long[is.na(values),list(count=.N),by=list(country_name, country_code)][order(-count)]

data_long[is.na(values),list(count=.N),by=list(series_code)][order(-count)]

missing_countries <- data_long[is.na(values),list(count=.N),by=list(country_name, country_code)][order(-count)][count<500, country_code]

data_long <- data_long[country_code %in% missing_countries]

data_spread <- spread(data_long, key="series_code", value="values", fill=NA, convert=FALSE)

data_long[is.na(values),list(count=.N),by=list(country_name, series_code)][order(-count)]

data_long[is.na(values),list(count=.N),by=list(country_name, country_code)][order(-count)]

data_long[is.na(values),list(count=.N),by=list(series_code)][order(-count)]

missing_countries_2 <- data_long[series_code=="gov_exp_edu_tot_perc_gdp"][is.na(values),list(count=.N), by=list(country_name, country_code)][count<12,country_code]

data_long <- data_long[country_code %in% missing_countries_2]

data_spread <- spread(data_long, key="series_code", value="values", fill=NA, convert=FALSE)

missing_years <- data_long[is.na(values),list(count=.N),by=list(year)][count<1500, year]

data_long <- data_long[year %in% missing_years]

data_spread <- spread(data_long, key="series_code", value="values", fill=NA, convert=FALSE)

data_detailed <- data_spread



data <- data_detailed[ , list(country_name,country_code,year, gov_exp_edu_tot_perc_gdp, pop_total, unemp_tot_ILO
                      , gdp_percap_const_2011_int, gdp_percap_growth_annual_perc,Life_exp)]

names(data) <- c(
  'country_name','country_code','year_code','edu_exp','pop','unemp','gdppc','gdppc_gr','Life_exp'
)

data <- data[ (!is.na(edu_exp)) & (!is.na(unemp)) 
                  & (!is.na(pop)) & (!is.na(gdppc)) ]
data[,pop := pop/10^6]
data[,country_code := factor(country_code)]


data[, year := substr(year_code,3,6)]


data <- data[year_code != 'yr1998']
data <- data[year_code != 'yr2014']
data <- data[year_code != 'yr2015']


ggplot(aes(x=edu_exp), data=data) +
  geom_histogram(bins = 35)


ggplot(aes(x=unemp), data=data) +
  geom_histogram(bins = 40)

data[unemp>20, list(country_name, country_code)]

# nonmissing years for each country

data[, COUNT := .N , by = country_code]

data[, .N, by=year]
unique(data[COUNT==15,list(country_code,COUNT)])
unique(data[COUNT!=15,list(country_code,COUNT)])

## 47 complee country
## 91 countries unbalanced

ggplot(aes(x=unemp), data=data) +
  geom_histogram(bins = 35)+
  xlab('Unemploymet')

ccols1 <- lm(formula= unemp~ edu_exp, data=data[year_code=='yr2003'])
ccols1_test <- coeftest(ccols1, vcov=sandwich)

ccols2 <- lm(formula= unemp~ edu_exp + log(gdppc) + log(pop), data=data[year_code=='yr2003'])
ccols2_test <- coeftest(ccols2, vcov=sandwich)


stargazer(
  list(ccols1_test, ccols2_test), type = 'text', digits = 2, title = "Cross-country OLS for 2003"
)


ggplot(aes(edu_exp,unemp ), data=data[year_code == 'yr2003'] ) + 
  geom_point(size = 3, color = 'orange', shape=4 ) +
  geom_smooth(method = "lm",se=FALSE  ) +
  ylab('Unemployment') +
  xlab('Government expenditure on education, total (% of GDP)')




world_trend <- data[,lapply(mget(c("edu_exp","unemp","gdppc")) ,weighted.mean,w=pop), 
                      by = year]

world_trend[order(year),`:=`(reledu_exp = edu_exp - first(edu_exp),
                             rellngdp = log(gdppc) - first(log(gdppc))) ]


ggplot(aes(year, unemp), data = world_trend) + 
  geom_line(size = 1, color = 'darkgreen') + 
  ylab('average unemployment')

ggplot(aes(x = year), data = world_trend) +
  geom_line(aes(y = reledu_exp), size = 1, linetype = 'dotted', color = 'blue') +
  geom_line(aes(y = rellngdp), size = 1, linetype = 'dashed', color = 'firebrick') +
  geom_text(x = 2009, y = 0.7, label = 'Education expenditure', color = 'blue') +
  geom_text(x = 2009, y = 0.25, label = 'GDP', color = 'firebrick') +
  ylab('log change from 1995')

grid.arrange(p1, p2, nrow=1)

# Analysis ---------------------------------------------------------------------
data[edu_exp==0,edu_exp:=NA]
panel_data<- pdata.frame(data, index = c('country_name', 'year'))
panel_data


# First differences
diff1 <- plm(
  diff(unemp) ~ diff(edu_exp) + year, 
  data = panel_data, model = 'pooling'
)
#obtain clustered standard error
c1 <- coeftest(diff1, vcov=vcovHC(diff1,type="HC0",cluster="group"))
c1
rownames(c1)[2] <- c('deduexp')

diff2 <- plm(
  diff(unemp) ~ diff(edu_exp) + lag(diff(edu_exp), 2:2) + year,
  data = panel_data, model = 'pooling'
)
?plm

c2 <- coeftest(diff2, vcov=vcovHC(diff2,type="HC0",cluster="group"))
rownames(c2)[2:(2+2)] <- c('deduexp',paste('deduexp lag',1:2, sep = "" ))

c2

diff3 <- plm(
  diff(unemp) ~ diff(edu_exp) + lag(diff(edu_exp), 4:4) + year, 
  data = panel_data, model = 'pooling'
)
c3 <- coeftest(diff3, vcov=vcovHC(diff3,type="HC0",cluster="group"))
rownames(c3)[2:(2+4)] <- c('deduexp',paste('deduexp lag',1:4, sep = "" ))

c3

diff4 <- plm(
  diff(unemp) ~ diff(log(edu_exp)) +  lag(diff(log(edu_exp)), 6:6) + year,
  data = panel_data, model = 'pooling'
)

c4 <- coeftest(diff4, vcov=vcovHC(diff4,type="HC0",cluster="group"))

rownames(c4)[2:(2+6)] <- c('deduexp',paste('deduexp lag',1:6, sep = "" ))

c4

stargazer(
  list(c1, c2, c3, c4), type = 'text',
  omit = 'year', digits = 2, title = "FD with lags"
)


# Cumulative associations without controls
diff6 <- plm(diff(unemp) ~ lag(diff(edu_exp), 2) + lag(diff(diff(edu_exp)), 1:1) 
             + year, data = panel_data, model = 'pooling')

c6 <- coeftest(diff6, vcov=vcovHC(diff6,type="HC0",cluster="group"))

rownames(c6)[2:(2+2)] <- c('deduexp',paste('deduexp lag',1:2, sep = "" ))

diff7 <- plm(diff(unemp) ~ lag(diff(edu_exp), 4) + lag(diff(diff(edu_exp)), 3:3) + year,
             data = panel_data, model = 'pooling')

c7 <- coeftest(diff7, vcov=vcovHC(diff7,type="HC0",cluster="group"))


# FE
fe1 <- plm( 
  unemp ~ edu_exp, data = panel_data, 
  model = 'within', effect = 'twoways'
)

fe1_test <- coeftest(fe1, vcov=vcovHC(fe1,type="HC0",cluster="group"))

fe2 <- plm( 
  unemp ~ edu_exp + log(pop), data = panel_data, 
  model = 'within', effect = 'twoways'
)
fe2_test <- coeftest(fe2, vcov=vcovHC(fe2,type="HC0",cluster="group"))

fe3 <- plm( 
  unemp ~ edu_exp + log(pop) + log(gdppc), data = panel_data, 
  model = 'within', effect = 'twoways'
)
fe3_test <- coeftest(fe3, vcov=vcovHC(fe3,type="HC0",cluster="group"))

stargazer(
  list(fe1_test, fe2_test, fe3_test), type = 'text', digits = 2, title = "FIXED EFFECTS"
)

#Long run
panel_data2 <- pdata.frame(data[(year==2013)|(year==1999)], 
                             index = c('country_name', 'year'))

lr1 <- plm(
  diff(unemp) ~ diff(edu_exp) + diff(log(pop)) + diff(log(gdppc)), 
  data = panel_data2, model = 'pooling'
)
lr1_test <- coeftest(lr1, vcov=vcovHC(lr1,method="white1"))
rownames(lr1_test)[2:3] <- c("Edu exp, change 1999 to 2013", "ln population, change 1999 to 2013")
stargazer(
  lr1_test, type = 'text', digits = 2, title = "Long differences"
)
