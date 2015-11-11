library(dplyr)
library(ggplot2)
library(stringr)
library(bigrquery)
library(utils)
library(tidyr)
data(iris)
ir  = tbl_df(iris)
irs = arrange(iris, Petal.Length)
View(irs) 
count(iris, Species)
group_by(iris, Petal.Width)
res = ir %>% group_by(Species) %>% summarise(mean = mean(Petal.Width), median = median(Petal.Width))
res

# ml_lit tests
db = src_sqlite('~/book/ml_lit/all_refs.sqlite3')
tab = tbl(db, sql('select TC, PY, TI from basic_refs limit 1000'))
tab %>% group_by(PY) %>% summarise(max = max(TC), mean = mean(TC))
tab_full = tbl(db, sql('select * from basic_refs'))
colnames(tab_full)
tab_full %>% group_by(anchor) %>% arrange(desc(TC)) %>% select(anchor, DE)
res3 = filter(tab_full, TC > 100, PY >2010) %>% arrange(TC)
collect(res3)
sc = group_by(tab_full, SC)
res2 = summarise(sc, tc_m = mean(TC), first = min(PY), pub_count = n())
collect(res2)
View(res2)
res2$query
head(res2)
tab_full <- tab_full %>% mutate(TI=TI, PY=PY, de = tolower(DE), sc = tolower(SC))
##separating

de = tab_full %>% select(de) %>% collect %>% separate(sep='; ',col=de, into = c('d1', 'd2','d3', 'd4'), extra='drop')
sc = tab_full %>% select(sc) %>% collect %>% separate(sep='; ',col=sc, into = c('s1', 's2','s3', 's4'), extra='merge')
df = select(tab_full, TI, PY) %>% collect %>% bind_cols(sc, de)
table(df$s1) %>% sort(decreasing=TRUE) %>% head()
table(df$d1) %>% sort(decreasing=TRUE) %>% head()
table(df$d1, df$s1) 
dfg = df %>% filter(PY > 1950, PY <=2014)  %>% group_by(s1, s2, d1, d2, PY) %>% arrange(desc(PY)) %>% summarise(c1 = n(), c2 = n())
head(dfg)
ggplot(filter(dfg,c1>20),  aes(x=PY, alpha=0.4, group=s1, fill=s1)) + geom_density()
dfgl = gather(dfg, 'd', 'disc', c(s1, s2))
ggplot(filter(dfgl, c1 >20), aes(x=PY, group = disc, colour = disc)) + geom_freqpoly()
dfgll = gather(dfgl, 'd_k', 'key', c(d1, d2))
ggplot(filter(dfgll,  c1>2, grepl(key, pattern='feature')), aes(x=PY, group = key, linetype=key, colour = key)) + geom_density(binwidth=1)
glimpse(dfgll)

head(dfg1)

#bigquery

proj = 'metacommunities'
gh = src_bigquery(project ='githubarchive', dataset = 'year', billing=proj)
repos = gh %>% tbl('2014') %>% select(repository_url, repository_name, repository_created_at, type, created_at)
events = repos %>% group_by(repository_name)  %>% summarise(count = count(type)) %>% arrange(desc(count))
head(events)

repos %>% group_by(repository_url) %>% summarise(dist = n())
created = repos %>% arrange(repository_created_at)
summarise(repos, min = min(repository_created_at))
repos %>% group_by(repository_url,type) %>% summarise(typ = count(type)) %>% arrange(desc(typ))
head(created)
