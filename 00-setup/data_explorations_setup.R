
#determining how to structure fleets

all.landings <- collect(mfdb_sample_totalweight(mdb, c('gear'), c(list(
gear = as.character(mfdb::gear$name),
sampling_type = 'LND',
species = defaults$species),
defaults))[[1]])
View(mfdb::gear)
gear_summary<-aggregate(all.landings$total_weight, by = list(all.landings$gear), sum)
sum(gear_summary[11:12,2])/sum(gear_summary[,2])
sum(gear_summary[4,2])/sum(gear_summary[,2])
sum(gear_summary[c(3,9),2])/sum(gear_summary[,2])
sum(gear_summary[c(5,6),2])/sum(gear_summary[,2])
sum(gear_summary[c(8),2])/sum(gear_summary[,2])



#determining how indices should be split by length group

bins<-seq(10,100,2)

igfs_cdat<-mfdb_sample_count(mdb, 
                  c('length'), 
                  c(list(
                  data_source = 'iceland-ldist',
                  sampling_type = 'IGFS',
                  length = mfdb_interval("len", c(100,160))),
                  defaults))[[1]][0,]


for(i in 1:(length(bins)-1))
{
  igfs.temp <- 
  mfdb_sample_count(mdb, 
                  c('length'), 
                  c(list(
                    data_source = 'iceland-ldist',
                    sampling_type = 'IGFS',
                    length = mfdb_interval("len", c(bins[i],bins[i+1]))),
                    defaults))[[1]]
igfs_cdat<-rbind(igfs_cdat,igfs.temp)  

}
igfs.cdat<-as.tibble(igfs_cdat)


aut_cdat<-mfdb_sample_count(mdb, 
                             c('length'), 
                             c(list(
                               data_source = 'iceland-ldist',
                               sampling_type = 'AUT',
                               length = mfdb_interval("len", c(100,160))),
                               defaults))[[1]][0,]


for(i in 1:(length(bins)-1))
{
  aut.temp <- 
    mfdb_sample_count(mdb, 
                      c('length'), 
                      c(list(
                        data_source = 'iceland-ldist',
                        sampling_type = 'AUT',
                        length = mfdb_interval("len", c(bins[i],bins[i+1]))),
                        defaults))[[1]]
  aut_cdat<-rbind(aut_cdat,aut.temp)  
  
}
aut.cdat<-as.tibble(aut_cdat)


library(tidyverse)

igfs.cdat %>% 
  mutate(length = gsub('len','',length) %>% as.numeric()) %>% 
  ggplot(aes(length, number/1e3)) + geom_line() + 
  facet_wrap(~year,scale='free_y') + 
  #geom_vline(xintercept = 13) +
  geom_vline(xintercept = 18) +
  geom_vline(xintercept = 24) + 
  geom_vline(xintercept = 31) +
  geom_vline(xintercept = 39) +
  geom_vline(xintercept = 48) +
  geom_vline(xintercept = 58)

ggsave('Indices.pdf')

off<-3
aut.cdat %>% 
  mutate(length = gsub('len','',length) %>% as.numeric()) %>% 
  ggplot(aes(length, number/1e3)) + geom_line() + 
  facet_wrap(~year,scale='free_y') + 
  #geom_vline(xintercept = 13) +
  geom_vline(xintercept = 18-off) +
  geom_vline(xintercept = 24-off) + 
  geom_vline(xintercept = 31-off) +
  geom_vline(xintercept = 39-off) +
  geom_vline(xintercept = 48-off) +
  geom_vline(xintercept = 58-off)

ggsave('AutumnIndices.pdf')


#age data that are probably wrong: largest 100 sizes  per age group
#
istemp<-filter(init.sigma, species == defaults$species,age >0,!is.na(length))
istemp2<-group_by(collect(select(istemp,age,length), n=Inf), age)
o<-order(istemp2$length[istemp2$age==1])
o2<-order(istemp2$length[istemp2$age==2])
o3<-order(istemp2$length[istemp2$age==3])
o4<-order(istemp2$length[istemp2$age==4])
o5<-order(istemp2$length[istemp2$age==5])
plot(istemp2$length[istemp2$age==1][rev(o)][1:1000], ylim = c(30, 100))
points(istemp2$length[istemp2$age==2][rev(o2)][1:1000], col=2)
points(istemp2$length[istemp2$age==3][rev(o3)][1:1000], col=3)
points(istemp2$length[istemp2$age==4][rev(o4)][1:1000], col=4)
points(istemp2$length[istemp2$age==5][rev(o5)][1:1000], col=5)
#could use max length cutoffs:
istemp2$length[istemp2$age==1][rev(o)][100] #33
istemp2$length[istemp2$age==2][rev(o2)][100] #48
istemp2$length[istemp2$age==3][rev(o3)][100] #60
istemp2$length[istemp2$age==4][rev(o4)][100] #71


testage0.igfs<- 
  mfdb_dplyr_sample(mdb) %>% 
  dplyr::filter(species == defaults$species,age >= 0,!is.na(length), sampling_type=='IGFS') %>% 
  dplyr::select(age,length) %>% 
  dplyr::collect(n=Inf) %>% 
  dplyr::group_by(age) %>% 
  dplyr::summarise(n=n(), ml=mean(length,na.rm=TRUE))
#  dplyr::summarise(ml=mean(length,na.rm=TRUE),ms=sd(length,na.rm=TRUE))
testage0.aut<- 
  mfdb_dplyr_sample(mdb) %>% 
  dplyr::filter(species == defaults$species,age >= 0,!is.na(length), sampling_type=='AUT', data_source = 'iceland-aldist') %>% 
  dplyr::select(age,length) %>% 
  dplyr::collect(n=Inf) %>% 
  dplyr::group_by(age) %>% 
  dplyr::summarise(n=n(), ml=mean(length,na.rm=TRUE))


## mean lengths by timestep and age
ml.step <- 
  mfdb_dplyr_sample(mdb) %>% 
  dplyr::filter(species == defaults$species,age >= 0,!is.na(length),
                gear == 'BMT',sampling_type == 'SEA') %>% 
  dplyr::select(age,length,month) %>% 
  dplyr::collect(n=Inf) %>% 
  #dplyr::mutate(step=ifelse(month<4, 1, ifelse(month>3 & month <7, 2, ifelse(month>6 & month<10, 3, 4)))) %>% 
  dplyr::mutate(step=ifelse(month<3, 1, ifelse(month>2 & month <5, 2, ifelse(month>4 & month<7, 3, ifelse(month>6 & month<9, 4, ifelse(month>8 & month<11, 5, 6)))))) %>% 
  dplyr::group_by(age, step) %>% 
  dplyr::summarise(ml=mean(length,na.rm=TRUE))%>%
  dplyr::ungroup() %>%
  dplyr::mutate(dl = c(ml[-1]-ml[-length(ml)], 0))

View(ml.step)

