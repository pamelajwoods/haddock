library(Rgadget)
tmp <- gadget.iterative(rew.sI=TRUE,
                        main='main',
                        grouping=list(sind1.i=c("si.i.10_15","si.i.15_21","si.i.21_28","si.i.28_36"),
                                      sind2.i=c("si.i.36_45","si.i.45_55","si.i.55_97"),
                                      sind1.a=c("si.a.10_15","si.a.15_21","si.a.21_28","si.a.28_36"),
                                      sind2.a=c("si.a.36_45","si.a.45_55","si.a.55_97"),
                                      comm1=c('ldist.igfs', 'aldist.igfs'),
                                      comm2=c('ldist.aut', 'aldist.aut'),
                                      comm3=c('ldist.bmt', 'aldist.bmt'),
                                      comm4=c('ldist.lln', 'aldist.lln'),
                                      comm5=c('ldist.dse', 'aldist.dse'),
                                      comm6=c('ldist.gil', 'aldist.gil')),
                        #cv.floor = 0.05,
                        resume.final = FALSE,
                        params.file = 'params.init',
                        wgts='WGTS')#,
                        #comp = c('ldist.aut', 'aldis.aut', indices...),inverse = TRUE) #throws out the aut survey results from likelihood

print('Running analytical retro')

gadget.retro(mainfile = 'WGTS/main.final',params.file = 'params.init')

