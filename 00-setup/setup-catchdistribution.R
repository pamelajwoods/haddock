minage <- had.imm[[1]]$minage
maxage <- had.mat[[1]]$maxage
maxlength <- had.mat[[1]]$maxlength 
minlength <- had.imm[[1]]$minlength
dl <- had.imm[[1]]$dl

## Query length data to create AUT catchdistribution components
ldist.aut <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'AUT',
                      age = mfdb_interval("all",c(minage,maxage),
                                       open_ended = c("upper","lower")),
                      length = mfdb_interval("len", 
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))

# for(i in seq_along(ldist.aut)){
#   attributes(ldist.aut[[i]])$age$all <- minage:maxage
#   attr(attributes(ldist.aut[[i]])$length$len0,'min') <- minlength
# }


## Age AUT
aldist.aut <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'),
                    c(list(sampling_type = 'AUT',
                           data_source = 'iceland-aldist',
                           age = mfdb_step_interval('age',by=1,from=minage,to=maxage,
                                                    open_ended = "upper"),
                           length = mfdb_interval("len", 
                                                  seq(minlength, maxlength, by = dl),
                                                  open_ended = c("upper","lower"))),
                      defaults))
# for(i in seq_along(aldist.aut)){
#   attr(attributes(aldist.aut[[i]])$length$len0,'min') <- minlength
# }


#POSSIBLY THIS WILL NOT BE USED
matp.aut <- 
  mfdb_sample_count(mdb, c('maturity_stage','age','length'),
                    append(defaults,
                           list(sampling_type='AUT',
                                age=mfdb_group(mat_ages=minage:maxage),
                                length = mfdb_interval('len',
                                                       seq(minlength, maxlength, by = 2*dl),
                                                       open_ended = c('lower','upper')),              
                                maturity_stage = mfdb_group(hadimm = 1, hadmat = 2:5))))


## Query length data to create IGFS catchdistribution components
ldist.igfs <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len", 
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))

# for(i in seq_along(ldist.igfs)){
#   attributes(ldist.igfs[[i]])$age$all <- minage:maxage
#   attr(attributes(ldist.igfs[[i]])$length$len0,'min') <- minlength
# }


## Age IGFS
aldist.igfs <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'),
                    c(list(sampling_type = 'IGFS',
                           data_source = 'iceland-aldist',
                           age = mfdb_step_interval('age',by=1,from=minage,to=maxage,
                                                    open_ended = "upper"),
                           length = mfdb_interval("len", 
                                                  seq(minlength, maxlength, by = dl),
                                                  open_ended = c("upper","lower"))),
                      defaults))
# for(i in seq_along(aldist.igfs)){
#   attr(attributes(aldist.igfs[[i]])$length$len0,'min') <- minlength
# }

matp.igfs <- 
  mfdb_sample_count(mdb, c('maturity_stage','age','length'),
                    append(defaults,
                           list(sampling_type='IGFS',
                                age=mfdb_group(mat_ages=minage:maxage),
                                length = mfdb_interval('len',
                                                       seq(minlength, maxlength, by = 2*dl),
                                                       open_ended = c('lower','upper')),              
                                maturity_stage = mfdb_group(hadimm = 1, hadmat = 2:5))))

## Query length data to create SEA lln catchdistribution components
ldist.lln <- 
  mfdb_sample_count(mdb, 
                    c('age', 'length'), 
                    c(list(
                      sampling_type = 'SEA',
                      data_source = 'iceland-ldist',
                      gear = c('LLN','HLN'),
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len", 
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))


aldist.lln <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'),
                    c(list(sampling_type = 'SEA',
                           data_source = 'iceland-aldist',
                           gear = c('LLN','HLN'),
                           age = mfdb_step_interval('age',by=1,from=minage,to=maxage,
                                                    open_ended = c("upper")),
                           length = mfdb_interval("len", 
                                                  seq(minlength, maxlength, by = dl),
                                                  open_ended = c("upper","lower"))),
                      defaults))

## Query length data to create SEA bmt catchdistribution components
ldist.bmt <- 
  mfdb_sample_count(mdb,
                    c('age', 'length'), 
                    c(list(
                      sampling_type = 'SEA',
                      data_source = 'iceland-ldist',
                      gear=c('BMT','DRD','NPT','SHT','PGT'),
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len", 
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))

aldist.bmt <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'),
                    c(list(sampling_type = 'SEA',
                           data_source = 'iceland-aldist',
                           gear=c('BMT','DRD','NPT','SHT','PGT'),
                           age = mfdb_step_interval('age',by=1,from=minage,to=maxage,
                                                    open_ended = c("upper")),
                           length = mfdb_interval("len", 
                                                  seq(minlength, maxlength, by = dl),
                                                  open_ended = c("upper","lower"))),
                      defaults))

## Query length data to create SEA dse catchdistribution components
ldist.dse <- 
  mfdb_sample_count(mdb,
                    c('age', 'length'), 
                    c(list(
                      sampling_type = 'SEA',
                      data_source = 'iceland-ldist',
                      gear=c('DSE','PSE'),
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len", 
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))

aldist.dse <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'),
                    c(list(sampling_type = 'SEA',
                           data_source = 'iceland-aldist',
                           gear=c('DSE','PSE'),
                           age = mfdb_step_interval('age',by=1,from=minage,to=maxage,
                                                    open_ended = c("upper")),
                           length = mfdb_interval("len", 
                                                  seq(minlength, maxlength, by = dl),
                                                  open_ended = c("upper","lower"))),
                      defaults))

## Query length data to create SEA gil catchdistribution components
ldist.gil <- 
  mfdb_sample_count(mdb, c('age', 'length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'SEA',
                      gear='GIL',
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("lower")),
                      length = mfdb_interval("len", 
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))


aldist.gil <-
  mfdb_sample_count(mdb, c('age', 'length'),
                    c(list(sampling_type = 'SEA',
                           data_source = 'iceland-aldist',
                           gear='GIL',
                           age = mfdb_step_interval('age',by=1,from=minage,to=maxage,
                                                    open_ended = c("upper")),
                           length = mfdb_interval("len", 
                                                  seq(minlength, maxlength, by = dl),
                                                  open_ended = c("upper","lower"))),
                      defaults))
