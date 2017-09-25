## Collect catches by fleet:
lln.landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('HLN','LLN'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults))


bmt.landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('BMT','DRD','NPT','SHT','PGT'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults))

dse.landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('DSE','PSE'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults))

gil.landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear='GIL',
  sampling_type = 'LND',
  species = defaults$species),
  defaults))

foreign.landings <-
  mfdb_sample_totalweight(mdb, NULL,
                          c(list(
                            sampling_type = 'FLND',
                            data_source = c('lods.foreign.landings','statlant.foreign.landings'),
                            species = defaults$species),
                            defaults))
old.landings <-
  mfdb_sample_totalweight(mdb, NULL,
                          c(list(
                            data_source = c('old.landings'),
                            species = defaults$species),
                            defaults))


igfs.landings <- 
  structure(data.frame(year=defaults$year,step=2,area=1,number=1),
            area_group = mfdb_group(`1` = 1)) #shows 1 kg being taken


aut.landings <- 
  structure(data.frame(year=defaults$year,step=4,area=1,number=1),
            area_group = mfdb_group(`1` = 1)) #shows 1 kg being taken



## write to file
gadgetfleet('Modelfiles/fleet',gd$dir,missingOkay = TRUE) %>% 
  gadget_update('totalfleet',
                name = 'aut',
                suitability = paste0('\n',
                                     paste(c('hadimm','hadmat'),
                                           'function','exponentiall50',
                                           '#had.aut.alpha','#had.aut.l50',
                                           collapse='\n')),
                data = aut.landings) %>%
  gadget_update('totalfleet',
                name = 'igfs',
                suitability = paste0('\n',
                                     paste(c('hadimm','hadmat'),
                                           'function','exponentiall50',
                                           '#had.igfs.alpha','#had.igfs.l50',
                                           collapse='\n')),
                data = igfs.landings) %>%
  gadget_update('totalfleet',
                name = 'lln',
                suitability = 
                  paste0('\n',
                         paste(c('hadimm','hadmat'),
                               'function','exponentiall50',
                               '#had.lln.alpha','#had.lln.l50',
                               collapse='\n')),
                data = lln.landings[[1]]) %>% 
  gadget_update('totalfleet',
                name = 'bmt',
                suitability = paste0('\n',
                                     paste(c('hadimm','hadmat'),
                                           'function','exponentiall50',
                                           '#had.bmt.alpha','#had.bmt.l50',
                                           collapse='\n')),
                data = bmt.landings[[1]]) %>% 
  gadget_update('totalfleet',
                name = 'dse',
                suitability = paste0('\n',
                                     paste(c('hadimm','hadmat'),
                                           'function','exponentiall50',
                                           '#had.dse.alpha','#had.dse.l50',
                                           collapse='\n')),
                data = dse.landings[[1]]) %>% 
  gadget_update('totalfleet',
                name = 'gil',
                suitability = paste0('\n',
                                     paste(c('hadimm','hadmat'),
                                           'function','exponentiall50',
                                           '#had.gil.alpha','#had.gil.l50',
                                           collapse='\n')),
                data = gil.landings[[1]]) %>% 
  gadget_update('totalfleet',
                name = 'foreign',
                suitability = 
                  paste0('\n',
                         paste(c('hadimm','hadmat'),
                               'function','exponentiall50',
                               '#had.lln.alpha','#had.lln.l50',
                               #'function','andersenfleet',
                               #'#had.lln.p0',to.gadget.formulae(quote(log(180/had.lln.lmode))),'#had.lln.p2',
                               #'#had.lln.p3','#had.lln.p4','180',
                               collapse='\n')),
                data = foreign.landings[[1]]) %>%
  gadget_update('totalfleet',
                name = 'old',
                suitability = 
                  paste0('\n',
                         paste(c('hadimm','hadmat'),
                               'function','exponentiall50',
                               '#had.bmt.alpha','#had.bmt.l50',
                                collapse='\n')),
                data = old.landings[[1]]) %>% 
  write.gadget.file(gd$dir)
