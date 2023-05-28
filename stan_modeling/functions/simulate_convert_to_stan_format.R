simulate_convert_to_standata <-function (mydatatype,path,cfg,var_toinclude){

  source('./functions/make_mystandata.R')

  #browser()
  #load artificial data
  file_name = paste0(mydatatype, '_data.Rdata')
  load(paste0(path$data,'/', file_name))
  
  cat(file_name)
  
  #convert
  data_for_stan<-make_mystandata(data                 = df, 
                                 subject_column       = df$subject,
                                 block_column         = df$block,
                                 var_toinclude        = var_toinclude,
                                 additional_arguments = list(
                                   Narms  = cfg$Narms, 
                                   Nraffle= cfg$Nraffle))

  #save
  if (mydatatype=='artificial'){save(data_for_stan,file=paste0(path$data,'/artificial_standata.Rdata'))
   cat(paste0('[stan_modeling]:  "artificial_standata.Rdata" was saved at "',path$data,'"'))}
  
  
  # if (mydatatype=='empirical'){save(data_for_stan,file=paste0(path$data,'/empirical_standata.Rdata'))
  #   cat(paste0('[stan_modeling]:  "empirical_standata.Rdata" was saved at "',path$data,'"'))}
  
  else {save(data_for_stan,file=paste0(path$data,'/',mydatatype, '_empirical_standata.Rdata'))
    cat(paste0('[stan_modeling]:', mydatatype, '"empirical_standata.Rdata was saved at',path$data,"'"))}
  
}