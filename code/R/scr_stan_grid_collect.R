scr_stan_grid_collect <-function(name,combine_again=F){
  
  
  full.path = paste0('./','stanfit/')
  if (combine_again==F && file.exists(paste0(full.path,name,'.RData'))){
    show('Already combined file Found, loading it')
    load(paste0(full.path,name,'.RData'))
  }
  else{
    files = list.files(full.path,patter=paste0(name,'grid.[0-9A-Za-z]{10}.RData'))
    show(paste0('Loading ',length(files),' files'))
    fitList = NULL
    for(k in files){
      load(paste0(full.path,k))
      fitList = c(fitList,fit) 
    }
    fit = rstan::sflist2stanfit(fitList)
    show('Saving Fit')
    save(fit,file=paste0(full.path,name,'.RData'))
  }
  return(fit)
  
}
