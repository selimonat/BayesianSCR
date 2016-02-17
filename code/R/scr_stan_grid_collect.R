scr_stan_grid_collect <-function(cfg,date=NULL){
  source('scr_stan_fit_filename.R')
  cfg$filename = scr_stan_fit_filename(cfg,date)
  print(paste0('loading:',cfg$filename))
  full.path = './stanfit/'
  filename = basename(strsplit(cfg$filename,'grid')[[1]][1])
  files = list.files(full.path,patter=paste0(filename,'grid[0-9A-Za-z]{10}','.RData') )
  
  show(paste0('Loading ',length(files),' files'))
  fitList = NULL
  #browser()
  
  for(k in files){
    load(paste0(full.path,k))
    fitList = c(fitList,fit) 
  }
  fit = rstan::sflist2stanfit(fitList)
  
  return(fit)
  
}
