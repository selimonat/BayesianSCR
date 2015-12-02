scr_stan_fit_filename<-function(cfg){
  date = format(Sys.time(), '%Y-%m-%d')
  
  subject = ifelse(!is.null(cfg$subject),paste0('sub',cfg$subject),'')
  
  grid = paste0('grid',paste0(sample(c(0:9, letters, LETTERS),10,replace=TRUE),collapse=''))
  
  adapt = ifelse(!is.null(cfg$adapt_delta),paste0('adaptdelta',cfg$adapt_delta),'')
  
  model = strsplit(cfg$model,'\\.')[[1]][1]
  
  name= paste(date,model,subject,adapt,grid,sep = '_')
  
  dir = './stanfit/'
  dir.create(file.path(dir), showWarnings = F)
  
  return(paste0(dir,name,'.RData'))
  
}