scr_get_stan_data<- function(cfg){
  source('scr_get_ML_scr.R')
  source('scr_stan_init.R')
  source('scr_stan_data.R')
  source('scr_model.R')
  source('scr_load_data.R')
  
  #load the data and resample it
  out = scr_load_data(plot=cfg$plot,resample_to_fs = cfg$resample_to_fs)
  data.scr = out$data.scr
  data.scr.resamp = out$data.scr.resamp

  if(!is.null(cfg$subject)&length(cfg$subject)!=1){
    # do subselection of subjects
    data.scr = subset(data.scr,data.scr$subject %in% cfg$subject)
    data.scr.resamp = subset(data.scr.resamp,data.scr.resamp$subject %in% cfg$subject)
    #cfg$subject = NULL
  }
  
  # generate the appropriate data_stan structure
  data_stan = scr_stan_data(data.scr,data.scr.resamp,cfg = cfg)
  
  #browser()
  # plot one subject if neccesary
  if (cfg$plot){
   if(is.null(cfg$subject) | length(cfg$subject)!=1){
     #multisubject
    # p = ggplot(data.frame(x=data_stan$x_per_subject[1:data_stan$ntime_per_subject[1]],scr=data_stan$scr[1:data_stan$ntime_per_subject[1]]))+geom_line(aes(x=x,y=scr))+
    #   geom_vline(data=data.frame(),aes(x=NULL,y=NULL,xintercept=data_stan$onset[1:data_stan$nonset[1]]))
     
   }else{
    p = ggplot(data=data.frame(x=data_stan$x,scr=data_stan$scr))+
      geom_line(aes(x=x,y=scr))+
      geom_vline(color='red',data=data.frame(onset=data_stan$onset),aes(xintercept=onset))
    show(p)
    
   }
  }
  
  # get the ML fit initial values
  init.f=scr_stan_init(resamp=cfg$resample_to_fs,subject=cfg$subject)
  if(cfg$plot){

    x = scr_get_ML_scr(data_stan,init.f,cfg)
    
    p = ggplot(x$scr,aes(x=time,y=scr,colour=type))+geom_line()+facet_wrap(~subject,scales ='free',ncol = 4)+
      geom_vline(data=x$onset,aes(x=NULL,y=NULL,xintercept=onset))
    show(p)
    
  }
  
  return(list(data.scr= data.scr,data.scr.resamp= data.scr.resamp,data_stan=data_stan,init.f = init.f))
}



