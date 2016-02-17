scr_get_ML_scr <- function(data_stan,init.f,cfg){
  val = init.f
  
  if (is.null(data_stan$nsubject)){#one subject detected
    scr = scr_model(x=data_stan$x,
                    onsets = data_stan$onset,
                    latency = rep(val$latency,data_stan$ntrial),
                    amp = val$amp_per_onset,
                    tau1 = val$tau1,
                    tau2= val$tau2)
    df = (data.frame(scr = data_stan$scr,scrML=scr,time=data_stan$x/(cfg$resample_to_fs),subject=cfg$subject))
    do = (data.frame(onset=data_stan$onset/cfg$resample_to_fs,subject=cfg$subject))
  }else{
    df = NULL
    do = NULL
    for (s in 1:data_stan$nsubject){
      if((s%%5)==0)print(s)
      #browser()
      if (s==1){
        xstart=0
        onsetstart=0
      }else{  
        xstart = sum(data_stan$ntime_per_subject[1:(s-1)]) 
        onsetstart = sum(data_stan$nonset[1:(s-1)])
      }
     
  #    browser() 
      timeidx = (xstart+1):(xstart+data_stan$ntime_per_subject[s])
      onsetidx = (onsetstart+1):(onsetstart+data_stan$nonset[s])
      if(length(onsetidx)!= data_stan$nonset[s] ||length(timeidx)!= data_stan$ntime_per_subject[s]) browser()
     
      scr = scr_model(x=data_stan$x_per_subject[timeidx],
                      onsets = data_stan$onset[onsetidx],
                      latency = rep(val$latency[s],data_stan$nonset[s]),
                      amp = val$amp_per_onset[onsetidx],
                      tau1 = val$tau1[s],
                      tau2= val$tau2[s])
      
      df = rbind(df,(data.frame(scr = data_stan$scr[timeidx],scrML=scr,time=data_stan$x[timeidx]/(cfg$resample_to_fs),subject=s)))
      do = rbind(do,(data.frame(onset=data_stan$onset[onsetidx]/cfg$resample_to_fs,subject=s)))
    }
    
    
  }
  
  
  
  return(list(scr=reshape2::melt(df,measure.vars=c('scr','scrML'),variable.name='type',value.name='scr'),onset=do))
}