scr_stan_data<-function(data.scr,data.scr.resamp,subselect = NULL,cfg){
  
  if (!is.null(subselect)){
    data.scrSL = subset(data.scr,data.scr$subject==subselect)
    data.scrSL.resamp = subset(data.scr.resamp,data.scr.resamp$subject==subselect)
    
    data_stan <- list(
      ntime=length(data.scrSL.resamp$time), # number of timepoints
      ntrial=sum(data.scrSL$onset), # number of onsets * nconditions
      ncondition = 1, # number of conditions
      scr = data.scrSL.resamp$scr,
      x = 0:(length(data.scrSL.resamp$scr)-1),
      onset = which(data.scrSL$onset==1)/(cfg$orgsamplingrate/cfg$resample_to_fs), # 
      condition = rep(1,sum(data.scrSL$onset)) # which Condition [a/b]?
    )
  }else{
    
    data_stan <- list(
      nsubject = length(unique(data.scr.resamp$subject)),
      nonset=(as.numeric(daply(data.scr,.(subject),summarise,sum(onset)))), # number of onsets * nconditions * subject, for the simulation only!
      ncondition = 1, # number of conditions
      ntime_per_subject =(as.numeric(daply(data.scr.resamp,.(subject),summarise,length(time)))),
      scr = data.scr.resamp$scr,
      x_per_subject =unlist(dlply(data.scr.resamp,.(subject),function(x){1:length(x$time)})),
      onset = unlist(dlply(data.scr,.(subject),function(x){which(x$onset==1)}))/(cfg$orgsamplingrate/cfg$resample_to_fs), #50 because we downsampled from 50 to 1
      condition = unlist(dlply(data.scr,.(subject),function(x){rep(1,sum(x$onset))}))
    )
    
  }
  return(data_stan)
}