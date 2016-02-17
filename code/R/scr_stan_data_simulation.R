scr_stan_data_simulation<-function(data.scr,subselect = -99,cfg){
  
  if(subselect!=-99)stop('subselect is deprecated')
  
  if (is.null(cfg$subject)|length(cfg$subject)!=1){
    print('multisubject found')
    #stop('not implemented')
    #rowser()
    
    data_stan <- list(
      nsubject = length(unique(cfg$subject)),
      nonset = laply(data.scr[,1],.fun=function(x)length(x)),
      ncondition = 1, # number of conditions
      ntime_per_subject =laply(data.scr[,2],.fun=function(x)length(x)),
      scr = as.vector(unlist(data.scr[,2])),
      x_per_subject =as.vector(unlist(llply(data.scr[,2],.fun=function(x)1:length(x)))),
      onset =as.vector(unlist(data.scr[,1])),
      condition = rep(1,length(unlist(data.scr[,1])))
    )
    
    
  }else{
    print('single subject found')
    
    data_stan <- list(
      ntime=length(data.scr$scr), # number of timepoints
      ntrial=length(data.scr$onset), # number of onsets * nconditions
      ncondition = 1, # number of conditions
      scr = data.scr$scr,
      x = 0:(length(data.scr$scr)-1),
      onset = data.scr$onset,
      condition = rep(1,length(data.scr$onset)) # which Condition [a/b]?
    )
  }
  return(data_stan)
}