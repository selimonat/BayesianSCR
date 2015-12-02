scr_stan_init<- function(resamp,subject=NULL){
  
  library(plyr)
  mlFit = read.csv('~/Documents/fancycarp/calibrationdata_56subjects_onsets.txt',header = F)
  colnames(mlFit) = c('idx','subject','latency','tau1','tau2','amp')
  mlFit$tau1 = resamp*mlFit$tau1
  mlFit$tau2 = resamp*mlFit$tau2
  mlFit$latency = resamp*mlFit$latency
  
  y =   ddply(mlFit,.(subject),summarise,l = mean(latency),t1=mean(tau1),t2=mean(tau2),a=mean(amp))
  
  
  if (is.null(subject)){
      l = list(
        m_tau1 = mean(y$t1),
        m_tau2 = mean(y$t2),
        m_latency = array(mean(y$l)),
        m_amp = array(mean(y$a)),
        m_scr_sigma = 0.7,
        s_tau1 = sd(y$t1),
        s_tau2 = sd(y$t2),
        s_latency = array(sd(y$l)),
        s_amp = array(sd(y$a)),
        s_scr_sigma = 0.5,
        tau1 = array(y$t1), #rep(mean(y$t1),length(y[,1])),#
        tau2 = array(y$t2),#rep(mean(y$t2),length(y[,1])),#
        latency = t(matrix(y$l,nrow=1)),
        
        amp = t(matrix(y$a,nrow=1)),
        scr_sigma = rep(0.7,length(y[,1])),
        amp_per_onset = matrix(mlFit$amp,nrow=1)
      )
    
  }else{#subject specific
       l = list(
        tau1 = y$t1[y$subject == subject], #rep(mean(y$t1),length(y[,1])),#
        tau2 = y$t2[subject],#rep(mean(y$t2),length(y[,1])),#
        latency = array(y$l[subject]),
        
        amp = array(y$a[subject]),
        scr_sigma = 0.7,
        amp_per_onset = matrix(mlFit$amp[mlFit$subject==subject],nrow=1)
        
      )

    
  }
  ix =l$tau1-l$tau2 < 0.1
  l$tau2[ix] = l$tau2[ix]*0.6
  
  
  return(l)
}