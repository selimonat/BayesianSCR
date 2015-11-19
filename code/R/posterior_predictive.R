posterior_predictive_single = function(params_org,x,niter=100,onsets,debug=F){
  if(class(params_org)=='stanfit') params=extract(params_org,pars=c('amp','latency','tau1','tau2'))
  params = data.frame(params)
  nOnsets = length(unlist(onsets))#nT
  nMCCSamples= dim(params$tau1)
  
  dat.all = NULL
  
  iterIdx = sample.int(nMCCSamples,niter)
  
  
  dat.all  =NULL
  
  for(i in 1:niter){
    it = iterIdx[i]
    dat.all = rbind(dat.all,cbind(iter=it,time=x,
                                  scr = scr_model(x,onsets = onsets$onset,latency=c(rep(params$latency.1[it],sum(onsets$condition==1)),
                                                                                    rep(params$latency.2[it],sum(onsets$condition==2))),
                                                  
                                                  amp = c(rep(params$amp.1[it],    sum(onsets$condition==1)),
                                                          rep(params$amp.2[it],    sum(onsets$condition==2))),
                                                  tau1=params$tau1[it],
                                                  tau2=params$tau2[it])
    ))
  }
  
  
  return(dat.all)
}



posterior_predictive_group = function(params_org,x,niter=100,nsubjects=10,onsets,debug=F){
  if(class(params_org)=='stanfit') params=extract(params_org,pars=c('m_amp','s_amp','m_latency','s_latency','m_tau1','s_tau1','m_tau2','s_tau2','m_scr_sigma','s_scr_sigma'))
  params = data.frame(params)
  nOnsets = length(unlist(onsets))#nT
  nMCCSamples= dim(params$m_tau1)
  
  dat.all = NULL
  
  iterIdx = sample.int(nMCCSamples,niter)
  
  params = params[,c(1,3,2,4,5,7,6,8:length(params[1,]))] 
  nameList = names(params)
  for(n in seq(1,length(nameList),2)){
    sub_niter = NULL  
    for(sub in 1:nsubjects){
      val = rnorm(rep(1,niter),params[iterIdx,nameList[n]], params[iterIdx,nameList[n+1]])
      if(nameList[n] == 'm_scr_sigma' || nameList[n] == 'amp.1' || nameList[n] == 'amp.2'||nameList[n] == 'm_tau1' || nameList[n] == 'm_tau2'){ # we need to make sure, that m_scr_sigma is positive!
        while(any(val<=0)){
          val[val<=0]=rnorm(rep(1,sum(val<=0)),params[iterIdx[val<=0],nameList[n]], params[iterIdx[val<=0],nameList[n]])    
        }
      }
      
      
      
      if(nameList[n] == 'm_tau2'){ # we need to make sure, that tau2 is smaller than tau1
        while(any(val>=tau1_samp[sub,])){
          val[val>=tau1_samp[sub,]]=rnorm(rep(1,sum(val>=tau1_samp[sub,])),params[iterIdx[val>=tau1_samp[sub,]],nameList[n]], params[iterIdx[val>=tau1_samp[sub,]],nameList[n]])    
          while(any(val<=0)){
            val[val<=0]=rnorm(rep(1,sum(val<=0)),params[iterIdx[val<=0],nameList[n]], params[iterIdx[val<=0],nameList[n]])    
          }
        }
      }
      sub_niter = rbind(sub_niter,val)
    }
    eval(parse(text = paste0(substr(nameList[n],3,nchar(nameList[n])),'_samp=sub_niter')))
    
  }
  
  
  dat.all  =NULL
  for(sub in 1:nsubjects){
    show(sprintf('subject  %i',sub))
    for(it in 1:niter){
      
      dat.all = rbind(dat.all,cbind(subject =sub,iter=it,time=x,
                                    scr=scr_model(x,onsets = onsets$onset[onsets$subject==sub],latency = c(rep(latency.1_samp[sub,it],sum(onsets$condition[onsets$subject==sub]==1)),
                                                                                                           rep(latency.2_samp[sub,it],sum(onsets$condition[onsets$subject==sub]==2))),
                                                  amp = c(rep(amp.1_samp[sub,it],    sum(onsets$condition[onsets$subject==sub]==1)),
                                                          rep(amp.2_samp[sub,it],    sum(onsets$condition[onsets$subject==sub]==2))),
                                                  tau1=tau1_samp[sub,it],
                                                  tau2=tau2_samp[sub,it])
      ))
    }
  }
  
  return(dat.all)
}