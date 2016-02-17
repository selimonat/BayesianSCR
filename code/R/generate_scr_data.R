generate_scr_data = function(nT=1000,
                             nOnsets = 10,
                             m_latency = 2.5,
                             m_amp = 0.5,
                             m_amp_sigma = 0.5,
                             m_tau1 = 20,
                             m_tau2 = 2.5,
                             m_scr_sigma = 0.01,
                             s_latency=1.5,
                             s_amp=0.4,
                             s_tau1=16,
                             s_tau2=3,
                             s_amp_sigma=0.5,
                             s_scr_sigma = 0.01,
                            onsets = NULL,
                            latency = NULL,
                            amp = NULL,
                            amp_sigma = NULL,
                            amp_per_onset=NULL,
                            tau1 = NULL,
                            tau2 = NULL,
                            scr_sigma = NULL,
                            debug=F){
  source('scr_model.R')
  

  if(is.null(onsets)) onsets = sample.int(nT,nOnsets)
  if(is.null(latency)) latency = m_latency+rnorm(1,0,s_latency)
  if(is.null(amp)) amp = m_amp +rnorm(1,0,s_amp)
  if(is.null(amp_sigma)) amp_sigma = m_amp_sigma +rnorm(1,0,s_amp_sigma)
  
  
  if(is.null(tau1)) tau1 = m_tau1 +rnorm(1,0,s_tau1)
  if(is.null(tau2)) tau2 = m_tau2 +rnorm(1,0,s_tau2)
  if(is.null(scr_sigma)) scr_sigma = m_scr_sigma +rnorm(1,0,s_scr_sigma)

  
  while(amp_sigma <= 0){
    amp_sigma = m_amp_sigma +rnorm(1,0,s_amp_sigma)
  }
  while(latency < 0){
    m_latency+rnorm(1,0,s_latency)
  }
  while(scr_sigma <= 0){
    scr_sigma = m_scr_sigma +rnorm(1,0,s_scr_sigma)
  }
  while(tau1 <= 0){
    tau1 = m_tau1 +rnorm(1,0,s_tau1)
  }
  while(tau2 >= tau1 | tau2<=0){
    tau2 = m_tau2 +rnorm(1,0,s_tau2)
  }
  
  
  if(is.null(amp_per_onset)) amp_per_onset = amp +rnorm(onsets,0,amp_sigma)
  while(any(amp_per_onset<=0)){
    amp_per_onset = amp +rnorm(onsets,0,amp_sigma)
  }
  
  #browser()
  scr_trial = scr_model(x = 1:nT,
                        onsets = onsets,
                        amp = amp_per_onset,
                        latency = rep(latency,length(onsets)),
                        tau1 = tau1,
                        tau2 = tau2)
  l = list(scr = scr_trial+rnorm(nT,0,scr_sigma),
       init.f = list( nOnsets = nOnsets,
                      m_latency = array(m_latency),
                      m_amp = array(m_amp),
                      m_amp_sigma = array(m_amp_sigma),
                      m_tau1 = m_tau1,
                      m_tau2 = m_tau2,
                      m_scr_sigma = m_scr_sigma,
                      s_latency=array(s_latency),
                      s_amp=array(s_amp),
                      s_tau1=s_tau1,
                      s_tau2=s_tau2,
                      s_amp_sigma= array(s_amp_sigma),
                      s_scr_sigma = s_scr_sigma,
                      onsets = onsets,
                      latency = t(matrix(latency,nrow=1)),
                      amp = array(amp),
                      amp_sigma = array(amp_sigma),
                      amp_per_onset=t(matrix(amp_per_onset,nrow=1)),
                      tau1 = tau1,
                      tau2 = tau2,
                      scr_sigma = scr_sigma))
  return(l)
}