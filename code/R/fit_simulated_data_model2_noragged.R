
library(ggplot2)
library(rstan)
library(plyr)
source('generate_scr_data.R')
source('scr_model.R')
set.seed(3)
# Generate Fake Data
cfg = list(plotData = T,
           nsub = 10,
           nonsets = 3,
           ntime = 300)

data.onset = NULL
data.sim = NULL
for(sub in 1:cfg$nsub){
  # This data.frame defines when the onsets (randomly placed) and which conditions they are
  allonsets = c(0.15,1,2)*15+sub*45#
  #allonsets = sort(sample.int(0.1*cfg$ntime,1*cfg$nonsets)*8)
  
  data = data.frame(condition = c(rep(1,cfg$nonsets)),#,rep(2,cfg$nonsets)),
                    onset = c(onsetsA = allonsets[1:cfg$nonsets],
                              onsetsB = NULL))
  
  data.sim = rbind(data.sim,data.frame(
    
    scr     = generate_scr_data(sigmaSCR=0.06,nT       = cfg$ntime, # Generate the data with the givven onsets, and number of samples
                                onsetsA = data$onset[data$condition==1],
                                onsetsB = NULL),
    subject = sub,
    time    = 1:cfg$ntime))
  
  
  data.onset = rbind(data.onset,cbind(data,subject=sub))
}
# we repeat the first converging subject 
data.onset = ddply(data.onset,.(subject),function(x){tmp = data.onset[data.onset$subject==1,];tmp$subject=x$subject;return(tmp)})
data.sim = ddply(data.sim,.(subject),function(x){tmp = data.sim[data.sim$subject==1,];tmp$subject=x$subject;return(tmp)})
if(cfg$plotData) ggplot(data.sim,aes(x=time,y=scr,color=factor(subject)))+geom_point()+facet_grid(subject~.)

# Fit STAN Model

# 
# int<lower=0> nsubject;
# int nonset;#number of stimuli per subject
# int ntime; # number of timepoints per subject
# 
# matrix[nsubject,ntime] scr; # The SCR Data, concatenated for each subject
# vector[ntime] x; # A simple index from 1:ntime, contactenated for all subjects
# matrix[nsubject,nonset] onset; # the onsets, concatenated for each subject (we know which is which due to nonset)
data_stan_norag <- list(
  nsubject = cfg$nsub,
  nonset= cfg$nonsets, # number of onsets * nconditions * subject, for the simulation only!
  ntime = cfg$ntime,
  scr = t(matrix(data.sim$scr,ncol=cfg$nsub)),
  x= 1:cfg$ntime,
  onset = t(matrix(data.onset$onset,nrow=cfg$nonsets))
)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# 
# latA = 10+rnorm(1,0,1),
# latB = 15+rnorm(1,0,1),
# ampA = 6+rnorm(1,0,.1),
# ampB = 3+rnorm(1,0,.1),
# tau1 = 4+rnorm(1,0,.1),
# tau2 = 2+rnorm(1,0,.1),
# sigmaSCR = 0.05+rnorm(1,0,.1),

#model_2 <- stan_model(file = 'scr_model2.stan')
model_2_norag <- stan_model(file = 'scr_model_2_nonragged_nocondition.stan')
init.f_norag = function(chain_id){
  l = list(
    tau1 = rep(4,cfg$nsub),
    tau2 = rep(2,cfg$nsub),
    latency = rep(10,cfg$nsub),
    amp = rep(6,cfg$nsub),
    scr_sigma = rep(0.05,cfg$nsub)
  )
  return(l)
}

fit2 <- sampling(model_2,data = data_stan_norag, algorithm='NUTS',init =init.f_norag,iter = 300, chains = 1,refresh=1,control=list(adapt_delta=0.8),verbose=T)
#fit2 <- sampling(model_2,data = data_stan, algorithm='NUTS',init=init.f,iter = 500, chains = 1,refresh=10,control=list(adapt_delta=0.8))

summary(do.call(rbind, args = get_sampler_params(fit2, inc_warmup = TRUE)),
        digits = 2)
source('posterior_predictive.R')
traceplot(fit2,pars=c('m_amp','m_latency','m_tau1','m_tau2','s_amp','s_latency','s_tau1','s_tau2'),inc_warmup=T)
#a = posterior_predictive_single(fit2,1:cfg$ntime,onsets=data.onset,niter = 15)
#ggplot(data.frame(a),aes(x=time,y=scr))+geom_point()+facet_grid(subject~iter)
traceplot(fit2,pars=c('amp','latency','tau1','tau2'),inc_warmup=T)

# Evaluate Fit