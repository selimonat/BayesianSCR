library(ggplot2)
library(rstan)
source('generate_scr_data.R')
# Generate Fake Data
cfg = list(plotData = T,
          nsub = 1,
          nonsets = 10,
          ntime = 1000)

data.sim = NULL
for(sub in 1:cfg$nsub){
data = data.frame(condition = c(rep(1,cfg$nonsets),rep(2,cfg$nonsets)),
               onset = c(sample.int(cfg$ntime,cfg$nonsets),onsetsB = sample.int(cfg$ntime,cfg$nonsets)))
data.sim = rbind(data.sim,data.frame(
            scr=generate_scr_data(nT = cfg$ntime,
            onsetsA = data$onset[data$condition==1],
            onsetsB = data$onset[data$condition==2]),
            subject=sub,time=1:cfg$ntime))
  
}
if(cfg$plotData) ggplot(data.sim,aes(x=time,y=scr,color=factor(subject)))+geom_point()+facet_grid(subject~.)

# Fit STAN Model

# int ntrial;#number of stimuli
# int ntime; # number of timepoints
# int ncondition; # number of conditions
# real scr[ntime];
# vector<lower=1>[ntrial] onset; # 
# vector<lower=1,upper=2>[ntrial] condition; # which Condition [a/b]?

data_stan <- list(
  ntime=cfg$ntime, # number of timepoints
  ntrial=cfg$nonsets*2, # number of onsets * nconditions
  ncondition = 2, # number of conditions
  scr = data.sim$scr,
  onset = data$onset, # 
  condition = data$condition # which Condition [a/b]?
)

#model_2_wavelength <- stan_model(file = 'model_2_wavelength.stan')
model_1a <- stan_model(file = 'scr_model1.stan')
#model_1c <- stan_model(file = 'model_1_twoColor_multiTrial_hierarchical.stan')
fit <- sampling(model_1a,data = data_stan, algorithm='NUTS',
                iter = 500, chains = 1,refresh=1)

traceplot(fit,pars=c('amp','latency','tau1','tau2'))

# Evaluate Fit