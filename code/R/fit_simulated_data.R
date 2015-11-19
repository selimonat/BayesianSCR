
library(ggplot2)
library(rstan)
source('generate_scr_data.R')
source('scr_model.R')
# Generate Fake Data
set.seed(1)
cfg = list(plotData = T,
           nsub = 1,
           nonsets = 2,
           ntime = 100)

set.seed(1)
data.sim = NULL
data.onset = NULL
for(sub in 1:cfg$nsub){
  allonsets = c(1,20,40,60)#sample.int(0.1*cfg$ntime,2*cfg$nonsets)*8
  
  data = data.frame(condition = c(rep(1,cfg$nonsets),rep(2,cfg$nonsets)),
                    onset = c(onsetsA = allonsets[1:cfg$nonsets],
                              onsetsB = allonsets[(cfg$nonsets+1):(cfg$nonsets*2)]))
  
  data.sim = rbind(data.sim,data.frame(
    scr=generate_scr_data(sigmaSCR=0.06,nT = cfg$ntime,
                          onsetsA = data$onset[data$condition==1],
                          onsetsB = data$onset[data$condition==2]),
    subject=sub,time=1:cfg$ntime))
  data.onset = rbind(data.onset,cbind(data,subject=sub))
}
if(cfg$plotData) ggplot(data.sim,aes(x=time,y=scr,color=factor(subject)))+geom_point()+facet_grid(subject~.)

# Fit STAN Model


data_stan <- list(
  ntime=cfg$ntime*length(unique(data.sim$subject)), # number of timepoints
  ntrial=length(unique(data.sim$subject))*cfg$nonsets*2, # number of onsets * nconditions
  ncondition = 2, # number of conditions
  scr = data.sim$scr,
  x = 1:length(data.sim$scr),
  onset = data.onset$onset, # 
  condition = data.onset$condition # which Condition [a/b]?
)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
init.f = function(chain_id){
  l = list(
    tau1 = 4,
    tau2 = 3,
    latency = c(10,10),
    amp = c(1,1),
    scr_sigma = 0.06
  )
  return(l)
}

model_1 <- stan_model(file = 'scr_model1.stan')
#model_1b <- stan_model(file = 'scr_model1b.stan')


#fit <- sampling(model_1,data = data_stan, algorithm='NUTS',iter = 200, chains = 1,refresh=1,pars=c('amp','latency','tau1','tau2','scr_sigma'),control=list(adapt_delta=0.5))
fit <- sampling(model_2,data = data_stan, algorithm='NUTS',iter = 200, chains = 1,refresh=1,pars=c('amp','latency','tau1','tau2'),control=list(adapt_delta=0.5))

traceplot(fit,pars=c('amp','latency','tau1','tau2'),inc_warmup=T)

# Evaluate Fit
source('posterior_predictive.R')
a = posterior_predictive_single(fit,1:cfg$ntime,onsets=data.onset)
ggplot(data.frame(a),aes(x=time,y=scr,group=iter))+geom_point()+facet_wrap(~iter,ncol = 10)
