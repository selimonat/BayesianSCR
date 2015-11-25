scr_load_data <- function(plotData=F,resample_to_fs=10,filename='~/Documents/scr_2/calibrationdata_56subjects.txt'){
  library(plyr)
  library(ggplot2)
cfg = list(plotData = plotData)

data.scr = data.frame(read.csv(file = filename))
colnames(data.scr) = c('subject','scr','onset','time')

data.scr$time=data.scr$time/1000

# get same 0-time point and trans
data.scr = ddply(data.scr,.(subject),function(x){minT = min(x$time);x$time = x$time-minT;return(x)})

data.scr = ddply(data.scr,.(subject),function(x){tmp = which(x$onset==1);return(subset(x,x$time<x$time[tmp[length(tmp)]]+20))})

same_sampling_rate=function(data,fs_target=NULL){
  ddply(data,.(subject),function(x){
    fs_org =1/median(diff(x$time),na.rm = T)
    minT = min(x$time) # in s
    maxT = max(x$time) # in s
    f <- approxfun(x$time,x$scr)
    
    # get interpolated values for time points 5, 20, 35, 50
    y = data.frame(scr=f(seq(minT,maxT,by=1/fs_target)),
                   time =seq(minT,maxT,by=1/fs_target),
                   subject=x$subject[1])
    return(y)
  })
}
data.scr.resamp = same_sampling_rate(data.scr,fs_target=resample_to_fs)


if(cfg$plotData){
  onset_data = ddply(data.scr,.(subject),function(x){data.frame(subject=x$subject[1],onset = x$time[which(x$onset==1)])})
  ggplot(data.scr.resamp,aes(x=time,y=scr,color=factor(subject)))+
    geom_point()+geom_line(alpha=0.5)+
    geom_vline(data=onset_data,aes(xintercept=onset),alpha=.3)+facet_grid(subject~.)
}



return(list(data.scr=data.scr,data.scr.resamp=data.scr.resamp))
}