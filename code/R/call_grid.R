call_grid <- function(cfg,t='1:4'){

#t = '1'
cmd=paste0('qsub -cwd -t ',t,
           ' -o ', cfg_grid$gridOutputPath, '/ -e ', cfg_grid$gridOutputPath, '/',
           ' -l ', cfg_grid$requirements, 
           ' -N ', cfg_grid$job_name, 
           ' -pe default ', 1,
           ' -q nbp.q ',cfg$script_name)
system(cmd)
}