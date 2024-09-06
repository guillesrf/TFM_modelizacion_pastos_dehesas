library(doParallel)
library(parallel)
library(foreach)
library(olsrr)

dir <- 'C:/TFM/ScriptsR/1_dataframetomodel'
setwd(dir)


data <- read.csv('./2_RESULTS/full_data_buffer500.csv')[,-c(1,2)]
varDep <- 'yield_kgDM_ha.1'
good_vars <- 'tas_min_spring + RIPI + P + CEC + fcc+ tas_min_year + pr_max_spring + T_OC + bio15_spring + bio4_spring + pr_min_year + spei12'


lm_subset <- lm(paste0(varDep,'~ ', good_vars),data=data)


# Number of cores to use
num_cores <- parallel::detectCores() - 1  # Use one less than the available cores

# Register parallel backend
cl <- makeCluster(num_cores)
doParallel::registerDoParallel(cl)

# Parallelize the best subset selection process
results <- foreach(i = 1:num_cores, .combine = 'list', .packages = 'olsrr') %dopar% {
  ols_step_best_subset(lm_subset, include = 'fcc')
}

# Stop the cluster after completion
stopCluster(cl)

print(results)

saveRDS(results,'./2_RESULTS/best_subsets.rds')

