############# Master code ############# 

# 0) load data for analysis
load('data/chp_cleaned.rdata')

# 1) descriptive statistics
source('scripts/1_descriptive-stats.r')

# 2) inferential statistics
source('scripts/2_inferential-stats.r')

# 3) model building: linear regression
source('scripts/3_linear-regression.r')
