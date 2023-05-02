source('functions.R')
df = read.csv('demo_data.csv')[-c(1,2),]
df$pID = paste0("PID", c(1:nrow(df))) # you need a column named "pID"
get_fun_df(df, "test") 


