# Read/Write local files efficently w/ Arrow
library(arrow)
my_table <- arrow_table(df)
write_parquet(my_table, file.path("data", "my_table.parquet"))
fast_df <- read_arrow("data", "my_table.parquet")
# note you can pipe an arrow table into many dplyr and tidyverse functions 
# however not all are supported. More info - https://arrow.apache.org/cookbook/r/index.html