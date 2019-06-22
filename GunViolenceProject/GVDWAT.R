

# 1 - The Art of Benchmarking

## R version
version

## Comparing read times of CSV and RDS files

saveRDS() # Saves files in R binary and is significantly faster than using CSV files
readRDS() 

## Operational differences: <- and =

# = sets object name OR argument
# <- sets object name AND argument

## Elapsed time

library(microbenchmark)
compare <- microbenchmark(read.csv("movies.csv"), 
                          readRDS("movies.rds"), 
                          times = 10)

## Benchmarking Machines

library(benchmarkme)

ram <- get_ram()
cpu <- get_cpu()

# Test reading and writing speeds for "size = " MB file
res <- benchmark_io(runs = 1, size = 5) 

# Compare results to others
plot(res)

# 2 - Fine Tuning: Efficient Base R

compare <- microbenchmark(bind_rows(Data_Incident_Characteristics$Incident_Characteristics, .id = "Incident"),
                           do.call(rbind, Data_Incident_Characteristics$Incident_Characteristics), 
                           times = 10)


## Data frames vs matrices

# matricies only use one type of data type, making storage and usage must faster

### Which is faster, mat[, 1] or df[, 1]? 
### microbenchmark(mat[, 1], df[,1])

## Unit: microseconds
## expr   min    lq    mean median     uq    max neval
## mat[, 1] 1.067 1.264 1.47869  1.391 1.5670  7.714   100
## df[, 1] 5.575 5.978 6.94740  6.224 6.4925 67.408   100

## matrix loaded far faster

## a row in a data frame returns another data frame

## microbenchmark(mat[1,], df[1,])

## Unit: microseconds
## expr      min       lq       mean   median       uq      max neval
## mat[1, ]    3.655    4.460   15.48212   20.768   24.250   40.472   100
## df[1, ] 4377.080 4682.611 5036.30844 4849.262 5040.777 9143.763   100

# 3 - Code Profiling
library(profvis)
profvis({
  # put all functions in here
})

# Using && will ignore the rest of the arguments if the first is false and only works with single logicals

# 4 - Parallel Programming
library(parallel)

#### Order of evaluation in parallel computing can't be predicted
#### can the loop be run forward and backward?

# has parallel function
# set number of cores and use parApply, or whatever function in parallel we have. 
# then close cluster with stopCluster

#parSapply 

do.call(rbind, Data_Incident_Characteristics$Incident_Characteristics)
?rbind(Data_Incident_Characteristics$Incident_Characteristics)
rbind_list(Data_Incident_Characteristics$Incident_Characteristics, .id = "Incident")
?bind_rows()
bind_rows(Data_Incident_Characteristics$Incident_Characteristics, .id = "Incident")
map_dfr(Data_Incident_Characteristics$Incident_Characteristics, as.character, .id = "Incident")

to_char <- function(df){
  
}

map(Data_Incident_Characteristics$Incident_Characteristics[1], unnest, as.character)
map(Data_Incident_Characteristics$Incident_Characteristics[1], as.list, as.character)

map_raw
map(Data_Incident_Characteristics$Incident_Characteristics[[1]], as.character)

tochar <- function(df){
  map_dfc(df, as.character)
}

map_dfc(Data_Incident_Characteristics$Incident_Characteristics[[1]], as.character)

nest(Data_Incident_Characteristics$Incident_Characteristics[[1]], tochar)

bind_rows(Data_Incident_Characteristics$Incident_Characteristics, .id = "Incident")
library(tidyverse)

library(kableExtra)


xtabs( ~ `Age Group` + Gender, data = map_dfc(Data_Participants, as.factor)) %>%
  kable("html") %>%
  kable_styling(full_width = F) %>%
  row_spec(0, background = "steelblue", color = "white")


xtabs( ~ Status + Type, data = map_dfc(Data_Participants, as.factor)) %>%
  kable("html") %>%
  kable_styling(full_width = F) %>%
  row_spec(0, background = "steelblue", color = "white")

