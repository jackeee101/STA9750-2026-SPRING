if(!require("tidyverse")) install.packages("tidyverse")
if(!require("nycflights13")) install.packages("nycflights13")
library(tidyverse)
library(nycflights13)

glimpse(flights)

flights |> summarize(max(arr_delay, na.rm=TRUE))

flights |> filter(!is.na(arr_delay))

dml1 <- filter(flights, !(arr_delay > 120 | dep_delay > 120))
dml2 <- filter(flights, arr_delay <= 120, dep_delay <= 120)
identical(dml1, dml2)
dml1

flights |> filter(arr_delay > 120)

# What carrier has the lowest rate of delayed flights?
# UA

flights |>
  filter(dep_delay == 0) |>
  count(carrier) |>
  slice_max(n, n = 5)

flights |>
  filter(arr_delay == 0) |>
  count(carrier) |>
  slice_max(n, n = 5)

#   What carrier has the highest chance of early arrivals?
# UA

flights |>
  filter(arr_time < sched_arr_time) |>
  count(carrier) |>
  slice_max(n, n=5)

#   What carrier is most likely to “make up time in flight” after a delayed departure?
# UA

flights |>
  filter(dep_time > sched_dep_time &
           arr_time < sched_arr_time) |>
  count(carrier) |>
  slice_max(n,n=5)

#   Which origin airport has the highest rate of delays?
# EWR

glimpse(flights)

flights |>
  filter(dep_delay != 0) |>
  count(origin) |>
  slice_max(n, n = 5)

flights |>
  filter(arr_delay != 0) |>
  count(origin) |>
  slice_max(n, n = 5)

#   Which month has the most flights?
# 7

flights |>
  count(month) |>
  slice_max(n, n = 5)


# What is the furthest flight in this data?
# 2013,01,01 Flight to HNL from JFK

flights |>
  slice_max(distance, n = 5)

#   What is the shortest flight in this data?
# 2013, 01, 03 Flight to PHL from EWR

flights |>
  slice_min(distance, n = 5)

#   Are longer flights more likely to be delayed than short ones?
# mean is 1040
# more than mean distance dep delay 118,770 arr delay 123.740
# less than mean distance dep delay 193,237 arr delay 198,197
# Answer: no

flights |>
  filter(distance < 1040, dep_delay!=0) 

flights |>
  filter(distance > 1040, dep_delay!=0) 

flights |>
  filter(distance < 1040, arr_delay!=0) 

flights |>
  filter(distance > 1040, arr_delay!=0)
  
118770 + 123740

193237 + 198197
  
  
  
  
  
  
  
  