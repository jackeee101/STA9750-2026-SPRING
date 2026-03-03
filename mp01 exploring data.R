#' Acquire IPEDS Data for MP#01
#' 
#' This function will acquire and standardize all data for MP#01
#' from IPEDS (https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx)
#' 
#' We're starting in 2010 as the data seems to be reasonably complete 
#' after that point. 
acquire_ipeds_data <- function(start_year=2010, end_year=2024){
  library(tidyverse)
  library(glue)
  
  data_dir <- file.path("data", "mp01")
  
  if(!dir.exists(data_dir)){
    dir.create(data_dir, showWarnings=FALSE, recursive=TRUE)
  }
  
  YEARS <- seq(start_year, end_year)
  
  EFA_ALL <- map(YEARS, function(yy){
    if(yy <= 2022){
      ef_url <- glue("https://nces.ed.gov/ipeds/datacenter/data/EF{yy}A.zip")
      
    } else {
      ef_url <- glue("https://nces.ed.gov/ipeds/data-generator?year={yy}&tableName=EF{yy}A&HasRV=0&type=csv")
    }
    
    ef_file <- file.path(data_dir, glue("ef{yy}a.csv.zip"))
    
    if(!file.exists(ef_file)){
      message(glue("Downloading Enrollment Data for {yy} from {ef_url}"))
      download.file(ef_url, destfile = ef_file, quiet=TRUE, mode="wb")    
    }
    
    read_csv(ef_file, 
             show_col_types=FALSE) |>
      mutate(year = yy, 
             # American Indian or Alaskan Native
             enrollment_m_aian = EFAIANM, 
             enrollment_f_aian = EFAIANW, 
             # Asian
             enrollment_m_asia = EFASIAM, 
             enrollment_f_asia = EFASIAW, 
             # Black or African-American, 
             enrollment_m_bkaa = EFBKAAM, 
             enrollment_f_bkaa = EFBKAAW, 
             # Hispanic 
             enrollment_m_hisp = EFHISPM, 
             enrollment_f_hisp = EFHISPW, 
             # Native Hawaiian or Other Pacific Islander 
             enrollment_m_nhpi = EFNHPIM, 
             enrollment_f_nhpi = EFNHPIW, 
             # White
             enrollment_m_whit = EFWHITM, 
             enrollment_f_whit = EFWHITW, 
             # Two or More Races
             enrollment_m_2mor = EF2MORM, 
             enrollment_f_2mor = EF2MORW, 
             # Unknown / Undisclosed Race
             enrollment_m_unkn = EFUNKNM, 
             enrollment_f_unkn = EFUNKNW, 
             # US Non-Resident
             enrollment_m_nral = EFNRALM, 
             enrollment_f_nral = EFNRALW, 
      ) |> filter(
        (EFALEVEL %in% c(2, 12)) | (LINE %in% c(1, 15))
        # Per 2024 Data Dictionary, 
        # - EFALEVEL 2 = undergrad 
        # - EFALELVE 12 = grad
        # - Line 1 = first year first time full-time undergrad
        # - Line 15 = first year first time part-time undergrad
      ) |> mutate(level = case_when(
        EFALEVEL == 2 ~ "all undergrad", 
        EFALEVEL == 12 ~ "all graduate",
        LINE %in% c(1, 15) ~ "first year undergrad"
      )
      ) |>
      select(institution_id = UNITID, 
             year, 
             level,
             starts_with("enrollment_")) |>
      group_by(institution_id, 
               year, 
               level) |>
      summarize(across(starts_with("enrollment_"), sum), 
                .groups = "drop")
    
  }) |> bind_rows()
  
  DESC_ALL <- map(YEARS, function(yy){
    if(yy <= 2022){
      hd_url <- glue("https://nces.ed.gov/ipeds/datacenter/data/HD{yy}.zip")
      
    } else {
      hd_url <- glue("https://nces.ed.gov/ipeds/data-generator?year={yy}&tableName=HD{yy}&HasRV=0&type=csv")
    }
    
    hd_file <- file.path(data_dir, glue("hd{yy}.csv.zip"))
    
    if(!file.exists(hd_file)){
      message(glue("Downloading Institutional Descriptions for {yy} from {hd_url}"))
      download.file(hd_url, destfile = hd_file, quiet=TRUE, mode="wb")    
    }
    
    suppressWarnings(
      read_csv(hd_file, 
               show_col_types=FALSE, 
               locale=locale(encoding=if_else(yy==2024, "utf-8", "windows-1252"))) |>
        mutate(year = yy, 
               INSTNM) |> 
        select(institution_id = UNITID, 
               institution_name = INSTNM, 
               state = STABBR, 
               year)
    )
    
  }) |> bind_rows()
  
  inner_join(EFA_ALL, 
             DESC_ALL, 
             join_by(institution_id == institution_id, 
                     year == year))
}

IPEDS <- acquire_ipeds_data()

IPEDS |>
  count(is_cuny ==TRUE)

IPEDS |>
  count(is_calpublic ==TRUE)
             
#Task 2: Identify CUNY Schools

IPEDS <- IPEDS |>
  mutate(is_cuny = str_detect(institution_name, "CUNY"))

# Task 3: Identify UC and CAl State Schools

IPEDS |>
  filter(str_detect(institution_name,"California")) |>
  distinct(institution_name) |>
  print(n = 200)

IPEDS <- IPEDS |>
  mutate(is_calpublic = str_detect(institution_name,"University of California") |
           str_detect(institution_name,"California State University"))


# Task 4 Intial EDA Pass

glimpse(IPEDS)

# glimpse shows the type of data each column has. 
# Year data are all int. 
# Level, state, institution_name are chr. 
# Is_cuny and is_calpublic are lgl. 
# And the rest are dbl.

summary(IPEDS)
# Summmary shows the min, median, max, and quartiles of numeric data.
# It shows the length of character data and the number of true/false for logic data.

install.packages("gt")
library(gt)

# Task 5 Exploratory Questions

# 1 How many distinct institutions appear in this data set?
# Answer: 11243

IPEDS |>
  distinct(institution_name)

# 2 How many graduate students were enrolled at Baruch in 2024?
# Answer: 3585

#shortcut
IPEDS |>
  filter(str_detect(institution_name, "Baruch") & 
  str_detect(level, "graduate") &
  year == 2024) |>
  summarize(total_enrollment =
              sum(unlist(across(contains("enrollment"))),
                  na.rm = TRUE))

# all rows

IPEDS |>
  filter(str_detect(institution_name, "Baruch") & 
           str_detect(level, "graduate") &
           year == 2024) |>
  print(n=25, width=Inf)
  
print(5+1+443+494+151+246+278+357+2+1+417+407+37+41+325+380)

# 3  How many total students were enrolled at Baruch in 2024?
# Answer: 20081

IPEDS |>
  distinct(level)

#shortcut
IPEDS |>
  filter(str_detect(institution_name, "Baruch") &
        !str_detect(level, "first year") &
           year == 2024) |>
  summarize(total_enrollment =
              sum(unlist(across(contains("enrollment"))),
                  na.rm = TRUE))
  print(n=25, width=Inf)

#all rows  
IPEDS |>
    filter(str_detect(institution_name, "Baruch") &
             !str_detect(level, "first year") &
             year == 2024) |>
  print(n=25, width=Inf)

print(5+11+1+17+443+2950+494+2840+151+619+246+841+278+2244+357+2501+2+8+1+8+417+1739+407+1282+37+214+41+191+325+441+380+590)

# 4 Which institution had the highest number of enrolled female students in 2019?
# Answer: Western Governors University

IPEDS |>
    group_by(institution_name) |>
    summarize(total_enrollment =
        sum(unlist(across(contains("enrollment_f"))),na.rm = TRUE)) |>
    slice_max(total_enrollment,n=5)

# 5 Which institution with over 1000 total students admitted the highest proportion of Native Hawaiian or Pacific Islander (nhpi) first-year undergraduates in 2024?
# Report at least both the institution and the fraction of relevant students.
# Answer: Northeast Technology Center, 20,8%

IPEDS |>
  filter(str_detect(level, "first year") &
           year == 2024) |>
  group_by(institution_name) |>
  summarize(
    total_enrollment =
      sum(across(contains("enrollment")), na.rm = TRUE),
    
    total_aian_enrollment =
      sum(across(contains("aian")), na.rm = TRUE)
  ) |>
  mutate(
    pct_aian = total_aian_enrollment / total_enrollment
  )|>
  filter(total_enrollment > 1000) |>
  slice_max(pct_aian,n=5)

# Task 6 Exploratory Questions

# 1 Which 5 states had the highest number of graduate students across all institutions located in that state?
# Answer: CA, TX, NY, FL, IL

IPEDS <- IPEDS |>
  mutate(
    total_enrollment = rowSums(across(contains("enrollment")), na.rm = TRUE)
  )

IPEDS |>
  group_by(state) |>
  summarize(
    total_enrollment = sum(total_enrollment, na.rm = TRUE)
  ) |>
  slice_max(total_enrollment, n=5)

# 2 In 2024, how many first year undergraduate students were enrolled at CUNY colleges and which colleges did they attend? Report both absolute enrollment numbers and percent of total first-year undergraduates?
# 



IPEDS |>
  filter(year == 2024 &
        is_cuny == TRUE)|>
  group_by(institution_name) |>
  summarize(first_year_enrollment = sum(
    unlist(across(contains("enrollment")))[str_detect(level, "first year")],
    na.rm = TRUE
  ),
  total_enrollment = sum(unlist(across(contains("enrollment"))), na.rm = TRUE),
  ) |>
  mutate(
    percent_first_year = first_year_enrollment / (total_enrollment - first_year_enrollment) * 100
  )

    
# 3 How has Baruch’s total undergraduate enrollment changed over the study period? Report both enrollment numbers and percent change year-over-year.

IPEDS <- IPEDS |>
  mutate(
    total_enrollment = rowSums(across(contains("enrollment")), na.rm = TRUE)
  )

IPEDS |>
  filter(str_detect(institution_name, "Baruch")) |>
  filter(year >= 2010) |>
  group_by(year) |>
  summarize(total_enrollment = sum(total_enrollment, na.rm = TRUE)) |>
  arrange(year) |>
  mutate(
    prev_enrollment = lag(total_enrollment),
    change = total_enrollment - prev_enrollment,
    percentage_change = (total_enrollment - prev_enrollment) / prev_enrollment * 100
  ) |>
  print(n = Inf, width = Inf)

# 4 At what 5 institutions did the fraction of white students decrease the most over the period from 2010 to 2020?
# University of the Cumberlands 
# Detroit Business Institute-Downriver
# Paul Mitchell the School-Rhode Island
# Robert Paul Academy of Cosmetology
# Vista College

IPEDS |>
  filter(year %in% c(2010, 2020)) |>
  group_by(institution_name, year) |>
  summarize(
    total_students = sum(total_enrollment, na.rm = TRUE),
    white_students = sum(enrollment_m_whit, na.rm = TRUE) + sum(enrollment_f_whit, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(fraction_white = white_students / total_students) |>
  select(institution_name, year, fraction_white) |>
  pivot_wider(
    names_from = year,
    values_from = fraction_white,
    names_prefix = "year_"
  ) |>
  filter(!is.na(year_2010) & !is.na(year_2020)) |>
  mutate(change_fraction_white = year_2020 - year_2010) |>
  arrange(change_fraction_white) |> 
  slice_head(n = 5) |>
  print()

# 5 In which 3 states did the fraction of female undergraduates increase the most over the period from 2010 to 2024?
# Answer: AS, FM, NH

IPEDS |>
  filter(year %in% c(2010, 2024)) |>
  group_by(state, year) |>
  summarize(
    total_students = sum(total_enrollment, na.rm = TRUE),
    female_students = sum(across(contains("enrollment_f")), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(fraction_female = female_students / total_students) |>
  select(state, year, fraction_female) |>
  pivot_wider(
    names_from = year,
    values_from = fraction_female,
    names_prefix = "year_"
  ) |>
  filter(!is.na(year_2010) & !is.na(year_2024)) |>
  mutate(change_fraction_female = year_2024 - year_2010) |>
  arrange(desc(change_fraction_female)) |>  # largest increase first
  slice_head(n = 5) |>
  print()












