library(tidyverse)

### LOAD THE DATA (R files had best results)

load(file = "28762-0001-Data.rda")
baseline <- data.frame(da28762.0001)

load(file = "29221-0001-Data.rda")
visit_one <- data.frame(da29221.0001)

load(file = "29401-0001-Data.rda")
visit_two <- data.frame(da29401.0001)

load(file = "29701-0001-Data.rda")
visit_three <- data.frame(da29701.0001)

load(file = "30142-0001-Data.rda")
visit_four <- data.frame(da30142.0001)

load(file = "30501-0001-Data.rda")
visit_five <- data.frame(da30501.0001)

load(file = "31181-0001-Data.rda")
visit_six <- data.frame(da31181.0001)

load(file = "31901-0001-Data.rda")
visit_seven <- data.frame(da31901.0001)

load(file = "32122-0001-Data.rda")
visit_eight <- data.frame(da32122.0001)

load(file = "32721-0001-Data.rda")
visit_nine <- data.frame(da32721.0001)

load(file = "32961-0001-Data.rda")
visit_ten <- data.frame(da32961.0001)


# Makes a wide version of the data that we don't necessarily need
full_data_wide <- full_join(baseline, visit_one, by = "SWANID")
full_data_wide <- full_join(full_data_wide, visit_two, by = "SWANID")
full_data_wide <- full_join(full_data_wide, visit_three, by = "SWANID")
full_data_wide <- full_join(full_data_wide, visit_four, by = "SWANID")
full_data_wide <- full_join(full_data_wide, visit_five, by = "SWANID")
full_data_wide <- full_join(full_data_wide, visit_six, by = "SWANID")
full_data_wide <- full_join(full_data_wide, visit_seven, by = "SWANID")
full_data_wide <- full_join(full_data_wide, visit_eight, by = "SWANID")
full_data_wide <- full_join(full_data_wide, visit_nine, by = "SWANID")
full_data_wide <- full_join(full_data_wide, visit_ten, by = "SWANID")


### CUTS NUMERIC VISIT INDICATOR OFF THE IND OF COLUMN NAMES (this allows bind_rows() to work)

colnames(baseline) <- str_sub(colnames(baseline), 0, -2)
names(baseline)[names(baseline) == 'SWANI'] <- 'SWANID'
names(baseline)[names(baseline) == 'VISI'] <- 'VISIT'
names(baseline)[names(baseline) == 'RAC'] <- 'RACE'

colnames(visit_one) <- str_sub(colnames(visit_one), 0, -2)
names(visit_one)[names(visit_one) == 'SWANI'] <- 'SWANID'
names(visit_one)[names(visit_one) == 'VISI'] <- 'VISIT'
names(visit_one)[names(visit_one) == 'RAC'] <- 'RACE'

colnames(visit_two) <- str_sub(colnames(visit_two), 0, -2)
names(visit_two)[names(visit_two) == 'SWANI'] <- 'SWANID'
names(visit_two)[names(visit_two) == 'VISI'] <- 'VISIT'
names(visit_two)[names(visit_two) == 'RAC'] <- 'RACE'

colnames(visit_three) <- str_sub(colnames(visit_three), 0, -2)
names(visit_three)[names(visit_three) == 'SWANI'] <- 'SWANID'
names(visit_three)[names(visit_three) == 'VISI'] <- 'VISIT'
names(visit_three)[names(visit_three) == 'RAC'] <- 'RACE'

colnames(visit_four) <- str_sub(colnames(visit_four), 0, -2)
names(visit_four)[names(visit_four) == 'SWANI'] <- 'SWANID'
names(visit_four)[names(visit_four) == 'VISI'] <- 'VISIT'
names(visit_four)[names(visit_four) == 'RAC'] <- 'RACE'

colnames(visit_five) <- str_sub(colnames(visit_five), 0, -2)
names(visit_five)[names(visit_five) == 'SWANI'] <- 'SWANID'
names(visit_five)[names(visit_five) == 'VISI'] <- 'VISIT'
names(visit_five)[names(visit_five) == 'RAC'] <- 'RACE'

colnames(visit_six) <- str_sub(colnames(visit_six), 0, -2)
names(visit_six)[names(visit_six) == 'SWANI'] <- 'SWANID'
names(visit_six)[names(visit_six) == 'VISI'] <- 'VISIT'
names(visit_six)[names(visit_six) == 'RAC'] <- 'RACE'

colnames(visit_seven) <- str_sub(colnames(visit_seven), 0, -2)
names(visit_seven)[names(visit_seven) == 'SWANI'] <- 'SWANID'
names(visit_seven)[names(visit_seven) == 'VISI'] <- 'VISIT'
names(visit_seven)[names(visit_seven) == 'RAC'] <- 'RACE'

colnames(visit_eight) <- str_sub(colnames(visit_eight), 0, -2)
names(visit_eight)[names(visit_eight) == 'SWANI'] <- 'SWANID'
names(visit_eight)[names(visit_eight) == 'VISI'] <- 'VISIT'
names(visit_eight)[names(visit_eight) == 'RAC'] <- 'RACE'

colnames(visit_nine) <- str_sub(colnames(visit_nine), 0, -2)
names(visit_nine)[names(visit_nine) == 'SWANI'] <- 'SWANID'
names(visit_nine)[names(visit_nine) == 'VISI'] <- 'VISIT'
names(visit_nine)[names(visit_nine) == 'RAC'] <- 'RACE'

colnames(visit_ten) <- str_sub(colnames(visit_ten), 0, -3)
names(visit_ten)[names(visit_ten) == 'SWAN'] <- 'SWANID'
names(visit_ten)[names(visit_ten) == 'VIS'] <- 'VISIT'
names(visit_ten)[names(visit_ten) == 'RA'] <- 'RACE'

# Only 115 column names shared across all 11 visits
length(intersect(intersect(intersect(intersect(intersect(intersect(intersect(intersect(intersect(colnames(baseline), colnames(visit_one)), colnames(visit_two)), colnames(visit_three)),
          colnames(visit_five)), colnames(visit_six)), colnames(visit_seven)), colnames(visit_eight)), colnames(visit_nine)), colnames(visit_ten)))

intersect(intersect(intersect(intersect(intersect(intersect(intersect(intersect(intersect(colnames(baseline), colnames(visit_one)), colnames(visit_two)), colnames(visit_three)),
          colnames(visit_five)), colnames(visit_six)), colnames(visit_seven)), colnames(visit_eight)), colnames(visit_nine)), colnames(visit_ten))


### TURNS ALL COLUMNS TO CHARACTER TYPE 
# (some columns are coded as different types across the visits some this will allow us to merge the data)

baseline[] <- lapply(baseline, as.character)
visit_one[] <- lapply(visit_one, as.character)
visit_two[] <- lapply(visit_two, as.character)
visit_three[] <- lapply(visit_three, as.character)
visit_four[] <- lapply(visit_four, as.character)
visit_five[] <- lapply(visit_five, as.character)
visit_six[] <- lapply(visit_six, as.character)
visit_seven[] <- lapply(visit_seven, as.character)
visit_eight[] <- lapply(visit_eight, as.character)
visit_nine[] <- lapply(visit_nine, as.character)
visit_ten[] <- lapply(visit_ten, as.character)

### MERGES THE DATA

full_data_long <- bind_rows(baseline, visit_one)
full_data_long <- bind_rows(full_data_long, visit_two)
full_data_long <- bind_rows(full_data_long, visit_three)
full_data_long <- bind_rows(full_data_long, visit_four)
full_data_long <- bind_rows(full_data_long, visit_five)
full_data_long <- bind_rows(full_data_long, visit_six)
full_data_long <- bind_rows(full_data_long, visit_seven)
full_data_long <- bind_rows(full_data_long, visit_eight)
full_data_long <- bind_rows(full_data_long, visit_nine)
full_data_long <- bind_rows(full_data_long, visit_ten)

### REMOVES NUMBER INDICATORS IN PARENTHESES OF FACTOR LEVELS

for (i in 1:ncol(full_data_long)) {
  full_data_long[,i] <- trimws(sub("\\(.*\\)", "", full_data_long[,i]))
}

### TURNS ANY NUMERIC COLUMNS BACK TO NUMERIC FROM CHARACTER

is_all_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}

full_data_long2 <- full_data_long %>%
  mutate_if(is_all_numeric,as.numeric)

# Checking none of these variables weren't actually numeric and turned to a column of NAs
# All good! Some columns do have a very large number of NAs. May choose to remove unhelpful columns
num_cols <- unlist(lapply(full_data_long2, is.numeric))
temp <- full_data_long2[ , num_cols]

a <- colSums(is.na(temp))
b <- as.data.frame(a)

### Removes NEGATIVE NUMERICS
# -9 = Missing, -8 = Do Not Know, -7 = Refused, and -1 = NA
# We chose not to capture this information because we didn't want classes for these things in a numeric vector during analysis

full_data_long2[full_data_long2 < 0] <- NA

### SAVING THE DATA TO FILES
# We noticed that when loading in the CSV, some character variable get coerced to logical and some NAs get added
# Best to use RDA file in R, but for Python a CSV has still been written

save(full_data_long2, file='full_data_long.rda')
write_csv(full_data_long2, file='full_data_long.csv')



