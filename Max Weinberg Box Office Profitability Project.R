# Loading in Dataset and Displaying it as Tibble
rotten_tomatoes_top_movies.copy <- read.csv(
  "~/Library/CloudStorage/OneDrive-Personal/rotten_tomatoes_top_movies copy.csv")
box_office <- rotten_tomatoes_top_movies.copy
dim(box_office)
library(tidyverse)
box_office <- as_tibble(box_office)
head(box_office)

# Putting in NA for all of the Blank Data Cells
box_office[box_office == ""] <- NA

# Removing any duplicate entries in the data
library(tidyverse)
box_office %>% distinct()

# Dropping Irrelevant Columns
columns_to_drop <- c( "link", "synopsis", "view_the_collection", "consensus")
box_office1 <- box_office %>%
  dplyr::select(-all_of(columns_to_drop))
sum(is.na(box_office1))

# Changing all NA in the "rating" Column to Display as "NR"
box_office1$rating[is.na(box_office1$rating)] <- "NR"

# Changing the "rating" column to numerical to analyze it
library(dplyr)
library(stringr)

clean_rating <- function(rating_str) {
  if (is.na(rating_str)) {
    return(NA_character_) 
  }
  
  match <- str_extract(rating_str, "^((PG-13|PG|G|R|NR))\\b")
  return(match)
}  
box_office1$rating <- as.character(sapply(box_office1$rating, clean_rating))
box_office1$rating <- sapply(box_office1$rating, clean_rating)

rating_counts <- table(box_office1$rating, useNA = "always")

cat("Counts of all ratings in the 'rating' column:\n")
rating_counts

rating_mapping <- c(
  "G" = 1,
  "PG" = 2,
  "PG-13" = 3,
  "R" = 4,
  "NR" = 5
)


box_office1$rating <- rating_mapping[box_office1$rating]

box_office1$rating <- as.numeric(box_office1$rating)

box_office1$total_ratings <- box_office1$total_ratings %>%
  str_replace_all("Fewer than 50", "25") %>%
  str_replace_all(" Verified", "") %>%
  str_replace_all(",", "") %>%
  str_replace_all("\\+", "")

box_office1$total_ratings <- suppressWarnings(as.numeric(box_office1$total_ratings))

cat("Updated 'total_ratings' column (first 10 rows):\n")
head(box_office1[, c("total_ratings")], 10)

type_counts <- table(box_office1$type, useNA = "always")
type_counts

# Cleaning data set so columns display numbers that have identical format

clean_box_office_r <- function(box_office_str) {
  if (is.na(box_office_str) || box_office_str == "") {
    return(NA_real_)
  }
  s <- str_replace_all(box_office_str, fixed("$"), "")
  s <- str_replace_all(s, fixed(","), "")
  s <- str_trim(s) 
  
  if (str_detect(s, "M$")) {
    return(as.numeric(str_replace(s, "M$", "")) * 1e6)
  } else if (str_detect(s, "K$")) {
    return(as.numeric(str_replace(s, "K$", "")) * 1e3)
  } else {
    return(as.numeric(s))
  }
}

box_office1$box_office_gross_usa <- sapply(box_office1$box_office_.gross_usa., clean_box_office_r)


box_office1 <- box_office1 %>% dplyr::select(-box_office_.gross_usa.)

convert_runtime_to_minutes_r <- function(runtime_str) {
  if (is.na(runtime_str) || runtime_str == "") {
    return(NA_real_) 
  }
  
  total_minutes <- 0
  s <- str_trim(runtime_str) 
  
  
  if (str_detect(s, "h")) {
    parts <- str_split(s, "h")[[1]]
    hours_str <- parts[1]
    total_minutes <- total_minutes + as.numeric(str_trim(hours_str)) * 60
    s <- parts[2] 
  }
  
  
  if (str_detect(s, "m")) {
    minutes_str <- str_replace(s, "m", "")
    
    if (str_trim(minutes_str) != "") {
      total_minutes <- total_minutes + as.numeric(str_trim(minutes_str))
    }
  }
  
  return(total_minutes)
}

box_office1$runtime_minutes <- sapply(box_office1$runtime, convert_runtime_to_minutes_r)
box_office1$runtime_minutes <- as.numeric(box_office1$runtime_minutes)
box_office1 <- box_office1 %>% dplyr::select(-runtime)


box_office1$critic_score <- suppressWarnings(as.numeric(box_office1$critic_score))
box_office1$people_score <- suppressWarnings(as.numeric(box_office1$people_score))
box_office1$total_reviews <- suppressWarnings(as.numeric(box_office1$total_reviews))
box_office1$year <- suppressWarnings(as.numeric(box_office1$year))

na_count <- sum(is.na(box_office1$box_office_gross_usa))
na_count

# Imputing missing data using MICE

library(mice)

imputation_cols <- c(
  "box_office_gross_usa",
  "critic_score",
  "people_score",
  "total_reviews",
  "total_ratings",
  "runtime_minutes",
  "year",
  "rating"
)

box_office1_for_imputation <- box_office1 %>%
  dplyr::select(all_of(imputation_cols))

cat("\nNumber of NAs in each column before imputation:\n")
colSums(is.na(box_office1_for_imputation))

set.seed(123)
imputed_data <- mice(box_office1_for_imputation, m = 5, method = 'cart', maxit = 10, printFlag = FALSE)

cat("\nMice imputation methods for each variable:\n")
imputed_data$method

box_office1_imputed_complete <- complete(imputed_data, 1)
cat("\nNumber of NAs in each column AFTER imputation (in one complete dataset):\n")
colSums(is.na(box_office1_imputed_complete))

cat("\nComparison of 'box_office_gross_usa_numeric' before and after imputation (first 10 rows):\n")
original_na_indices <- which(is.na(box_office1$box_office_gross_usa))

comparison_df <- data.frame(
  Original = box_office1$box_office_gross_usa[original_na_indices[1:min(10, length(original_na_indices))]],
  Imputed = box_office1_imputed_complete$box_office_gross_usa[original_na_indices[1:min(10, length(original_na_indices))]]
)
comparison_df

#Inserting Imputed Data Back into Original Dataset

box_office1$box_office_gross_usa <- box_office1_imputed_complete$box_office_gross_usa

colSums(is.na(box_office1))

box_office1$people_score <- box_office1_imputed_complete$people_score
box_office1$rating <- box_office1_imputed_complete$rating
box_office1$runtime_minutes <- box_office1_imputed_complete$runtime_minutes
colSums(is.na(box_office1))

box_office1 <- box_office1 %>% dplyr::select(-aspect_ratio)
box_office1 <- box_office1 %>% dplyr::select(-sound_mix)

colSums(is.na(box_office1))

# Inserting the word "Unknown" into remaining NA in the dataset

for (col_name in names(box_office1)) {
  if (is.character(box_office1[[col_name]])) {
    box_office1[[col_name]][is.na(box_office1[[col_name]])] <- "Unknown"
  } else if (is.factor(box_office1[[col_name]])) {
    levels(box_office1[[col_name]]) <- c(levels(box_office1[[col_name]]), "Unknown")
    box_office1[[col_name]][is.na(box_office1[[col_name]])] <- "Unknown"
  }
}

colSums(is.na(box_office1))

box_office1 <- box_office1 %>% dplyr::select(-genre)

library(fastDummies)

# Ordinal Encoding for "original_language" column (alphabetical order)

library(tidyverse)

unique_languages <- unique(box_office1$original_language)
unique_languages <- unique_languages[!is.na(unique_languages)]
desired_language_order_alpha <- sort(unique_languages)

cat("Unique values in the 'original_language' column (sorted alphabetically for encoding):\n")
print(desired_language_order_alpha)

box_office1$original_language_encoded_alpha <- factor(box_office1$original_language,
                                             levels = desired_language_order_alpha,
                                             ordered = TRUE)

box_office1$original_language_numeric_alpha <- as.numeric(box_office1$original_language_encoded_alpha)

cat("\nFirst 10 rows showing original 'original_language' and new alphabetically encoded columns:\n")
print(head(box_office1[, c("original_language", "original_language_encoded_alpha", "original_language_numeric_alpha")], 10))

cat("\nMapping of 'original_language' categories to numeric values (alphabetical order):\n")
unique_mapping_language_alpha <- box_office1 %>%
  dplyr::select(original_language, original_language_numeric_alpha) %>%
  distinct() %>%
  arrange(original_language_numeric_alpha)
print(unique_mapping_language_alpha)

cat("\nNumber of NA values in 'original_language_numeric_alpha' after encoding:\n")
print(sum(is.na(box_office1$original_language_numeric_alpha)))

box_office1 <- box_office1 %>% dplyr::select(-original_language)


type_counts <- table(box_office1$type, useNA = "always")
type_counts

# Ordinal Encoding for the "type" column (alphabetical order)

unique_types <- unique(box_office1$type)
unique_types <- unique_types[!is.na(unique_types)]
desired_type_order_alpha <- sort(unique_types)
cat("Unique values in the 'type' column (sorted alphabetically for encoding):\n")
print(desired_type_order_alpha)
box_office1$type_encoded_alpha <- factor(box_office1$type,
                                levels = desired_type_order_alpha,
                                ordered = TRUE)
box_office1$type_numeric_alpha <- as.numeric(box_office1$type_encoded_alpha)
cat("\nFirst 10 rows showing original 'type' and new alphabetically encoded columns:\n")
print(head(box_office1[, c("type", "type_encoded_alpha", "type_numeric_alpha")], 10))
cat("\nMapping of 'type' categories to numeric values (alphabetical order):\n")
unique_mapping_alpha <- box_office1 %>%
  dplyr::select(type, type_numeric_alpha) %>%
  distinct() %>%
  arrange(type_numeric_alpha)
print(unique_mapping_alpha)
cat("\nNumber of NA values in 'type_numeric_alpha' after encoding:\n")
print(sum(is.na(box_office1$type_numeric_alpha)))
names(box_office1)

cat("\nFirst 10 rows showing original 'type' and the 'type_numeric_alpha' column:\n")
print(head(box_office1[, c("type", "type_numeric_alpha")], 10))

box_office1 <- box_office1 %>% dplyr::select(-type)

# Converting 'release_date_.theaters.' column to "MM-DD-YYYY" format

library(lubridate)

box_office1$release_date_theaters_parsed <- parse_date_time(box_office1$`release_date_.theaters.`,
                                                   orders = c("mdy", "ymd", "dmy", "B d, Y", "b d, Y", "B d Y", "m/d/y", "Y-m-d"),
                                                   locale = "en_US.UTF-8")

box_office1$release_date_theaters_formatted <- format(box_office1$release_date_theaters_parsed, "%m-%d-%Y")


box_office1 <- box_office1 %>% dplyr::select(-release_date_.theaters.)

# Converting 'release_date_.streaming.' column to "MM-DD-YYYY" format 

box_office1$release_date_streaming_parsed <- parse_date_time(box_office1$`release_date_.streaming.`,
                                                    orders = c("mdy", "ymd", "dmy", "B d, Y", "b d, Y", "B d Y", "m/d/y", "Y-m-d"),
                                                    locale = "en_US.UTF-8")

box_office1$release_date_streaming_formatted <- format(box_office1$release_date_streaming_parsed, "%m-%d-%Y")

# Removing unnecessary columns

box_office1 <- box_office1 %>% dplyr::select(-release_date_.streaming.)

box_office1 <- box_office1 %>% dplyr::select(-release_date_streaming_parsed)

box_office1 <- box_office1 %>% dplyr::select(-release_date_theaters_parsed)

box_office1$theaters_release_date <- box_office1$release_date_theaters_formatted

box_office1$streaming_release_date <- box_office1$release_date_streaming_formatted

box_office1 <- box_office1 %>% dplyr::select(-release_date_theaters_formatted)

box_office1 <- box_office1 %>% dplyr::select(-release_date_streaming_formatted)

box_office1 <- box_office1 %>%
  rename(
    type = 'type_encoded_alpha',
    type_numeric = 'type_numeric_alpha',
    original_language = 'original_language_encoded_alpha',
    original_language_numeric = 'original_language_numeric_alpha'
  )

sum(is.na(box_office1$theaters_release_date))
sum(is.na(box_office1$streaming_release_date))

box_office1$theaters_release_date[is.na(box_office1$theaters_release_date)] <- "Unknown"
box_office1$streaming_release_date[is.na(box_office1$streaming_release_date)] <- "Unknown"

sum(is.na(box_office1$theaters_release_date))
sum(is.na(box_office1$streaming_release_date))

summary(box_office1)

str(box_office1)

head(box_office1)
tail(box_office1)

numeric_box_office1 <- box_office1[, sapply(box_office1, is.numeric)]

str(numeric_box_office1)

summary(numeric_box_office1)

# Visualizing Data

pairs(numeric_box_office1, main = "Pairplot of Numeric Box Office Data")

cor(numeric_box_office1)

# Building a Multiple Linear Regression Model

library(MASS)

fit <- lm(box_office_gross_usa ~ ., data = numeric_box_office1)
fit
summary(fit)
stepwisemodel <- stepAIC(fit, direction = 'both')

# Step:  AIC=58535.35
# box_office_gross_usa ~ X + year + total_reviews + total_ratings + 
#     rating + runtime_minutes + original_language_numeric + type_numeric


summary(stepwisemodel)

library(leaps)

best_subsets_model <- regsubsets(box_office_gross_usa ~ . , data = numeric_box_office1)
summary(best_subsets_model)


coef(best_subsets_model, id = 4)

summary(numeric_box_office1)

# Transferring R data to Tableau

library(Rserve)

write.csv(box_office1, "box_office1.csv", row.names = FALSE)

plot(stepwisemodel)

# Checking to see if all my data manipulations and visualizations are accurate

library(dataMaid)

makeDataReport(box_office1, output = "html", replace = TRUE)

library(DataExplorer)
create_report(box_office1)
library(SmartEDA)
ExpReport(box_office1, op_file = 'smartEDA.html')

