library(readxl)
library(purrr)
library(dplyr)
library(data.table)

path <- "Marion Stokes videocassette.xlsx"


read_excel_special <- function(s){
  
  data <- read_excel(path = path, sheet = s)
  
  ## Check for the weird empty columns in some sheets
  if (all(is.na(data[[1]][1:50]))) { 
    cells <- cell_cols(2:10) }
  else {
    cells <- cell_cols(1:9)
  }
  
  data <- read_excel(path = path,
             sheet = s, col_types = c("text", "text", "text", "text", "numeric", 
                                      "text", "text", "text", "text"),
             range = cells) # Exclude the first column
  
  names(data)[1] <- "Box #"
  
  return (data)
}

## Download the sheets into one dataframe
full_data_frame <- path |>
  excel_sheets() |>
  set_names() |>
  discard(~ .x == tail(excel_sheets(path), 1)) |>
  map(read_excel_special) |>
  list_rbind()

# Convert to data table
dt <- as.data.table(full_data_frame)


setnames(dt, c("Format ( B or V)", "date YYYY-MM-DD", "Format details", "start time", "channel / network", "Description - Program titles - any/all other information written out on tape label", "Notes by Logger", "logged by", "Box #"),
         c("Format", "date", "Format_details", "start_time", "channel_network", "Description", "Notes", "logged_by", "Box"), 
         skip_absent=TRUE)

# toWrite <- dt %>%
#   mutate(across(where(is.list), ~ sapply(., toString))) # Convert list columns to strings
# write.csv(toWrite, file = "simpleCombinedDataset.csv", na='')

date_col <- "date"
time_col <- "start_time"

split_and_convert_dates <- function(date_string) {
  if (is.na(date_string)) {
    return(NA)  # Handle NA values
  }
  
  # Check if the entry is a numeric Excel date
  if (grepl("^\\d+\\.?\\d*$", date_string)) {
    # Convert numeric Excel date to R Date
    return(as.Date(as.numeric(date_string), origin = "1899-12-30"))
  } else {
    # Split the string by delimiters (; or /) and trim whitespace
    date_list <- unlist(strsplit(date_string, "[;/]"))
    date_list <- trimws(date_list)  # Remove leading/trailing whitespace
    
    # Handle partial dates (e.g., "2004-07-26/27")
    date_list <- lapply(date_list, function(date_part) {
      if (nchar(date_part) == 2) {
        # If the part is just a day (e.g., "27"), infer the full date
        prev_date <- date_list[which(nchar(date_list) == 10)[1]]  # Find the previous full date
        if (!is.na(prev_date)) {
          year_month <- substr(prev_date, 1, 8)  # Extract year and month
          return(paste0(year_month, date_part))  # Combine to form full date
        }
      }
      return(date_part)  # Return as-is if not a partial date
    })
    
    # Convert to Date objects and remove names
    date_list <- unname(as.Date(unlist(date_list), format = "%Y-%m-%d"))
    return(date_list) ## this was return(date_list) for the whole list, but I am only taking the first 
  }
}

# Apply the function to the date columns
dt[, (date_col) := lapply(.SD, function(col) lapply(col, split_and_convert_dates)), .SDcols = date_col]

## Grab the first date
dt[, (date_col) := sapply(get(date_col), function(x) x[1])]
dt[, (date_col) := lapply(.SD, as.Date), .SDcols = date_col]

# Convert the second numeric column to Time
dt[, (time_col) := hms::hms(hours = get(time_col) * 24)]


split_by_channel <- function(dt) {
  to_return <- dt[, channel_network := fifelse(channel_network == "NA", NA_character_, channel_network)]
  to_return <- to_return[, 
                         .(split_col = unlist(strsplit((channel_network), "[/;,&]"))), 
                         by = setdiff(names(to_return), "channel_network")]
  
  to_return <- to_return[, channel_network := toupper(gsub("[^a-zA-Z0-9]", "", split_col))]
  to_return[, split_col := NULL]
  return(to_return)
}

clean_dates <- function(dt, remove_bad_dates = TRUE) {
  dt_clean <- dt
  if (remove_bad_dates) { dt_clean <- dt_clean[!is.na(`date`)]}  # Filter out rows with NA in the `dates` column
  dt_clean[, month := format(date, "%Y-%m")] # Add column for month
  dt_clean[, year := format(date, "%Y")] # Add column for year
  if (remove_bad_dates) {dt_clean <- dt_clean[date >= as.Date("1976-01-01") &
                                               date <= as.Date("2012-12-31")]} # chop off dates before 1980 and after 2012
  return (dt_clean)
}

delete_NA_rows <- function(dt) {
  to_return <- dt[rowSums(!is.na(dt)) > 0]
  return( to_return)
}

capitalize_format_detaials <- function(dt) {
  to_return <- dt
  to_return[, Format_details := toupper(Format_details)]
  return(to_return)
}

dt_clean <- clean_dates(dt, remove_bad_dates = FALSE)
dt_no_na <- delete_NA_rows(dt)
dt_split_channel <- split_by_channel(dt)

dt_final <- split_by_channel(clean_dates(delete_NA_rows(dt)))
dt_final <- capitalize_format_detaials(dt_final)

dt_final[, month_without_year := format(date, "%m")]


# ddd <- dt_final
# 
# ddd[, start_time := as.numeric(start_time)]
# 
# ddd[is.na(start_time), start_time := 0]  # Replace NA with 0 or another default value if needed
# 
# ddd[, start_time := format(as.POSIXct(start_time, origin = "1970-01-01", tz = "UTC"), format = "%H:%M:%S")]
# 
# setorder(ddd, date)
# 
# fwrite(dt, file = "dt_raw.csv", na = "")




## Chekcing out which entries have over 2 dates
# dt_filtered <- dt_crude[sapply(get(date_col), length) >= 2]
# 
# dt_filtered[, diff_days := sapply(get(date_col), function(dates) as.numeric(difftime(dates[2], dates[1], units = "days")))]
# 
# dt_filtered <- dt_filtered[!is.na(diff_days) & diff_days >= -10000 & diff_days <= 10000]
# # Print the filtered data.table with differences
# print(dt_filtered)
# 
# dt_filtered[diff_days > 30 | diff_days < -30]