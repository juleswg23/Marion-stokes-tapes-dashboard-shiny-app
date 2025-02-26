library(readxl)
library(purrr)
library(dplyr)
library(data.table)
library(ggplot2)

# 
# 
# ## Set this path to whatever the file path is on your computer
# path <- "Marion Stokes videocassette.xlsx"
# 
# ## Function for reading the specific sheet
# read_excel_special <- function(s){
#   data <- read_excel(path = path, sheet = s)
#   
#   ## Check for the weird empty columns in some sheets
#   if (all(is.na(data[[1]][1:50]))) { 
#     cells <- cell_cols(2:10) }
#   else {
#     cells <- cell_cols(1:9)
#   }
#     
#   read_excel(path = path,
#              sheet = s, col_types = c("text", "text", "text", "date", "date", 
#                                       "text", "text", "text", "text"),
#              range = cells) # Exclude the first column
# }
# 
# ## Download the sheets into one dataframe
# full_data_simple <- path |>
#   excel_sheets() |>
#   set_names() |>
#   discard(~ .x == tail(excel_sheets(path), 1)) |>
#   map(read_excel_special) |>
#   list_rbind()
# 
# # Convert to data table
# dt <- as.data.table(full_data_simple)
# setnames(dt, c("Format ( B or V)", "Format details", "start time", "channel / network", "Description - Program titles - any/all other information written out on tape label", "Notes by Logger", "logged by", "Box #"),
#          c("Format", "Format_details", "start_time", "channel_network", "Description", "Notes", "logged_by", "Box"), 
#          skip_absent=TRUE)
# 

# Write back to excel sheet
# toWrite <- dt %>%
#   mutate(across(where(is.list), ~ sapply(., toString))) # Convert list columns to strings
# write.csv(toWrite, file = "simpleCombinedDataset.csv", na='')
# 
# ## Clean the data
# clean_dates <- function(dt, remove_na_dates = TRUE) {
#   dt_clean <- dt
#   if (remove_na_dates) { dt_clean <- dt_clean[!is.na(`date YYYY-MM-DD`)]}  # Filter out rows with NA in the `dates` column
#   dt_clean[, date := as.Date(`date YYYY-MM-DD`, format = "%Y-%m-%d")] # Convert dates column to proper format
#   dt_clean[, month := format(date, "%Y-%m")] # Add column for month
#   dt_clean[, year := format(date, "%Y")] # Add column for year
#   if (remove_na_dates) {dt_clean <- dt_clean[date >= as.Date("1980-01-01") &
#                          date <= as.Date("2012-12-31")]} # chop off dates before 1980 and after 2012
#   return (dt_clean)
# }
# 
# dt_clean <- clean_dates(dt)
# dt_clean_withNA <- clean_dates(dt, remove_na_dates = FALSE)


## 2/24 work, cleaning data## 2/24 work, cleaning data

# dt_channel_split <- dt[, channel_network := fifelse(channel_network == "N/A", NA_character_, channel_network)]
# dt_channel_split <- dt_channel_split[, 
#          .(channel_network = unlist(strsplit(channel_network, "[/;,&]"))), 
#          by = setdiff(names(dt_channel_split), "channel_network")]
# 
# dt_channel_split <- dt_channel_split[, channel_network := toupper(gsub("[^a-zA-Z0-9]", "", channel_network))]

# split_by_symbols_column <- function(dt, category) {
#   col_name <- deparse(category)
#   col_name <- gsub(".*\\$", "", col_name)  # Remove "dt$" prefix
#   
#   
#   to_return <- dt[, (col_name) := fifelse(channel_network == "N/A", NA_character_, get(col_name))]
#   to_return <- to_return[, 
#                                        .(split_col = unlist(strsplit(get(col_name), "[/;,&]"))), 
#                                        by = setdiff(names(to_return), col_name)]
#   
#   to_return <- to_return[, (col_name) := toupper(gsub("[^a-zA-Z0-9]", "", split_col))]
# }
# 
# dt_split <- split_by_symbols_column(dt, category = substitute(dt$`channel_network`))

# View(dt_channel_split[, .N, by = dt_channel_split$channel_network][order(-N)])

## testing
# write.table(
#   dt_split[, .N, by = dt_split$channel_network][order(-N)],
#   file = paste(dirName, "new_channel_network", ".csv", sep = ""),
#   sep = ", ",
#   row.names = FALSE,
#   quote = FALSE,
#   col.names = c("channel_network", "N")  # Set column names explicitly
# )


### Back to older work


#### 2/10
#### Counting the frequency of dates

dirName = "./columnCounts/"
dir.create(dirName)

for (col in names(dt)) {
  # Create the file name dynamically
  file_name <- paste0("col_", col, "_Count.csv")
  
  sanitize_name <- function(name) {
    # Replace invalid characters (spaces, slashes, etc.) with underscores
    gsub("[^a-zA-Z0-9]", "_", name)
  }
  
  sanitized_col <- sanitize_name(col)

  write.table(
    dt[, .N, by = eval(as.name( col))][order(-N)],
    file = paste(dirName, sanitized_col, ".csv", sep = ""),
    sep = ", ",
    row.names = FALSE,
    quote = FALSE,
    col.names = c(col, "N")  # Set column names explicitly
  )
}

# Plot the histogram of frequencies bucketed by month or year
plot_dates_histogram <- function(dt, byMonth = FALSE, byYear = FALSE) {
  # Conditionals on whether or not this is month or year
  title <- ifelse(byMonth, "Histogram of Date Frequencies by Month", "Histogram of Date Frequencies by Year")
  x_label <- ifelse(byMonth, "Month", "Year")
  x_aesthetic <- if (byMonth) dt$month else dt$year
    
  # Return the plot
  ggplot(dt, aes(x = x_aesthetic)) +
    geom_bar(position = "dodge", color = "black") +
    labs(
      title = title,
      x = x_label,
      y = "Frequency",
      fill = "Category" # Legend title for the fill aesthetic
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

## Display the plots
## png(filename = "byMonth.png")
plot_dates_histogram(dt = dt_final, byMonth = TRUE)
## dev.off()

##png(filename = "byYear.png")
plot_dates_histogram(dt = dt_final, byYear = TRUE)
##dev.off()


### 2/17

# dt_split[, .N, by = .(`channel_network`)][order(-N)]


# ### frequency over time by network (grab top n)
# 
# 
# byMonth = FALSE
# x_aesthetic <- if (byMonth) dt$month else dt$year
# 
# dt_clean[, year := as.numeric(year)]
# n=1
# dt_clean[, year_bucket := paste0((year %/% n) * n, "-", ((year %/% n) * n + n - 1))]
# dt_top_channels <- dt_clean[, .N, by = channel_network][order(-N)][1:8, channel_network]
# dt_filtered <- dt_clean[channel_network %in% dt_top_channels]
# dt_agg <- dt_filtered[, .N, by = .(year_bucket, channel_network)]
# 
# ggplot(dt_agg, aes(x = year_bucket, y = N, color = channel_network, group = channel_network)) +
#   geom_line(size = 1) +
#   geom_point(size = 3)
#   # labs(
#   #   title = title,
#   #   y = "Frequency",
#   #   fill = "Category" # Legend title for the fill aesthetic
#   # ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 

  
bucket_size = 3
category <- substitute(dt_clean$Format)
top_n = 10

plot_frequency_by_category <- function(dt, bucket_size, category, top_n) {
  col_name <- deparse(category)
  col_name <- gsub(".*\\$", "", col_name)  # Remove "dt$" prefix
  
  dt_clean <- dt
  dt_clean[, year := as.numeric(year)]
  
  
  dt_clean[, year_bucket := paste0((year %/% bucket_size) * bucket_size, "-", ((year %/% bucket_size) * bucket_size + bucket_size - 1))]
  dt_top_channels <- dt_clean[, .N, by = col_name][order(-N)][1:top_n][, get(col_name)]
  dt_filtered <- dt_clean[get(col_name) %in% dt_top_channels]
  dt_agg <- dt_filtered[, .N, by = .(year_bucket, get(col_name))]
  
  ggplot(dt_agg, aes(x = year_bucket, y = N, color = dt_agg$get, group = dt_agg$get)) +
  geom_line(linewidth = 1) +
    geom_point(size = 3) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 70, hjust = 1))
  
}

plot_frequency_by_category(dt_final, bucket_size = 4, category = substitute(dt_final$`channel_network`), top_n = 8)



## later do some combining manually of channels



## better reading in



