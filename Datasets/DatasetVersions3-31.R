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
                     sheet = s, col_types = c("text", "text", "text", "text", "text", 
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

write.table(dt, file = "CrudeMerged.csv", sep = ", ",
            row.names = FALSE)



