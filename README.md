# TV News Archive Dashboard

An interactive dashboard for exploring and analyzing television news data from the Marion Stokes archive.

![Dashboard Screenshot](<img width="1435" alt="SampleDashboard" src="https://github.com/user-attachments/assets/1f40a1e5-094f-429d-8429-c57eae7d0682" />) 

## About the Project

This Shiny dashboard provides tools to visualize and analyze television news data from the Marion Stokes archive. The dashboard allows users to:

- Explore frequency distributions of different categories (like channel networks)
- Adjust time bucket sizes for temporal analysis
- Filter data by specific categories
- View top N items in interactive tables

## Data Source

The data backing this dashboard comes from the Marion Stokes Television News Archive:

[Marion Stokes Archive on archive.org](https://archive.org/details/marionstokes_201706)

## Live Dashboard

Access the live dashboard here:

[Analyzing Daniel Data Dashboard](https://jules-dev.shinyapps.io/analyzingdanieldata/)

## Code Structure

The dashboard consists of:

1. Data cleaning script (`superClean.R`)
2. Shiny app with UI and server components

### Main Features

- Interactive frequency plots with adjustable time buckets
- Dynamic filtering options
- Interactive data tables
- Responsive design that works on different screen sizes

## Installation & Usage

To run this dashboard locally:

1. Clone this repository
2. Ensure you have R and the required packages installed
3. Run the Shiny app

### Required R Packages

```r
install.packages(c("shiny", "ggplot2", "data.table", "DT"))
```


### Running the App
```r
source("superClean.R")
shiny::runApp()
```


## How to Use

1. Select a category from the dropdown menu
2. Adjust the bucket size (years) to group temporal data
3. Choose how many top items to display
4. Use the "View Only" filters to focus on specific subsets
5. The plot and table will update automatically
6. 
