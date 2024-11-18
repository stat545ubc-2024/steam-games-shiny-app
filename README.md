[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/_WsouPuM)

# Steam Game Shiny App


#### [View the Live Shiny App]( https://zhili617.shinyapps.io/steam_games_shiny_app/)

### Content 

1. [Description](#description)
  
2. [Dataset Source](#dataset-source)
  
3. [How to Run This App Locally](#how-to-run-this-app-locally)
  

## Description
This **steam game shiny app** is an interactive web application designed to help 
users explore and analyze data related to Steam games. This app contain two main 
pages:

### 1. **Data Table View**:

* **Interactive Filtering**: 
 - Users can select one or more variables to see detailed information about each Steam game. 
 
 - Among these selected variables, the user can further enter text or values to do a precise Search.
 
 - If users want to search certain name or id of team games, they can use the search bar on the top right.

* **Sorting**: 
Each variable has two sorting arrows. If a variable is numerical, it will be sorted by number;
if the variable is not numerical, it will follow lexicographic order to sort.

* **Download Data**: 
After applying these filters, users can download the result dataset as a CSV file for offline analysis. 
If there are no selected variables, the download file would be the original steam games dataset.

* **Reset Filters**: 
Users can click reset button to reset all filters to their default state.

* **Navigation**: 
Users can click *Go to Graph page* to switch to graph view. 

### 2. **Data Graph View**:

* **Graph Selection**: 
There are three variables' distribution histograms. (discount price, original price, and achievements)
Users can see these graphs by clicking the corresponding button.

* **Dynamic Histograms**: 
Use a slider to select the range of discount prices, 
updating the graphs in real-time based on the selected range.
Users can control the range of discount prices they are interested in analyzing.
By doing so, users can explore the relationship between discount prices and original prices 
and explore the relationship between discount prices and achievements. 

* **Navigation**: 
Users can click *Go to Table page* to switch to data table view. 


## Dataset Source

The data utilized in this application is sourced from the [datateachr](https://github.com/UBC-MDS/datateachr.git) package, 
which provides curated datasets for educational purposes.

**Package**: [datateachr](https://github.com/UBC-MDS/datateachr.git)

**Dataset**: *steam_games*; This dataset contains detail information of games from steam shop.

## How to Run This App Locally

If you wish to run this application locally, 

1. Clone this repository
```bash
   git clone https://github.com/yourusername/steam-games-shiny-app.git
```

2. Navigate to the Project Directory
```bash
cd steam-games-shiny-app
```

3. Install the necessary packages if haven't install.
```r
install.packages(c("shiny", "DT", "dplyr", "ggplot2", "datateachr", "tidyverse", "devtools"))
devtools::install_github("UBC-MDS/datateachr")
```

4. Run the app
```r
library(shiny)
runApp()
```

For future information, please see [Sharing apps to run locally](https://shiny.posit.co/r/articles/share/deployment-local/)
  