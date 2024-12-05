[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/_WsouPuM)

# Steam Game Shiny App


#### [View the Live Shiny App version 1.0.0]( https://zhili617.shinyapps.io/steam_games_shiny_app/)
#### [View the Live Shiny App version 2.0.0]( https://zhili617.shinyapps.io/steam_games_shiny_app_v2/)


### Content 

1. [Description](#description)
  
2. [Dataset Source](#dataset-source)

3. [Other Source](#other-source) (new)
  
4. [How to Run This App Locally](#how-to-run-this-app-locally)
  

## Description
This **steam game shiny app** is an interactive web application designed to help 
users explore and analyze data related to Steam games. In the newest version, this shiny app uses a sky blue theme to make the overall look better.
This app contain three main pages:

### 1. **Data Table View**:

* **Interactive Filtering**: 
1. Users can select one or more variables to see detailed information about each Steam game. 
 
2. Among these selected variables, the user can further enter text or values to do a precise search. 
 
3. If users want to search certain name or id of team games, they can use the search bar on the top right.

4. Variable descriptions are provided so that users can understand the meaning and details of each variable. (new)

* **Sorting**: 

Each variable has two sorting arrows. If a variable is numerical, it will be sorted by number;
if the variable is not numerical, it will follow lexicographic order to sort.

* **Download Data**: 
1. After applying these filters, users can download the result dataset as a CSV file for offline analysis. 

2. If no variables are selected, the download file will contain the original Steam games dataset.

* **Reset Filters**: 

Users can click reset button to reset all filters to their default state.

* **Navigation**: 

Users can click *Data Graph* on the top to switch to graph view and click *Help* on the top to witch to help page.

* **Real-Time Currency Conversion**：(new)
1. Prices can be converted into different currencies using real-time exchange rates.
(Supported currencies include USD, EUR, GBP, JPY, and CNY.)

2. The secondary search range of discount price and original price will follow the new currency.

* **Clickable Game Titles**: (new)
Game names in the datasheet are clickable links that open the corresponding Steam page in a new tab.

### 2. **Data Graph View**:

* **Graph Selection**: 
1. There are three variables' distribution histograms. (discount price, original price, and achievements)

2. Users can see these graphs by clicking the corresponding button.

* **Dynamic Histograms**: 
1. Use a slider to select the range of discount prices, updating the graphs in real-time based on the selected range.

2. Users can control the range of discount prices they are interested in analyzing.

3. By doing so, users can explore the relationship between discount prices and original prices 
and explore the relationship between discount prices and achievements. 

* **Download Graph** (new)
Users can download the current graph with selected discount price range.


* **Navigation**: 
Users can click *Data Table* on the top to switch to data table view and click *Help* on the top to witch to help page.

### 3. **Help Page**: (new)
* **General description**:
1. The help page provides detailed instructions and explanations for each feature of this shiny app.

2. Users can refer to this page to learn how to navigate and use the app effectively.

* **Navigation**: 

Users can click *Data Table* on the top to switch to data table view and click *Data Graph* on the top to switch to graph view.


## Dataset Source

The data utilized in this application is sourced from the [datateachr](https://github.com/UBC-MDS/datateachr.git) package, 
which provides curated datasets for educational purposes.

**Package**: [datateachr](https://github.com/UBC-MDS/datateachr.git)

**Dataset**: *steam_games*; This dataset contains detail information of games from steam shop. It currently has 40833 rows and 21 columns.

## Other Source

This shiny app uses the [ExchangeRate-API](https://www.exchangerate-api.com/) to obtain real-time currency exchange rates, allowing users to view prices in different currencies.

Also, this app use the [Cerulean](https://bootswatch.com/cerulean/) to select the Sky Blue Theme.


## How to Run This App Locally

If you wish to run this application locally, 

1. Clone this repository
```bash
   git clone https://github.com/zhili617/steam-games-shiny-app.git
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

For further information, please see [Sharing apps to run locally](https://shiny.posit.co/r/articles/share/deployment-local/)
  