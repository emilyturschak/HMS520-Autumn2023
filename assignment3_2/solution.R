#Assignment 3

library("readr")
library("dplyr")
library("data.table")


base_url <- "https://static.usafacts.org/public/data/covid-19"
deaths_url <- paste(base_url, "covid_deaths_usafacts.csv", sep = "/")
cases_url <- paste(base_url, "covid_confirmed_usafacts.csv", sep = "/")
population_url <- paste(base_url, "covid_county_population_usafacts.csv", sep = "/")

# options are "read.csv", "read_csv" and "fread"
csv_loader <- fread

deaths <- csv_loader(deaths_url)
cases <- csv_loader(cases_url)
population <- csv_loader(population_url)

#Data cleaning

#Clean column names
clean_column_names <- function(df) {
  colnames(df) <- tolower(colnames(df))
  if ("date" %in% colnames(df)) {
    df$date <- as.Date(df$date, format = "%Y-%m-%d")
  }
  return(df)
}

deaths <- clean_column_names(deaths)
cases <- clean_column_names(cases)

#Collect date column names
death_dates <- colnames(deaths)[colnames(deaths) != "countyFIPS" & colnames(deaths) != "County Name" & colnames(deaths) != "State" & colnames(deaths) != "stateFIPS"]
case_dates <- colnames(cases)[colnames(cases) != "countyFIPS" & colnames(cases) != "County Name" & colnames(cases) != "State" & colnames(cases) != "stateFIPS"]

#Aggregate deaths, cases, and population by state
deaths <- deaths[, .(deaths = sum(deaths)), by = State]
cases <- cases[, .(cases = sum(cases)), by = State]
population <- population[, .(population = sum(population)), by = State]

#Reshape deaths and cases
deaths_long <- melt(deaths, id.vars = "State", measure.vars = death_dates, variable.name = "date", value.name = "deaths")
cases_long <- melt(cases, id.vars = "State", measure.vars = case_dates, variable.name = "date", value.name = "cases")

#Merge deaths, cases, and population
counts <- merge(deaths_long, cases_long, by = c("State", "date"))
counts <- merge(counts, population, by = "State")

#Data validation and processing 
#Order data by state and date 
counts <- counts[order(counts$state, counts$date), ]

#Compute new deaths and cases 
counts$new_deaths <- unlist(tapply(counts$deaths, counts$state, function(x) c(x[1], diff(x))))
counts$new_cases <- unlist(tapply(counts$cases, counts$state, function(x) c(x[1], diff(x))))

#Replace negative values with 0 
counts$new_deaths[counts$new_deaths < 0] <- 0
counts$new_cases[counts$new_cases < 0] <- 0

#Recompute cumulative sum of new deaths and new cases
counts$deaths <- ave(counts$new_deaths, counts$state, FUN = cumsum)
counts$cases <- ave(counts$new_cases, counts$state, FUN = cumsum)

#Compute infection fatality rate 
counts$ifr <- counts$deaths / counts$cases

#Compute mortality rate 
counts$mr <- counts$deaths / counts$population

#Data analysis and results
#List the top five states with the most deaths and cases
top_five_deaths <- counts %>% group_by(state) %>% summarise(total_deaths = max(deaths)) %>% top_n(5, total_deaths)
top_five_cases <- counts %>% group_by(state) %>% summarise(total_cases = max(cases)) %>% top_n(5, total_cases)

#List the top five states with the highest infection fatality rate and mortality rate
top_five_ifr <- counts %>% group_by(state) %>% summarise(avg_ifr = mean(ifr, na.rm = TRUE)) %>% top_n(5, avg_ifr)
top_five_mr <- counts %>% group_by(state) %>% summarise(avg_mr = mean(mr, na.rm = TRUE)) %>% top_n(5, avg_mr)
