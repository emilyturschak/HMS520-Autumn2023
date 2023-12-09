#Assignment3 redo 
library("readr")
library("dplyr")
library("data.table")


base_url <- "https://static.usafacts.org/public/data/covid-19"

#first attempt answer:
#deaths_url <- paste(base_url, "covid_deaths_usafacts.csv", sep = "/")
#cases_url <- paste(base_url, "covid_confirmed_usafacts.csv", sep = "/")
#population_url <- paste(base_url, "covid_county_population_usafacts.csv",
#                        sep = "/")

#load data using list
data_urls <- list(
  cases = paste0(base_url, "/covid_confirmed_usafacts.csv"),
  deaths = paste0(base_url, "/covid_deaths_usafacts.csv"),
  population = paste0(base_url, "/covid_county_population_usafacts.csv")
)

# options are "read.csv", "read_csv" and "fread"
csv_loader <- fread

#first attempt answer: 
##deaths <- csv_loader(deaths_url)
##cases <- csv_loader(cases_url)
##population <- csv_loader(population_url)

#new attempt using lapply:
data <- lapply(data_urls, csv_loader)

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
#first attempt answer:
#death_dates <- colnames(deaths)[colnames(deaths) != "countyFIPS" & colnames(deaths) != "County Name" & colnames(deaths) != "State" & colnames(deaths) != "stateFIPS"]
#case_dates <- colnames(cases)[colnames(cases) != "countyFIPS" & colnames(cases) != "County Name" & colnames(cases) != "State" & colnames(cases) != "stateFIPS"]

#new attempt answer using lapply
dates <- lapply(
  data[c("cases", "deaths")],
  function(dt) names(dt)[grep("\\d{4}-\\d{2}-\\d{2}", names(dt))]
)

#Aggregate deaths, cases, and population by state
#first attempt answer:
##deaths <- deaths[, .(deaths = sum(deaths)), by = State]
##cases <- cases[, .(cases = sum(cases)), by = State]
##population <- population[, .(population = sum(population)), by = State]

#new attempt answer using lapply
for (key in c("cases", "deaths")) {
  data[[key]] <- data[[key]][, lapply(.SD, sum), by = state, .SDcols = dates[[key]]]
}
data[["population"]] <- data[["population"]][
  , list(population = sum(population)), by = state
]

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
