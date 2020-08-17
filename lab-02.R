# Catherine Rauch
# 8/16/2020
# lab 02 script to be converted to Rmd

coviddata <- read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')

stateofinterest <- 'California'


## Question 1

addednewcases <- coviddata %>% # filter data by state of interest
  filter(state == stateofinterest) %>%
  group_by(county, date) %>%
  summarise(cases = sum(cases), fips) %>%
  mutate(newCases = cases - lag(cases)) %>% # creates new column withe daily new cases
  ungroup()

mostnewcases <- addednewcases %>% # finds top 5 counties with most new cases per day
    filter(date == max(date)) %>%
    slice_max(newCases, n = 5) %>%
    select(county, newCases)

mostcumulativecases <- addednewcases %>% # finds top 5 counties with most cumulative cases
  filter(date == max(date)) %>%
  slice_max(cases, n = 5) %>%
  select(county, cases)

knitr::kable(mostnewcases, caption = "Most New Cases California Counties", col.names = c("County", "New Cases"),
             format.args = list(big.mark = ","))

knitr::kable(mostcumulativecases, caption = "Most Cumulative Cases California Counties",
             col.names = c("County", "Total Cases"), format.args = list(big.mark = ","))


PopulationEstimates <- read_excel("~/github/geog-176A-labs/data/PopulationEstimates.xls", skip = 2)
dim(PopulationEstimates)
nrow(PopulationEstimates)
head(PopulationEstimates)

populationestimates <- PopulationEstimates %>% # select only necessary columns
  select(fips = "FIPStxt", state = "State", Area_Name, pop2019 = "POP_ESTIMATE_2019")


CovidandPopulation <- addednewcases %>%
  inner_join(populationestimates, by = "fips" ) # join to previous data


mostnewcasespc <- CovidandPopulation %>% # finds top 5 counties with daily new cases per capita
  filter(date == max(date)) %>%
  group_by(county) %>%
  mutate(casepercap = cases / pop2019) %>%
  ungroup() %>%
  slice_max(casepercap, n = 5) %>%
  select(county, casepercap)


mostcumulativecasespc <-CovidandPopulation %>% # finds top 5 counties with cumulative cases per capita
  filter(date == max(date)) %>%
  group_by(county) %>%
  mutate(newcasepercap = newCases / pop2019) %>%
  ungroup() %>%
  slice_max(newcasepercap, n = 5) %>%
  select(county, newcasepercap)

knitr::kable(mostnewcasespc, caption = "Most New Cases California Counties Per Capita", col.names = c("County", "New Cases Per Capita"))

knitr::kable(mostcumulativecasespc, caption = "Most Cumulative Cases California Counties Per Capita",
             col.names = c("County", "Total Cases Per Capita"))

numofdays <- 14

CovidandPopulation %>%
  filter(date == (max(date) - numofdays)) %>%
  group_by(county) %>%
  summarise(ncases =  100000 * (sum(newCases) / pop2019)) %>%
  arrange(desc(ncases))

state.of.interest = "Washington"


## Question2

statesofinterest <- c("New York", "California", "Louisiana","Florida")

coviddatastate <- coviddata %>%
  filter(state %in% statesofinterest) %>%
  group_by(state, date) %>%
  summarise(cases = sum(cases)) %>%
  mutate(newCases = cases - lag(cases),  roll7 = rollmean(newCases, 7, fill = NA, align="right")) %>% # creates new column withe daily new cases
  ungroup()

  coviddatastate %>%
  ggplot(aes(x = date, y = newCases)) +  geom_col(aes(y = newCases, col = state))+
  geom_line(aes(col = state)) + geom_line(aes(y = roll7), size = 1) +
  facet_grid(~state, scale = "free_y", space = "free_y") + ggthemes::theme_solarized()+
  theme(legend.position = "bottom") +
  theme(legend.position = "NA") +
  labs(title = "Daily new cases by State", y = "Daily New Count", x = "Date",
       caption = "Lab 02")


coviddatastate %>%
  inner_join(populationestimates, by = c("state" = "Area_Name") ) %>%
  group_by(state, date) %>%
  summarise(newcases = sum(newCases), pop2019, .groups = "drop") %>%
  mutate(newcasepercap = (newcases / pop2019), roll7 = rollmean(newcasepercap, 7, fill = NA, align="right")) %>%
  ggplot(aes(x = date, y = newcasepercap)) +
  geom_col(aes(y = newcasepercap, col = state))+
  geom_line(aes(col = state)) + geom_line(aes(y = roll7), size = 1)+
  facet_grid(~state) + ggthemes::theme_solarized() +
  theme(legend.position = "bottom") +
  theme(legend.position = "NA") +
  labs(title = "Daily new cases Per Capita by State", y = "Daily New Count Per Capita", x = "Date",
       caption = "Lab 02")




## Question 3

countydata <- read_csv("~/github/geog-176A-labs/data/county-centroids.csv")

# join it to your raw COVID-19 data using the statefp and fips attributes.

Covidwithcounty <- coviddata %>%
                  full_join(populationestimates, by = c("fips" = "fips"))

# Xcoord = sum(latittude * wi) / sum(wi)

# Ycoord = sum(longititude * wi) / sum(wi)


