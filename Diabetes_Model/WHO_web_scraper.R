# =========================================================================
# Title: WHO_web_scraper.R
# Author: Matt Buckley
# Description: 
# Sources: World Health Organization
# Liscense: MIT license: http://www.opensource.org/licenses/mit-license.php 
# =========================================================================


# =========================================================================
# Scrape data on diabetes prevalence from the relevant page on the World 
# Health Organization site
# =========================================================================
library(XML)
library(RCurl)

# the url of the section of the WHO website that contians diabetes info
who_url <- "http://www.who.int/diabetes/facts/world_figures/en/index"

# a vector to store the results
results<-character(0)

# go through all 6 pages containing the info we want, and store
# the html in a list
for (page in 1:6) {
  who_search <- paste(who_url, page, '.html', sep='')
  page = readHTMLTable(htmlParse(who_search))
  results = c(page, results)
}



df <- data.frame()

for (i in results) {
  df <- rbind(df,i)
}

df[,'2000'] <- gsub(',', '', df[,'2000'])
df[,'2030'] <- gsub(',','', df[,'2030'])
df['Country'] <- gsub(' ', '_', df[,'Country'])
df <- df[df[,1] != 'Total',]

country_subset <- df[(df[,1] %in% rownames(all_data)),]

diabetes_data <- cbind (country_subset, populations.df)

diabetes_data <- cbind(diabetes_data[,2:4], 
                       normalized_diabetes_rates_2000=10*(as.numeric(diabetes_data[,'2000'])/as.numeric(diabetes_data[,'population_2000'])),
                       row.names=diabetes_data[,1])
