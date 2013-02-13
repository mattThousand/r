# =========================================================================
# Title: web_scraper_thekickbackmachine.R
# Author: Matt Buckley
# Description: Scrape thekickbackmachine.com for information on successful
# and failed Kickstarter campaigns
# Liscense: MIT license: http://www.opensource.org/licenses/mit-license.php 
# =========================================================================

## STEPS
# 1. specify some empty vectors to fill with scraped info
# 2. set base url
# 3. scrape the first 2000 browsable pages from 
# http://www.thekickbackmachine.com 
# 4. extract info on project titles, project categories,
# initial fundraising goal amount, and amount ultimatety 
# pledged
# 5. organize the results into a data frame

library(XML)
library(RCurl)


# specify some empty vectors to fill with scraped info
success<-numeric()
title<-character()
subcategory<-character()
category<-character()
goal<-numeric()
pledged<-numeric()


#set base url
kickscrape_url <- "http://www.thekickbackmachine.com/browse/all"


# scrape the first 2000 browsable pages from http://www.thekickbackmachine.com
for (i in 1:2000) {
  kickscrape_page <- paste(kickscrape_url,"/?page=",i, sep="")
  webpage <- getURL(kickscrape_page)
  
  pagetree <- htmlTreeParse(webpage, useInternalNodes=T)
  
  
  # extract project names
  project_names <- xpathApply(pagetree, "//h4//a[@href]",xmlValue)
  for (i in 1:length(project_names)) {
    title<-c(title, project_names[[i]][1])
  }
  
  #extract project categories
  project_categories <- xpathApply(pagetree, "//div[@class='caption']//p//a[@href]",xmlValue)
  for (i in 1:length(project_categories)) {
    project_categories[[i]][1]=gsub("/\n/", "", project_categories[[i]][1])
    project_categories[[i]][1]=gsub("^\\s+|\\s+$", "", project_categories[[i]][1])
    subcategory<-c(subcategory, project_categories[[i]][1])
  }
  
  # extract $ amounts from <p> tags
  p_tags <- xpathApply(pagetree, "//p",xmlValue)
  
  project_goals<-character()
  
  for (i in 1:length(p_tags)) {
    if (regexpr("(pledged)", p_tags[[i]][1]) > 0) {
      project_goals=c(project_goals, strsplit(p_tags[i][[1]][1],'\n'))
    }
  }
  for (i in 1:length(project_goals)) {
    a_p= gsub("^\\s+|\\s+$", "", project_goals[[i]][1])
    a_p= gsub("\\D+", "", a_p)
    a_g=strsplit(project_goals[[i]][3],'goal')[[1]][1]
    a_g=gsub("\\D+", "", a_g)
    if ((as.numeric(a_p))<(as.numeric(a_g))) {
      success=c(success, 0)
    }
    else {
      success=c(success, 1)
    }
    goal<-c(goal,as.numeric(a_g))
    pledged<-c(pledged,as.numeric(a_p))
  }
}

# create parent category for 'subcategory' so that R can read it as a categorical variable (R only
# reads categorical variables with 32 or fewer levels)

for (i in subcategory) {
  
  if (i %in% c('Art Book',"Children's Book",
               'Nonfiction','Publishing','Periodical',
               'Poetry')) {
    category<-c(category, 'written_word')
  }
  else if (i %in% c('Design','ProductDesign','Hardware','OpenHardware')) {
    category<-c(category, 'design+materials')
  }
  else if (i =='Food') {
    category<-c(category, 'food')
  }
  else if (i %in% c('Fashion')) {
    category<-c(category, 'fashion')
  }
  else if (i %in% c('Film&Video','Documentary','NarrativeFilm','ShortFilm')) {
    category<-c(category, 'film&video')
  }
  else if (i %in% c('PerformanceArt','Dance','Sculpture','MixedMedia','Comics')) {
    category<-c(category, 'other_art')
  }
  else if (i %in% c('ClassicalMusic','ClassicalMusic','Hip-Hop',
                    'Rock','Country&Folk','Music','Jazz','ElectronicMusic','WorldMusic')){
    category<-c(category, 'music')
  }
  else if (i %in% c('Photography','PublicArt','PerformanceArt','Animation',
                    'GraphicDesign','Art','Comics','ArtBook','ConceptualArt',
                    'DigitalArt','Illustration','Painting')){
    category<-c(category, 'visual_art')
  }
  else if (i=='Journalism') {
    category<-c(category, 'journalism')
  }
  else if (i %in% c('Games','TabletopGames','VideoGames','Board&CardGames')){
    category<-c(category, 'games')
  }
  else if (i=='Theater') {
    category<-c(category, 'theater')
  }
  else if (i %in% c('Technology','OpenSoftware')) {
    category<-c(category, 'technology')
  }
  else {
    category<-c(category,'other')
  }
}

# organize the results into a data frame
kickscrape.df<-data.frame(success=as.factor(success), title=title, category=as.factor(category),
                          subcategory=subcategory,pledged=pledged,goal=goal)

# save kickscrape.df
save(kickstarter.df, file='kickstarter_df.Rda')