#General information
##The Idea
This project was initiated with an idea to scrape reviews from Google Play store and than anaylze/visualize them inside a dashboard. As an example, this particular project analyzes data about eight offline navigation apps but it can be used to analyze any other set of mobile application.

##Important notes
When entering the dashboad via [link](https://fsviben.shinyapps.io/offline_navigation_dashboard/) give it arround 45-60 second to load. Dashboard is hosted on a free shinyapps.io server and it is not the fastest.
Prior to entering the dashboard and while exploring it, I would advise you to check out the [Dashboard user manual.](https://documentcloud.adobe.com/link/review?uri=urn:aaid:scds:US:bd0dcada-2734-44f6-b9ae-b203feb24874#pageNum=1)

##Project steps
Project itself is divided into three key steps (each is represented by a folder in the main repository folder):
  1. Web scraping - crawling through data on G-Play store and scraping it
  2. Data manipulation - cleaning data and preparing specific tables which will be used to create the final dashboard
  3. Dashboard - creating interactive dashboard to visualize and analyze reviews

##Technologies
Whole project (all three steps) is written in R.
Scraping and data manipulation are written in standard R scripts while dashboard is written in R Markdown file using `shiny` and `flexdashboard` packages.
Dashboard is hosted inside the shinyapps.io server.

##Dependencies
Apart from the set of popular R packages like `tidyr`, `dplyr`, `ggplot2`... following packages have also been used:
`RSelenium` & `wdman` - used for web crawling and web scraping
`bsts` - used for analyzing and creating ML models on time series data
`tm` and `sentimentr` - used for text data manipulation and sentiment analytics
`shiny` and `flexdashboard` - used to create dashboard

#Project steps detailed information
##1. Web scraping
As mentioned earlier web crawling and web scraping were done with R allthough other tools (eg. Python) would probably be more efficient in it.
R script needed arround 8 hours of runtime to scroll through the reviews and click on all "show more" and simillar buttons in order to scrape cca 40.000 reviews. If someone would need to scrape reviews from a very popular app which has 100.000+ reviews, this script will probably brake at some point. Though dashboard was the main delivery objective of this project and not the web scraping script.

##2. Data manipulation
This step of the project has single goal of analyzing & cleaning data, creating models and exporting simple tables on which the final dashboard will be used. Allthough all of this can be done within the dashboard script, this step was introduced to simplyfy the dashboard script as much as possible.

##3. Dashboard - this was the main objective of the project. Dashboard is interactive and it covers several analytics like word frequency, sentiment analysis, rating analysis etc. Majority of the dashboard was created with `flexdashboard` package, while `shiny` was introduced only when interactivity was needed.


