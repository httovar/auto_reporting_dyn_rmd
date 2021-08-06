# Set Up##############
library(dplyr)
library(rvest)
library(xml2)
library(tibble)
library(tidyr)
library(stringr)
library(readr)

#Setting working directory
setwd(eval(parse(text = keyring::key_get("proj_wd"))))


# Data Accessing Step ###################
#Web scraping data with rvest
german_charts_path <- "https://www.offiziellecharts.de/charts"

de_page <- read_html(german_charts_path)

info <- c('this-week', 'last-week', 'info-artist', 'info-title', 'info-label')
chart_scrap <-  tibble(current = numeric(length = 100),
                       last_week = numeric(length = 100),
                       artist = character(length = 100),
                       title = character(length = 100),
                       label = character(length = 100))


for (i in 1:5){
  chart_scrap[,i] <- de_page%>%
    html_nodes("body")%>%
    xml2::xml_find_all(paste0("//span[contains(@class, '", info[i], "')]"))%>% 
    rvest::html_text2()
}

#Meta data that need a little more processing and can't be looped with the rest
other_meta <- c("weeks_in_charts", "peak_in_charts")

for(i in 1:2){
  chart_scrap[other_meta[i]] <-  de_page %>%
    html_nodes("body")%>%
    xml2::xml_find_all(paste0("//span[contains(@class, 'plus-data')]"))%>% 
    rvest::html_text2()%>%
    .[seq(from=i, to=200, by=2)]%>%
    str_extract(pattern = "[0-9]+")%>%
    as.numeric()
}


#last touches
chart_scrap <- chart_scrap%>%
  mutate(current = as.numeric(current),
         last_week = as.numeric(last_week),
         last_week_fill = replace_na(last_week, replace = 0))


#Writing file to csv for processing in rmd file
#This will saved a csv file for each scheduled iteration
# 
# #First creating new folder in blogdown folder structure
dir_path <- paste("auto_reporting_blogdown","content","post",
                  paste0(Sys.Date(),"_charts_report"), sep = "\\")

dir.create(dir_path)

#ssaving csv file to new folder
write_csv(x = chart_scrap, file = paste0(dir_path,"//",Sys.Date(),"_charts_report.csv"))


# Dynamic Creation of RMarkdown Document ###########################################
#The code will separate each chunk of the Rmarkdown document for readability.
#At the end, these will be put together to a single string
#and saved as .rmd in the newly created folder

#Note that calling Sys.Date() now will save the date when the file is created. 
#This will later on make it possible for files to keep the same names and dates 
#when they are processed again at a different date.


#Yaml Header
yaml_header <- paste("---",
                     paste("title:", "Charts Report for Week of ", Sys.Date()),
                     paste("subtitle:","Based on Data Scraped from `www.offiziellecharts.de`"),
                     paste("date:", Sys.Date()),
                     paste("author:","Henning Tovar"),
                     paste("categories:", '["R", "Report","Music"]'),
                     paste("tags:", '["Dynamic RMarkdown",  "Automated Report"]'),
                     "---",
                     sep = "\n")

#First code chunk
code_chunk1 <- paste("```{r setup, include=FALSE, message=FALSE}",
                     "library(ggplot2)",
                     "library(gridExtra)",
                     "library(readr)",
                     "library(dplyr)",
                     paste0("chart_scrap <- read_csv('",Sys.Date(),"_charts_report.csv","')"),
                     "```",
                     sep = "\n")

#First descriptive element (includes inline code)
markdown_body1 <- paste(paste("This is a standardized report that analyses weekly developments",
                              "in the [Top 100 German Single charts](https://www.offiziellecharts.de/charts).",
                              "The data is automatically scraped in weekly intervals and the report is subsequently generated.",
                              "The following figures and tables show some basic development in the charts.",
                              "As the report is automatically generated, the content is standardized and each week's report comes",
                              "in the same layout. The Single charts rank which single has performed best during the previous week",
                              "(starting and ending on Friday). Performance is measured by the sum of sales, downloads, radio plays,",
                              "and streaming requests. For more information on this see the following",
                              "[website](https://www.offiziellecharts.de/info/faq). As this is a ranked evaluation, a smaller rank",
                              "(i.e. closer to 1) indicates better performance than a larger rank."),"",
                        paste("## Weekly Development in German Single Charts (Week Nr.",format(Sys.Date(), '%W'),")"),
                        paste("For this week, the best performing single is `r chart_scrap$title[chart_scrap$current==1]`",
                              "by `r chart_scrap$artist[chart_scrap$current==1]`. The song has been in the charts for",
                              "`r chart_scrap$weeks_in_charts[chart_scrap$current==1]` weeks and was",
                              "`r ifelse(chart_scrap$last_week_fill[chart_scrap$current==1]==0,",
                              "no = paste('on position', chart_scrap$last_week_fill[chart_scrap$current==1],",
                              "'last week'),yes = 'not in the charts last week')` last week. The song that is currently in the charts",
                              "and has been in the charts the longest is `r chart_scrap$title[which.max(chart_scrap$weeks_in_charts)]`",
                              "by `r chart_scrap$artist[which.max(chart_scrap$weeks_in_charts)]`. The song has been in the charts for",
                              "`r chart_scrap$weeks_in_charts[which.max(chart_scrap$weeks_in_charts)]` weeks and is currently ranked",
                              "`r chart_scrap$current[which.max(chart_scrap$weeks_in_charts)]`. For this week, there are",
                              "`r sum(is.na(chart_scrap$last_week))` newcomers to the list. The ranks of the new songs range from place",
                              "`r min(chart_scrap$current[is.na(chart_scrap$last_week)])` to",
                              "`r max(chart_scrap$current[is.na(chart_scrap$last_week)])`."),"",
                        paste("The plots below show a visual approach to the weekly developments. The four scatter plots below show",
                              "typical patterns in the data. For these plots, the most interesting data points are those that fall outside",
                              "the obvious patterns. These data points can indicate particularly rapid developments. The plot in the top left",
                              "shows the strong association between the current position in the charts and the rank it achieved in the previous",
                              "week. Observations in the bottom right or top left of this plot either improved their position substantially",
                              "relative to the previous week (bottom right) or performed substantially worse compared to the previous week",
                              "(top left)."),"",
                        paste("The plot in the top right shows the association between peak position and the rank a song achieved in the",
                              "previous week. Unsurprisingly, this association does not look strong. There is no reason to assume that songs",
                              "peaked just in the previous week. Rather the plot shows that many songs peaked towards the top of the charts and",
                              "are then moving towards the bottom. The top left half of the scatter plot is not achievable for any observation",
                              "as this would indicate the contradictory situation that a song achieved a higher rank in the previous week than",
                              "the peak rank it achieved overall."),"",
                        paste("The plots in the lower half show that songs typically do not stay in the charts for a long time and move",
                              "towards the lower ranks as the number of weeks in the charts increases. The plot on the left shows that",
                              "songs that peak towards the lower end of the charts often only remain a few weeks in the charts. But even",
                              "songs that peak towards the top of the charts typically do not remain in the charts for a long time.",
                              "The stay in the charts is `r round(median(chart_scrap$weeks_in_charts),0)` weeks. The scatter plot on",
                              "the left indicates that songs that have been in the charts for a relatively long time, typically are",
                              "in the lower half of the ranks. However, a song that places high in the current position and the number",
                              "of weeks in the charts (bottom right of the plot on the right) would display extraordinary performance over",
                              "a long time."),"",
                        sep="\n")

code_chunk2 <- paste("```{r scatter plots, echo=FALSE}",
                     "#Reporting Figures and Tables",
                     "#Scatter Plots",
                     "current_v_last <- ggplot(data = chart_scrap,
                                     aes(y = current, x=last_week,
                                     col=ifelse(is.na(last_week), yes = 'NA' , no = 'NonNA')))+
                                      geom_point(aes(y = current, x=last_week_fill),show.legend = F)+
                                      scale_color_manual(values = c('#FF0000','#000000'))+
                                      labs(title = 'Current Place of a Song \\nvs. Place in Previous Week',
                                           subtitle = 'New Songs are marked in Red',
                                           y='Current Place', x='Place in Previous Week')+
                                        theme_bw()",
                     "weeks_v_peak <- ggplot(data = chart_scrap,
                                             aes(x=weeks_in_charts, y=peak_in_charts))+
                                              geom_point()+
                                              labs(title = 'Peak Place in Charts \\nvs. Weeks in Charts',
                                                   x='Weeks in Charts', y='Peak Place')+
                                              theme_bw()",
                     "week_v_current <- ggplot(data = chart_scrap, aes(x=weeks_in_charts, y=current))+
                                      geom_point()+
                                      labs(title = 'Current Place in Charts \\nvs. Weeks in Charts',
                                           x='Weeks in Charts', y='Current Place')+
                                      theme_bw()",
                     "last_v_peak <- ggplot(data = chart_scrap, aes(x=last_week_fill, y=peak_in_charts,
                                                col=ifelse(is.na(last_week), yes = 'NA' , no = 'NonNA')))+
                                    geom_point(show.legend = F)+
                                    labs(title = 'Place in Previous Week \\nvs. Peak Position',
                                         subtitle = 'New Songs are marked Red',
                                         x='Position in Previous Week', y='Peak Place')+
                                    scale_color_manual(values = c('#FF0000','#000000'))+
                                    theme_bw()",
                     "grid.arrange(current_v_last,last_v_peak, weeks_v_peak,week_v_current, nrow=2)",
                     "``` ","",sep = "\n")

markdown_body2 <- paste("The bar plots below show the number of songs a respective label or artists is able to place",
                        "in charts simultaneously. The following tables then drill down into this and show the exact",
                        "artists and labels with more than one song in the charts. Unsurprisingly, very few artists are",
                        "able to place more than one song in the charts at the same time. Notably, collaborative songs are",
                        "not double counted for each collaborating artist but rather the collaboration is taken as a figurative artist.")

code_chunk3 <- paste("```{r Tables and Bar Plots, echo=FALSE}",
                     "chart_scrap%>%
                      count(artist)%>%
                      ggplot(aes(x=n))+
                      geom_bar()+
                      labs(title = 'Number of Songs simultaneously in the Top 100 Charts',
                           subtitle = 'By the Same Artists or Collarboration',
                           y='Frequency',
                           x=NULL,
                           caption = 'Source: https://www.offiziellecharts.de/charts')+
                      theme_bw()",
                     "chart_scrap%>%
                        count(artist,sort = T)%>%
                        filter(n>1)%>%
                        knitr::kable()",
                     "chart_scrap%>%
                      count(label)%>%
                      ggplot(aes(x=as.factor(n)))+
                      geom_bar()+
                      geom_bar()+
                      labs(title = 'Number of Songs simultaneously in the Top 100 Charts',
                           subtitle = 'By the Same Label',
                           y='Frequency',
                           x=NULL,
                           caption = 'Source: https://www.offiziellecharts.de/charts')+
                      theme_bw()",
                     "chart_scrap%>%
                        count(label, sort = T)%>%
                        filter(n>1)%>%
                        knitr::kable()",
                     "```", sep = "\n")

markdown_body3 <- paste("Lastly, the following table shows songs that for which the current",
                        "position also equals their peak positions. In general, these songs are",
                        "performing at their peak currently. However, it is also possible that a song",
                        "is returning to a place it peaked on previously or is remaining on a peak for a week.")

code_chunk4 <- paste("```{r table 2,echo=FALSE}",
                     "#Songs that are currently peaking",
                     "chart_scrap%>%
                        filter(current == peak_in_charts)%>%
                        select(artist, title, current)%>%
                        rename(Position = current)%>%
                        knitr::kable()",
                     "```",sep = "\n")

complete_doc <- paste(yaml_header,code_chunk1,markdown_body1, code_chunk2, markdown_body2,
                      code_chunk3, markdown_body3, code_chunk4, sep = "\n")

write(complete_doc,
      paste0(dir_path,"/",Sys.Date(),"_charts_report.Rmd"))

#Set wd to the working directory of the blogdown page. This is necessary for blogdown::build_site() to run properly
setwd("auto_reporting_blogdown")
source(file ="git_commands.R")

