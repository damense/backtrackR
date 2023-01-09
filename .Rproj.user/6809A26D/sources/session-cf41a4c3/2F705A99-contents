# Author:       David Mendez
# Date:         09Jan2023
# Description:  File that creates a dataframe with info from CRAN.

## Load packages ----
library(tidyverse)
library(rvest)
library(readtext)
library(flextable)
library(webdriver)
library(lubridate)

## Define urls ----
url <- "https://cran.r-project.org/web/packages/available_packages_by_name.html"
url_basic <- "https://cran.r-project.org/web/packages/"

## Download hyperlinks ----
webtxt <- read_html(url) %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr('href')

## Prepare to crawl ----
webs <- paste0(url_basic,unlist(str_split(webtxt,"../../")))

packs <- unlist(str_split(unlist(str_split(webtxt,
                                           "../../web/packages/")),
                          "/index.html"))
packs <- packs[packs!=""]
packs <- packs[!grepl("*#",packs)]

db_packs <- list()

## crawl through the cran pages ----
for (i in 1:length(packs)){
        url_temp <- paste(url_basic,packs[i],'/index.html', sep = "")
        all_info <- read_html(url_temp)
        
        #get name, title and description of the package
        name <- packs[i]
        title<- as.character(all_info %>% html_nodes("h2"))
        desc<- as.character({all_info %>% html_nodes("p")}[1])
        
        #rest of the info is in the body of the html code
        html_doc <- as.character(all_info %>% html_nodes("td"))
        html_doc <- gsub("<td>","",gsub("</td>","",html_doc))
        
        #get the indices of the different pieces of info
        vers <- which(html_doc == "Version:")+1
        dep <- which(html_doc == "Depends:")+1
        imp <- which(html_doc == "Imports:")+1
        pub <- which(html_doc == "Published:")+1
        if (length(vers)>0){
                version <- html_doc[vers]
        }
        if (length(dep)>0){
                # Get the depends and clean starting spaces and brackets
                depend <- gsub("^ ",
                               "",
                               gsub("\n",
                                    "",
                                    gsub("<(.*?)>",
                                         "",
                                         str_split(html_doc[dep], 
                                                   ","
                                         )[[1]]
                                    )
                               )
                )
                # store the version of R used
                r_vers <- gsub("R \\(",
                               "",
                               gsub("\\)",
                                    "",
                                    depend[1]
                               )
                )
                depend[1] <- "R"
                
        }
        if (length(imp)>0){
                # get the imports and clean the text from new lines, brackets and spaces
                imports <- gsub(" \\((.*?)\\)",
                                "",
                                gsub("^ ",
                                     "",
                                     gsub("\n",
                                          "",
                                          gsub("<(.*?)>",
                                               "",
                                               str_split(html_doc[imp], 
                                                         ","
                                               )[[1]]
                                          )
                                     )
                                )
                )
        } else {
                # if no imports, leave empty
                imports <- ''
        }
        if(length(pub)){
                # publishing date
                date_pub <- as.Date(html_doc[pub])
        }
        
        db_packs[[i]] <- data.frame(name=name,
                                    title=title,
                                    desc=desc,
                                    vers=vers,
                                    depend=paste(depend,collapse=","),
                                    r_vers=r_vers,
                                    imports=paste(imports,collapse=","),
                                    date_pub=date_pub)
        
        
        # printer to get progress every 1k packages 
        if((i/1000)%%1 ==0){
                print(paste(round(i*100/length(packs)),"%"))
        }
        # to not overload the server
        Sys.sleep(0.001)
}

df_packs <- do.call("rbind", db_packs)
write.csv(df_packs,"df_packs.csv")