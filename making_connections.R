# Author:       David Mendez
# Date:         09Jan2023
# Description:  File the takes a dataframe with data from packages and creates a
#               dataframe with the connections between them. This file treats
#               dependencies and imports as the same


library(stringr)

## Read data ----
df_packs <- read.csv("df_packs.csv")

## Create the connections dataframe ----
connections <- data.frame(source=character(0),
                          target=character(0))
for (j in 1:dim(df_packs)[1]){
        new_df <- data.frame(target=unlist(c(str_split(df_packs$depend[j],","),
                                             str_split(df_packs$import[j],","))))
        new_df$source <- df_packs$name[j]
        connections <- rbind(connections,
                             new_df)
}

## Tidy the dataframe erasing empty values and brackets ----
connections <- distinct(connections[connections$target!="",])
connections$target <- gsub(" \\((.*?)\\)", "", connections$target)
connections$source <- gsub(" \\((.*?)\\)", "", connections$source)

write.csv(connections,"connections.csv")