# Author:       David Mendez
# Date:         09Jan2023
# Description:  Function that takes as an argument the name of an R package and 
#               a dataframe with all connections between packages (created by 
#               making_connections.R) and outputs a diagram that shows all
#               links between packages.


#connections <- read.csv("connections.csv")


backtrackR <- function(name, connections){
        # name needs to be a string, the name of a package.
        # connections needs to be a dataframe with all connections between packages
        #like the one created by making_connections.R
        
        
        
        ## libraries needed ----
        library(tidyverse)
        library(diagram)
        
        ## Tidying data -----
        # names of packages are stored in a list where each element is a level.
        level <- 1
        packages <- list()
        packages[[level]] <- name
        number_new_packages <- 1
        while (number_new_packages>0){
                level <- level+1
                packages[[level]] <- unique(connections[connections$source %in% unlist(packages[[level-1]]),]$target)
                number_new_packages <- length(unique(unlist(packages[1:(level)])))-
                        length(unique(unlist(packages[1:(level-1)])))
                
                # if a package is in a higher level, it is deleted from lower 
                # levels to prevent duplications
                for (i in 1:(level-1)){
                        packages[[i]] <- packages[[i]][!packages[[i]] %in% packages[[level]]]
                }
        }
        ## Preparing data to be plotted ----
        # pos calculates the amount of packages per level
        pos <- numeric(0)
        for (i in 1:length(packages)){
                pos <- c(pos,length(packages[[i]]))
        }
        names <- unlist(packages)
        pos <- pos[!pos==0]
        M <- matrix(nrow = length(names),
                    ncol = length(names),
                    byrow = TRUE, data = 0)
        # substitutes 0 by "" in the preallocated matrix of links when there's a link
        for (j in 1:length(names)){
                origin <- which(names==names[j])
                end_packages <- connections[connections$source==names[j],]$target
                idx <- match(end_packages,names)
                for (k in idx){
                        M[j,idx] <- ""
                }
        }
        
        ## Plotting data ----
        plotmat(M, 
                main= paste("Diagram of", name),
                pos = pos, 
                name = names,
                relsize=1,
                lcol="black",
                lwd = 1,
                box.lwd = 2, 
                box.cex = 0.8,
                box.size = 0.06,
                box.type = "circle", 
                box.prop = 0.5)
}       
