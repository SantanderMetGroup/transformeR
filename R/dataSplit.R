##     dataSplit.R Split data into two different sets
##
##     Copyright (C) 2017 Santander Meteorology Group (http://www.meteo.unican.es)
##
##     This program is free software: you can redistribute it and/or modify
##     it under the terms of the GNU General Public License as published by
##     the Free Software Foundation, either version 3 of the License, or
##     (at your option) any later version.
## 
##     This program is distributed in the hope that it will be useful,
##     but WITHOUT ANY WARRANTY; without even the implied warranty of
##     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##     GNU General Public License for more details.
## 
##     You should have received a copy of the GNU General Public License
##     along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @title Split data into two different sets.
#' @description Split data into two different sets by a specific fraction. Splitting data is 
#' normally used to obtain a train and a validation set.
#' @param x The input grid object.
#' @param y The observations object.
#' @param f Could be a fraction, value between (0,1) indicating the fraction of the data that will define the train set, 
#' or an integer indicating the number of folds. It can also be a list of folds indicating the years of each fold. 
#' @param type A string. Indicates if the splitting should be random (type = "random"),
#' chronological (type = "chronological") or specified by the user (type = NULL). Default is "random". 
#' Default is "random".
#' @return A list of folds containing the x and y splitted.
#' @author J. Bano-Medina
#' @export
#' @examples \donttest{
#' require(climate4R.datasets)
#' x <- makeMultiGrid(NCEP_Iberia_hus850, NCEP_Iberia_psl, NCEP_Iberia_ta850)
#' y <- VALUE_Iberia_pr
#' ### Split the data in train and test (f < 1)###
#' data.splitted <- dataSplit(x,y,f = 3/4, type = "chronological")
#' str(data.splitted[[1]]$train$y$Dates) # 2 folds out of 3 for train  
#' str(data.splitted[[1]]$test$y$Dates)  # 1 fold out of 3 for test
#' ### Split the data in 3 folds ###
#' data.splitted <- dataSplit(x,y,f = 3, type = "chronological")
#' str(data.splitted[[1]]$train$y$Dates) # 2 folds out of 3 for train  
#' str(data.splitted[[1]]$test$y$Dates)  # 1 fold out of 3 for test
#' str(data.splitted[[2]]$train$y$Dates) # 2 folds out of 3 for train  
#' str(data.splitted[[2]]$test$y$Dates)  # 1 fold out of 3 for test
#' str(data.splitted[[3]]$train$y$Dates) # 2 folds out of 3 for train  
#' str(data.splitted[[3]]$test$y$Dates)  # 1 fold out of 3 for test
#' data.splitted <- dataSplit(x,y,f = 3, type = "random")
#' str(data.splitted[[1]]$train$y$Dates) # 2 folds out of 3 for train  
#' str(data.splitted[[1]]$test$y$Dates)  # 1 fold out of 3 for test
#' str(data.splitted[[2]]$train$y$Dates) # 2 folds out of 3 for train  
#' str(data.splitted[[2]]$test$y$Dates)  # 1 fold out of 3 for test
#' str(data.splitted[[3]]$train$y$Dates) # 2 folds out of 3 for train  
#' str(data.splitted[[3]]$test$y$Dates)  # 1 fold out of 3 for test
### Split the data in 3 folds indicating the years of each fold ###
#' data.splitted <- dataSplit(x,y,type = "chronological", 
#'                            f = list(c("1983","1984","1985","1986","1987",
#'                                       "1988","1989","1990","1991"),
#'                                     c("1992","1993","1994","1995","1996",
#'                                       "1997","1998","1999"),
#'                                     c("2000","2001","2002")))
#' str(data.splitted[[1]]$train$y$Dates) # 2 folds out of 3 for train  
#' str(data.splitted[[1]]$test$y$Dates)  # 1 fold out of 3 for test
#' str(data.splitted[[2]]$train$y$Dates) # 2 folds out of 3 for train  
#' str(data.splitted[[2]]$test$y$Dates)  # 1 fold out of 3 for test
#' str(data.splitted[[3]]$train$y$Dates) # 2 folds out of 3 for train  
#' str(data.splitted[[3]]$test$y$Dates)  # 1 fold out of 3 for test 
#' }

dataSplit <- function(x, y, f = 3/4, type = "random") {
    if (is.numeric(f)) {
        if (f < 1) {
            out <- vector("list", 2)
            if (type == "random") {
                indT <- sample(1:getShape(y, dimension = "time"),
                               size = floor(f * getShape(y,dimension = "time")),
                               replace = FALSE) %>% sort()
                indt <- setdiff(1:getShape(y,dimension = "time"), indT)}
            else if (type == "chronological") {
                indT <- 1:floor(f*getShape(y,dimension = "time"))
                indt <- setdiff(1:getShape(y,dimension = "time"), indT)}
            train <- list("x" = subsetDimension(x,dimension = "time", indices = indT),
                          "y" = subsetDimension(y,dimension = "time", indices = indT))
            test  <- list("x" = subsetDimension(x,dimension = "time", indices = indt),
                          "y" = subsetDimension(y,dimension = "time", indices = indt))
            out <- list(list("train" = train, "test" = test))
        }
        else if (f >= 1) {
            size_fold <- floor(getShape(y,dimension = "time") / f)
            if (type == "random") {
                inds <- array(data = sample(1:(size_fold*f), size = length(1:(size_fold*f)), replace = FALSE),
                              dim = c(size_fold, f))
            } else if (type == "chronological") {
                inds <- array(data = 1:(size_fold*f), dim = c(size_fold, f))}
            inds_fold <- lapply(1:f, function(z) inds[,z])
            if (length(1:getShape(y,dimension = "time")) != length(1:(size_fold * f))) {
                ind_out <- setdiff(1:getShape(y,dimension = "time"), as.vector(inds))
                inds_fold[[f]] <- c(inds_fold[[f]], ind_out)
            }
            out <- lapply(1:f, function(z) {
                indT <- setdiff(1:f, z)
                range <- c()
                for (i in indT) {
                    range <- c(range, inds_fold[[i]])
                }
                train <- list("x" = subsetDimension(x,dimension = "time", indices = range),
                              "y" = subsetDimension(y,dimension = "time", indices = range))
                test  <- list("x" = subsetDimension(x,dimension = "time", indices = inds_fold[[z]]),
                              "y" = subsetDimension(y,dimension = "time", indices = inds_fold[[z]]))
                list("train" = train, "test" = test)
            })
        }
    } else {
        out <- lapply(1:length(f), function(z) {
            indT <- setdiff(1:length(f), z)
            range <- c()
            for (i in indT) range <- c(range, f[[i]])
            train <- list("x" = subsetGrid(x,years = range, drop = FALSE),
                          "y" = subsetGrid(y,years = range, drop = FALSE))
            test  <- list("x" = subsetGrid(x,years = f[[z]], drop = FALSE),
                          "y" = subsetGrid(y,years = f[[z]], drop = FALSE))
            return(list("train" = train, "test" = test))
        })
    }
    names(out) <- paste0("f", 1:length(f))
    return(out)
}

