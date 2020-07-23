# Extracting results

# 1) set folder with results
results.folder<-"C:/Users/Ed/Dropbox/OHDSI/covid/charybdis/StudyResults"
# zipped results folders from the sftp
# note, at the moment the feedernet zip is not in my results folder
# this feedernet one would need to split up 
# (as it currently has two different results sets within it)

# Note, the combined reults set will be saved as an RDATA set in the results folder
# If you have previously run, and have this object in the folder but want to overwrite it,
# then recommend deleting it manually before running the below 

# 2) run
# first make sure the packages below are installed
# then run the below code (you'll have time to get a coffee while you wait)
# once run, you should now have the RDATA object with everything in your results folder

# Note, if you set the merge to be false below, you can just load the file 
# if previously created

# packages-----
library(CohortDiagnostics)
library(dplyr)
library(stringr)

# functions ------
# taken from Shiny app
preMergeResultsFiles <- function(dataFolder) {
  zipFiles <- list.files(dataFolder, pattern = ".zip", full.names = TRUE)
  
  loadFile <- function(file, folder, overwrite) {
    # print(file)
    tableName <- gsub(".csv$", "", file)
    camelCaseName <- SqlRender::snakeCaseToCamelCase(tableName)
    data <- readr::read_csv(file.path(folder, file), col_types = readr::cols(), guess_max = 1e7, locale = readr::locale(encoding = "UTF-8"))
    colnames(data) <- SqlRender::snakeCaseToCamelCase(colnames(data))
    
    if (!overwrite && exists(camelCaseName, envir = .GlobalEnv)) {
      existingData <- get(camelCaseName, envir = .GlobalEnv)
      if (nrow(existingData) > 0) {
        if (nrow(data) > 0) {
          if (all(colnames(existingData) %in% colnames(data)) &&
              all(colnames(data) %in% colnames(existingData))) {
            data <- data[, colnames(existingData)]
          } else {
            stop("Table columns do no match previously seen columns. Columns in ", 
                 file, 
                 ":\n", 
                 paste(colnames(data), collapse = ", "), 
                 "\nPrevious columns:\n",
                 paste(colnames(existingData), collapse = ", "))
            
          }
        }
      }
      data <- rbind(existingData, data)
    }
    assign(camelCaseName, data, envir = .GlobalEnv)
    
    invisible(NULL)
  }
  tableNames <- c()
  for (i in 1:length(zipFiles)) {
    writeLines(paste("Processing", zipFiles[i]))
    tempFolder <- tempfile()
    dir.create(tempFolder)
    unzip(zipFiles[i], exdir = tempFolder)
    
    csvFiles <- list.files(tempFolder, pattern = ".csv")
    tableNames <- c(tableNames, csvFiles)
    lapply(csvFiles, loadFile, folder = tempFolder, overwrite = (i == 1))
    
    unlink(tempFolder, recursive = TRUE)
  }
  
  # Update the covariate names for the age groups 
  # that are > 100 years since they are truncated to 2
  # digits
  ageCovariateIdsToReformat <- seq(200031,380031, by=10000) # This represents the covariate IDs that are used to represent age groups from 100-200
  if (exists("covariate", envir = .GlobalEnv)) {
    covars <- get("covariate", envir = .GlobalEnv)
    
    #Ensure all covariates are unique by making all covarateName fields lower case
    covars$covariateName <- tolower(covars$covariateName)
    covars <- unique(covars)
    
    # Reformat age covariates
    ageCovars <- covars[covars$covariateId %in% ageCovariateIdsToReformat, ]
    if (nrow(ageCovars) > 0) {
      for (i in 1:nrow(ageCovars)) {
        covars[covars$covariateId == ageCovars$covariateId[i], ]$covariateName <- reformatAgeCovariateDescription(ageCovars$covariateName[i])
      }
    }
    
    # Re-assign to the global environment
    assign("covariate", covars, envir = .GlobalEnv)
  }

  tableNames <- unique(tableNames)
  tableNames <- gsub(".csv$", "", tableNames)
  tableNames <- SqlRender::snakeCaseToCamelCase(tableNames)
  save(list = tableNames, file = file.path(dataFolder, "PreMerged.RData"), compress = TRUE)
  ParallelLogger::logInfo("Merged data saved in ", file.path(dataFolder, "PreMerged.RData"))
}
reformatAgeCovariateDescription <- function(description) {
  splitDesc <- strsplit(description, " ") # Split to get the age range
  ageRange <- strsplit(splitDesc[[1]][3], "-")
  lowerBound <- as.integer(ageRange[[1]][1]) + 100
  upperBound <- as.integer(ageRange[[1]][2]) + 100
  return(paste0("age group: ", lowerBound, "-", upperBound))
}

get.results.set<-
  function(results.folder, 
           merge.results){
if(merge.results==TRUE){
preMergeResultsFiles(results.folder)
  }else {
load(paste0(results.folder, "/PreMerged.RData"), envir = globalenv())
  }
  }
# get results -----
get.results.set(results.folder=results.folder, 
           merge.results=TRUE)


