#############################################
### Alexandre Adalardo 12 de setembro de 2022
#############################################
# OpenDataBio Permanent Plot Data
#############################################
##' @title Organize ODBio datasets
##' @description Organize data object from exported opendatabio datasets.   
##' @description  OpenDataBio datasets are exported as zip file containing several tabular data compressed files, with basic information about individuos (Organisms.csv), identification (Taxons.csv) and mapping (Locations.csv). If there is any measurements associated with the individual there is a file (MeasurementsOrFacts.csv)  containing those measurements. This function organize those mesaruments in an format that is used by CTFS/FoestGEO network.
##' @param dirPath directory address with the uncompressed files exported from openDataBio datasets.
##' @return 'odbData' returns a data frame organized as CTFS\\/ForestGEO format.and a  
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso \code{\link{data.frame}} 
##' \url{http://labtrop.ib.usp.br}
##' @examples
##' \dontrun{
##' dir.create(".Rtmp")
##' species <- paste("species", 1:10, sep = "_")
##' family <-  paste("family", 1:10, sep = "_")
##' spOrder <- sample(1:10, size = 100, replace = TRUE)
##' x <- sample(seq(0.1, 19.9, by =0.1), size = 100, replace = TRUE)
##' y <- sample(seq(0.1, 19.9, by =0.1), size = 100, replace = TRUE)
##' quads <- subplotXY(xcode = LETTERS[1:3], ycode = paste("0", 0:2, sep = "")) 
##' locationName <- sample(quads$subplot, size =100, replace= TRUE)
##' ordLoc <- match(locationName, quads$subplot)
##' gx <- x + quads$xlim[ordLoc] 
##' gy <- y + quads$ylim[ordLoc]
##' recordNumber = sample(1:10000, size = 100)
##' 
##' write.table(x = data.frame(id = 1:100,
##'                            recordNumber,  
##'                            organismID = paste(recordNumber,"-USER-", locationName, sep = ""),
##'                            scientificName = species[spOrder],
##'                            family = family[spOrder],
##'                            locationName = locationName,
##'                            locationParentName = "paradisePlot",
##'                            decimalLatitude =NA,
##'                            decimalLongitude = NA,
##'                            x = x,
##'                            y = y,
##'                            gx = gx,
##'                            gy = gy,
##'                            datasetName = "imaginationPlot"),
##'             file =".Rtmp/.testOrganisms.csv", sep = ",", row.names = FALSE)
##' dap <- exp(rnorm(100, mean = 3.68, sd = 0.91))
##' dap[dap > 10100]<- 64
##' dap[dap < 10]<- 10
##' dap <- round(dap)
##' alt <- exp(-0.73 + 0.63* log(dap) + rnorm(100, 0, 0.35))
##' alt <- round(alt, 1)
##'
##' write.table(data.frame(id = 1:200,
##'                        measured_id = c(1:100, 1:100),
##'                        measurementType = rep(c("dbh", "height"), each = 100),
##'                        measurementValue = c(dap, alt),
##'                        measurementDeterminedDate = "2019-01-10",
##'                        resourceRelationshipID = paste(recordNumber,"-USER-", locationName, sep = ""),
##'                        datasetName = "imaginationPlot" ),
##'             ".Rtmp/.testMeasurementsOrFacts.csv", sep = ",", row.names = FALSE)
##' dataForestGEO <- odb_organize_dataset(path = ".Rtmp")
##' str(dataForestGeo)
##' unlink(""
##' }
##' @export
odb_organize_dataset <- function(path = getwd())
{
    fnamesPath <- list.files(path, all.files = TRUE)
    orgFileName <-  fnamesPath[grep("Organisms.csv", fnamesPath)]
    mesFileName <- fnamesPath[grep("MeasurementsOrFacts.csv", fnamesPath)]
    if(length(orgFileName)==0)
    {
        stop("There is no odbio Organisms.csv file in the workdirectory, please make sure the workdirectory is right")
    }
    orgData0 <- read.table(file.path(path, orgFileName), sep = ",", header = TRUE)
    namesVarOrg <- c('id', 'recordNumber', 'scientificName', 'family', 'locationName', 'locationParentName', 'decimalLatitude', 'decimalLongitude', 'x', 'y', 'gx', 'gy', 'datasetName')
    data <- orgData0[,namesVarOrg]
    names(data) <- c('odbIndId', 'tag', 'species', 'family', 'quadrat', 'plotName', 'Latitude', 'Longitude', 'x', 'y', 'gx', 'gy', 'datasetName') 
    if(length(mesFileName) ==1)
    {
        mesData0 <- read.table(file.path(path, mesFileName), sep = ",", header = TRUE)
        mesTypes <- unique(mesData0$measurementType)
        mesVarNames <- c("measured_id", "measurementValue", "measurementDeterminedDate")
#        data$date <- NA
        for(i in mesTypes)
        {
            mesTypeData <- mesData0[mesData0$measurementType == i, mesVarNames]
            iMatch <- match(mesTypeData$measured_id, data$odbIndId)
            data[,i] <- NA
            data[iMatch,i] <- mesTypeData$measurementValue
            data[iMatch, paste("date_", i, sep = "")] <- mesTypeData$measurementDeterminedDate
        }
        data$datasetName <- unique(mesData0$datasetName)[1]
    }
    return(data)
}


