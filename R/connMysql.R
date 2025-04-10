##' @title Connection to PPSP MySQL database
##'
##' @description
##' Interactive function to connect and query data from remote  duckewiki database
##' @details
##' SSH protocol to stablish connection and query database structured. This function should work on any duckewiki database.
##' 
##' @param database character string. Name of data base.
##' @param user character string.
##' @param census integer vector. Census years. 
##' @return 'connMysql' returns a dataframe with data.
##' @author Alexandre Adalardo de Oliveira \email{ecovirtualpackage@@gmail.com}
##' @seealso \url{{http://labtrop.ib.com.br}; {http://github/adalardo/duckewiki}}
##' @keywords mysql, remote connection, database access
##' @examples
##' 
##' \dontrun{
##' connMysql(database = "ppsp",  census = NULL)
##' }
##' 
##' @export 
connMysql = function(dbname = "ppsp")
{
    #library(RMySQL) should be import by the package NAMESPACE
    ## supressing annoying warnings messages from mysql  
    ## oldw <- getOption("warn")
    ## options(warn = -1)
    ## ##################
    ## host <- as.character(readline("Give the domain name or IP to access remote database: ")) 
    ## user <- readline(paste("MySQL user name for database",dbname,": "))
    ## pwd <- readline(paste("Password for", user, ": "))
    ## sqlcenso <- "SELECT CensoID as censoid, CensoNome as plotdata FROM Censos"
    ## con <- dbConnect(RMySQL::MySQL(), host = host, user = user, password = pwd, dbname=dbname)
    ## cen = dbGetQuery(con, sqlcenso)
    ## ncen<-dim(cen)[1]
    ## strcen <- paste(1:ncen," - ", cen$plotdata, "\n ", collapse="")
    ## sn = "n"
    ## while(sn != "y")
    ## {
    ##     cat("Plot and censos in database:  \n", strcen)
    ##     censos <- readline(paste( "Select plot and censo by the integer listed, separated by\n semicolon (p.ex: 1; 4; 7): "))
    ##     censos<- gsub(" ", "", censos)
    ##     pos <- unlist(strsplit(censos, split = ";"))
    ##     if(sum (pos%in%(1:ncen)) == length(pos))
    ##     {
    ##     censoNames <- cen[pos,2]
    ##     censoID <- cen[pos, 1]
    ##     sn <- readline (paste("Selected: \n", paste(censoNames, "\n", collapse=" "), "\n confirm (y/n): "))
    ##     sn <- tolower(gsub(" ", "", sn))
    ##     }
    ##     if(sn !="y")
    ##     {
    ##         readline("Selected number is not a valid census. Press RETURN to try again!")
    ##     }
    ## }
    ## sqldapid <-  "SELECT TraitID FROM Traits WHERE TraitName IN ('dap', 'DAP', 'Dap')"
    ## dapid = as.numeric(dbGetQuery(con, sqldapid))
    ## censoid = paste("(", paste( censoID,  collapse = ", ", sep=""), ")")

    ## sqldata = paste("SELECT  pltb.PlantaTag as tag,  getidentidade(pltb.DetID,1,0,1,0,0) AS fam,  getidentidade(pltb.DetID,1,0,0,1,0) AS gen,  getidentidade(pltb.DetID,1,0,0,0,1) AS sp, parcelafiels(pltb.GazetteerID, pltb.GPSPointID, 'PARGAZ_SPEC')  as plot, parcelafiels(pltb.GazetteerID, pltb.GPSPointID, 'PARDIMX')  as dimx, parcelafiels(pltb.GazetteerID, pltb.GPSPointID, 'PARDIMY')  as dimy, localidadefields(pltb.GazetteerID, pltb.GPSPointID,0,0,0, 'GAZETTEER_SPEC')  as quad, parcelafiels(pltb.GazetteerID, pltb.GPSPointID, 'DIMX')  as qdimx, parcelafiels(pltb.GazetteerID, pltb.GPSPointID, 'DIMY')  as qdimy, parcelafiels(pltb.GazetteerID, pltb.GPSPointID, 'STARTX')  as startx, parcelafiels(pltb.GazetteerID, pltb.GPSPointID, 'STARTY')  as starty, pltb.X as px, pltb.Y as py, moni.TraitUnit as dbh_unit, moni.TraitVariation as dbh, moni.DataObs as day FROM Monitoramento as moni JOIN Plantas AS pltb ON moni.PlantaID=pltb.PlantaID WHERE moni.TraitID=", dapid , " AND moni.CensoID IN ", censoid, sep="", collapse="")     
    ## datacensos = dbGetQuery(con, sqldata)
    ## options(warn=0)
    ## return(datacensos)
}
