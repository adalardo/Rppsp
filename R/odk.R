############################################
### Alexandre Adalardo 15 de outubro de 2018
#############################################
# export data from odk collect using
# odkbriefcase java package
##############################
##############################
##' Export data from okd collect to a csv files 
##'
##' @param odkbriefcase  ODK briefcase java jar file.
##' @param dir_st  ODK storage directory address.
##' @param dir_col ODK collect directory.
##' @param form_id ODK form id, defined in the settings of xls and xml forms
##' @param dir_exp Data export directory 
##' @param file_name prefix name for csv files. 
##' @return 'odkexp' returns csv files export from odk collect. 
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso 
##' \url{http://labtrop.ib.usp.br}
##' @references \url{https://opendatakit.org/} 
##' @keywords
##' @examples
##' 
##' \dontrun{
##' odkexp(odkbriefcase = "odkbriefcase.jar", dir_st = getwd(), dir_col = getwd(), dir_exp = getwd(), form_id = "odkform", file_name = "odkdata")
##' }
##' 
##' @export
##'
##'
##'
odkexp <- function(odkbriefcase = "odkbriefcase.jar", dir_st = getwd(), dir_col = getwd(), dir_exp = getwd(), form_id = NULL, file_name = "censoPeic", start = NULL, end = NULL )
{
    ## exportando o formulario do odk collect
    odkstdir <- file.path(dir_st, "ODK Briefcase Storage")
    if(dir.exists(odkstdir))
    {
        unlink(odkstdir, recursive = TRUE)
    }
    briefexp <- paste("java -jar", odkbriefcase , " --pull_collect --storage_directory ", dir_st, " --odk_directory", dir_col)
    
    system(briefexp)
    ## criando os arquivos csv  de dados
    ## testing if the files already exist
    if(length(grep(file_name, list.files(dir_exp))) >0)
    {
        yn <-  readline("File name already exists in the export directory, overwrite it (y/n): ")
        if(yn=="n")
        {
            stop("Try again with a new export file name")
        }
    }
if(is.null(start) | is.null(end))
{
    briefcsv <- paste("java -jar",  odkbriefcase , " --export --form_id ", form_id, "--storage_directory ", dir_st, " --export_directory ", dir_exp, " --export_filename ", file_name)
} else{
    briefcsv <- paste("java -jar",  odkbriefcase , " --export --form_id ", form_id, "--storage_directory ", dir_st, " --export_directory ", dir_exp, " --export_filename ", file_name, "--export_start_date", start , "--export_end_date", end)
}
    system(briefcsv)
    unlink(odkstdir, recursive = TRUE)
}

##############################
##' Read odk exported csv files to a single data frame
##'
##' @param base_name Files prefix name and directory direction without extension
##' @param form_rep Form repeat sections names in order
##' @param dir_exp Data export directory 
##' @param data_type Long or short version of the data
##' @return 'odkexp' returns csv files export from odk collect. 
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso 
##' \url{http://labtrop.ib.usp.br}
##' @references \url{https://opendatakit.org/} 
##' @keywords
##' @examples
##' 
##' \dontrun{
##' odkexp(odkbriefcase = "odkbriefcase.jar", dir_st = getwd(), dir_col = getwd(), dir_exp = getwd(), form_id = "odkform", file_name = "odkdata")
##' }
##' 
##' @export
##'
##'
##'
read.csvODK <- function(base_name = "censoPeic",  form_rep = c("subquad", "tree", "sec_fuste", "rep_miss"), save_file = TRUE, dir_exp = getwd(), file_name = NULL)
{
    ## base dir
    base_dir <- dirname(base_name)
    ## base form file
    names.csv <- paste(base_name, c("",rep("-",4)),c("", form_rep), ".csv", sep="")
    for(j in 0:length(form_rep))
    {
        assign(paste("form", j, sep=""), read.table(names.csv[j+1],header=TRUE, as.is=TRUE, sep=","))
    }
   form0names <- c('today', 'start', 'end',  'tree_gps.Latitude', 'tree_gps.Longitude', 'tree_gps.Altitude', 'tree_gps.Accuracy', 'equipe.equipe_nomes', 'equipe.equipe_nomes_other', 'equipe.lider', 'parcela.quadrat','KEY')
    form1names <- c('quad5x5', 'sel_subquad', 'tag_selected', 'tree_miss.tag_miss', 'tree_miss.n_miss', 'PARENT_KEY', 'KEY')
    form01 <- merge(form0[,form0names], form1[form1names], by.x="KEY", by.y="PARENT_KEY")
    form2names <- c('n_tree', 'tree_type',  'tag_ok', 'tree_recenso.arvmap_key', 'confplaq_map.ileg_conf', 'tree_recenso.tree_tag_map', 'tree_recenso.tree_tag', 'tree_recenso.old_dap',  'tree_recenso.old_alt', 'tree_recenso.old_dx', 'tree_recenso.old_dy',  'tree_recenso.old_fam', 'tree_recenso.old_sp', 'tree_recenso.old_nfuste', 'tree_recenso.arvmap_conf', 'newmap_codpos',   'new_data.new_tag', 'new_data.new_tag_picture', 'new_data.num_tag',  'new_data.alt_max0', 'new_data.alt_max1', 'new_data.tipo_med',  'new_dap.dap2018_mm', 'new_dap.dap2018_cm', 'new_dap.pap2018_cm', 'new_dap.one_fuste',  'info_id', 'new_id.new_fam', 'new_id.new_gen', 'new_id.new_sp', 'conf_info.map2018', 'conf_info.dap2018', 'conf_info.alt2018', 'conf_info.fam2018', 'conf_info.sp2018', 'conf_info.conf_ok', 'PARENT_KEY', 'KEY') 
    tree <- form2[, form2names]
    treenames <- gsub("tree_recenso.", "", form2names)
    treenames <- gsub("tree_recenso.", "", treenames)
    treenames <- gsub("new_data.", "", treenames)
    treenames <- gsub("new_dap.", "", treenames)
    treenames <- gsub("new_id.", "", treenames)
    treenames <- gsub("conf_info.", "", treenames)
    names(tree) <- treenames
    names(tree)[names(tree) =='KEY'] <- "key_tree"
    names(tree)[names(tree) =='PARENT_KEY'] <- "key_quad"
    names(tree)[names(tree) =='confplaq_map.ileg_conf'] <- "confTagMap"
    treequad <- merge(form01[, c("today", "parcela.quadrat", "quad5x5","KEY.y")], tree, by.x="KEY.y", by.y="key_quad")
    names(treequad)[names(treequad) =='KEY.y'] <- "key_quad"
    names(treequad)[names(treequad) =='parcela.quadrat'] <- "quadrat"
    treequad$quad5x5 <- gsub("quad_", "", treequad$quad5x5)
    startxy <- strsplit( treequad$quad5x5, "x")
    stx <- as.numeric(sapply(startxy, function(x){x[1]}))
    sty <- as.numeric(sapply(startxy, function(x){x[2]}))
    newMap <- treequad$arvmap_conf != "map_yes"
    iMap <- which(newMap)
    treequad$new_dx2018 <- treequad$new_dy2018 <- NA 
for(i in iMap)
    {
        codMap <- treequad$newmap_codpos[i]
        codMap <- gsub("quad_", "", codMap)
        xy <- strsplit(codMap, "x")[[1]]
        treequad$new_dx2018[i] <- as.numeric(xy[1]) + stx[i]
        treequad$new_dy2018[i] <- as.numeric(xy[2]) + sty[i]
    }
    treequad$nfuste2018 <- 1
    treequad$dap2018_sec_mm <- NA
    form3$dap2mm <- form3$dap_mm_sec
    form3$dap2mm[!is.na(form3$dap_cm_sec)] <- form3$dap_cm_sec[!is.na(form3$dap_cm_sec)] *10
    oneKey <- unique(form3$PARENT_KEY)
    for(i in 1:length(oneKey))
    {
        k <- oneKey[i]
        secF <- form3[form3$PARENT_KEY == k,]
        nf <- nrow(secF) + 1
        dap01 <- treequad$dap2018[treequad$key_tree == k]
        mdap <- sort(as.numeric(c(dap01,secF$dap2mm)), decreasing=TRUE)
        dapsec <- paste(mdap[-1], collapse = ";")
        treequad[treequad$key_tree == k, c("dap2018","nfuste2018", "dap2018_sec_mm")] <- c(as.numeric(mdap[1]), nf, dapsec) 
     }
    nres <- c("today","tree_type", "tag_ok","tree_tag", "new_tag",  "num_tag",  "quadrat", "quad5x5","old_dx", "old_dy","new_dx2018", "new_dy2018", "old_alt", "alt2018","old_nfuste", "nfuste2018", "old_dap", "dap2018","dap2018_sec_mm", "old_fam", "old_sp", "fam2018", "sp2018")
    treeres <- treequad[, nres]
###### miss trees
    namiss <- c('miss_num', 'miss_dap', 'miss_dbhcm', 'miss_alt', 'miss_dx', 'miss_dy',  'miss_fam',  'miss_sp', 'miss_group.miss_type', 'miss_group.map_miss', 'PARENT_KEY', 'KEY')
namiss <- c('miss_num', 'miss_dap', 'miss_dbhcm', 'miss_alt', 'miss_dx', 'miss_dy',  'miss_fam',  'miss_sp', 'miss_group.miss_type', 'miss_group.map_miss', 'PARENT_KEY', 'KEY')
    treemiss <- form4[, namiss]
    names(treemiss)[names(treemiss) =='KEY'] <- "key_treemiss"
    names(treemiss)[names(treemiss) =='PARENT_KEY'] <- "key_subquad"
    names(treemiss)[names(treemiss) =='KEY'] <- "key_treemiss"
    names(treemiss)[names(treemiss) =='miss_group.miss_type'] <- "miss_type"
    names(treemiss)[names(treemiss) =='miss_group.map_miss'] <- "map_miss"
    treemiss <- merge(treemiss, form01[, c("parcela.quadrat", "quad5x5","KEY.y")], by.x="key_subquad", by.y="KEY.y")
    names(treemiss)[names(treemiss) =='parcela.quadrat'] <- "quadrat"
    treemiss$quad5x5 <- gsub("quad_", "", treemiss$quad5x5)
    treemiss$key_subquad <- paste("sub", sapply(strsplit( treemiss$key_subquad, "/sub"), function(x){x[[2]]}), sep="")
    treemiss$key_treemiss <- paste("sub", sapply(strsplit( treemiss$key_treemiss, "/sub"), function(x){x[[2]]}), sep="")
### Incluir uma saida de objeto em lista com o treemiss e tree
    if(save_file)
    {
        qname <- paste(unique(treeres$quadrat), collapse="_")
        dname <- file.path(dir_exp, qname)
        if(!dir.exists(dname))
        {
            #dir.create(dname)
            dir.create(file.path(dname, "baseData"), recursive=TRUE)
        }
        file.rename(from = names.csv, to=  file.path(dname, "baseData", paste("censoPeic",c("",form_rep), qname,".csv", sep="")))
        if(dir.exists(file.path(base_dir, "media")))
            {
                media.names <- list.files(file.path(base_dir, "media"))
                dir.create(file.path(dname, "media"), recursive=TRUE)
                file.rename(from = file.path(base_dir, "media", media.names), to = file.path(dname, "media", media.names))
            }
        write.table(treequad, file.path(dname, paste("tree",qname,".txt", sep="")), sep="\t", row.names=FALSE)
        write.table(treeres, file.path(dname, paste("treeResumo",qname,".txt", sep="")), sep="\t", row.names=FALSE)
        write.table(treemiss, file.path(dname, paste("treeMiss",qname,".txt", sep="")), sep="\t", row.names=FALSE)
    }
### INCLUIR UMA SAIDA DE LEITURA DE DADOS EM UM DIRETORIO ESPECIFICO
invisible(treeres)
}

    
#############################
##' Check census data with previous census 
##'
##' @param base_name Files prefix name and directory direction without extension
##' @param form_rep Form repeat sections names in order
##' @param dir_exp Data export directory 
##' @param data_type Long or short version of the data
##' @return 'odkexp' returns csv files export from odk collect. 
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso 
##' \url{http://labtrop.ib.usp.br}
##' @references \url{https://opendatakit.org/} 
##' @keywords
##' @examples
##' 
##' \dontrun{
##' censoAudit(censofile = "treeA01.txt", olddata = "/home/aao/Ale2016/AleProjetos/PPPeic/censo2018/openDataKit/peic09.csv")
##' }
##' 
##' @export
##'
##'
##'
censoAudit <- function(dir.exp = getwd(), quadnames = "A00_A01", olddata = "peic09.csv")
{
    abasal <- function(x){sum(((x)/2)^2 * pi)}
    filetree <- file.path(dir.exp, quadnames, paste("tree", quadnames, ".txt", sep=""))
    if(!file.exists(filetree) | !file.exists(olddata) )
    {
        stop("actual and previous census data files should exist")
    }
    auddir <- file.path(dir.exp, quadnames, "auditData")
    if(!dir.exists(auddir))
    {
        dir.create(auddir, recursive=TRUE)
    }
    tfile <- strsplit( olddata, "\\.")[[1]]
    tfile <- tfile[length(tfile)]
    sc <- ifelse(tfile=="csv",  ",", "\t")
###########################
### ACTUAL CENSUS DATA
###########################  
    tree <- read.table(filetree, as.is=TRUE, header=TRUE, sep="\t")
    tree$key_tree <- gsub(".*/sub", "sub", tree$key_tree)
    tree <- tree[,-c(grep("key_quad", names(tree)))]
    tmiss <- read.table(file.path(dir.exp, quadnames, paste("treeMiss", quadnames, ".txt", sep="") ), as.is=TRUE, header=TRUE, sep="\t")
    
############################
## PREVIOUS CENSUS DATA
###########################
    censo09 <- read.table(olddata, header=TRUE, as.is=TRUE, sep=sc)
    dquad09 <- censo09[censo09$quad %in% unique(tree$quadrat), ] # so os quadrats com dados 2018
    dquad09A <- dquad09[dquad09$status=="A",]
    ntquad09 <- table(dquad09A[dquad09$status=="A","quad"])
    #mquad09 <- table(dquad09[dquad09$status=="D","quad"])
    ab09 <- tapply(dquad09A$dbhcm, dquad09A$quad, abasal)

#####################################
### Dados nao confirmados em campo
#####################################
    confno <- sum(is.na(tree$conf_ok))
    tagconfno <- tree$num_tag[is.na(tree$conf_ok)]
    if(confno > 0)
    {
        fnotconf <- file.path(auddir, "datanotconfirmed.txt")
        if(file.exists(fnotconf))
        {
            confyes <- readline(paste("File", fnotconf, "already exist. Overwrite it (y/n): " ))
        } 
        if(file.exist(fnotconf) | confyes==yes)
        {
            naoconf <- tree[tree$num_tag %in% tagconfno, ]
            write.table(naoconf, , sep="\t", row.names=FALSE)
            message(paste("datanotconfirmed.txt file saved in dir:", auddir))
        }
     }
#####################################
### DADOS DE TAG REPETIDO (confirmar)
#####################################
    reptag <- duplicated(tree$num_tag)
    nreptag <- sum(reptag)
    tagrep <- NULL
    if(nreptag> 0)
    {
        tagrep <- tree[reptag,"num_tag"]
        dtagrep<- tree[tree$num_tag %in% tagrep, ]
        data.tagrepetido <- dtagrep[order(dtagrep$num_tag),]  ### saida para a tela
        write.table(data.tagrepetido, file.path(auddir, "tagrepeated.txt"), sep="\t", row.names=FALSE)
        message(paste("tagrepeated.txt file saved in dir:", auddir))

    }
###############################
### DESAPARECIDAS ENCONTRADAS
###############################
    tenc  <- tree$num_tag %in% tmiss$miss_num
    tagenc <- NULL
    if(sum(tenc)>0)
    {
        tagenc <- tree[tenc, "num_tag"]
        dmenc <- tmiss[tmiss$miss_num %in% tagenc,c("miss_num", "miss_type", "quadrat", "quad5x5")]
        dtenc <-  tree[tree$num_tag %in% tagenc,c("num_tag", "tree_type", "quadrat", "quad5x5", "old_dx", "old_dy", "map2018","new_dx2018", "new_dy2018")]
        denc <-cbind(dmenc,dtenc[match( dmenc$miss_num, dtenc$num_tag),])
        missNF <- tmiss[! tmiss$miss_num %in% denc$miss_num, ]
#source("/home/aao/Ale2016/AleProjetos/Rppsp/R/quadrat.R")
#newind <- subquad(denc$new_dx2018, denc$new_dy2018)
## abaixo os comandos da funcao 'subquad'
        pos <- seq(5,15, by= 5)
        dx.ind <- apply(sapply(pos, function(x){ifelse(denc$new_dx2018 >= x, 5,0)}), 1, sum)
        dy.ind <- apply(sapply(pos, function(x){ifelse(denc$new_dy2018 >= x, 5,0)}), 1, sum)
        dxy <- paste(dx.ind, "x",dy.ind, sep="")
        dxy[grep("NA",dxy) ] <- NA
#denc$indnew <- dxy
        namap <- is.na(denc$new_dx2018)
        names(denc)[7:8] <- c("quad2018", "subq2018")
        samequad <- denc$quadrat == denc$quad2018 & denc$quad5x5 == denc$subq2018
        denc$erroType <- NA
#denc$ver<- "no"
### Mesmo subq , nao remapeada
        denc$erroType[namap & samequad] <- "sq_nr"  ### incluir na saída
### Outro subquad, não mapeada
        denc$erroType[namap & !samequad] <- "oq_nr" ## incluir na saida
### Mesmo subq, remapeada
        denc$erroType[! namap & samequad] <- "sq_yr"## incluir na saída
### Outro subquad e remapeada
        denc$erroType[!namap & !samequad] <- "oq_yr" ## incluir na saida
        error.definitions <- data.frame(error= c("sq", "oq", "yr", "nr"), definition= c("same quadrat", "other quadrat", "remaped", "notremaped"),stringsAsFactors = FALSE )
       attributes(denc)$error.definitions <- error.definitions
################
        write.table(denc, file.path(auddir, "misstreefound.txt"), sep="\t", row.names=FALSE)
        message(paste("misstreefound.txt file saved in dir:", auddir))
        print(error.definitions)
################
    } else{missNF <- tmiss}
################################################
###  Estatistica quadrats
################################################
### estatistica por quadrado de 20x20
    quad <- 
    reptagquad <- tapply(reptag, tree$quadrat, sum)
    ntquad <- table(tree$quadrat)
    nsubq <- tapply(tree$quad5x5, tree$quadrat, function(x){length(unique(x))})
    dataquad <- tapply(tree$today, tree$quadrat, function(x){paste(unique(x), collapse = ";")})
    
################################
##### MORTALITY
################################
    mquad   <- table(missNF$quadrat)
    m10q <- table(missNF$quadrat[missNF$miss_dbhcm > 10])
    mrquad <- round((1- ((ntquad09 - mquad)/ntquad09)^(1/8)) *100, 2)
    mr10q <- round((1- ((ntquad09 - m10q)/ntquad09)^(1/8))*100,2)
################################
##### RECRUITMENT 
################################
#table(tree$tree_type)
    recquad <- tapply(tree$tree_type == "sem_info" & is.na(tree$tree_tag), tree$quadrat, sum)
    maxdaprec <-  tapply(tree$dap2018[tree$tree_type == "sem_info" & is.na(tree$tree_tag)], tree$quadrat[tree$tree_type == "sem_info" & is.na(tree$tree_tag)], max)
    meandaprec <-  tapply(tree$dap2018[tree$tree_type == "sem_info" & is.na(tree$tree_tag)], tree$quadrat[tree$tree_type == "sem_info" & is.na(tree$tree_tag)], mean)
    daprecmais5 <-  tapply(tree$dap2018[tree$tree_type == "sem_info" & is.na(tree$tree_tag)], tree$quadrat[tree$tree_type == "sem_info" & is.na(tree$tree_tag)], function(x){sum(x>50)})
    tagrec5 <- tree$num_tag[tree$tree_type == "sem_info" & is.na(tree$tree_tag) & tree$dap2018> 50]
       if(sum(daprecmais5)> 0)
    {
        listmais5 <- tree[tree$num_tag %in% tagrec5,]
        write.table(listmais5, file.path(auddir, "newmorethan5cmdbh.txt"), sep="\t", row.names=FALSE)
        message(paste("newmorethan5cmdbh.txt file saved in dir:", auddir))
    }
###############################
##### BASAL AREA
###############################
    ab18 <- tapply(tree$dap2018/10, tree$quadrat, abasal)
    abdiff <- round((ab18 - ab09)/ab09,3)
## relative growth rate in diameter for main stem
    dap09 <- as.numeric(sapply(strsplit(tree$old_dap, ";"), function(x){x[1]}))
    rgrdap <- round((tree$dap2018 - dap09)/dap09, 3)
## diminuiu mais de 5% ou mais de 10mm
#cdap[c(which(rgrdap < -0.10) , which((tree$dap2018 - dap09)< -10)),]
    rgbless <-  tree[c(which(rgrdap < -0.10) , which((tree$dap2018 - dap09)< -10)),]
    rgbless$erroType <- "negative growth"
## aumentou mais de 20% ou mais de 50mm 
#cdap[c(which(rgrdap > 0.5) , which((tree$dap2018 - dap09) > 100)),]
    rgbmore <-  tree[c(which(rgrdap > 0.5) , which((tree$dap2018 - dap09) > 100)),]
    rgbmore$erroType <- "rapid growth"
    rgbcheck <- rbind(rgbless, rgbmore)
   
    if(nrow(rgbcheck)> 0)
    {
        write.table(rgbcheck, file.path(auddir, "atipicalgrowthh.txt"), sep="\t", row.names=FALSE)
        message(paste("atipicalgrowth.txt file saved in dir:", auddir))
    }
### quadrat resume
   summaryQuad <- data.frame(quad = names(ntquad), data = dataquad, nsubq = nsubq, ntree18 = as.vector(ntquad) , ntree09 = as.vector(ntquad09), dead18 = as.vector(mquad), deadrate2018 = as.vector(mrquad), deadbig2018 = as.vector(m10q), drbigTree2018 = as.vector(mr10q), basalarea09= round(as.vector(ab09),1), basalarea18 =  round(as.vector(ab18),1), recr18 = as.vector(recquad))
     write.table(summaryQuad, file.path(auddir, "summaryQuadrat.txt"), sep="\t", row.names=FALSE)
    message(paste("summaryQuadrat.txt file saved in dir:", auddir))
#######################
#### Audit all trees
####################
    allaudit <- tree[tree$num_tag %in% unique(c(tagconfno, tagrep, tagenc , tagrec5,  rgbcheck$num_tag)), ]
    #c("today", "quad", "tree_type", "tree_tag", "num_tag", "old_dx","old_dy", "new_dx2018", "new_dy2018", "old_dap","dap2018", "alt2018","fam2018", "sp2018", "key_tree")
     
    errortype <- rep(NA, nrow(allaudit))
    errortype[allaudit$num_tag %in% tagrep] <- "tag number duplicated"
    for(i in tagrep)
    {
        allaudit$num_tag[allaudit$num_tag == i] <- paste(allaudit$num_tag[allaudit$num_tag == i], 1: sum(allaudit$num_tag ==i), sep=".0")
    }
    errortype[allaudit$num_tag %in% tagenc] <- paste(errortype[allaudit$num_tag %in% tagenc],"tag miss found", sep="; ")
    errortype[allaudit$num_tag %in% tagrec5] <- paste(errortype[allaudit$num_tag %in% tagrec5],"recruit > 5cm", sep="; ")
    errortype[allaudit$num_tag %in% rgbcheck$num_tag] <- paste(errortype[allaudit$num_tag %in% rgbcheck$num_tag],"atypical growth", sep="; ")
    errortype <- sub("NA; ", "", errortype)
    allaudit$errorType <- errortype
    write.table(allaudit, file.path(auddir, "treeaudit.csv"), sep=",", row.names=FALSE)
    message(paste("treeaudit.csv file saved in dir:", auddir))
}
