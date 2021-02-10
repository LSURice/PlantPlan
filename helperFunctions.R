library(httr)
library(jsonlite)
library(ggplot2)


####  BrAPI functions ####


get_token <- function(user, passwd, con){
  brapi_call <- paste0(con, "/brapi/v2/token?username=",
                       user, "&password=", passwd)
  res <- httr::GET(brapi_call)
  cont <- httr::content(x=res, as="text", encoding="UTF-8")
  token <- jsonlite::fromJSON(txt=cont)$access_token
  return(token)
}


get_brapi_json <- function(brapi_call, token){
  # Get a json object for brapi call
  res <- httr::GET(brapi_call,
                   config=add_headers("Authorization"=paste("Bearer", token))) 
  cont <- httr::content(x=res, as="text", encoding="UTF-8")
  contList <- jsonlite::fromJSON(txt=cont)
  return(contList)
}


get_pageNumber <- function(brapi_call, token){
  # Get number of pages for call
  brapi_json <- get_brapi_json(brapi_call, token)
  pages <- brapi_json[["metadata"]][["pagination"]]$totalPages - 1
  return(pages)
}


get_seasons_named_list <- function(con, token){
  # Retrieve available sesason (years)
  call <- paste0(con, "/brapi/v2/seasons?pageSize=1000")
  pages <- get_pageNumber(call, token)
  seasonData <- data.frame()
  for(i in 0:pages){
  	contList <- get_brapi_json(paste0(call,"&page=",i), token)
  	year <- contList[["result"]][["data"]][["year"]]
  	seasonDbId <- contList[["result"]][["data"]][["seasonDbId"]]
  	seasonData <- rbind(seasonData, data.frame(year=year,
  	                                           seasonDbId=seasonDbId,
  	                                           stringsAsFactors=F))
  }
 seasonData < seasonData[order(seasonData$year),]
 seasonDict <- seasonData$seasonDbId
 names(seasonDict) <- seasonData$year
 return(seasonDict)
}


get_study_named_list <- function(con, token, seasonDbId){
  # Retrieve a df with all studies using brapi
  call <- paste0(con,"/brapi/v2/studies?pageSize=1000&seasonDbId=",seasonDbId)  # Get page number
  pages <- get_pageNumber(call, token)
  trialData <- data.frame()
  for(i in 0:pages){  # Collect results from each page
    contList <- get_brapi_json(paste0(call,"&page=",i), token)
    studyName <- contList[["result"]][["data"]][["studyName"]]
    studyDbId <- contList[["result"]][["data"]][["studyDbId"]]
    trialData<- rbind(trialData, data.frame(studyName=studyName,
                                            studyDbId=studyDbId,
                                            stringsAsFactors=F))
  }
  trialData <- trialData[order(trialData$studyName),]
  studyDict <- trialData$studyDbId
  names(studyDict) <- trialData$studyName
  return(studyDict)
}


get_layout_info <- function(con, studyDbId, token){
  # Retrieves a df with all study layout data
  call <- paste0(con, "/brapi/v1/studies/", studyDbId,"/layout?pageSize=1000")
  pages <- get_pageNumber(call, token)
  layout <- data.frame()
  for(i in 0:pages){  # Collect results from each page
    contList <- get_brapi_json(paste0(call,"&page=",i), token)
    observationUnitName <- contList[["result"]][["data"]][["observationUnitName"]]
    accession <- contList[["result"]][["data"]][["germplasmName"]]
    plotNumber <- contList[["result"]][["data"]][["additionalInfo"]][["plotNumber"]]
    replicate <- contList[["result"]][["data"]][["replicate"]]
    blockNumber <- contList[["result"]][["data"]][["blockNumber"]]
    X <- contList[["result"]][["data"]][["X"]] 
    Y <- contList[["result"]][["data"]][["Y"]]
    layout <- rbind(layout, data.frame(observationUnitName=observationUnitName,
                                       accession=accession, plotNumber=plotNumber, 
                                       replicate=replicate, blockNumber=blockNumber,
                                       X=X, Y=Y,
                                       stringsAsFactors=F))
  }
  call <- paste0(con, "/brapi/v1/studies/", studyDbId)
  contList <- get_brapi_json(call, token)
  studyName <- contList[["result"]][["studyName"]]
  locationName <-contList[["result"]][["location"]][["abbreviation"]]
  layout$studyName <- rep(studyName, nrow(layout))
  layout$locationName <- rep(locationName, nrow(layout))
  return(layout)
}


#### Plot functions ####

layoutPlot <- function(layout_info, input){
    x.max <- max(as.numeric(layout_info$X)) + 1
    y.max <- max(as.numeric(layout_info$Y)) + 1
	p <- ggplot(data=layout_info, aes(x=as.numeric(X), y=as.numeric(Y),
	                                    fill=blockNumber)) + 
	           geom_tile(color="black", size=1) +
	           geom_text(aes(label=plotNumber)) +
	           ggtitle(isolate(input$trialName)) +
	           xlab("Pass") + ylab("Tier") +
	           xlim(0, x.max) + ylim(0, y.max) +
	           scale_x_continuous(breaks=seq(1, x.max-1)) +
	           scale_y_continuous(breaks=seq(1, y.max-1)) +
	           theme(panel.grid=element_blank(),
	                 legend.position="none")
	return(p)
}


#### General functions ####

is_null_layout <- function(layout_info){
  # returns TRUE if there is no layout and FALSE
  # otherwise
  if(layout_info$X[1] == ""){ 
  # Check if X coordinate is empty; empty for one = empty for all
    return(TRUE)
  } else {
  # If first X coordinate is not empty there is a layout
    return(FALSE)
  }
}


is_valid_design <- function(nRow, nCol, layout_info, design_type){
  # Checks that the proposed grid is valid
  # for the chosen design
  
  nRow <- as.numeric(nRow)
  nCol <- as.numeric(nCol)
  max_plots <- as.numeric(nRow) * as.numeric(nCol)
  nPlots <- nrow(layout_info())
  nBLK <- max(as.numeric(layout_info()$blockNumber))
  if(nPlots == max_plots){
  	if(design_type == "Stacked"){
  	  return(TRUE)
  	} else {
  	  if(nCol %% nBLK == 0){  # Number of passes must be evenly divisible by block number
  	  	return(TRUE)
  	  } else {
  	  	return(FALSE)
  	  }  		
  	}
  } else{
  	return(FALSE)
  }
}


read_metadata <- function(metadata){
  # Make sure the required columns are  present in the spreadsheet
  cols <- c("Entry", "Name", "HT", "Grain_Class", "Grain_Type")
  if(!is.null(metadata)){
    metadata.file <- read.xlsx(metadata$datapath)
    if(sum(cols %in% colnames(metadata.file)) == 5){
  	  return(metadata.file)
    } else{
  	  return(NULL)
    }
  } else{
  	return(NULL)
  }
}


get_combine_order <- function(dim1_len, dim2_len, nPlots, flip){
  # This function generates a serpentine 
  # planting or harvest order 
  
  #flip <- 0  # used to indicate when order should be reversed
  tmp <- c()  # holds a single row/column
  all <- c()  # holds final vector with all rows/columns
  n <- 1
  for(i in 1:dim1_len){  
    for(j in 1:dim2_len){
	  tmp <-c(tmp, n)
	  n <- n + 1  # If n is greater than total plots stop and return.
	  if(n > nPlots){
	    if(flip == 0){  
	      all <- c(all, tmp)
	    } else{
	      rev_tmp <- rev(tmp)
	      all <- c(all, rev_tmp) 	
	    }
	    return(all)
	  }
	}
	if(flip == 0){
	  flip <- 1  
	  all <- c(all, tmp) 
	  tmp <- c()  
	} else{
	  flip <- 0
      rev_tmp <- rev(tmp)  	
	  all <- c(all, rev_tmp)
	  tmp <- c()
	}
  }
}


make_serpentine <- function(dim1_len, dim2_len, nPlots){
  # This function generates a serpentine 
  # pattern for a given number of plots 
  # in the direction of dim1. A dim (dimension) is
  # a row or column in a field.
  
  flip <- 0  # used to indicate when order should be reversed
  tmp <- c()  # holds single row/column at a time
  all <- c()  # holds final vector with all rows/columns
  n <- 1
  for(i in 1:dim1_len){  
    for(j in 1:dim2_len){
	  tmp <-c(tmp, j)
	  n <- n + 1  # If n is greater than total plots stop and return.
	  if(n > nPlots){
	    if(flip == 0){  
	      all <- c(all, tmp)
	    } else{
	      rev_tmp <- rev(tmp)
	      all <- c(all, rev_tmp) 	
	    }
	    return(all)
	  }
	}
	if(flip == 0){
	  flip <- 1  
	  all <- c(all, tmp) 
	  tmp <- c()  
	} else{
	  flip <- 0
      rev_tmp <- rev(tmp)  	
	  all <- c(all, rev_tmp)
	  tmp <- c()
	}
  }
}


make_default_layout <- function(layout_info){
  # Makes default layout: 
  # One rep = one row
  # Serpentine
  new_layout <- layout_info
  new_layout$Y <- as.numeric(layout_info$blockNumber)
  new_layout$plotNumber <- as.numeric(new_layout$plotNumber)
  sorted_new_layout <- new_layout[order(new_layout$Y, new_layout$plotNumber),]
  nTiers <- max(sorted_new_layout$Y)
  nPlots <- nrow(sorted_new_layout)
  nPasses <- ceiling(nPlots/nTiers)
  X_new <- make_serpentine(nTiers, nPasses, nPlots)
  sorted_new_layout$X <- X_new
  return(sorted_new_layout)
}


make_side_to_side_layout <- function(layout_info, nTiers, nPasses){
  # This makes layouts where the blocks are
  # side-to-side pass-wise
  nTiers <- as.numeric(nTiers)
  nPasses <- as.numeric(nPasses)
  nPlots <- nrow(layout_info)
  nBlks <- max(as.numeric(layout_info$blockNumber))
  nBlkPasses <- nPasses/nBlks
  nBlkPlots <- nPlots/nBlks
  new_layout <- data.frame()
  for(blk in 1:nBlks){
    blk_layout <- layout_info[layout_info$blockNumber == as.character(blk),]
    blk_layout <- blk_layout[order(as.numeric(blk_layout$plotNumber)),]
    blk_layout$X <- as.numeric(blk_layout$X)
    blk_layout$Y <- as.numeric(blk_layout$Y) 
    pass_offset <- (blk - 1) * nBlkPasses
    blk_layout$X <- make_serpentine(nTiers, nBlkPasses, nBlkPlots) + pass_offset
    blk_layout$Y <- rep(1:nTiers, each=nBlkPasses)
    new_layout <- rbind(new_layout, blk_layout)
  }
  return(new_layout)
}


make_stacked_layout <- function(layout_info, nTiers, nPasses){
  # This makes layouts where the blocks are 
  # stacked on top of each other tier-wise
  new_layout <- layout_info
  nTiers <- as.numeric(nTiers)
  nPasses <- as.numeric(nPasses)
  nPlots <- nrow(new_layout)
  new_layout <- new_layout[order(as.numeric(new_layout$plotNumber)),]
  new_layout$X <- make_serpentine(nTiers, nPasses, nPlots)
  new_layout$Y <- rep(1:nTiers, each=nPasses)
  return(new_layout)
}


get_plant_plan <- function(layout_info, border, metadata){
  # Creates planting plan info from layout_info


  padNum <- function(num, digits){
    # Returns a number a string with padded
    # zeros
    numLen <- nchar(as.character(num))
    pad <- digits - numLen
    padNum <- paste0(paste0(rep("0", pad), collapse=""), num)
    return(padNum)
  }
  

  # Get field dimensions
  nRows <- max(as.numeric(layout_info$Y))
  nCols <- max(as.numeric(layout_info$X))
  nPlots <- nrow(layout_info)

  # Subset layout dataframe and sort layout 
  # by pass and then tier within pass
  df <- layout_info[,c("observationUnitName", "plotNumber",
                       "studyName", "locationName", 
                       "accession", "replicate", "blockNumber", "X","Y")]
  sorted_df <- df[order(as.numeric(df$X), as.numeric(df$Y)),]
  colnames(sorted_df) <- c("plot_name", "plot_number", "study_name",
                            "location", "accession", "rep", "block", "pass", "tier")
  sorted_df$tier <- as.numeric(sorted_df$tier)
  sorted_df$pass <- as.numeric(sorted_df$pass)
  
  # Get planting order
  if(!is.null(border)){
    if("Left" %in% border){ 
      planting_order <- get_combine_order(dim1_len=nCols, dim2_len=nRows,
                                    nPlots=nPlots, flip=1) + nRows
    } else{
     planting_order <- get_combine_order(dim1_len=nCols, dim2_len=nRows,
                                         nPlots=nPlots, flip=0)
    }  	
  } else{
    planting_order <- get_combine_order(dim1_len=nCols, dim2_len=nRows,
                                        nPlots=nPlots, flip=0)  	
  }
  sorted_df$planting_order <- planting_order

  # Get harvest order
  sorted_df$harvest_order <- get_combine_order(dim1_len=nCols, dim2_len=nRows,
                                               nPlots=nPlots, flip=0)
  # Add metadata if it is present
  metadata <- read_metadata(metadata)  
  if(!is.null(metadata)){
    new_df <- data.frame()
  	for(i in 1:nrow(sorted_df)){
  	  name <- sorted_df[i,"accession"]
  	  if(name %in% metadata$Name){
  	  	pos <- which(metadata$Name == name)
  	  	new_df <- rbind(new_df, data.frame(plot_name=sorted_df[i, "plot_name"],
  	  	                                   plot_number=sorted_df[i, "plot_number"],
  	  	                                   study_name=sorted_df[i, "study_name"],
  	  	                                   location=sorted_df[i, "location"],
  	  	                                   rep=sorted_df[i, "rep"],
  	  	                                   block=sorted_df[i, "block"],
  	  	                                   entry=metadata[pos, "Entry"],
  	  	                                   accession=sorted_df[i, "accession"],
  	  	                                   herbicide_class = metadata[pos, "HT"],
  	  	                                   grain_class = metadata[pos, "Grain_Class"],
  	  	                                   grain_type = metadata[pos, "Grain_Type"],
  	  	                                   tier = sorted_df[i, "tier"],
  	  	                                   pass =sorted_df[i, "pass"],
  	  	                                   planting_order=sorted_df[i, "planting_order"],
  	  	                                   harvest_order=sorted_df[i, "harvest_order"]
  	  	                                   ))
  	  }	else{
 	  	new_df <- rbind(new_df, data.frame(plot_name=sorted_df[i, "plot_name"],
  	  	                                   plot_number=sorted_df[i, "plot_number"],
  	  	                                   study_name=sorted_df[i, "study_name"],
  	  	                                   location=sorted_df[i, "location"],
  	  	                                   rep=sorted_df[i, "rep"],
  	  	                                   block=sorted_df[i, "block"],
  	  	                                   entry="FILLER",
  	  	                                   accession=sorted_df[i, "accession"],
  	  	                                   herbicide_class = NA,
  	  	                                   grain_class = NA,
  	  	                                   grain_type = NA,
  	  	                                   tier = sorted_df[i, "tier"],
  	  	                                   pass =sorted_df[i, "pass"],
  	  	                                   planting_order=sorted_df[i, "planting_order"],
  	  	                                   harvest_order=sorted_df[i, "harvest_order"]
  	  	                                   ))  	  
  	  }
  	}
  	sorted_df <- new_df[order(as.character(new_df$entry), new_df$rep),]
    # Pad entries
  	entDigits <- nchar(max(as.numeric(sorted_df$entry,na.rm=T)))
  	sorted_df$entry <- sapply(sorted_df$entry, function(x) padNum(x, digits=entDigits))
  }
  # Pad plot_number, planting, and harvest order
  plotDigits <- nchar(max(as.numeric(sorted_df$plot_number, na.rm=T)))
  plantDigits <- nchar(max(as.numeric(sorted_df$planting_order, na.rm=T)))
  harvestDigits <- nchar(max(as.numeric(sorted_df$harvest_order, na.rm=T)))
  
  sorted_df$plot_number <- sapply(sorted_df$plot_number, function(x) padNum(x, digits=plotDigits))
  sorted_df$planting_order <- sapply(sorted_df$planting_order, function(x) padNum(x, digits=plantDigits))
  sorted_df$harvest_order <- sapply(sorted_df$harvest_order, function(x) padNum(x, digits=harvestDigits))
  return(sorted_df)
}


get_field_grid <- function(data, border){
  data$Y <- as.numeric(data$Y)
  data$X <- as.numeric(data$X)
  nRows <- max(data$Y)
  nCols <- max(data$X)
  offset <- 0
  if(!is.null(border)){
    nCols <- nCols + length(border)
    if("Left" %in% border){
      offset <- 1
    }
  }
  nPlots <- nCols * nRows
  fieldMat <- matrix(rep("B", nPlots), nrow=nRows, ncol=nCols )
  for(i in 1:nrow(data)){
  	fieldMat[data[i,"Y"], data[i,"X"] + offset] <- data[i,"plotNumber"]
  }
  fieldMat <- as.data.frame(fieldMat[rev(1:nRows),])
  return(fieldMat)	
}

