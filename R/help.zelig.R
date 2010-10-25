help.zelig <- function (...)  {

        driver  <- match.call()
        driver  <- as.character(driver)
        name <- NULL
        if(length(driver) > 1){ 
		name <- driver[2]
	} else {
                print(do.call("help",list(package="Zelig"), envir=parent.frame()))
                return(invisible(NULL))
	}
        if (name == "models"){
                print(do.call("vignette", list(package="Zelig")))
                return(invisible(NULL))
        }
                  
        filesPDf <- NULL
        
        helpfile <- try(system.file("Meta", "vignette.rds", package="Zelig"))
        
        if(helpfile!=""){
                helpMtrx <- .readRDS(helpfile)
                ix <- grep("[pP][dD][fF]", colnames(helpMtrx))
                if(length(ix)) filesPDF <- helpMtrx[,ix]
                
                if(length(filesPDF) && length(name))
                  {
                          fl  <- paste("^",name,".pdf$",sep="")
                          ix  <- grep(fl,filesPDF)
                          if(length(ix)){
                                  file <- filesPDF[ix]
                                  print(do.call("vignette", c(list(topic=name),list(package="Zelig"))))
                                  return(invisible(list()))
                          }
                  }
        }
        helpfile  <- try(system.file("Meta", "hsearch.rds", package="Zelig"))
        
        fileshtml <- NULL
        if(helpfile != "")
          {
                  helpMtrx  <- .readRDS(helpfile)
                  fileshtml <- helpMtrx[[2]][,"Aliases"]
                  ix <- grep("url$", fileshtml)
                  if(length(ix))
                    fileshtml <- fileshtml[-ix]
                  if(length(fileshtml) && length(name))
                    {
                            ix <- grep(name, fileshtml)
                            
                            if(length(ix)){
                                    
                                    print(do.call("help", c(list(as.name(name)), list(package="Zelig")), envir=parent.frame()))
                                    return(invisible(NULL))
                            }
                            
                                              }
          }
        ##message("Not valid input...Showing package description")
        do.call("help", c(list("Zelig"), list(package="Zelig")), envir=parent.frame())
}

