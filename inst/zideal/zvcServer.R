
### DESCRIPTION: Reads all functions describe.#.R that are part of
###              Zelig package and gets all direct dependencies of Zelig
###              packages names and url's to installed them
###              Taken from Ferdi Alhimadi's code
###
zeligDescribeModels <- function(PKG="Zelig", baseOrRecPkgs=NULL)
  {
    descList <- ls(envir=asNamespace(PKG),pattern="^describe")
    model <- NULL
    if(length(baseOrRecPkgs) <= 0){
      baseOrRecPkgs <- rownames(installed.packages(priority = "base"))
      baseOrRecPkgs <- getRidVersion( baseOrRecPkgs)
    }
###  model <- pkgDepends(PKG)$Depends
    url <- NULL
    for (desc in descList){
      ## call the function which return a list
      tmp <- do.call(desc,list(),env=asNamespace("Zelig"))
      ## get the package name from the list (maybe CRAN too)
      nm <- names(model)
      tmpmodel <- tmp$package$name
      tmpmodel <- trim.blanks(tmpmodel)
      tmpmodel <- getRidVersion(tmpmodel, TRUE)
      model <- c(model, tmpmodel)
      names(model) <- c(nm,tmpmodel)
      nmu <- names(url)
      tmpurl <- tmp$package$CRAN
      
      if(length(tmpurl) > 0 )
        tmpurl <- trim.blanks(tmpurl)
      else
        tmpurl <- NA
      url <- c(url,tmpurl)
      names(url) <- c(nmu,tmpmodel)
    
    }
    model <- unique.default(model)
  
    savenm <- F
    allPkgs <- names(model)
    if(length(allPkgs) <= 0){
      savenm <- T
      allPkgs <- model
    }
    allPkgs <- matrix(as.vector(allPkgs), ncol=1)
    if(length(baseOrRecPkgs))
      allPkgs  <- allPkgs[!(allPkgs %in% baseOrRecPkgs)]
    model <- allPkgs
    if(savenm)
      names(model) <- allPkgs
 
    ind <- sapply(model, match, names(url))
    if(length(ind)){
      ind <- suppressWarnings(na.omit(unlist(ind)))
      url <- url[ind]
    }
  
    lst <- c(list(model=model), list(url=url))
    return(lst)
  }

### DESCRIPTION: Gets all the depends packages recursevely starting
###              with the input package repList as argument of the function call.  
###              It obtains all dependends packages and their version numbers
###              from the description docs of the packages.   
###
### OUTPUT: A list of two elements, the packages edges and their versions. 
###         if res<- makeLinks("Zelig"), then names(res) = c(edges, vers)
###         res$edges is a list whose elements are packages names,i.e. nodes,
###         and whose values are a vector of packages names that the list node
###         depends upon; res$vers is also a list that contains the nodes of 
###         res$edges but the values of the list nodes are the versions of the
###         dependencies.
###
### INPUT: repList the mane of a package or vector with packages names;
###        it can also be an url, in that case gets all vailable packages.
###        keep.builtin where to stop the recursion, if false it will
###        stop for the packages that are installed in the machine
###        at priority="base";  if keep.builtin is string or null 
###        it stops according to the specify priority,
###        keep.builtin = "high","NA", NULL,"recommended"; 
###        where "high"= c("base", "recommended");  
###        if keep.builtin =TRUE it continues recursively until returns nothing.
###        baseOrRecPkgs if not null it has a vector of packages and then
###        overwrites keep.builtin and stops the recursion for packages in the vector.  
###        norepeat means that, for given node, packages that are listed in depends
###        and also in either imports or suggests are shown only once for depends.   
###       
###
### USE res <- makeLinks(pkg)
###
### USES grep.exact, readDescription
###
### AUTHOR Elena Villalon
###        evillalon@iq.harvard.edu
###
###        Last modified: 12/19/2006
###

makeLinks <- function (repList, keep.builtin = FALSE,baseOrRecPkgs=NULL,norepeat=FALSE)
{
  iter <- 100 ### not to get into an infinite loop

  fromurl <- FALSE
  if (is.character(repList) && length(grep("http", repList)) >0)
    pkgMatList <- lapply(repList, function(x) {
      available.packages(contrib.url(x))
      fromurl <- TRUE
    })

  if(length(baseOrRecPkgs) <= 0 ){
    if(!is.logical(keep.builtin))
      baseOrRecPkgs <- rownames(installed.packages(priority = keep.builtin))
    else if (!keep.builtin) 
      baseOrRecPkgs <- rownames(installed.packages(priority = "base"))
    baseOrRecPkgs <- getRidVersion( baseOrRecPkgs)
  }
###
  keep.builtin <- ifelse(length(baseOrRecPkgs) >0, FALSE, TRUE)
  
  if(is.character(repList) && fromurl){
    
    allPkgs <- unlist(sapply(pkgMatList, function(x) rownames(x)))
  }else if (length(repList) > 0){
    
    allPkgs <- matrix(as.vector(repList), ncol=1)
  }
  if (!length(allPkgs)) 
      stop("no packages in specified repositories")
  allPkgs   <- unique(allPkgs)
  allPkgs   <- unlist(sapply(allPkgs, FUN="trim.blanks"))
  allPkgs   <- sort(allPkgs)
  basePkgs  <- allPkgs
  basePkgs  <- getRidVersion(basePkgs)
 
  if (length(allPkgs) && !keep.builtin) 
    allPkgs  <- allPkgs[!(basePkgs %in% baseOrRecPkgs)]
  if(length(allPkgs) <= 0){
    message(basePkgs, " is part of base packages. Set keep.builtin = T") 
    return(list())
  }
  pkgEdges  <- list()
  pkgVers   <- list()
  pkgURL    <- list()
  pkgLevel  <- list()
  cnt=0;

  Rvers <- unlist(packageDescription(basePkgs[1])$Built)
  if(class(Rvers) == "try-error")
     Rvers <- unlist(packageDescription(allPkgs[1])$Built)
  
  Rvers <- trim.blanks(Rvers)

  RV1   <-  strsplit(sub("^R[[:space:]]*([-0-9 .]*)","\\1", Rvers), ";")[[1]][1]
  Rvers <- trim.blanks(RV1)
  RV2 <- strsplit(sub("^R[[:space:]]*([-[:digit:].]*)","\\1", Rvers), ";")[[1]][1]
  RV2 <- trim.blanks(RV2)
  RV  <- strsplit(sub("^R[[:space:]]*(.*)","\\1", Rvers), ";")[[1]][1]
  Rvers <- trim.blanks(RV)
 if (!identical(RV1, Rvers)||!identical(RV2,Rvers ))
   warning("Bad regx in makeLinks...check")
###   0-9==[:digit:]
  
 
  while(length(allPkgs)>0){
     cnt <- cnt +1
     
###     message("counting ....")
###     print(cnt)
     iterPkgs <- NULL
     for(n in 1:length(allPkgs)){
       cat(n, ": ")
       cat(allPkgs[n],"\n")
       pack <- allPkgs[n]
###      print(pack)
      
       lst <- suppressWarnings(readDescription(pack, norepeat))
###    lst <- readDescription(pack, norepeat)
        
       if(class(lst) == "try-error")
         next; 
    
       if(length(lst$totdepend) <= 0)
         next; 
       
       pkgNames <- lst$totdepend
       nm <- names(pkgNames)
       pkgNames <- getRidVersion(pkgNames)
       names(pkgNames) <- nm
       url <- unlist(lst$url)
       Rvers <- unlist(lst$Rvers)    
       pkgVersions <- lst$totversion
       
       deps  <- unlist(pkgNames)
       
       
       depsV <- unlist(pkgVersions)
       ix <- match("R",names(depsV))
       if(!is.na(ix))
         depsV <- depsV[-ix]
 ###     message("Packages names...")
 ###     print(deps)
 ###     message("Packages versions...")
 ###     print(depsV)
     
      if (length(deps) && !keep.builtin) {
        deps  <- deps[!(deps %in% baseOrRecPkgs)]
        depsV <- depsV[!(names(depsV) %in% baseOrRecPkgs)]
        url <- url[!(names(url) %in%  baseOrRecPkgs)]
      }
      if(length(deps) <= 0)
        next;
      nm <- NULL
      if(length(pkgEdges) > 0)
        nm <- names(pkgEdges)      
       pkgEdges <- c(pkgEdges, list(deps))
       names(pkgEdges) <- c(nm, pack)
       pkgVers <- c(pkgVers, list(depsV))
       names(pkgVers) <- c(nm, pack)
       pkgURL <- c(pkgURL, list(url))
       names(pkgURL) <- c(nm, pack)
       pkgLevel <- c(pkgLevel, list(cnt))
       names(pkgLevel) <- c(nm, pack)
       iterPkgs <- c(iterPkgs, deps)  
      
     }
    
     nmedge   <- names(pkgEdges)
     nmedge   <- sapply(nmedge, FUN="trim.blanks")
     countfor <- NULL
     nmedge   <- unique.default(nmedge)
     iterPkgs <- unlist(sapply(iterPkgs, FUN="trim.blanks"))
     iterPkgs <- unique.default(iterPkgs)
     diffPkgs <- NULL
     if(length(nmedge) > 0){
       commonPkgs <- intersect(nmedge, iterPkgs)
       diffPkgs   <- setdiff(iterPkgs, nmedge)
       diffPkgs <- unique.default(diffPkgs)
       nmedgemo <- sapply(nmedge,function(nm) paste("^",nm,"$", sep=""))
       ### no need to call grep.exact 
       countfor <- unlist(sapply(nmedgemo,grep,iterPkgs,extended=T))
    
     }
    
     if(length(countfor) > 0){
       names(iterPkgs)[countfor] <-iterPkgs[countfor] ### packages found by grepping 
       ix <- grep.exact(iterPkgs[countfor], nmedge)$index ### eliminate not exact matches
       if(length(ix) > 0)
         countfor <- countfor[-ix]
     }
     if(length(countfor) > 0)
       iterPkgs <- iterPkgs[-countfor]
     
     
     if(length(diffPkgs) > 0 && length(iterPkgs) > 0
        && any(sort(iterPkgs) != sort(diffPkgs))){
       warning("Bad counting of packages...check code")
       best1 <- setdiff(iterPkgs, diffPkgs)
       best2 <- setdiff(diffPkgs, iterPkgs)
     }
     if(length(iterPkgs) > 0)
       allPkgs <- iterPkgs
     else
       break;
     
###     message("next iteration...")
     names(allPkgs) <- NULL
     allPkgs <- unlist(sapply(allPkgs, FUN="trim.blanks"))
     allPkgs <- sort(allPkgs)
###     message("What is left....")
  
     
     if(cnt > iter) {
       warning("Iterations limit to get the dependencies reach at..",iter) 
       break;
     }
   }
       
   
###    nodeDataDefaults(depG, attr = "size") <- as.numeric(NA)
###        if (dosize) {
###            sizes <- getDownloadSizesBatched(makePkgUrl(pMat))
###            nodeData(depG, n = rownames(pMat), attr = "size") <- sizes
###        }
###  print(pkgEdges)
###  print(pkgVers)
### 
  
 
  if((length(basePkgs) <= 1 && !identical("Zelig", basePkgs))){
    rootlst  <- read.root(basePkgs)
    pkgEdges <- c(rootlst[1], pkgEdges)
    nm <- names(pkgEdges)
    pkgVers <- c(rootlst[2], pkgVers)
    names(pkgVers) <- nm
    pkgURL <- c(rootlst[4], pkgURL)
    names(pkgURL) <- nm
    pkgLevel <- c(rootlst[5], pkgLevel)
    names(pkgLevel) <- nm
  }
   pkgVers <- c(Rvers,pkgVers)
  lst <- c(edges=list(pkgEdges),vers=list(pkgVers),url=list(pkgURL), level=list(pkgLevel))
  return(lst)
}

### DESCRIPTION: helper func that gets information about packages in vector basePkgs
###              as stored in packageDescription and returns list
###              with elements of list that will be used in Zelig.
###

read.root <- function(basePkgs){
  alledge <- NULL 
  allvers <- NULL
  allnode <- NULL
  allurl  <- NULL
  nm <- names(basePkgs) 
  basePkgs <- getRidVersion(basePkgs)
  names(basePkgs) <- nm
  for(pack in basePkgs){
    
    lst <- try(packageDescription(pack), silent=TRUE)
    vers <- lst$Version
    Rbuilt <- lst$Built
    Rbuilt <- trim.blanks(Rbuilt)
 ###trying different ways for regex; they should all give same result
    RV1 <-  strsplit(sub("R[[:space:]]*([0-9 .]*)","\\1", Rbuilt), ";")[[1]][1]
    RV1 <- trim.blanks(RV1)
    RV2 <-  strsplit(sub("R[[:space:]]*([[:digit:].]*)","\\1", Rbuilt), ";")[[1]][1]
    RV2 <- trim.blanks(RV2)
    RV <-  strsplit(sub("R[[:space:]]*(.*)","\\1", Rbuilt), ";")[[1]][1]
###   print(RV)
    Rvers <- trim.blanks(RV)
    if(!identical(RV1, Rvers) || !identical(RV2, Rvers))
      warning("Bad regex in read.root...check")
    
    Rvers <- paste("R", Rvers)
    Rvers <- trim.blanks(Rvers)
    url <- lst$URL
    
    if(length(url) <= 0 )url <-  "CRAN"
    if( is.na(url)) url <-  "CRAN"
   
    alledge <- c(alledge, pack)
    names(alledge) <- rep("depends", length(alledge))
    allvers <- c(allvers, vers)
    
    allnode <- c(allnode, Rvers)
 
    allurl  <- c(allurl,url)
    
  }
 
   names(allvers) <- alledge
   names(allurl)  <- alledge
  
  allnode <- unique.default(allnode)
  mxR <- sort(allnode)[1]
  redge <- c(list(alledge), list(allvers), list(allnode), list(allurl), list("0"))
  names(redge) <- c(mxR, "vers", "node", "url", "level")
  

  return(redge)
    
    
  }
### DESCRIPTION: helper function to makeLinks; it reads the package
###              description and returns dependencies (i.e. depends, imports, suggests)
###              of derived packages, the url's to download dependent packages and R version
### INPUT: name of a package
###

readDescription <- function(pack,norep){
### Get rid of leading a trailing white spaces

  nm <- names(pack)
  pack   <- suppressWarnings(trim.blanks(pack))
  packno <- pack
  if(length(grep("_", pack)) > 0){ 
    packno <- sub("(.*)_([-0-9.]*)","\\1", packno)
    packno <- suppressWarnings(trim.blanks(packno))
    names(packno) <- nm
  }
 
  lst0 <- try(packageDescription(packno), silent=TRUE)
 
  if(class(lst0) == "try-error")
    lst0 <- try(packageDescription(pack), silent=TRUE)
###      print(lst$Package) 
###      print(lst$Version)
  if(class(lst0) == "try-error")
    return;
                
  depends  <- lst0$Depends
  Rvers <- NA
  dependsV <- NULL
  if(length(depends) > 0){
  
    dependslst <- sapply(strsplit(depends,",")[[1]], FUN="trim.blanks")
   
    lst <- find.names.vers(dependslst)
    
    depends  <- unlist(lst$pkgnames)
    dependsV <- unlist(lst$pkgnumbers)
    Rvers <- try(dependsV["R"], silent=T)
    ix <- match("R", depends)
    ixg <- grep("^R", depends, extended=T)
    if(length(ixg)> 1)
      warning("check readDescription")
   
    if(!is.na(ix))
      depends <- depends[-ix]
    ix <- try(match("R", names(dependsV)), silent=T)
   
    if(!is.na(ix))
      dependsV <- dependsV[-ix]
      
  }
  
    
  rdep <-  trim.blanks(lst0$Built)
 
  
  if(!is.logical(Rvers) && length(rdep) > 0 && rdep != ""){
###   RV <-  strsplit(sub("R[[:space:]]*([0-9 .]*)","\\1", rdep), ";")[[1]][1]
      RV <-  strsplit(sub("R[[:space:]]*(.*)","\\1", rdep), ";")[[1]][1]
###   print(RV)
    Rvers <- trim.blanks(RV)

  }
     
###      print(depends)
  suggests <- lst0$Suggests
  suggestsV <- NULL
  if(!is.null(suggests)){
  
    suggestslst <- unlist(sapply(strsplit(suggests,",")[[1]], FUN="trim.blanks"))
    lst <-  find.names.vers(suggestslst)
    suggests <- lst$pkgnames
    suggestsV <- lst$pkgnumbers
    ind <- na.omit(unlist(sapply(depends,match,suggests)))
    ind1 <- na.omit(unlist(sapply(depends,match,names(suggestsV))))
    ### package are shown under any category if repeated when norep=F
   
    
    if(length(unlist(ind)) > 0 && !norep)
      suggests <- suggests[-ind]
    if(length(unlist(ind1)) > 0 && norep)
      suggestsV <- suggests[-ind1]

  }
  
  imports  <- lst0$Imports
  importsV <- NULL
  if(length(imports)>0){
    
    importslst <- unlist(sapply(strsplit(imports,",")[[1]], FUN="trim.blanks"))
    lst <-  find.names.vers(importslst)
    imports <- lst$pkgnames
    importsV <- lst$pkgnumbers

    ind <- na.omit(unlist(sapply(depends,match,imports)))
    ind1 <- na.omit(unlist(sapply(depends,match,names(importsV))))
   
### package are shown under any category if repeated and norep =F
    
    if(length(unlist(ind1)) > 0 && !norep)
      importsV <- imports[-ind1]
    if(length(unlist(ind)) > 0 && !norep)
      imports <- imports[-ind]
      
    ind <- na.omit(unlist(sapply(suggests,match,imports)))
    ind1 <- na.omit(unlist(sapply(suggests,match,names(importsV))))
 
    if(length(unlist(ind1)) > 0 && !norep)
      importsV <- imports[-ind1]
    if(length(unlist(ind)) > 0 && !norep)
      imports <- imports[-ind]
    
  }
     
  if(length(depends) <= 0 && length(suggests) <= 0 && length(imports) <= 0)
    return(list());
     
  allpkg <- c(depends, imports, suggests)
  allvers <- c(dependsV, importsV, suggestsV)

  dep <- rep("depends",length(depends))
  if(length(depends) > 0)
    names(depends) <- dep
  sgg <- rep("suggests",length(suggests))
  if(length(suggests) > 0)
    names(suggests) <- sgg
  imp <- rep("imports",length(imports))
  if(length(imports) > 0)
    names(imports) <- imp
        
  totdepend <- c(depends,imports, suggests)
  if(length(totdepend) <= 0){
    lst <- c(Rvers=list(Rvers))
    return(lst)
  }
 
  url <- sapply(allpkg, function(pkg)
                {
                  nm <- packageDescription(pkg)$URL
                  if(length(nm) > 0)
                    nm <- trim.blanks(nm)
              
                  if(length(nm) <= 0) 
                    nm <- "CRAN"
               
                  return(nm)
                })
  
###  print(totdepend)                    
  lst <- c(totdepend =list(totdepend), totversion = list(allvers),
           url=list(url), Rvers=list(Rvers))
  return(lst)
}
### DESCRIPTION: helper function to makeLinks & readDescription; it reads the package
###              description and returns the name, version # of pkgs in dependlst 
###              
### INPUT: dependlst a list with the string return from one of: dependlst = 
###        packageDescription(pkg)$Depends,  packageDescription(pkg)$Imports
###         packageDescription(pkg)$Suggests
###        
###
### OUTPUT: names and version numbers for the packages in dependlst
###
 find.names.vers <- function(dependslst){
   
   depends <- suppressWarnings(unlist(sapply(dependslst, FUN="trim.blanks")))
   tag <- "([[:alnum:]_ .]*)[[:space:]]*([-\\(<>= .[:alnum:][:space:]\\)]*)"
   
   ### from packages.dependencies (library tools):
   ### pat <- "^([^\\([:space:]]+)[[:space:]]*\\(([^\\)]+)\\).*"
  
   pkgnames <- sapply(depends, function(nm) {
     res <- sub(tag,"\\1", nm)})
   names(pkgnames) <- NULL
   pkgnames <- suppressWarnings(sapply(pkgnames, FUN="trim.blanks"))
   names(pkgnames) <- pkgnames
   ind <- 1:length(dependslst)
   names(ind) <- pkgnames
   pkgnumbers <- sapply(ind, function(n){
     dep <- dependslst[n]
     pk  <- pkgnames[n]
     names(pk) <- NULL
     if(suppressWarnings(identical(pk,"R"))){ 
       names(dep) <- NULL
       res <- sub(tag,"\\2", dep)
       res <- suppressWarnings(trim.blanks(res))
       pat <- "[-\\(<>= [:space:]]*([0-9.]*)[\\)]*"
       res <-  sub(pat,"\\1",res)  
       return(res)
     }
     desc <- NULL
     names(pk) <- NULL
     if(!suppressWarnings(identical(pk, "R"))){
       
       desc <- try(packageDescription(pk), silent=TRUE)
     }
     nm <- NULL
     nm0 <- nm
     if(class(desc)!="try-error" && length(desc) > 0) 
       nm0 <- try(desc$Version, silent=TRUE)
    
     if(class(nm0)!="try-error" && length(nm0) > 0)
       nm <- trim.blanks(nm0)
        
     if(length(nm) <= 0 || nm=="" ) nm <- NA
     return(nm)})
   
   lst <- c(list(pkgnames=pkgnames), list(pkgnumbers=pkgnumbers))}
                      
    

trim.blanks <- function(x) {
### at the beginning of string"^" gets anny number (+) of white spaces
  f <- x
  if(length(x))
   f <- na.omit(x)
    
   if(length(f) <= 0)
     return(x)
   if(length(f)>1)
    print(f)
    if(f=="" )
      return(x)
    x <- sub("^[[:space:]]*(.*)", "\\1",x) ###get \n\t
    x <- sub('^ +', '', x) ###get white spaces
     
### at the ending of string"$" gets anny number (+) of white spaces
 
    x <- sub("(.*)[[:space:]]*$", "\\1", x)
    x <- sub(' +$', '', x)
     return(x)
    }

### 
### DESCRIPTION: Creates list with packages that are in input list edge
###              List elements are vectors with the package name,
###              its version and the parent.  For every package in
###              edge, finds out the version stored in vers and the parent, 
###              which is the name of the list element the package belongs to
###              
### INPUT: The list output of function makeLinks
###        lstoutput <- makeLinks("Zelig")
###
### OUTPUT: A list of packages, whose values is a vector of three values 
###         First, the package name with any of the tags depends, suggests
###         and imports as name of the vector component.  Second, the version
###         of the package and third the parent it was derived from.
###
### USE: lst the output of makeLinks, then
###      res <- reverseLinks(lstoutput <- makeLinks(pkg))
###         
### AUTHOR Elena Villalon
###        evillalon@iq.harvard.edu
###
###        Last modified: 01/11/2007
###

reverseLinks <- function(lstoutput){
  edge <- lstoutput$edges
  if(length(edge) <= 0)
    stop(paste("No leaves for ", lstoutput))
  
  vers <- lstoutput$vers
  urls <- lstoutput$url
  level <- sapply(lstoutput$level, as.numeric)
  mxlevel <- max(unlist(level))
  
  cnt    <- length(edge)
  nmpkg  <- names(edge)
  outlst <- list()
  inc <- 0
  if(length(vers) > length(edge)) inc <- 1
  
  while(cnt > 0){
    
    pkgs <- edge[[cnt]]
    nm   <- nmpkg[cnt]
    pkgV <- vers[[cnt+inc]]
    pkgU <- urls[[cnt]]
    ind  <- as.list(1:length(pkgs))
    depth <- level[[cnt]] 
 ###   print(depth)
    
    lst <- lapply(ind, function(n){
      pkg <- pkgs[n]
      ver <- pkgV[n]
      url <- pkgU[n]
   ###   print(url)
      names(ver) <- NULL
      names(url) <- NULL
      ret <- c(pkg, vers=ver, parent=nm, URL=url, depth=depth)
    })
                  
    nmlst <- names(outlst)
    outlst <- c(outlst, lst)
    names(outlst) <- c(nmlst, nm)
    cnt = cnt -1;
    
  }
  vec <- sapply(outlst, function(m) m[1])
  names(outlst) <- vec
  return(outlst)
    
}

### 
### DESCRIPTION: Creates matrix with packages that are input list lst
###              List elements are vectors with the package name,
###              its version and the parent.  Unlist the input lst
###              and form a matrix, which every row the package name, version, parent  
###              and type of dependency (depends, imports, suggests).  
###              
### INPUT: list lst that is the output of function reverseLinks
###        Each element is a package and the value is the version and parent
###        
### OUTPUT: Dependency matrix similar to available.packages. 
###         Rows for every package; columns the name, version, parent
###         and type of dependency from parent description.
###
### USE: mat <- listomat( res <- reverseLinks(res <- makeLinks(package)))
###
###
### AUTHOR Elena Villalon
###        evillalon@iq.harvard.edu
###
###        Last modified: 01/11/2007
###
listomat <- function(lst){
  
  rw  <-  length(lst)
###  print(lst)
  if(length(lst) <=0)
    stop(paste("No leaves for ", lst)) 
  
  if(length(lst) <= 1){
    mat <- as.matrix(lst[[1]])
    mat <- t(mat)
  }else {  
    cl  <- length(lst[[1]])
    mat <- matrix(unlist(lst), ncol=cl, byrow=T)
  }
 
  colnames(mat) <- c("Package", "Version", "Node", "URL", "Depth")
  matdepth <- mat[,5]
  maturl <- mat[, 4]
  mat <- mat[,1:3 ]

  nm  <- names(lst[[1]])
  nm  <- c(nm, "Relation")
  nm[1] <- "Package"
  vec <- sapply(lst, function(m) names(m)[1])

  if(length(dim(mat)) > 0)
    mat <- cbind(mat, Relation=vec, URL=maturl, depth=matdepth)
  else{
    names(vec) <- NULL
    mat <- c(mat,  Relation=vec, URL=maturl, depth=matdepth)
  }
   mat <- clean.sweep(mat)
 
  return(mat)
}

###DESCRIPTION: Utility function to remove trailing white spaces

 clean.sweep <- function(mat){
    if(length(dim(mat))>0){
      nm <- colnames(mat)
      mat <- apply(mat,2, function(cl) {sapply(cl, FUN="trim.blanks")})
      rownames(mat) <- mat[,"Package"]
      colnames(mat) <- nm
      return(mat)
    }
      nm <- names(mat) 
      mat <- unlist(sapply(mat,FUN="trim.blanks"))
      names(mat) <- nm
    return(mat)
  }
  
### DESCRIPTION: Utility function to check the results of applying grep
###              grep may not get the exact full name but uses a loose
###              regex to get all names that contains the input words
###              For example grep("abc", c("abc", "pab", "dabcm", "clr, "abc""))
###              will return 1, 3, 5. This function eliminates 3, counting characters
###              NOTE: no need to apply this function if you use grep with extended=TRUE
###              grep("^describe$", c("describe", "le describe", "desc", "describeport"), extended=T)
###              gets only [1] 1
###
### NOTE: match will get the exact full string and will dismiss anything
###       that is not an exact match: match("abc", "pabcm")=NA; however,
###       it only finds the first occurance,
###       i.e. match("abc",c("pabcqr", "abc", "lmn","vabc","abc"))= 2
###       Same as grep("^abc$", c("pabcqr", "abc", "lmn","vabc","abc"), extended=T)
###  
### USES:        grep
###              
### INPUT:  matinst <- grep.exact(); outcome a vector of character
###         we want to check for correctness.
###         input is another vector of the strings
###         that need to be found in the outcome.  
###        
### OUTPUT: It checks that outcome and input contain the same values 
###         and eliminate those that are not exact match between outcome and input.
###         Returns outcome with all not exact matches eliminated; and
###         index ix of the strins that were eliminated from the
###         original outcome.
###         
### AUTHOR Elena Villalon
###        evillalon@iq.harvard.edu
###
###        Last modified: 01/24/2007
### 
  grep.exact <- function(outcome, input){
   ind <- 1:length(outcome) 
   names(ind) <- outcome
     
   ix <- sapply(ind, function(n){
     ret <- NULL
     if(length(outcome[n]) <= 0)
       return(ret)
     nm  <- trim.blanks(outcome[n])
     pkg <- trim.blanks(input[n])
    
    if(nchar(nm) != nchar(pkg))
      ret <- n
    return(ret)})
   ix <- unlist(ix)
   if(length(ix) > 0)
    outcome <- outcome[-ix]
   lst <- list(list(outcome=outcome), list(index=ix))
   return(lst)
 }
### DESCRIPTION: Utility function to create a matrix with packages
###              that are in one of the describe.#.R functions of Zelig
###              Columns are package name, version, R or Zelig dependency,
###              relation and URL 
###
### USES:        packageDescription 
###              
### INPUT:  vector with packages names
###         cran is string with the URL of the pkg obtained from describe.# 
###        
### OUTPUT: Matrix of dependencies and versions. 
###         
### AUTHOR Elena Villalon
###        evillalon@iq.harvard.edu
###
###        Last modified: 01/31/2007
###
pkgZeligDescribe <- function(pkg,cran){
  
    pkg <- unlist(suppressWarnings(sapply(pkg, FUN="trim.blanks")))
    zelig <- pkg[length(pkg)]
  #####R Version under which packages in pkg were built ##### 
    pkgV <- sapply(pkg, function(p){
      desc <- packageDescription(p)$Version})
    
    pkgV <- unlist(pkgV)
    pkgUrl <- sapply(pkg, function(p){
    
      url <- packageDescription(p)$URL
      url <- trim.blanks(url)
      if(length(url) <= 0)
        url <- "CRAN"
      else if(is.na(url) || identical(url,"") )
        url <- "CRAN"
      return(url)
    })
    pkgUrl.old <- unlist(pkgUrl)
    ln <- length(pkgUrl.old)
    
### as per Ferdi's last advised (02/15/2007) use information 
### in the describe.#.R funcs for the URL of packages
    zeligurl <- pkgUrl.old[ln]
    pkgUrl <- c(cran, Zelig=zeligurl)
   
    pkgUrl[is.na(pkgUrl)] <- "CRAN"
    
    matadd <- matrix(NA, nrow=length(pkg), ncol=5)
   
    for(n in 1:length(pkg)){
     
      vec <- pkg[n]###1st col: package name
      vec <- c(vec,pkgV[n])###2nd col: package version
      if(n == nrow(matadd)){
        rdep <- packageDescription(pkg[n])$Built 
        rdep <- strsplit(sub("(R[-0-9 .]*)","\\1", rdep), ";")[[1]][1]
        vec <- c(vec, rdep)
      }else
      vec <- c(vec,zelig)### 3rd col: Zelig
      vec <- c(vec,"depends")### 4th col: relation
      vec <- c(vec,pkgUrl[n]) ### 5th col: URL
      if(length(vec) > 0)
        matadd[n,] <- vec
    
    }
    
    rownames(matadd) <- pkg
### end of R version######
   
    colnames(matadd) <- c("Package", "Version", "Node", "Relation", "URL")
   
 
    return(matadd)
}
### DESCRIPTION first level dependencies (depends, imports, suggests)
###             of Zelig as obtained from packageDescription, no recursion
###
### USES: readDescription
###
### INPUT: name of package, and boolean keep.builtin to discard or
###        not to discard base packages that comes with R version
###        baseOrRecPkgs vector of packages not included in output
###        norepeat only includes packages once for given node
###        (see makeLinks)
###
### OUTPUT: matrix with information of first level dependencies
###
ZeligfstLevel <- function(pkg0, keep.builtin,baseOrRecPkgs, norepeat){

  fstlevel <- readDescription(pkg0,norepeat)
  pkgfst   <- unlist(fstlevel$totdepend)
  fstnames <- names(pkgfst)
  urlfst   <- unlist(fstlevel$url)
  names(urlfst) <- fstnames
  fstvers <- fstlevel$totversion
  names(fstvers) <- pkgfst
  matdev <- NULL
  if(length(pkgfst) <= 0) return(matdev)
  
  if(length(baseOrRecPkgs) <= 0){
    if(!is.logical(keep.builtin))
      baseOrRecPkgs <- rownames(installed.packages(priority = keep.builtin))
    else if(!keep.builtin) 
      baseOrRecPkgs <- rownames(installed.packages(priority = "base"))
    baseOrRecPkgs <- getRidVersion(baseOrRecPkgs)
  }
  keep.builtin <- ifelse(length(baseOrRecPkgs) >0, FALSE, TRUE)
    
  pkgfst  <- pkgfst[!(pkgfst %in% baseOrRecPkgs)]
  if(length(pkgfst) <= 0) return(matdev)
  fstnames <- names(pkgfst)
  fstvers <- fstvers[!(names(fstvers) %in% baseOrRecPkgs)]
  urlfst <- urlfst[!(names(urlfst) %in%  baseOrRecPkgs)]
  
  matdev <- matrix(NA, nrow=length(pkgfst), ncol=5)
  colnames(matdev) <- c("Package", "Version", "Node", "Relation", "URL")
  for(m in (1:length(pkgfst)))
    matdev[m, ] <-  c(pkgfst[m],fstvers[m], pkg0, fstnames[m],urlfst[m] )
    
  rownames(matdev) <- pkgfst
  
  return(matdev)              
}
### DESCRIPTION Helper function. If the package have attached the version number
###             it removes them.Example, "YourCast_2.9-8" becomes "YourCast"
###
getRidVersion <- function(zmat, oneonly=FALSE){
 nm <- NULL
 
 if(oneonly && length(grep("_", zmat)) <= 0)
   return(zmat)
 else if(oneonly){
   nm <- sub("(.*)_([-0-9.]*)","\\1", zmat)
   nm <- trim.blanks(nm)
   return(nm)
 }

   if(length(dim(zmat)) <= 0 || dim(zmat)[1] <=1){
   pkginst <- zmat["Package"]
  
 }else{
   pkginst <- zmat[,"Package"]
 }
 
 pkginst <- sapply(pkginst, function(nm){
   if(length(grep("_", nm)) <= 0)
     return(nm)
   nm <- sub("(.*)_([-0-9.]*)","\\1", nm)
   nm <- trim.blanks(nm)
 })

 pkginst <- unlist(pkginst)
 if(length(dim(zmat)) <= 0|| dim(zmat)[1] <=1){
   zmat["Package"] <- pkginst
  
 }else{

   zmat[,"Package"] <- pkginst
   rownames(zmat) <-  zmat[,"Package"]
  
  
 }
 return(zmat)
}



###   
### 
### DESCRIPTION: Returns/Creates matrix of details corresponding to packages
###              Zelig depends on directly or indirectly and are installed
###              in the server where Zelig runs.
###              Level of dependency is depends, imports, suggests.
###              Every row describes  a dependent package with the name,
###              the version, the parent package and the relation to the parent.
###
### USES:        makeLinks,reverseLinks,listomat,zeligDescribeModels,ZeligfstLevel
###              
### USE:         mat <- create.zelig.all.packages(pkg)
###              Even when this function is specifically written to return
###              the matrix of Zelig dependencies, it may be used for
###              any other packages.  
###
### INPUT:       the package name pkg, boolean zdescribe (default true); 
###              if it is Zelig and zdescribe=T, then calls zeligDescribeModels
###              that returns a list of packages;
###              keep.builtin where to stop the recursion.  If false it stops
###              for packages that are installed in the machine at priority="base",
###              if true it will continue recursively to all dependencies levels.
###              If keepbuiltin is a character string then it will set priority
###              of installed.packages to the string value of keep.builtin.  
###              norepeat boolean indicating if from a given node the descendants
###              are repeated if they appear simultaneously in, depends, suggests, imports.
###              showurl, boolean to display the urls as in packageDescription or not.    
###
### OUTPUT: Dependency matrix similar to available.packages. 
###         Rows for each package that Zelig depends;
###         columns the name, version, parent
###         and type of relation (dependency) from parent description. 
###         
### AUTHOR Elena Villalon
###        evillalon@iq.harvard.edu
###
###        Last modified: 01/12/2007
###
create.zelig.all.packages <- function(pkg="Zelig", zdescribe=TRUE, keep.builtin = FALSE,
                                      norepeat=FALSE, showurl=FALSE)
{
 ### pkg <- trim.blanks(pkg)
  pkg0 <- pkg
  baseOrRecPkgs <- NULL
  matzelig <- NULL
  
  
  if(length(baseOrRecPkgs) <= 0 ){
    if(!is.logical(keep.builtin))
      baseOrRecPkgs <- rownames(installed.packages(priority = keep.builtin))
    else if (!keep.builtin) 
      baseOrRecPkgs <- rownames(installed.packages(priority = "base"))
    baseOrRecPkgs <- getRidVersion( baseOrRecPkgs)
  }
  keep.builtin <- ifelse(length(baseOrRecPkgs) >0, FALSE, TRUE)
  
###specific to Zelig: include all models in functions describe.R
  cran <- NULL
  if(identical(pkg, "Zelig") && zdescribe){
    zpack <- zeligDescribeModels(PKG=pkg, baseOrRecPkgs)
    pkg <- c(zpack$model, pkg)
    cran <- c(zpack$url, cran)
  }
###specific to Zelig:include first level dependencies of Zelig
  
  if(identical(pkg0, "Zelig")){
    pkgz <- NULL
    matzelig <- ZeligfstLevel(pkg0,keep.builtin, baseOrRecPkgs,norepeat)
 
    if(length(matzelig) > 0)
      pkgz <- rownames(matzelig)
    matadd <- pkgZeligDescribe(pkg,cran)
    lnz <- dim(matzelig)
    lnz <- ifelse(length(lnz) > 0,lnz[1],1)
    if(!length(matzelig)) lnz <- 0
  
    lnf <- dim(matadd)
    lnf <- ifelse(length(lnf) > 0,lnf[1],1)
    if(!length(matadd)) lnf <- 0
  
    matzelig <- rbind(matzelig, matadd)
    clnm <- colnames(matzelig)
    vlevz <- NULL
    vlevf <- NULL
    if(lnz) vlevz <- rep("1", lnz)
    if(lnf) vlevf <- rep("1z",lnf) 
    depth <- c(vlevz, vlevf)
   
    matzelig <- cbind(matzelig, depth)
 
    ind <- match("Zelig", matzelig[,1])
    if(!is.na(ind)) matzelig[ind, "depth"] <- "0"
    pkg <- pkg[-length(pkg)]
    pkg <- c(pkg, pkgz)
  }

 
### valid for Zelig or any other package
  
  res <- makeLinks(pkg,keep.builtin, baseOrRecPkgs,norepeat)


  if(length(res$edges) <= 0)
    return(matzelig)
  edge <- res$edges
  vers <- res$vers
  
   ### edge is a list with the direct dependencies of Zelig 
   ### and any of the packages derived from Zelig dependencies
   ### edge corresponds to nodes and the edges in graph theory
   ### The list names are the nodes or packages names.
   ### The list elements are the direct dependencies of the nodes  
   ### vers is the same list as edge but instead of
   ### of the package names it has their versions
   ### or NA if not provided in the description of each package
   ### The first list element of vers is the R version, the rest
   ### of the elements map one to one with those in edge.  
   ### URL are the websites to get the packages as in package Description
   ### depth the level of graphs (or dependencies) with root starting at 0

   lst <- reverseLinks(res)

###   print(lst)
   ### it is a list with every element a package and
   ### the value is the name, version, parent, and relation to parent
   if(length(unlist(lst)) <= 0)
     return(matzelig)
   
   mat <- listomat(lst)
   add <- F

   if(length(dim(mat)) <= 0){ ### only one row
     nmadd <- mat[1]
     add <- T
   }
   ### converts the list return with reverseLinks into a matrix
   ### colnames(mmm) =  
   ### "Package"  "Version"  "Node"   "Relation", "URL", "depth"
  pkg <- sapply(pkg,FUN="trim.blanks")

  
  if(identical(pkg0, "Zelig")){
    if(length(dim(mat)) > 0)
      mat[,"depth"] <- as.numeric(mat[,"depth"]) + 1
    else
      mat["depth"] <- as.numeric(mat["depth"]) + 1
  }
 
  mat <- rbind(mat, matzelig)
  
  if(add)
    rownames(mat)[1] <- nmadd
  ix <- match("URL", colnames(mat))
  
  if(!showurl && !identical(pkg0,"Zelig")) 
    mat <- mat[,-ix]
  
  if(!showurl &&identical(pkg0,"Zelig") ){
    ixz <- grep("^1z$", mat[, "depth"], extended=T)

    if (length(ixz) >0){
      nr <- nrow(mat[-ixz, ])
      mat[-ixz, "URL"] <-  rep("CRAN", nr)
   } else
      mat <- mat[, -ix]
               
  }
  if(!identical(pkg0,"Zelig")){
    vec <- addzerodepth(pkg0,colnames(mat))
    rnm <- rownames(mat)
    mat <- rbind(mat, vec)
    nm <- mat[,"Package"]
    rownames(mat) <- nm
  }
 
    
   return(mat)
}

###DESCRIPTION : If no Zelig add a bottom row with the root pkg information
 addzerodepth <- function(pkg,cols){
    pkg <- trim.blanks(pkg)
    pkg0 <- pkg
    if(length(grep("_", pkg)) > 0){
      pkg <- sub("(.*)_([-0-9.]*)","\\1", pkg0)
      pkg <- trim.blanks(pkg)
    }
    lst <- try(packageDescription(pkg), silent=T)
    if(class(lst) =="try-error")
      lst <-  try(packageDescription(pkg0), silent=T)
    if(class(lst) =="try-error")
      return(list())
    rdep <- lst$Built 
    rdep <- strsplit(sub("(R[-0-9 .]*)","\\1", rdep), ";")[[1]][1]
    vec <- c(pkg,lst$Version, rdep, "depends")
    if(length(cols) > 5)
      vec <- c(vec, "CRAN","0")
    else
      vec <- c(vec, "0")
    names(vec) <- cols
    return(vec)
}
### DESCRIPTION Apply the function create.zelig.all.packages
###             to all packages in the system as obtained from 
###             mat <-installed.packages().
###             Gets pkgs names as mat[,1]
###
### USES installed.packages,create.zelig.all.packages
###
### OUTPUT a list with matrices, each matrix has the dependencies
###        of the packages in the system. Eliminates erros. 
###
### 02/14/2007
###
testall <- function(prior=NULL){
  localinst <- installed.packages(prior)
  str <-  " "
  res <- lapply(localinst[,1], function(nm) {
  res0 <- NULL
  res <- try(create.zelig.all.packages(nm))
  if(class(res)!="try-error")
    return(res)
  else{
    message("Error pkg ", nm)
    str <- paste(str, nm)
    
    return(res0)
  }})
  
  return(invisible(res))
}
### DESCRIPTION:Takes the list output of testall and finds the dimensions
###             of all the matrix elements in the list,e.g. rows and columns

dimtestall <- function(lst=NULL){
  if (length(lst) <= 0)
    lst <- testall()
  ret <- lapply(lst, dim)
}
ind.null <- function(d=NULL){
  if(length(d) <= 0)
    d <- dimtestall(outp <- testall())
  ix <- sapply(d,is.null)
  return(ix)
}

### DESCRIPTION: Same as testall but the list element output for pkgs
###              with errors messages is the name of the package
###              instead of NULL for that element.
###              If erroronly=T, then it calls
###              testerror(lst <- testinst(erroronly=T))
###
testinst <- function(prior=NULL, erroronly=F){
  localinst <- installed.packages(prior)
  res <- lapply(localinst[,1], function(nm) {
  res0 <- nm
  print(nm)
  res <- try(create.zelig.all.packages(nm))
  if(class(res)!="try-error")
    return(res)
  else{
    message("Error pkg ", nm)
    return(res0)
  }})
}
### DESCRIPTION: Uses the list output of testinst and eliminates
###              packages that do not produce errors' messages
###              It returns the list of packages with errors.
###              The errors may be due to packages in
###              baseOrRecPkgs <- rownames(installed.packages(priority = "base"))
###
testerror <- function(lst=NULL){
  if(length(lst) <= 0)
    lst <- testinst()
  error <- sapply(lst, function(mat) {
    ret <- NULL
    if(length(mat) == 1)
      return(mat)
    else
      return(ret)})
  error <- unlist(error)
  return(error)
  
}
### Eliminates null values from the list=lst
### lst <- testall()
getNull <- function(lst=NULL){
  if(!length(lst))
    lst <- testall()
  
  ixnull <- unlist(sapply(lst,is.null))
  if(length(ixnull)){
    print(names(lst)[ixnull])
    lst <- lst[!ixnull]
  }
  return(lst)
  
}
checkspaces <- function(allpkg){  
  res <- lapply(allpkg, function(mat) {
    rw <- rownames(mat)
    spc <- sapply(rw, function(nm){ grep("[[:space:]]", nm, extended=T)})
    spc <- unlist(spc)
  })
return(res)
}
### err <-  testerror(outp <- testinst())
###
###       "base"        "boot"       "class"     "cluster"    "datasets" 
###    "foreign"    "graphics"   "grDevices"        "grid" "httpRequest" 
### "KernSmooth"     "lattice"        "MASS"     "methods"        "mgcv" 
###       "nlme"        "nnet"       "rpart"     "spatial"     "splines" 
###      "stats"      "stats4"    "survival"       "tcltk"       "tools" 
###      "utils"
###
### Total : 26 packages, with 25 of them in  baseOrRecPkgs.  
### Note: All errors pkgs are in baseOrRecPkgs <- rownames(installed.packages(priority = "base"))
### except for "httpRequest", with fields
### packageDescription("httpRequest")$Depends, Imports, Suggests = NULL
### > dim(zmatrecm)= 21  6
### > dim(zmat)= 18  6
### > dim(zmatbase)= 18  6
### > dim(zmatnull)= 50  6
### > dim(zmathigh)= 9 6
### > dim(zmatNA) = 38  6


