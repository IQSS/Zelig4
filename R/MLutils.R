###
## Take a formula in any of the reduced form or in a structural form
## and returns the most reduced form of that formula
## 

reduceMI <-function(f){
        if(class(f)=="list")
          f <- structuralToReduced(f)
        return(.reduceFurther(f))
}


##
# Transform the multilevel's structural form formulas into
# reduced form
# input: formula in structural form ( a list of formulas)
# output: formula in reduced form
# possible bugs: What if class(f) is multiple and not list??

structuralToReduced <- function(f){

        ## input should be a list
        if(class(f) != "list" || (class(f)=="list" && length(f)<2))
          stop("the input should be a list of formulas")

        ## take the first formula; It should be of length 3
        main.fml <- f[[1]]
        if(length(main.fml)!=3)
          stop("the main formula in the extended form should be of length 3 !")
        lhs<- main.fml[[2]]
        TT <- terms(main.fml,specials="tag")
        TT.labels <- attr(TT,"term.labels")
        TT.vars <- attr(TT,"variables")
        tagattr<-attr(TT,"specials")$tag
        hastag<-!(is.null(tagattr))
        if (hastag){
                for(j in tagattr){
                        lind<-j-1
                        vind<-j+1
                        tg<- .deparseTag(TT.vars[[vind]])
                        whicheq<-which(names(f) %in% tg$label)
                        if (length(whicheq)!=0)
                          tg$label<-deparse(f[[whicheq]][[2]])
                        else
                          stop("one of the equation's name is expected to be ",tg$label)
                        TT.labels[[lind]]<-.newTag(tg)
                        res<-(as.formula(paste(lhs,"~",paste(TT.labels,collapse="+"))))
                }
        } else
        stop("tag is missing in the first equation\n")
        return(res)
}

###
##  take a formula in the reduced form and return it in
##  lmer representation (basically remove starting "tag"
##  of each term)
##  possible errors: What if input is not in reduced form?
##                   Maybe call reduceMI first??

tolmerFormat<-function(f){
        lhs <- f[[2]]
        tt <- terms(f, specials="tag")
        tt.labels<-attr(tt,"term.labels")
        for (i in 1:length(tt.labels)){
                tt.labels[[i]]<-.trim(tt.labels[[i]])
                tt.labels[[i]]<-gsub('^tag',"",tt.labels[[i]])
        }
        rhs <- paste(tt.labels,collapse="+")
        res <- as.formula(paste(lhs,"~",rhs,sep=""))
        return(res)
}


###
## given a formula in a reduced from, output the most reduced one
##

.reduceFurther <- function(f){
        
        if(length(f)!=3)
          stop("the main formula in the extended form should be of length 3 !")
        lhs<- f[[2]]
        TT <- terms(f,specials="tag")
        TT.labels <- attr(TT,"term.labels")
        TT.vars <- attr(TT,"variables")
        tagattr<-attr(TT,"specials")$tag
        hastag<-!(is.null(tagattr))
        lstOfTags<-c()
        if (hastag){
                for(j in tagattr){
                        vind<-j+1
                        lstOfTags<-c(lstOfTags,.expandTag(.deparseTag(TT.vars[[vind]])))
                }
        } else
        stop("tag is missing in the first equation\n")
        tmp <- paste(lstOfTags,collapse="+")
        rhs<-paste(.replace(TT.labels,tagattr-1,tmp),collapse="+" )
        return(as.formula(paste(lhs,"~",rhs,sep="")))
}

###
## Helper function which takes the term with tag and return
## all its parts
##
## input: a tag like call/list i.e. tag(z1,w1+w2 | state)
## output: list(var= "z1", label="w1 + w2", id="state")

.deparseTag <- function(f){

        f <- as.character(f)
        res<-list()
        if(length(f) == 3){
                ## tag(var,label|id) or tag(var,label)
                res$var <- f[[2]]
                tmp <- .trim(unlist(strsplit(f[[3]],"|",fixed=TRUE)))
                if(length(tmp) == 2){
                        ## tag(var,label|id)
                        res$label <- tmp[[1]]
                        res$id <- tmp[[2]]
                }else{
                        ## tag(var,label)
                        if(length(tmp)==1){
                                res$label <-tmp[[1]]
                                res$id <- "none"
                        }else
                        stop("wrong use of tag function!!")
                }
                
        } else {
                ## tag(var|id)
                tmp <- .trim(unlist(strsplit(f[[2]],"|",fixed=TRUE)))
                res$var <- tmp[[1]]
                res$id <- tmp[[2]]
                res$label="none"
        }  
        return(res)
}

###
## takes the output from .deparseTag (a list) and construct a new tag
## as a string 
## i.e. takes list(var="z",label="w1",id="state") and output "tag(z,w1|state)

.newTag <- function(lst){

        res <- "tag("
        if (lst$var != "none")                        # must have var
          res <- paste(res,lst$var,sep="")
        else
          stop("wrong use of tag(); variable is missing")
        if (lst$label !="none"){                      ## tag(z,gamma??)
                res <-paste(res,",",sep="")
                res <- paste(res,lst$label,sep="")
                if(lst$id != "none")                  ## tag(z,gamma|state)
                  res <- paste(res,"|",lst$id,sep="")
        }else{                                        ## tag(z|state)
                res <-paste(res,"|",sep="")
                if(lst$id !="none")
                  res <- paste(res,lst$id,sep="")
                else
                  stop("wrong use of tab")  # tag(x |)
        }
        res <- paste(res,")",sep = "")
        return(res)
}


###
## expands tag.  tag(1,w1+w2 | state) => tag(w1|state) + tag(w2|state)
##               tag(z,w1+w2 | state) => tag(z:w1|state)+ tag(z:w2|state)
## input tag as a list; i.e the output from .deparseTag



.expandTag <- function(l){

        if(l$var == "1" && l$label!="none"){
                ## tag(1,z1 | state) == tag (z1|state)
                l$var <- l$label
                l$label <- "none"
          
        }
        if(l$label =="none"){
                ## tag(1+z1|state)
                vars<-unlist(strsplit(l$var,"+", fixed=TRUE))
        }else{
                ## tag(z1,w1+w2|state)
                vars<-unlist(strsplit(l$label,"+", fixed=TRUE))
        }
        if(length(vars) == 1){
                ## nothing to expand
                return (.newTag(l))
        }else{
                alltgs<-c()
                for(i in 1:length(vars)){
                        if(l$label == "none")
                          alltgs <- c(alltgs,.newTag(list(label="none",var=vars[[i]],id=l$id)))
                        else
                          alltgs <- c(alltgs,.newTag(list(label="none",var=paste(l$var,":",vars[[i]],sep=""),id=l$id)))
                        
                }
        }
        return (paste(alltgs,collapse="+"))

}

###
## In the vector 'src' replace the element in the position
## 'index' with elementSSS in vector 'dest'

.replace<-function(src,index,dest){
        "%w/o%" <- function(x,y) x[!x %in% y]
return (c(src %w/o% src[index],dest))
        
        if(1==2){
                if(index <1 || index > length(src))
                  stop("wrong index arguemnt in function .replace")
                if(index==1)
                  beforeEls<-c()
                else
                  beforeEls<-src[1:(index-1)]
                
                if(index == length(src))
                  afterEls<-c()
                else
                  afterEls<-src[(index+1):length(src)]
                
                return(c(beforeEls,dest,afterEls))
        }
        
}


##
# Trim the word's white spaces
# input : one word string s
# output: trimed version of s

.trim <-function(v){
        for(i in 1:length(v)){
        v[[i]] <- gsub('^[[:space:]]+', '', v[[i]])
       v[[i]]<- gsub('[[:space:]]+$', '', v[[i]])
}
        return(v)
}

##
#   Reaction ~ Days + tag(1 + Days | subject) ==>
#   list (fixed = ~ Days,
#         random = ~ 1 + Days)
#

.getRandAndFixedTerms <- function (fml){
        f <- function(x){
                as.formula(paste("~",paste(x, collapse = "+")))
        }
        res <- list()
        if(length(fml)!=3)
          stop("the main formula in the extended form should be of length 3 !")
        lhs <- fml[[2]]
        rhs <- fml[[3]]
        TT <- terms(fml,specials="tag")
        TT.labels <- attr(TT,"term.labels")
        TT.vars <- attr(TT,"variables")
        tagattr<-attr(TT,"specials")$tag

        hastag<-!(is.null(tagattr))

        if (hastag){
                ## fixed
                F.labels <- TT.labels[-(tagattr-1)]
                if (!length(F.labels))
                  F.labels <- 1
                res$fixed <- as.formula(paste("~",paste(F.labels,collapse="+")))

                ## random
                random <- list()
                idx = 1
                for (j in tagattr){
                        vind <- j + 1
                        tmp <- .deparseTag(TT.vars[[vind]])
                        idx <- idx + 1
                        ## if tags have the same id, merge them together
                        if (tmp$id %in% names(random)){
                            random[[tmp$id]] <- c(random[[tmp$id]], tmp$var)    
                        } else {
                                random[[tmp$id]] <- tmp$var
                        }
                }
                res$random <- lapply(random,f)
                
        } else {
                res$fixed <- fml
        }
        return(res)
}

