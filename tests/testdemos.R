## This is a script to test all the demos in the zelig's demo directory.
## It opens a connection to each demo file,manipulate the script (i.e.
## commenting 'user.prompt()' etc. and then run it
##
## NOTE: The idea is to make this process as silent as possible.
## invisible() will work for model's output but not for some R messages
## (i.e. require(VGAM) or library(VGAM) will still give some warnnings)
## To avoid those warnings one can use library(VGAM,warn.conflicts=FALSE)
## or require(VGAM, warn.conflicts=FALSE) This could be fixed when we
## change zelig2* functions. Which means that we are not going to use
## require(...) in each zelig2* but in one central function which gets
## the info. on which packages to load by looking at describe.mymodel
## function.
## 03/23/2007 - look at the package "seesion"; maybe smth interesting..?
##
## some helping functions

"%w/o%" <- function(x,y) x[!x %in% y]

## get the path of zelig's demo directory
zpath<- system.file(package="Zelig")
zpath<-paste(zpath,"/demo/",sep="")

## get the file list of all demos
demoFiles<- list.files(zpath,pattern="\\.[RrSsQq]$")

## Keep a list of demos which failed
demoFail <- c()

require(Zelig)

## process each demo
##for(demo in c("ls.R","blogit.R","ei.dynamic.R")){
for(demo in demoFiles){
        ## list of attached packages before this demo is run
        beforePkgs <- search()
        beforeLs <- ls()
        print(demo)

        ## if file does not exists, we dont have demo for this file
        ## so add it to the list of non-working demos and go to the
        ## next demo
        if (!file.exists(paste(zpath,demo,sep=""))){
                demoFail <- c(demoFail, demo)
                next
        }
        
        ## open a connection to the file and read all the lines
        con <- file(paste(zpath,demo,sep=""),"r")
        lines<- readLines(con)

        ## find all the lines starting with zero or more spaces
        ## then 'user.prompt()' and comment them out
        newlines<-gsub('^ *user',"##user",lines)

        ## try to run the edited demo. "invisible" is uded to make it quiet
        res=try(invisible(capture.output(eval(parse(text=newlines)))))
        if (inherits(res,"try-error")) {
                demoFail <- c(demoFail,demo)
        }
        
        ## close the connection
        close(con)

        ## running this demo may have attached some
        ## packages. Let's detach them
        afterPkgs <- search()
        pkgToDetach <- afterPkgs %w/o% beforePkgs
        for ( pkg in pkgToDetach){
                ## you cant use simple detach(pkg) here
                ## see examples on ?detach 
                detach(pos=match(pkg,search()))
        }
        afterLs <-ls()

        ## Clean up all the objects
        rm(list = (afterLs %w/o% beforeLs))
        
}

## just print the list of demos which failed
if (length(demoFail) > 0){
        cat ("The following demos failed:\n")
        print (demoFail)
} else {
        cat ("SUCCESS, all demos run sucessfully \n")
}
