.setUp <-function(){
require(Zelig)
}

###
## runit function for testing parse.formula()
##
test.parse.formula <- function(){
        ## the TRUTH object. Put all the parsed
        ## formulas here and save int the disk
        parsed<-list()

        data(mexico)
        fml1<- as.factor(vote88)~pristr + othcok + othsocok

        fml2 <-list(mu1 = import ~ coop,
                  mu2 = export ~ cost + target)

        ##parsed$fml1<-parse.formula(fml1,model="mlogit",data=mexico)#
        ##parsed$fml2<-parse.formula(fml2,model="bprobit")#
        ##save(parsed, file="test.parse.formula.RData")#
        load("test.parse.formula.RData")

        ## testing ...
        trial.fml1<-parse.formula(fml1,model="mlogit", data=mexico)
        checkEquals(trial.fml1,parsed$fml1)

        trial.fml2<-parse.formula(fml2,model="bprobit")
        checkEquals(trial.fml2,parsed$fml2)
}

###
## runit function for testing terms.multiple()
##
test.terms.multiple<-function(){

        ## list of formulas to be tested
        listMI <- list(
                       fml1 = list(mu1 = import ~ coop, mu2 = export ~ cost + target),
                       
                       fml2= list(mu    = y ~ x + tag(z, gamma| state)+ tag(z2, gamma2| state),
                         gamma =    ~ w1 + w2 + tag(w3,eta),
                         gamma2 =   ~ w1 + w2 + tag(w3,eta)),
                       
                       fml3 = y ~ x1 + x2 + tag(1 | state),
                       
                       fml4 = list(mu=y ~ x1 + x2 + tag(1, gamma | state),
                         gamma = ~ 1 + z1),
                       
                       fml5=y ~ x1 + x2 + tag(1, 1 + z1 | state),
                       
                       fml6 = list(mu= y ~ x1 + x2 + tag(1, gamma1 | state) + tag(1, gamma2 | year),
                         gamma1 = ~1,
                         gamma2 = ~1),
                       
                       fml7 = list(mu=y ~ x1 + x2 + tag(1, gamma | state),
                         gamma = ~ z1 + z2),
                       
                       fml8 = y ~ x1 + x2 + tag(z1 + z2 | state),
                       
                       fml9 = y ~ x1 + x2 + tag(1, z1 + z2 | state),
                       
                       fml10 = list(mu=y~x1 + x2 + tag(z1, gamma | state),
                         gamma = ~ w1 + w2),
                       
                       fml11 = y ~ x1 + x2 + tag(z1:w1 | state) + tag(z1:w2 | state),
                       
                       fml12 = y ~ x1 + x2 + tag(z1, w1 + w2 | state),
                       
                       fml13 = list(mu=y~x1 + x2 + tag(z1, gamma1 | state) + tag(z2, gamma2 | state),
                         gamma1 = ~ w1 + w2,
                         gamma2 = ~ w3 + w4),
                       
                       fml14 = y ~ x1 + x2 + tag(z1:w1 | state) + tag(z1:w2 | state) + tag(z2:w3 | state) + tag(z2:w4 | state),
                       
                       fml15=list(mu=y~x1 + x2 + tag(z1, gamma1 | state) + tag(z2, gamma2 | state),
                         gamma1 = ~ w1 + tag(w2,state),
                         gamma2 = ~ w3 + tag(w4,state))
                       )
        ## the TRUTH object.
        parsed<-list();
        for(i in 1:length(listMI)){
                fml<-listMI[[i]]
                objname <- paste("fml",i,sep="")
                this.terms <- Zelig:::terms.multiple(fml)
                ##parsed[[objname]]<-this.terms ##
                ##save(parsed,file="test.terms.multiple.RData" )##
                load("test.terms.multiple.RData")
                checkEquals(this.terms,parsed[[objname]])
        }
        
}



###
## runit function for testing process_as_factor()
## DEACTIVATED for the moment.
##
test.process.as.factor<-function(){
        DEACTIVATED("later...when new version of parse.formula...")
        ## lets create a dummy dataframe
        dd <- matrix(1:5,3,5)
        colnames(dd) <- c("x1", "x2", "x3", "x4", "x5")
        dd <- as.data.frame(dd)
        ## a formula
        fml <- as.factor(x1) ~ x2 + x3
        ## the result should be
        res <-list(id(x1,"2") ~ x2 + x3, 
                      id(x1,"3") ~ x2 + x3)
        ## check it
        checkEquals(Zelig:::process.as.factor(fml,dd),res)
}
