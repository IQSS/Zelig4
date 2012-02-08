library(Zelig)

# A complete list of officially supported models
models <- c("gamma", "logit", "ls", "negbinom", "normal", "poisson", "probit", 
            "blogit", "bprobit", "logit.gam", "normal.gam", "poisson.gam", 
            "probit.gam", "gamma.gee", "logit.gee", "normal.gee", "poisson.gee",
            "probit.gee", "gamma.mixed", "logit.mixed", "ls.mixed", 
            "normal.mixed", "poisson.mixed", "probit.mixed", "mlogit", 
            "mprobit", "ologit", "oprobit", "gamma.survey", "logit.survey", 
            "normal.survey", "poisson.survey", "probit.survey", "factor.bayes", 
            "logit.bayes", "mlogit.bayes", "normal.bayes", "oprobit.bayes", 
            "poisson.bayes", "probit.bayes", "aov", "sur", "twosls", "threesls",
            "cloglog.net", "gamma.net", "logit.net", "ls.net", "negbinom.net", 
            "normal.net", "poisson.net", "probit.net")


for (m in models)
  cat(sprintf("%s in %s\n", m, Zelig:::get.package(m)))


Zelig:::get.package("fake package")
Zelig:::get.package(1)
Zelig:::get.package(list(x=1))
Zelig:::get.package(c("x", "logit"))
Zelig:::get.package("probit.net")
Zelig:::get.package(NULL)
