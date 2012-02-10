# Zelig 4

Zelig 4 is a complete rewrite of the Zelig software. Its intention is to
maintain complete backwards compatability from the End-user experience, while
prodividing developers a new, streamlined API to author new models in.


## How to Install

There are several methods for installation.


### Pre-install Checklist

* A major operating system (Mac OS X 10.4+, Windows or Linux/Unix-alike)
* [R (>= 2.14.1)](http://cran.r-project.org/)


### Setup-Wizard Install

Zelig has an online install option! Using this method, users have the option to
selectively install Zelig packages or install everything via one simple command.
This install script can be found at: http://r.iq.harvard.edu/install_live.R

To load the installer, simply follow these directions:

1. Install the latest version of R at http://cran.r-project.org/
2. Open an R-session
3. Enter the following into an R-session: 
```source("http://r.iq.harvard.edu/install_live.R")```



### Manual Install

In addition to the provided setup wizard, Zelig can be installed manually on a
package-by-package basis. To do this, simply use the ```install.packages```
function:

```R
install.packages(
                 "Zelig",
                 repos = "http://r.iq.harvard.edu/",
                 type  = "source"
                 )
```

Packages can be individually installed using a similar method to the one above:

```R
install.packages(
                 "<PACKAGE NAME>",
                 repos = "http://r.iq.harvard.edu/",
                 type  = "source"
                 )
```

Where "<PACKAGE NAME>" is the name of an available Zelig package. A complete list can be found below.



## Available Packages

* Zelig
  * The core package of Zelig.
  * [Source Code](https://github.com/zeligdev/Zelig)
  * [Issue Queue](https://github.com/zeligdev/Zelig/issues)
  * Contains: gamma, logit, ls, negbinom, normal, poisson, probit
* ZeligMultivariate
  * A package containing Zelig's Multivariate Regression offerings.
  * [Source Code](https://github.com/zeligdev/ZeligMultivariate)
  * [Issue Queue](https://github.com/zeligdev/ZeligMultivariate/issues)
  * Contains: ```blogit```, ```bprobit```
* ZeligGAM
  * Generalized Additive Models for Zelig
  * [Source Code]()
  * [Issue Queue]()
  * Contains: 
* Zelig
  * 
  * [Source Code]()
  * [Issue Queue]()
  * Contains: 



## Latest Changes


## Future Goals


