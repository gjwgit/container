\name{container-package}
\alias{container-package}
\alias{container}
\docType{package}
\title{

  Define reference classes for datasets and models for data analytics.
  
}
\description{
  
  The RefClass system is used to define a collection of reference
  classes to encapsulate the objects dealt with in data analytics. At
  the highest level this includes Dataset and Model. Classifier,
  Regressor, and Cluster extend Model. Then, for example, Rpart extends
  Classifier.

}
\details{
  \tabular{ll}{
    
    Package: \tab container\cr
    Type: \tab Package\cr
    Version: \tab 1.0\cr
    Date: \tab 2012-04-22\cr
    License: \tab GPL (>=3)\cr
  }

  Instances of a container are created using, for example, Dataset$new.
  
}
\author{

  Graham Williams (Togaware)

  Maintainer: Graham Williams <Graham.Williams@togaware.com>

}
\keyword{ package }
\examples{
irisDS <- Dataset$new(data=iris, target="Species", ignore="Sepal.Width")
irisDS
}
