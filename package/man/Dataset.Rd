\name{Dataset}
\alias{Dataset}
\title{
  A reference class for encapsulting a dataset.
}
\description{
  DESCRIPTION
}
\usage{
Dataset$new(data, target, inputs=NULL, ignore=NULL, risk=NULL,
           weights=NULL, seed=NULL)
}
\arguments{
  \item{data}{

    The dataset set encapsulatred within the container. Once specified
    the dataset is locked---it can not be changed.
    
  }
  \item{target}{

    A character string naming the target variable. Once specified the
    target is locked---it can not be changed.
    
  }
  \item{inputs}{

    An optional character vector of names of input variables. If NULL
    then all variables within data, but excluding any specified as
    ignored, target and risk, will be identified as inputs.
    
  }
  \item{ignore}{

    An optional character vector of names of variables to ignore.
    
  }
  \item{risk}{

    An option name of a variable to be treated as a risk variable. A
    risk variable is a measure of the size or significance of the
    outcome.
    
  }
  \item{weights}{

    An optional character string representing a formula to be used to
    weight the observations.
    
  }
  \item{seed}{

    An optional numeric used to set the random number seed, useful for
    repeatbility of the dataset sampling into training and test
    datasets.
    
  }
}
\details{

  More details than the description above.

}
\value{

  A container (implemented as an environment) is created. The resulting
  container has at least the following elements:

  \item{data }{The actual dataset.}
  \item{form }{Formula for the model.}
  \item{ignore }{List of ignored variables.}
  \item{inputs }{List of model input variables.}
  \item{na.obs }{Indicies of the dataset having NAs.}
  \item{ninputs }{Number of input variables.}
  \item{nobs }{Number of observations (rows).}
  \item{numeris }{The numeric input variables.}
  \item{risk }{The name of a risk variable, if any.}
  \item{seed }{Random seed to partition the dataset, if any.}
  \item{target }{The name of the target variable.}
  \item{test }{Indicies for the test dataset (30\%).}
  \item{test.na.omit }{Indicies of complete test observations.}
  \item{train }{Indicies for the training dataset (70\%).}
  \item{train.na.omit }{Indicies of complete training observations.}
  \item{vars }{All variables for modelling, including inputs and target.}
  \item{weighted }{Indicies for a weighted training dataset.}
  \item{weights }{A string representing a weights formula, if any.}
  
}
\references{

  \url{http://onepager.togaware.com}
  
}
\author{

  Graham Williams
  
}
