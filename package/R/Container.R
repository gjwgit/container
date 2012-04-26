.initialized <-
  function(fname)
{
  "Check if a field has been initialized."
  ! inherits(fname, "uninitializedField")
}

########################################################################

Container <-
  setRefClass("Container",
              
              fields=c(
                "time.stamp"
                ),
              
              methods=list(
                #------------------------------
                size=function()
                {
                  sum(sapply(ls(.self),
                             function(e) object.size(get(e, .self))))
                }))

########################################################################

Dataset <-
  setRefClass("Dataset",
              
              fields=list(
                         
                # Required fields
                data="data.frame",
                target="character",
                
                # Optional fields
                inputs="character",
                ignore="character",
                risk="character",
                weights="numeric",
                seed="numeric",
                
                # Derived fields
                vars="character",
                ninputs="integer",
                nobs="integer",
                numerics="character",
                form="formula",
                train="integer",
                test="integer",
                na.obs="integer",
                train.na.omit="integer",
                test.na.omit="integer"
                ),
              
              contains="Container",
              
              methods=list(
                #------------------------------
                initialize=function(...)
                {
                  "Fill in all optional and derived fields."
                  
                  callSuper(...) # Setup the instance first.
                  
                  if (length(ignore) && ! length(inputs))
                    inputs <<- setdiff(names(data), c(target, risk, ignore))
                  if (! length(inputs))
                    inputs <<- setdiff(names(data), c(target, risk))
                  
                  vars <<- c(inputs, target)
                  ninputs <<- length(inputs)
                  nobs <<- nrow(data)
                  form <<- as.formula(paste(target, "~ ."))
                  if (length(seed)) set.seed(seed)
                  train <<- sample(nobs, 0.7*nobs)
                  test <<- setdiff(1:nobs, train)
                  time.stamp <<- date()
                },
                #------------------------------
                show=function()
                {
                  "Method for automtically printing the Dataset."
                  
                  cat("Dataset object of class", classLabel(class(.self)),
                      "with time stamp", time.stamp, "\n\n")

                  cat("Dimensions:", format(nobs, big.mark=","),
                      "observations and", format(ncol(data), big.mark=","),
                      "variables.\n\n")

                  cat("Target:", target, "\n\n")

                  if (length(risk)) cat("Measure of Risk:", risk, "\n\n")

                  cat(sprintf("Inputs (%d): %s\n\n", length(inputs),
                              paste(strwrap(paste(inputs, collapse=", "),
                                            exdent=13), collapse="\n")))
                  cat(sprintf("Ignore (%d): %s\n\n", length(ignore),
                              paste(strwrap(paste(ignore, collapse=", "),
                                            exdent=12), collapse="\n")))
                  cat(sprintf("Train on %s obs and test on %s obs.\n",
                              format(length(train), big.mark=","),
                              format(length(test), big.mark=",")))
                }))

# Make some fields read-only once set.

Dataset$lock("data", "target")

########################################################################

Model <-
  setRefClass("Model",
              
              fields=c(
                         
                # Required fields
                "dataset",
                
                # Calculated fields
                # "dataset.name",
                "model",
                "test.actual",
                "test.risks",
                "test.predicted",
                "build.time",
                "build.time.stamp",
                "eval.time"
                ),

              contains="Container",

              methods=list(
                #------------------------------
                build=function()
                {
                  build.time.stamp <<- date()
                },
                #------------------------------
                evaluate=function()
                {
                  test.actual <<- with(dataset, data[test, target])
                  names(test.actual) <<- dataset$test
                  if (length(dataset$risk))
                    test.risks <<- with(dataset, data[test, risk])
                  else
                    test.risks <<- NULL
                },
                #------------------------------
                initialize=function(...)
                {
                  "Fill in all optional and derived fields."
                           
                  callSuper(...) # Setup the instance first.

                  time.stamp <<- date()
                },
                #------------------------------
                show=function()
                {
                  "Method for automatically printing the Model."
                  
                  cat("Model object of class", classLabel(class(.self)),
                      "with time stamp", time.stamp, "\n\n")

                  #cat(sprintf("Dataset is '%s'.", dataset.name),
                  #    sprintf("Use 'print(%s)' for a summary.\n\n", dataset.name))
                  
                  if (! .initialized(build.time.stamp))
                    cat("The model has not yet been built.",
                        "Use '$build()' to do so.\n")
                  else
                  {
                    cat(sprintf("Model built in %s seconds with time stamp %s.\n\n",
                                round(build.time[3], 2), build.time.stamp))
                    
                    if (! .initialized(test.evaluation))
                      # cat("Evaluation on test data from", dataset.name,
                      cat("Evaluation on test data",
                          "has not been performed.",
                          "Use '$evaluate()' to do so.\n")
                    else
                      cat(sprintf("Evaluation on test data took %s seconds.\n",
                                  round(eval.time[3], 2)))
                  }
                }))

# Model$lock("dataset")

#-----------------------------------------------------------------------

Classifier <-
  setRefClass("Classifier",

              fields=c(

                # Calculated fields
                "test.probability",
                "test.evaluation"
                ),

              contains="Model",

              methods=list(
                #------------------------------
                evaluate=function()
                {
                  "Generate test dataset evaluation for the classifier."

                  eval.time <<- system.time({
                    callSuper()
                  
                    test.predicted <<- .self$predictc(with(dataset, data[test, inputs]))
                  
                    # Extract probabiliy of the right-most class. This
                    # is usually the positive class for binary classification.
                  
                    test.probability <<- .self$predictp(with(dataset,
                                                             data[test, inputs]))

                    test.probability <<- test.probability[,ncol(test.probability)]

                    suppressPackageStartupMessages(require(rattle))
                  
                    test.evaluation <<- evaluateRisk(test.probability,
                                                     test.actual,
                                                     test.risks)
                  })
                }
                ))

#------------------------------------

AdaClassifier <-
  setRefClass("AdaClassifier",

              contains="Classifier",

              methods=list(
                #------------------------------
                build=function(...)
                {
                  "Build an adaptive boosted decision tree classifier."

                  suppressPackageStartupMessages(require(ada))

                  if (length(dataset$weights))
                    build.time <<-
                      system.time(model <<-
                                  with(dataset,
                                       ada(form, data[rep(row.names(data[train,]),
                                                          as.integer(weights[train])),
                                                      vars])))
                  else
                    build.time <<-
                      system.time(model <<-
                                  with(dataset,
                                       ada(form, data[train, vars])))
                  callSuper(...)
                },
                #------------------------------
                predictc=function(data)
                {
                  "Return the predicted class."

                  pr <- predict(model, data, type="vector")
                  names(pr) <- rownames(data)
                  return(pr)
                },
                #------------------------------
                predictp=function(data)
                {
                  "Return the predicted probability for each class."

                  pr <- predict(model, data, type="probs")
                  # Row and column names not there by default, so add
                  # them.
                  rownames(pr) <- rownames(data)
                  colnames(pr) <- levels(model$fit)
                  return(pr)
                }
                ))

#------------------------------------

RpartClassifier <-
  setRefClass("RpartClassifier",

              contains="Classifier",

              methods=list(
                #------------------------------
                build=function(...)
                {
                  "Build an rpart decision tree classifier."

                  suppressPackageStartupMessages(require(rpart))
                  build.time <<-
                    system.time(model <<-
                                with(dataset,
                                     rpart(form, data[train, vars])))
                  callSuper(...)
                },
                #------------------------------
                predictc=function(data)
                {
                  "Return the predicted class."

                  predict(model, data, type="class")
                },
                #------------------------------
                predictp=function(data)
                {
                  "Return the predicted probability for each class."

                  predict(model, data, type="prob")
                }
                ))

########################################################################
