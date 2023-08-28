##' Template of a \code{sl3} Learner.
##'
##' This is a template for defining a new learner.
##' This can be copied to a new file using \code{\link{write_learner_template}}.
##' The remainder of this documentation is an example of how you might write documentation for your new learner.
##' This learner uses \code{\link[my_package]{my_ml_fun}} from \code{my_package} to fit my favorite machine learning algorithm.
##'
##' @docType class
##' @importFrom R6 R6Class
##' @export
##' @keywords data
##' @return Learner object with methods for training and prediction. See \code{\link{Lrnr_base}} for documentation on learners.
##' @format \code{\link{R6Class}} object.
##' @family Learners
##'
##' @section Parameters:
##' \describe{
##'   \item{\code{param_1="default_1"}}{ This parameter does something.
##'   }
##'   \item{\code{param_2="default_2"}}{ This parameter does something else.
##'   }
##'   \item{\code{...}}{ Other parameters passed directly to \code{\link[my_package]{my_ml_fun}}. See its documentation for details.
##'   }
##' }
##'
##' @section Methods:
##' \describe{
##' \item{\code{special_function(arg_1)}}{
##'   My learner is special so it has a special function.
##'
##'   \itemize{
##'     \item{\code{arg_1}: A very special argument.
##'    }
##'   }
##'   }
##' }
##' This learner for density ratio is only applicable for marginal density ratio or
##' conditional density ratio with the variables conditioned on have the same distributions.
Lrnr_densratio_classification <- R6Class(
    classname = "Lrnr_densratio_classification", inherit = Lrnr_base,
    portable = TRUE, class = TRUE,
    public = list(
        initialize = function(classifier = NULL, ...) {
            # this captures all parameters to initialize and saves them as self$params
            params <- args_to_list()
            
            if (is.null(params$classifier)) {
                params$classifier <- make_learner(Lrnr_glm_fast)
            }
            super$initialize(params = params, ...)
        },
        
        # maybe allow the inner classifiers to output the predicted outcomes
        # of the classification?
        special_function = function(arg_1) {
        }
    ),
    private = list(
        # list properties your learner supports here.
        # Use sl3_list_properties() for a list of options
        .properties = c("densratio"),
        # .train takes task data and returns a fit object that can be used to generate predictions
        .train = function(task) {
            classifier <- self$params$classifier
            prob_fit <- classifier$train(task)
            fit_object <- prob_fit
            
            # return the fit object, which will be stored
            # in a learner object and returned from the call
            # to learner$predict
            return(fit_object)
        },
        
        # .predict takes a task and returns predictions from that task
        .predict = function(task = NULL) {
            prob <- self$fit_object$predict(task)
            predictions <- prob / (1 - prob)
            return(predictions)
        }
    )
)