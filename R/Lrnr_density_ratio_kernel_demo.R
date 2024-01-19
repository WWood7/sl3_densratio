##' Template of a \code{sl3} Learner.
##'
##' This learner uses \code{\link[densratio]{densratio}} from \code{densratio} to kernel-based density ratio estimation algorithms.
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
Lrnr_densratio_kernel <- R6Class(
    classname = "Lrnr_densratio_kernel", inherit = Lrnr_base,
    portable = TRUE, class = TRUE,

    public = list(
        # if possible, your learner should define defaults for all required parameters
        initialize = function(sigma = 'auto', lambda = 'auto', alpha = 0.1, 
                              kernel_num = 100, fold = 5, verbose =TRUE,
                              method = 'KLIEP', stage2 = FALSE,...) {
            # this captures all parameters to initialize and saves them as self$params
            params <- args_to_list()
            super$initialize(params = params, ...)
        }
        
        # you can define public functions that allow your learner to do special things here
        # for instance glm learner might return prediction standard errors
        # special_function = function(arg_1) {
        # }
    ),
    private = list(
        # Use sl3_list_properties() for a list of options
        .properties = c("densratio"),
        
        # list any packages required for your learner here.
        .required_packages = c("densratio"),
        
        # .train takes task data and returns a fit object that can be used to generate predictions
        .train = function(task) {
            if (self$params$stage2 == TRUE){
              # modify the task
              covariates_set <- task$nodes$covariates[2:length(task$nodes$covariates)]
              task <- task$next_in_chain(
                covariates = covariates_set
              )
            }
            args <- self$params
            
            # get outcome variable type
            # preferring learner$params$outcome_type first, then task$outcome_type
            outcome_type <- self$get_outcome_type(task)
            # should pass something on to your learner indicating outcome_type
            # e.g. family or objective
            
            # add task data to the argument list
            # split the data into 2 sets, each represent a sample from one distribution
            whole_data <- data.table::setDT(task$data)
            # x1 is the numerator distribution
            # x2 is the denominator distribution
            data_x1 <- whole_data[indicator == 1][, indicator := NULL]
            data_x2 <- whole_data[indicator == 0][, indicator := NULL]
            args$x1 <- as.matrix(data_x1)
            args$x2 <- as.matrix(data_x2)
            
            # only add arguments on weights and offset
            # if those were specified when the task was generated
            if (task$has_node("weights")) {
                args$weights <- task$weights
            }
            
            if (task$has_node("offset")) {
                args$offset <- task$offset
            }
            
            # call a function that fits your algorithm
            # with the argument list you constructed
            fit_object <- call_with_args(densratio::densratio, args)
            
            # return the fit object, which will be stored
            # in a learner object and returned from the call
            # to learner$predict
            return(fit_object)
        },
        
        # .predict takes a task and returns predictions from that task
        .predict = function(task) {
            # check stage
            if (self$params$stage2 == TRUE){
              stage1_results <- task$data$stage1_results
              covariates_set <- task$nodes$covariates[2:length(task$nodes$covariates)]
              task <- task$next_in_chain(
                covariates = covariates_set
              )
            }
            # get the upper bound
          
            pred_data <- data.table::setDT(task$X)
            pred_data <- as.matrix(pred_data)
            predictions <- self$fit_object$compute_density_ratio(pred_data)
            # if this is the stage2 estimation    
            if (self$params$stage2 == TRUE){
              predictions <- stage1_results / predictions
            }
            return(predictions)
        },
        
        .chain = function(task){
          # the second to the last are the sets of variables we condition on
          stage1_results <- self$predict(task)
          stage1_results <- as.data.table(stage1_results)
          task <- task$revere_fold_task("full")
          new_col_names <- task$add_columns(stage1_results, self$fit_uuid)
          covariates_set <- c(names(stage1_results), task$nodes$covariates[2:length(task$nodes$covariates)])
          return(task$next_in_chain(
            covariates = covariates_set,
            column_names = new_col_names
          ))
        }
    )
)

