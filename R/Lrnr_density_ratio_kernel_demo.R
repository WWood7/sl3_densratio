#' Density Ratio Estimation Using Kernel-Based Methods
#'
#' This learner uses \code{\link[densratio]{densratio}} from \code{densratio} to conduct marginal density ratio estimation.
#' Can be used in \code{\link[sl3]{Pipeline}} to do conditional density ratio estimation.
#'
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return Learner object with methods for training and prediction. See \code{\link{Lrnr_base}} for documentation on learners.
#' @format \code{\link{R6Class}} object.
#' @family Learners
#'
#' @section Parameters:
#' \describe{
#'   \item{\code{method='KLIEP'}}{The kernel method used. Support "KLIEP", "RuLSIF" and "uLSIF". See \code{\link[densratio]{densratio}}.
#'   }
#'   \item{\code{sigma='auto'}}{ A positive numeric vector as the search range of Gaussian kernel bandwidth. See \code{\link[densratio]{densratio}}.
#'   }
#'   \item{\code{lambda='auto'}}{A positive numeric vector as the search range of regularization parameter for uLSIF and RuLSIF. See \code{\link[densratio]{densratio}}.
#'   }
#'   \item{\code{alpha=0.1}}{A numeric in [0, 1]. Relative parameter for RuLSIF. See \code{\link[densratio]{densratio}}.
#'   }
#'   \item{\code{kernul_num=100}}{A positive integer, number of kernels. See \code{\link[densratio]{densratio}}.
#'   }
#'   \item{\code{fold=5}}{A positive integer, number of the folds of cross validation for getting bandwidth. See \code{\link[densratio]{densratio}}.
#'   }
#'   \item{\code{verbosed=TRUE}}{Verbose logical. See \code{\link[densratio]{densratio}}.
#'   }
#'  \item{\code{conditional_set=NULL}}{A vector of variable names that are contained in the conditional set. Should only be used in conditional density
#'   ratio estimation.
#'   }
#' }
#'
#' @section Examples:
#' \describe{
#'  \item{Marginal Density Estimation}{
#'  Density ratio is not a variable that can be explicitly observed, so defining the \code{\link[sl3]{sl3_Task}} in this setting can be a little tricky.
#'  To estimate density ratio in the form \eqn{f(x1, x2) / g(x1, x2)}, the covariates defined in the \code{\link[sl3]{sl3_Task}} should be (x1, x2), the outcome should
#'  be a binary indicator that marks data points from the numerator distribution as 1 and denominator distribution as 0.
#'  
#'  \itemize{
#'  \item{define a task}: \code{task <- sl3_Task$new(data = dataset, covariate = c('x1', 'x2'), outcome = indicator_name)}
#'  \item{define a learner}: \code{lr <- Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = 100, alpha = 0.8)}
#'  }
#'  }
#'  \item{Conditional Density Estimation}{
#'  To estimate density ratio in the form \eqn{f(x1 | x2) / g(x1 | x2) = f(x1, x2) / g(x1 , x2) / [f(x2) / g(x2)]}, 
#'  it is doable to construct a \code{\link[sl3]{Pipeline}} with 2 densratio learners. In this case the second learner should
#'  have a non-null \code{conditional_set} argument. The task should be defined in the same way as marginal density estimation.
#'  
#'  \itemize{
#'  \item{define a task}: \code{task <- sl3_Task$new(data = dataset, covariate = c('x1', 'x2'), outcome = indicator_name)}
#'  \item{define a learner using \code{\link[sl3]{Pipeline}}}: 
#'  \itemize{
#'  \item{} \code{
#'  lr1 <- Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = 100, alpha = 0.8, name = 'lr1')
#'  }
#'  \item{}\code{
#'  lr2 <- Lrnr_densratio_kernel$new(method = 'uLSIF', kernel_num = 200, name = 'lr2', conditional_set = 'x2')}
#'  \item{}\code{
#'  lr <- Pipeline$new(lr1, lr2)
#'  }
#'  }
#'  }
#'  }
#'  }
#' @template common_parameters

Lrnr_densratio_kernel <- R6Class(
    classname = "Lrnr_densratio_kernel", inherit = Lrnr_base,
    portable = TRUE, class = TRUE,

    public = list(
        # if possible, your learner should define defaults for all required parameters
        initialize = function(sigma = 'auto', lambda = 'auto', alpha = 0.1, 
                              kernel_num = 100, fold = 5, verbose =TRUE,
                              method = 'KLIEP',
                              conditional_set = NULL, ...) {
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
            if (!is.null(self$params$conditional_set)){
              if (!(self$params$conditional_set %in% task$nodes$covariates)) {
                stop("Conditional set specified does not exist among the task's covariates.")
              }
              
              # Identify the index of the conditional set in the covariates
              conditional_index <- which(task$nodes$covariates == self$params$conditional_set)
              covariates_set <- task$nodes$covariates[conditional_index]
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
            data_x1 <- subset(whole_data[get(task$nodes$outcome) == 1], select = names(task$X))
            data_x2 <- subset(whole_data[get(task$nodes$outcome) == 0], select = names(task$X))
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
            if (!is.null(self$params$conditional_set)){
              stage1_results <- task$data$stage1_results
              if (!(self$params$conditional_set %in% task$nodes$covariates)) {
                stop("Conditional set specified does not exist among the task's covariates.")
              }
              
              # Identify the index of the conditional set in the covariates
              conditional_index <- which(task$nodes$covariates == self$params$conditional_set)
              covariates_set <- task$nodes$covariates[conditional_index]
              task <- task$next_in_chain(
                covariates = covariates_set
              )
            }
            # get the upper bound
          
            pred_data <- data.table::setDT(task$X)
            pred_data <- as.matrix(pred_data)
            predictions <- self$fit_object$compute_density_ratio(pred_data)
            # if this is the stage2 estimation    
            if (!is.null(self$params$conditional_set)){
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
          covariates_set <- c(names(stage1_results), task$nodes$covariates)
          return(task$next_in_chain(
            covariates = covariates_set,
            column_names = new_col_names
          ))
        }
    )
)

