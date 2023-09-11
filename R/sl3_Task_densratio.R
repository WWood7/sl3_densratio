

sl3_Task_densratio <- R6Class(
    classname = "sl3_Task_densratio",
    inherit = sl3_Task,
    portable = TRUE,
    class = TRUE,
    public = list(
        initialize = function(data, covariates, outcome = NULL, folds = NULL,
                              id = NULL, weights = NULL, column_names = NULL,
                              method = NULL, how = NULL, nodes = NULL, 
                              row_index = NULL, outcome_type = NULL){
            # 
            # first check if the method is supported
            # ensure that there is no variable named indicator
            
            # based on method, augment the data
            if (is.null(method)){
                new_data <- data
            }else if(method == 'discrete'){
                varname <- how$varname
                numerator_value <- how$value[1]
                denominator_value <- how$value[2]
                
                new_data <- data
                new_data$indicator <- 0
                new_data$indicator[new_data[[varname]] == numerator_value] <- 1
                new_data <- new_data[new_data[[varname]] %in% how$value, ]
                new_data[[varname]] <- NULL
                
                new_data$id <- seq_len(nrow(new_data))
            }else if (method == 'formula'){
                if (is.null(id)){
                    n <- nrow(data)
                    data$id <- seq_len(n)
                } else{
                    data$id <- data[[id]]
                    data[[id]] <- NULL
                }
                aug_data <- data
                formulas <- how$formulas
                variables <- how$variables
                for (i in 1:length(formulas)){
                    formula_name <- paste0('formula', as.character(i))
                    temp_formula <- formulas[[formula_name]]
                    variables_name <- paste0('variables', as.character(i))
                    # get the name of the new variable
                    target_varname <- variables[[variables_name]][1]
                    # extract the variables needed for evaluating the formula
                    formula_varname <- variables[[variables_name]][2:length(variables[[variables_name]])]
                    formula_args <- lapply(formula_varname, function(v) aug_data[[v]])
                    aug_data[[target_varname]] <- do.call(gsubfn::as.function.formula(temp_formula), formula_args)
                    
                }
                aug_data$indicator <- 1
                data$indicator <- 0
                new_data <- rbind(data, aug_data)
            }
            # now use the augmented data as data
            # call super$initialize
            super$initialize(
                data = new_data, covariates = covariates,
                outcome = 'indicator', folds = folds,
                id = 'id', weights = weights, column_names = column_names,
                nodes = nodes
            )
            
            
                
        }
    )
)