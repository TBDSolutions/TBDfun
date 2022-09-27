
# This function takes a dataframe and uses the number of columns
# with the most amount of non-missing data (specified with the n argument)
# to predict the other columns that have missing values. If n is specified as
# one, then only linear regression will be used using that column with the most
# observations, otherwise muliple linear regression will be used.


regression_impute = function(x,n,id_var){

  # x = clust_df
  # id_var = 'tract'
  # n = 10

  # Pre Steps
  library(tidyverse)

  # Take the variables that will be used as predictors for the missing columns.
  # These should be as many variables as are suitable to populate the required rows.

  pred_vars =
    data.frame(complete_cases =
                 apply(x %>% select(-id_var),MARGIN = 2,FUN =function(x) sum(!is.na(x)))) %>%
    rownames_to_column("var") %>%
    arrange(desc(complete_cases)) %>%
    slice(1:n) %>%
    select(var) %>%
    pull()

  # Since every missing column needing to be imputed will require a full scoring
  # matrix without any missing values, this takes the pred_var with the least amount
  # of missing values. It will serve as the lowest common denominator.
  lcd =
    x %>%
    select(id_var,pred_vars[n]) %>%
    na.omit() %>%
    select(id_var) %>% pull()

  # Create the empty data frame to store the newly minted columns with missing
  # values imputed.
  final_result<-
    x %>%
    select(id_var) %>%
    distinct()

  # Generate the list of feature names which need to be imputed.
  # By deffinition, these features must not be part of the predictor
  # variables that will be used to predict the missing values.
  feature_names = colnames(x[which(!names(x) %in% c(id_var,pred_vars))])

  #=========================================#

  # Start for loop

  for( i in feature_names){

    #i = feature_names[1]

    # Create modeling data frame using only those tracts that have a complete
    # set of observations.
    model_df =
      x %>%
      select(all_of(c(id_var,i,pred_vars))) %>%
      filter(tract %in% lcd)

    # Generate the model and scoring data sets
    missing_index = which(is.na(model_df[,i]))

    scoring_data = model_df[missing_index,]
    model_data = model_df[-missing_index,]


    equation = noquote(paste0(i, " ~ ."))

    # Build the model and generate the missing values through
    # imputation
    model = lm(equation, data = model_data %>% select_if(is.numeric))

    imputed_values =
      scoring_data %>%
      mutate(imputed_values = round(as.numeric(predict(model, scoring_data %>% select(-all_of(c(i,id_var))))),8)) %>%
      select(tract,imputed_values)

    # Replace NA values in the missing column with the
    # values for the imputed regression predictions.
    full_set<-
      model_df %>%
      select(id_var,i) %>%
      left_join(imputed_values, by = id_var)

    vecs = list(
      full_set[,i] %>% pull(),
      full_set[,"imputed_values"] %>% pull())


    df=
      full_set %>%
      mutate(!!i := round(coalesce(!!!vecs),2)) %>%
      select(1,2) %>%
      na.omit()

    # Add to the final result dataset.
    final_result<-
      final_result %>% left_join(df,by = id_var) %>%
      na.omit()

  }

  # End for loop

  return(final_result)

  rm(full_set,vecs,df,model,equation,missing_index,scoring_data,
     vecs,model,model_df,feature_names,lcd,pred_vars)

}

#df_imputed = regression_impute(x = clust_df,n = 10, id_var = 'tract')
