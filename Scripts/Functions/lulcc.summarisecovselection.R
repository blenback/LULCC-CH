### =========================================================================
### lulcc.summarisecovselection: Aggregate summary information on results of
### 2 stage covariate selection procedure
### =========================================================================
#'
#'
#' @param nested_list_of_trans list of transitions with result elements
#'    produced by lulcc.analysecovselection function
#' @param split_by Character determining aspect of transition
#'  datasets to summarize by accepts:
#'  "across_trans", "Bioregion", "Initial_lulc", "Final_lulc".
#'
#' @author Ben Black
#' @export

lulcc.summarisecovselection <- function(nested_list_of_trans, split_by) {
  if (split_by == "Bioregion") {
    # 1. create a summary table across the levels of the nested list
    # of the number of covariates remaining after each selection process and
    # the measures of central tendency for the sets of covariates that
    # each process produces

    # drop the two dataframes of cov occurences
    subsetted_summary <- lapply(nested_list_of_trans, function(x) unlist(x[c(1, 2, 5, 6)])) # drop the two dataframes of cov occurences and unlist to named vector
    summarized_dataframe <- round(do.call(rbind.data.frame, args = list(subsetted_summary, stringsAsFactors = FALSE)), digits = 0) # rbind the list of vectors into a dataframe
    rownames(summarized_dataframe) <- str_replace_all((str_remove_all(names(unlist(subsetted_summary[[1]])), "MOCT_")), "[._]", " ") # clean up names

    # 2. Create a summary table across the levels of the nested list of the frequency
    # of occurence of covariates under each selection process

    # First combine results from both the collinearity selection process and
    # embedded selection process for separate initial classes into two tables

    # create vector of suffix names
    sfx <- names(nested_list_of_trans)

    # subset to dataframes of cov occurence under each selection process
    subset_cov_occurences <- lapply(nested_list_of_trans, function(x) x[c(3, 4)])

    # instantiate results object for collinearity selection
    res_collinearity <- subset_cov_occurences[[1]][[1]]

    # loop through list items
    for (i in head(seq_along(subset_cov_occurences), -1)) {
      res_collinearity <- merge(res_collinearity, subset_cov_occurences[[i + 1]][[1]],
        all = TRUE,
        suffixes = sfx[i:(i + 1)], by = "Var1"
      )
    }

    colnames(res_collinearity) <- c("Covariate", sfx)
    res_collinearity$total_occurences <- rowSums(res_collinearity[, 2:ncol(res_collinearity)], na.rm = TRUE)
    res_collinearity_sorted <- res_collinearity[order(-res_collinearity$total_occurences), ]

    # instantiate results object for embedded selection
    res_embedded <- subset_cov_occurences[[2]][[1]]

    # loop through list items
    for (i in head(seq_along(subset_cov_occurences), -1)) {
      res_embedded <- merge(res_embedded, subset_cov_occurences[[i + 1]][[2]],
        all = TRUE,
        suffixes = sfx[i:(i + 1)], by = "Var1"
      )
    }
    colnames(res_embedded) <- c("Covariate", sfx)
    res_embedded$total_occurences <- rowSums(res_embedded[, 2:ncol(res_embedded)], na.rm = TRUE)
    res_embedded_sorted <- res_embedded[order(-res_embedded$total_occurences), ]

    # Create a list of the two tables of cov_occurence results
    Cov_occurence_tabular <- list(
      Cov_occurence_after_collinearity_selection = res_collinearity_sorted,
      Cov_occurence_after_embedded_selection = res_embedded_sorted
    )


    # 3. Produce plots from each of the summary tables of freq of covariate
    # occurence limited to covariates that occurred <10 times

    # vectors used to convert data from long to wide which are applicable for both selection processes
    keycol <- "Region"
    valuecol <- "Frequency"
    gathercols <- sfx
    # loop over each table in the list of results

    Cov_occurence_plots <- lapply(seq_along(Cov_occurence_tabular), function(x) {

      # convert data from wide to long
      long_data <- gather_(Cov_occurence_tabular[[x]], keycol, valuecol, gathercols)

      # drop rows that do not occur in more than 10 transitions
      Filtered_data <- long_data %>% filter(total_occurences > 10)
      
      #order the data by descending order of total_occurences
      ordered_data <- Filtered_data[order(-Filtered_data$total_occurences),]

      
      Plot_title <- str_replace_all(str_replace(paste(names(Cov_occurence_tabular)[[x]]), "Cov", "Covariate"), "[._]", " ") 
      
      # create plot
      ggplot(data = ordered_data, aes(fill = Region, y = Frequency, x =fct_reorder(Covariate, total_occurences, .desc = TRUE))) +
        geom_bar(position = "stack", stat = "identity") +
        labs(fill = "Bioregion", x = "Covariates" , y = "Frequency across transition datasets", title = Plot_title) +
        theme(axis.text.x = element_text(size = 9,angle = 90)) + 
        scale_fill_brewer(palette = "Dark2")
    })
    names(Cov_occurence_plots) <- c("Cov_occurence_collinearity_selection_plot", "Cov_occurence_embedded_selection_plot")

    # Group the general cov_selection summary results together with the cov_occurence results as one list for final output
    return(list(
      Cov_selection_summary = summarized_dataframe,
      Cov_occurence_summary = list(
        Cov_occurence_tables = Cov_occurence_tabular,
        cov_occurence_plots = Cov_occurence_plots
      )
    ))
  }


  if (split_by == "Initial_lulc") {

    # 1. create a summary table across the levels of the nested list
    # of the number of covariates remaining after each selection process and
    # the measures of central tendency for the sets of covariates that
    # each process produces

    # drop the two dataframes of cov occurences
    subsetted_summary <- lapply(nested_list_of_trans, function(x) unlist(x[c(1, 2, 5, 6)])) # drop the two dataframes of cov occurences and unlist to named vector
    summarized_dataframe <- round(do.call(rbind.data.frame, args = list(subsetted_summary, stringsAsFactors = FALSE)), digits = 0) # rbind the list of vectors into a dataframe
    rownames(summarized_dataframe) <- str_replace_all((str_remove_all(names(unlist(subsetted_summary[[1]])), "MOCT_")), "[._]", " ") # clean up names

    # 2. Create a summary table across the levels of the nested list of the frequency
    # of occurence of covariates under each selection process

    # First combine results from both the collinearity selection process and
    # embedded selection process for separate initial classes into two tables

    # create vector of suffix names
    sfx <- names(nested_list_of_trans)

    # subset to dataframes of cov occurence under each selection process
    subset_cov_occurences <- lapply(nested_list_of_trans, function(x) x[c(3, 4)])

    # instantiate results object for collinearity selection
    res_collinearity <- subset_cov_occurences[[1]][[1]]

    # loop through list items
    for (i in head(seq_along(subset_cov_occurences), -1)) {
      res_collinearity <- merge(res_collinearity, subset_cov_occurences[[i + 1]][[1]],
        all = TRUE,
        suffixes = sfx[i:(i + 1)], by = "Var1"
      )
    }

    colnames(res_collinearity) <- c("Covariate", sfx)
    res_collinearity$total_occurences <- rowSums(res_collinearity[, 2:ncol(res_collinearity)], na.rm = TRUE)
    res_collinearity_sorted <- res_collinearity[order(-res_collinearity$total_occurences), ]

    # instantiate results object for embedded selection
    res_embedded <- subset_cov_occurences[[2]][[1]]

    # loop through list items
    for (i in head(seq_along(subset_cov_occurences), -1)) {
      res_embedded <- merge(res_embedded, subset_cov_occurences[[i + 1]][[2]],
        all = TRUE,
        suffixes = sfx[i:(i + 1)], by = "Var1"
      )
    }
    colnames(res_embedded) <- c("Covariate", sfx)
    res_embedded$total_occurences <- rowSums(res_embedded[, 2:ncol(res_embedded)], na.rm = TRUE)
    res_embedded_sorted <- res_embedded[order(-res_embedded$total_occurences), ]

    # Create a list of the two tables of cov_occurence results
    Cov_occurence_tabular <- list(
      Cov_occurence_after_collinearity_selection = res_collinearity_sorted,
      Cov_occurence_after_embedded_selection = res_embedded_sorted
    )


    # 3. Produce plots from each of the summary tables of freq of covariate
    # occurence limited to covariates that occurred <10 times

    # vectors used to convert data from long to wide which are applicable for both selection processes
    keycol <- "Initial_LULC"
    valuecol <- "Frequency"
    gathercols <- sfx
    # loop over each table in the list of results
    
        Cov_occurence_plots <- lapply(seq_along(Cov_occurence_tabular), function(x) {

      # convert data from wide to long
      long_data <- gather_(Cov_occurence_tabular[[x]], keycol, valuecol, gathercols)

      # drop rows that do not occur in more than 10 transitions
      Filtered_data <- long_data %>% filter(total_occurences > 10)
      
      #order the data by descending order of total_occurences
      ordered_data <- Filtered_data[order(-Filtered_data$total_occurences),]

      
      Plot_title <- str_replace_all(str_replace(paste(names(Cov_occurence_tabular)[[x]]), "Cov", "Covariate"), "[._]", " ") 
      
      # create plot
      ggplot(data = ordered_data, aes(fill = Initial_LULC, y = Frequency, x =fct_reorder(Covariate, total_occurences, .desc = TRUE))) +
        geom_bar(position = "stack", stat = "identity") +
        labs(fill = "Initial LULC class", x = "Covariates" , y = "Frequency across transition datasets", title = Plot_title) +
        theme(axis.text.x = element_text(size = 9,angle = 90)) + 
        scale_fill_brewer(palette = "Dark2")
    })
    names(Cov_occurence_plots) <- c("Cov_occurence_collinearity_selection_plot", "Cov_occurence_embedded_selection_plot")


    # Group the general cov_selection summary results together with the cov_occurence results as one list for final output
    return(list(
      Cov_selection_summary = summarized_dataframe,
      Cov_occurence_summary = list(
        Cov_occurence_tables = Cov_occurence_tabular,
        cov_occurence_plots = Cov_occurence_plots
      )
    ))
  }



  if (split_by == "Final_lulc") {

    # 1. create a summary table across the levels of the nested list
    # of the number of covariates remaining after each selection process and
    # the measures of central tendency for the sets of covariates that
    # each process produces

    # drop the two dataframes of cov occurences
    subsetted_summary <- lapply(nested_list_of_trans, function(x) unlist(x[c(1, 2, 5, 6)])) # drop the two dataframes of cov occurences and unlist to named vector
    summarized_dataframe <- round(do.call(rbind.data.frame, args = list(subsetted_summary, stringsAsFactors = FALSE)), digits = 0) # rbind the list of vectors into a dataframe
    rownames(summarized_dataframe) <- str_replace_all((str_remove_all(names(unlist(subsetted_summary[[1]])), "MOCT_")), "[._]", " ") # clean up names
    # 2. Create a summary table across the levels of the nested list of the frequency
    # of occurence of covariates under each selection process

    # First combine results from both the collinearity selection process and
    # embedded selection process for separate initial classes into two tables

    # create vector of suffix names
    sfx <- names(nested_list_of_trans)

    # subset to dataframes of cov occurence under each selection process
    subset_cov_occurences <- lapply(nested_list_of_trans, function(x) x[c(3, 4)])

    # instantiate results object for collinearity selection
    res_collinearity <- subset_cov_occurences[[1]][[1]]

    # loop through list items
    for (i in head(seq_along(subset_cov_occurences), -1)) {
      res_collinearity <- merge(res_collinearity, subset_cov_occurences[[i + 1]][[1]],
        all = TRUE,
        suffixes = sfx[i:(i + 1)], by = "Var1"
      )
    }

    colnames(res_collinearity) <- c("Covariate", sfx)
    res_collinearity$total_occurences <- rowSums(res_collinearity[, 2:ncol(res_collinearity)], na.rm = TRUE)
    res_collinearity_sorted <- res_collinearity[order(-res_collinearity$total_occurences), ]

    # instantiate results object for embedded selection
    res_embedded <- subset_cov_occurences[[2]][[1]]

    # loop through list items
    for (i in head(seq_along(subset_cov_occurences), -1)) {
      res_embedded <- merge(res_embedded, subset_cov_occurences[[i + 1]][[2]],
        all = TRUE,
        suffixes = sfx[i:(i + 1)], by = "Var1"
      )
    }
    colnames(res_embedded) <- c("Covariate", sfx)
    res_embedded$total_occurences <- rowSums(res_embedded[, 2:ncol(res_embedded)], na.rm = TRUE)
    res_embedded_sorted <- res_embedded[order(-res_embedded$total_occurences), ]

    # Create a list of the two tables of cov_occurence results
    Cov_occurence_tabular <- list(
      Cov_occurence_after_collinearity_selection = res_collinearity_sorted,
      Cov_occurence_after_embedded_selection = res_embedded_sorted
    )


    # 3. Produce plots from each of the summary tables of freq of covariate
    # occurence limited to covariates that occurred <10 times

    # vectors used to convert data from long to wide which are applicable for both selection processes
    keycol <- "Final_LULC"
    valuecol <- "Frequency"
    gathercols <- sfx
    # loop over each table in the list of results

    Cov_occurence_plots <- lapply(seq_along(Cov_occurence_tabular), function(x) {

      # convert data from wide to long
      long_data <- gather_(Cov_occurence_tabular[[x]], keycol, valuecol, gathercols)

      # drop rows that do not occur in more than 10 transitions
      Filtered_data <- long_data %>% filter(total_occurences > 10)
      
      #order the data by descending order of total_occurences
      ordered_data <- Filtered_data[order(-Filtered_data$total_occurences),]

      
      Plot_title <- str_replace_all(str_replace(paste(names(Cov_occurence_tabular)[[x]]), "Cov", "Covariate"), "[._]", " ") 
      
      # create plot
      ggplot(data = ordered_data, aes(fill = Final_LULC, y = Frequency, x =fct_reorder(Covariate, total_occurences, .desc = TRUE))) +
        geom_bar(position = "stack", stat = "identity") +
        labs(fill = "Final LULC class", x = "Covariates" , y = "Frequency across transition datasets", title = Plot_title) +
        theme(axis.text.x = element_text(size = 9,angle = 90)) + 
        scale_fill_brewer(palette = "Dark2")
    })
    names(Cov_occurence_plots) <- c("Cov_occurence_collinearity_selection_plot", "Cov_occurence_embedded_selection_plot")

    # Group the general cov_selection summary results together with the cov_occurence results as one list for final output
    return(list(
      Cov_selection_summary = summarized_dataframe,
      Cov_occurence_summary = list(
        Cov_occurence_tables = Cov_occurence_tabular,
        cov_occurence_plots = Cov_occurence_plots
      )
    ))
  }
}


