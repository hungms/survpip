#' Make a multivariate table
#'
#' This function makes a multivariate table for a given column in a data frame.
#' It fits a survival model and creates a table with the hazard ratios and p-values.
#'
#' @param df A data frame containing the data.
#' @param variables A vector of column names in the data frame that contains the variables to include in the table.
#' @param covariates A vector of column names in the data frame that contains the covariates.
#' @param time The column name in the data frame that contains the time variable.
#' @param event The column name in the data frame that contains the event variable.
#' @return A data frame containing the multivariate table.
#' @export
make_multivariate_table <- function(df, variables, covariates, time, event){
    
    stopifnot(is.data.frame(df))
    stopifnot(all(variables %in% colnames(df)))
    stopifnot(all(covariates %in% colnames(df)))
    stopifnot(all(c(time, event) %in% colnames(df)))

    surv.formula <- paste0("Surv(", time, ", ", event, ")")
    covariate.table <- as.data.frame(df) %>% 
        finalfit(
            surv.formula, 
            covariates, 
            add_dependent_label = FALSE) %>%
        as.data.frame(.) %>%
        filter(!levels %in% c("Missing", "not determined"))

    for(i in seq_along(variables)){
        variable.table <- as.data.frame(df) %>% 
            finalfit(
                surv.formula, 
                c(covariates, variables[i]), 
                add_dependent_label = FALSE) %>%
            as.data.frame(.) %>%
            filter(label == variables[i])
        covariate.table <- bind_rows(list(covariate.table, variable.table))
    }

    table <- covariate.table

    # final formatting
    colnames(table) <- c("Category", "Subgroup", "n", "HR (univariate)", "HR (multivariate)")
    table$n <- gsub(")", "%)", table$n)
    table <- table %>% 
        filter(Subgroup != " NA") %>%
        mutate(
            Subgroup = ifelse(Subgroup == "Mean (SD)", Category, Subgroup),
            Category = ifelse(Subgroup == Category, "", Category),
            n = ifelse(str_detect(n, "0.0 \\(1.0\\%\\)"), length(na.omit(df[[time]])), n))

    table[table == "-"] <- "Reference"
    table[table[] == "NA (NA-NA, p=NA)"] <- "NA"
    remove <- which(str_detect(table$`HR (multivariate)`, "Reference"))
    table <- as.matrix(table)
    # remove reference rows
    for(r in remove){
        table[c(r + 1),1] <- table[c(r),1]}
    table <- as.data.frame(table)
    table <- table[-c(remove),]

    return(table)
}

#' Plot a forest plot
#' 
#' This function plots a forest plot for a given table. 
#' 
#' @param table A data frame containing the table.
#' @param title The title of the plot.
#' @param analysis The type of analysis to perform. Can be "multivariate" or "univariate".
#' @param remove_conf Logical indicating whether to remove the confidence interval.
#' @param clip The range of the x-axis.
#' @return A ggplot object.
#' @export
plot_forest <- function(table, title = "", analysis = "multivariate", remove_conf = TRUE, clip = c(0.3, 4)){

    analysis_types <- c("multivariate", "univariate")
    stopifnot(analysis %in% analysis_types)
    analysis_keep <- ifelse(analysis == "multivariate", "HR (multivariate)", "HR (univariate)")
    analysis_remove <- ifelse(analysis == "multivariate", "HR (univariate)", "HR (multivariate)")

    # format foreset plot
    table <- table %>%
        mutate(
            # mean
            mean = ifelse(!!sym(analysis_keep) == "-", NA, !!sym(analysis_keep)),
            mean = as.numeric(gsub(" \\(.*", "", mean)),
            # lower
            lower = ifelse(!!sym(analysis_keep) == "-", NA, !!sym(analysis_keep)),
            lower = gsub(".*\\(", "", lower),
            lower = as.numeric(gsub("-.*", "", lower)),
            # upper
            upper = ifelse(!!sym(analysis_keep) == "-", NA, !!sym(analysis_keep)),
            upper = gsub(".*-", "", upper),
            upper = as.numeric(gsub(",.*", "",upper)))

    if(remove_conf){
        table <- table %>%
            mutate(
                `HR (univariate)` = gsub("\\(.*?, p", "(p", `HR (univariate)`),
                `HR (multivariate)` = gsub("\\(.*?, p", "(p", `HR (multivariate)`))}

    table <- table %>%
        mutate(
            mean = as.numeric(mean),
            lower = as.numeric(lower),
            upper = as.numeric(upper)
        )

    for(i in 1:nrow(table)){
    if(i == 1){
        txt_gp <- list(gpar(fontfamily = "Helvetica", fontsize = 12))}
    else{
        txt_gp <- c(txt_gp, list(gpar(fontfamily = "Helvetica", fontsize = 10, col = "grey10")))}}

    labeltext_cols <- c("Category", "Subgroup", "n", analysis_keep)

    plot <- as_tibble(table) %>%
        dplyr::select(-c(analysis_remove)) %>%
        forestplot(
            labeltext = table[, labeltext_cols], 
            clip = clip, 
            boxsize = 0.4,
            vertices = TRUE,
            xlog = TRUE,
            xlab = analysis_keep,
            txt_gp = fpTxtGp(label = txt_gp)
            ) %>%
        {
            header_list <- list(
            Category = c(title, "Category"),
            Subgroup = c("", "Subgroup"),
            n = c("", "n")
            )
            header_list[[analysis_keep]] <- c("", analysis_keep)
            do.call(fp_add_header, c(list(x = .), header_list))
        }
    
    line_rows <- which(str_detect(table[[1]], "[0-9]|[A-Z]|[a-z]"))
    botton_line <- paste0('h_', nrow(table) + 3, ' = gpar(lwd = 1, columns = 1:5, col = "black"))')
    if(length(line_rows) > 1){
        line_functions <- paste0(paste0('h_', line_rows + 2, ' = gpar(lwd = 1, lty = 2, columns = 1:5, col = "grey50"), '), collapse = "\n")
        eval(parse(text = paste0(
            'plot <- plot %>% fp_add_lines(h_3 = gpar(lwd = 1, columns = 1:5, col = "black"),', line_functions, botton_line)))}
    else{
        eval(parse(text = paste0(
            'plot <- plot %>% fp_add_lines(h_3 = gpar(lwd = 1, columns = 1:5, col = "black"),', botton_line)))}
    
    return(plot)        
    }


#' Plot a forest plot for genes
#' 
#' This function plots a forest plot for a given table. 
#' 
#' @param table A data frame containing the table.
#' @param title The title of the plot.
#' @param analysis The type of analysis to perform. Can be "multivariate" or "univariate".
#' @param clip The range of the x-axis.
#' @return A ggplot object.
#' @export
plot_forest_gene <- function(table, title = "", analysis = "multivariate", clip = c(0.3, 4)){

    analysis_types <- c("multivariate", "univariate")
    stopifnot(analysis %in% analysis_types)
    analysis_keep <- ifelse(analysis == "multivariate", "HR (multivariate)", "HR (univariate)")
    analysis_remove <- ifelse(analysis == "multivariate", "HR (univariate)", "HR (multivariate)")

    # format foreset plot
    table <- table %>%
        mutate(
            Gene = Subgroup,
            # mean
            mean = ifelse(!!sym(analysis_keep) == "-", NA, !!sym(analysis_keep)),
            mean = as.numeric(gsub(" \\(.*", "", mean)),
            # lower
            lower = ifelse(!!sym(analysis_keep) == "-", NA, !!sym(analysis_keep)),
            lower = gsub(".*\\(", "", lower),
            lower = as.numeric(gsub("-.*", "", lower)),
            # upper
            upper = ifelse(!!sym(analysis_keep) == "-", NA, !!sym(analysis_keep)),
            upper = gsub(".*-", "", upper),
            upper = as.numeric(gsub(",.*", "",upper)),
            `HR (univariate)` = gsub("\\(.*?, p", "(p", `HR (univariate)`),
            `HR (multivariate)` = gsub("\\(.*?, p", "(p", `HR (multivariate)`)
            ) %>%
        dplyr::select(c("Gene", analysis_keep, "mean", "lower", "upper"))

    for(i in 1:nrow(table)){
    if(i == 1){
        txt_gp <- list(gpar(fontfamily = "Helvetica", fontsize = 12))}
    else{
        txt_gp <- c(txt_gp, list(gpar(fontfamily = "Helvetica", fontsize = 10, col = "grey10")))}}

    labeltext_cols <- c("Gene", analysis_keep)

    plot <- as_tibble(table) %>%
        forestplot(
            labeltext = table[, labeltext_cols], 
            clip = clip, 
            boxsize = 0.4,
            vertices = TRUE,
            xlog = TRUE,
            xlab = analysis_keep,
            txt_gp = fpTxtGp(label = txt_gp)
            ) %>%
        {
            header_list <- list(
                Gene = c(title, "Gene")
            )
            header_list[[analysis_keep]] <- c("", analysis_keep)
            do.call(fp_add_header, c(list(x = .), header_list))
        }
    
    botton_line <- paste0('h_', nrow(table) + 3, ' = gpar(lwd = 1, columns = 1:3, col = "black"))')
    eval(parse(text = paste0(
        'plot <- plot %>% fp_add_lines(h_3 = gpar(lwd = 1, columns = 1:3, col = "black"),', botton_line)))
    
    return(plot)        
    }