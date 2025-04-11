#' Find the cutoff for a given column in a data frame
#'
#' This function finds the cutoff for a given column in a data frame that maximizes the difference in survival between two groups.
#' It fits a Cox proportional hazards model and finds the cutoff that maximizes the log-rank test p-value.
#'
#' @param df A data frame containing the data.
#' @param column The column name in the data frame that contains the grouping variable. 
#' @param time The column name in the data frame that contains the time variable.
#' @param event The column name in the data frame that contains the event variable.
#' @param set_cutoff A numeric value specifying the cutoff to use. If NULL, the function will find the cutoff that maximizes the log-rank test p-value.
#' @param covariates A vector of column names in the data frame that contains the covariates.
#' @param interquartile Logical indicating whether to use the interquartile range to set the cutoff.
#' @return A numeric value specifying the cutoff.
#' @export
find_coxph_cutoff <- function(df, column, time, event, set_cutoff = NULL, covariates = NULL, interquartile = T){

    #split data at 200 intervals and find the cutoff with most significant difference in survival
    if(length(covariates) > 0){
        covariates_coxph <- paste0(" + ", paste0(covariates, collapse = " + "))
        print(paste0("set covariates ===> ", covariates_coxph))}
    else{
        covariates_coxph <- ""}
    cutoff <- NULL

    if(interquartile){
        lower <- quantile(df[[column]])[[2]]
        upper <- quantile(df[[column]])[[4]]}
    else{
        lower <- min(df[[column]])
    upper <- max(df[[column]])}

    no <- nrow(df)

    for (split in seq(lower, upper, length.out = 200)) {
        df[["coxph"]] <- ifelse(df[[column]] > split, "High", "Low")
        df[["coxph"]] <- factor(df[["coxph"]], levels = c("Low", "High"))
        remove <- any(table(df$coxph) < no / 20)
        if (remove) {
            next
        }
        surv.cox <- coxph(as.formula(paste0("Surv(", time, ",", event, ") ~ coxph", covariates_coxph)), data = df)
        
        # Extract HR and p-value
        hr <- round(exp(coef(surv.cox))[1], 3)  # Hazard Ratio
        logr <- round(summary(surv.cox)$coefficients[1, "Pr(>|z|)"], 3)  # p-value
        
        cutoff <- rbind(cutoff, c(split, hr, logr))
        colnames(cutoff) <- c("cutoff", "HR", "logr")
    }


    # use most significant cutoff to split data in "High" & "Low" groups
    cutoff <- as.data.frame(cutoff) %>% arrange(logr)
    if(length(set_cutoff) == 0){
        threshold <- cutoff$cutoff[1]}
    else if(length(set_cutoff) == 1){
        threshold <- set_cutoff}
    else{
    error("set_cutoff should be set to length of 1")}
    print(paste0("for <", column, ">, cutoff is set at  => ", threshold))
    df[[paste0(column, "_coxph")]] <- ifelse(df[[column]] > paste0(threshold), "High", "Low")
    df[[paste0(column, "_coxph")]] <- factor(df[[paste0(column, "_coxph")]], levels=c("Low", "High"))
    df$coxph <- NULL
    return(df)
}

#' Plot survival curves
#'
#' This function plots survival curves for a given column in a data frame.
#' It fits a survival model and creates a Kaplan-Meier plot with a risk table.
#'
#' @param df A data frame containing the data.
#' @param column The column name in the data frame that contains the grouping variable.
#' @param time The column name in the data frame that contains the time variable.   
#' @param event The column name in the data frame that contains the event variable.
#' @param title The title of the plot.
#' @param plot Logical indicating whether to plot the survival curves.
#' @param pval.coord The coordinates of the p-value text.
#' @param legend.position The position of the legend.
#' @param ... Additional arguments to pass to the ggsurvplot function.
#' @return A ggplot object.
#' @export
plot_survival <- function(df, column, time, event, title = "", plot = T, pval.coord = c(0.1, 0.1), legend.position = c(0.95, 0.425), ...){
    
    stopifnot(all(c(column, time, event) %in% colnames(df)))
    stopifnot(is.factor(df[[column]]))
    stopifnot(length(unique(df[[column]])) > 1)

    b <- as.formula(paste0("Surv(", time, ",", event, ") ~ ", column))
    fit <- eval(substitute(survfit(b, data = df, conf.type = "log-log"), list(b = b)))

    names(fit$strata) <- gsub(".*=", "", names(fit$strata))
    names(fit$strata) <- factor(names(fit$strata), levels = levels(df[[column]]))
    
    kmplot <- ggsurvplot(
            fit,
            data = df,
            risk.table = T,
            pval = T,
            risk.table.y.text.col = T,
            risk.table.y.text = F,
            legend = "right",
            pval.coord = pval.coord,
            ...)

    kmplot[[1]] <- kmplot[[1]] + 
            ggtitle(title) +
            theme_border() +
            theme_text() + 
            theme(
                legend.title = element_blank(),
                legend.background = element_rect(fill = "white", color = "black"),
                legend.position = legend.position, # Adjust the position inside the plot
                legend.justification = c(1, 0)) + 
            coord_cartesian(clip = "off")

    kmplot[[2]] <- kmplot[[2]] + 
    #        theme_border() 
            no_gridlines() +
            xlab(NULL) +
            theme(
                panel.border = element_rect(fill = NA, color = "black", size = 0.8),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank())

    if(plot){
        kmplot <- plot_grid(kmplot[[1]], kmplot[[2]], ncol = 1, rel_heights = c(2.6,1), align = "hv")}
    return(kmplot)}