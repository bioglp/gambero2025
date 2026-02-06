corr_M <- function (df, var, ignore = NA, trim = 0, clean = FALSE, plot = TRUE,
                    top = NA, ceiling = 100, max_pvalue = 1, limit = 10, ranks = FALSE,
                    zeroes = FALSE, save = FALSE, quiet = FALSE, ...)
{
    vars <- enquos(var)
    var <- as_label(vars[[1]])
    df <- select(df, -contains(paste0(var, "_log")))
    rs <- corr(df, ignore = ignore, limit = limit, pvalue = TRUE,
               ...)
    if (!var %in% colnames(rs$cor)) {
        msg <- paste("Not a valid input:", var, "was transformed or does not exist.")
        maybes <- colnames(rs$cor)[grepl(var, colnames(rs$cor))]
        if (length(maybes) > 0 & maybes[1] %in% colnames(rs$cor)) {
            if (!quiet)
                warning(sprintf("Maybe you meant one of: %s",
                                vector2text(head(maybes, 10))))
            if (!quiet)
                message(sprintf("Automatically using '%s", maybes[1]))
            var <- maybes[1]
            fixable <- TRUE
        }
        else fixable <- FALSE
        if (fixable)
            warning(msg)
        else stop(msg)
    }
    d <- data.frame(variables = colnames(rs$cor), corr = rs$cor[,
                                                                c(var)], pvalue = rs$pvalue[, c(var)])
    d <- d[(d$corr < 1 & !is.na(d$corr)), ]
    d <- d[order(-abs(d$corr)), ]
    original_n <- nrow(d)
    if (!zeroes)
        d <- d[d$corr != 0, ]
    if (max_pvalue < 1)
        d <- d %>% mutate(pvalue = as.numeric(ifelse(is.na(.data$pvalue),
                                                     1, .data$pvalue))) %>% filter(.data$pvalue <= max_pvalue)
    if (is.na(top) & nrow(d) > 30) {
        top <- 30
        if (!quiet)
            message(paste("Automatically reduced results to the top",
                          top, "variables.", "Use the 'top' parameter to override this limit."))
    }
    if (ceiling < 100) {
        d <- d[abs(d$corr) < ceiling/100, ]
        if (!quiet)
            message(paste0("Removing all correlations greater than ",
                           ceiling, "% (absolute)"))
    }
    d <- d[complete.cases(d), ]
    if (!is.na(top))
        d <- head(d, top)
    if (trim > 0) {
        d$variables <- substr(d$variables, 1, trim)
        if (!quiet)
            message(paste("Trimmed all name values into", trim,
                          "characters"))
    }
    if (ranks)
        d <- mutate(d, variables = sprintf("%s. %s", row_number(),
                                           .data$variables))
    if (plot) {
        p <- ungroup(d) %>% filter(.data$variables != "pvalue") %>%
            mutate(pos = ifelse(.data$corr > 0, TRUE, FALSE),
                   hjust = ifelse(abs(.data$corr) < max(abs(.data$corr))/1.5,
                                  -0.1, 1.1)) %>% ggplot(aes(x = reorder(.data$variables,
                                                                         abs(.data$corr)), y = abs(.data$corr), fill = .data$pos,
                                                             label = signif(100 * .data$corr, 3))) + geom_hline(yintercept = 0,
                                                                                                                alpha = 0.5) + geom_col(colour = "transparent") +
            coord_flip() + geom_text(aes(hjust = .data$hjust),
                                     size = 3, colour = "black") + scale_fill_manual(values = c(`FALSE` = "#E5586E",
                                                                                                `TRUE` = "#59B3D2")) + guides(fill = FALSE) + labs(title = paste("Correlations of",
                                                                                                                                                                 var, "[%]"), x = NULL, y = NULL) + scale_y_percent(expand = c(0,
                                                                                                                                                                                                                               0), position = "right") + theme_lares(pal = 2)
        if (!is.na(top) & top < original_n)
            p <- p + labs(subtitle = 'Males')
        if (max_pvalue < 1)
            p <- p + labs(caption = paste("Correlations with p-value <",
                                          max_pvalue))
    }
    if (plot)
        return(p)
    else return(d)
}
