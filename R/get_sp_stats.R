#' Get statistical test results for the plotted data
#'
#' @param df data frame containing the data
#' @param rep_summary summary statistic to use for testing
#' @param cond column name for the condition
#' @param repl column name for the replicate
#' @param ncond number of unique values in cond
#' @param nrepl number of unique values in repl
#' @param stats_test string for statistical test to use, select
#' ("para_unpaired", "para_paired", "nonpara_unpaired", or "nonpara_paired")
#'
#' @returns nothing, prints results to console
#' @importFrom stats t.test wilcox.test kruskal.test TukeyHSD
#' @export
get_sp_stats <-  function(df, rep_summary, cond, repl, ncond, nrepl,
                          stats_test) {
  # if ncond is 1, then we can't do any tests
  if (ncond == 1) {
    cat("Only one condition, no statistical tests performed\n")
    return(NULL)
  }
  # if nrepl is less than three, then we can't do any tests
  if (nrepl < 3) {
    cat("Less than three replicates, no statistical tests performed\n")
    return(NULL)
  }
  # if ncond is 2 and stats_test is para_unpaired or para_paired,
  # then we do a t-test
  if (ncond == 2 & stats_test %in% c("para_unpaired", "para_paired")) {
    x <- df[df[[cond]] == unique(df[[cond]])[1], rep_summary]
    y <- df[df[[cond]] == unique(df[[cond]])[2], rep_summary]
    cat("Performing t-test\n")
    if (stats_test == "para_unpaired") {
      ttest <- t.test(x, y)
    } else {
      ttest <- t.test(x, y, paired = TRUE)
    }
    print(ttest)
  }
  # if ncond is 2 and stats_test is nonpara_unpaired or nonpara_paired,
  # then we do a Wilcoxon test
  if (ncond == 2 & stats_test %in% c("nonpara_unpaired", "nonpara_paired")) {
    x <- df[df[[cond]] == unique(df[[cond]])[1], rep_summary]
    y <- df[df[[cond]] == unique(df[[cond]])[2], rep_summary]
    cat("Performing Wilcoxon test\n")
    if (stats_test == "nonpara_unpaired") {
      wilcox <- wilcox.test(x, y)
    } else {
      wilcox <- wilcox.test(x, y, paired = TRUE)
    }
    print(wilcox)
  }
  # if ncond is greater than 2 and stats_test is para_unpaired or para_paired,
  # then we do an ANOVA followed by Tukey's HSD test
  if (ncond > 2 & stats_test %in% c("para_unpaired", "para_paired")) {
    if (stats_test == "para_unpaired") {
      cat("Performing one-way ANOVA\n")
      aov <- aov(df[[rep_summary]] ~ df[[cond]])
      print(aov)
      print(summary(aov))
      # if Pr is < 0.05, then we do Tukey's HSD test
      if (summary(aov)[[1]][["Pr(>F)"]][1] < 0.05) {
        cat("ANOVA significant, performing Tukey's HSD test\n")
        print(TukeyHSD(aov))
      } else {
        cat("ANOVA not significant, no Tukey's HSD test performed\n")
      }
    } else {
      cat("Performing repeated measures ANOVA\n")
      aov <- aov(df[[rep_summary]] ~ df[[cond]] + Error(df[[repl]]))
      print(aov)
      print(summary(aov))
      # if Pr is < 0.05, then we do Tukey's HSD test
      if (summary(aov)[[2]][[1]]["Pr(>F)"][[1]][1] < 0.05) {
        cat("Pr < 0.05, perform multiple comparisons manually\n")
      } else {
        cat("ANOVA not significant\n")
      }
    }
  }
  # if ncond is greater than 2 and stats_test is nonpara_unpaired or
  # nonpara_paired, then we shoulddo a Kruskal-Wallis test followed by Dunn's
  if (ncond > 2 & stats_test %in% c("nonpara_unpaired", "nonpara_paired")) {
    if (stats_test == "nonpara_unpaired") {
      cat("Performing Kruskal-Wallis test\n")
      kruskal <- kruskal.test(df[[rep_summary]] ~ df[[cond]])
      print(kruskal)
      # if p-value is < 0.05, then we do Dunn's test
      if (kruskal$p.value < 0.05) {
        cat("Pr < 0.05, perform multiple comparisons (Dunn test) manually\n")
      } else {
        cat("Kruskal-Wallis test not significant\n")
      }
    } else {
      cat("Selected nonpara-paired, and there are more than 2 groups. Please
          consider performing a Friedman test manually\n")
    }
  }
}
