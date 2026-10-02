# Helpers for the Affective Dynamics study pages
# ----------------------------------------------
# load_affect()    reads the N = 66 data and sets up factors
# pick_reports()   keeps only some of the three reports (CB, SH, PF)
# load_results()   reads the results fitted ahead of time by R/fit_study_models.R
# show_six(), plot_six()   table and plots of one result for all six feelings
# order_table()    the "What about order?" comparison for one research question
# group_means()    mean and standard error per group
# map_*()          pictures that show what each model coefficient means

sam_feelings     <- c("pleasure", "arousal", "dominance")   # -4 to +4, linear mixed model
ordinal_feelings <- c("confusion", "frustration", "boredom") # 1 to 5, ordinal mixed model
out_of_order     <- c("P003", "P005", "P012", "P021", "P058") # rehearsals not run in the planned order

load_affect <- function(path = "data/affect_n66.csv") {
  d <- read.csv(path)
  d$loop_type  <- factor(d$loop_type, levels = c("baseline", "challenging"))
  d$checkpoint <- factor(d$checkpoint,
                         levels = c("child_behavior", "self_handling", "post_feedback"))
  d$function_class <- factor(d$function_class, levels = c("baseline", "PR", "NR", "AR"))

  # 0/1 versions, used for random slopes
  d$challenge <- as.integer(d$loop_type == "challenging")       # 0 = baseline, 1 = challenging
  d$sh        <- as.integer(d$checkpoint == "self_handling")    # 1 = SH report
  d$pf        <- as.integer(d$checkpoint == "post_feedback")    # 1 = PF report

  # Design variables. "Sum coding" compares each level with the average of all
  # levels, so the model's intercept still means "the average baseline rating".
  for (v in c("pair_position", "pb_order_group", "feedback_condition", "challenge_position")) {
    d[[v]] <- factor(d[[v]])
    contrasts(d[[v]]) <- contr.sum(nlevels(d[[v]]))
  }

  # Boredom ratings of 4 and 5 were very rare, so they are combined into "4 or more"
  d$boredom <- pmin(d$boredom, 4)
  for (v in c(sam_feelings, ordinal_feelings)) {
    d[[paste0(v, "_ord")]] <- factor(d[[v]], ordered = TRUE)
  }
  d
}

# Keep only some reports, e.g. pick_reports(d, c("child_behavior", "self_handling")).
# The first report you list becomes the comparison report.
pick_reports <- function(data, reports) {
  x <- data[data$checkpoint %in% reports, ]
  x$checkpoint <- factor(x$checkpoint, levels = reports)
  x
}

# Short, readable names for the model terms
term_label <- function(term) {
  labels <- c(
    "loop_typechallenging" = "challenging vs baseline (CB)",
    "loop_typechallenging:checkpointself_handling" = "gap change, CB to SH",
    "loop_typechallenging:checkpointpost_feedback" = "gap change, SH to PF",
    "function_classNR" = "NR vs PR",
    "function_classAR" = "AR vs PR",
    "function_NRAR"    = "AR vs NR")
  out <- unname(labels[term])
  ifelse(is.na(out), term, out)
}

# Results fitted ahead of time (the ordinal models take minutes in a browser)
load_results <- function(path = "data/study_results.csv") read.csv(path)
load_checks  <- function(path = "data/study_checks.csv")  read.csv(path)

# One result for all six feelings
six_results <- function(results, rq, term) {
  res <- results[results$rq == rq & results$term == term, ]
  res$feeling <- factor(res$feeling, levels = rev(c(sam_feelings, ordinal_feelings)))
  res
}

# Print a rounded table
show_six <- function(res) {
  out <- res[, c("feeling", "estimate", "lower", "upper", "p", "random_effects")]
  out$estimate <- round(out$estimate, 2)
  out$lower <- round(out$lower, 2)
  out$upper <- round(out$upper, 2)
  out$p <- signif(out$p, 2)
  out <- out[order(match(out$feeling, c(sam_feelings, ordinal_feelings))), ]
  rownames(out) <- NULL
  out
}

# "What about order?": the main result next to three alternative versions
order_table <- function(checks, rq, term) {
  x <- checks[checks$rq == rq & checks$term == term &
              checks$version %in% c("main", "no_design", "trend", "in_order_only"), ]
  x$cell <- ifelse(is.na(x$p), "could not be fit",
                   sprintf("%.2f (p %s)", x$estimate,
                           ifelse(x$p < 0.001, "< .001",
                                  paste("=", sub("^0", "", sprintf("%.3f", x$p))))))
  w <- reshape(x[, c("feeling", "version", "cell")], idvar = "feeling",
               timevar = "version", direction = "wide")
  names(w) <- sub("cell.", "", names(w), fixed = TRUE)
  w <- w[match(c(sam_feelings, ordinal_feelings), w$feeling),
         c("feeling", "no_design", "main", "trend", "in_order_only")]
  names(w) <- c("feeling", "no order variables", "main model", "steady trend instead",
                "planned order only")
  rownames(w) <- NULL
  w
}

# Two small plots: SAM differences and ordinal odds ratios
plot_six <- function(res, sam_label = "Difference in SAM points", title = NULL) {
  library(ggplot2)
  res$clear <- ifelse(res$p < 0.05, "p < .05", "not clear")
  sam <- subset(res, model == "Linear mixed model")
  ord <- subset(res, model != "Linear mixed model")
  cols <- c("p < .05" = "#2166ac", "not clear" = "grey60")

  p1 <- ggplot(sam, aes(estimate, feeling, colour = clear)) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.2) +
    geom_point(size = 3) +
    scale_colour_manual(values = cols, drop = FALSE) +
    labs(x = sam_label, y = NULL, colour = NULL,
         title = "Pleasure, arousal, dominance", subtitle = "0 = no difference") +
    theme_minimal(base_size = 13)

  p2 <- ggplot(ord, aes(estimate, feeling, colour = clear)) +
    geom_vline(xintercept = 1, linetype = 2) +
    geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.2) +
    geom_point(size = 3) +
    scale_x_log10() +
    scale_colour_manual(values = cols, drop = FALSE) +
    labs(x = "Odds ratio (log scale)", y = NULL, colour = NULL,
         title = "Confusion, frustration, boredom", subtitle = "1 = no difference") +
    theme_minimal(base_size = 13)

  print(p1)
  print(p2)
}

# Mean and standard error for each group, for descriptive plots
group_means <- function(data, outcome, by) {
  agg <- aggregate(data[[outcome]], data[by], function(x)
    c(mean = mean(x), se = sd(x) / sqrt(length(x))))
  out <- cbind(agg[by], as.data.frame(agg$x))
  out$feeling <- outcome
  out
}

# ---- Pictures of what the coefficients mean -------------------------------

report_names <- c(child_behavior = "Child's behavior (CB)",
                  self_handling  = "Own handling (SH)",
                  post_feedback  = "After feedback (PF)")
rehearsal_cols <- c(baseline = "#4393c3", challenging = "#d6604d")

# RQ1: one report, two kinds of rehearsal
map_rq1 <- function(data, outcome = "pleasure") {
  library(ggplot2)
  m <- group_means(subset(data, checkpoint == "child_behavior"), outcome, "loop_type")
  b <- m$mean[m$loop_type == "baseline"]; ch <- m$mean[m$loop_type == "challenging"]
  ggplot(m, aes(loop_type, mean, colour = loop_type)) +
    geom_hline(yintercept = b, linetype = 3, colour = "grey50") +
    geom_point(size = 5) +
    annotate("segment", x = 2, xend = 2, y = b, yend = ch,
             arrow = arrow(length = unit(0.25, "cm")), linewidth = 1) +
    annotate("label", x = 1, y = b, vjust = -0.6, size = 4.2,
             label = sprintf("Intercept = %.2f\n(baseline average)", b)) +
    annotate("label", x = 1.5, y = (b + ch) / 2, size = 4.2,
             label = sprintf("Slope = %.2f\n(loop_typechallenging)", ch - b)) +
    scale_colour_manual(values = rehearsal_cols, guide = "none") +
    scale_y_continuous(expand = expansion(mult = 0.35)) +
    labs(x = NULL, y = paste("Average", outcome)) +
    theme_minimal(base_size = 14)
}

# RQ2 and RQ3: two reports, two kinds of rehearsal (a 2 x 2 picture)
map_two_reports <- function(data, outcome, reports) {
  library(ggplot2)
  sub <- droplevels(subset(data, checkpoint %in% reports))
  m <- group_means(sub, outcome, c("loop_type", "checkpoint"))
  m$x <- match(m$checkpoint, reports)
  get <- function(lt, cp) m$mean[m$loop_type == lt & m$checkpoint == cp]
  b1 <- get("baseline", reports[1]);   b2 <- get("baseline", reports[2])
  c1 <- get("challenging", reports[1]); c2 <- get("challenging", reports[2])
  ggplot(m, aes(x, mean, colour = loop_type)) +
    geom_line(linewidth = 1) + geom_point(size = 4) +
    annotate("segment", x = 0.92, xend = 0.92, y = b1, yend = c1,
             arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
    annotate("segment", x = 2.08, xend = 2.08, y = b2, yend = c2,
             arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
    annotate("label", x = 0.88, y = (b1 + c1) / 2, hjust = 1, size = 3.8,
             label = sprintf("gap = %.2f", c1 - b1)) +
    annotate("text", x = 1, y = b1, vjust = -1.2, size = 3.8,
             label = sprintf("Intercept = %.2f", b1)) +
    annotate("label", x = 2.12, y = (b2 + c2) / 2, hjust = 0, size = 3.8,
             label = sprintf("gap = %.2f", c2 - b2)) +
    scale_x_continuous(breaks = 1:2, labels = report_names[reports],
                       limits = c(0.3, 2.7)) +
    scale_colour_manual(values = rehearsal_cols) +
    scale_y_continuous(expand = expansion(mult = c(0.08, 0.2))) +
    labs(x = NULL, y = paste("Average", outcome), colour = "Rehearsal",
         title = sprintf("Interaction = change in the gap = %.2f - (%.2f) = %.2f",
                         c2 - b2, c1 - b1, (c2 - b2) - (c1 - b1))) +
    theme_minimal(base_size = 13) + theme(legend.position = "bottom")
}

# RQ4: three functions of challenging behavior
map_rq4 <- function(data, outcome = "pleasure") {
  library(ggplot2)
  sub <- subset(data, checkpoint == "child_behavior" & loop_type == "challenging")
  sub$function_class <- factor(sub$function_class, levels = c("PR", "NR", "AR"))
  m <- group_means(sub, outcome, "function_class")
  pr <- m$mean[m$function_class == "PR"]
  m$label <- ifelse(m$function_class == "PR", sprintf("Intercept = %.2f", pr),
                    sprintf("function_class%s = %.2f", m$function_class, m$mean - pr))
  ggplot(m, aes(function_class, mean)) +
    geom_hline(yintercept = pr, linetype = 3, colour = "grey50") +
    geom_segment(aes(xend = function_class, y = pr, yend = mean),
                 arrow = arrow(length = unit(0.2, "cm")), colour = "grey40") +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1, colour = "#d6604d") +
    geom_point(size = 4, colour = "#d6604d") +
    geom_label(aes(label = label), nudge_x = 0.35, size = 3.6, hjust = 0) +
    scale_x_discrete(labels = c(PR = "PR\n(demanding)", NR = "NR\n(screaming, avoiding)",
                                AR = "AR\n(repetition)"),
                     expand = expansion(add = c(0.5, 1.3))) +
    labs(x = NULL, y = paste("Average", outcome)) +
    theme_minimal(base_size = 14)
}
