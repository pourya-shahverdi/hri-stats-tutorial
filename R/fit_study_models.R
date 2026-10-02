# Fit every model used in Part 2 (the Affective Dynamics study)
# --------------------------------------------------------------
# Run from the project folder in RStudio:
#   source("R/fit_study_models.R")
# It takes a few minutes and writes:
#   data/study_results.csv  main result for every research question and feeling
#   data/study_checks.csv   the same results under alternative model versions
#
# The pages read these files, because several ordinal mixed models take
# minutes to fit in a web browser.
#
# How each model was chosen
#   * Each RQ uses only the reports it needs (e.g. RQ1 = CB reports).
#   * Fixed effects: the comparison of interest + the design variables
#     (pair position, order group, trainer feedback style), sum-coded.
#   * Random effects: start with the richest structure that lets every
#     trainee (and every rehearsal) have their own version of the effect,
#     then simplify step by step only if the model cannot be estimated
#     (singular fit, failed convergence, or undefined standard errors).

suppressPackageStartupMessages({ library(lmerTest); library(ordinal) })
source("R/study_helpers.R")
d <- load_affect()

design_rq13 <- "pair_position + pb_order_group + feedback_condition"
trend_rq13  <- "loop_index + pb_order_group + feedback_condition"   # steady drift over the 6 rehearsals
design_rq4  <- "challenge_position + pb_order_group + feedback_condition"
trend_rq4   <- "as.numeric(challenge_position) + pb_order_group + feedback_condition"

challenging_cb <- subset(d, checkpoint == "child_behavior" & loop_type == "challenging")
challenging_cb$function_class <- factor(challenging_cb$function_class, levels = c("PR", "NR", "AR"))
challenging_cb$function_NR <- relevel(challenging_cb$function_class, ref = "NR")

rqs <- list(
  RQ1 = list(
    data = pick_reports(d, "child_behavior"),
    fixed = "loop_type", design = design_rq13, trend = trend_rq13,
    terms = c("loop_typechallenging"),
    lmm = c("(1 + challenge | participant_id)",
            "(1 + challenge || participant_id)",
            "(1 | participant_id)"),
    clmm = c("(1 | participant_id) + (0 + challenge | participant_id)",
             "(1 | participant_id)")),
  RQ2 = list(
    data = pick_reports(d, c("child_behavior", "self_handling")),
    fixed = "loop_type * checkpoint", design = design_rq13, trend = trend_rq13,
    terms = c("loop_typechallenging:checkpointself_handling"),
    lmm = c("(1 + challenge * sh || participant_id) + (1 | loop_id)",
            "(1 + challenge + sh || participant_id) + (1 | loop_id)",
            "(1 + challenge || participant_id) + (1 | loop_id)",
            "(1 | participant_id) + (1 | loop_id)"),
    clmm = c("(1 | participant_id) + (0 + challenge | participant_id) + (1 | loop_id)",
             "(1 | participant_id) + (1 | loop_id)",
             "(1 | participant_id) + (0 + challenge | participant_id)",
             "(1 | participant_id)")),
  RQ3 = list(
    data = pick_reports(d, c("self_handling", "post_feedback")),
    fixed = "loop_type * checkpoint", design = design_rq13, trend = trend_rq13,
    terms = c("loop_typechallenging:checkpointpost_feedback"),
    lmm = c("(1 + challenge * pf || participant_id) + (1 | loop_id)",
            "(1 + challenge + pf || participant_id) + (1 | loop_id)",
            "(1 + challenge || participant_id) + (1 | loop_id)",
            "(1 | participant_id) + (1 | loop_id)"),
    clmm = c("(1 | participant_id) + (0 + challenge | participant_id) + (1 | loop_id)",
             "(1 | participant_id) + (1 | loop_id)",
             "(1 | participant_id) + (0 + challenge | participant_id)",
             "(1 | participant_id)")),
  RQ4 = list(
    data = challenging_cb,
    fixed = "function_class", design = design_rq4, trend = trend_rq4,
    terms = c("function_classNR", "function_classAR"),
    lmm = c("(1 | participant_id)"),     # one report per trainee and function: an intercept is all we can estimate
    clmm = c("(1 | participant_id)"))
)

# ---- fitting with the simplification ladder --------------------------------

fit_lmm <- function(outcome, rhs, data) {
  suppressMessages(lmer(as.formula(paste(outcome, "~", rhs)), data = data))
}
lmm_ok <- function(m) !isSingular(m) && is.null(m@optinfo$conv$lme4$messages)

fit_clmm <- function(outcome, rhs, data) {
  f <- as.formula(paste0(outcome, "_ord ~ ", rhs))
  try_fit <- function(...) tryCatch(suppressWarnings(clmm(f, data = data, ...)),
                                    error = function(e) NULL)
  one_term <- length(gregexpr("\\|", rhs)[[1]]) == 1
  # Try a few fitting methods; keep the first one that converges properly
  attempts <- if (one_term) {
    list(list(control = clmm.control(method = "ucminf")), list(), list(nAGQ = 7))
  } else {
    list(list())   # models with several random terms only allow the default method
  }
  m <- NULL
  for (a in attempts) {
    m <- do.call(try_fit, a)
    if (clmm_ok(m)) return(m)
  }
  m
}
clmm_ok <- function(m) {
  if (is.null(m)) return(FALSE)
  se <- suppressWarnings(coef(summary(m)))[, 2]
  # Converged: tiny gradient at the end, and every standard error defined
  max(abs(m$gradient)) < 1e-3 && all(is.finite(se)) && all(se > 1e-3)
}

choose <- function(outcome, spec, data, kind, extra = spec$design) {
  ladder <- if (kind == "lmm") spec$lmm else spec$clmm
  for (re in ladder) {
    rhs <- paste(spec$fixed, "+", extra, "+", re)
    m <- if (kind == "lmm") fit_lmm(outcome, rhs, data) else fit_clmm(outcome, rhs, data)
    ok <- if (kind == "lmm") lmm_ok(m) else clmm_ok(m)
    if (ok) return(list(model = m, re = re))
  }
  stop("No random-effects structure could be estimated for ", outcome)
}

term_row <- function(m, term, kind) {
  cf <- suppressWarnings(coef(summary(m)))[term, ]
  if (kind == "lmm") {
    half <- qt(0.975, cf[["df"]]) * cf[["Std. Error"]]
    c(estimate = cf[["Estimate"]], lower = cf[["Estimate"]] - half,
      upper = cf[["Estimate"]] + half, p = cf[["Pr(>|t|)"]])
  } else {
    c(estimate = exp(cf[["Estimate"]]),
      lower = exp(cf[["Estimate"]] - 1.96 * cf[["Std. Error"]]),
      upper = exp(cf[["Estimate"]] + 1.96 * cf[["Std. Error"]]),
      p = cf[["Pr(>|z|)"]])
  }
}

refit <- function(outcome, spec, data, kind, re, extra) {
  rhs <- paste(spec$fixed, if (nzchar(extra)) paste("+", extra) else "", "+", re)
  if (kind == "lmm") fit_lmm(outcome, rhs, data) else fit_clmm(outcome, rhs, data)
}

results <- list(); checks <- list()
add <- function(lst, ...) { lst[[length(lst) + 1]] <- data.frame(...); lst }

for (rq in names(rqs)) {
  spec <- rqs[[rq]]
  for (v in c(sam_feelings, ordinal_feelings)) {
    kind <- if (v %in% sam_feelings) "lmm" else "clmm"
    cat(rq, v, "\n")
    best <- choose(v, spec, spec$data, kind)
    model_label <- if (kind == "lmm") "Linear mixed model" else "Ordinal mixed model (odds ratio)"

    # RQ4 also compares AR with NR (NR as the comparison group)
    fits <- list(main = best$model)
    if (rq == "RQ4") {
      fits$nr <- refit(v, modifyList(spec, list(fixed = "function_NR")), spec$data, kind,
                       best$re, spec$design)
    }

    for (term in c(spec$terms, if (rq == "RQ4") "function_NRAR")) {
      m <- if (term == "function_NRAR") fits$nr else fits$main
      sp <- if (term == "function_NRAR") modifyList(spec, list(fixed = "function_NR")) else spec
      r <- term_row(m, term, kind)
      results <- add(results, rq = rq, feeling = v, term = term, model = model_label,
                     random_effects = best$re, estimate = r[["estimate"]],
                     lower = r[["lower"]], upper = r[["upper"]], p = r[["p"]])

      # Alternative versions of the same model
      versions <- list(
        main = m,
        no_design = refit(v, sp, spec$data, kind, best$re, ""),
        trend = refit(v, sp, spec$data, kind, best$re, spec$trend),
        in_order_only = refit(v, sp, spec$data[!spec$data$participant_id %in% out_of_order, ],
                              kind, best$re, spec$design))
      for (nm in names(versions)) {
        mm <- versions[[nm]]
        ok <- if (kind == "lmm") TRUE else clmm_ok(mm)
        rr <- if (ok) term_row(mm, term, kind) else c(estimate = NA, lower = NA, upper = NA, p = NA)
        checks <- add(checks, rq = rq, feeling = v, term = term, version = nm,
                      estimate = rr[["estimate"]], lower = rr[["lower"]],
                      upper = rr[["upper"]], p = rr[["p"]])
      }

      # SAM feelings: does an ordinal model (treating -4..+4 as ordered categories) agree?
      if (kind == "lmm") {
        ord_re <- spec$clmm[length(spec$clmm)]           # participant intercept
        mo <- fit_clmm(v, paste(sp$fixed, "+", spec$design, "+", ord_re), spec$data)
        rr <- if (clmm_ok(mo)) term_row(mo, term, "clmm") else c(estimate = NA, lower = NA, upper = NA, p = NA)
        checks <- add(checks, rq = rq, feeling = v, term = term, version = "as_ordinal",
                      estimate = rr[["estimate"]], lower = rr[["lower"]],
                      upper = rr[["upper"]], p = rr[["p"]])
      }

      # Ordinal feelings: proportional-odds check (does the effect look the same at every cut-point?)
      if (kind == "clmm") {
        fixed_terms <- strsplit(gsub(" ", "", sp$fixed), "\\*|\\+")[[1]]
        po_data <- spec$data
        po_data$y <- po_data[[paste0(v, "_ord")]]
        mc <- clm(as.formula(paste("y ~", sp$fixed)), data = po_data)
        nt <- nominal_test(mc)
        key <- if (grepl(":", term)) "loop_type:checkpoint" else fixed_terms[1]
        p_po <- if (key %in% rownames(nt)) nt[key, "Pr(>Chi)"] else NA
        checks <- add(checks, rq = rq, feeling = v, term = term, version = "proportional_odds",
                      estimate = NA, lower = NA, upper = NA, p = p_po)
      }
    }

    # RQ4: one overall test per feeling ("do the three functions differ at all?")
    if (rq == "RQ4") {
      if (kind == "lmm") {
        p_all <- anova(best$model)["function_class", "Pr(>F)"]
      } else {
        m0 <- fit_clmm(v, paste(spec$design, "+", best$re), spec$data)
        lr <- 2 * (as.numeric(logLik(best$model)) - as.numeric(logLik(m0)))
        p_all <- pchisq(lr, df = 2, lower.tail = FALSE)
      }
      results <- add(results, rq = rq, feeling = v, term = "overall", model = model_label,
                     random_effects = best$re, estimate = NA, lower = NA, upper = NA, p = p_all)
    }
  }
}

results <- do.call(rbind, results)
checks  <- do.call(rbind, checks)
write.csv(results, "data/study_results.csv", row.names = FALSE)
write.csv(checks,  "data/study_checks.csv",  row.names = FALSE)
cat("Done: data/study_results.csv and data/study_checks.csv\n")
