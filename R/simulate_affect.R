# Simulated teaching data shaped like the N66 affect study
# ---------------------------------------------------------
# These data are SIMULATED. They copy the design of the real study
# (66 participants x 6 rehearsals x 3 reporting contexts) but contain no
# participant data. Effect sizes are loosely inspired by the real results
# so the numbers feel familiar; they will not match the paper.
#
# Regenerate the CSV used by the tutorial with:
#   source("R/simulate_affect.R")
#   write.csv(simulate_affect(), "data/affect_sim.csv", row.names = FALSE)

simulate_affect <- function(n_participants = 66, seed = 2026) {
  set.seed(seed)

  contexts <- c("child_behavior", "self_handling", "post_feedback")
  scenario_orders <- list(
    A = c("PR", "NR", "AR"),
    B = c("NR", "AR", "PR"),
    C = c("AR", "PR", "NR")
  )
  ids <- sprintf("S%03d", seq_len(n_participants))
  order_group <- rep(names(scenario_orders), length.out = n_participants)
  feedback <- sample(rep(c("wording_1", "wording_2"), length.out = n_participants))

  # ---- Design: 3 baseline-challenge pairs per participant, 3 reports each ----
  design <- do.call(rbind, lapply(seq_len(n_participants), function(i) {
    pair <- rep(1:3, each = 2)
    loop <- rep(c("baseline", "challenge"), times = 3)
    fun  <- ifelse(loop == "challenge", scenario_orders[[order_group[i]]][pair], "none")
    reh <- data.frame(
      participant_id     = ids[i],
      pb_order_group     = order_group[i],
      feedback_condition = feedback[i],
      rehearsal_id       = sprintf("%s_R%d", ids[i], 1:6),
      pair_position      = pair,
      loop_type          = loop,
      function_class     = fun
    )
    reh <- reh[rep(1:6, each = 3), ]
    reh$checkpoint <- rep(contexts, times = 6)
    reh
  }))
  rownames(design) <- NULL
  design$challenge <- as.integer(design$loop_type == "challenge")

  n  <- nrow(design)
  p_idx <- match(design$participant_id, ids)
  ri <- match(design$rehearsal_id, unique(design$rehearsal_id))
  ci <- match(design$checkpoint, contexts)

  # Correlated participant intercepts and challenge slopes
  person_effects <- function(sd0, sd1, rho) {
    z0 <- rnorm(n_participants)
    z1 <- rho * z0 + sqrt(1 - rho^2) * rnorm(n_participants)
    cbind(u0 = sd0 * z0, u1 = sd1 * z1)
  }

  # Challenge effect for each row: context-specific gap + scenario shift
  # (scenario shifts apply to child-behavior reports only)
  challenge_part <- function(gap, fun_eff, u1) {
    scen <- ifelse(design$checkpoint == "child_behavior" & design$challenge == 1,
                   fun_eff[design$function_class], 0)
    scen[is.na(scen)] <- 0
    design$challenge * (gap[ci] + u1[p_idx]) + scen
  }

  # ---- SAM outcomes: 9 ordered values, -4 to +4 ----
  sam <- function(b0, ctx, gap, fun_eff, sd0, sd1, rho, sd_reh, sd_e) {
    u <- person_effects(sd0, sd1, rho)
    latent <- b0 + ctx[ci] + u[p_idx, "u0"] + rnorm(length(unique(ri)), 0, sd_reh)[ri] +
      challenge_part(gap, fun_eff, u[, "u1"]) + rnorm(n, 0, sd_e)
    as.integer(pmin(4, pmax(-4, round(latent))))
  }

  # ---- Educational emotions: 5 ordered values, 1 to 5 (cumulative logit) ----
  ord5 <- function(thresholds, gap, fun_eff, sd0, sd1, rho, sd_reh) {
    u <- person_effects(sd0, sd1, rho)
    eta <- u[p_idx, "u0"] + rnorm(length(unique(ri)), 0, sd_reh)[ri] +
      challenge_part(gap, fun_eff, u[, "u1"])
    latent <- eta + rlogis(n)
    as.integer(1 + rowSums(outer(latent, thresholds, ">")))
  }

  design$pleasure <- sam(
    b0 = 2.7, ctx = c(0, -0.1, -0.1), gap = c(-1.8, -0.35, -0.3),
    fun_eff = c(PR = 0.3, NR = -0.9, AR = 0.6),
    sd0 = 1.2, sd1 = 1.1, rho = -0.4, sd_reh = 0.4, sd_e = 1.2
  )
  design$arousal <- sam(
    b0 = 0.2, ctx = c(0, -0.1, -0.15), gap = c(0.55, 0.1, 0),
    fun_eff = c(PR = -0.2, NR = 0.3, AR = -0.1),
    sd0 = 1.5, sd1 = 0.6, rho = 0, sd_reh = 0.3, sd_e = 1.2
  )
  design$dominance <- sam(
    b0 = 2.2, ctx = c(0, -0.15, -0.3), gap = c(-1.35, -0.7, -0.3),
    fun_eff = c(PR = 0.15, NR = -0.4, AR = 0.25),
    sd0 = 1.3, sd1 = 0.9, rho = -0.3, sd_reh = 0.4, sd_e = 1.2
  )
  design$confusion <- ord5(
    thresholds = c(2.0, 3.5, 5.0, 6.5), gap = c(2.8, 2.3, 0.8),
    fun_eff = c(PR = -0.3, NR = 0.3, AR = 0.2),
    sd0 = 1.8, sd1 = 1.0, rho = 0, sd_reh = 0.5
  )
  design$frustration <- ord5(
    thresholds = c(3.5, 5.0, 6.5, 8.0), gap = c(3.2, 2.6, 1.4),
    fun_eff = c(PR = 0.1, NR = 0.4, AR = -0.4),
    sd0 = 2.0, sd1 = 1.2, rho = 0, sd_reh = 0.5
  )
  design$boredom <- ord5(
    thresholds = c(2.5, 4.0, 5.5, 7.0), gap = c(-1.5, -1.2, -0.6),
    fun_eff = c(PR = 0, NR = 0.3, AR = -0.3),
    sd0 = 2.2, sd1 = 0.8, rho = 0, sd_reh = 0.5
  )

  design
}
