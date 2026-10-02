# Simulated data for the introductory chapters
# --------------------------------------------
# Two small, made-up human-robot interaction studies. No real participants.
#
#   one_visit: 40 people each meet ONE robot once (neutral or expressive)
#              and rate their trust on a 0-100 slider.
#   sessions:  30 people each meet the SAME robot (neutral or expressive)
#              in 5 weekly sessions. After every session they rate trust on
#              a 0-100 slider and answer one 1-5 agreement item:
#              "I would trust this robot to help me."
#
# Regenerate the CSVs with:
#   source("R/simulate_tutorial_data.R")
#   write_tutorial_data()

clamp <- function(x, lo, hi) pmin(hi, pmax(lo, x))

simulate_one_visit <- function(n_per_group = 20, seed = 11) {
  set.seed(seed)
  robot <- rep(c("neutral", "expressive"), each = n_per_group)
  true_mean <- ifelse(robot == "expressive", 66, 55)
  data.frame(
    participant_id = sprintf("P%02d", seq_along(robot)),
    robot = robot,
    trust = round(clamp(rnorm(length(robot), true_mean, 15), 0, 100))
  )
}

simulate_sessions <- function(n_per_group = 15, n_sessions = 5, seed = 22,
                              robot_effect = 8, growth = 3, extra_growth = 1.5) {
  set.seed(seed)
  n <- 2 * n_per_group
  people <- data.frame(
    participant_id = sprintf("S%02d", 1:n),
    robot = rep(c("neutral", "expressive"), each = n_per_group),
    own_start = rnorm(n, 0, 10),   # some people trust robots more than others
    own_growth = rnorm(n, 0, 1.5)  # some warm up faster than others
  )
  d <- people[rep(1:n, each = n_sessions), ]
  d$session <- rep(1:n_sessions, times = n)
  expressive <- as.integer(d$robot == "expressive")
  time <- d$session - 1
  hidden_trust <- 48 + robot_effect * expressive + d$own_start +
    (growth + extra_growth * expressive + d$own_growth) * time +
    rnorm(nrow(d), 0, 6)
  d$trust <- round(clamp(hidden_trust, 0, 100))
  # The 1-5 item is a coarse view of the same hidden trust, plus some noise
  d$agree <- as.integer(cut(hidden_trust + rnorm(nrow(d), 0, 5),
                            breaks = c(-Inf, 40, 52, 64, 76, Inf),
                            labels = FALSE))
  rownames(d) <- NULL
  d[, c("participant_id", "robot", "session", "trust", "agree")]
}

write_tutorial_data <- function(dir = "data") {
  write.csv(simulate_one_visit(), file.path(dir, "robot_one_visit.csv"), row.names = FALSE)
  write.csv(simulate_sessions(), file.path(dir, "robot_sessions.csv"), row.names = FALSE)
}
