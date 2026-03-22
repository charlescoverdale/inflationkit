# Ensure .Random.seed exists before any test runs.
# The package's sample_data functions save/restore .Random.seed,
# which fails if no RNG has been used yet in the session.
if (!exists(".Random.seed", envir = globalenv())) {
  set.seed(NULL)
}
