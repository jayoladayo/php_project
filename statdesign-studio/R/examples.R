# Class examples, copied from the course R scripts.

CLASS_EXAMPLES <- list(
  rcbd_coded = list(
    title = "Maize fertilizer trial (coded)",
    design = "RCBD",
    desc = "The RCBD class data with farms and fertilizers stored as codes 1, 2, 3, as they often arrive from Excel.",
    data = data.frame(
      Farm = rep(1:3, each = 3),
      Fertilizer = rep(1:3, times = 3),
      Yield = c(50, 48, 60, 55, 47, 62, 53, 49, 61),
      `Plot ID` = sprintf("P%02d", 1:9),
      check.names = FALSE
    )
  ),
  fact = list(
    title = "Factorial: yield by temperature and pressure",
    design = "FACT",
    desc = "Two factors (3 temperatures × 3 pressures), two replicates of every combination, completely randomised (Montgomery, Problem 5.x).",
    data = data.frame(
      Temperature = rep(c(150, 160, 170), times = 6),
      Pressure = rep(rep(c(200, 215, 230), each = 3), times = 2),
      Replicate = rep(1:2, each = 9),
      Yield = c(90.4, 90.1, 90.5, 90.7, 90.5, 90.8, 90.2, 89.9, 90.4, 90.2, 90.3, 90.7, 90.6, 90.6, 90.9, 90.4, 90.1, 90.1)
    )
  ),
  crd = list(
    title = "CRD: crop yield by fertilizer",
    design = "CRD",
    desc = "Three fertilizers (A, B, C), three plots each, fully randomised.",
    data = data.frame(
      Fertilizer = rep(c("A", "B", "C"), each = 3),
      Yield = c(50, 55, 53, 48, 47, 49, 60, 62, 61)
    )
  ),
  rcbd = list(
    title = "RCBD: crop yield by fertilizer and block",
    design = "RCBD",
    desc = "Three fertilizers tested once in each of three blocks.",
    data = data.frame(
      Fertilizer = rep(c("A", "B", "C"), times = 3),
      Block = rep(1:3, each = 3),
      Yield = c(50, 48, 60, 55, 47, 62, 53, 49, 61)
    )
  ),
  lsd = list(
    title = "Latin Square: crop yield",
    design = "LSD",
    desc = "Three treatments arranged in a 3 × 3 Latin square of rows and columns.",
    data = data.frame(
      Treatment = c("A", "B", "C", "B", "C", "A", "C", "A", "B"),
      Row = rep(1:3, times = 3),
      Column = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
      Yield = c(50, 48, 60, 55, 47, 62, 53, 49, 61)
    )
  ),
  glsd = list(
    title = "Graeco-Latin Square: crop yield",
    design = "GLSD",
    desc = "Crop variety (Latin) and fertilizer type (Greek) in a 4 × 4 square of soil quality and irrigation.",
    data = data.frame(
      SoilQuality = rep(c("High", "Medium-High", "Medium-Low", "Low"), each = 4),
      Irrigation = rep(c("Very Low", "Low", "Medium", "High"), times = 4),
      FertilizerType = c("Organic", "Chemical", "Mixed", "Untreated",
                         "Untreated", "Organic", "Chemical", "Mixed",
                         "Chemical", "Mixed", "Untreated", "Organic",
                         "Organic", "Chemical", "Mixed", "Untreated"),
      Treatment = c("A", "B", "C", "D", "B", "C", "D", "A",
                    "C", "D", "A", "B", "D", "A", "B", "C"),
      Yield = c(15.2, 14.8, 15.0, 14.9, 14.7, 15.3, 14.6, 15.1,
                15.0, 14.9, 15.4, 14.8, 14.6, 15.2, 14.7, 15.3)
    )
  )
)

example_dataset <- function(key) {
  ex <- CLASS_EXAMPLES[[key]]
  new_dataset(ex$title, ex$data, source = "Class example")
}
