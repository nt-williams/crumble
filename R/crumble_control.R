crumble_control <- new_class("crumble_control",
  properties = list(
    crossfit_folds = new_property(class_integer, default = 10L),
    learners_propensity = new_property(class_character, default = "glm"),
    learners_propensity2 = new_property(class_character, default = "glm"),
    learners_outcome = new_property(class_character, default = "glm")
  ),
  validator = function(self) {
    if (length(self@crossfit_folds) != 1) {
      "@crossfit_folds must be length 1"
    }
  }
)
