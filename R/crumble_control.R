crumble_control <- new_class("crumble_control",
  properties = list(
    crossfit_folds = new_property(class_integer, default = 10L),
    mlr3superlearner_folds = new_property(class_integer, default = 10L),
    epochs = new_property(class_integer, default = 100L),
    learning_rate = new_property(class_numeric, default = 0.01)
  ),
  validator = function(self) {
    if (length(self@crossfit_folds) != 1) {
      "@crossfit_folds must be length 1"
    } else if (length(self@mlr3superlearner_folds) != 1) {
      "@mlr3superlearner_folds must be length 1"
    } else if (self@mlr3superlearner_folds < 2) {
      "@mlr3superlearner_folds must be greater than 1"
    }
  }
)
