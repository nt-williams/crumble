crumble_control <- S7::new_class("crumble_control",
  properties = list(
    crossfit_folds = S7::new_property(S7::class_integer, default = 10L),
    mlr3superlearner_folds = S7::new_property(S7::class_integer, default = 10L),
    zprime_folds = S7::new_property(S7::class_integer, default = 1L),
    epochs = S7::new_property(S7::class_integer, default = 100L),
    learning_rate = S7::new_property(S7::class_numeric, default = 0.01)
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
