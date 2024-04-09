# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

crumble_vars <- new_class("crumble_vars",
  properties = list(
    A = class_character,
    Y = class_character,
    M = class_character,
    Z = class_character,
    W = class_character,
    C = new_property(class = class_character, default = "")
  ),
  validator = function(self) {
    if (length(self@A) != 1) {
      "trt must be length 1"
    } else if (length(self@Y) != 1) {
      "outcome must be length 1"
    } else if (length(self@C) != 1) {
    	"cens must be length 1"
    }
  }
)
