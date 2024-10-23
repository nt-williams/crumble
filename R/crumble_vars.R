# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
#' @importFrom stats na.omit weighted.mean var qnorm dist model.matrix predict setNames
NULL

crumble_vars <- S7::new_class("crumble_vars",
  properties = list(
    A = S7::class_character,
    Y = S7::class_character,
    M = S7::class_character,
    Z = S7::class_character,
    W = S7::class_character,
    C = S7::new_property(class = S7::class_character, default = NA_character_),
    id = S7::new_property(class = S7::class_character, default = NA_character_)
  ),
  validator = function(self) {
     if (length(self@Y) != 1) {
      "self@outcome must be length 1"
    } else if (length(self@C) != 1) {
    	"self@cens must be length 1"
    } else if (length(self@id) != 1) {
    	"self@id must be length 1"
    }
  }
)
