isSame <- function(x){
  ## grepl("^ *SAME *\\([0-9]+\\) *",x)
  ## grepl("^ *SAME *\\([0-9]+\\) *",x)
grepl("^\\s*SAME\\s*(\\(\\s*\\d+\\s*\\))?\\s*$", x)
}

