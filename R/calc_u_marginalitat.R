#' Càlcul de la unitat de marginalitat
#'
#' Calcula el percentatge d'excés de facturació respecte a la quantitat
#' contractada.
#'
#' @param contracte Quantitat contractada.
#' @param facturacio Quantitat facturada/realitzada.
#'
#' @return Un numèric: `NA` si el contracte no és vàlid, `0` si no hi ha
#'   excés, o el percentatge d'excés en cas contrari.
#' @export
#'
#' @examples
#' calc_u_marginalitat(contracte = 100, facturacio = 120)
calc_u_marginalitat <- function(contracte, facturacio) {
  if (is.na(contracte) || is.na(facturacio) || contracte <= 0) return(NA_real_)
  if (facturacio <= contracte) return(0)
  (facturacio - contracte) / contracte
}
