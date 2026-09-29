#' Càlcul de la despesa meritada — Hospitalització d'Aguts: reingressos
#'
#' @param QF Quantitat/import facturat (realitzat).
#' @param reingressos Nombre de reingressos.
#' @param tarifa_alta Tarifa unitària.
#' @param tarifa_reingres Tarifa unitària.
#' @param pct_reingressos_limit Paràmetre `pct_reingressos_limit`.
#' @param pct_reingressos_pactat Paràmetre `pct_reingressos_pactat`.
#'
#' @return Despesa meritada calculada.
#' @export
HA_reingressos<- function(QF, 
                          reingressos,
                          tarifa_alta,
                          tarifa_reingres,
                          pct_reingressos_limit,
                          pct_reingressos_pactat 
                          ) {
          arguments <- as.list(environment())
          
          pct_tarifa_excés_reingrés <- 0.35
          
          stopifnot(
            is.numeric(c(QF, reingressos, tarifa_alta, tarifa_reingres, pct_reingressos_pactat, pct_reingressos_limit)),
            all(c(QF, reingressos, tarifa_alta, tarifa_reingres) >= 0, na.rm = TRUE),
            all(pct_reingressos_pactat >= 0 & pct_reingressos_pactat <= 1, na.rm = TRUE),
            all(pct_reingressos_limit >= 0 & pct_reingressos_limit <= 1, na.rm = TRUE))
          
          df_meritada <- tryCatch(
            data.frame(arguments),
            error = function(e) stop('***Revisa que tots els vectors son de la mateixa mida perquè no es pot crear un dataframe***')
          )
          
          funcio_calcul_meritada <- function(QF, 
                                             reingressos,
                                             tarifa_alta,
                                             tarifa_reingres,
                                             pct_reingressos_limit,
                                             pct_reingressos_pactat ) {
            QF <- num0(QF); reingressos <- num0(reingressos)
            tarifa_alta <- num0(tarifa_alta); tarifa_reingres <- num0(tarifa_reingres)
            
            if (QF == 0) {
              meritada <- 0
            } else {
              ratio_reingressos <- reingressos / QF
              
              if (ratio_reingressos > pct_reingressos_limit) {
                meritada <- round_excel(
                  pct_reingressos_pactat * QF * tarifa_alta +
                    (pct_reingressos_limit - pct_reingressos_pactat) * QF * tarifa_reingres +
                    (reingressos - pct_reingressos_limit * QF) *
                    round_excel(tarifa_reingres * pct_tarifa_excés_reingrés, 2),
                  2
                )
              } else if (ratio_reingressos > pct_reingressos_pactat) {
                meritada <- round_excel(
                  pct_reingressos_pactat * QF * tarifa_alta +
                    (reingressos - pct_reingressos_pactat * QF) * tarifa_reingres,
                  2
                )
              } else {
                meritada <- reingressos * tarifa_alta
              }
            }
            
            meritada
          }
          
          df_meritada$meritada <- mapply(
            funcio_calcul_meritada,
            QF, 
            reingressos,
            tarifa_alta,
            tarifa_reingres,
            pct_reingressos_limit,
            pct_reingressos_pactat 
          )
          
          return(df_meritada$meritada)
}
