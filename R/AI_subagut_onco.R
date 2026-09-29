#' Càlcul de la despesa meritada — Atenció Intermèdia: subagut onco
#'
#' @param QC Quantitat/import contractat.
#' @param QF Quantitat/import facturat (realitzat).
#' @param tarifa Tarifa unitària.
#'
#' @return Despesa meritada calculada.
#' @export
AI_subagut_onco<-function(QC,
                          QF,
                          tarifa
                          ){
  
                          arguments<-as.list(environment())
                          cpr_vars <- unlist(arguments[startsWith(names(arguments), "cpr_")])
                          
                          # llindars de variables fixes
                          pes_fix <- 0.7           # percentatge tarifa part fixa
                          pes_variable <- 0.3      # percentatge tarifa part variable
                          
                          # validació d'inputs
                          stopifnot(all(sapply(arguments, is.numeric)),
                                    all(unlist(arguments) >= 0, na.rm = TRUE))
                          
                          #creem df a partir dels arguments (Permetrà treballar amb vectors)  
                          df_meritada <- tryCatch(data.frame(arguments),
                                                  error=function(e){
                                                    stop('***Revisa que tots els vectors son de la mateixa mida perque no es pot crear un dataframe***') #control 
                                                  }
                          )
                          
                          # funció Subaguts onco
                          funcio_calcul_meritada <- function(QC,
                                                             QF,
                                                             tarifa) {
                            
                            import.fix <- QC*tarifa*pes_fix
                            import.variable <- if(QF>QC){QC*tarifa*pes_variable} else{QF*tarifa*pes_variable}
                            
                            meritada <- round_excel(import.fix, 2) + round_excel(import.variable, 2)
                            return(meritada)
                          }
                          
                          # apliquem la funció a cada fila del df
                          df_meritada$DM = mapply(funcio_calcul_meritada,
                                                  QC,
                                                  QF,
                                                  tarifa) 
                          
                          return(df_meritada$DM)
                        }
