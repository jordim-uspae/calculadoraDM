#' Càlcul de la despesa meritada — Atenció Intermèdia: llarga estada
#'
#' @param QC Quantitat/import contractat.
#' @param QF Quantitat/import facturat (realitzat).
#' @param QF_sida Quantitat/import facturat (realitzat).
#' @param tarifa Tarifa unitària.
#' @param tarifa_sida Tarifa unitària.
#' @param cpr Contraprestació per resultats (proporció entre 0 i 1).
#'
#' @return Despesa meritada calculada.
#' @export
AI_llarga_estada<- function(QC,
                            QF,
                            QF_sida,
                            tarifa,
                            tarifa_sida,
                            cpr) {
                            arguments<-as.list(environment())
                            
                            # llindars de variables fixes
                            llindar_minim_CF <- 0.80  # per sota de quin punt de la relació QC i QF es paga només activitat realitzada?
                            pes_fix <- 0.70           # percentatge tarifa part fixa
                            pes_variable <- 0.3      # percentatge tarifa part variable
                            pes_cpr <- 0.03           # pes de contraprestació per resultats
                            
                            # validació d'inputs
                            stopifnot(all(sapply(arguments, is.numeric)),
                                      all(unlist(arguments) >= 0, na.rm = TRUE),
                                      all(cpr >= 0 & cpr <= 1, na.rm = TRUE))
                            
                            #creem df a partir dels arguments (Permetrà treballar amb vectors)  
                            df_meritada <- tryCatch(data.frame(arguments),
                                                    error=function(e){
                                                      stop('***Revisa que tots els vectors son de la mateixa mida perque no es pot crear un dataframe***') #control 
                                                    }
                            )
                            
                            # funció LLE
                            funcio_calcul_meritada <- function(QC,
                                                               QF,
                                                               QF_sida,
                                                               tarifa,
                                                               tarifa_sida,
                                                               cpr) {
                              
                              factu_total <- QF + QF_sida
                              
                              if (factu_total < QC * llindar_minim_CF) {
                                # Cas 1: activitat inferior al 80%
                                import_variable <- (QF * tarifa) + (QF_sida * tarifa_sida)
                                import_cpr <- ((QF * tarifa) + (QF_sida * tarifa_sida)) * pes_cpr * cpr
                                
                                meritada <- round_excel(import_variable, 2) + round_excel(import_cpr, 2)
                                
                              } else if (factu_total < QC) {
                                # Cas 2: activitat entre 80% i 100%
                                import_fix <- ((QC - QF_sida) * tarifa * pes_fix) + (QF_sida * tarifa_sida * pes_fix)
                                import_variable <- (QF * tarifa * pes_variable) +(QF_sida * tarifa_sida * pes_variable)
                                import_cpr <- ((QF * tarifa) + (QF_sida * tarifa_sida)) * pes_cpr * cpr
                                
                                meritada <- round_excel(import_fix, 2) + round_excel(import_variable, 2) + round_excel(import_cpr, 2)
                                
                              } else {
                                # Cas 3: activitat superior al 100%
                                import_fix <- ((QC - QF_sida) * tarifa * pes_fix) + (QF_sida * tarifa_sida * pes_fix)
                                import_variable <- (QF_sida * tarifa_sida * pes_variable) + ((QC - QF_sida) * tarifa * pes_variable)
                                import_cpr <- (((QC - QF_sida) * tarifa) + (QF_sida * tarifa_sida)) * pes_cpr * cpr
                                
                                meritada <- round_excel(import_fix, 2) + round_excel(import_variable, 2) + round_excel(import_cpr, 2)
                                
                              }
                              return(meritada)
                            }
                            
                            # apliquem la funció a cada fila del df
                            df_meritada$DM = mapply(funcio_calcul_meritada,
                                                    QC,
                                                    QF,
                                                    QF_sida,
                                                    tarifa,
                                                    tarifa_sida,
                                                    cpr) 
                            
                            return(df_meritada$DM)
                          }
