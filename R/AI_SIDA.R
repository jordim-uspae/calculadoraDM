#' Càlcul de la despesa meritada — Atenció Intermèdia: SIDA
#'
#' @param QC Quantitat/import contractat.
#' @param QF Quantitat/import facturat (realitzat).
#' @param tarifa Tarifa unitària.
#' @param cpr Contraprestació per resultats (proporció entre 0 i 1).
#'
#' @return Despesa meritada calculada.
#' @export
AI_SIDA<- function(QC,
                   QF,
                   tarifa,
                   cpr){

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
                    
                    # funció sida
                    funcio_calcul_meritada <- function(QC,
                                                       QF,
                                                       tarifa,
                                                       cpr) {
                      
                      import.fix <- if(QF<(QC*llindar_minim_CF)){0}else{QC*tarifa*pes_fix}
                      
                      import.variable<-if(QF<(QC*llindar_minim_CF)){
                        QF*tarifa
                      }else if(QF>QC){
                        QC*tarifa*pes_variable
                      }else{
                        QF*tarifa*pes_variable
                      }
                      
                      import.cpr<-if(QF>QC){ 
                        (QC*tarifa*pes_cpr)*cpr
                      }else{
                        QF*tarifa*pes_cpr*cpr
                      }
                      
                      meritada<-round_excel(import.fix, 2)+round_excel(import.variable, 2)+round_excel(import.cpr, 2)
                      return(meritada)
                    }
                    
                    # apliquem la funció a cada fila del df
                    df_meritada$DM = mapply(funcio_calcul_meritada,
                                            QC,
                                            QF,
                                            tarifa,
                                            cpr) 
                    
                    return(df_meritada$DM)
                  }
