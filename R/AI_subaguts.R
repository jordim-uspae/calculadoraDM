#' Càlcul de la despesa meritada — Atenció Intermèdia: subaguts
#'
#' @param QC Quantitat/import contractat.
#' @param QF Quantitat/import facturat (realitzat).
#' @param tarifa Tarifa unitària.
#' @param cpr Contraprestació per resultats (proporció entre 0 i 1).
#'
#' @return Despesa meritada calculada.
#' @export
AI_subaguts<-function(QC,
                      QF,
                      tarifa,
                      cpr
                      ){
  
                    arguments<-as.list(environment())
                    
                    # llindars de variables fixes
                    pes_fix <- 0.95           # percentatge tarifa part fixa
                    pes_cpr <- 0.05           # pes de contraprestació per resultats
                    
                    # validació d'inputs
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
                    
                      # funció subaguts 
                      funcio_calcul_meritada <- function(QC,
                                                         QF,
                                                         tarifa,
                                                         cpr) {
                        
                        #Si facturades>=Contractades
                        if(QF>=QC){
                          import_fix<-QC*tarifa*pes_fix
                          import_cpr<-QC*tarifa*pes_cpr*cpr
                          
                        }else{ #si facturades<Contractades
                          import_fix<-QF*tarifa*pes_fix
                          import_cpr<-QF*tarifa*pes_cpr*cpr
                        }
                        meritada<-round_excel(import_fix, 2)+round_excel(import_cpr, 2)
                        
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
