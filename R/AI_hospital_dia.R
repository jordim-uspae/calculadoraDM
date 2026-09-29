#' Càlcul de la despesa meritada — Atenció Intermèdia: hospital dia
#'
#' @param QC Quantitat/import contractat.
#' @param QF_menjador Quantitat/import facturat (realitzat).
#' @param QF_no_menjador Quantitat/import facturat (realitzat).
#' @param tarifa_menjador Tarifa unitària.
#' @param tarifa_no_menjador Tarifa unitària.
#'
#' @return Despesa meritada calculada.
#' @export
AI_hospital_dia<-function(QC,
                         QF_menjador,
                         QF_no_menjador,
                         tarifa_menjador,
                         tarifa_no_menjador
                        ){
  
                          arguments<-as.list(environment())
                          
                          # llindars de variables fixes
                          llindar_minim_CF <- 0.80  # per sota de quin punt de la relació QC i QF es paga només activitat realitzada?
                          pes_fix <- 0.7
                          pes_variable <- 0.3
                          
                          # validació d'inputs
                          stopifnot(all(sapply(arguments, is.numeric)),
                                    all(unlist(arguments) >= 0, na.rm = TRUE))
                          
                          #creem df a partir dels arguments (Permetrà treballar amb vectors)  
                          df_meritada<-tryCatch(data.frame(arguments),
                                                error=function(e){
                                                  stop('***Revisa que tots els vectors son de la mateixa mida perque no es pot crear un dataframe***') #control 
                                                }
                          )
                          # funcio calcul HD 
                          funcio_calcul_meritada <- function(QC,
                                                             QF_menjador,
                                                             QF_no_menjador,
                                                             tarifa_menjador,
                                                             tarifa_no_menjador){
                            
                            if(QF_menjador+QF_no_menjador>=QC){ #Si facturades>=Contractades
                              import_fix<- ((QF_menjador*tarifa_menjador)+ ((QC-QF_menjador)*tarifa_no_menjador))*pes_fix
                              import_variable<-((QF_menjador*tarifa_menjador)+ ((QC-QF_menjador)*tarifa_no_menjador))*pes_variable
                              
                              
                            }else if((QF_menjador+QF_no_menjador)<QC*llindar_minim_CF){ #si facturades< 80% de Contractades
                              import_fix<-0
                              import_variable<-((QF_menjador*tarifa_menjador)+ (QF_no_menjador*tarifa_no_menjador))
                              
                            }else{ #Entre el 80 i el 100% de les contractades
                              import_fix<-((QF_menjador*tarifa_menjador)+ ((QC-QF_menjador)*tarifa_no_menjador))*pes_fix
                              import_variable<-((QF_menjador*tarifa_menjador)+ (QF_no_menjador*tarifa_no_menjador))*pes_variable
                            }
                            
                            meritada<- round_excel(import_fix,2) + round_excel(import_variable,2)
                            
                            return(meritada)
                          }
                          
                          #Apliquem la funció a cada row del df:
                          df_meritada$DM = mapply(funcio_calcul_meritada,
                                                  QC,
                                                  QF_menjador,
                                                  QF_no_menjador,
                                                  tarifa_menjador,
                                                  tarifa_no_menjador)
                          
                          return(df_meritada$DM)
                          
}
