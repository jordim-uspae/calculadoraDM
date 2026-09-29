#' Càlcul de la despesa meritada — Salut Mental: hosp parcial infantil
#'
#' @param QC Quantitat/import contractat.
#' @param pct_pla Percentatge d'assoliment del pla de salut.
#'
#' @return Despesa meritada calculada.
#' @export
SM_hosp_parcial_infantil<-function(
              QC,      # altes cont
              EM,      # estada mitjana (i fem altes cont x EM = estades contractades)
              QF,      # altes facturades
              tarifa,  
              pct_pla   #pct assoliment pla salut
          ){
  
          arguments<-as.list(environment())
          pct_args <- arguments[startsWith(names(arguments), "pct_")]
          
          # validació d'inputs
          stopifnot(
            all(sapply(arguments, is.numeric)),
            all(unlist(arguments) >= 0, na.rm = TRUE),
            all(sapply(pct_args,function(x) all(x >= 0 & x <= 1, na.rm = TRUE)))
          )
          
          
          #creem df a partir dels arguments (Permetrà treballar amb vectors)  
          df.meritada<-tryCatch(
            data.frame(arguments),
            error=function(e){
              stop('***Revisa que tots els vectors son de la mateixa mida perque no es pot crear un dataframe***') #control 
            }
          )
          
          #Fixem pesos de cada import
          assoliment_minim<-0.8
          pes.fix<-0.95
          pes.pla_salut<-0.05
          
          
          #La funció a aplicar per calcular la DM  
          funcio.calcul.meritada<-function( QC,      # altes cont
                                            EM,      # estada mitjana (i fem altes cont x EM = estades contractades)
                                            QF,      # altes facturades
                                            tarifa,  
                                            pct_pla){
                import_fix <- if (QF < QC*assoliment_minim) {
                  (QF*EM*tarifa)
                } else{
                  round_excel((QC*EM*tarifa)*pes.fix,2)
                }
                
                #PS:Pla de salut
                import_PS <-if (QF < QC*assoliment_minim) {
                  0
                } else{
                  round_excel((QC*EM*tarifa*pct_pla*pes.pla_salut),2)
                }
                
                meritada<-import_fix+import_PS
                return(meritada)
              }
          #Apliquem la funció a cada row del df:
          df.meritada$DM=mapply(
            funcio.calcul.meritada,
            QC,      # altes cont
            EM,      # estada mitjana (i fem altes cont x EM = estades contractades)
            QF,      # altes facturades
            tarifa,  
            pct_pla)
          
          return(df.meritada$DM)
}
