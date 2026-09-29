#' Càlcul de la despesa meritada — Salut Mental: aguts
#'
#' @param QC Quantitat/import contractat.
#' @param EM Paràmetre `EM`.
#' @param QF Quantitat/import facturat (realitzat).
#' @param tarifa_altes Tarifa unitària.
#' @param tarifa_fix Tarifa unitària.
#' @param tarifa_variable Tarifa unitària.
#' @param pct_pla Percentatge d'assoliment del pla de salut.
#'
#' @return Despesa meritada calculada.
#' @export
SM_aguts<-function(
    QC,
    EM,#estada mitjana contractada
    QF,
    tarifa_altes,
    tarifa_fix,
    tarifa_variable,
    pct_pla){
          arguments<-as.list(environment())
          
          # validació d'inputs
          stopifnot(
            all(sapply(arguments, is.numeric)),
            all(unlist(arguments) >= 0, na.rm = TRUE),
            all(pct_pla >= 0 & pct_pla <= 1, na.rm = TRUE)
          )
          
          #creem df a partir dels arguments (Permetrà treballar amb vectors)  
          df.meritada<-tryCatch(
            data.frame(arguments),
            error=function(e){
              stop('***Revisa que tots els vectors son de la mateixa mida perque no es pot crear un dataframe***') #control 
            }
          )
          
          #Fixem pesos de cada import 
          assoliment_minim<-0.9
          pes.pla_salut<-0.005
          pes.variable<-0.995
          marge.exces.estades<-1.05
          
          #La funció a aplicar per calcular la DM  
          funcio.calcul.meritada<-function(QC,
                                           EM,#estada mitjana contractada
                                           QF,
                                           tarifa_altes,
                                           tarifa_fix,
                                           tarifa_variable,
                                           pct_pla){
            
            estades<-QC*EM #estades contractades
            QC_PS<-estades*tarifa_altes*pes.pla_salut #Import pla de salut 
            
            #import fix:
            import_fix<-if(QF<(QC*assoliment_minim)){
              0
            }else{
              tarifa_fix*12
            }
            
            #import variable
            import_variable<-if(QF>(QC*marge.exces.estades)){
              QC*marge.exces.estades*EM*tarifa_variable
            }else if(import_fix==0){
              QF*EM*tarifa_altes*pes.variable
            }else{
              QF*EM*tarifa_variable
            }
            
            #import pla salut
            import.pla.salut<- if(import_fix==0){
              QF*EM*tarifa_altes*pes.pla_salut*pct_pla
            }else{
              QC_PS*pct_pla  
            }
            
            meritada<-round_excel(import_fix)+round_excel(import_variable)+import.pla.salut #PS no fan round
            return(meritada)
          }
          
          #Apliquem la funció a cada row del df:
          df.meritada$DM=mapply(
            funcio.calcul.meritada,
            QC,
            EM,#estada mitjana contractada
            QF,
            tarifa_altes,
            tarifa_fix,
            tarifa_variable,
            pct_pla)
          
          return(df.meritada$DM)
        }
