#' Càlcul de la despesa meritada — Salut Mental: hosp parcial adults
#'
#' @param QC Quantitat/import contractat.
#' @param EM Paràmetre `EM`.
#' @param QF Quantitat/import facturat (realitzat).
#' @param tarifa Tarifa unitària.
#' @param pct_pla Percentatge d'assoliment del pla de salut.
#'
#' @return Despesa meritada calculada.
#' @export
SM_hosp_parcial_adults<-function(
              QC,
              EM,
              QF,
              tarifa,
              pct_pla
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
          pes.fix<-0.9
          pes.variable<-0.995
          pes.tarifa.variable<-0.095
          pes.pla_salut<-0.005
          marge.exces<-1.05
          
          #La funció a aplicar per calcular la DM  
          funcio.calcul.meritada<-function(
              QC,
              EM,
              QF,
              tarifa,
              pct_pla){
                  estades<-QC*EM
                  pagament_fixe<-(estades*tarifa*pes.fix)/12
                  pagament_variable<-round(tarifa*pes.tarifa.variable,2)# round() si no, no quadra
                  
                  import.fix<-if(QF<(QC*pes.fix)){0}else{pagament_fixe*12}
                  
                  import.variable<-if(QF>(QC*marge.exces)){
                    (QC*marge.exces)*EM*pagament_variable
                  }else if(import.fix==0){
                    QF*EM*tarifa*pes.variable
                  }else{
                    QF*EM*pagament_variable
                  }
                  import.pla.salut<- (estades*tarifa)*pes.pla_salut*pct_pla #Alguns casos a excel SI(U743=0). Ho obviem
                  
                  meritada<-import.fix+round_excel(import.variable,2)+import.pla.salut #nomes fa round a variable
                  
                  return(meritada)
            }
          #Apliquem la funció a cada row del df:
          df.meritada$DM=mapply(
            funcio.calcul.meritada,
            QC,
            EM,
            QF,
            tarifa,
            pct_pla)
          
          return(df.meritada$DM)
        }
