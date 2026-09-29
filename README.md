# calculadoraMeritada

Funcions per calcular la despesa meritada dels contractes sanitaris del
CatSalut (USPAE), replicant les fórmules de facturació de l'annex de
tarifes, organitzades en tres àmbits.

## Instal·lació

```r
# install.packages("remotes")
remotes::install_github("jordim-uspae/calculadoraMeritada")
```

## Funcions incloses

### Atenció Intermèdia (AI)
`AI_llarga_estada`, `AI_mitja_estada`, `AI_subaguts`, `AI_hospital_dia`,
`AI_PADES`, `AI_UFISS`, `AI_EAIA`, `AI_PIUC`, `AI_subagut_onco`, `AI_SIDA`

### Salut Mental (SM)
`SM_aguts`, `SM_subaguts`, `SM_mitja_llarga_estada`, `SM_hosp_parcial_adults`,
`SM_hosp_parcial_infantil`, `SM_rehabilitacio_AIJ`, `SM_CSMA_CSMIJ`,
`SM_drogodependencies`, `SM_crisi_adolesc_aguts`, `SM_subaguts_adolescents`,
`SM_patologia_dual`, `SM_programes`

### Hospitalització d'Aguts (HA)
`HA_hospitalitzacions`, `HA_reingressos`, `HA_urgencies_triatge`,
`HA_urgencies_atesa`, `HA_hospital_dia_cir_menor`, `HA_CMA_complexa`,
`HA_TTPE`, `HA_reproduccio_assistida`, `HA_rehabilitacio`,
`HA_terapies_resp_domicili`, `HA_TTPE_alta_complexitat`,
`HA_onco_mastectomia`, `HA_AIR`, `HA_protesis`, `HA_bombes_insulina`,
`HA_lipoatrofia_facial`, `HA_urgencies_hivern`

### Auxiliar
`calc_u_marginalitat`

## Exemple

```r
library(calculadoraMeritada)

HA_hospitalitzacions(QC = 100, QF = 110, reingressos = 2, tarifa = 500)
```

## Desenvolupament

```r
devtools::load_all()
devtools::document()
devtools::test()
devtools::check()
```
