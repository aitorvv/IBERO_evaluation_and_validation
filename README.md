
# IBERO evaluation and validation

*Original data, code and results of: Evaluation and validation of forest growth models for Mediterranean and Scots pine in Spain*

---

That repository contains the information related with the paper titled: 

**Evaluation and validation of forest growth models for Mediterranean and Scots pine in Spain**

Repository contents:

- :floppy_disk: **data**:
    
    - :sunny: climate data:
        
        - data extracted from [AEMET](https://www.aemet.es/es/portada) for different meteorological stations

    - :deciduous_tree: IFN (Spanish Forest National Inventory) data:
        
        - 0_raw: just a test dataset is available in this folder, the original data can be downloaded from their original [website](https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional/default.aspx). Consider to download a piece of the original dataset if you are interested into test the code.

        - 1_processed: data at different levels of preparation (check R code)

    - :seedling: SIMANFOR data:
    
        - inputs: plots and trees data from NFI2 and NFI3 to use in models IBEROPT and IBEROPS
    
        - outputs: [SIMANFOR](https://www.simanfor.es/) outputs for each simulation and plot. Names shortcuts:
    
    
            - *IBEROPT/PS* is referred to the IBERO model used
    
            - *IFN2/3* is referred to the Spanish National Forest Inventory edition
    
            - *calibrated* indicates that the following variables were already modified including the improvements mentioned in the paper
            
            - *ingrowth* indicates that are results for ingrowth experiments
            
            - *mortality* indicates that are results of mortality experiments

- :chart_with_upwards_trend: **graphs**: 
    
    - graphs included on the paper

- :computer: **scripts**:

    - R:

        - code in R for:

            - 0_procesado_datos_IFN: adapt initial IFN data

            - 1_generador_inventarios: generate SIMANFOR inventories

            - 2_recalcular_IFN3: recalculate IFN3 variables to 10 years gap between measurements

            - 3_extraer_info_simulaciones: extract simulation information to develop statistics analysis
    
    - statistics (divided by models):
    
        - inputs: data used to perform analysis (SIMANFOR outputs)
    
        - outputs: outputs obtained for each analysis using SAS
    
        - scripts: code used to perform the analysis of each study case
    
        - Names shortcuts:
    
            - *IBEROPT/PS* is referred to the IBERO model used
    
            - *IFN2/3* is referred to the Spanish National Forest Inventory edition
    
            - *calibrated* indicates that the following variables were already modified including the improvements mentioned in the paper
            
            - *ingrowth* indicates that are results for ingrowth experiments
            
            - *mortality* indicates that are results of mortality experiments

---


[IBERO evaluation and validation](https://github.com/aitorvv/IBERO_evaluation_and_validation) 

:link: More about me:
[ORCID](https://orcid.org/0000-0003-0227-506X) \\
[Researchgate](https://www.researchgate.net/profile/Aitor_Vazquez_Veloso) \\
[LinkedIn](https://www.linkedin.com/in/aitorvazquezveloso/) \\
[Twitter](https://twitter.com/aitorvv) \\
[iuFOR](http://sostenible.palencia.uva.es/users/aitorvv)