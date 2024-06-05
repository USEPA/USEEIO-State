# USEEIO State Model Specification Files

EPA's USEEIO State model v1.0 were created with the StateEEIOv1.0 specification files under all_states. 

All model specification files follow the useeior [model specification](https://github.com/USEPA/useeior/blob/master/format_specs/ModelSpecification.md) format.

## General characteristics 
- Naming pattern is `location-version-extensions(optional)-year`
- Model specification files exist that create models for all states in one build (in the [all_states](/all_states) folder and have the name 'StateEEIO') and others that create individual state models, in state-specific folders.
- By default all EEIO v1 models have **GHG, WAT, CHAIR, GRDREL, WATREL, LAND, CRHW, EMP, VADD** extensions (see any model spec for the full names). If the model only has one or more extensions, this is included in the name, e.g. ('-GHG-') in the name indicates GHG extensions only.
- Individual state models the names use the state 2 letter acronym plus 'EEIO' like 'COEEIO' for Colorado.
- Models are specific to year, generally named after the input-output data year, where the last two digits of the year appear at the end of the model name. Data years for extensions can vary and do not always match a model IO year. 


## USEEIO State model version guide

|  Version     | Import Emission Factors | Default Extensions                                     | stateior version |
|:-------------|:------------------------|:------------------------------------------------------ |:-----------------|
| v1.0         | No                      | GHG, WAT, CHAIR, GRDREL, WATREL, LAND, CRHW, EMP, VADD | 2012 (v0.2.1)    |
| v1.1         | Yes                     | GHG                                                    | 2012 (v0.2.1)    | 

By default the v1.1 models use import emissions factors build using EXIOBASE to represent embodied GHGs in imports in place of using the domestic technology assumption. 


