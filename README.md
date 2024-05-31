# USEEIO State Models
This repository provides code to use USEEIO State Models and the model specification/configuration files for generating USEEIO State Models. Like for other USEEIO family models, [useeior](https://github.com/USEPA/useeior) is the primary tool used to assemble the USEEIO State models.

The first version of the USEEIO State models (v1.0) are described in the EPA report [USEEIO State Models v1.0: Environmentally-Extended Input-Output Models for U.S. States](https://cfpub.epa.gov/si/si_public_record_Report.cfm?dirEntryId=360453).

USEPA generated and quality-assured versions of models are stored in the EPA Data Commons under the [USEEIO-State directory](https://dmap-data-commons-ord.s3.amazonaws.com/index.html#USEEIO-State/). Some of the examples in this repository retrieve these stored, pre-built models. Other examples build the models using the model specification files and then perform uses of those models.

We recommend downloading a copy or cloning the entire repository to use the code.

## Contents
[examples](/examples) contains simple interactive R markdown files to use USEEIO State models. 

[model_specs](/model_specs) contains original model specification/configuration files for USEEIO State models including EPA v1.0 models.

[R](/R) contains helper scripts called in examples. These R scripts are not intended for independent use.

_model_ and _output_ subfolders will be created locally upon execution of some of the example code to store a local copy of a model or provide an output.

## Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information.  Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.
