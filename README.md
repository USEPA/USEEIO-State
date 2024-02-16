# USEEIO State Models
Simple interactive R markdown files to use USEEIO State models.

The models are described in the EPA report [USEEIO State Models v1.0: Environmentally-Extended Input-Output Models for U.S. States](https://cfpub.epa.gov/si/si_public_record_Report.cfm?dirEntryId=360453).

## Usage
This repository uses R Markdown (Rmd) files. These files are designed to run in RStudio. 
Models and outputs are saved to subfolders of your local directory.

Each Rmd performs tasks suggested by the name.

To run a file, open the Rmd in RStudio and use the command "Knit with Parameters".

This will produce a pop-up window asking you to select values. 

One selected and entered the code will execute and produce output in your local folders.

A markdown file will be generated with the same name as the Rmd file but an md extension that only contain confirmation of execution.

### Steps
You must run DownloadandSaveModelLocally.Rmd first to save the model you wish to use locally before other .Rmd files can be used.

## Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information.  Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.
