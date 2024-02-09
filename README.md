# USEEIO State Models
Code to build [USEEIO family](https://www.epa.gov/land-research/us-environmentally-extended-input-output-useeio-models) Environmentally-Extended Input-Output models for US states and calculate example results.

The models are described in the EPA report [USEEIO State Models v1.0: Environmentally-Extended Input-Output Models for U.S. States](https://cfpub.epa.gov/si/si_public_search_results.cfm?simpleSearch=0&showCriteria=2&sortBy=pubDate&searchAll=useeio+state+models&TIMSType=Published+Report&dateBeginPublishedPresented=).

## Usage

The code in this repository can be used to generate the StateEEIO models based on options selected by the user.

Run the file [BuildStateEEIO.Rmd](BuildStateEEIO.Rmd)

A single year can be selected from the range of 2012 - 2020.

A single state can be selected using two digit codes starting with "US-", e.g., "US-GA".

An excel version of the model is produced and stored in a folder labeled "output". A markdown file, BuildStateEEIO.md, is automatically generated during execution, but it contains no original content.

## Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information.  Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.
