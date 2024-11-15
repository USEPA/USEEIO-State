# Example Uses of USEEIO State Models

These files are designed to run in RStudio. 
Models and outputs are saved to subfolders of your local directory upon execution of this code.

Each Rmd performs tasks suggested by the name.

To run a file, open the Rmd in RStudio and use the command "Knit with Parameters".

This will produce a pop-up window asking you to select values. 

One selected and entered the code will execute and produce output in your local folders.

A markdown file will be generated with the same name as the Rmd file but an md extension that only contain confirmation of execution.

Uses are exemplified in separate combination .Rmd (for code) and, when applicable, an .md (for printed output readable on github and also translatable into .docx or other format supported by pandoc). 

1. **DownloadandSaveModelLocally**. Interactively select and download an EPA-built USEEIO State model. Save the model locally as .rds for quick retrieval for further uses. Optionally write the model out to Excel. [Code](DownloadandSaveModelLocally.Rmd). Output provides record of action.  

2. **BuildandSaveModelsLocally**. Interactively select and build USEEIO State models from specs and save the model locally as .rds for quick retrieval for further uses. Optionally write the models out to Excel. [Code](BuildandSaveModelsLocally.Rmd). Output provides record of action.

3. **Demo-Multipliers-CO**. Build an original Colorado model with jobs, GHG, and value added satellite tables for use in demonstrating the calculation of economic and environmental Type I multipliers using a USEEIO State model. [Code](Demo-Multipliers-CO.Rmd).  [Output](Demo-Multipliers-CO.md)  

4. **StateCBE**. Calculate and display consumption based GHG emissions for a selected state, rendered as a Microsoft Word file. [Code](StateCBE.Rmd).
Requires additional packages:

```{r}
install.packages(c("officedown"))
install.packages(c("bookdown"))
```
