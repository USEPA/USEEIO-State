# Demonstrate the Computation of Type I Multipliers and Related Impacts of construction spending from a Colorado EEIO model

This script builds a Colorado USEEIO State model for 2020 with
extensions for Jobs, Greenhouse Gas Emissions, and Value Added
components.

It requires *useeior* \>= v1.5.1 which will be installed from github
using the remotes library if *useeior* doesn’t exist already in your R
environment. The kableExtra package is used for formatting the tables
for output here.

Your R environment needs to be setup to use the Project directory to
knit this file (usually from RStudio).

``` r
# Load a local R file with an install_useeior() function
source("R/utils.R")
install_useeior() # This install
library(useeior)
library(kableExtra)
```

Then we build a custom state EEIO model for Colorado using *useeior* and
a [new custom model specification
file](model_specs/COEEIOv1.0-s-JGV-20.yml). All the raw files including
the relevant StateIO models and National totals of Employment and [GHGs
by sector](https://doi.org/10.23719/1529805), and Indicators (for GHGs
to convert to kg CO2e) tables are retrieved from the EPA Data Commons
and stored locally in your machine-defined `appdata` directory under
*stateio* and *flowsa*, and *lciaformatter*, respectively (each are EPA
Tools for Industrial Ecology modeling).

After building the model, it will store a version in RDS and Excel
formats in an output directory. If you’ve already built it once, it will
load the stored RDS version.

``` r
dir <- file.path("output")
local_model_RDS <- "COEEIOv1.0-s-JGV-20.rds"
dir_local_model_RDS <- file.path(dir,local_model_RDS)
if (!file.exists(dir_local_model_RDS)) {
  CO <- buildModel("COEEIOv1.0-s-JGV-20","model_specs/COEEIOv1.0-s-JGV-20.yml")  
  if (!dir.exists(dir)) dir.create(dir)
  saveRDS(CO,dir_local_model_RDS)
  writeModeltoXLSX(CO,outputfolder=dir)
} else {
  CO <- readRDS(dir_local_model_RDS)
}
```

    ## 2024-05-29 10:50:43 INFO::Begin model initialization...
    ## 2024-05-29 10:50:43 INFO::Initializing IO tables...
    ## 2024-05-29 10:50:44 INFO::Initializing Chain Price Index tables...
    ## 2024-05-29 10:50:44 INFO::Initializing model satellite tables...
    ## 2024-05-29 10:50:44 INFO::Loading Greenhouse Gases flows from DataCommons...
    ## 2024-05-29 10:50:48 INFO::Loading Employment flows from DataCommons...
    ## 2024-05-29 10:50:49 INFO::Generating Value Added flows...
    ## 2024-05-29 10:50:50 INFO::No duplicate flows exist across satellite tables.
    ## 2024-05-29 10:50:50 INFO::Initializing model indicators...
    ## 2024-05-29 10:50:50 INFO::Getting Greenhouse Gases indicator from DataCommons...
    ## 2024-05-29 10:50:50 INFO::Getting Value Added indicator from useeior...
    ## 2024-05-29 10:50:50 INFO::Getting Jobs Supported indicator from useeior...
    ## 2024-05-29 10:50:50 INFO::Loading demand vectors ...
    ## 2024-05-29 10:50:50 INFO::Loading US-CO CompleteProduction demand vector...
    ## 2024-05-29 10:50:50 INFO::Loading US-CO DomesticProduction demand vector...
    ## 2024-05-29 10:50:50 INFO::Loading US-CO CompleteConsumption demand vector...
    ## 2024-05-29 10:50:50 INFO::Loading US-CO DomesticConsumption demand vector...
    ## 2024-05-29 10:50:50 INFO::Loading RoUS CompleteProduction demand vector...
    ## 2024-05-29 10:50:50 INFO::Loading RoUS DomesticProduction demand vector...
    ## 2024-05-29 10:50:50 INFO::Loading RoUS CompleteConsumption demand vector...
    ## 2024-05-29 10:50:50 INFO::Loading RoUS DomesticConsumption demand vector...
    ## 2024-05-29 10:50:50 INFO::Generating A matrix of SoI2SoI Use table ...
    ## 2024-05-29 10:50:50 INFO::Generating A matrix of RoUS2SoI Use table ...
    ## 2024-05-29 10:50:50 INFO::Generating A matrix of SoI2RoUS Use table ...
    ## 2024-05-29 10:50:50 INFO::Generating A matrix of RoUS2RoUS Use table ...
    ## 2024-05-29 10:50:50 INFO::Generating A matrix of SoI2SoI Domestic Use table ...
    ## 2024-05-29 10:50:50 INFO::Generating A matrix of RoUS2SoI Domestic Use table ...
    ## 2024-05-29 10:50:50 INFO::Generating A matrix of SoI2RoUS Domestic Use table ...
    ## 2024-05-29 10:50:50 INFO::Generating A matrix of RoUS2RoUS Domestic Use table ...
    ## 2024-05-29 10:50:50 INFO::Building commodity-by-commodity A matrix (direct requirements)...
    ## 2024-05-29 10:50:50 INFO::Building commodity-by-commodity A_d matrix (domestic direct requirements)...
    ## 2024-05-29 10:50:50 INFO::Calculating L matrix (total requirements)...
    ## 2024-05-29 10:50:50 INFO::Calculating L_d matrix (domestic total requirements)...
    ## 2024-05-29 10:50:50 INFO::Building B matrix (direct emissions and resource use per dollar)...
    ## 2024-05-29 10:50:50 INFO::Building C matrix (characterization factors for model indicators)...
    ## 2024-05-29 10:50:50 INFO::Calculating D matrix (direct environmental impacts per dollar)...
    ## 2024-05-29 10:50:50 INFO::Calculating M matrix (total emissions and resource use per dollar)...
    ## 2024-05-29 10:50:50 INFO::Calculating M_d matrix (total emissions and resource use per dollar from domestic activity)...
    ## 2024-05-29 10:50:50 INFO::Calculating N matrix (total environmental impacts per dollar)...
    ## 2024-05-29 10:50:50 INFO::Calculating N_d matrix (total environmental impacts per dollar from domestic activity)...
    ## 2024-05-29 10:50:50 INFO::Calculating Rho matrix (price year ratio)...
    ## 2024-05-29 10:50:50 INFO::Calculating Phi matrix (producer over purchaser price ratio)...
    ## 2024-05-29 10:50:50 INFO::Calculating Tau matrix (basic over producer price ratio)...
    ## 2024-05-29 10:50:50 INFO::Model build complete.
    ## 2024-05-29 10:50:50 INFO::Model metadata written to C:\Users\BYoung\AppData\Local/useeior/Model_Builds/COEEIOv1.0-s-JGV-20/build/data/COEEIOv1.0-s-JGV-20.
    ## 2024-05-29 10:50:51 INFO::Model written to Excel workbook (.xlsx) in output.

Assume a scenario of \$2 million in spending in construction in
Colorado.

Get the Type I (indirect) output multiplier, defined here has the sum of
total requirements table column for that sector and region of interest.

``` r
# Create a custom demand vector for CO construction
sector <- c("23/US-CO")
demand <- c(2E6)
regions <- c("CO","RoUS","All US")

output_multiplier_CO <- sum(CO$L_d[grep("US-CO",row.names(CO$L_d)),sector])
output_multiplier_RoUS <- sum(CO$L_d[grep("RoUS",row.names(CO$L_d)),sector])
output_multiplier <- output_multiplier_CO + output_multiplier_RoUS

kable(data.frame(Output_Multiplier=c(output_multiplier_CO,output_multiplier_RoUS,output_multiplier), row.names=regions))
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Output_Multiplier
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
CO
</td>
<td style="text-align:right;">
1.3326813
</td>
</tr>
<tr>
<td style="text-align:left;">
RoUS
</td>
<td style="text-align:right;">
0.4540408
</td>
</tr>
<tr>
<td style="text-align:left;">
All US
</td>
<td style="text-align:right;">
1.7867221
</td>
</tr>
</tbody>
</table>

Look at total output stimulated in CO, the rest of the U.S., and all the
U.S.

``` r
total_output_stimulated_CO <- demand*output_multiplier_CO
total_output_stimulated_RoUS <- demand*output_multiplier_RoUS
total_output_stimulated <- demand*output_multiplier

kable(data.frame(`Direct_Indirect_Output_Stimulated_$`=c(total_output_stimulated_CO,total_output_stimulated_RoUS,total_output_stimulated), row.names=regions))
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Direct_Indirect_Output_Stimulated\_.
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
CO
</td>
<td style="text-align:right;">
2665362.6
</td>
</tr>
<tr>
<td style="text-align:left;">
RoUS
</td>
<td style="text-align:right;">
908081.7
</td>
</tr>
<tr>
<td style="text-align:left;">
All US
</td>
<td style="text-align:right;">
3573444.3
</td>
</tr>
</tbody>
</table>

Now calculate the direct and indirect impacts on greenhouse gas
emissions, jobs supported, and value added.

``` r
y <- setNames(demand, sector)
y <- formatDemandVector(y, CO$L)
# Use a direct perspective calculation which will show the impact in the sectors in which it occurs
result <- calculateEEIOModel(CO, perspective = "DIRECT", use_domestic_requirements=TRUE, demand = y)
```

    ## 2024-05-29 10:50:51 INFO::Calculating Direct Perspective LCI...
    ## 2024-05-29 10:50:51 INFO::Calculating Direct Perspective LCIA...
    ## 2024-05-29 10:50:51 INFO::Result calculation complete.

Show the totals for the results for GHGs (kg CO2e), Jobs, and Value
Added (\$) by Region

``` r
CO_result <- colSums(result$LCIA_d[grep("US-CO",row.names(result$LCIA_d)),])
RoUS_result <- colSums(result$LCIA_d[grep("RoUS",row.names(result$LCIA_d)),])
US_result <- CO_result+RoUS_result

kable(t(data.frame(CO=CO_result,RoUS=RoUS_result,`AllUS`=US_result)))
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Greenhouse Gases
</th>
<th style="text-align:right;">
Jobs Supported
</th>
<th style="text-align:right;">
Value Added
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
CO
</td>
<td style="text-align:right;">
247223.9
</td>
<td style="text-align:right;">
9.866408
</td>
<td style="text-align:right;">
1353392.9
</td>
</tr>
<tr>
<td style="text-align:left;">
RoUS
</td>
<td style="text-align:right;">
156217.8
</td>
<td style="text-align:right;">
2.957062
</td>
<td style="text-align:right;">
480119.8
</td>
</tr>
<tr>
<td style="text-align:left;">
AllUS
</td>
<td style="text-align:right;">
403441.6
</td>
<td style="text-align:right;">
12.823470
</td>
<td style="text-align:right;">
1833512.7
</td>
</tr>
</tbody>
</table>

The full results can be viewed by sector and location

``` r
kable(result$LCIA_d)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Greenhouse Gases
</th>
<th style="text-align:right;">
Jobs Supported
</th>
<th style="text-align:right;">
Value Added
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
111CA/US-CO
</td>
<td style="text-align:right;">
4.372717e+03
</td>
<td style="text-align:right;">
0.0046477
</td>
<td style="text-align:right;">
6.309772e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
113FF/US-CO
</td>
<td style="text-align:right;">
6.149429e+02
</td>
<td style="text-align:right;">
0.0121821
</td>
<td style="text-align:right;">
1.512345e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
211/US-CO
</td>
<td style="text-align:right;">
1.888866e+03
</td>
<td style="text-align:right;">
0.0015766
</td>
<td style="text-align:right;">
9.660404e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
212/US-CO
</td>
<td style="text-align:right;">
1.782466e+04
</td>
<td style="text-align:right;">
0.0381971
</td>
<td style="text-align:right;">
1.145973e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
213/US-CO
</td>
<td style="text-align:right;">
2.468659e+02
</td>
<td style="text-align:right;">
0.0019355
</td>
<td style="text-align:right;">
3.677521e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
22/US-CO
</td>
<td style="text-align:right;">
4.388679e+04
</td>
<td style="text-align:right;">
0.0231614
</td>
<td style="text-align:right;">
8.589544e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
23/US-CO
</td>
<td style="text-align:right;">
6.678314e+04
</td>
<td style="text-align:right;">
7.4739166
</td>
<td style="text-align:right;">
1.020958e+06
</td>
</tr>
<tr>
<td style="text-align:left;">
321/US-CO
</td>
<td style="text-align:right;">
2.976932e+03
</td>
<td style="text-align:right;">
0.0847331
</td>
<td style="text-align:right;">
1.088232e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
327/US-CO
</td>
<td style="text-align:right;">
3.554883e+04
</td>
<td style="text-align:right;">
0.1555065
</td>
<td style="text-align:right;">
2.927643e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
331/US-CO
</td>
<td style="text-align:right;">
8.711160e+03
</td>
<td style="text-align:right;">
0.0149515
</td>
<td style="text-align:right;">
2.262371e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
332/US-CO
</td>
<td style="text-align:right;">
2.408247e+03
</td>
<td style="text-align:right;">
0.1611999
</td>
<td style="text-align:right;">
1.900603e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
333/US-CO
</td>
<td style="text-align:right;">
7.173337e+02
</td>
<td style="text-align:right;">
0.0582765
</td>
<td style="text-align:right;">
7.242672e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
334/US-CO
</td>
<td style="text-align:right;">
1.009701e+02
</td>
<td style="text-align:right;">
0.0078160
</td>
<td style="text-align:right;">
1.928873e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
335/US-CO
</td>
<td style="text-align:right;">
1.161121e+03
</td>
<td style="text-align:right;">
0.0283527
</td>
<td style="text-align:right;">
5.483406e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
3361MV/US-CO
</td>
<td style="text-align:right;">
1.336798e+02
</td>
<td style="text-align:right;">
0.0022231
</td>
<td style="text-align:right;">
2.101388e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
3364OT/US-CO
</td>
<td style="text-align:right;">
4.430213e+00
</td>
<td style="text-align:right;">
0.0003269
</td>
<td style="text-align:right;">
6.950088e+01
</td>
</tr>
<tr>
<td style="text-align:left;">
337/US-CO
</td>
<td style="text-align:right;">
3.731103e+02
</td>
<td style="text-align:right;">
0.0459009
</td>
<td style="text-align:right;">
5.134083e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
339/US-CO
</td>
<td style="text-align:right;">
8.321836e+00
</td>
<td style="text-align:right;">
0.0017598
</td>
<td style="text-align:right;">
3.220868e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
311FT/US-CO
</td>
<td style="text-align:right;">
5.840084e+01
</td>
<td style="text-align:right;">
0.0016976
</td>
<td style="text-align:right;">
2.368277e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
313TT/US-CO
</td>
<td style="text-align:right;">
9.315244e+01
</td>
<td style="text-align:right;">
0.0020982
</td>
<td style="text-align:right;">
1.298549e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
315AL/US-CO
</td>
<td style="text-align:right;">
3.647327e-01
</td>
<td style="text-align:right;">
0.0000437
</td>
<td style="text-align:right;">
4.724435e+00
</td>
</tr>
<tr>
<td style="text-align:left;">
322/US-CO
</td>
<td style="text-align:right;">
2.523393e+03
</td>
<td style="text-align:right;">
0.0045183
</td>
<td style="text-align:right;">
6.269971e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
323/US-CO
</td>
<td style="text-align:right;">
3.747567e+01
</td>
<td style="text-align:right;">
0.0034265
</td>
<td style="text-align:right;">
3.485353e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
324/US-CO
</td>
<td style="text-align:right;">
1.977179e+04
</td>
<td style="text-align:right;">
0.0087999
</td>
<td style="text-align:right;">
7.383219e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
325/US-CO
</td>
<td style="text-align:right;">
9.909314e+03
</td>
<td style="text-align:right;">
0.0217245
</td>
<td style="text-align:right;">
5.037961e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
326/US-CO
</td>
<td style="text-align:right;">
1.171139e+03
</td>
<td style="text-align:right;">
0.0431529
</td>
<td style="text-align:right;">
6.443880e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
42/US-CO
</td>
<td style="text-align:right;">
2.132506e+02
</td>
<td style="text-align:right;">
0.0973550
</td>
<td style="text-align:right;">
2.025622e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
441/US-CO
</td>
<td style="text-align:right;">
4.485222e+01
</td>
<td style="text-align:right;">
0.0331775
</td>
<td style="text-align:right;">
2.815312e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
445/US-CO
</td>
<td style="text-align:right;">
6.559368e+00
</td>
<td style="text-align:right;">
0.0035476
</td>
<td style="text-align:right;">
2.528849e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
452/US-CO
</td>
<td style="text-align:right;">
1.042600e+01
</td>
<td style="text-align:right;">
0.0255294
</td>
<td style="text-align:right;">
1.597947e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
4A0/US-CO
</td>
<td style="text-align:right;">
3.440352e+02
</td>
<td style="text-align:right;">
0.5663365
</td>
<td style="text-align:right;">
5.183665e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
481/US-CO
</td>
<td style="text-align:right;">
6.063539e+02
</td>
<td style="text-align:right;">
0.0027635
</td>
<td style="text-align:right;">
3.127186e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
482/US-CO
</td>
<td style="text-align:right;">
1.546266e+03
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
1.835533e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
483/US-CO
</td>
<td style="text-align:right;">
9.249784e+01
</td>
<td style="text-align:right;">
0.0001750
</td>
<td style="text-align:right;">
1.748185e+01
</td>
</tr>
<tr>
<td style="text-align:left;">
484/US-CO
</td>
<td style="text-align:right;">
1.630790e+04
</td>
<td style="text-align:right;">
0.0888263
</td>
<td style="text-align:right;">
1.009370e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
485/US-CO
</td>
<td style="text-align:right;">
3.386896e+02
</td>
<td style="text-align:right;">
0.0094590
</td>
<td style="text-align:right;">
6.661151e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
486/US-CO
</td>
<td style="text-align:right;">
9.137222e+02
</td>
<td style="text-align:right;">
0.0003977
</td>
<td style="text-align:right;">
4.735877e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
487OS/US-CO
</td>
<td style="text-align:right;">
1.124001e+03
</td>
<td style="text-align:right;">
0.0590048
</td>
<td style="text-align:right;">
4.739444e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
493/US-CO
</td>
<td style="text-align:right;">
3.662863e+02
</td>
<td style="text-align:right;">
0.0388361
</td>
<td style="text-align:right;">
1.905210e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
511/US-CO
</td>
<td style="text-align:right;">
4.366303e-01
</td>
<td style="text-align:right;">
0.0011699
</td>
<td style="text-align:right;">
4.235670e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
512/US-CO
</td>
<td style="text-align:right;">
3.036321e-01
</td>
<td style="text-align:right;">
0.0003225
</td>
<td style="text-align:right;">
4.426739e+01
</td>
</tr>
<tr>
<td style="text-align:left;">
513/US-CO
</td>
<td style="text-align:right;">
3.679597e+01
</td>
<td style="text-align:right;">
0.0158763
</td>
<td style="text-align:right;">
4.748579e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
514/US-CO
</td>
<td style="text-align:right;">
1.460722e+01
</td>
<td style="text-align:right;">
0.0096818
</td>
<td style="text-align:right;">
2.816527e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
521CI/US-CO
</td>
<td style="text-align:right;">
3.138332e+01
</td>
<td style="text-align:right;">
0.0138574
</td>
<td style="text-align:right;">
2.858563e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
523/US-CO
</td>
<td style="text-align:right;">
3.434655e+01
</td>
<td style="text-align:right;">
0.0142382
</td>
<td style="text-align:right;">
2.916921e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
524/US-CO
</td>
<td style="text-align:right;">
1.395269e+00
</td>
<td style="text-align:right;">
0.0041663
</td>
<td style="text-align:right;">
8.895953e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
525/US-CO
</td>
<td style="text-align:right;">
1.161582e+00
</td>
<td style="text-align:right;">
0.0000012
</td>
<td style="text-align:right;">
2.647596e+00
</td>
</tr>
<tr>
<td style="text-align:left;">
HS/US-CO
</td>
<td style="text-align:right;">
0.000000e+00
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.000000e+00
</td>
</tr>
<tr>
<td style="text-align:left;">
ORE/US-CO
</td>
<td style="text-align:right;">
1.024095e+03
</td>
<td style="text-align:right;">
0.1149480
</td>
<td style="text-align:right;">
2.021138e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
532RL/US-CO
</td>
<td style="text-align:right;">
2.653179e+02
</td>
<td style="text-align:right;">
0.0320451
</td>
<td style="text-align:right;">
8.398994e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
5411/US-CO
</td>
<td style="text-align:right;">
4.392880e+00
</td>
<td style="text-align:right;">
0.0280155
</td>
<td style="text-align:right;">
5.671653e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
5415/US-CO
</td>
<td style="text-align:right;">
4.689810e+01
</td>
<td style="text-align:right;">
0.0304356
</td>
<td style="text-align:right;">
4.944459e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
5412OP/US-CO
</td>
<td style="text-align:right;">
1.215137e+02
</td>
<td style="text-align:right;">
0.0895166
</td>
<td style="text-align:right;">
1.392125e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
55/US-CO
</td>
<td style="text-align:right;">
4.716880e+01
</td>
<td style="text-align:right;">
0.0665011
</td>
<td style="text-align:right;">
1.345191e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
561/US-CO
</td>
<td style="text-align:right;">
3.267777e+02
</td>
<td style="text-align:right;">
0.1537784
</td>
<td style="text-align:right;">
1.192436e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
562/US-CO
</td>
<td style="text-align:right;">
3.238884e+02
</td>
<td style="text-align:right;">
0.0018362
</td>
<td style="text-align:right;">
2.487192e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
61/US-CO
</td>
<td style="text-align:right;">
6.295236e+00
</td>
<td style="text-align:right;">
0.0075058
</td>
<td style="text-align:right;">
1.739652e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
621/US-CO
</td>
<td style="text-align:right;">
5.489256e-01
</td>
<td style="text-align:right;">
0.0007536
</td>
<td style="text-align:right;">
7.378505e+01
</td>
</tr>
<tr>
<td style="text-align:left;">
622/US-CO
</td>
<td style="text-align:right;">
1.477660e-02
</td>
<td style="text-align:right;">
0.0000148
</td>
<td style="text-align:right;">
1.282599e+00
</td>
</tr>
<tr>
<td style="text-align:left;">
623/US-CO
</td>
<td style="text-align:right;">
7.639530e-02
</td>
<td style="text-align:right;">
0.0001098
</td>
<td style="text-align:right;">
5.408795e+00
</td>
</tr>
<tr>
<td style="text-align:left;">
624/US-CO
</td>
<td style="text-align:right;">
2.234784e-01
</td>
<td style="text-align:right;">
0.0005189
</td>
<td style="text-align:right;">
2.102401e+01
</td>
</tr>
<tr>
<td style="text-align:left;">
711AS/US-CO
</td>
<td style="text-align:right;">
3.428043e+00
</td>
<td style="text-align:right;">
0.0051279
</td>
<td style="text-align:right;">
7.994970e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
713/US-CO
</td>
<td style="text-align:right;">
2.001414e+01
</td>
<td style="text-align:right;">
0.0017502
</td>
<td style="text-align:right;">
1.356450e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
721/US-CO
</td>
<td style="text-align:right;">
7.372902e+00
</td>
<td style="text-align:right;">
0.0111605
</td>
<td style="text-align:right;">
7.806272e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
722/US-CO
</td>
<td style="text-align:right;">
3.277113e+01
</td>
<td style="text-align:right;">
0.0363217
</td>
<td style="text-align:right;">
1.676264e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
81/US-CO
</td>
<td style="text-align:right;">
1.721671e+02
</td>
<td style="text-align:right;">
0.0921409
</td>
<td style="text-align:right;">
1.012775e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
GFGD/US-CO
</td>
<td style="text-align:right;">
0.000000e+00
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.000000e+00
</td>
</tr>
<tr>
<td style="text-align:left;">
GFGN/US-CO
</td>
<td style="text-align:right;">
0.000000e+00
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.000000e+00
</td>
</tr>
<tr>
<td style="text-align:left;">
GFE/US-CO
</td>
<td style="text-align:right;">
4.204614e+01
</td>
<td style="text-align:right;">
0.0015709
</td>
<td style="text-align:right;">
2.056540e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
GSLG/US-CO
</td>
<td style="text-align:right;">
0.000000e+00
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.000000e+00
</td>
</tr>
<tr>
<td style="text-align:left;">
GSLE/US-CO
</td>
<td style="text-align:right;">
8.569876e+02
</td>
<td style="text-align:right;">
0.0000308
</td>
<td style="text-align:right;">
2.530495e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
Used/US-CO
</td>
<td style="text-align:right;">
5.594438e+02
</td>
<td style="text-align:right;">
0.0054786
</td>
<td style="text-align:right;">
2.348229e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
Other/US-CO
</td>
<td style="text-align:right;">
0.000000e+00
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.000000e+00
</td>
</tr>
<tr>
<td style="text-align:left;">
111CA/RoUS
</td>
<td style="text-align:right;">
4.966973e+03
</td>
<td style="text-align:right;">
0.0063571
</td>
<td style="text-align:right;">
1.064502e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
113FF/RoUS
</td>
<td style="text-align:right;">
5.863316e+02
</td>
<td style="text-align:right;">
0.0305740
</td>
<td style="text-align:right;">
2.942480e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
211/RoUS
</td>
<td style="text-align:right;">
1.117211e+04
</td>
<td style="text-align:right;">
0.0060399
</td>
<td style="text-align:right;">
4.711086e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
212/RoUS
</td>
<td style="text-align:right;">
5.888301e+03
</td>
<td style="text-align:right;">
0.0152429
</td>
<td style="text-align:right;">
4.878851e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
213/RoUS
</td>
<td style="text-align:right;">
3.174171e+02
</td>
<td style="text-align:right;">
0.0018619
</td>
<td style="text-align:right;">
3.253893e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
22/RoUS
</td>
<td style="text-align:right;">
3.039037e+04
</td>
<td style="text-align:right;">
0.0209616
</td>
<td style="text-align:right;">
8.879707e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
23/RoUS
</td>
<td style="text-align:right;">
3.299742e+02
</td>
<td style="text-align:right;">
0.0183865
</td>
<td style="text-align:right;">
2.299112e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
321/RoUS
</td>
<td style="text-align:right;">
1.489084e+03
</td>
<td style="text-align:right;">
0.0833790
</td>
<td style="text-align:right;">
8.952028e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
327/RoUS
</td>
<td style="text-align:right;">
1.430921e+04
</td>
<td style="text-align:right;">
0.0613247
</td>
<td style="text-align:right;">
1.028948e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
331/RoUS
</td>
<td style="text-align:right;">
1.279860e+04
</td>
<td style="text-align:right;">
0.0410447
</td>
<td style="text-align:right;">
6.742383e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
332/RoUS
</td>
<td style="text-align:right;">
2.115605e+03
</td>
<td style="text-align:right;">
0.2405432
</td>
<td style="text-align:right;">
2.574237e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
333/RoUS
</td>
<td style="text-align:right;">
3.229353e+02
</td>
<td style="text-align:right;">
0.0390668
</td>
<td style="text-align:right;">
5.904598e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
334/RoUS
</td>
<td style="text-align:right;">
1.872361e+02
</td>
<td style="text-align:right;">
0.0261315
</td>
<td style="text-align:right;">
7.737725e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
335/RoUS
</td>
<td style="text-align:right;">
6.608946e+02
</td>
<td style="text-align:right;">
0.0534731
</td>
<td style="text-align:right;">
8.989865e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
3361MV/RoUS
</td>
<td style="text-align:right;">
1.084401e+02
</td>
<td style="text-align:right;">
0.0101017
</td>
<td style="text-align:right;">
1.750520e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
3364OT/RoUS
</td>
<td style="text-align:right;">
1.376615e+01
</td>
<td style="text-align:right;">
0.0017948
</td>
<td style="text-align:right;">
3.156191e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
337/RoUS
</td>
<td style="text-align:right;">
2.776077e+02
</td>
<td style="text-align:right;">
0.0396607
</td>
<td style="text-align:right;">
3.497137e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
339/RoUS
</td>
<td style="text-align:right;">
1.946025e+01
</td>
<td style="text-align:right;">
0.0036073
</td>
<td style="text-align:right;">
6.156298e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
311FT/RoUS
</td>
<td style="text-align:right;">
1.505079e+02
</td>
<td style="text-align:right;">
0.0045568
</td>
<td style="text-align:right;">
6.798366e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
313TT/RoUS
</td>
<td style="text-align:right;">
1.909240e+02
</td>
<td style="text-align:right;">
0.0110628
</td>
<td style="text-align:right;">
9.088859e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
315AL/RoUS
</td>
<td style="text-align:right;">
2.660146e+00
</td>
<td style="text-align:right;">
0.0006544
</td>
<td style="text-align:right;">
6.166385e+01
</td>
</tr>
<tr>
<td style="text-align:left;">
322/RoUS
</td>
<td style="text-align:right;">
2.420183e+03
</td>
<td style="text-align:right;">
0.0178552
</td>
<td style="text-align:right;">
3.001447e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
323/RoUS
</td>
<td style="text-align:right;">
8.378829e+01
</td>
<td style="text-align:right;">
0.0091811
</td>
<td style="text-align:right;">
9.873997e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
324/RoUS
</td>
<td style="text-align:right;">
1.152162e+04
</td>
<td style="text-align:right;">
0.0085224
</td>
<td style="text-align:right;">
8.435607e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
325/RoUS
</td>
<td style="text-align:right;">
1.087273e+04
</td>
<td style="text-align:right;">
0.0266485
</td>
<td style="text-align:right;">
1.193364e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
326/RoUS
</td>
<td style="text-align:right;">
8.551564e+02
</td>
<td style="text-align:right;">
0.0519644
</td>
<td style="text-align:right;">
6.151848e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
42/RoUS
</td>
<td style="text-align:right;">
1.226334e+03
</td>
<td style="text-align:right;">
0.3393886
</td>
<td style="text-align:right;">
7.512516e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
441/RoUS
</td>
<td style="text-align:right;">
6.348446e+00
</td>
<td style="text-align:right;">
0.0046059
</td>
<td style="text-align:right;">
5.596137e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
445/RoUS
</td>
<td style="text-align:right;">
1.296415e+00
</td>
<td style="text-align:right;">
0.0007135
</td>
<td style="text-align:right;">
3.982129e+01
</td>
</tr>
<tr>
<td style="text-align:left;">
452/RoUS
</td>
<td style="text-align:right;">
1.022220e+00
</td>
<td style="text-align:right;">
0.0022470
</td>
<td style="text-align:right;">
1.201041e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
4A0/RoUS
</td>
<td style="text-align:right;">
5.601405e+01
</td>
<td style="text-align:right;">
0.0840431
</td>
<td style="text-align:right;">
7.857029e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
481/RoUS
</td>
<td style="text-align:right;">
1.750549e+03
</td>
<td style="text-align:right;">
0.0072024
</td>
<td style="text-align:right;">
9.031926e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
482/RoUS
</td>
<td style="text-align:right;">
1.522275e+03
</td>
<td style="text-align:right;">
0.0000385
</td>
<td style="text-align:right;">
1.807230e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
483/RoUS
</td>
<td style="text-align:right;">
7.278220e+02
</td>
<td style="text-align:right;">
0.0024704
</td>
<td style="text-align:right;">
3.730316e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
484/RoUS
</td>
<td style="text-align:right;">
1.500111e+04
</td>
<td style="text-align:right;">
0.0835219
</td>
<td style="text-align:right;">
9.286405e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
485/RoUS
</td>
<td style="text-align:right;">
2.099277e+02
</td>
<td style="text-align:right;">
0.0082457
</td>
<td style="text-align:right;">
5.766567e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
486/RoUS
</td>
<td style="text-align:right;">
6.146696e+03
</td>
<td style="text-align:right;">
0.0032062
</td>
<td style="text-align:right;">
2.363776e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
487OS/RoUS
</td>
<td style="text-align:right;">
1.174929e+03
</td>
<td style="text-align:right;">
0.0642851
</td>
<td style="text-align:right;">
5.534163e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
493/RoUS
</td>
<td style="text-align:right;">
6.615116e+02
</td>
<td style="text-align:right;">
0.0905361
</td>
<td style="text-align:right;">
4.986183e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
511/RoUS
</td>
<td style="text-align:right;">
2.562036e+00
</td>
<td style="text-align:right;">
0.0032982
</td>
<td style="text-align:right;">
1.357152e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
512/RoUS
</td>
<td style="text-align:right;">
2.007379e+00
</td>
<td style="text-align:right;">
0.0035631
</td>
<td style="text-align:right;">
8.368217e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
513/RoUS
</td>
<td style="text-align:right;">
7.722263e+01
</td>
<td style="text-align:right;">
0.0137903
</td>
<td style="text-align:right;">
6.848177e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
514/RoUS
</td>
<td style="text-align:right;">
3.537902e+01
</td>
<td style="text-align:right;">
0.0128909
</td>
<td style="text-align:right;">
4.496251e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
521CI/RoUS
</td>
<td style="text-align:right;">
1.719156e+02
</td>
<td style="text-align:right;">
0.0775196
</td>
<td style="text-align:right;">
2.249601e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
523/RoUS
</td>
<td style="text-align:right;">
2.953318e+01
</td>
<td style="text-align:right;">
0.0093819
</td>
<td style="text-align:right;">
3.343348e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
524/RoUS
</td>
<td style="text-align:right;">
1.289113e+01
</td>
<td style="text-align:right;">
0.0405015
</td>
<td style="text-align:right;">
1.070738e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
525/RoUS
</td>
<td style="text-align:right;">
1.358289e+01
</td>
<td style="text-align:right;">
0.0000661
</td>
<td style="text-align:right;">
3.643286e+01
</td>
</tr>
<tr>
<td style="text-align:left;">
HS/RoUS
</td>
<td style="text-align:right;">
0.000000e+00
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.000000e+00
</td>
</tr>
<tr>
<td style="text-align:left;">
ORE/RoUS
</td>
<td style="text-align:right;">
5.861879e+02
</td>
<td style="text-align:right;">
0.0238692
</td>
<td style="text-align:right;">
9.406523e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
532RL/RoUS
</td>
<td style="text-align:right;">
5.071046e+02
</td>
<td style="text-align:right;">
0.0507270
</td>
<td style="text-align:right;">
1.912941e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
5411/RoUS
</td>
<td style="text-align:right;">
4.976047e+00
</td>
<td style="text-align:right;">
0.0291315
</td>
<td style="text-align:right;">
7.275555e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
5415/RoUS
</td>
<td style="text-align:right;">
9.326748e+01
</td>
<td style="text-align:right;">
0.0320129
</td>
<td style="text-align:right;">
5.175161e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
5412OP/RoUS
</td>
<td style="text-align:right;">
3.164060e+03
</td>
<td style="text-align:right;">
0.5211508
</td>
<td style="text-align:right;">
8.406747e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
55/RoUS
</td>
<td style="text-align:right;">
6.481340e+01
</td>
<td style="text-align:right;">
0.0899175
</td>
<td style="text-align:right;">
1.588037e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
561/RoUS
</td>
<td style="text-align:right;">
8.145808e+02
</td>
<td style="text-align:right;">
0.3254748
</td>
<td style="text-align:right;">
2.393798e+04
</td>
</tr>
<tr>
<td style="text-align:left;">
562/RoUS
</td>
<td style="text-align:right;">
8.322570e+03
</td>
<td style="text-align:right;">
0.0284310
</td>
<td style="text-align:right;">
3.951625e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
61/RoUS
</td>
<td style="text-align:right;">
4.724916e+01
</td>
<td style="text-align:right;">
0.0285297
</td>
<td style="text-align:right;">
7.929376e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
621/RoUS
</td>
<td style="text-align:right;">
4.422952e-01
</td>
<td style="text-align:right;">
0.0003530
</td>
<td style="text-align:right;">
3.491578e+01
</td>
</tr>
<tr>
<td style="text-align:left;">
622/RoUS
</td>
<td style="text-align:right;">
8.270980e-02
</td>
<td style="text-align:right;">
0.0000216
</td>
<td style="text-align:right;">
2.076703e+00
</td>
</tr>
<tr>
<td style="text-align:left;">
623/RoUS
</td>
<td style="text-align:right;">
4.483090e-02
</td>
<td style="text-align:right;">
0.0000789
</td>
<td style="text-align:right;">
3.608306e+00
</td>
</tr>
<tr>
<td style="text-align:left;">
624/RoUS
</td>
<td style="text-align:right;">
8.619530e-02
</td>
<td style="text-align:right;">
0.0001819
</td>
<td style="text-align:right;">
6.374182e+00
</td>
</tr>
<tr>
<td style="text-align:left;">
711AS/RoUS
</td>
<td style="text-align:right;">
1.141214e+01
</td>
<td style="text-align:right;">
0.0097607
</td>
<td style="text-align:right;">
1.500314e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
713/RoUS
</td>
<td style="text-align:right;">
1.269557e+02
</td>
<td style="text-align:right;">
0.0077336
</td>
<td style="text-align:right;">
4.674542e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
721/RoUS
</td>
<td style="text-align:right;">
2.683401e+01
</td>
<td style="text-align:right;">
0.0184385
</td>
<td style="text-align:right;">
1.244160e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
722/RoUS
</td>
<td style="text-align:right;">
5.243697e+01
</td>
<td style="text-align:right;">
0.0425126
</td>
<td style="text-align:right;">
1.833734e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
81/RoUS
</td>
<td style="text-align:right;">
9.518395e+01
</td>
<td style="text-align:right;">
0.0440548
</td>
<td style="text-align:right;">
4.899393e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
GFGD/RoUS
</td>
<td style="text-align:right;">
0.000000e+00
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.000000e+00
</td>
</tr>
<tr>
<td style="text-align:left;">
GFGN/RoUS
</td>
<td style="text-align:right;">
0.000000e+00
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.000000e+00
</td>
</tr>
<tr>
<td style="text-align:left;">
GFE/RoUS
</td>
<td style="text-align:right;">
3.800443e+02
</td>
<td style="text-align:right;">
0.0178622
</td>
<td style="text-align:right;">
1.782801e+03
</td>
</tr>
<tr>
<td style="text-align:left;">
GSLG/RoUS
</td>
<td style="text-align:right;">
0.000000e+00
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.000000e+00
</td>
</tr>
<tr>
<td style="text-align:left;">
GSLE/RoUS
</td>
<td style="text-align:right;">
6.169977e+02
</td>
<td style="text-align:right;">
0.0003517
</td>
<td style="text-align:right;">
3.167451e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
Used/RoUS
</td>
<td style="text-align:right;">
4.196063e+02
</td>
<td style="text-align:right;">
0.0049548
</td>
<td style="text-align:right;">
9.584619e+02
</td>
</tr>
<tr>
<td style="text-align:left;">
Other/RoUS
</td>
<td style="text-align:right;">
0.000000e+00
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.000000e+00
</td>
</tr>
</tbody>
</table>
