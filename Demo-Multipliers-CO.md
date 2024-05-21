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

|        | Output_Multiplier |
|:-------|------------------:|
| CO     |         1.3326813 |
| RoUS   |         0.4540408 |
| All US |         1.7867221 |

Look at total output stimulated in CO, the rest of the U.S., and all the
U.S.

``` r
total_output_stimulated_CO <- demand*output_multiplier_CO
total_output_stimulated_RoUS <- demand*output_multiplier_RoUS
total_output_stimulated <- demand*output_multiplier

kable(data.frame(`Direct_Indirect_Output_Stimulated_$`=c(total_output_stimulated_CO,total_output_stimulated_RoUS,total_output_stimulated), row.names=regions))
```

|        | Direct_Indirect_Output_Stimulated\_. |
|:-------|-------------------------------------:|
| CO     |                            2665362.6 |
| RoUS   |                             908081.7 |
| All US |                            3573444.3 |

Now calculate the direct and indirect impacts on greenhouse gas
emissions, jobs supported, and value added.

``` r
y <- setNames(demand, sector)
y <- formatDemandVector(y, CO$L)
# Use a direct perspective calculation which will show the impact in the sectors in which it occurs
result <- calculateEEIOModel(CO, perspective = "DIRECT", use_domestic_requirements=TRUE, demand = y)
``` 

    ## 2024-05-21 15:14:41.902822 INFO::Calculating Direct Perspective LCI...
    ## 2024-05-21 15:14:41.933144 INFO::Calculating Direct Perspective LCIA...
    ## 2024-05-21 15:14:41.967774 INFO::Result calculation complete.

Show the totals for the results for GHGs (kg CO2e), Jobs, and Value
Added (\$) by Region

``` r
CO_result <- colSums(result$LCIA_d[grep("US-CO",row.names(result$LCIA_d)),])
RoUS_result <- colSums(result$LCIA_d[grep("RoUS",row.names(result$LCIA_d)),])
US_result <- CO_result+RoUS_result

kable(t(data.frame(CO=CO_result,RoUS=RoUS_result,`AllUS`=US_result)))
```

|       | Greenhouse Gases | Jobs Supported | Value Added |
|:------|-----------------:|---------------:|------------:|
| CO    |         247223.9 |       9.866408 |   1353392.9 |
| RoUS  |         156217.8 |       2.957062 |    480119.8 |
| AllUS |         403441.6 |      12.823470 |   1833512.7 |

The full results can be viewed by sector and location

``` r
kable(result$LCIA_d)
```

|              | Greenhouse Gases | Jobs Supported |  Value Added |
|:-------------|-----------------:|---------------:|-------------:|
| 111CA/US-CO  |     4.372717e+03 |      0.0046477 | 6.309772e+02 |
| 113FF/US-CO  |     6.149429e+02 |      0.0121821 | 1.512345e+03 |
| 211/US-CO    |     1.888866e+03 |      0.0015766 | 9.660404e+02 |
| 212/US-CO    |     1.782466e+04 |      0.0381971 | 1.145973e+04 |
| 213/US-CO    |     2.468659e+02 |      0.0019355 | 3.677521e+02 |
| 22/US-CO     |     4.388679e+04 |      0.0231614 | 8.589544e+03 |
| 23/US-CO     |     6.678314e+04 |      7.4739166 | 1.020958e+06 |
| 321/US-CO    |     2.976932e+03 |      0.0847331 | 1.088232e+04 |
| 327/US-CO    |     3.554883e+04 |      0.1555065 | 2.927643e+04 |
| 331/US-CO    |     8.711160e+03 |      0.0149515 | 2.262371e+03 |
| 332/US-CO    |     2.408247e+03 |      0.1611999 | 1.900603e+04 |
| 333/US-CO    |     7.173337e+02 |      0.0582765 | 7.242672e+03 |
| 334/US-CO    |     1.009701e+02 |      0.0078160 | 1.928873e+03 |
| 335/US-CO    |     1.161121e+03 |      0.0283527 | 5.483406e+03 |
| 3361MV/US-CO |     1.336798e+02 |      0.0022231 | 2.101388e+02 |
| 3364OT/US-CO |     4.430213e+00 |      0.0003269 | 6.950088e+01 |
| 337/US-CO    |     3.731103e+02 |      0.0459009 | 5.134083e+03 |
| 339/US-CO    |     8.321836e+00 |      0.0017598 | 3.220868e+02 |
| 311FT/US-CO  |     5.840084e+01 |      0.0016976 | 2.368277e+02 |
| 313TT/US-CO  |     9.315244e+01 |      0.0020982 | 1.298549e+02 |
| 315AL/US-CO  |     3.647327e-01 |      0.0000437 | 4.724435e+00 |
| 322/US-CO    |     2.523393e+03 |      0.0045183 | 6.269971e+02 |
| 323/US-CO    |     3.747567e+01 |      0.0034265 | 3.485353e+02 |
| 324/US-CO    |     1.977179e+04 |      0.0087999 | 7.383219e+03 |
| 325/US-CO    |     9.909314e+03 |      0.0217245 | 5.037961e+03 |
| 326/US-CO    |     1.171139e+03 |      0.0431529 | 6.443880e+03 |
| 42/US-CO     |     2.132506e+02 |      0.0973550 | 2.025622e+04 |
| 441/US-CO    |     4.485222e+01 |      0.0331775 | 2.815312e+03 |
| 445/US-CO    |     6.559368e+00 |      0.0035476 | 2.528849e+02 |
| 452/US-CO    |     1.042600e+01 |      0.0255294 | 1.597947e+03 |
| 4A0/US-CO    |     3.440352e+02 |      0.5663365 | 5.183665e+04 |
| 481/US-CO    |     6.063539e+02 |      0.0027635 | 3.127186e+02 |
| 482/US-CO    |     1.546266e+03 |      0.0000000 | 1.835533e+03 |
| 483/US-CO    |     9.249784e+01 |      0.0001750 | 1.748185e+01 |
| 484/US-CO    |     1.630790e+04 |      0.0888263 | 1.009370e+04 |
| 485/US-CO    |     3.386896e+02 |      0.0094590 | 6.661151e+02 |
| 486/US-CO    |     9.137222e+02 |      0.0003977 | 4.735877e+02 |
| 487OS/US-CO  |     1.124001e+03 |      0.0590048 | 4.739444e+03 |
| 493/US-CO    |     3.662863e+02 |      0.0388361 | 1.905210e+03 |
| 511/US-CO    |     4.366303e-01 |      0.0011699 | 4.235670e+02 |
| 512/US-CO    |     3.036321e-01 |      0.0003225 | 4.426739e+01 |
| 513/US-CO    |     3.679597e+01 |      0.0158763 | 4.748579e+03 |
| 514/US-CO    |     1.460722e+01 |      0.0096818 | 2.816527e+03 |
| 521CI/US-CO  |     3.138332e+01 |      0.0138574 | 2.858563e+03 |
| 523/US-CO    |     3.434655e+01 |      0.0142382 | 2.916921e+03 |
| 524/US-CO    |     1.395269e+00 |      0.0041663 | 8.895953e+02 |
| 525/US-CO    |     1.161582e+00 |      0.0000012 | 2.647596e+00 |
| HS/US-CO     |     0.000000e+00 |      0.0000000 | 0.000000e+00 |
| ORE/US-CO    |     1.024095e+03 |      0.1149480 | 2.021138e+04 |
| 532RL/US-CO  |     2.653179e+02 |      0.0320451 | 8.398994e+03 |
| 5411/US-CO   |     4.392880e+00 |      0.0280155 | 5.671653e+03 |
| 5415/US-CO   |     4.689810e+01 |      0.0304356 | 4.944459e+03 |
| 5412OP/US-CO |     1.215137e+02 |      0.0895166 | 1.392125e+04 |
| 55/US-CO     |     4.716880e+01 |      0.0665011 | 1.345191e+04 |
| 561/US-CO    |     3.267777e+02 |      0.1537784 | 1.192436e+04 |
| 562/US-CO    |     3.238884e+02 |      0.0018362 | 2.487192e+02 |
| 61/US-CO     |     6.295236e+00 |      0.0075058 | 1.739652e+02 |
| 621/US-CO    |     5.489256e-01 |      0.0007536 | 7.378505e+01 |
| 622/US-CO    |     1.477660e-02 |      0.0000148 | 1.282599e+00 |
| 623/US-CO    |     7.639530e-02 |      0.0001098 | 5.408795e+00 |
| 624/US-CO    |     2.234784e-01 |      0.0005189 | 2.102401e+01 |
| 711AS/US-CO  |     3.428043e+00 |      0.0051279 | 7.994970e+02 |
| 713/US-CO    |     2.001414e+01 |      0.0017502 | 1.356450e+02 |
| 721/US-CO    |     7.372902e+00 |      0.0111605 | 7.806272e+02 |
| 722/US-CO    |     3.277113e+01 |      0.0363217 | 1.676264e+03 |
| 81/US-CO     |     1.721671e+02 |      0.0921409 | 1.012775e+04 |
| GFGD/US-CO   |     0.000000e+00 |      0.0000000 | 0.000000e+00 |
| GFGN/US-CO   |     0.000000e+00 |      0.0000000 | 0.000000e+00 |
| GFE/US-CO    |     4.204614e+01 |      0.0015709 | 2.056540e+02 |
| GSLG/US-CO   |     0.000000e+00 |      0.0000000 | 0.000000e+00 |
| GSLE/US-CO   |     8.569876e+02 |      0.0000308 | 2.530495e+02 |
| Used/US-CO   |     5.594438e+02 |      0.0054786 | 2.348229e+03 |
| Other/US-CO  |     0.000000e+00 |      0.0000000 | 0.000000e+00 |
| 111CA/RoUS   |     4.966973e+03 |      0.0063571 | 1.064502e+03 |
| 113FF/RoUS   |     5.863316e+02 |      0.0305740 | 2.942480e+03 |
| 211/RoUS     |     1.117211e+04 |      0.0060399 | 4.711086e+03 |
| 212/RoUS     |     5.888301e+03 |      0.0152429 | 4.878851e+03 |
| 213/RoUS     |     3.174171e+02 |      0.0018619 | 3.253893e+02 |
| 22/RoUS      |     3.039037e+04 |      0.0209616 | 8.879707e+03 |
| 23/RoUS      |     3.299742e+02 |      0.0183865 | 2.299112e+03 |
| 321/RoUS     |     1.489084e+03 |      0.0833790 | 8.952028e+03 |
| 327/RoUS     |     1.430921e+04 |      0.0613247 | 1.028948e+04 |
| 331/RoUS     |     1.279860e+04 |      0.0410447 | 6.742383e+03 |
| 332/RoUS     |     2.115605e+03 |      0.2405432 | 2.574237e+04 |
| 333/RoUS     |     3.229353e+02 |      0.0390668 | 5.904598e+03 |
| 334/RoUS     |     1.872361e+02 |      0.0261315 | 7.737725e+03 |
| 335/RoUS     |     6.608946e+02 |      0.0534731 | 8.989865e+03 |
| 3361MV/RoUS  |     1.084401e+02 |      0.0101017 | 1.750520e+03 |
| 3364OT/RoUS  |     1.376615e+01 |      0.0017948 | 3.156191e+02 |
| 337/RoUS     |     2.776077e+02 |      0.0396607 | 3.497137e+03 |
| 339/RoUS     |     1.946025e+01 |      0.0036073 | 6.156298e+02 |
| 311FT/RoUS   |     1.505079e+02 |      0.0045568 | 6.798366e+02 |
| 313TT/RoUS   |     1.909240e+02 |      0.0110628 | 9.088859e+02 |
| 315AL/RoUS   |     2.660146e+00 |      0.0006544 | 6.166385e+01 |
| 322/RoUS     |     2.420183e+03 |      0.0178552 | 3.001447e+03 |
| 323/RoUS     |     8.378829e+01 |      0.0091811 | 9.873997e+02 |
| 324/RoUS     |     1.152162e+04 |      0.0085224 | 8.435607e+03 |
| 325/RoUS     |     1.087273e+04 |      0.0266485 | 1.193364e+04 |
| 326/RoUS     |     8.551564e+02 |      0.0519644 | 6.151848e+03 |
| 42/RoUS      |     1.226334e+03 |      0.3393886 | 7.512516e+04 |
| 441/RoUS     |     6.348446e+00 |      0.0046059 | 5.596137e+02 |
| 445/RoUS     |     1.296415e+00 |      0.0007135 | 3.982129e+01 |
| 452/RoUS     |     1.022220e+00 |      0.0022470 | 1.201041e+02 |
| 4A0/RoUS     |     5.601405e+01 |      0.0840431 | 7.857029e+03 |
| 481/RoUS     |     1.750549e+03 |      0.0072024 | 9.031926e+02 |
| 482/RoUS     |     1.522275e+03 |      0.0000385 | 1.807230e+03 |
| 483/RoUS     |     7.278220e+02 |      0.0024704 | 3.730316e+02 |
| 484/RoUS     |     1.500111e+04 |      0.0835219 | 9.286405e+03 |
| 485/RoUS     |     2.099277e+02 |      0.0082457 | 5.766567e+02 |
| 486/RoUS     |     6.146696e+03 |      0.0032062 | 2.363776e+03 |
| 487OS/RoUS   |     1.174929e+03 |      0.0642851 | 5.534163e+03 |
| 493/RoUS     |     6.615116e+02 |      0.0905361 | 4.986183e+03 |
| 511/RoUS     |     2.562036e+00 |      0.0032982 | 1.357152e+03 |
| 512/RoUS     |     2.007379e+00 |      0.0035631 | 8.368217e+02 |
| 513/RoUS     |     7.722263e+01 |      0.0137903 | 6.848177e+03 |
| 514/RoUS     |     3.537902e+01 |      0.0128909 | 4.496251e+03 |
| 521CI/RoUS   |     1.719156e+02 |      0.0775196 | 2.249601e+04 |
| 523/RoUS     |     2.953318e+01 |      0.0093819 | 3.343348e+03 |
| 524/RoUS     |     1.289113e+01 |      0.0405015 | 1.070738e+04 |
| 525/RoUS     |     1.358289e+01 |      0.0000661 | 3.643286e+01 |
| HS/RoUS      |     0.000000e+00 |      0.0000000 | 0.000000e+00 |
| ORE/RoUS     |     5.861879e+02 |      0.0238692 | 9.406523e+03 |
| 532RL/RoUS   |     5.071046e+02 |      0.0507270 | 1.912941e+04 |
| 5411/RoUS    |     4.976047e+00 |      0.0291315 | 7.275555e+03 |
| 5415/RoUS    |     9.326748e+01 |      0.0320129 | 5.175161e+03 |
| 5412OP/RoUS  |     3.164060e+03 |      0.5211508 | 8.406747e+04 |
| 55/RoUS      |     6.481340e+01 |      0.0899175 | 1.588037e+04 |
| 561/RoUS     |     8.145808e+02 |      0.3254748 | 2.393798e+04 |
| 562/RoUS     |     8.322570e+03 |      0.0284310 | 3.951625e+03 |
| 61/RoUS      |     4.724916e+01 |      0.0285297 | 7.929376e+02 |
| 621/RoUS     |     4.422952e-01 |      0.0003530 | 3.491578e+01 |
| 622/RoUS     |     8.270980e-02 |      0.0000216 | 2.076703e+00 |
| 623/RoUS     |     4.483090e-02 |      0.0000789 | 3.608306e+00 |
| 624/RoUS     |     8.619530e-02 |      0.0001819 | 6.374182e+00 |
| 711AS/RoUS   |     1.141214e+01 |      0.0097607 | 1.500314e+03 |
| 713/RoUS     |     1.269557e+02 |      0.0077336 | 4.674542e+02 |
| 721/RoUS     |     2.683401e+01 |      0.0184385 | 1.244160e+03 |
| 722/RoUS     |     5.243697e+01 |      0.0425126 | 1.833734e+03 |
| 81/RoUS      |     9.518395e+01 |      0.0440548 | 4.899393e+03 |
| GFGD/RoUS    |     0.000000e+00 |      0.0000000 | 0.000000e+00 |
| GFGN/RoUS    |     0.000000e+00 |      0.0000000 | 0.000000e+00 |
| GFE/RoUS     |     3.800443e+02 |      0.0178622 | 1.782801e+03 |
| GSLG/RoUS    |     0.000000e+00 |      0.0000000 | 0.000000e+00 |
| GSLE/RoUS    |     6.169977e+02 |      0.0003517 | 3.167451e+02 |
| Used/RoUS    |     4.196063e+02 |      0.0049548 | 9.584619e+02 |
| Other/RoUS   |     0.000000e+00 |      0.0000000 | 0.000000e+00 |
