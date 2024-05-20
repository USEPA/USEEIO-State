# Demonstrate the Computation of Type I Multiplers and Related Impacts of construction spending from a Colorado EEIO model

This script builds a Colorado USEEIO State model for 2020 with
extensions for Jobs, Greenhouse Gas Emissions, and Value Added
components.

It requires *useeior* \>= v1.5.1 which will be installed from github
using the remotes library if *useeior* doesn’t exist already in your R
environment.

Your R environment needs to be setup to use the Project directory to
knit this file (usually from RStudio).

``` r
# Load a local R file with an install_useeior() function
source("R/utils.R")
install_useeior() # This install
library(useeior)
```

Then we build a custom state EEIO model for Colorado using useeior and a
[new custom model specification
file](model_specs/COEEIOv1.0-s-JGV-20.yml). All the raw files including
the relevant StateIO and National totals of Employment and GHGs by
sector, and Indicators (for GHGs to convert to kg CO2e) tables are
retrieved from the EPA Data Commons and stored locally in your
machine-defined `appdata` directory under *stateio* and *flowsa*, and
*lciaformatter*, respectively (each are EPA Tools for Industrial Ecology
modeling).

If you’ve already built it once, it will load a stored RDS version from
an output directory in this project.

``` r
dir <- file.path("output")
local_model_RDS <- "COEEIOv1.0-s-JGV-20.rds"
dir_local_model_RDS <- file.path(dir,local_model_RDS)
if (!file.exists(dir_local_model_RDS)) {
  CO <- buildModel("COEEIOv1.0-s-JGV-20","model_specs/COEEIOv1.0-s-JGV-20.yml")  
  if (!dir.exists(dir)) dir.create(dir)
  saveRDS(CO,dir_local_model_RDS)
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

data.frame(Output_Multiplier=c(output_multiplier_CO,output_multiplier_RoUS,output_multiplier), row.names=regions)
```

    ##        Output_Multiplier
    ## CO             1.3326813
    ## RoUS           0.4540408
    ## All US         1.7867221

Look at total output stimulated in CO, the rest of the U.S., and all the
U.S.

``` r
total_output_stimulated_CO <- demand*output_multiplier_CO
total_output_stimulated_RoUS <- demand*output_multiplier_RoUS
total_output_stimulated <- demand*output_multiplier

data.frame(`Direct_Indirect_Output_Stimulated_$`=c(total_output_stimulated_CO,total_output_stimulated_RoUS,total_output_stimulated), row.names=regions)
```

    ##        Direct_Indirect_Output_Stimulated_.
    ## CO                               2665362.6
    ## RoUS                              908081.7
    ## All US                           3573444.3

Now calculate the direct and indirect impacts on greenhouse gas
emissions, jobs supported, and value added.

Show the totals for the results for GHGs (kg CO2e), Jobs, and Value
Added (\$)

``` r
colSums(result$LCIA_d)
```

    ## Greenhouse Gases   Jobs Supported      Value Added 
    ##     4.034416e+05     1.282347e+01     1.833513e+06

The full results can be viewed by sector and location

``` r
result$LCIA_d
```

    ##              Greenhouse Gases Jobs Supported  Value Added
    ## 111CA/US-CO      4.372717e+03   4.647717e-03 6.309772e+02
    ## 113FF/US-CO      6.149429e+02   1.218213e-02 1.512345e+03
    ## 211/US-CO        1.888866e+03   1.576552e-03 9.660404e+02
    ## 212/US-CO        1.782466e+04   3.819706e-02 1.145973e+04
    ## 213/US-CO        2.468659e+02   1.935451e-03 3.677521e+02
    ## 22/US-CO         4.388679e+04   2.316143e-02 8.589544e+03
    ## 23/US-CO         6.678314e+04   7.473917e+00 1.020958e+06
    ## 321/US-CO        2.976932e+03   8.473305e-02 1.088232e+04
    ## 327/US-CO        3.554883e+04   1.555065e-01 2.927643e+04
    ## 331/US-CO        8.711160e+03   1.495153e-02 2.262371e+03
    ## 332/US-CO        2.408247e+03   1.611999e-01 1.900603e+04
    ## 333/US-CO        7.173337e+02   5.827649e-02 7.242672e+03
    ## 334/US-CO        1.009701e+02   7.815972e-03 1.928873e+03
    ## 335/US-CO        1.161121e+03   2.835273e-02 5.483406e+03
    ## 3361MV/US-CO     1.336798e+02   2.223072e-03 2.101388e+02
    ## 3364OT/US-CO     4.430213e+00   3.269347e-04 6.950088e+01
    ## 337/US-CO        3.731103e+02   4.590086e-02 5.134083e+03
    ## 339/US-CO        8.321836e+00   1.759807e-03 3.220868e+02
    ## 311FT/US-CO      5.840083e+01   1.697586e-03 2.368277e+02
    ## 313TT/US-CO      9.315244e+01   2.098167e-03 1.298549e+02
    ## 315AL/US-CO      3.647327e-01   4.370743e-05 4.724435e+00
    ## 322/US-CO        2.523393e+03   4.518305e-03 6.269971e+02
    ## 323/US-CO        3.747567e+01   3.426524e-03 3.485353e+02
    ## 324/US-CO        1.977179e+04   8.799872e-03 7.383219e+03
    ## 325/US-CO        9.909314e+03   2.172450e-02 5.037961e+03
    ## 326/US-CO        1.171139e+03   4.315291e-02 6.443880e+03
    ## 42/US-CO         2.132506e+02   9.735498e-02 2.025622e+04
    ## 441/US-CO        4.485222e+01   3.317747e-02 2.815312e+03
    ## 445/US-CO        6.559368e+00   3.547623e-03 2.528849e+02
    ## 452/US-CO        1.042600e+01   2.552938e-02 1.597947e+03
    ## 4A0/US-CO        3.440352e+02   5.663365e-01 5.183665e+04
    ## 481/US-CO        6.063539e+02   2.763529e-03 3.127186e+02
    ## 482/US-CO        1.546266e+03   2.616462e-08 1.835533e+03
    ## 483/US-CO        9.249784e+01   1.750242e-04 1.748185e+01
    ## 484/US-CO        1.630790e+04   8.882629e-02 1.009370e+04
    ## 485/US-CO        3.386896e+02   9.459019e-03 6.661151e+02
    ## 486/US-CO        9.137222e+02   3.976986e-04 4.735877e+02
    ## 487OS/US-CO      1.124001e+03   5.900475e-02 4.739444e+03
    ## 493/US-CO        3.662863e+02   3.883612e-02 1.905210e+03
    ## 511/US-CO        4.366303e-01   1.169902e-03 4.235670e+02
    ## 512/US-CO        3.036321e-01   3.224904e-04 4.426739e+01
    ## 513/US-CO        3.679597e+01   1.587634e-02 4.748579e+03
    ## 514/US-CO        1.460722e+01   9.681848e-03 2.816527e+03
    ## 521CI/US-CO      3.138332e+01   1.385743e-02 2.858563e+03
    ## 523/US-CO        3.434655e+01   1.423819e-02 2.916921e+03
    ## 524/US-CO        1.395269e+00   4.166302e-03 8.895953e+02
    ## 525/US-CO        1.161582e+00   1.175073e-06 2.647596e+00
    ## HS/US-CO         0.000000e+00   0.000000e+00 0.000000e+00
    ## ORE/US-CO        1.024095e+03   1.149480e-01 2.021138e+04
    ## 532RL/US-CO      2.653179e+02   3.204515e-02 8.398994e+03
    ## 5411/US-CO       4.392880e+00   2.801548e-02 5.671653e+03
    ## 5415/US-CO       4.689810e+01   3.043555e-02 4.944459e+03
    ## 5412OP/US-CO     1.215137e+02   8.951656e-02 1.392125e+04
    ## 55/US-CO         4.716880e+01   6.650109e-02 1.345191e+04
    ## 561/US-CO        3.267777e+02   1.537784e-01 1.192436e+04
    ## 562/US-CO        3.238884e+02   1.836212e-03 2.487192e+02
    ## 61/US-CO         6.295236e+00   7.505835e-03 1.739652e+02
    ## 621/US-CO        5.489256e-01   7.535613e-04 7.378505e+01
    ## 622/US-CO        1.477659e-02   1.477157e-05 1.282599e+00
    ## 623/US-CO        7.639535e-02   1.097729e-04 5.408795e+00
    ## 624/US-CO        2.234784e-01   5.189004e-04 2.102401e+01
    ## 711AS/US-CO      3.428043e+00   5.127886e-03 7.994970e+02
    ## 713/US-CO        2.001414e+01   1.750240e-03 1.356450e+02
    ## 721/US-CO        7.372902e+00   1.116046e-02 7.806272e+02
    ## 722/US-CO        3.277113e+01   3.632169e-02 1.676264e+03
    ## 81/US-CO         1.721671e+02   9.214095e-02 1.012775e+04
    ## GFGD/US-CO       0.000000e+00   0.000000e+00 0.000000e+00
    ## GFGN/US-CO       0.000000e+00   0.000000e+00 0.000000e+00
    ## GFE/US-CO        4.204614e+01   1.570945e-03 2.056540e+02
    ## GSLG/US-CO       0.000000e+00   0.000000e+00 0.000000e+00
    ## GSLE/US-CO       8.569876e+02   3.083479e-05 2.530495e+02
    ## Used/US-CO       5.594438e+02   5.478619e-03 2.348229e+03
    ## Other/US-CO      0.000000e+00   0.000000e+00 0.000000e+00
    ## 111CA/RoUS       4.966973e+03   6.357129e-03 1.064502e+03
    ## 113FF/RoUS       5.863316e+02   3.057397e-02 2.942480e+03
    ## 211/RoUS         1.117211e+04   6.039945e-03 4.711086e+03
    ## 212/RoUS         5.888301e+03   1.524285e-02 4.878851e+03
    ## 213/RoUS         3.174171e+02   1.861885e-03 3.253893e+02
    ## 22/RoUS          3.039037e+04   2.096163e-02 8.879707e+03
    ## 23/RoUS          3.299742e+02   1.838647e-02 2.299112e+03
    ## 321/RoUS         1.489084e+03   8.337898e-02 8.952028e+03
    ## 327/RoUS         1.430921e+04   6.132466e-02 1.028948e+04
    ## 331/RoUS         1.279860e+04   4.104468e-02 6.742383e+03
    ## 332/RoUS         2.115605e+03   2.405432e-01 2.574237e+04
    ## 333/RoUS         3.229353e+02   3.906676e-02 5.904598e+03
    ## 334/RoUS         1.872361e+02   2.613151e-02 7.737725e+03
    ## 335/RoUS         6.608946e+02   5.347310e-02 8.989865e+03
    ## 3361MV/RoUS      1.084401e+02   1.010165e-02 1.750520e+03
    ## 3364OT/RoUS      1.376615e+01   1.794806e-03 3.156191e+02
    ## 337/RoUS         2.776077e+02   3.966075e-02 3.497137e+03
    ## 339/RoUS         1.946025e+01   3.607317e-03 6.156298e+02
    ## 311FT/RoUS       1.505079e+02   4.556783e-03 6.798366e+02
    ## 313TT/RoUS       1.909240e+02   1.106282e-02 9.088859e+02
    ## 315AL/RoUS       2.660146e+00   6.543543e-04 6.166385e+01
    ## 322/RoUS         2.420183e+03   1.785520e-02 3.001447e+03
    ## 323/RoUS         8.378829e+01   9.181093e-03 9.873997e+02
    ## 324/RoUS         1.152162e+04   8.522444e-03 8.435607e+03
    ## 325/RoUS         1.087273e+04   2.664847e-02 1.193364e+04
    ## 326/RoUS         8.551564e+02   5.196436e-02 6.151848e+03
    ## 42/RoUS          1.226334e+03   3.393886e-01 7.512516e+04
    ## 441/RoUS         6.348446e+00   4.605877e-03 5.596137e+02
    ## 445/RoUS         1.296415e+00   7.134832e-04 3.982129e+01
    ## 452/RoUS         1.022220e+00   2.247019e-03 1.201041e+02
    ## 4A0/RoUS         5.601405e+01   8.404307e-02 7.857029e+03
    ## 481/RoUS         1.750549e+03   7.202436e-03 9.031926e+02
    ## 482/RoUS         1.522275e+03   3.851732e-05 1.807230e+03
    ## 483/RoUS         7.278220e+02   2.470411e-03 3.730316e+02
    ## 484/RoUS         1.500111e+04   8.352186e-02 9.286405e+03
    ## 485/RoUS         2.099277e+02   8.245749e-03 5.766567e+02
    ## 486/RoUS         6.146696e+03   3.206197e-03 2.363776e+03
    ## 487OS/RoUS       1.174929e+03   6.428508e-02 5.534163e+03
    ## 493/RoUS         6.615116e+02   9.053613e-02 4.986183e+03
    ## 511/RoUS         2.562036e+00   3.298160e-03 1.357152e+03
    ## 512/RoUS         2.007379e+00   3.563097e-03 8.368217e+02
    ## 513/RoUS         7.722263e+01   1.379035e-02 6.848177e+03
    ## 514/RoUS         3.537902e+01   1.289090e-02 4.496251e+03
    ## 521CI/RoUS       1.719156e+02   7.751964e-02 2.249601e+04
    ## 523/RoUS         2.953318e+01   9.381889e-03 3.343348e+03
    ## 524/RoUS         1.289113e+01   4.050154e-02 1.070738e+04
    ## 525/RoUS         1.358289e+01   6.611265e-05 3.643286e+01
    ## HS/RoUS          0.000000e+00   0.000000e+00 0.000000e+00
    ## ORE/RoUS         5.861879e+02   2.386923e-02 9.406523e+03
    ## 532RL/RoUS       5.071046e+02   5.072703e-02 1.912941e+04
    ## 5411/RoUS        4.976047e+00   2.913154e-02 7.275555e+03
    ## 5415/RoUS        9.326748e+01   3.201291e-02 5.175161e+03
    ## 5412OP/RoUS      3.164060e+03   5.211508e-01 8.406747e+04
    ## 55/RoUS          6.481340e+01   8.991751e-02 1.588037e+04
    ## 561/RoUS         8.145808e+02   3.254748e-01 2.393798e+04
    ## 562/RoUS         8.322570e+03   2.843102e-02 3.951625e+03
    ## 61/RoUS          4.724916e+01   2.852967e-02 7.929376e+02
    ## 621/RoUS         4.422952e-01   3.530464e-04 3.491578e+01
    ## 622/RoUS         8.270980e-02   2.158632e-05 2.076704e+00
    ## 623/RoUS         4.483092e-02   7.890097e-05 3.608306e+00
    ## 624/RoUS         8.619533e-02   1.818583e-04 6.374183e+00
    ## 711AS/RoUS       1.141214e+01   9.760709e-03 1.500314e+03
    ## 713/RoUS         1.269557e+02   7.733596e-03 4.674542e+02
    ## 721/RoUS         2.683401e+01   1.843853e-02 1.244160e+03
    ## 722/RoUS         5.243697e+01   4.251263e-02 1.833734e+03
    ## 81/RoUS          9.518395e+01   4.405478e-02 4.899393e+03
    ## GFGD/RoUS        0.000000e+00   0.000000e+00 0.000000e+00
    ## GFGN/RoUS        0.000000e+00   0.000000e+00 0.000000e+00
    ## GFE/RoUS         3.800443e+02   1.786217e-02 1.782801e+03
    ## GSLG/RoUS        0.000000e+00   0.000000e+00 0.000000e+00
    ## GSLE/RoUS        6.169977e+02   3.517432e-04 3.167451e+02
    ## Used/RoUS        4.196063e+02   4.954811e-03 9.584619e+02
    ## Other/RoUS       0.000000e+00   0.000000e+00 0.000000e+00
