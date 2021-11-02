# annex80-heat-wave-epw
Generating TMY and typical heat wave years from bias-corrected CORDEX weather simulation data, and preparing EPW files for BEM.

1. `1-raw-to-csv` reads the 20 period CORDEX weather simulation data in `netCDF4` format, and generates 20 year weather data required for BEM in CSV format, saved in the `results` folder.

2. `2-bias-correction` folder contains the R code to generate bias-corrected weather data using 20 years of observed historical data.

3. `3-tmy-and-heat-waves` takes inputs from step 2 and generate TMY weather files and heat wave periods in CSV format.

4. `4-Los_Angeles_TMY_2010s Solar  irradiance` takes input from the global solar irradiance on the horizontal plane from the CORDEX simulation data and separate to beam solar, diffuse horizontal and direct normal solar irradiance. Solar data should be appended to the previously generated CSV files.

5. `5-write-epw` converts the prepared CSV to EPW format.

Note: code and methods references and documentaton can be found at `Dropbox/Creating Future Weather Files/Documentation/workshop`.
