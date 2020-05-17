These files replicate the results in “Anticipation of an Interest Rate Shockand Monetary Policy in the Eurozone”
By Aymeric Lachaux, Jung Hyun Kim

May 17, 2020

*******************************************************************************************
Prerequisites
*******************************************************************************************
These files require a working installation of R and Matlab. 

*******************************************************************************************
Replication
*******************************************************************************************

The plots in the paper can be replicated by .R using the attached saved
files.
For a full replication:
	first)	 run VAR_KIM.R
	use IRF_function.R to call "ellen" function
	second)  run BVAR_KIM.R
	use IRF_function.R to call "ellen" function

********************************************************************************************
File Structure
********************************************************************************************
data_eurozone.R:

	The code merge each input to create the dataset. 
  Detailed procedures are contained inside this code file.

	input:
		- interest_rate.xlsx
		- inflation.xlsx
    - gdp_real.xlsx
    - gdp_potential.xlsx

	output:

		if do_write:
			- data_eurozone.csv

___________________________________________________________________________________________________

.m

	This script . It computes .


	input:
		- data_eurozone.csv

	output:
		- 

___________________________________________________________________________________________________

VAR_KIM.R
BVAR_KIM.R

	This script estimates VAR and BVAR.

	input:
		- data_eurozone.csv
		
	output:
		- Forecasted values (ex.P6Q, R2Q, Y4Q, ....)
		
