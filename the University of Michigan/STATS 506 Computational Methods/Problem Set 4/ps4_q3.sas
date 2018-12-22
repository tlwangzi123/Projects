
/* Analysis of the 2016 Medicare Provider Utilization and Payment data
   The 2016 Medicare Provider Utilization and Payment data used in this script 
   can be found at the link below:
   https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Physician-and-Other-Supplier2016.html
   Author: Zi Wang (tlwangzi@umich.edu)
   Updated: Dec 10, 2018
*/

/* a */
/* Load the data into SAS */
DATA Medicare_PS_PUF;
	LENGTH
		npi              					$ 10
		nppes_provider_last_org_name 		$ 70
		nppes_provider_first_name 			$ 20
		nppes_provider_mi					$ 1
		nppes_credentials 					$ 20
		nppes_provider_gender				$ 1
		nppes_entity_code 					$ 1
		nppes_provider_street1 				$ 55
		nppes_provider_street2				$ 55
		nppes_provider_city 				$ 40
		nppes_provider_zip 					$ 20
		nppes_provider_state				$ 2
		nppes_provider_country				$ 2
		provider_type 						$ 55
		medicare_participation_indicator 	$ 1
		place_of_service					$ 1
		hcpcs_code       					$ 5
		hcpcs_description 					$ 256
		hcpcs_drug_indicator				$ 1
		line_srvc_cnt      					8
		bene_unique_cnt    					8
		bene_day_srvc_cnt   				8
		average_Medicare_allowed_amt   		8
		average_submitted_chrg_amt  		8
		average_Medicare_payment_amt   		8
		average_Medicare_standard_amt		8;
	INFILE './data/Medicare_Provider_Util_Payment_PUF_CY2016.txt'

		lrecl=32767
		dlm='09'x
		pad missover
		firstobs = 3
		dsd;

	INPUT
		npi             
		nppes_provider_last_org_name 
		nppes_provider_first_name 
		nppes_provider_mi 
		nppes_credentials 
		nppes_provider_gender 
		nppes_entity_code 
		nppes_provider_street1 
		nppes_provider_street2 
		nppes_provider_city 
		nppes_provider_zip 
		nppes_provider_state 
		nppes_provider_country 
		provider_type 
		medicare_participation_indicator 
		place_of_service 
		hcpcs_code       
		hcpcs_description 
		hcpcs_drug_indicator
		line_srvc_cnt    
		bene_unique_cnt  
		bene_day_srvc_cnt 
		average_Medicare_allowed_amt 
		average_submitted_chrg_amt 
		average_Medicare_payment_amt
		average_Medicare_standard_amt;

	LABEL
		npi     							= "National Provider Identifier"       
		nppes_provider_last_org_name 		= "Last Name/Organization Name of the Provider"
		nppes_provider_first_name 			= "First Name of the Provider"
		nppes_provider_mi					= "Middle Initial of the Provider"
		nppes_credentials 					= "Credentials of the Provider"
		nppes_provider_gender 				= "Gender of the Provider"
		nppes_entity_code 					= "Entity Type of the Provider"
		nppes_provider_street1 				= "Street Address 1 of the Provider"
		nppes_provider_street2 				= "Street Address 2 of the Provider"
		nppes_provider_city 				= "City of the Provider"
		nppes_provider_zip 					= "Zip Code of the Provider"
		nppes_provider_state 				= "State Code of the Provider"
		nppes_provider_country 				= "Country Code of the Provider"
		provider_type	 					= "Provider Type of the Provider"
		medicare_participation_indicator 	= "Medicare Participation Indicator"
		place_of_service 					= "Place of Service"
		hcpcs_code       					= "HCPCS Code"
		hcpcs_description 					= "HCPCS Description"
		hcpcs_drug_indicator				= "Identifies HCPCS As Drug Included in the ASP Drug List"
		line_srvc_cnt    					= "Number of Services"
		bene_unique_cnt  					= "Number of Medicare Beneficiaries"
		bene_day_srvc_cnt 					= "Number of Distinct Medicare Beneficiary/Per Day Services"
		average_Medicare_allowed_amt 		= "Average Medicare Allowed Amount"
		average_submitted_chrg_amt 			= "Average Submitted Charge Amount"
		average_Medicare_payment_amt 		= "Average Medicare Payment Amount"
		average_Medicare_standard_amt		= "Average Medicare Standardized Payment Amount";
RUN;

/* b. */
/* Keep variables that are related to the following problems*/
data Medicare_PS_PUF1;
set Medicare_PS_PUF(keep = hcpcs_description hcpcs_code line_srvc_cnt 
                           average_Medicare_payment_amt); 
run;

/* Reduce the data set to those rows with “MRI” in the ‘hcpcs_description’ field 
   and where ‘hcpcs_code’ starts with a 7 */
data Medicare_PS_PUF_b;
set Medicare_PS_PUF1;
where hcpcs_description like '%MRI%' and hcpcs_code like'7%';
run;

/* c. */
/* Calculate volume, total payment and average payment
   within each MRI procedure*/ 
proc sort data = Medicare_PS_PUF_b;
by hcpcs_description;
run;

data Medicare_PS_PUF_b;
set Medicare_PS_PUF_b;
  ind_payment = average_Medicare_payment_amt * line_srvc_cnt;  
run;

proc summary data = Medicare_PS_PUF_b;
by hcpcs_description;
output out = MRI_procedures
  sum(line_srvc_cnt) = volume
  sum(ind_payment) = total_payment;
run;

data MRI_procedures;
set MRI_procedures;
  average_payment = total_payment/volume; 
  drop _TYPE_ _FREQ_;
run;

/* Determine the MRI procedure with the highest volume */
proc sort data = MRI_procedures;
  by DESCENDING volume;
run;

data MRI_procedures1;
set MRI_procedures;
  length Item $ 60; 
  Item = "The highest volume";
  MRI_procedure = hcpcs_description;
  Value = volume;
  if _n_ = 1;
  keep Item MRI_procedure Value;
run;

/* Determine the MRI procedure with the highest total payment */
proc sort data = MRI_procedures;
  by DESCENDING total_payment;
run;

data MRI_procedures2;
set MRI_procedures;
  length Item $ 60; 
  Item = "The highest total payment";
  MRI_procedure = hcpcs_description;
  Value = total_payment;
  if _n_ = 1;
  keep Item MRI_procedure Value;
run;

/* Determine the MRI procedure with the highest average payment */
proc sort data = MRI_procedures;
  by DESCENDING average_payment;
run;

data MRI_procedures3;
set MRI_procedures;
  length Item $ 60; 
  Item = "The highest average payment";
  MRI_procedure = hcpcs_description;
  Value = average_payment;
  if _n_ = 1;
  keep Item MRI_procedure Value;
run;

/* Merge these three data */
data MRI_procedures_c;
set MRI_procedures1 MRI_procedures2 MRI_procedures3;
run;

proc print data = MRI_procedures_c;
run;

/* d. */
proc sql;
  /* Calculate volume, total payment and average payment
     within each MRI procedure*/ 
  create table Medicare_PS_PUF_d as
    select hcpcs_description, sum(line_srvc_cnt) as volume, 
           sum(average_Medicare_payment_amt * line_srvc_cnt) as total_payment
	from Medicare_PS_PUF
	where hcpcs_description like '%MRI%' and hcpcs_code like'7%'
    group by hcpcs_description;
  create table Medicare_PS_PUF_d1 as
    select hcpcs_description, volume, total_payment,
	       total_payment/volume as average_payment
	from Medicare_PS_PUF_d;

  /* Determine the MRI procedures with the highest volume */
  create table MRI_procedures_d1 as
    select hcpcs_description as MRI_procedure, max(volume) as Value
	from Medicare_PS_PUF_d1
	having volume = max(volume);
  alter table MRI_procedures_d1
    add Item char(60);
  update MRI_procedures_d1
    set Item = 'The highest volume';
 
  /* Determine the MRI procedures with the highest total payment */
  create table MRI_procedures_d2 as
    select hcpcs_description as MRI_procedure, max(total_payment) as Value
	from Medicare_PS_PUF_d1
	having total_payment = max(total_payment);
  alter table MRI_procedures_d2
    add Item char(60);
  update MRI_procedures_d2
    set Item = 'The highest total payment';

  /* Determine the MRI procedures with the highest average payment */
  create table MRI_procedures_d3 as
    select hcpcs_description as MRI_procedure, max(average_payment) as Value
	from Medicare_PS_PUF_d1
	having average_payment = max(average_payment);
  alter table MRI_procedures_d3
    add Item char(60);
  update MRI_procedures_d3
    set Item = 'The highest average payment';

  /* Join these three tables */
  create table MRI_procedures_d as
    select Item, MRI_procedure, Value
	from(
         select * 
	     from MRI_procedures_d1
	     union
	         (
	          select *
	     	  from MRI_procedures_d2
		      union
              select *
	          from MRI_procedures_d3
	         )
	);
quit;

proc print data = MRI_procedures_d;
run;

/* e. */
/* Export the results from (c) to csv */
data  MRI_procedures_c;
set MRI_procedures_c;
Value = round(Value, .01);
run;

proc export data = MRI_procedures_c outfile = 'ps4_q3c.csv'
  dbms = dlm replace;
  delimiter = ",";
run;

/* Export the results from (d) to csv */
data  MRI_procedures_d;
set MRI_procedures_d;
Value = round(Value, .01);
run;

proc export data = MRI_procedures_d outfile = 'ps4_q3d.csv'
  dbms = dlm replace;
  delimiter = ",";
run;

/* Verify that the results from (c) and (d) match */
proc compare base = MRI_procedures_c
              compare = MRI_procedures_d;
run;

