********************;
*				    ;
* Tables for Paper  ;
*                   ;
********************;

************;
*DATA IMPORT;
************;
libname omi "\\cdc.gov\project\CCID_NCPDCID_NHSN_SAS\Data\work\_Projects\LTC\COVID-19\Codes\Jason\KML_Shape_Epi_Curve_Omicron";
run;

proc import out= model_data
    datafile="\\cdc.gov\project\CCID_NCPDCID_NHSN_SAS\Data\work\_Projects\LTC\COVID-19\Codes\Jason\KML_Shape_Epi_Curve_Omicron\model_data.csv"
    dbms=csv
    replace;
    getnames=YES;
run;

proc import out= omicron_clusters
    datafile="\\cdc.gov\project\CCID_NCPDCID_NHSN_SAS\Data\work\_Projects\LTC\COVID-19\Codes\Jason\KML_Shape_Epi_Curve_Omicron\omicron_clusters.csv"
    dbms=csv
    replace;
    getnames=YES;
run;


***************************************;
* Joining Cluster and Covariate Data   ;
***************************************;

data model_data;
set model_data;
drop cluster;
run;

proc sql;
	create table model_data as
	select * from model_data as x left join omicron_clusters as y
	on x.orgid = y.orgid;
quit;




*************************;
* Continuous Variables   ;
*************************;

*Total Clusters;
	     proc freq data = model_data ;
         tables cluster ;
		 run;


*Booster Rate;
		 proc means data = model_data;
		 var avg_booster_cov;
		 run;

		 proc means data = model_data;
		 var avg_booster_cov;
		 where cluster = "1";
		 run;

		 proc means data = model_data;
		 var avg_booster_cov;
		 where cluster = "2";
		 run;


*SVI;
		 proc means data = model_data;
		 var SVI;
		 run;

		 proc means data = model_data;
		 var SVI;
		 where cluster = "1";
		 run;

		 proc means data = model_data;
		 var SVI;
		 where cluster = "2";
		 run;




*************************;
* Categorical Variables  ;
*************************;

%macro table1(var);

	*Number of Facilities; 

		*Total;
	     proc freq data = model_data ;
         tables &var. ;
		 run;

		*By Cluster;
		 proc freq data = model_data ;
         tables &var.*cluster ;
		 run;


	*Number of Covid Cases;

		*Total;
		proc sql;
		create table covid&var. as 
select orgid, cluster, sum_c19, avg_numres, &var.,
sum(sum_c19) as covid&var. 
    	from model_data
    	group by &var.;
		quit;

		*Cluster 1;
		proc sql;
		create table covid_c1&var. as 
select orgid, cluster, sum_c19, avg_numres, &var.,
sum(sum_c19) as covid&var. 
    	from model_data 
        where cluster = "1"
    	group by &var.;
		quit;

		*Cluster 2;
		proc sql;
		create table covid_c2&var. as 
select orgid, cluster, sum_c19, avg_numres, &var.,
sum(sum_c19) as covid&var. 
    	from model_data 
        where cluster = "2"
    	group by &var.;
		quit;


    *Number of Residents;

		*Total;
		proc sql;
		create table res&var. as 
select orgid, cluster, sum_c19, avg_numres, &var.,
sum(avg_numres) as res&var. 
    	from model_data
    	group by &var.;
		quit;

		*Cluster 1;
		proc sql;
		create table res_c1&var. as 
select orgid, cluster, sum_c19, avg_numres, &var.,
sum(avg_numres) as res&var. 
    	from model_data 
        where cluster = "1"
    	group by &var.;
		quit;

		*Cluster 2;
		proc sql;
		create table res_c2&var. as 
select orgid, cluster, sum_c19, avg_numres, &var.,
sum(avg_numres) as res&var. 
    	from model_data 
        where cluster = "2"
    	group by &var.;
		quit;

%mend;


*Covariates for Macro;
%table1(region); run; *Region;
%table1(U_or_R); run; *Urbanicity;
%table1(beds_tertiles); run; *Number of beds (tertiles);
%table1(ltcCert); run; *LTC Healthcare Certification;
%table1(facowner); run; *Facility Private vs. Non Private;







*Total facilities combining all covariates;

	*Number of Facilities; 

		*By Cluster (sum for total);
		 proc freq data = model_data ;
         tables cluster ;
		 run;


	*Number of Covid Cases;

		*Cluster 1;
		proc sql;
		create table covid_t_c1 as 
select orgid, cluster, sum_c19, avg_numres,
sum(sum_c19) as covid_t_c1 
       from model_data 
       where cluster = "1" ;
		quit;

		*Cluster 2;
	proc sql;
		create table covid_t_c2 as 
select orgid, cluster, sum_c19, avg_numres,
sum(sum_c19) as covid_t_c2 
       from model_data 
       where cluster = "2" ;
		quit;


    *Number of Residents;

		*Cluster 1;
		proc sql;
		create table res_t_c1 as 
select orgid, cluster, sum_c19, avg_numres,
sum(avg_numres) as res_t_c1 
       from model_data 
       where cluster = "1" ;
		quit;

		*Cluster 2;
	proc sql;
		create table res_t_c2 as 
select orgid, cluster, sum_c19, avg_numres,
sum(avg_numres) as res_t_c2 
       from model_data 
       where cluster = "2" ;
		quit;

