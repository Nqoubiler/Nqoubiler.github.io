libname clib1 base "/home/u62196846/STAT305/PROJECT";
proc import datafile="/home/u62196846/STAT305/PROJECT/Heart Data.xlsx"
	replace dbms=xlsx out=clib1.our_data;
	
/*THE ORIGINAL DATA*/
proc contents data=clib1.our_data;
run;

 	
data clib1._data; set clib1.our_data;
	if smoking = 'Yes' then smoking = 'Exposed';
	else if  smoking ='No' then smoking = 'Unexposed';
	if CVD = 'Yes' then CVD = 'Diseased';
	else if CVD ='No' then CVD = 'Healthy';
	if high_blood_pressure = 'Yes' then high_blood_pressure = 'Exposed';
	else if high_blood_pressure ='No' then high_blood_pressure = 'Unexposed';
	if anaemia = 'Yes' then anaemia = 'Exposed';
	else if anaemia ='No' then anaemia = 'Unexposed';
	if diabetes = 'Yes' then diabetes = 'Exposed';
	else if diabetes ='No' then diabetes = 'Unexposed';
run;

/*PRINTING OUR NEW DATA*/
proc print data=clib1._data;
run;

 /*Creating one-way frequency tables for categorical variables */
PROC FREQ DATA=clib1._data;
    TABLES CVD anaemia high_blood_pressure diabetes gender smoking;
RUN;

/* Generating basic summary statistics for quantitative variables */
 PROC UNIVARIATE DATA=clib1._data;
    var age creatinine_phosphokinase ejection_fraction platelets serum_sodium;
RUN;

/*********************************************************************************************************************************/

/* Chi-Square test of association for Anaemia and CVD */
PROC FREQ DATA=clib1._data;
    TABLES anaemia*CVD / CHISQ;
    
RUN;

/* Chi-Square test of association for High Blood Pressure and CVD */
PROC FREQ DATA=clib1._data;
    TABLES high_blood_pressure*CVD / CHISQ;
   
RUN;

/* Chi-Square test of association for Diabetes and CVD */
PROC FREQ DATA=clib1._data;
    TABLES diabetes*CVD / CHISQ;
    
RUN;

/* Chi-Square test of association for Gender and CVD */
PROC FREQ DATA=clib1._data;
    TABLES gender*CVD / CHISQ;
   
RUN;

/* Chi-Square test of association for Smoking and CVD */
PROC FREQ DATA=clib1._data;
    TABLES smoking*CVD / CHISQ;
    
RUN;

/*********************************************************************************************************************************/

/* Logistic regression model with both categorical and quantitative explanatory variables */
PROC LOGISTIC DATA=clib1._data DESCENDING plots=all;
    CLASS smoking (REF='Une')
          anaemia (REF='Une')
          high_blood_pressure (REF='Une')
          diabetes (REF='Une')
          gender (REF='Female') / PARAM=REF;
    
     MODEL CVD(event='Dis') = smoking anaemia high_blood_pressure diabetes gender
                                  age creatinine_phosphokinase ejection_fraction platelets serum_sodium /
 	 scale=none clparm=wald clodds=pl rsquare lackfit expb stb aggregate=(smoking anaemia high_blood_pressure diabetes gender
                                  age creatinine_phosphokinase ejection_fraction platelets serum_sodium) ;
RUN;

/*********************************************************************************************************************************/

/* First fitted model */
ODS GRAPHICS OFF;
Title "First fitted model";
proc logistic data=clib1._DATA plots=roc;  
	 class  Diabetes high_blood_pressure smoking  / param=ref;
     model CVD(event='Dis') = high_blood_pressure smoking
 	 							age ejection_fraction 
 	 						creatinine_phosphokinase Diabetes
 	 						serum_sodium /
 	 scale=none clparm=wald clodds=pl rsquare lackfit expb stb aggregate=(smoking anaemia high_blood_pressure diabetes gender
                                  age creatinine_phosphokinase ejection_fraction platelets serum_sodium);
run;

/*************************************************************************************************************************/
 
/* Second fitted model */
Title "Second fitted model";
proc logistic data=clib1._DATA plots=roc;  
	 class high_blood_pressure smoking / param=ref;
     model CVD(event='Dis') = high_blood_pressure smoking
 	 							age ejection_fraction 
 	 						creatinine_phosphokinase
 	 						serum_sodium /
 	 scale=none clparm=wald clodds=pl rsquare lackfit expb stb aggregate=(smoking anaemia high_blood_pressure diabetes gender
                                  age creatinine_phosphokinase ejection_fraction platelets serum_sodium);
run;

/*****************************************************************************************************************************/

/* Third fitted model */
Title "Third fitted model";
proc logistic data=clib1._DATA plots=roc;  
	 class high_blood_pressure / param=ref;
     model CVD(event='Dis') = high_blood_pressure
 	 							age ejection_fraction 
 	 						serum_sodium /
 	 scale=none clparm=wald clodds=pl rsquare lackfit expb stb aggregate=(smoking anaemia high_blood_pressure diabetes gender
                                  age creatinine_phosphokinase ejection_fraction platelets serum_sodium);
run;

/***************************************************************************************************************************/

/* Fourth fitted model */
Title "Fourth fitted model";
proc logistic data=clib1._DATA plots=roc;   
     model CVD(event='Dis') =  age ejection_fraction 
 	 						serum_sodium /
 	 scale=none clparm=wald clodds=pl rsquare lackfit expb stb aggregate =(smoking anaemia high_blood_pressure diabetes gender
                                  age creatinine_phosphokinase ejection_fraction platelets serum_sodium);
run;
 	  	 
/* Performing goodness-of-fit test without non-significant predictors and checking for overdispersion */

ODS GRAPHICS ON;
TITLE"Goodness-of-fit test with significant predictors";
proc logistic data=clib1._DATA plots=all ;
  model CVD(event='Dis') = serum_sodium age ejection_fraction / 
  scale=none clparm=wald clodds=pl rsquare lackfit expb stb aggregate;
  
  /* Produce deviance and Pearson Chi-Square statistics, checks for influential observations and Checks for influential observations */
    output out=fit_stats
           dfbetas=dfb_serum_sodium dfb_age dfb_ejection_fraction
           p=predicted
           reschi=residuals 
           resdev=deviance; 
run;

/* Calculate the threshold for DFBETAS and flag influential observations */
data influential;
    set fit_stats;
    n = 299; /* Total number of observations */
    threshold = 2 / sqrt(n); /* Calculate the threshold for influential observations */
    
    /* Flag observations with DFBETAS exceeding the threshold */
    if abs (dfb_serum_sodium) > threshold or
       abs(dfb_age) > threshold or
       abs(dfb_ejection_fraction) > threshold then do;
        influential_flag = 1; /* Mark as influential */
        output; /* Output influential observations */
    end;
run;

/* Displaying influential observations */
proc print data=influential;
    title "Influential Observations Based on DFBETAS Exceeding Threshold";
    var _all_; 
run;

/*Displaying the predicted probabilities for the first three observations */
proc print data=fit_stats(obs=3); 
    var predicted; 
    title "Predicted Probabilities for the First 3 Observations";
run;

/*********************************************************************************************************************************/
/* PLOTS SHOWING OUTLIERS*/

/*  Scatter Plot of Residuals vs. Predicted Probabilities */
proc sgplot data=fit_stats;
    scatter x=predicted y=residuals / markerattrs=(symbol=circlefilled color=blue);
    refline 0 / axis=y lineattrs=(pattern=shortdash color=red);
    xaxis label='Predicted Probabilities';
    yaxis label='Residuals';
    title 'Residuals vs. Predicted Probabilities';
run;

/*   Histogram of Residuals */
proc sgplot data=fit_stats;
    histogram residuals / scale=count;
    xaxis label='Residuals';
    title 'Histogram of Residuals';
run;

/*  QQ Plot of Residuals */
proc univariate data=fit_stats normal;
    var residuals;
    qqplot / normal;
    title 'QQ Plot of Residuals';
run;











