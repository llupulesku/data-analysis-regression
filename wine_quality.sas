PROC IMPORT datafile = "wine_quality.txt" out = wine_quality replace;
delimiter = '09'x;
getnames = yes;
datarow = 2;
RUN;

TITLE "Wine quality Dataset";
PROC PRINT;
RUN;

TITLE "5-Point Summary";
PROC MEANS min max p25 p50 p75;
VAR quality fixed_acidity volatile_acidity citric_acid residual_sugar chlorides free_sulfur_dioxide total_sulfur_dioxide density pH sulphates alcohol;
RUN;

TITLE "Histogram";
PROC UNIVARIATE normal;
VAR quality;
histogram / normal (mu = est sigma = est);
RUN;

PROC SGSCATTER;
TITLE"Scatterplot matrix between quality and other variables";
MATRIX quality fixed_acidity volatile_acidity citric_acid residual_sugar chlorides free_sulfur_dioxide total_sulfur_dioxide density pH sulphates alcohol; 
RUN;

PROC CORR;
TITLE"Correlation values of Quality vs. other variables";
VAR quality fixed_acidity volatile_acidity citric_acid residual_sugar chlorides free_sulfur_dioxide total_sulfur_dioxide density pH sulphates alcohol;
RUN;

PROC REG;
MODEL quality = fixed_acidity volatile_acidity citric_acid residual_sugar chlorides free_sulfur_dioxide total_sulfur_dioxide density pH sulphates alcohol/ VIF  INFLUENCE R;
RUN;

data winenew;
set wine_quality;
If _n_ in (46,391,441,460,653,814,833,900,1236, 1277,1479,1506) then delete;
run;

Proc reg data = winenew;
MODEL quality = fixed_acidity volatile_acidity citric_acid residual_sugar chlorides free_sulfur_dioxide total_sulfur_dioxide density pH sulphates alcohol/ VIF  INFLUENCE R;
RUN;

data winenew1;
Set winenew;
If _n_ in (635,686,870,1231,1460) then delete;
run;

Proc reg data = winenew1;
MODEL quality = fixed_acidity volatile_acidity citric_acid residual_sugar chlorides free_sulfur_dioxide total_sulfur_dioxide density pH sulphates alcohol/ VIF  INFLUENCE R;
RUN;

data winenew2;
Set winenew1;
If _n_ in (492,495,514,1114) then delete;
run;

Proc reg data = winenew2;
MODEL quality = fixed_acidity volatile_acidity citric_acid residual_sugar chlorides free_sulfur_dioxide total_sulfur_dioxide density pH sulphates alcohol/ VIF  INFLUENCE R;
RUN;

data winenew3;
set winenew2;
if _n_ in (1357,1386) then delete;
run;

Proc reg data = winenew3;
MODEL quality = fixed_acidity volatile_acidity citric_acid residual_sugar chlorides free_sulfur_dioxide total_sulfur_dioxide density pH sulphates alcohol/ VIF  INFLUENCE R;
RUN;

proc reg data = winenew3;
model quality = fixed_acidity volatile_acidity citric_acid residual_sugar chlorides free_sulfur_dioxide total_sulfur_dioxide density pH sulphates alcohol/vif stb;
PLOT STUDENT.*(fixed_acidity volatile_acidity citric_acid residual_sugar chlorides free_sulfur_dioxide total_sulfur_dioxide density pH sulphates alcohol);
PLOT STUDENT.*PREDICTED.;
PLOT NPP.*STUDENT.;
RUN;

Title "Test and Train Sets For Quality";
proc surveyselect data=winenew3 out=xv_all seed=45687
samprate=0.75 outall;
run;

PROC PRINT;
RUN;

data xv_all;
set xv_all;
if selected then new_y=quality; 
run;

proc print data=xv_all;
run;

TITLE "Model selection backward";
proc reg data = xv_all;
model new_y = fixed_acidity volatile_acidity citric_acid residual_sugar chlorides free_sulfur_dioxide total_sulfur_dioxide density pH sulphates alcohol/ selection = backward sle=0.05 sls=0.05;
run;

TITLE "Model selection cp";
proc reg data = xv_all;
model new_y = fixed_acidity volatile_acidity citric_acid residual_sugar chlorides free_sulfur_dioxide total_sulfur_dioxide density pH sulphates alcohol/ selection = cp sle=0.05 sls=0.05;
run;

TITLE "Model selection stepwise";
proc reg data = xv_all;
model new_y = fixed_acidity volatile_acidity citric_acid residual_sugar chlorides free_sulfur_dioxide total_sulfur_dioxide density pH sulphates alcohol/ selection = stepwise sle=0.05 sls=0.05;
run;

* Fitted Model - Goodness of Fit with standardized coefficentts ;
* stb - standardized parameter estimates;
title "Final Model";
proc reg;
model quality = alcohol volatile_acidity sulphates total_sulfur_dioxide chlorides pH/stb;
run;

*Checking model assumptions - Resiiduals and Probability Plots;
title "Residual and Probability Plots for Quality";
proc reg;
model quality = alcohol volatile_acidity sulphates total_sulfur_dioxide chlorides pH;
* Residual Plots;
plot student.*predicted.;
plot npp.*student.;
plot student.*(alcohol volatile_acidity sulphates total_sulfur_dioxide chlorides pH);
run;

* Influential Points;
title "Influential Points for Quality";
proc reg;
model quality = alcohol volatile_acidity sulphates total_sulfur_dioxide chlorides pH / influence;
run;

*Trying predictions;
title "Predicting Quality";
data pred1;
input volatile_acidity chlorides total_sulfur_dioxide sulphates alcohol;
datalines;
0.7 0.089 34.0 0.55 9.9
0.53 0.014 65.0 0.75 9.1
;
run;
data newp;
set pred1 winenew3;
proc reg data = newp;
model Quality =  volatile_acidity chlorides total_sulfur_dioxide sulphates alcohol/clm cli;
run;

*Computing a prediction based on my model. Choosing values for the x-variables to predict quality;
*creating new dataset for prediction;
title "Predicting Quality";
data pred;
input alcohol volatile_acidity sulphates total_sulfur_dioxide chlorides pH;
datalines;
9.9	0.7	0.55 34.0 0.089 3.11
9.1	0.53 0.75 65.0 0.014 3.37
;
run;
data new;
set pred winenew3;
proc reg data = new;
model Quality = alcohol volatile_acidity sulphates total_sulfur_dioxide chlorides pH/clm cli;
run;

TITLE "Validation- Test Set";
proc reg data = xv_all;
*MODEL1;
model new_y = volatile_acidity chlorides total_sulfur_dioxide sulphates alcohol;
output out=outm1(where=(new_y=.)) p=yhat;

*MODEL2;
model new_y = alcohol volatile_acidity sulphates total_sulfur_dioxide chlorides pH;
output out=outm2(where=(new_y=.)) p=yhat;
run;

proc print data=outm1;
run;
proc print data=outm2;
run;


/* summarizing the results of the cross-validations for model-1*/
title "Difference between Observed and Predicted in Test Set";
data outm1_sum;
set outm1;
d=quality-yhat;
absd=abs(d);
run;
/* computing predictive statistics)*/
proc summary data=outm1_sum;
var d absd;
output out=outm1_stats std(d)=rmse mean(absd)=mae ;
run;
proc print data=outm1_stats;
title 'Validation  statistics for Model';
run;
*computing correlation of observed and predicted values in test set;
proc corr data=outm1;
var quality yhat;
run;

/* summarizing the results of the cross-validations for model-2*/
title "Difference between Observed and Predicted in Test Set";
data outm2_sum;
set outm2;
d=quality-yhat; 
absd=abs(d);
run;
/* computing predictive statistics*/
proc summary data=outm2_sum;
var d absd;
output out=outm2_stats std(d)=rmse mean(absd)=mae ;
run;
proc print data=outm2_stats;
title 'Validation  statistics for Model';
run;
*computing correlation of observed and predicted values in test set;
proc corr data=outm2;
var quality yhat;
run;

