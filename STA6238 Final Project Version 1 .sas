/*Name: Xiaoling Sundberg 
Date: April 26, 2026*/

/*Read in dataset - 'homicide_california_2014.csv' 
this is a subset of the homicide.csv dataset that contains over 630,000 observations.
The original dataset includes all homicide cases across 50 states in the US from 1980 to 2014.
For this analysis, I chose the state of California in year 2014 because it is a big state and has great ethnic diversity. 
The binary target variable is 'Crime Solved' with outcome 'Yes' and 'No' 
The analysis dataset contians 1,872 observations.*/

*Read in Dataset; 
proc import datafile="/home/u64397595/STA 6238/homicide_california_2014.csv"
    out=homicide_california_2014
    dbms=csv
    replace;
    guessingrows=max;
run;
proc print data=homicide_california_2014 (obs=10);
run;

/****************************************
SECTION I: EXPLORATORY DATA ANALYSIS.
*****************************************/

*1.Select Predictors; 
data crime_analysis;
    set homicide_california_2014;

    Obs = _N_;

    keep Obs
         'Crime Solved'n
         'Victim Sex'n
         'Victim Race'n
         'Victim Age'n
         'Agency Type'n
         Relationship
         Weapon;
run;
proc print data=crime_analysis(obs=10);
run;


*2.Recode the categorical predictors 'Relationship' and 'Weapon';
*Relationship original levels - using SQL; 
proc sql;
    select distinct Relationship
    from crime_analysis
    order by Relationship;
quit;

*Recode Relationship;
data crime_analysis;
    set crime_analysis;

    length Relationship2 $25;

    if Relationship in ("Boyfriend", "Boyfriend/Girlfriend", "Girlfriend",
                        "Husband", "Wife", "Common-Law Husband", "Common-Law Wife",
                        "Ex-Wife") then Relationship2 = "Intimate Partner";

    else if Relationship in ("Brother", "Daughter", "Family", "Father",
                             "In-Law", "Mother", "Sister", "Son",
                             "Stepfather", "Stepson") then Relationship2 = "Family";

    else if Relationship in ("Friend", "Acquaintance", "Neighbor", "Employer") 
        then Relationship2 = "Friend";

    else if Relationship = "Stranger" then Relationship2 = "Stranger";
	else if Relationship = "Unknown" then Relationship2 = "Unknown";


    else Relationship2 = "Other";
run;

*Check recoded relationship levels; 
proc freq data=crime_analysis;
	tables Relationship2 / missing;
run;

*Compare original and recoded relationship variables;
proc freq data=crime_analysis;
    tables Relationship*Relationship2 / missing;
run;



*Weapon original levels - using SQL; 
proc sql;
    select distinct Weapon
    from crime_analysis
    order by Weapon;
quit;

*Recode Weapon;
data crime_analysis;
    set crime_analysis;

    length Weapon2 $20;

    if Weapon in ("Firearm", "Gun", "Handgun", "Rifle", "Shotgun") then Weapon2 = "Firearm";
    else if Weapon = "Knife" then Weapon2 = "Knife";
    else if Weapon in ("Unknown", "") then Weapon2 = "Unknown";
    else Weapon2 = "Other";
run;

*Check recoded weapon levels; 
proc sql;
    select distinct Weapon2
    from crime_analysis
    order by Weapon2;
quit;

*Compare original and recoded weapon variables;
proc freq data=crime_analysis;
    tables Weapon*Weapon2 / missing;
run;

*Check selected and recoded variables;
proc print data=crime_analysis(obs=20);
    var 'Crime Solved'n
        'Victim Age'n
        'Victim Sex'n
        'Victim Race'n
        'Agency Type'n
        Relationship2
        Weapon2;
run;





*3.Victim Age is a quantitative predictor variable. Check outliers; 
proc univariate data=crime_analysis nextrobs=10;
    var 'Victim Age'n;
run;

*Found age 998, which appears to be a placeholder. Recode it as missing;
data crime_analysis;
    set crime_analysis;

    if 'Victim Age'n = 998 then 'Victim Age'n = .;
run;

*Re-run after recoding age 998 as missing;
proc univariate data=crime_analysis nextrobs=10;
    var 'Victim Age'n;
run; 
*The new highest age value is now 96;

/* There is also age of zero as the lowest value.
Since age zero can represent an infant victim, keep age zero as valid.
There are 5 missing values after recoding 998 as missing. */
proc freq data=crime_analysis;
    tables 'Victim Age'n / missing;
run;

*Exploratory scatter plot of Victim Age vs. Crime Solved;
proc sgplot data=crime_analysis;
    yaxis label="Crime Solved";
    xaxis label="Victim Age" values=(0 to 100 by 10); 
    scatter x='Victim Age'n y='Crime Solved'n;
run;

/*4. Univariate and bivariate analysis:
Examine the relationship between each predictor and Crime Solved */

/* Categorical predictors */
proc freq data=crime_analysis;
    tables 'Crime Solved'n*'Victim Sex'n / chisq;
    tables 'Crime Solved'n*'Victim Race'n / chisq;
    tables 'Crime Solved'n*'Agency Type'n / chisq;
    tables 'Crime Solved'n*Relationship2 / chisq;
    tables 'Crime Solved'n*Weapon2 / chisq;
run;



/* Quantitative predictor: Victim Age */
proc logistic data=crime_analysis;
    model 'Crime Solved'n(event='Yes') = 'Victim Age'n;
run;
/*Note:	5 observations were deleted due to missing values for 
the response or explanatory variables*/



/********************************************************************
SECTION II: BUILD A FULL LOGISTIC REGRESSION MODEL
********************************************************************/

*Full candidate model with all selected predictors;
proc logistic data=crime_analysis;
    class 'Victim Sex'n
          'Victim Race'n
          'Agency Type'n
          Relationship2
          Weapon2 / param=ref;

    model 'Crime Solved'n(event='Yes') =
          'Victim Age'n
          'Victim Sex'n
          'Victim Race'n
          'Agency Type'n
          Relationship2
          Weapon2
          / link=logit clodds=wald;
run;
/*In the full model, Relationship2 was statistically significant; 
however, the model showed evidence of quasi-complete separation, 
with extremely large standard errors for some relationship categories. 
Because relationship status may depend on whether the perpetrator was 
identified, Relationship2 was excluded from the final model.*/

proc logistic data=crime_analysis plots(only)=roc;
    class 'Victim Sex'n
          'Victim Race'n
          'Agency Type'n
          Weapon2 / param=ref;

    model 'Crime Solved'n(event='Yes') =
          'Victim Age'n
          'Victim Sex'n
          'Victim Race'n
          'Agency Type'n
          Weapon2
          / link=logit clodds=wald ctable pprob=0.5;
run;
/* After excluding Relationship2, the model no longer showed the same
separation issue. Victim Sex, Victim Race, and Weapon2 were statistically
significant predictors. Agency Type was not significant and was removed
from the next model. Victim Age was not significant at the 0.05 level but
was retained temporarily because it was significant in the bivariate model
and remained close to significant in the multivariable model. */

*Create interaction term(s);
proc logistic data=crime_analysis;
    class 'Victim Sex'n
          'Victim Race'n
          Weapon2 / param=ref;

    model 'Crime Solved'n(event='Yes') =
    'Victim Age'n
    'Victim Sex'n
    'Victim Race'n
    Weapon2
    'Victim Age'n*'Victim Sex'n
    'Victim Age'n*'Victim Race'n 
    'Victim Race'n* 'Victim Sex'n
    'Victim Sex'n*Weapon2
    'Victim Race'n*Weapon2
    / link=logit clodds=wald ctable pprob=0.5;
run;
*Victim Race*Victim Sex and Victim Rae*Weapon2 are significant.

*Interaction model retaining significant two-way interactions;
proc logistic data=crime_analysis;
    class 'Victim Sex'n
          'Victim Race'n
          Weapon2 / param=ref;

    model 'Crime Solved'n(event='Yes') =
          'Victim Age'n
          'Victim Sex'n
          'Victim Race'n
          Weapon2
          'Victim Race'n*'Victim Sex'n
          'Victim Race'n*Weapon2
          / link=logit clodds=wald ctable pprob=0.5;
run;


/* The interaction terms were evaluated using joint Wald tests because
categorical interactions are represented by multiple parameter estimates.
The joint test for Victim Race by Weapon2 was significant, although many
individual interaction parameters were not significant. This can occur
when the overall pattern differs across categories, but no single
category-level comparison is estimated precisely. Some very large standard
errors suggest sparse cells for certain race-weapon combinations, so this
interaction should be interpreted cautiously. */

*Check the 3-way table;
proc freq data=crime_analysis;
    tables 'Victim Race'n*Weapon2*'Crime Solved'n / missing;
run;
/* Note on Victim Race by Weapon2 interaction:
The joint test for the Victim Race by Weapon2 interaction was statistically
significant, suggesting that the association between weapon type and case
resolution may differ by victim race. However, cross-tabulations showed
sparse cells for some race-weapon combinations, especially for Native
American/Alaska Native victims and Unknown victim race. These sparse cells
produced very large standard errors for some interaction parameters.
Therefore, this interaction was interpreted cautiously and was not retained
in the final model.
*/

**************** Final model *********************; 
*Final model with goodness-of-fit and classification table;
proc logistic data=crime_analysis plots(only)=roc;
    class 'Victim Sex'n(ref='Male')
          'Victim Race'n(ref='White')
          Weapon2(ref='Firearm') / param=ref;

    model 'Crime Solved'n(event='Yes') =
          'Victim Age'n
          'Victim Sex'n
          'Victim Race'n
          Weapon2
          / link=logit clodds=wald ctable lackfit pprob=0.5;
run;
/* Victim Age was retained in the final model because it was statistically
significant in the bivariate logistic regression model and is a meaningful
demographic predictor. Although it was not significant at the 0.05 level in
the adjusted final model (p = 0.0709), it remained close to significance
and was kept to adjust for potential age-related differences in case
resolution. */


*Final model diagnostics;
proc logistic data=crime_analysis plots(only label)=(phat leverage dpc);
    class 'Victim Sex'n(ref='Male')
          'Victim Race'n(ref='White')
          Weapon2(ref='Firearm') / param=ref;

    model 'Crime Solved'n(event='Yes') =
          'Victim Age'n
          'Victim Sex'n
          'Victim Race'n
          Weapon2;
run;