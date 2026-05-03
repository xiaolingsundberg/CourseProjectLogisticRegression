/* Import homicide data */
proc import datafile="/home/u64397595/STA 6238/homicide_california_2014.csv"
    out=homicide
    dbms=csv
    replace;
    guessingrows=max;
run;


/* Check variable structure */
proc contents data=homicide;
run;


/* Frequency checks for categorical variables */
proc freq data=homicide;
    tables 
        'Crime Solved'n
        'Agency Type'n
        'Victim Sex'n
        'Victim Race'n
        'Victim Ethnicity'n
        'Perpetrator Sex'n
        'Perpetrator Race'n
        Relationship
        Weapon
        / missing;
run;


/* Check numeric variables */
proc means data=homicide n mean std min q1 median q3 max;
    var 'Victim Age'n 'Perpetrator Age'n 'Victim Count'n 'Perpetrator Count'n;
run;


/* Clean and recode variables */
data homicide_clean;
    set homicide;

    /* Outcome: 1 = solved, 0 = not solved */
    if 'Crime Solved'n = "Yes" then Solved = 1;
    else if 'Crime Solved'n = "No" then Solved = 0;

    /* Clean victim age */
    if 'Victim Age'n = 998 then Victim_Age_Clean = .;
    else Victim_Age_Clean = 'Victim Age'n;

    /* Recode Agency Type */
    length Agency_Type2 $20;
    if 'Agency Type'n = "Municipal Police" then Agency_Type2 = "Municipal Police";
    else Agency_Type2 = "Non-Municipal";

    /* Recode Victim Race */
    length Victim_Race2 $30;
    if 'Victim Race'n in ("White", "Black", "Asian/Pacific Islander") then 
        Victim_Race2 = 'Victim Race'n;
    else Victim_Race2 = "Other/Unknown";

    /* Recode Weapon */
    length Weapon2 $20;
    if Weapon in ("Handgun", "Firearm", "Rifle", "Shotgun", "Gun") then Weapon2 = "Firearm";
    else if Weapon = "Knife" then Weapon2 = "Knife";
    else if Weapon = "Blunt Object" then Weapon2 = "Blunt Object";
    else if Weapon = "Unknown" then Weapon2 = "Unknown";
    else Weapon2 = "Other";
run;


/* Check recoded variables */
proc freq data=homicide_clean;
    tables Solved Agency_Type2 Victim_Race2 Weapon2 / missing;
run;


/* Bivariate analysis: categorical variables */
proc freq data=homicide_clean;
    tables Solved*Agency_Type2 / chisq;
    tables Solved*'Victim Sex'n / chisq;
    tables Solved*Victim_Race2 / chisq;
    tables Solved*Weapon2 / chisq;
run;


/* Bivariate analysis: victim age */
proc means data=homicide_clean n mean std min q1 median q3 max;
    class Solved;
    var Victim_Age_Clean;
run;

proc ttest data=homicide_clean;
    class Solved;
    var Victim_Age_Clean;
run;


/* Full candidate logistic regression model */
proc logistic data=homicide_clean descending;
    class 
        Agency_Type2 (param=ref ref="Municipal Police")
        'Victim Sex'n (param=ref ref="Male")
        Victim_Race2 (param=ref ref="White")
        Weapon2 (param=ref ref="Firearm");
    model Solved = 
        Victim_Age_Clean
        Agency_Type2
        'Victim Sex'n
        Victim_Race2
        Weapon2
        / clodds=wald;
run;


/* Reduced final logistic regression model */
proc logistic data=homicide_clean descending;
    class 
        'Victim Sex'n (param=ref ref="Male")
        Victim_Race2 (param=ref ref="White")
        Weapon2 (param=ref ref="Firearm");
    model Solved = 
        'Victim Sex'n
        Victim_Race2
        Weapon2
        / clodds=wald lackfit;
run;