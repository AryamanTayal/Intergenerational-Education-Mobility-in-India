*DO File for Econometric Project
*Does intergenerational education mobility differ by caste in India, and did it change between 2004-05 and 2011-12?
clear
use "C:\aryaman\Masters\SEM 2\IHDS\data2005\ICPSR_22626-V12\ICPSR_22626\DS0001\22626-0001-Data.dta"
describe
sort STATEID DISTID PSUID HHID HHSPLITID
merge m:1 STATEID DISTID PSUID HHID HHSPLITID using "C:\aryaman\Masters\SEM 2\IHDS\data2005\ICPSR_22626-V12\ICPSR_22626\DS0002\22626-0002-Data.dta"
tab _merge
save "C:\aryaman\Masters\SEM 2\IHDS\data2005\ICPSR_22626-V12\ICPSR_22626\my_changes\ind_hh_merge.dta", replace
browse

tab HHED5F  
tab HHED5M
lookfor age
tab RO5

misstable summarize ED5 HHED5M HHED5F GROUPS8 INCOME COPC HHASSETS URBAN RO5 SWEIGHT
summarize COPC

summarize ED5

sum ED5 if RO5 >= 17 & RO5 <=21
tab GROUPS8 if RO5 >=17 & RO5 <= 21
tabstat ED5, by(GROUPS8) stats(mean sd n)
tabstat ED5 if RO5 >=17 & RO5 <= 21, by(GROUPS8) stats(mean sd n)
codebook HHED5F
tab ED5, nolabel
*First I want to see in how many observations is women's max > men's max
generate male_more = (HHED5M > HHED5F)
tab male_more 

codebook male_more
generate max_hhED5 = max(HHED5M, HHED5F)

*Now some data cleaning. Delete all irrelevant obs under each column. Let me confirm
* the columns once
/*PART 1: SAMPLE & DATA CLEANING
  ├── 1a. Define analysis sample (HH with children 17-21)
  ├── 1b. Handle missing values across all variables
  ├── 1c. Construct/finalize all variables
  └── 1d. Final sample check

PART 2: DESCRIPTIVE ANALYSIS
  ├── 2a. Summary statistics table (Table 1)
  └── 2b. Visual exploration

PART 3: REGRESSION ANALYSIS
  ├── 3a. Baseline model
  ├── 3b. Sequential variable addition
  └── 3c. Interaction terms

PART 4: [THE LAST PART — we'll come back to this]
  └── IHDS-II 2012 wave + panel comparison*/
  
* Collecting Variables
tab HHASSETS // wealth control
tab GROUPS8, nolabel
* First recode GROUPS8 into your 5 categories
gen caste5 = .
replace caste5 = 1 if GROUPS8 == 1 | GROUPS8 == 2  // Upper caste
replace caste5 = 2 if GROUPS8 == 3                  // OBC
replace caste5 = 3 if GROUPS8 == 4                  // Dalit (SC)
replace caste5 = 4 if GROUPS8 == 5                  // Adivasi (ST)
replace caste5 = 5 if GROUPS8 == 6                  // Muslim

label define caste5lbl 1 "Upper" 2 "OBC" 3 "Dalit" 4 "Adivasi" 5 "Muslim"
label values caste5 caste5lbl

* In regression, use i.caste5
* Stata automatically creates dummies, upper caste = reference
* state fixed effects
tab STATEID 
tab STATEID2
/*stateid2: stateid2 is a slightly collapsed version of stateid that creates 22 states and state groups from the 33 states in IHDS. stateid2 also sorts the states into a slightly different regional order. Chandigarh is collapsed into Punjab. All Northeast states and Sikkim are treated as a single group. Daman and Diu is collapsed into Gujarat. Dadra and Nagar Haveli is also collapsed into Gujarat. Goa is collapsed into Maharashtra. Pondicherry is collapsed into Tamil Nadu.*/
* interaction with dummy c.par_edu#i.caste5

*******************************************************************************
/*X7 — % Below Poverty Line?
No, don't include poor as a regressor. Here's why — it's derived from copc (consumption per capita). If you include both poverty dummy and consumption/assets, they're measuring the same thing and you'll have collinearity.
However, poor is very useful for:

Your summary statistics table (show poverty rates by caste)
Subsample analysis — run your regression separately for poor vs non-poor households to see if mobility differs by poverty status  */
*********************************************************************************
*Step 1a — Define Your Analysis Sample
* Start from your merged dataset
* Keep only households with at least one child aged 17-21

* First, flag eligible children
gen eligible_child = (RO5 >= 17 & RO5 <= 21)

* Keep only eligible children
* (one row per child, household vars attached via merge)
save"C:\aryaman\Masters\SEM2\IHDS\data2005\ICPSR_22626-V12\ICPSR_22626\my_changes\ind_hh_merge.dta", replace
keep if eligible_child == 1
save "analysis_sample_2005.dta", replace
* Check how many you have
count
* Should be several thousand  
  
*Step 1b
local myvars "ED5 HHED5M HHED5F HHASSETS COPC GROUPS8 URBAN"

foreach var of local myvars {
    * Valid blank — legitimately not applicable
    replace `var' = .a if `var' == -1
    
    * Invalid skip — data error
    replace `var' = .b if `var' == -4 
}
	
count if HHED5M == .a   // no adult male in household (valid)
count if HHED5M == .b   // data entry error (invalid)
count if HHED5M == .     // any other missing	

* continuing with the cleaning
gen par_edu = max(HHED5M, HHED5F)

* Verify no negatives remain anywhere
local myvars "ED5 HHED5M HHED5F HHASSETS COPC GROUPS8 URBAN"
foreach var of local myvars {
    count if `var' < 0
}

* Check how many valid blanks vs invalid skips you have
* across your key variables
foreach var of local myvars {
    di "=== `var' ==="
    count if `var' == .a    // valid blank
    count if `var' == .b    // invalid skip
}
	
* Now drop observations missing on key variables
* Important: in Stata, ALL missing types (.  .a  .b  .c etc)
* satisfy the condition (var >= .) 
* So this single condition catches everything:

egen nmiss = rowmiss(ED5 par_edu caste5 HHASSETS URBAN STATEID2)
tab nmiss

* See what you're losing and why before dropping
count if nmiss > 0

* Then drop
drop if nmiss > 0
drop nmiss	
	
* Constructing final variables
*Caste already made

* ── Child gender dummy ──
* Check how sex is coded first
tab RO3
* Then create clean dummy
gen girl = (RO3 == 2)   // adjust 2 to whatever female code is
	
* ── Rural dummy ──
* URBAN variable: 0=rural, 1=URBAN
* Already binary, use directly
* But verify:
tab URBAN 
* ── Log transformations for skewed variables ──
* Income and consumption are right-skewed

* Note: log(0) = missing, check for zero values first:
count if COPC == 0
count if INCOME == 0
*Recoding copc 0 consumption
replace COPC = . if COPC <= 0
count if COPC <=0

gen log_copc = log(COPC)        // log per capita consumption
count if log_copc == .  // should return 28
* Use IHS transformation — handles both cases cleanly
gen ihs_income = log(INCOME + sqrt(INCOME^2 + 1))

* Verify it worked — IHS has no undefined values
count if ihs_income == .  // should be 0 or very small
sum ihs_income, detail	// ignore IHS- Inverse Hyperbloic sine


*Step 1d	
* How big is your final sample?
count

* Composition by caste
tab caste5

* Composition by gender
tab girl

* Rural/URBAN split
tab URBAN

* Verify education variables look sensible
sum ED5 par_edu, detail
* ED5 should be 0-15
* par_edu should be 0-15
* No negatives should remain

* Save your clean analysis dataset
save "analysis_sample_2005.dta", replace
* Important — save this separately so you don't 
* have to redo cleaning every time	
	
*Next step
/*1. Show who is in your sample (composition)
 2. Show raw education gaps across caste groups before any regression 
3. Show that caste groups differ on wealth, poverty, location — motivating the need for controls*/
*Making another do file. Will run this again cleanly see if the merge file I get is any different from the one I have currently and then combine the code for the table thing

* MAIN STEPS BEFORE REGRESSION

* ── Step 1: Verify caste variable looks right ──
tab caste5

* ── Step 2: Core Table 1 ──
* Weighted means by caste group for all key variables

tabstat ED5 par_edu HHASSETS log_copc URBAN girl RO5 ///
        [aweight = SWEIGHT], ///
        by(caste5) ///
        stats(mean sd) ///
        format(%8.2f) ///
        nototal

* This gives you mean and SD for each variable
* broken down by your 5 caste groups
* ── Step 3: Sample sizes by caste ──
* Unweighted N (actual observations)
tab caste5

* Weighted population shares
tab caste5 [aweight = SWEIGHT]	

* ── Step 4: Poverty rates by caste ──
* poor is a useful summary even though
* we're not using it in regression
tabstat POOR [aweight = SWEIGHT], ///
        by(caste5) ///
        stats(mean) ///
        format(%8.3f)	
	
* ── Step 5: Test if differences are significant ──
* One-way ANOVA — are group means statistically different?
oneway ED5 caste5, tabulate
oneway par_edu caste5, tabulate
* F-statistic and p-value tells you if caste groups
* differ significantly on education	
	
	
* ── Conditional means table ──
* Mean child education by BOTH parental education 
* AND caste group
* Shows the mobility gradient visually

* Split parental education into categories
gen par_edu_cat = .
replace par_edu_cat = 1 if par_edu == 0
replace par_edu_cat = 2 if par_edu >= 1 & par_edu <= 5
replace par_edu_cat = 3 if par_edu >= 6 & par_edu <= 10
replace par_edu_cat = 4 if par_edu >= 11

label define parcatlbl ///
    1 "No education" ///
    2 "Primary (1-5)" ///
    3 "Secondary (6-10)" ///
    4 "Higher (11+)"
label values par_edu_cat parcatlbl


	
* Run for each caste group separately
forvalues c = 1/5 {
    di "=== Caste Group `c' ==="
    tabstat ED5 [aweight = SWEIGHT] if caste5 == `c', ///
            by(par_edu_cat) ///
            stats(mean) ///
            format(%8.2f)
}
* ── Panel A: Education variables ──
tabstat ED5 par_edu [aweight = SWEIGHT], ///
        by(caste5) ///
        stats(mean sd) ///
        format(%8.2f) ///
        nototal

* ── Panel B: Socioeconomic variables ──
tabstat HHASSETS POOR log_copc [aweight = SWEIGHT], ///
        by(caste5) ///
        stats(mean sd) ///
        format(%8.2f) ///
        nototal

* ── Panel C: Demographic variables ──
tabstat URBAN girl RO5 [aweight = SWEIGHT], ///
        by(caste5) ///
        stats(mean sd) ///
        format(%8.2f) ///
        nototal

* ── Sample sizes ──
tab caste5                        // unweighted N
tab caste5 [aweight = SWEIGHT]    // weighted shares	
	
* Install if needed
ssc install estout

* ── For each caste group, store estimates ──
forvalues c = 1/5 {
    estpost tabstat ED5 par_edu HHASSETS POOR ///
                   URBAN girl RO5 ///
            if caste5 == `c' ///
            [aweight = SWEIGHT], ///
            stats(mean sd) ///
            columns(statistics)
    estimates store caste`c'
}

* ── Output all groups in one table ──
esttab caste1 caste2 caste3 caste4 caste5 ///
    using "table1.csv", ///
    replace ///
    cells("mean(fmt(2)) sd(fmt(2))") ///
    mtitles("Upper" "OBC" "Dalit" "Adivasi" "Muslim") ///
    title("Table 1: Summary Statistics by Caste Group") ///
    label ///
    noobs	

label variable ED5      "Child's Education (years)"
label variable par_edu  "Parental Education (years)"
label variable HHASSETS "Asset Index (0-33)"
label variable POOR     "Below Poverty Line (%)"
label variable URBAN    "Urban (%)"
label variable girl     "Female (%)"
label variable RO5      "Age"	
	
*Regression part

* ── Install outreg2 if not already installed ──
ssc install outreg2

* ────────────────────────────────────────────
* MODEL 1: Baseline — parental education only
* ────────────────────────────────────────────
reg ED5 par_edu RO5 girl URBAN ///
    [pweight = SWEIGHT], ///
    cluster(DISTID)

outreg2 using "regression_table.xls", ///
    replace ///
    label ///
    title("Table 2: Intergenerational Education Mobility by Caste") ///
    ctitle("Model 1") ///
    addtext(Caste FE, No, Caste X ParEdu, No, ///
            Wealth Controls, No, State FE, No)

* ────────────────────────────────────────────
* MODEL 2: Add caste dummies
* ────────────────────────────────────────────
reg ED5 par_edu i.caste5 ///
    RO5 girl URBAN ///
    [pweight = SWEIGHT], ///
    cluster(DISTID)

outreg2 using "regression_table.xls", ///
    append ///
    label ///
    ctitle("Model 2") ///
    addtext(Caste FE, Yes, Caste X ParEdu, No, ///
            Wealth Controls, No, State FE, No)

* ────────────────────────────────────────────
* MODEL 3: Add caste × par_edu interactions
* MAIN SPECIFICATION
* ────────────────────────────────────────────
reg ED5 c.par_edu##i.caste5 ///
    RO5 girl URBAN ///
    [pweight = SWEIGHT], ///
    cluster(DISTID)

outreg2 using "regression_table.xls", ///
    append ///
    label ///
    ctitle("Model 3 (Main)") ///
    addtext(Caste FE, Yes, Caste X ParEdu, Yes, ///
            Wealth Controls, No, State FE, No)

* ────────────────────────────────────────────
* MODEL 4: Add wealth controls
* ────────────────────────────────────────────
reg ED5 c.par_edu##i.caste5 ///
    HHASSETS ///
    RO5 girl URBAN ///
    [pweight = SWEIGHT], ///
    cluster(DISTID)

outreg2 using "regression_table.xls", ///
    append ///
    label ///
    ctitle("Model 4") ///
    addtext(Caste FE, Yes, Caste X ParEdu, Yes, ///
            Wealth Controls, Yes, State FE, No)

* ────────────────────────────────────────────
* MODEL 5: Add state fixed effects
* MOST CREDIBLE SPECIFICATION
* ────────────────────────────────────────────
reg ED5 c.par_edu##i.caste5 ///
    HHASSETS ///
    RO5 girl URBAN ///
    i.STATEID2 ///
    [pweight = SWEIGHT], ///
    cluster(DISTID)

outreg2 using "regression_table.xls", ///
    append ///
    label ///
    ctitle("Model 5") ///
    addtext(Caste FE, Yes, Caste X ParEdu, Yes, ///
            Wealth Controls, Yes, State FE, Yes)	
	

	* Run Model 3 first, then:
margins caste5, at(par_edu = (0 2 4 6 8 10 12 14))
marginsplot, ///
    title("Predicted Child Education by Parental Education and Caste") ///
    xtitle("Parental Education (years)") ///
    ytitle("Predicted Child Education (years)") ///
    legend(order(1 "Upper" 2 "OBC" 3 "Dalit" 4 "Adivasi" 5 "Muslim"))
	
	
	
	
	
	