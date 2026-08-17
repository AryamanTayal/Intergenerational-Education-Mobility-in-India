*DO File for Econometric Project
*Does intergenerational education mobility differ by caste in India, and did it change between 2004-05 and 2011-12?

cd "C:\"
use "22626-0001-Data.dta", clear
describe // loading in the individual file
sort STATEID DISTID PSUID HHID HHSPLITID
merge m:1 STATEID DISTID PSUID HHID HHSPLITID using "22626-0002-Data.dta"
tab _merge
save "ind_hh_merge_redo.dta", replace
browse
******************************************************************************
*getting to understand the data
tab HHED5F  
tab HHED5M
lookfor age
tab RO5
summarize COPC
summarize ED5
*looking for missing values
misstable summarize ED5 HHED5M HHED5F GROUPS8 INCOME COPC HHASSETS URBAN RO5 SWEIGHT

*understanding the age group 
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
******************************************************************************

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
save"ind_hh_merge_redo.dta", replace
keep if eligible_child == 1
save "analysis_sample_2005_redo.dta", replace
* Check how many you have
count
* Should be several thousand  *this is second run tally with original run 
  
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
replace COPC = . if COPC <= 0 //replaced 0 values with missing
count if COPC <=0

gen log_copc = log(COPC)        // log per capita consumption
count if log_copc == .  // should return 28
* Use IHS transformation — handles both cases cleanly
*not using


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
save "analysis_sample_2005_redo.dta", replace
* Important — save this separately so you don't 
* have to redo cleaning every time	
	
