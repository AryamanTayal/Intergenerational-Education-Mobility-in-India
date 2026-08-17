*Tables and Regression
*Next step
/*1. Show who is in your sample (composition)
 2. Show raw education gaps across caste groups before any regression 
3. Show that caste groups differ on wealth, poverty, location — motivating the need for controls*/
*Making another do file. Will run this again cleanly see if the merge file I get is any different from the one I have currently and then combine the code for the table thing

* MAIN STEPS BEFORE REGRESSION
cd "C:\"
use "analysis_sample_2005_redo.dta", clear
* ── Step 1: Verify caste variable looks right ──
tab caste5

* ── Step 2: Core Table 1 ──
* Weighted means by caste group for all key variables

tabstat ED5 par_edu HHASSETS log_copc URBAN girl RO5 ///
        [aweight = SWEIGHT], ///
        by(caste5) ///
        stats(mean sd) ///
        format(%8.2f) ///
        nototal /// this is table in excel

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
	
	
* ── Conditional means table ── (Second table we will generate so far)
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
label variable ED5      "Child's Education (years)"
label variable par_edu  "Parental Education (years)"
label variable HHASSETS "Asset Index (0-33)"
label variable POOR     "Below Poverty Line (%)"
label variable URBAN    "Urban (%)"
label variable girl     "Female (%)"
label variable RO5      "Age"	

* ── Output all groups in one table ──
esttab caste1 caste2 caste3 caste4 caste5 ///
    using "table1.csv", ///
    replace ///
    cells("mean(fmt(2)) sd(fmt(2))") ///
    mtitles("Upper" "OBC" "Dalit" "Adivasi" "Muslim") ///
    title("Table 1: Summary Statistics by Caste Group") ///
    label ///
    noobs	


	
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
	
	
