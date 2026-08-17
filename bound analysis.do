cd "C:\aryaman\Masters\SEM 2\IHDS\data2005\ICPSR_22626-V12\ICPSR_22626\my_changes"
use "analysis_pooled.dta", clear
ssc install psacalc

* ── STEP 1: RUN THE FULL MODEL ONLY ──────────────────────────────────────────
* Run your primary regression with interactions, weights, and clustering.
reg EDU c.par_edu##i.caste5 assets_std RO5 girl URBAN i.STATEID ///
    [pweight=weight_unified] if wave==0, cluster(distid_cluster)

* ── STEP 2: COMPUTE OSTER'S DELTA FOR PARENTAL EDUCATION ──────────────────────
* This calculates how strong unobservables need to be to make the effect 0.
* We use rmax(0.5889) because 1.3 * 0.453 = 0.5889
psacalc delta par_edu, rmax(0.5889) beta(0)

* ── STEP 3: COMPUTE OSTER'S DELTA FOR CASTE (E.G., DALIT COEFFICIENT) ────────
* Note: If '3.caste5' is your Dalit indicator, this tests the caste gap robustness.
psacalc delta 3.caste5, rmax(0.5889) beta(0)

* ── STEP 4: IDENTIFIED SET (BOUNDS) FOR PARENTAL EDUCATION ────────────────────
* This gives you the bias-adjusted coefficient under the assumption that 
* selection on unobservables is equal to selection on observables (delta=1).
psacalc beta par_edu, rmax(0.5889) delta(1)
psacalc beta 3.caste5, rmax(0.5889) delta(1)

psacalc beta  5.caste5, rmax(0.5889) delta(1)
psacalc delta 5.caste5, rmax(0.5889) beta(0)