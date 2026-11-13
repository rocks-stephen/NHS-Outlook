# A&E outturn: George Eliot patient-flow deep dive

This optional case study extends the **monthly outturn**, not the advance forecast. It starts from George Eliot's sustained favourable A&E trajectory signal and investigates possible contributions from attendance, bed pressure and discharge. A forecast residual is a discovery signal, **not** proof of causality.

## Source material

Place NHS England provider source files in `data/flow/` (excluded from Git). The script scans CSV/XLS/XLSX locally and uses the trust code **RLT**, checking organisation names in the QA inventory. It prioritises monthly Beds SitRep (G&A beds available/occupied and patients staying 21+ days) and A&E activity. For discharge-ready-date and weekday/weekend discharge measures, verify exact NHS England provider fields before supplying `data/flow/validated_discharge_monthly.csv`. Never mix DRD delayed days with no-longer-meets-criteria-to-reside snapshots or monthly Beds SitRep with quarterly KH03 occupancy.

Expected validated file fields: `month` (YYYY-MM-01), `drd_delayed_bed_days`, `drd_delayed_days_share_pct`, `weekday_discharges`, `weekend_discharges` (the latter two must be **total** discharges in each group of days). The script derives weekend share. Missing inputs remain missing, not zero.

## Run

From the project root:

```r
options(nhs.outlook.deep_dive.data_dir = 'data/flow')
source('scripts/21_build_ae_flow_deep_dive.R')
```

By default no files are downloaded. Explicitly opt in to source discovery/downloads with `options(nhs.outlook.deep_dive.download = TRUE)`; check download inventory and revised-file conflicts before release. Outputs appear in `output/deep-dives/george-eliot/`.

The monthly **outturn** publisher can embed the case-study link and generate the HTML case-study page after its regular outputs are built. With no verified source data, the published case-study page is explicitly marked **awaiting validated supplementary data**, and existing outturn/QA outputs remain unchanged.

## Editorial/QA rules

- Separate verified descriptive changes from interpretations and from trust-attributed explanations.
- Assess changes over at least 2024–26, noting reporting breaks and changes in acute capacity.
- Recheck case-selection against the current frozen-forecast provider watch; do not imply a favourable signal is necessarily still active.
- Test total and Type 1 attendance, case mix, long waits, delayed discharge, G&A occupancy, seven-day discharge and possible coding changes.
- Check the latest actual month; flag stale measures rather than carrying them forward.
- Do not assert that the additional fraction of four-hour completions is attributable to a single intervention without a suitable counterfactual.
