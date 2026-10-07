# Build-time validation record

Inspection date: 30 September 2026.

## Checks completed

- Parsed the live NHS England A&E index and found 12 monthly financial-year pages.
- Parsed all 12 pages and found 231 relevant provider data links representing 133
  consecutive activity months from June 2015 to June 2026.
- Confirmed that one XLS/XLSX provider workbook is available for every one of those
  133 months; CSV alternatives account for 98 additional links.
- Downloaded and inspected representative provider workbooks for June 2015 and
  April months in later schema eras, including 2016, 2017, 2021 and 2026.
- Confirmed the older `A&E Data` and later `Provider Level Data` sheet names, the
  movement of the Type 1 breach group when a direct within-four-hours group was
  added, and the addition of booked-appointment reporting.
- Downloaded and inspected the current England workbook. Its `Performance` sheet
  has 188 monthly data rows from November 2010 to June 2026 and supplies direct
  Type 1 within-four-hours, over-four-hours and percentage fields.
- Compared April 2021 workbook and CSV representations. Fourteen CRS field-test
  providers have missing four-hour performance in the workbook but zero breach
  values in the CSV. This is why workbooks are the default source.
- Performed delimiter and CSV-width checks on the project source/configuration files.
- Inspected the July 2026 RTT full extract and England overview workbook, confirming
  `Part_2`, `C_999`, weekly bands through 18 weeks and the estimated national series.
- Confirmed the RTT national series has 228 consecutive valid months from August
  2007 to July 2026 after parsing the official `* Feb-24` footnote marker.
- Inspected the July 2026 DM01 provider and time-series workbooks, confirming the
  total waiting-list and 6+ week fields.
- Inspected the July 2026 cancer combined CSV and confirmed the single all-cancer,
  all-route, all-modality `62D` headline row at England and provider level.
- Downloaded and inspected the July 2026 CWT CRS national time-series workbook. Its
  combined 62-day block contains 52 consecutive provider-based England months from
  April 2022 to July 2026; every row satisfies within plus outside equals total and
  the recalculated proportion equals the published performance.
- Inspected the August 2026 AmbSYS CSV and current indicator list, confirming that
  `A31` is Category 2 mean response time in seconds.
- Confirmed that monthly discovery derives the activity month from the link label,
  publication-page title and file URL together; this covers generic labels such as
  `Full CSV data file` and `Monthly Combined CSV`.
- Checked all R source files for balanced delimiters/quotes, all configuration CSVs
  for consistent row widths, and both HTML templates for complete placeholder maps.
- Added a generated national-history audit which requires each of the four added core
  metrics to support its complete configured training and rolling-backtest windows.
- Inspected representative official UCR, community waiting-list and Talking Therapies
  workbooks and recorded three hand-checked source values in
  `config/core_qa_reference_values.csv`.
- Recalculated the January 2023 England community measure directly from the total,
  18-to-52-week and over-52-week bands and confirmed the configured value.
- Inspected all eight service tabs (Tables 4–4g) in the January 2023 community
  waiting-list workbook. The reshaped panel contains 47 adult and children’s service
  categories across England, region, ICB and organisation rows. England service totals
  sum exactly to the Table 3 headline total of 862,432. The importer supports both the
  older single over-52-week band and the later split 52–104 and over-104-week bands.
- Confirmed that the service derivation retains suppressed and unsubmitted cells as
  distinct states. It derives within-18-week performance from total less the published
  over-18-week bands, and separately reports material differences between total and the
  sum of all published bands instead of silently forcing reconciliation.
- Added a run-specific forecast-method record for every published indicator, including
  component definitions, modelling scale, final training length, rolling test window,
  error scores, interval method and calibration sample. The release gate checks that
  this record matches the actual forecast outputs.
- Added a consolidated release gate covering provenance, numerator/denominator
  reconciliation, time-series gaps, unusual latest movements, reference values,
  benchmark accuracy, archived forecast preservation and PDF page geometry.
- Tested the PDF page and MediaBox detection against the supplied draft PDFs; the
  checks correctly identify those older drafts as one-page landscape rather than the
  required A4 portrait output.

## Checks outstanding

R is not installed in the build environment, so the R pipeline has not been executed
and `renv.lock` has not been generated. The first R run must complete scripts 12–20
and retain `output/qa/release_signoff.csv` before publication. Static checks confirmed
balanced R delimiters and quoted strings and consistent configuration CSV widths.
