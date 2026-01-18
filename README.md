# Qirāʾāt Bibliography: Mašriq-Maġrib Analysis

Interactive analysis of Qurʾānic reading tradition (qirāʾāt) canonization patterns.

## About

This Shiny application explores the pedagogical differences between Mašriqī (Eastern) and Maġribī (Western) scholarly communities in their approaches to documenting Qurʾānic reading traditions. It combines bibliographic data, geographic visualizations, and Bayesian statistical analysis.

Research from the QurCan ERC Grant project (101054849) at Leiden University.

## Features

- **Bayesian Multinomial Regression** - Pre-computed posterior analysis comparing regional preferences
- **Regional Probability Comparisons** - Mašriq vs Maġrib system preferences by century
- **Temporal Trends** - 4th-7th century AH probability evolution
- **Geographic Scholar Mobility Explorer** - Interactive map of scholar travel routes
- **Full Bibliography** - Searchable database with ~240 works from ~135 authors

## Technical Notes

- Bayesian MCMC results pre-computed for cloud deployment (no cmdstanr required)
- Model: Multinomial logistic regression with region + century predictors
- 4 chains × 1000 iterations = 4,000 posterior draws
- All R-hat values < 1.01 (good convergence)

## File Structure

```
deploy/
├── app.R                           # Main Shiny application
├── data/
│   ├── iqsa_bibliography.db        # SQLite database
│   └── precomputed_bayesian_results.rds  # Pre-computed model results
├── routes/
│   ├── all_scholar_routes.json     # Scholar travel routes
│   ├── sea_routes_gis.json         # GIS sea route geometries
│   ├── sea_route_geometries.json   # Coastal route coordinates
│   └── mediterranean_ports.json    # Port locations
└── output/
    └── animations/                 # Map animations (optional)
```

## Citation

Ireland, J.F. (2025). The Mašriqī and Maġribī Pedagogical Canons of Qurʾānic Reading Traditions.
IQSA 2025 Annual Meeting. Leiden University.

## License

Research data and analysis tools from the QurCan ERC Grant project.
