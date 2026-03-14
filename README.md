# Chartify-shiny
Chartify is a data analytics dashboard that leverages Spotify's song features dataset to uncover patterns and characteristics of successful songs. Built for music producers and A&R professionals, it provides actionable insights on tempo, energy, danceability, and other key metrics that correlate with chart performance and cross-platform popularity. The main goal of this to to help stakeholders make data-driven decisions in hit song production.

Note: This app is a smaller version of [Chartify](https://github.com/UBC-MDS/DSCI-532_2026_31_Chartify) (coded in Python) and does not contain the same functionalities as its Python counterpart at this moment.

## Live Dashboard
You can access the deployed dashboard here: https://chrbcho-chartify-shiny.share.connect.posit.cloud

## Run the Dashboard Locally

1. **Clone the repository** and go into the project folder:
   ```bash
   git clone https://github.com/chrbcho/Chartify-shiny.git
   cd Chartify-shiny
   ```

2. **Open App.R file in RStudio:**
   ```bash
   open -a RStudio app.R
   ```
   
4. **Install the required R packages (in RStudio console):**
   ```r
   install.packages(c("shiny", "bslib", "dplyr"))
   ```

5. **Start the dashboard (in RStudio console):**
   ```r
   shiny::runApp("app.R")
   ```

## Contributor
**Christine Chow** ([@chrbcho](https://github.com/chrbcho))

If you are interested in contributing to this dashboard, please review the [CONTRIBUTING.md](./CONTRIBUTING.md) document for more information. To run the app locally, please follow the steps in the "Run the Dashboard Locally" section above.

By contributing to the project you accept and agree to follow the [Code of Conduct](CODE_OF_CONDUCT.md).

## Copyright
- Copyright (c) 2026 [Christine Chow](https://github.com/chrbcho)
- Free software distributed under the [MIT License](./LICENSE.md)