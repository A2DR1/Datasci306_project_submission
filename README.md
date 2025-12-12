# Datasci 306 project submission

## Instruction

This project contains two files: project.R and final_jobs.rds. project.R contain the source code of the shiny app and final_jobs.rds is contains the dataset we used in this final project.

To run this app locally, make sure that shiny is setup and all required packages are installed.

Here are the packages that the project would use.
```{R}
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyr)
library(stringr)
library(maps)
library(tibble)
```

Open project.R in rstudio. If everything is setup correctly, you should see "Run App" button on the top right corner. Click on it and wait for the app to start runing. 

## Summary

### Skill–Company Navigator
-----------------------

The Skill–Company Navigator is an interactive web application built in R Shiny that allows users to explore how specific skills relate to job opportunities, employer behavior, and salary patterns. Using a structured dataset of job postings, the app reveals how skills influence earnings, which companies demand them, and where opportunities are located geographically.

### Key Features
------------

1. Skill Explorer
   - Allows users to select a skill (e.g., Python, SQL).
   - Displays hourly salary distribution for postings requiring the skill.
   - Shows top companies hiring for the selected skill, including posting counts and median pay.
   - Identifies industries where the skill is in demand.
   - Includes a U.S. geographic heatmap showing either job counts or median hourly wages.
   - Helps answer the question: “If I learn skill X, what opportunities and salaries will I unlock?”

2. Company Explorer
   - Allows users to select a company (e.g., Amazon, Google, Tesla).
   - Shows the top skills the company hires for.
   - Provides salary summaries for that company’s postings.
   - Displays the industries represented in the company’s listings.
   - Includes a scatterplot relating employer size to median hourly pay.
   - Provides a sortable, filterable table of company metrics.
   - Includes options to filter by skill, sort by different metrics, or highlight a specific company.
   - Helps answer the question: “Which companies best match my skillset and salary expectations?”

### Interactive Components
----------------------

The app includes:
- Dropdown selectors for skills and companies
- Sliders for choosing the number of companies displayed
- Skill-based filters for narrowing company results
- Radio buttons for choosing map metrics
- Interactive tables that update based on user input
- Plots that dynamically respond to selections

### Visualizations
--------------

The app provides several meaningful visual outputs, including:
- Salary histograms
- Bar charts of industries and skills
- Scatterplots comparing pay and company size
- Choropleth maps of the United States
- Tables summarizing company performance metrics

### Reproducibility
---------------

The application is fully reproducible from the provided code and dataset.
It uses a cleaned `.rds` file of job postings and standard R packages such as shiny, dplyr, ggplot2, DT, and maps.
The repository includes:
- app.R
- final_jobs.rds
- Instructions for running the application locally

### Purpose
-------

This project is designed to help users understand the relationship between skills, salaries, job availability, and employer behavior. It provides insight into how learning new skills may expand job opportunities, increase salary potential, or change the types of companies and industries available to a job seeker.
