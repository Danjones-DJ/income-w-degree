# income-w-degree
Pure Degree Salary Analysis - Shiny App
An interactive R Shiny dashboard for analyzing salary data from pure degree programs at universities.
Overview
This application provides comprehensive analysis of salary outcomes for various degree programs, including salary distributions, A-Level requirements, and program options like year abroad and industry placements.
Features
📊 Overview Dashboard

Key Metrics: Total courses, average median salary, and highest salary
Salary Distribution: Interactive visualization by degree type
Top Performers: Top 15 courses by median salary

🔍 Detailed Analysis

Interactive Filters: Filter by degree type, A-Level requirements, and program options
Salary Range Analysis: Scatter plot showing relationship between median salary and salary range
A-Level Requirements: Box plot analysis of salary distribution by entry requirements

📋 Data Table

Complete Dataset: Searchable and sortable table with all course information
Export Options: Download filtered data for further analysis

Installation
Prerequisites
Make sure you have R installed with the following packages:
rinstall.packages(c(
  "shiny",
  "DT",
  "ggplot2",
  "dplyr",
  "plotly",
  "shinydashboard"
))
Setup

Clone this repository:

bashgit clone https://github.com/yourusername/pure-degree-salary-app.git
cd pure-degree-salary-app

Ensure your CSV file (pure_degree_sal.csv) is in the same directory as app.R
Run the application:

rshiny::runApp()
Data Structure
The application expects a CSV file with the following columns:

kiscourseid: Course identifier
title: Course title
degree_type: Type of degree (BA, BSc, BEng, etc.)
A_level: A-Level requirements
year_abroad_available: Whether year abroad is available (Yes/No)
industry_year_available: Whether industry year is available (Yes/No)
foundation_year_available: Whether foundation year is available (Yes/No)
uq_sal_15m: Upper quartile salary at 15 months
med_sal_15m: Median salary at 15 months
lq_sal_15m: Lower quartile salary at 15 months

Usage
Navigation
The app has three main sections:

Overview: Get a quick summary of the data with key metrics and visualizations
Detailed Analysis: Use filters to explore specific subsets of the data
Data Table: Browse the complete dataset with search and sort functionality

Filters

Degree Type: Filter by specific degree types (BA, BSc, BEng, etc.)
A-Level Requirements: Filter by entry requirements
Program Options: Filter courses that offer year abroad, industry year, or foundation year

Visualizations

All plots are interactive using Plotly
Hover over data points for detailed information
Click and drag to zoom into specific areas
Use the toolbar to pan, zoom, and reset views

Key Insights
The dashboard helps identify:

Highest Paying Degrees: Which programs offer the best salary outcomes
Entry Requirements Impact: How A-Level requirements correlate with salary
Program Options Value: Whether additional program options affect salary outcomes
Degree Type Comparison: Salary differences between BA, BSc, BEng, etc.

Customization
Adding New Visualizations
To add new plots, modify the server function in app.R:
routput$new_plot <- renderPlotly({
  # Your plot code here
})
Styling
The app uses shinydashboard for layout. Modify the UI section to change:

Colors and themes
Box layouts
Menu items

Technical Details

Framework: R Shiny with shinydashboard
Visualization: ggplot2 + plotly for interactive charts
Data Processing: dplyr for data manipulation
Tables: DT package for interactive data tables

Contributing

Fork the repository
Create a feature branch (git checkout -b feature/new-feature)
Commit your changes (git commit -am 'Add new feature')
Push to the branch (git push origin feature/new-feature)
Create a Pull Request

License
This project is licensed under the MIT License - see the LICENSE file for details.
Support
For issues and questions:

Open an issue on GitHub
Check the documentation in the code comments
Review the R Shiny documentation for advanced customization

Data Source
The salary data represents outcomes at 15 months post-graduation for various degree programs. All salary figures are in GBP (£).

Note: Make sure your CSV file is properly formatted and contains no sensitive information before sharing publicly.
