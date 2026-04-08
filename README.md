# Newcastle MOOC: Learner Retention and Drop-Off Patterns

## Overview
This project analyzes learner engagement in a Newcastle University online cybersecurity MOOC. The goal is to identify where and why learners drop off during the course and to provide data-driven insights that may enhance course design and learner retention.

## Research Questions
**Primary Question:**  
Is there a significant drop-off in learner participation across different course steps?  

**Secondary Question:**  
What factors contribute to learner dropout?

## Approach
The analysis uses step-level data encompassing:
- Number of active learners per step  
- Median time spent on each step  

**Key Methods:**
- Retention analysis across course progression  
- Visualizations combining learner counts and step duration  
- Examination of step characteristics (e.g., type, difficulty)

## Expected Outcomes
- Identification of high-attrition points  
- Highlighting potential bottlenecks in course progression  
- Actionable recommendations to improve retention and engagement  

## Project Structure
```
.
├── cache/       # Cached intermediate results for faster execution
├── config/      # Configuration files (paths, parameters, settings)
├── munge/       # Data cleaning and preprocessing scripts
├── reports/     # Analytical outputs (HTML, PDF, Word reports)
```

## Setup Instructions
1. **Install Required R Version**  
   Confirm that you have the R version specified in `renv.lock`.

2. **Clone or Download the Repository**  
   ```
   git clone <repo-url>
   ```
   Alternatively, download and extract the ZIP file.

3. **Open the Project in RStudio**  
   Open the `.Rproj` file located in the project root to ensure proper environment configuration.

4. **Restore Dependencies**  
   If `renv` is not installed, install it and restore the project environment:  
   ```R
   install.packages("renv")
   renv::restore()
   ```

## Running the Project
1. **Data Preparation**  
   Execute scripts in the `munge/` directory to clean and prepare the raw data.

2. **Analysis and Reporting**  
   Run the analysis scripts or render the R Markdown files.  
   Final outputs will be saved in the `reports/` directory.

## Outputs
The project produces the following deliverables:
- Learner retention curves  
- Step-level engagement visualizations  
- Comprehensive reports summarizing dropout patterns and insights  

## Example Key Insights
- Steps with highest learner drop-off identified  
- Correlation between step duration and disengagement established  
- Influence of content type and difficulty on retention examined  

## Requirements
- R (version specified in `renv.lock`)  
- RStudio (recommended)  
- `renv` for dependency management  

## Contributing
Contributions, suggestions, and improvements are welcome.  
Please open an issue or submit a pull request to collaborate on project development.

## License
Developed by Pavithra Govinda Raj in liaison with Newcastle University.
