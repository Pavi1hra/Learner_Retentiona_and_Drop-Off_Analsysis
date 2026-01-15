# "Newcastle MOOC : Learner Retention and Drop-Off Patterns"

#Description:
This project investigates learner engagement in a Newcastle University online 
cybersecurity course. The primary research question is whether there is a 
significant drop-off during the course, and the secondary question explores factors 
that may influence learner dropout. Step-level data, including median time spent per step 
and learner counts, are analyzed to identify points where learners disengage. Visualizations 
overlaying step duration and learner retention help highlight bottlenecks, 
while further analysis considers step characteristics, such as type or difficulty, 
as potential factors affecting drop-off. The findings aim to pinpoint stages of high 
attrition and provide insights to improve course design and learner retention.

#Directory information: 
the major folders in the project directory that we will be using are: 

cache/ – Stores cached intermediate results to speed up analyses and avoid repeated computations.
reports/ – Contains final rendered outputs (e.g., PDF/HTML/Word reports) for submission or presentation.
munge/ – Holds scripts for cleaning, transforming, and preparing raw data for analysis.
config/ – Centralizes project configuration settings such as paths, parameters, and global options.

#Project setup 
The project is fairly easy to setup, downloading the zipped folder and running it with the r version that is denoted in the renv.lock file

#Project execution instruction
- Install the required R version
- Ensure that the version of R specified in the renv.lock file is installed on your system.
- Download and extract the project
- Download the zipped project folder and extract it to a local directory.
- Open the project in RStudio
- Open the .Rproj file located in the project root to ensure correct working directories and project settings.
- Restore the project environment
- Install renv if it is not already installed, then restore the project-specific package library:
install.packages("renv")
- renv::restore()
- Run data preparation scripts
- Execute the scripts in the munge/ directory to clean and prepare the raw step-level data for analysis.
- Run analysis and generate outputs
- Run the analysis scripts or render the report files (e.g., R Markdown) to reproduce figures and results.
Final outputs will be saved in the reports/ directory.

