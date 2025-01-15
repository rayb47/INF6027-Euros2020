# INF6027-Euros2020

This project analyzes football tournament data (from the 2020 Euros) to address 3 main proposed research questions,
1. What are the differences in statistics between the group stage and knockout stage for different teams?
2. Do attacking or defensive metrics play a more critical role in determining whether a team qualifies for the knockout stage?
3. Can team performances throughout the tournament be clustered into distinct groups based on various performance metrics?

The code for this project is well-documented and neatly organized to ensure clarity and reproducibility. The scripts are well-indented with appropriate comments to explain the steps of the analysis. All the code is divided into 2 files,
1. main.R - This file handles the processing of data, computation of metrics and the entire analysis
2. functions.R - This files contains all the user-created functions that are necessary to carry out the required analysis. This file does not need to be visited, as it is executed through main.R.

Note
File main.R is split into 4 main parts,
- The first handles the loading and processing of data to create a custom dataframe to store all the computed metrics (average goals scored, fouls committed, possession percentage, etc.). It also uses the data from the multiple dataset files to tabularize the wins, losses and draws for the top 8 teams at the tournament, in the knockout stage and group stage respectively.
- The second, third and fourth part handle the analysis for the first, second and third research questions respectively.
- The second part

## Steps to Execute the Code
1. Clone the contents of the repository to your local machine.
   
   ```git clone https://github.com/yourusername/your-repo-name.git```
2. Open R or RStudio and set the working directory to the cloned repository

   ```setwd("path/to/your-repo-name")```
3. Run the code step-by-step

   - Highlight a section of code, or place your cursor on a specific line that you want to execute and press Ctrl+Enter to execute the code.
   - This will display results in the R Console (Tables or resutls of statistical tests) or the Plots panel (Visualizations).
  
## Generated Visualizations
Once you have finished running the main.R file, you would have seen the following visualizations (in order of generation in the code).

### Research Question 1
All the metric values here are averages (per game) and only for the top 8 teams in the tournament. That is, the 8 teams that made it to the quarter finals.

### Dotplot for Average Goals Scored (Across Group and Knockout stages)
![DotPlot1](https://github.com/user-attachments/assets/46900303-19f8-4e7b-8125-a69c2f63b7a8)

### Dotplot for Average Goals Conceded (Across Group and Knockout stages)
![DotPlot2](https://github.com/user-attachments/assets/65f94b5a-4171-4434-b47e-1e49139cfcc4)

### Clustered Bar Chart showing Average Possession Percentage Maintained (Across Group and Knockout stages)
![BarChart1](https://github.com/user-attachments/assets/c9f9e6cd-4f94-48f6-921e-951b9ee6c96c)

### Heatmap for Average Number of Fouls Committed 
![HeatMap](https://github.com/user-attachments/assets/c7dd011e-29e4-46fb-9fbe-4a8a8210e48e)

### Boxplots for Combined Average Fouls Committed by All Teams Grouped by Stage
![Boxplot](https://github.com/user-attachments/assets/e9229a26-8c8e-4d63-8e84-ffd8bee33b51)

The output of the statistical tests (Mann Whitney Test, Welch's t-tests) conducted for this research question will look like this in the R console,
![image](https://github.com/user-attachments/assets/712ea251-49fe-4af7-88e4-8dd5ecfa8fa4)



### Research Question 2
All the metric values here are averages for all teams in the Group Stage of the tournament, since we are measuring the impact they have on qualification to the knockout stage.

### Bar Chart for Correlation Analysis Results
![CorrelationAnalysis](https://github.com/user-attachments/assets/0ae1f5e6-8ce8-4adb-bec2-7a3b68a5a551)

### Bar Chart for Feature Importance Results using Random Forest Model 
![FeatureImportance2](https://github.com/user-attachments/assets/bc104b44-37ea-4c08-83cf-9945c1f421bf)


### Research Question 3
All metric values here are averages (per-game) for all the teams in tournaments, combined across both stages.

### Possession Maintained vs Goals Scored
![Cluster1](https://github.com/user-attachments/assets/2bcc679b-5107-48df-8ef4-5500cbf84b94)

### Attacks vs Attempts on Goal
![Cluster2](https://github.com/user-attachments/assets/429296db-6328-4b17-90e4-f2d007256b7a)

### Passing Accuracy vs Attempted Passes
![Cluster3](https://github.com/user-attachments/assets/bf3597f5-00bc-4e45-a4a8-db907be0db68)








