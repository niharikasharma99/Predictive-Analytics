# Predictive-Analytics

## Hospital Readmission Prediction

### Overview
This project aims to predict hospital readmission using a dataset obtained from Kaggle. The dataset consists of over 25,000 records spanning a period of 10 years and comprises 14 columns. The level of observation is individual patient level.

### Data Preprocessing
The dataset underwent data cleaning and preprocessing, including handling missing values and creating dummy variables.
The dependent variable, "readmitted," was converted into binary form (0 and 1) for modeling purposes.

### Exploratory Analysis
A correlation matrix was generated to identify highly correlated and negatively correlated variables.
Distribution plots were created for both continuous and categorical variables to assess their impact on the dependent variable.

### Bivariate Analysis
Bivariate analysis was conducted to explore relationships between pairs of variables and their association with readmission.

### Data Modeling:
Two main techniques were employed for data modeling: Linear Regression and Decision Tree.

#### Linear Regression:

Logistic regression analysis was performed to estimate the probability of readmission based on predictor variables.
This method allowed for the estimation of readmission likelihood for each patient, considering factors such as medication usage, hospital visits, and demographic information.
The model achieved an accuracy of 61.34%, providing valuable insights into readmission patterns.

#### Decision Tree:

Decision tree analysis was utilized to reveal hierarchical relationships between predictor variables and their impact on readmission outcomes.
By partitioning the data based on different attributes, the decision tree model provided a clear visualization of the decision-making process.
Although slightly less accurate than logistic regression (achieving 60.25% accuracy), the decision tree model offered valuable insights into the most influential factors contributing to readmission risk.

## Results and Conclusion
#### Key Findings:
Based on initial Exploratory Data Analysis (EDA), as well as the results from the decision tree and logistic regression models, it is evident that patients already taking diabetes medication are at a higher risk of readmission.
Both models highlight several significant predictor variables, including inpatient/outpatient status, diabetes medication usage, emergency visits, length of hospital stay, and age (with higher age correlating with increased readmission risk).
However, due to the high number of missing values or lack of testing data for glucose diagnosis, A1c tests, and general diagnosis, it is challenging to definitively determine the necessity of readmission for diabetic patients.
