# NYC School Bus Delay Analytics

**Leveraging Data to Uncover Delay Insights and Draft Recommendations to Improve Network Reliability**

[![R](https://img.shields.io/badge/R-276DC3?style=flat&logo=r&logoColor=white)](https://www.r-project.org/)
[![Analytics](https://img.shields.io/badge/Analytics-Strategy-blue)](https://github.com/yourusername/nyc-bus-analytics)
[![Data](https://img.shields.io/badge/Data-NYC_Open_Data-green)](https://data.cityofnewyork.us/)

**Team 6:** Liu Chang, Xu Anlan, Chen Zixuan, Li Ang, Arif Farhan Bukhori, Simon Eppig  
**Course:** AN6003 – Analytics Strategy | AY 2025/2026  
**Institution:** Nanyang Business School, Nanyang Technological University

---

## 📊 Executive Summary

This project addresses New York City's persistent school bus delay problem through comprehensive data analytics. With **150,000 children** relying on school buses—including **66,000 with disabilities or in temporary housing**—delays threaten educational access for the city's most vulnerable students.

**Key Findings:**
- Average delay of **39 minutes** (75% of delays cause missed class time)
- **Bus operator** and **route number** are the only significant delay factors
- **6 problematic operators** identified requiring immediate action
- **Multiple congested routes** need re-routing or schedule adjustments

**Impact:** Data-driven recommendations to reduce delays and ensure equitable education access for underprivileged students.

---

## 🎯 Business Problem

**Problem Statement:** What are the main drivers of school bus delays in New York City and which measures can the city take to improve bus network reliability?

**Context:**
- 80,000 delays reported in 2023/24 school year
- 20% increase in bus complaints compared to previous year
- Persistent reliability issues affecting educational outcomes

---

## 📁 Project Structure

```
nyc-bus-analytics/
│
├── Team 6 Script.R                    # Main R analysis script
├── Team 6 Project Report.docx         # Comprehensive project report
├── Presentation.pdf                   # Executive presentation slides
│
├── data/
│   ├── bus_original.csv               # Raw NYC Open Data
│   ├── BusDelay_clean12.csv          # Intermediate cleaned data
│   ├── BusDelay_clean19.csv          # Final cleaned dataset
│   └── bus_new.xlsx                   # Processed data with features
│
├── outputs/
│   └── bus_company_reason_proportion.csv  # Operational error index
│
└── README.md
```

---

## 🚀 Getting Started

### Prerequisites

```r
# Required R packages
install.packages(c(
  "dplyr",
  "stringr",
  "lubridate",
  "ggplot2",
  "tidyr",
  "tidyverse",
  "data.table",
  "readxl",
  "reticulate"  # For Python integration
))
```

### Data Source

Dataset: [NYC Open Data - Bus Breakdown and Delays](https://data.cityofnewyork.us/Transportation/Bus-Breakdown-and-Delays/ez4e-fazm)
- **Records:** ~750,000 delay incidents (2015-2025)
- **Variables:** 21 original columns
- **Format:** CSV

### Running the Analysis

```r
# 1. Update file paths in the script (lines 34, 293, 736, etc.)
df <- read.csv("YOUR_PATH/bus_original.csv")

# 2. Run analysis sections sequentially:
source("Team 6 Script.R")

# Section 1: Setup and Data Import (lines 30-275)
# Section 2: Data Description (lines 282-714)
# Section 3: Model Analysis (lines 723-1294)
```

---

## 🔍 Methodology

### 1. Data Cleaning & Preparation

**Challenge:** Severe data quality issues due to manual entry
- Inconsistent delay time formats (e.g., "15m", "1:30h", "20-30m")
- Duplicate company names with spelling variations
- Missing values and format inconsistencies

**Solution:**
- Standardized 8+ time formats into unified minute values
- Normalized company names using 5-character prefix mapping
- Removed ~170,000 invalid/incomplete records

**Final Dataset:** 580,073 records, 16 variables

### 2. Descriptive Analytics

**Delay Distribution:**
- **Minimum:** 8 minutes
- **Median:** 38 minutes
- **Mean:** 39 minutes
- **Maximum:** 76 minutes

**Key Insight:** 75% of delays (>23 min) cause students to miss class time

**Top Delay Causes:**
1. Heavy Traffic (177k incidents)
2. Operator Issues (Problem Run, Mechanical, etc.)
3. Weather Conditions

### 3. Statistical Modeling

#### A. CART Analysis
```r
# Variable Importance (Scale 0-100)
Bus Operator:  57.23  **
Route Number:  39.66  *
Natural Factors: 2.16
Hours:          0.71
Borough:        0.24
Day:            0.00
```

#### B. Random Forest
```r
# Feature Importance (Scale 0-1)
Bus Operator:  0.53  **
Route Number:  0.41  *
Hours:         0.04
Borough:       0.02
Weekend:       0.00
```

**Conclusion:** Only bus operator and route number significantly determine delays.

#### C. Bootstrap Analysis
- Identified **6 problematic operators** with highest delay ranges
- Categorized by failure types:
  - **Mechanical Issues:** First Steps, Jofaz, Boro Transit, SNT Bus
  - **Operational Issues:** Leesel Transp Corp
  - **Mixed Issues:** Quality Transportation Co.

#### D. Operational Performance Index (OPI)

**Formula:**
```
OPI = (Efficiency × 40%) + (Responsiveness × 30%) + 
      (Reliability × 20%) + (Compliance × 10%)
```

**Components:**
- **Efficiency:** Adjusted delay coefficient (lower is better)
- **Responsiveness:** Time to inform city (lower is better)
- **Reliability:** Severe delay odds ratio (OR < 1 is better)
- **Compliance:** Notification success odds ratio (OR > 1 is better)

**Results (Scale 0-100):**
| Rank | Company | OPI Score | Decision |
|------|---------|-----------|----------|
| 1 | LEESEL TRANSP CORP | 83.57 | ⚠️ Warning + Support |
| 2 | SNT BUS INC | 73.00 | ⚠️ Warning + Support |
| 3 | BORO TRANSIT INC | 72.36 | ⚠️ Warning + Support |
| 4 | QUALITY TRANSPORTATION | 67.59 | ❌ No Contract Renewal |
| 5 | FIRST STEPS TRANSP | 62.76 | ❌ No Contract Renewal |
| 6 | JOFAZ TRANSPORTATION | 60.72 | ❌ No Contract Renewal |

**Decision Threshold:** OPI < 70 = No contract renewal

#### E. Route Analysis

**Bootstrap Results:** Routes with high external delays (traffic, time of day)
- Identified 18 critical routes across 6 operators
- Examples: Q863, M136, M166, Y508, K276, etc.

**Regression Results:** Routes with systematic delays (design issues)
- Identified 18 problematic routes needing re-routing
- Examples: M130, K9216, K2245, M9175, etc.

---

## 🎯 Recommendations

### 1. Operator Management

**Immediate Actions:**
- **Terminate 3 contracts:** Jofaz, First Steps, Quality Transportation (OPI < 70)
- **Issue warnings to 3 operators:** Leesel, SNT Bus, Boro Transit (OPI 70-84)
- **Offer support programs:** Collaborate on resolving mechanical/operational issues

**Rationale:** Balanced approach minimizes network disruption while addressing poor performance

### 2. Route Optimization

**Short-term:**
- Analyze 36 identified problematic routes for congestion patterns
- Re-route buses around high-traffic segments where feasible
- Adjust schedules to allow extra time on unavoidable congested routes

**Long-term:**
- Integrate real-time traffic data into routing algorithms
- Coordinate with city traffic management for school bus priority

### 3. Data Quality Improvements

**System Enhancements:**
- Implement input format restrictions in reporting system
- Use dropdown menus for categorical variables (company names, reasons)
- Enforce numeric-only inputs for delay times
- Add real-time validation at data entry point

**Expected Impact:** Reduce cleaning overhead and enable faster analysis

---

## 📊 Technical Highlights

### Advanced R Techniques

**Data Cleaning:**
```r
# Complex regex for time parsing
mask_hm_combo <- grepl("\\d", df$How_Long_Delayed) &
                 grepl("h", df$How_Long_Delayed) &
                 grepl("m", df$How_Long_Delayed) &
                 !grepl("-|/|:", df$How_Long_Delayed)
```

**Company Name Normalization:**
```r
prefix <- substr(df_clean$Bus_company_name2, 1, 5)
mapping <- tapply(df_clean$Bus_company_name2, prefix, function(x) x[1])
df_clean$Bus_company_name2 <- mapping[prefix]
```

**Bootstrap Resampling:**
```python
def bootstrap_analysis_py(data, n_iter=1000):
    for company in top_companies:
        delay_stats = [np.mean(company_data.sample(
            frac=1, replace=True)['Delay_clean']) 
            for _ in range(n_iter)]
        delay_ci = np.percentile(delay_stats, [2.5, 97.5])
```

### Models Implemented

1. **CART (Classification and Regression Trees)** - Variable importance
2. **Random Forest** - Feature importance with cross-validation
3. **Bootstrap** - 95% confidence intervals for delay estimates
4. **Linear Regression** - Efficiency and responsiveness metrics
5. **Logistic Regression** - Reliability and compliance odds ratios

---

## 📊 Results & Impact

### Quantitative Outcomes

| Metric | Before Analysis | After Recommendations |
|--------|----------------|----------------------|
| Problematic Operators Identified | Unknown | 6 (with action plan) |
| Critical Routes Flagged | Unknown | 36 (across 6 operators) |
| Data Quality Issues | Severe | Mitigation strategy provided |
| Decision Framework | None | OPI-based evaluation system |

### Qualitative Impact

**Educational Access:**
- Reduced delays mean fewer missed classes for disadvantaged students
- 66,000 children with disabilities/temporary housing benefit most
- Improved reliability supports educational equity goals

**Operational Efficiency:**
- Data-driven operator selection and management
- Evidence-based route optimization
- Systematic performance monitoring framework

**Policy Implications:**
- Demonstrates value of open data for public good
- Replicable methodology for other cities
- Foundation for ongoing delay reduction efforts

---

## 🛠️ Technologies Used

- **R 4.x** - Primary analysis language
- **Python 3.x** - Bootstrap analysis via reticulate
- **Libraries:**
  - Data Manipulation: dplyr, tidyr, stringr
  - Visualization: ggplot2, data.table
  - Modeling: scikit-learn (via reticulate)
  - Statistical: boot, stats
- **Tools:**
  - RStudio
  - Microsoft Excel (data inspection)
  - NYC Open Data API

---

## 📝 Documentation

### Report Structure

**Section 1: Business Situation (Pages 1-2)**
- Problem definition and context
- Stakeholder impact analysis

**Section 2: Initial Findings (Pages 3-9)**
- Data cleaning methodology
- Descriptive analytics results
- Delay cause analysis

**Section 3: Advanced Analytics (Pages 10-15)**
- CART and Random Forest models
- Bootstrap operator analysis
- OPI regression framework
- Route identification

**Section 4: Recommendations (Pages 16-18)**
- Operator management strategy
- Route optimization plan
- Implementation roadmap

---

## 🎓 Academic Context

**Course:** AN6003 – Analytics Strategy  
**Program:** Business Analytics, Nanyang Business School  
**Focus:** Applying advanced analytics to real-world public sector challenges

**Learning Objectives Demonstrated:**
- Data quality assessment and cleaning
- Descriptive and diagnostic analytics
- Predictive modeling with multiple techniques
- Prescriptive recommendations with implementation plans
- Stakeholder communication and visualization


---

## 📚 References

1. New York City. (2025). *Bus Breakdown and Delays.* NYC Open Data. [Link](https://data.cityofnewyork.us/Transportation/Bus-Breakdown-and-Delays/ez4e-fazm)

2. New York City. (n.d.). *NYCPS Data at a Glance.* New York City Public Schools. [Link](https://www.schools.nyc.gov/about-us/reports/nycps-data-at-a-glance)

3. Parra, D. (2025). *A New School Year is Here—And So Are School Bus Delays.* CityLimits. [Link](https://citylimits.org/a-new-school-year-is-here-and-so-are-school-bus-delays/)

---

## 📧 Contact

**Team 6 - NYC School Bus Analytics**  
Nanyang Business School  
Nanyang Technological University, Singapore

For inquiries about this project:
- Open an issue in this repository
- Contact course instructor: AN6003 Analytics Strategy

---

## 📄 License

This project is submitted as academic coursework for AN6003 at Nanyang Technological University. The analysis uses publicly available NYC Open Data.

**Data License:** NYC Open Data Terms of Use  
**Code License:** Educational use only

---

## 🙏 Acknowledgments

- **NYC Open Data Platform** for providing comprehensive transit data
- **Advocates for Children of New York** for highlighting the importance of school bus reliability
- **Course Instructors** for guidance on analytics strategy and methodology
- **NYC Department of Education** for operating the school bus network serving 150,000 students

---

## 🔄 Future Work

**Potential Extensions:**
1. **Real-time Prediction:** Deploy machine learning model for delay forecasting
2. **Geographic Analysis:** GIS mapping of congested areas and route overlays
3. **Longitudinal Study:** Track delay reduction after implementing recommendations
4. **Cost-Benefit Analysis:** Quantify financial impact of operator changes
5. **Student Outcomes:** Correlate delay reduction with attendance/performance metrics

**Scalability:**
- Adapt methodology for other cities with similar transit challenges
- Develop automated dashboard for continuous monitoring
- Create API for real-time delay alerts to schools/parents

---

*Last Updated: January 2025*

**Status:** ✅ Analysis Complete | 📊 Recommendations Delivered | 🎯 Awaiting Implementation
