# Codebook: Chrononutrition Patterns Dataset

## Dataset Information

- **File**: `chrono_patterns_n459.csv`
- **Sample Size**: n = 459 university students
- **Complete Cases for Clustering**: n = 388 (participants with all three meal times)
- **Collection Period**: 2023-2024
- **Location**: Universidad de Guadalajara, Mexico

---

## Variables

### Identification

| Variable | Description | Type | Values |
|----------|-------------|------|--------|
| `id` | Anonymous participant identifier | Integer | 1-459 |

---

### Demographic Variables (6)

| Variable | Description | Type | Values | Missing |
|----------|-------------|------|--------|---------|
| `age` | Age in years (rounded to integers) | Numeric | 18-35 | 0 |
| `sex` | Biological sex | Categorical | `Female`, `Male` | 0 |
| `academic_program` | Field of study | Categorical | `Nutrition`, `Medicine`, `Psychology`, `Other` | 0 |
| `semester_level` | Academic progress | Categorical | `1-2_years`, `3-5_years` | 0 |
| `employment` | Currently employed | Categorical | `Yes`, `No` | 0 |
| `marital_status` | Relationship status | Categorical | `Single`, `In_relationship` | 0 |

---

### Anthropometric Variables (2)

| Variable | Description | Type | Unit | Range | Missing |
|----------|-------------|------|------|-------|---------|
| `bmi` | Body Mass Index | Numeric | kg/m² | 15-45 | 0 |
| `waist_circumference` | Waist circumference | Numeric | cm | 50-130 | 0 |

---

### Food Quality (1)

| Variable | Description | Type | Values | Missing |
|----------|-------------|------|--------|---------|
| `food_intake_quality` | Dietary quality from Mini-ECCA v.2 | Categorical | `Healthy`, `Needs_improvement`, `Unhealthy` | 0 |

---

### Chronotype (1)

| Variable | Description | Type | Values | Missing |
|----------|-------------|------|--------|---------|
| `chronotype` | Circadian preference (MEQ-based) | Categorical | `Morning`, `Intermediate`, `Evening` | 0 |

---

### Chrononutrition Variables (7)

All time variables are expressed in **decimal hours** (e.g., 8.5 = 8:30 AM).

| Variable | Description | Type | Unit | Range | Missing |
|----------|-------------|------|------|-------|---------|
| `breakfast_time` | Habitual breakfast time | Numeric | hours | 5.0-12.0 | 71 |
| `lunch_time` | Habitual lunch time | Numeric | hours | 11.0-18.0 | 0 |
| `dinner_time` | Habitual dinner time | Numeric | hours | 17.0-24.0 | 0 |
| `eating_midpoint` | Midpoint of eating window | Numeric | hours | 10.0-20.0 | 71 |
| `eating_window` | Duration of eating window | Numeric | hours | 6.0-18.0 | 71 |
| `overnight_fasting` | Overnight fasting duration | Numeric | hours | 6.0-18.0 | 71 |
| `breakfast_lunch_interval` | Time between breakfast and lunch | Numeric | hours | 1.0-10.0 | 71 |
| `lunch_dinner_interval` | Time between lunch and dinner | Numeric | hours | 2.0-12.0 | 0 |

**Note**: Missing values (71 participants) correspond to individuals who habitually skip breakfast.

---

## Derived Variables for Analysis

The following subsets are used in the analysis:

### Complete Cases (n = 388)
Participants with non-missing values for `breakfast_time`, `lunch_time`, and `dinner_time`.

### Clustering Variables
The clustering analysis uses **3 primary variables**:
1. `breakfast_time`
2. `lunch_time`
3. `dinner_time`

All variables are **z-score standardized** before clustering.

---

## Data Quality Notes

1. **Anonymization**: All direct identifiers have been removed. Indirect identifiers (age, times) have been rounded.
2. **Time Rounding**: Meal times are rounded to the nearest 5 minutes (0.083 hours).
3. **Outlier Review**: Extreme values have been reviewed to ensure they do not compromise anonymity.
4. **Missing Data Pattern**: Missing chrononutrition values are structurally missing (meal skipping), not random.

---

## Variable Coding

### Sex
- `Female` = 1
- `Male` = 2

### Academic Program
- `Nutrition` = 1
- `Medicine` = 2
- `Psychology` = 3
- `Other` = 4

### Semester Level
- `1-2_years` = 1 (Early academic stage)
- `3-5_years` = 2 (Advanced academic stage)

### Employment
- `No` = 0
- `Yes` = 1

### Marital Status
- `Single` = 1
- `In_relationship` = 2

### Food Intake Quality
- `Healthy` = 1
- `Needs_improvement` = 2
- `Unhealthy` = 3

### Chronotype
- `Morning` = 1
- `Intermediate` = 2
- `Evening` = 3

---

## References

For detailed methodology, see the associated paper:

> Mora-Almanza, J. G., Betancourt-Núñez, A., Nava-Amante, P. A., Bernal-Orozco, M. F., Díaz-López, A., Martínez, J. A., & Vizmanos, B. (2025). Traditional and non-traditional clustering techniques for identifying chrononutrition patterns. *Manuscript in preparation*.
