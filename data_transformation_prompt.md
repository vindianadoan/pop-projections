# Data Transformation Prompt for Population Projections Dashboard

## Task Overview
Transform existing population projection data into the specific format required by a Shiny dashboard application. The dashboard expects data in a very specific structure with exact column names and values.

## Required Output Format

### Data Frame Structure
The output must be a data frame with these **exact column names** and data types:

| Column Name | Data Type | Description | Required Values |
|-------------|-----------|-------------|-----------------|
| `geography_level` | character | Geographic hierarchy level | "national", "state", "sa4", "sa3", "gcc" |
| `geography_name` | character | Name of the geography | Actual geography names (e.g., "New South Wales", "Sydney Inner City") |
| `parent_geography` | character | Parent geography name | Parent geography or NA for national |
| `year` | integer | Projection year | Any year (e.g., 2020, 2021, 2022, etc.) |
| `age_group` | character | Age group label | "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+" |
| `sex` | character | Sex category | "Male", "Female" |
| `population` | numeric | Population count | Any positive number |
| `scenario` | character | Projection scenario | "Low", "Medium", "High" |

### Critical Requirements

1. **Exact Column Names**: Use the exact column names listed above - no variations allowed
2. **Exact Value Formats**: Use the exact values specified for categorical variables
3. **Complete Combinations**: Every combination of geography_level + geography_name + year + age_group + sex + scenario must have a row
4. **No Missing Values**: All columns must be complete (except parent_geography can be NA for national level)
5. **Data Types**: Ensure correct data types as specified

## Geography Hierarchy Rules

### National Level
- `geography_level` = "national"
- `geography_name` = "Australia" 
- `parent_geography` = NA

### State Level
- `geography_level` = "state"
- `geography_name` = State names (e.g., "New South Wales", "Victoria", "Queensland", "South Australia", "Western Australia", "Tasmania", "Northern Territory", "Australian Capital Territory")
- `parent_geography` = "Australia"

### SA4 Level
- `geography_level` = "sa4"
- `geography_name` = SA4 region names
- `parent_geography` = The state name that contains this SA4

### SA3 Level
- `geography_level` = "sa3"
- `geography_name` = SA3 region names
- `parent_geography` = The SA4 name that contains this SA3

### GCC Level
- `geography_level` = "gcc"
- `geography_name` = Greater Capital City names (e.g., "Greater Sydney", "Greater Melbourne", "Greater Brisbane", "Greater Adelaide", "Greater Perth", "Greater Hobart", "Greater Darwin", "Australian Capital Territory")
- `parent_geography` = The state name that contains this GCC

## Age Groups
Use these exact age group labels:
- "0-4", "5-9", "10-14", "15-19", "20-24", "25-24", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"

## Scenarios
Use these exact scenario names:
- "Low" (pessimistic projection)
- "Medium" (most likely projection) 
- "High" (optimistic projection)

## Data Transformation Steps

1. **Identify Source Data**: Locate your existing population projection data
2. **Map Geography Levels**: Determine which geographies belong to which level (national/state/sa4/sa3/gcc)
3. **Create Parent Relationships**: Establish parent-child relationships between geographies
4. **Standardize Age Groups**: Convert your age groups to the required format
5. **Standardize Sex Values**: Ensure sex values are exactly "Male" and "Female"
6. **Create Scenario Variations**: If you only have one scenario, create Low/Medium/High variations
7. **Generate All Combinations**: Create rows for every combination of geography + year + age_group + sex + scenario
8. **Validate Structure**: Ensure all required columns exist with correct names and types

## Example Output Structure

```r
# Example of correct output format:
data <- data.frame(
  geography_level = c("state", "state", "sa4", "sa4", "national"),
  geography_name = c("New South Wales", "Victoria", "Sydney - Inner West", "Melbourne - Inner", "Australia"),
  parent_geography = c("Australia", "Australia", "New South Wales", "Victoria", NA),
  year = c(2020, 2020, 2020, 2020, 2020),
  age_group = c("25-29", "25-29", "25-29", "25-29", "25-29"),
  sex = c("Male", "Male", "Male", "Male", "Male"),
  population = c(150000, 120000, 25000, 20000, 2000000),
  scenario = c("Medium", "Medium", "Medium", "Medium", "Medium"),
  stringsAsFactors = FALSE
)
```

## Validation Checklist

Before finalizing the data, verify:
- [ ] All 8 required columns are present with exact names
- [ ] All geography_level values are from the allowed set
- [ ] All age_group values are from the required set
- [ ] All sex values are exactly "Male" or "Female"
- [ ] All scenario values are exactly "Low", "Medium", or "High"
- [ ] Parent-child geography relationships are correct
- [ ] No missing values except parent_geography for national level
- [ ] Population values are numeric and positive
- [ ] Year values are integers
- [ ] All combinations of dimensions have rows

## Output Instructions

1. **Provide the transformed data** as a data frame in R format
2. **Include data validation summary** showing:
   - Total number of rows
   - Number of unique values in each column
   - Any data quality issues found
3. **Provide code to save the data** as an RDS file: `saveRDS(data, "data/population_projections.rds")`
4. **Include a sample** of the first 10 rows to verify structure

## Notes
- The dashboard application expects this exact structure and will fail if column names or values don't match
- Missing combinations will result in empty plots/tables
- Incorrect parent relationships will break the filtering functionality
- The application includes comprehensive logging to help validate the data structure

Transform your existing population projection data to match this exact specification.
