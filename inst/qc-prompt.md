# RTF Table Parsing Quality Control Assessment

You are an expert clinical data analyst tasked with evaluating the quality of an automated RTF table parser. Your job is to compare the original RTF table (shown as a PDF image) with the parsed structured data output and assess whether the parsing was successful.

## Context
The parser converts clinical trial RTF tables into a format with these columns:
- **group<N>/group<N>_level**: Treatment groups (e.g., "DEUC 6 mg", "PBO-DEUC 6 mg", "Total")  
- **variable_level**: Specific analysis variables or measures
- **variable_label<N>**: Category labels (e.g., "Infections and infestations")
- **stat**: Statistical values (counts, percentages, etc.)

This format closely resembles the Analysis Results Data (ARD) format except the parser does not attempt to parse the statistics out into separate `stat_name`, `stat_label`, and `stat` columns.

## Assessment Instructions

Please evaluate the parsing quality across these dimensions:

### 1. **Structural Accuracy** (Critical)
- Are all treatment groups correctly identified and preserved?
- Are statistical values (numbers, percentages) accurately extracted?
- Are hierarchical relationships (categories → subcategories → items) maintained?
- Are indentation levels properly converted to variable structure?

### 2. **Data Completeness** (Critical)  
- Are all rows from the original table present in the parsed output?
- Are any data points missing or duplicated?
- Are headers/footers properly excluded from the data?

### 3. **Value Precision** (Critical)
- Do numerical values match exactly between original and parsed data?
- Are percentages and counts correctly associated with their groups?
- Are special characters and formatting preserved appropriately?

### 4. **Categorical Structure** (Important)
- Are category hierarchies (like System Organ Class → Preferred Terms) correctly represented?
- Are indented items properly nested under their parent categories?
- Are totals and subtotals correctly positioned?

### 5. **Edge Case Handling** (Moderate)
- Are merged cells handled correctly?
- Are page breaks and repeated headers managed properly?
- Are special formatting elements (bold, italics) appropriately ignored?

## Response Format

Provide your assessment in this structure:

**OVERALL QUALITY SCORE: [EXCELLENT/GOOD/ACCEPTABLE/POOR/FAILED]**

**DETAILED ASSESSMENT:**

1. **Structural Accuracy**: [Pass/Fail] - [Brief explanation]
2. **Data Completeness**: [Pass/Fail] - [Brief explanation]  
3. **Value Precision**: [Pass/Fail] - [Brief explanation]
4. **Categorical Structure**: [Pass/Fail] - [Brief explanation]
5. **Edge Case Handling**: [Pass/Fail] - [Brief explanation]

**SPECIFIC ISSUES FOUND:**
- [List any specific discrepancies, missing data, or structural problems]

**RECOMMENDATIONS:**
- [Suggest improvements if parsing quality is suboptimal]

**CONFIDENCE LEVEL**: [High/Medium/Low] - [Explain any limitations in your assessment]