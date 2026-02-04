# Process 2: Economics

## Overview

The Office of the Chief Actuary uses the Economic process to project OASDI employment and earnings-related variables, such as the average wage index and the effective taxable payroll. The Economic process receives input data from the Demography process and provides output data to the Beneficiaries and the Trust Fund Operations & Actuarial Status processes.

## Subprocesses

The Economic Process is composed of five subprocesses:

| Subprocess | Description | Output Frequency | Projection Period |
|------------|-------------|------------------|-------------------|
| **U.S. EMPLOYMENT** | Projects U.S. employment data | Quarterly | Full (75 years) |
| **U.S. EARNINGS** | Projects U.S. earnings data | Quarterly | Full (75 years) |
| **COVERED EMPLOYMENT AND EARNINGS** | Converts employment and earnings variables to OASDI covered concepts | Calendar Year | Full (75 years) |
| **TAXABLE PAYROLL** | Converts OASDI covered earnings to taxable concepts for payroll tax income estimation | Calendar Year | Full (75 years) |
| **REVENUES** | Converts taxable concepts into projected OASDI wage tax liabilities (WTL) and self-employment tax liabilities (SEL) | Quarterly, Fiscal Year, Calendar Year | Short-range (10 years) |

## Data Flow

```
┌─────────────┐     ┌─────────────────┐     ┌──────────────────────────────────────┐
│ Demography  │────▶│    Economics    │────▶│ Beneficiaries & Trust Fund Operations│
│   Process   │     │     Process     │     │      & Actuarial Status              │
└─────────────┘     └─────────────────┘     └──────────────────────────────────────┘
```

### Process Flow Within Economics

```
U.S. EMPLOYMENT ──┐
                  ├──▶ COVERED EMPLOYMENT ──▶ TAXABLE PAYROLL ──▶ REVENUES
U.S. EARNINGS ────┘    AND EARNINGS
```

## Output Details

- **U.S. EMPLOYMENT** and **U.S. EARNINGS**: Produce output by quarter
- **COVERED EMPLOYMENT AND EARNINGS** and **TAXABLE PAYROLL**: Calendar year amounts
- **REVENUES**: Produces:
  - Wages paid to employees during a quarter
  - Tax liabilities owed on those wages
  - Taxes collected by quarter, fiscal year, and calendar year

## Appendices

Two appendices accompany this documentation:

1. **Appendix 2-1**: Provides details for most of the equations given in the Economic process descriptions
   - See: `equations_abbreviations/2025_LR_Model_Documentation_economics_equations_*.pdf`

2. **Appendix 2-2**: Provides a listing with explanations of abbreviations and labels used in this documentation
   - See: `equations_abbreviations/2025_LR_Model_Documentation_economics_equations_Abbreviations.pdf`

## Related Documentation

- [U.S. Employment Subprocess](2025_LR_Model_Documentation_Economics_1_USEmployment.pdf)
- [U.S. Earnings Subprocess](2025_LR_Model_Documentation_Economics_2_USEarnings.pdf)
- [OASDI Covered Employment and Earnings](2025_LR_Model_Documentation_Economics_3_OASDICovEmpEarn.pdf)
- [Effective Taxable Payroll](2025_LR_Model_Documentation_Economics_4_EffectiveTaxablePayroll.pdf)
- [Revenues](2025_LR_Model_Documentation_Economics_5_Revenues.pdf)
