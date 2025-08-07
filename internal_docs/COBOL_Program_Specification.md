COBOL Program Specification: ACCVAL01
=====================================

**Program ID:** ACCVAL01**Version:** 3.2**Last Updated:** January 22, 2024**Author:** Banking Systems Team**Business Owner:** Treasury Operations

Program Overview
----------------

### Business Purpose

ACCVAL01 performs comprehensive account validation and overdraft limit calculations for all customer transactions. This program is called by transaction processing systems before any debit operation to ensure account integrity and regulatory compliance.

**Processing Volume:** 850,000 validations per day**Peak TPS:** 120 transactions per second**Critical Path:** Yes - All debits blocked until validation completes

### Business Rules Implemented

#### Rule 1: Account Status Validation

*   **Rule ID:** BR-001
    
*   **Description:** Only ACTIVE accounts can process debit transactions
    
*   **Implementation:** Check ACCT-STATUS-CD field
    
*   **Valid Values:** 'A' (Active), 'S' (Suspended), 'C' (Closed), 'F' (Frozen)
    
*   **Business Impact:** Prevents transactions on inactive accounts
    

#### Rule 2: Overdraft Limit Calculation

*   **Rule ID:** BR-002
    
*   **Description:** Calculate available overdraft based on customer tier and payment history
    
*   Available\_Overdraft = Base\_Limit + (Credit\_Score\_Bonus \* Tier\_Multiplier) - Current\_Overdraft\_Usage
    
*   **Tier Multipliers:**
    
    *   Premium (P): 2.5x
        
    *   Gold (G): 1.8x
        
    *   Silver (S): 1.2x
        
    *   Standard (N): 1.0x
        

#### Rule 3: Daily Transaction Limits

*   **Rule ID:** BR-003
    
*   **Description:** Enforce daily debit limits based on account type
    
*   **Limits:**
    
    *   Checking: $5,000 per day
        
    *   Savings: $1,000 per day (Regulation D compliance)
        
    *   Business: $25,000 per day
        
    *   Premium: $50,000 per day
        

#### Rule 4: Regulatory Compliance Checks

*   **Rule ID:** BR-004
    
*   **Description:** BSA/AML compliance for large transactions
    
*   **Trigger Amount:** $10,000 single transaction OR $10,000 cumulative daily
    
*   **Action:** Flag for manual review, continue processing
    
*   **Compliance Officer:** Auto-notification required
    

Data Structures
---------------

### Input Parameters (COMMAREA)

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   01  INPUT-COMMAREA.      05  COMM-CUSTOMER-ID        PIC X(10).      05  COMM-ACCOUNT-NUMBER     PIC X(12).      05  COMM-TRANSACTION-AMOUNT PIC S9(11)V99 COMP-3.      05  COMM-TRANSACTION-TYPE   PIC X(4).      05  COMM-BRANCH-CODE        PIC X(4).      05  COMM-TELLER-ID          PIC X(8).   `

### Working Storage Key Fields

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   01  ACCOUNT-RECORD.      05  ACCT-CUSTOMER-ID        PIC X(10).      05  ACCT-NUMBER             PIC X(12).      05  ACCT-TYPE-CODE          PIC X(1).          88  CHECKING-ACCOUNT    VALUE 'C'.          88  SAVINGS-ACCOUNT     VALUE 'S'.           88  BUSINESS-ACCOUNT    VALUE 'B'.          88  PREMIUM-ACCOUNT     VALUE 'P'.      05  ACCT-STATUS-CD          PIC X(1).          88  ACCOUNT-ACTIVE      VALUE 'A'.          88  ACCOUNT-SUSPENDED   VALUE 'S'.          88  ACCOUNT-CLOSED      VALUE 'C'.          88  ACCOUNT-FROZEN      VALUE 'F'.      05  ACCT-CURRENT-BALANCE    PIC S9(11)V99 COMP-3.      05  ACCT-AVAILABLE-BALANCE  PIC S9(11)V99 COMP-3.      05  ACCT-OVERDRAFT-LIMIT    PIC S9(9)V99 COMP-3.      05  ACCT-DAILY-DEBIT-TOTAL  PIC S9(9)V99 COMP-3.      05  ACCT-LAST-ACTIVITY-DATE PIC X(10).  01  CUSTOMER-PROFILE.      05  CUST-ID                 PIC X(10).      05  CUST-TIER-CODE          PIC X(1).          88  TIER-PREMIUM        VALUE 'P'.          88  TIER-GOLD           VALUE 'G'.          88  TIER-SILVER         VALUE 'S'.          88  TIER-STANDARD       VALUE 'N'.      05  CUST-CREDIT-SCORE       PIC 9(3).      05  CUST-RELATIONSHIP-START PIC X(10).      05  CUST-PAYMENT-HISTORY    PIC 9(3)V99.   `

### Database Tables Accessed

#### Primary Tables

1.  **CUSTOMER\_ACCOUNTS** (DB2)
    
    *   Primary Key: CUSTOMER\_ID + ACCOUNT\_NUMBER
        
    *   Indexes: ACCT\_STATUS\_CD, ACCT\_TYPE\_CODE
        
    *   Row Count: ~15 million
        
2.  **CUSTOMER\_PROFILE** (DB2)
    
    *   Primary Key: CUSTOMER\_ID
        
    *   Indexes: CUST\_TIER\_CODE, CUST\_CREDIT\_SCORE
        
    *   Row Count: ~8 million
        
3.  **DAILY\_TRANSACTION\_SUMMARY** (DB2)
    
    *   Primary Key: ACCOUNT\_NUMBER + TRANSACTION\_DATE
        
    *   Purpose: Track daily debit totals
        
    *   Retention: 90 days rolling
        

#### Copybooks Used

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   COPY CUSTACCT.     // Customer account structure  COPY CUSTPROF.     // Customer profile structure    COPY TRANTYPE.     // Transaction type definitions  COPY ERRORCDS.     // Standard error codes  COPY SQLCODES.     // DB2 SQL return codes   `

Program Logic Flow
------------------

### Main Processing Steps

#### Step 1: Input Validation

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   VALIDATE-INPUT-SECTION.      IF COMM-CUSTOMER-ID = SPACES OR LOW-VALUES          MOVE 'INVALID CUSTOMER ID' TO ERROR-MESSAGE          MOVE '001' TO RETURN-CODE          GO TO EXIT-PROGRAM      END-IF.      IF COMM-ACCOUNT-NUMBER = SPACES OR LOW-VALUES            MOVE 'INVALID ACCOUNT NUMBER' TO ERROR-MESSAGE          MOVE '002' TO RETURN-CODE          GO TO EXIT-PROGRAM      END-IF.      IF COMM-TRANSACTION-AMOUNT <= ZERO          MOVE 'INVALID TRANSACTION AMOUNT' TO ERROR-MESSAGE          MOVE '003' TO RETURN-CODE          GO TO EXIT-PROGRAM      END-IF.   `

#### Step 2: Account Lookup and Status Check

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   CHECK-ACCOUNT-STATUS-SECTION.      EXEC SQL          SELECT ACCT_TYPE_CODE, ACCT_STATUS_CD, ACCT_CURRENT_BALANCE,                 ACCT_AVAILABLE_BALANCE, ACCT_OVERDRAFT_LIMIT          INTO :ACCT-TYPE-CODE, :ACCT-STATUS-CD, :ACCT-CURRENT-BALANCE,               :ACCT-AVAILABLE-BALANCE, :ACCT-OVERDRAFT-LIMIT          FROM CUSTOMER_ACCOUNTS            WHERE CUSTOMER_ID = :COMM-CUSTOMER-ID            AND ACCOUNT_NUMBER = :COMM-ACCOUNT-NUMBER      END-EXEC.      IF SQLCODE NOT = 0          IF SQLCODE = 100              MOVE 'ACCOUNT NOT FOUND' TO ERROR-MESSAGE              MOVE '404' TO RETURN-CODE          ELSE              MOVE 'DATABASE ERROR' TO ERROR-MESSAGE                MOVE '500' TO RETURN-CODE          END-IF          GO TO EXIT-PROGRAM      END-IF.      IF NOT ACCOUNT-ACTIVE          EVALUATE ACCT-STATUS-CD              WHEN 'S' MOVE 'ACCOUNT SUSPENDED' TO ERROR-MESSAGE              WHEN 'C' MOVE 'ACCOUNT CLOSED' TO ERROR-MESSAGE                WHEN 'F' MOVE 'ACCOUNT FROZEN' TO ERROR-MESSAGE              WHEN OTHER MOVE 'INVALID ACCOUNT STATUS' TO ERROR-MESSAGE          END-EVALUATE          MOVE '403' TO RETURN-CODE          GO TO EXIT-PROGRAM      END-IF.   `

#### Step 3: Overdraft Calculation

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   CALCULATE-OVERDRAFT-SECTION.      // Get customer tier for overdraft calculation      EXEC SQL          SELECT CUST_TIER_CODE, CUST_CREDIT_SCORE, CUST_PAYMENT_HISTORY          INTO :CUST-TIER-CODE, :CUST-CREDIT-SCORE, :CUST-PAYMENT-HISTORY          FROM CUSTOMER_PROFILE          WHERE CUSTOMER_ID = :COMM-CUSTOMER-ID      END-EXEC.      // Calculate credit score bonus (max 500 points)      COMPUTE WS-CREDIT-BONUS = (CUST-CREDIT-SCORE - 600) * 10.      IF WS-CREDIT-BONUS < 0          MOVE 0 TO WS-CREDIT-BONUS      END-IF.      IF WS-CREDIT-BONUS > 500            MOVE 500 TO WS-CREDIT-BONUS      END-IF.      // Apply tier multiplier      EVALUATE CUST-TIER-CODE          WHEN 'P' COMPUTE WS-TIER-MULTIPLIER = 2.5          WHEN 'G' COMPUTE WS-TIER-MULTIPLIER = 1.8          WHEN 'S' COMPUTE WS-TIER-MULTIPLIER = 1.2            WHEN 'N' COMPUTE WS-TIER-MULTIPLIER = 1.0          WHEN OTHER COMPUTE WS-TIER-MULTIPLIER = 1.0      END-EVALUATE.      // Final overdraft calculation      COMPUTE WS-AVAILABLE-OVERDRAFT =           ACCT-OVERDRAFT-LIMIT +           (WS-CREDIT-BONUS * WS-TIER-MULTIPLIER) -          (ACCT-CURRENT-BALANCE * -1).   `

#### Step 4: Daily Limit Enforcement

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   CHECK-DAILY-LIMITS-SECTION.      // Get today's debit total      EXEC SQL          SELECT SUM(TRANSACTION_AMOUNT)          INTO :WS-DAILY-TOTAL          FROM DAILY_TRANSACTION_SUMMARY          WHERE ACCOUNT_NUMBER = :COMM-ACCOUNT-NUMBER            AND TRANSACTION_DATE = CURRENT DATE            AND TRANSACTION_TYPE = 'DEBIT'      END-EXEC.      // Set daily limit based on account type      EVALUATE ACCT-TYPE-CODE          WHEN 'C' MOVE 5000.00 TO WS-DAILY-LIMIT          WHEN 'S' MOVE 1000.00 TO WS-DAILY-LIMIT          WHEN 'B' MOVE 25000.00 TO WS-DAILY-LIMIT          WHEN 'P' MOVE 50000.00 TO WS-DAILY-LIMIT      END-EVALUATE.      COMPUTE WS-REMAINING-DAILY-LIMIT =           WS-DAILY-LIMIT - WS-DAILY-TOTAL - COMM-TRANSACTION-AMOUNT.      IF WS-REMAINING-DAILY-LIMIT < 0          MOVE 'DAILY LIMIT EXCEEDED' TO ERROR-MESSAGE          MOVE '429' TO RETURN-CODE          GO TO EXIT-PROGRAM      END-IF.   `

### Output Parameters

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   01  OUTPUT-COMMAREA.      05  OUT-RETURN-CODE         PIC X(3).          88  VALIDATION-PASSED   VALUE '000'.          88  VALIDATION-FAILED   VALUE '001' THRU '999'.      05  OUT-ERROR-MESSAGE       PIC X(50).      05  OUT-AVAILABLE-BALANCE   PIC S9(11)V99 COMP-3.      05  OUT-OVERDRAFT-AVAILABLE PIC S9(9)V99 COMP-3.      05  OUT-DAILY-LIMIT-REMAIN  PIC S9(9)V99 COMP-3.      05  OUT-COMPLIANCE-FLAG     PIC X(1).          88  BSA-REVIEW-REQUIRED VALUE 'Y'.          88  NO-REVIEW-NEEDED    VALUE 'N'.   `

Error Handling
--------------

### Standard Error Codes

*   **000:** Validation successful
    
*   **001:** Invalid customer ID
    
*   **002:** Invalid account number
    
*   **003:** Invalid transaction amount
    
*   **404:** Account not found
    
*   **403:** Account status prevents transaction
    
*   **429:** Daily limit exceeded
    
*   **500:** Database error
    
*   **501:** Calculation overflow
    
*   **999:** Unknown system error
    

### Performance Requirements

*   **Response Time:** < 150ms for 95% of requests
    
*   **Availability:** 99.95% during business hours
    
*   **Concurrency:** Support 200 concurrent executions
    
*   **Resource Usage:** < 64KB working storage per execution
    

Testing Data
------------

### Valid Test Scenarios

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   Customer: CU00123456, Account: ACC987654321  - Active checking account with $1,500 balance  - Premium tier customer (credit score: 750)  - Overdraft limit: $1,000  - Expected overdraft available: $2,500   `

### Error Test Scenarios

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   Customer: CU99999999, Account: ACC000000000  - Closed account status  - Expected: Return code 403, "ACCOUNT CLOSED"   `

Migration Notes for Java Conversion
-----------------------------------

### Key Considerations

1.  **COMP-3 Fields:** All monetary amounts use packed decimal - requires BigDecimal in Java
    
2.  **Condition Names (88-levels):** Convert to Java enums for type safety
    
3.  **SQL Embedded:** Replace with JPA/Hibernate or MyBatis
    
4.  **Error Handling:** COBOL's simple error codes need structured Java exception hierarchy
    
5.  **Performance:** Current sub-second response time must be maintained
    

### Business Logic Preservation

*   All 4 business rules must be exactly replicated
    
*   Regulatory compliance calculations cannot be modified
    
*   Error messages must remain consistent for downstream systems
    
*   Daily limit enforcement logic is audited monthly
    

**Document Classification:** Internal Use Only**Next Review Date:** June 30, 2024**Related Documents:** ACCVAL01-TEST-PLAN.pdf, ACCOUNT-VALIDATION-API-SPEC.md