Troubleshooting Guide: COBOL to Java Migration
==============================================

**Document ID:** TRB-2024-001**Version:** 2.3**Last Updated:** March 10, 2024**Maintained By:** Migration Support Team**On-Call Support:** migration-support@company.com

Data Conversion Issues
----------------------

### Issue 1: Decimal Precision Lost During COBOL COMP-3 to BigDecimal Conversion

**Error Symptoms:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   java.lang.ArithmeticException: Rounding necessary      at java.math.BigDecimal.setScale(BigDecimal.java:2796)      at com.company.service.AccountValidationService.calculateOverdraft(AccountValidationService.java:147)   `

**Root Cause:** COBOL COMP-3 fields with implicit decimal scaling being converted without proper BigDecimal scale handling.

**COBOL Source Example:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   01  ACCOUNT-BALANCE    PIC S9(11)V99 COMP-3.  01  INTEREST-RATE      PIC S9(3)V99 COMP-3.   `

**Incorrect Java Conversion:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   // This causes precision loss  BigDecimal balance = new BigDecimal(rs.getDouble("account_balance"));   `

**Correct Java Solution:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   // Use BigDecimal constructor with proper scale  BigDecimal balance = rs.getBigDecimal("account_balance")      .setScale(2, RoundingMode.HALF_UP);  // For calculations, maintain precision  BigDecimal interestAmount = balance      .multiply(interestRate)      .divide(new BigDecimal("365"), 4, RoundingMode.HALF_UP)      .setScale(2, RoundingMode.HALF_UP);   `

**Prevention Steps:**

1.  Always use BigDecimal for monetary amounts
    
2.  Set explicit scale and rounding mode
    
3.  Use database DECIMAL columns, not FLOAT/DOUBLE
    
4.  Validate data conversion with sample records
    

**Verification Query:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   -- Compare original vs converted values  SELECT       original_balance,      converted_balance,      ABS(original_balance - converted_balance) as difference  FROM balance_comparison   WHERE ABS(original_balance - converted_balance) > 0.01;   `

### Issue 2: EBCDIC to ASCII Character Encoding Problems

**Error Symptoms:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   Customer name displays as: "JO■N ■M▓TH" instead of "JOHN SMITH"  Account numbers contain special characters: "ABC▓12345■"   `

**Root Cause:** Mainframe data stored in EBCDIC encoding not properly converted to UTF-8/ASCII.

**Detection Script:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   public boolean hasEbcdicArtifacts(String text) {      // Check for common EBCDIC conversion artifacts      Pattern artifacts = Pattern.compile("[■▓▪▫]");      return artifacts.matcher(text).find();  }   `

**Solution - Data Conversion:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   import java.nio.charset.Charset;  import java.nio.charset.StandardCharsets;  public class EbcdicConverter {      private static final Charset EBCDIC = Charset.forName("IBM037");      public String convertFromEbcdic(byte[] ebcdicBytes) {          // Convert EBCDIC bytes to UTF-8 string          String intermediate = new String(ebcdicBytes, EBCDIC);          return new String(intermediate.getBytes(StandardCharsets.UTF_8), StandardCharsets.UTF_8)              .trim()              .replaceAll("\\u0000", ""); // Remove null characters      }  }   `

**Database Migration Fix:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   -- Update corrupted customer names  UPDATE customer_profile   SET customer_name = REPLACE(REPLACE(REPLACE(customer_name, '■', ''), '▓', ''), '▪', '')  WHERE customer_name ~ '[■▓▪▫]';  -- Validate encoding fix  SELECT customer_id, customer_name   FROM customer_profile   WHERE customer_name ~ '[^A-Za-z0-9 \-\.]';   `

### Issue 3: Date Format Conversion Errors

**Error Symptoms:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   java.time.format.DateTimeParseException: Text '2024-03-32' could not be parsed  Invalid date: 1240315 (COBOL format YYMMDD)   `

**Root Cause:** COBOL date formats (YYMMDD, Julian dates) not properly converted to Java LocalDate.

**COBOL Date Formats:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   01  TRANSACTION-DATE    PIC 9(6).      // YYMMDD: 240315 = March 15, 2024  01  JULIAN-DATE         PIC 9(5).      // YYDDD: 24074 = March 15, 2024    01  TIMESTAMP           PIC 9(14).     // YYYYMMDDHHMMSS   `

**Java Conversion Solution:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   @Component  public class CobolDateConverter {      public LocalDate convertYYMMDD(String cobolDate) {          if (cobolDate == null || cobolDate.length() != 6) {              throw new IllegalArgumentException("Invalid COBOL YYMMDD date: " + cobolDate);          }          try {              int year = Integer.parseInt(cobolDate.substring(0, 2));              int month = Integer.parseInt(cobolDate.substring(2, 4));              int day = Integer.parseInt(cobolDate.substring(4, 6));              // Handle Y2K: assume 00-49 = 2000-2049, 50-99 = 1950-1999              int fullYear = year <= 49 ? 2000 + year : 1900 + year;              return LocalDate.of(fullYear, month, day);          } catch (Exception e) {              throw new IllegalArgumentException("Cannot parse COBOL date: " + cobolDate, e);          }      }      public LocalDate convertJulianDate(String julianDate) {          if (julianDate == null || julianDate.length() != 5) {              throw new IllegalArgumentException("Invalid Julian date: " + julianDate);          }          int year = Integer.parseInt(julianDate.substring(0, 2));          int dayOfYear = Integer.parseInt(julianDate.substring(2, 5));          int fullYear = year <= 49 ? 2000 + year : 1900 + year;          return LocalDate.ofYearDay(fullYear, dayOfYear);      }  }   `

**Database Schema Migration:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   -- Convert COBOL date columns to proper DATE type  ALTER TABLE customer_accounts   ADD COLUMN last_activity_date_new DATE;  UPDATE customer_accounts   SET last_activity_date_new =       CASE           WHEN last_activity_date ~ '^[0-9]{6}$' THEN              TO_DATE(                  CASE WHEN SUBSTR(last_activity_date, 1, 2)::INT <= 49                        THEN '20' || last_activity_date                       ELSE '19' || last_activity_date                  END,                   'YYYYMMDD'              )          ELSE NULL      END;   `

Performance Issues
------------------

### Issue 4: Slow Database Query Performance After Migration

**Symptoms:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   Account lookup taking 2.5 seconds (was 180ms in COBOL)  Database connection pool exhausted  High CPU usage on RDS instance   `

**Diagnostic Queries:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   -- Find slow queries  SELECT query, mean_time, calls, total_time  FROM pg_stat_statements   WHERE mean_time > 1000 -- queries taking more than 1 second  ORDER BY mean_time DESC;  -- Check missing indexes  SELECT schemaname, tablename, attname, n_distinct, correlation  FROM pg_stats  WHERE schemaname = 'public'     AND n_distinct > 100    AND correlation < 0.1;   `

**Common Causes & Solutions:**

#### Cause 1: Missing Indexes

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   -- COBOL program had implicit VSAM key access  -- Java JPA generates full table scans  -- Add missing indexes based on query patterns  CREATE INDEX idx_customer_accounts_customer_id   ON customer_accounts(customer_id);  CREATE INDEX idx_customer_accounts_status_type   ON customer_accounts(account_status, account_type);  -- Composite index for common queries  CREATE INDEX idx_customer_accounts_lookup   ON customer_accounts(customer_id, account_number, account_status);   `

#### Cause 2: N+1 Query Problem

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   // Problem: N+1 queries generated  @GetMapping("/customers/{customerId}/accounts")  public List getCustomerAccounts(@PathVariable String customerId) {      Customer customer = customerService.findById(customerId);      return customer.getAccounts().stream()  // Lazy loading causes N+1          .map(AccountMapper::toDTO)          .collect(Collectors.toList());  }  // Solution: Use fetch joins  @Query("SELECT c FROM Customer c JOIN FETCH c.accounts WHERE c.customerId = :customerId")  Customer findCustomerWithAccounts(@Param("customerId") String customerId);   `

#### Cause 3: Connection Pool Exhaustion

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   # Fix connection pool settings  spring:    datasource:      hikari:        maximum-pool-size: 25        # Was: 10 (too small)        minimum-idle: 5        connection-timeout: 10000        idle-timeout: 300000        max-lifetime: 1200000        leak-detection-threshold: 60000   `

### Issue 5: Memory Leaks in Batch Processing

**Error Symptoms:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   java.lang.OutOfMemoryError: Java heap space  GC overhead limit exceeded  Batch job fails after processing 50,000 records   `

**Root Cause Analysis:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   // Memory profiling revealed objects not being garbage collected  // Problem code - accumulating objects in memory  @Bean  public ItemReader accountReader() {      return new JdbcCursorItemReader() {{          setRowMapper(new CustomerAccountRowMapper());          setSql("SELECT * FROM customer_accounts"); // Loads entire table!      }};  }   `

**Solution - Chunked Processing:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   @Bean  public Job batchProcessingJob() {      return jobBuilderFactory.get("accountProcessing")          .start(accountProcessingStep())          .build();  }  @Bean  public Step accountProcessingStep() {      return stepBuilderFactory.get("accountProcessingStep")          .chunk(1000) // Process in chunks of 1000          .reader(accountReader())          .processor(accountProcessor())          .writer(accountWriter())          .taskExecutor(taskExecutor()) // Parallel processing          .build();  }  // Proper pagination  @Bean  public JdbcPagingItemReader accountReader() {      JdbcPagingItemReader reader = new JdbcPagingItemReader<>();      reader.setDataSource(dataSource);      reader.setQueryProvider(queryProvider());      reader.setPageSize(1000);      reader.setRowMapper(new CustomerAccountRowMapper());      return reader;  }   `

**JVM Tuning for Batch Jobs:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   # Batch job JVM settings  java -Xms2g -Xmx4g \       -XX:+UseG1GC \       -XX:MaxGCPauseMillis=200 \       -XX:+HeapDumpOnOutOfMemoryError \       -XX:HeapDumpPath=/var/log/heapdumps/ \       -jar batch-processor.jar   `

Business Logic Errors
---------------------

### Issue 6: Incorrect Overdraft Calculation Results

**Symptoms:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   Customer complaint: "Java system approved $5,000 overdraft, COBOL only allowed $2,500"  Audit findings: Overdraft calculations differ between systems   `

**Root Cause:** COBOL conditional logic not properly translated to Java.

**COBOL Original Logic:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   CALCULATE-OVERDRAFT-SECTION.      IF CUST-TIER-CODE = 'P'          COMPUTE WS-TIER-MULTIPLIER = 2.5      ELSE          IF CUST-TIER-CODE = 'G'              COMPUTE WS-TIER-MULTIPLIER = 1.8          ELSE              IF CUST-TIER-CODE = 'S'                  COMPUTE WS-TIER-MULTIPLIER = 1.2              ELSE                  COMPUTE WS-TIER-MULTIPLIER = 1.0              END-IF          END-IF      END-IF.      // Credit score bonus calculation      COMPUTE WS-CREDIT-BONUS = (CUST-CREDIT-SCORE - 600) * 10.      IF WS-CREDIT-BONUS < 0          MOVE 0 TO WS-CREDIT-BONUS      END-IF.      IF WS-CREDIT-BONUS > 500          MOVE 500 TO WS-CREDIT-BONUS        END-IF.   `

**Incorrect Java Translation:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   // Bug: Using floating point arithmetic  public double calculateOverdraft(CustomerProfile profile, BigDecimal baseLimit) {      double multiplier = switch (profile.getTierCode()) {          case "P" -> 2.5;          case "G" -> 1.8;           case "S" -> 1.2;          default -> 1.0;      };      // Bug: Credit bonus calculation wrong      double creditBonus = Math.max(0, (profile.getCreditScore() - 600) * 10);      creditBonus = Math.min(creditBonus, 500); // Missing .00 decimal      return baseLimit.doubleValue() + (creditBonus * multiplier);  }   `

**Correct Java Solution:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   public BigDecimal calculateOverdraftLimit(CustomerProfile profile, BigDecimal baseLimit) {      // Use exact same logic as COBOL with BigDecimal precision      BigDecimal tierMultiplier = switch (profile.getTierCode()) {          case PREMIUM -> new BigDecimal("2.50");          case GOLD -> new BigDecimal("1.80");          case SILVER -> new BigDecimal("1.20");          default -> new BigDecimal("1.00");      };      // Replicate exact COBOL credit bonus calculation      BigDecimal creditBonus = BigDecimal.ZERO;      if (profile.getCreditScore() != null && profile.getCreditScore() > 600) {          creditBonus = BigDecimal.valueOf(profile.getCreditScore() - 600)              .multiply(BigDecimal.TEN)              .setScale(2, RoundingMode.HALF_UP);          // Cap at $500.00 exactly as COBOL does          if (creditBonus.compareTo(new BigDecimal("500.00")) > 0) {              creditBonus = new BigDecimal("500.00");          }      }      return baseLimit.add(creditBonus.multiply(tierMultiplier))          .setScale(2, RoundingMode.HALF_UP);  }   `

**Testing Approach:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   @Test  void shouldMatchCobolOverdraftCalculation() {      // Test data from COBOL system      CustomerProfile premiumCustomer = CustomerProfile.builder()          .customerId("CU12345678")          .tierCode(TierCode.PREMIUM)          .creditScore(750)          .build();      BigDecimal baseLimit = new BigDecimal("1000.00");      // Expected result from COBOL: $4750.00      // Base: $1000 + Credit Bonus: $1500 * Multiplier: 2.5 = $3750 = $4750 total      BigDecimal expected = new BigDecimal("4750.00");      BigDecimal actual = overdraftCalculator.calculateOverdraftLimit(premiumCustomer, baseLimit);      assertThat(actual).isEqualByComparingTo(expected);  }   `

Integration Issues
------------------

### Issue 7: Connection Timeout Errors with External Systems

**Error Symptoms:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   org.springframework.web.client.ResourceAccessException:   I/O error on POST request for "https://external-api.bank.com/validate":   Connection timed out; nested exception is java.net.SocketTimeoutException   `

**Root Cause:** Default Java HTTP client timeouts too short for mainframe-style synchronous processing.

**Problem Configuration:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   // Default RestTemplate has very short timeouts  @Bean  public RestTemplate restTemplate() {      return new RestTemplate(); // Uses default 2-second timeout  }   `

**Solution - Proper Timeout Configuration:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   @Configuration  public class RestTemplateConfig {      @Bean      public RestTemplate restTemplate() {          HttpComponentsClientHttpRequestFactory factory =               new HttpComponentsClientHttpRequestFactory();          // Configure timeouts based on external system SLAs          factory.setConnectTimeout(10000);     // 10 seconds to establish connection          factory.setReadTimeout(30000);        // 30 seconds for response          factory.setConnectionRequestTimeout(5000); // 5 seconds to get connection from pool          // Connection pooling          PoolingHttpClientConnectionManager connectionManager =               new PoolingHttpClientConnectionManager();          connectionManager.setMaxTotal(100);          connectionManager.setDefaultMaxPerRoute(20);          CloseableHttpClient httpClient = HttpClients.custom()              .setConnectionManager(connectionManager)              .build();          factory.setHttpClient(httpClient);          RestTemplate restTemplate = new RestTemplate(factory);          // Add retry interceptor          restTemplate.setInterceptors(List.of(new RetryInterceptor()));          return restTemplate;      }  }  // Retry interceptor for transient failures  public class RetryInterceptor implements ClientHttpRequestInterceptor {      private static final int MAX_RETRIES = 3;      @Override      public ClientHttpResponse intercept(              HttpRequest request,               byte[] body,               ClientHttpRequestExecution execution) throws IOException {          for (int i = 0; i < MAX_RETRIES; i++) {              try {                  return execution.execute(request, body);              } catch (IOException e) {                  if (i == MAX_RETRIES - 1) {                      throw e;                  }                  // Wait before retry (exponential backoff)                  try {                      Thread.sleep((long) Math.pow(2, i) * 1000);                  } catch (InterruptedException ie) {                      Thread.currentThread().interrupt();                      throw new IOException("Interrupted during retry", ie);                  }              }          }          return null;      }  }   `

### Issue 8: Spring Batch Job Failures with Restart Issues

**Error Symptoms:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   JobInstanceAlreadyCompleteException: A job instance already exists and is complete  JobExecutionAlreadyRunningException: A job execution for this job is already running  Batch job stuck in "STARTED" status   `

**Diagnostic Commands:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   -- Check batch job status  SELECT job_instance_id, job_name, start_time, end_time, status, exit_code  FROM batch_job_execution   WHERE job_name = 'dailyAccountProcessing'  ORDER BY start_time DESC;  -- Find stuck jobs  SELECT * FROM batch_job_execution   WHERE status = 'STARTED'     AND start_time < NOW() - INTERVAL '1 hour';   `

**Solution - Proper Job Configuration:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   @Configuration  @EnableBatchProcessing  public class BatchConfig {      @Bean      public Job dailyAccountProcessingJob() {          return jobBuilderFactory.get("dailyAccountProcessing")              .incrementer(new RunIdIncrementer()) // Allows multiple runs              .start(balanceCalculationStep())              .next(interestCalculationStep())              .next(reportGenerationStep())              .build();      }      @Bean      public Step balanceCalculationStep() {          return stepBuilderFactory.get("balanceCalculation")              .chunk(1000)              .reader(accountReader())              .processor(balanceProcessor())              .writer(accountWriter())              .faultTolerant()              .retryLimit(3)              .retry(Exception.class)              .skipLimit(100)              .skip(ValidationException.class)              .build();      }  }  // Job restart utility  @Component  public class BatchJobManager {      @Autowired      private JobLauncher jobLauncher;      @Autowired      private JobExplorer jobExplorer;      public void restartFailedJob(String jobName) {          List jobInstances = jobExplorer.getJobInstances(jobName, 0, 10);          for (JobInstance instance : jobInstances) {              List executions = jobExplorer.getJobExecutions(instance);              for (JobExecution execution : executions) {                  if (execution.getStatus() == BatchStatus.FAILED) {                      JobParameters restartParams = new JobParametersBuilder()                          .addLong("restart.timestamp", System.currentTimeMillis())                          .toJobParameters();                      try {                          jobLauncher.run(dailyAccountProcessingJob, restartParams);                          log.info("Restarted failed job: {}", jobName);                      } catch (Exception e) {                          log.error("Failed to restart job: {}", jobName, e);                      }                  }              }          }      }  }   `

Monitoring and Alerting
-----------------------

### Critical Alerts Setup

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   # CloudWatch Alarms  alerts:    - name: "High Response Time"      metric: "avg_response_time"      threshold: 200ms      action: "page_oncall"    - name: "Error Rate Spike"        metric: "error_rate"      threshold: 1%      action: "email_team"    - name: "Database Connection Pool Exhausted"      metric: "db_pool_active_connections"      threshold: 90%      action: "scale_up"   `

### Troubleshooting Commands

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   # Check application logs  kubectl logs -f deployment/account-service --tail=100  # Database performance  psql -c "SELECT * FROM pg_stat_activity WHERE state = 'active';"  # JVM memory usage  jstat -gc -t [PID] 5s  # Network connectivity  curl -w "@curl-format.txt" -o /dev/null -s "https://api.endpoint.com/health"   `

Emergency Procedures
--------------------

### Production Issue Response

1.  **Immediate Response (0-15 minutes)**
    
    *   Check application health endpoints
        
    *   Review CloudWatch dashboards
        
    *   Verify database connectivity
        
    *   Check for recent deployments
        
2.  **Investigation (15-30 minutes)**
    
    *   Analyze application logs
        
    *   Query database for anomalies
        
    *   Compare with baseline metrics
        
    *   Identify affected customers
        
3.  **Resolution (30-60 minutes)**
    
    *   Apply immediate fixes
        
    *   Scale resources if needed
        
    *   Rollback if necessary
        
    *   Communicate to stakeholders
        

### Rollback Procedures

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   # Application rollback  kubectl rollout undo deployment/account-service  # Database rollback (if schema changes)  flyway repair  flyway migrate -target=1.2.3  # Traffic routing rollback  aws elbv2 modify-listener --listener-arn $LISTENER_ARN \    --default-actions Type=forward,TargetGroupArn=$OLD_TARGET_GROUP   `

**Document Maintenance:** Updated monthly based on new issues discovered**Escalation Path:** L1 → Migration Team → Architecture Team → CTO Office**Emergency Contact:** +1-555-MIGRATE (24/7 on-call rotation)