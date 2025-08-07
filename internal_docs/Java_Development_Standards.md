Java Development Standards for Mainframe Migration
==================================================

**Document ID:** JDS-2024-001**Version:** 1.8**Effective Date:** February 1, 2024**Owner:** Enterprise Architecture Team**Approved By:** CTO Office

Framework Selection Standards
-----------------------------

### Primary Framework Stack

#### Spring Boot 3.2+ (Mandatory)

**Rationale:** Provides enterprise-grade transaction management, security, and operational monitoring required for financial applications migrated from mainframe systems.

**Required Dependencies:**

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML    `org.springframework.boot      spring-boot-starter-web      org.springframework.boot      spring-boot-starter-data-jpa      org.springframework.boot      spring-boot-starter-batch      org.springframework.boot      spring-boot-starter-security`

#### Database Layer Standards

**JPA/Hibernate:** Required for entity management**Connection Pooling:** HikariCP (default in Spring Boot)**Migration Tool:** Flyway for database versioning

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   // Standard entity annotation pattern  @Entity  @Table(name = "customer_accounts")  @Data  @NoArgsConstructor  @AllArgsConstructor  public class CustomerAccount {      @Id      @Column(name = "account_number", length = 12)      private String accountNumber;      @Column(name = "customer_id", length = 10, nullable = false)      private String customerId;      // Financial amounts MUST use BigDecimal      @Column(name = "current_balance", precision = 15, scale = 2)      private BigDecimal currentBalance;      @Enumerated(EnumType.STRING)      @Column(name = "account_status")      private AccountStatus accountStatus;      @CreationTimestamp      @Column(name = "created_date")      private LocalDateTime createdDate;      @UpdateTimestamp        @Column(name = "last_modified_date")      private LocalDateTime lastModifiedDate;  }   `

Monetary Calculation Standards
------------------------------

### BigDecimal Usage (Mandatory for Financial Data)

**Rule:** All monetary amounts, percentages, and financial calculations MUST use java.math.BigDecimal

**Forbidden:** Never use float, double, or int for money

#### Standard Implementation Pattern

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   @Service  @Transactional  public class AccountValidationService {      // Correct: BigDecimal with explicit scale and rounding      private static final BigDecimal OVERDRAFT_MULTIPLIER_PREMIUM = new BigDecimal("2.50");      private static final BigDecimal OVERDRAFT_MULTIPLIER_GOLD = new BigDecimal("1.80");      private static final BigDecimal ZERO_AMOUNT = BigDecimal.ZERO;      public OverdraftCalculationResult calculateAvailableOverdraft(              String customerId, String accountNumber, BigDecimal transactionAmount) {          CustomerAccount account = accountRepository.findByAccountNumber(accountNumber)              .orElseThrow(() -> new AccountNotFoundException(accountNumber));          CustomerProfile profile = customerRepository.findById(customerId)              .orElseThrow(() -> new CustomerNotFoundException(customerId));          // Calculate credit score bonus with proper scale          BigDecimal creditScoreBonus = calculateCreditScoreBonus(profile.getCreditScore());          // Apply tier multiplier          BigDecimal tierMultiplier = getTierMultiplier(profile.getTierCode());          // Final calculation with controlled rounding          BigDecimal availableOverdraft = account.getOverdraftLimit()              .add(creditScoreBonus.multiply(tierMultiplier))              .subtract(account.getCurrentBalance().abs())              .setScale(2, RoundingMode.HALF_UP);          return OverdraftCalculationResult.builder()              .availableOverdraft(availableOverdraft)              .creditScoreBonus(creditScoreBonus)              .tierMultiplier(tierMultiplier)              .build();      }      private BigDecimal calculateCreditScoreBonus(Integer creditScore) {          if (creditScore == null || creditScore < 600) {              return ZERO_AMOUNT;          }          BigDecimal bonus = BigDecimal.valueOf(creditScore - 600)              .multiply(BigDecimal.TEN)              .setScale(2, RoundingMode.HALF_UP);          // Cap at $500 maximum bonus          return bonus.min(new BigDecimal("500.00"));      }  }   `

### Rounding Standards

*   **Default Rounding:** RoundingMode.HALF\_UP (banker's rounding)
    
*   **Scale:** 2 decimal places for currency amounts
    
*   **Interest Calculations:** 4 decimal places, round to 2 for final amounts
    

Entity Design Patterns
----------------------

### Naming Conventions

#### Class Names

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   // Entity classes: Singular nouns  public class CustomerAccount { }  public class TransactionHistory { }  // Service classes: [Entity]Service pattern  public class CustomerAccountService { }  public class TransactionProcessingService { }  // Repository classes: [Entity]Repository pattern    public interface CustomerAccountRepository extends JpaRepository { }  // DTO classes: [Entity][Purpose]DTO pattern  public class CustomerAccountResponseDTO { }  public class TransactionRequestDTO { }   `

#### Field and Method Names

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   // Use camelCase, be descriptive  private BigDecimal currentBalance;          // Good  private BigDecimal bal;                     // Bad - too abbreviated  // Boolean methods: is/has/can prefix  public boolean isAccountActive() { }  public boolean hasOverdraftProtection() { }  public boolean canProcessTransaction() { }  // Calculation methods: calculate/compute prefix  public BigDecimal calculateDailyInterest() { }  public OverdraftResult computeAvailableOverdraft() { }   `

### Enum Usage (Replace COBOL 88-levels)

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   // Convert COBOL condition names to Java enums  public enum AccountStatus {      ACTIVE("A", "Account is active and can process transactions"),      SUSPENDED("S", "Account is temporarily suspended"),      CLOSED("C", "Account is permanently closed"),      FROZEN("F", "Account is frozen by compliance");      private final String code;      private final String description;      AccountStatus(String code, String description) {          this.code = code;          this.description = description;      }      public static AccountStatus fromCode(String code) {          return Arrays.stream(values())              .filter(status -> status.code.equals(code))              .findFirst()              .orElseThrow(() -> new IllegalArgumentException("Invalid status code: " + code));      }      // Getters      public String getCode() { return code; }      public String getDescription() { return description; }  }  // Usage in entity  @Enumerated(EnumType.STRING)  @Column(name = "account_status")  private AccountStatus accountStatus;   `

### Validation Standards

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   // Use Bean Validation annotations  @Entity  public class CustomerAccount {      @NotBlank(message = "Account number is required")      @Pattern(regexp = "^[A-Z]{3}\\d{9}$", message = "Account number must be 3 letters followed by 9 digits")      private String accountNumber;      @NotBlank(message = "Customer ID is required")       @Pattern(regexp = "^CU\\d{8}$", message = "Customer ID must be CU followed by 8 digits")      private String customerId;      @DecimalMin(value = "-999999999.99", message = "Balance cannot be less than -999,999,999.99")      @DecimalMax(value = "999999999.99", message = "Balance cannot exceed 999,999,999.99")      @Digits(integer = 9, fraction = 2, message = "Balance must have at most 9 integer digits and 2 decimal places")      private BigDecimal currentBalance;      @NotNull(message = "Account status is required")      private AccountStatus accountStatus;  }   `

Service Layer Patterns
----------------------

### Transaction Management

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   @Service  @Transactional  public class AccountTransactionService {      // Read-only operations should be marked as such      @Transactional(readOnly = true)      public CustomerAccount findAccountByNumber(String accountNumber) {          return accountRepository.findByAccountNumber(accountNumber)              .orElseThrow(() -> new AccountNotFoundException(accountNumber));      }      // Financial operations require new transaction      @Transactional(propagation = Propagation.REQUIRES_NEW)      public TransactionResult processDebitTransaction(              String accountNumber, BigDecimal amount, String transactionType) {          // Validate account          CustomerAccount account = findAccountByNumber(accountNumber);          validateAccountStatus(account);          // Check daily limits          validateDailyLimits(account, amount);          // Process transaction          account.setCurrentBalance(account.getCurrentBalance().subtract(amount));          accountRepository.save(account);          // Record transaction history          recordTransactionHistory(accountNumber, amount, transactionType);          return TransactionResult.success(account.getCurrentBalance());      }      private void validateAccountStatus(CustomerAccount account) {          if (account.getAccountStatus() != AccountStatus.ACTIVE) {              throw new InvalidAccountStatusException(                  String.format("Account %s is %s",                       account.getAccountNumber(),                       account.getAccountStatus().getDescription()));          }      }  }   `

### Error Handling Standards

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   // Custom exception hierarchy  @ResponseStatus(HttpStatus.NOT_FOUND)  public class AccountNotFoundException extends RuntimeException {      public AccountNotFoundException(String accountNumber) {          super(String.format("Account not found: %s", accountNumber));      }  }  @ResponseStatus(HttpStatus.BAD_REQUEST)  public class InvalidAccountStatusException extends RuntimeException {      public InvalidAccountStatusException(String message) {          super(message);      }  }  @ResponseStatus(HttpStatus.TOO_MANY_REQUESTS)  public class DailyLimitExceededException extends RuntimeException {      public DailyLimitExceededException(String accountNumber, BigDecimal limit) {          super(String.format("Daily limit exceeded for account %s. Limit: %s",               accountNumber, limit));      }  }  // Global exception handler  @ControllerAdvice  public class GlobalExceptionHandler {      @ExceptionHandler(AccountNotFoundException.class)      public ResponseEntity handleAccountNotFound(AccountNotFoundException ex) {          ErrorResponse error = ErrorResponse.builder()              .errorCode("ACCOUNT_NOT_FOUND")              .message(ex.getMessage())              .timestamp(LocalDateTime.now())              .build();          return ResponseEntity.status(HttpStatus.NOT_FOUND).body(error);      }  }   `

Testing Standards
-----------------

### Required Testing Frameworks

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML    `org.junit.jupiter      junit-jupiter      test      org.mockito      mockito-core      test      org.testcontainers      junit-jupiter      test      org.testcontainers      postgresql      test`

### Unit Testing Pattern

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   @ExtendWith(MockitoExtension.class)  class AccountValidationServiceTest {      @Mock      private CustomerAccountRepository accountRepository;      @Mock        private CustomerRepository customerRepository;      @InjectMocks      private AccountValidationService accountValidationService;      @Test      @DisplayName("Should calculate correct overdraft for premium customer")      void shouldCalculateCorrectOverdraftForPremiumCustomer() {          // Given          String accountNumber = "ACC123456789";          String customerId = "CU12345678";          CustomerAccount account = CustomerAccount.builder()              .accountNumber(accountNumber)                .customerId(customerId)              .currentBalance(new BigDecimal("1500.00"))              .overdraftLimit(new BigDecimal("1000.00"))              .build();          CustomerProfile profile = CustomerProfile.builder()              .customerId(customerId)              .tierCode(TierCode.PREMIUM)              .creditScore(750)              .build();          when(accountRepository.findByAccountNumber(accountNumber))              .thenReturn(Optional.of(account));          when(customerRepository.findById(customerId))              .thenReturn(Optional.of(profile));          // When          OverdraftCalculationResult result = accountValidationService              .calculateAvailableOverdraft(customerId, accountNumber, new BigDecimal("100.00"));          // Then          assertThat(result.getAvailableOverdraft())              .isEqualByComparingTo(new BigDecimal("3875.00")); // 1000 + (1500 * 2.5)          assertThat(result.getCreditScoreBonus())              .isEqualByComparingTo(new BigDecimal("1500.00")); // (750-600) * 10          assertThat(result.getTierMultiplier())              .isEqualByComparingTo(new BigDecimal("2.50"));      }      @Test      @DisplayName("Should throw AccountNotFoundException when account does not exist")      void shouldThrowExceptionWhenAccountNotFound() {          // Given          String accountNumber = "INVALID123";          when(accountRepository.findByAccountNumber(accountNumber))              .thenReturn(Optional.empty());          // When & Then          assertThatThrownBy(() ->               accountValidationService.calculateAvailableOverdraft("CU12345678", accountNumber, BigDecimal.TEN))              .isInstanceOf(AccountNotFoundException.class)              .hasMessageContaining("INVALID123");      }  }   `

### Integration Testing Pattern

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   @SpringBootTest  @Testcontainers  @TestPropertySource(properties = {      "spring.datasource.url=jdbc:tc:postgresql:13:///testdb"  })  class AccountTransactionServiceIntegrationTest {      @Container      static PostgreSQLContainer postgres = new PostgreSQLContainer<>("postgres:13")              .withDatabaseName("testdb")              .withUsername("test")              .withPassword("test");      @Autowired      private AccountTransactionService accountTransactionService;      @Autowired      private TestEntityManager entityManager;      @Test      @Transactional      @Rollback      void shouldProcessDebitTransactionSuccessfully() {          // Given - Insert test data          CustomerAccount account = CustomerAccount.builder()              .accountNumber("ACC987654321")              .customerId("CU87654321")              .currentBalance(new BigDecimal("2000.00"))              .accountStatus(AccountStatus.ACTIVE)              .build();          entityManager.persistAndFlush(account);          // When          TransactionResult result = accountTransactionService              .processDebitTransaction("ACC987654321", new BigDecimal("500.00"), "DEBIT");          // Then          assertThat(result.isSuccess()).isTrue();          assertThat(result.getNewBalance()).isEqualByComparingTo(new BigDecimal("1500.00"));          // Verify database state          CustomerAccount updatedAccount = entityManager              .find(CustomerAccount.class, "ACC987654321");          assertThat(updatedAccount.getCurrentBalance())              .isEqualByComparingTo(new BigDecimal("1500.00"));      }  }   `

Security Standards
------------------

### Authentication and Authorization

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   @RestController  @RequestMapping("/api/v1/accounts")  @PreAuthorize("hasRole('BANK_TELLER') or hasRole('SYSTEM_ADMIN')")  public class AccountController {      @GetMapping("/{accountNumber}")      @PreAuthorize("hasPermission(#accountNumber, 'ACCOUNT', 'READ')")      public ResponseEntity getAccount(              @PathVariable String accountNumber) {          // Implementation      }      @PostMapping("/{accountNumber}/transactions")      @PreAuthorize("hasPermission(#accountNumber, 'ACCOUNT', 'TRANSACT')")      public ResponseEntity processTransaction(              @PathVariable String accountNumber,              @Valid @RequestBody TransactionRequest request) {          // Implementation        }  }   `

### Input Validation and Sanitization

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   @Component  public class InputSanitizer {      public String sanitizeAccountNumber(String accountNumber) {          if (accountNumber == null) {              throw new IllegalArgumentException("Account number cannot be null");          }          // Remove any non-alphanumeric characters          String cleaned = accountNumber.replaceAll("[^A-Za-z0-9]", "");          // Validate format          if (!cleaned.matches("^[A-Z]{3}\\d{9}$")) {              throw new IllegalArgumentException("Invalid account number format");          }          return cleaned;      }      public BigDecimal sanitizeMonetaryAmount(String amount) {          if (amount == null || amount.trim().isEmpty()) {              throw new IllegalArgumentException("Amount cannot be null or empty");          }          try {              BigDecimal value = new BigDecimal(amount.trim());              if (value.scale() > 2) {                  throw new IllegalArgumentException("Amount cannot have more than 2 decimal places");              }              return value.setScale(2, RoundingMode.HALF_UP);          } catch (NumberFormatException e) {              throw new IllegalArgumentException("Invalid amount format: " + amount);          }      }  }   `

Performance Requirements
------------------------

### Response Time Standards

*   **Account Lookup:** < 100ms (95th percentile)
    
*   **Transaction Processing:** < 200ms (95th percentile)
    
*   **Batch Operations:** < 50ms per record (95th percentile)
    

### Caching Strategy

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   @Service  public class CustomerProfileService {      @Cacheable(value = "customerProfiles", key = "#customerId")      public CustomerProfile getCustomerProfile(String customerId) {          return customerRepository.findById(customerId)              .orElseThrow(() -> new CustomerNotFoundException(customerId));      }      @CacheEvict(value = "customerProfiles", key = "#profile.customerId")      public CustomerProfile updateCustomerProfile(CustomerProfile profile) {          return customerRepository.save(profile);      }  }   `

Code Quality Standards
----------------------

### Required Code Analysis Tools

*   **SonarQube:** Minimum quality gate score of 80%
    
*   **SpotBugs:** Zero high-priority issues allowed
    
*   **Checkstyle:** Enforced coding style compliance
    
*   **JaCoCo:** Minimum 85% test coverage for service classes
    

### Documentation Requirements

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   /**   * Calculates available overdraft amount for a customer account.   *    * This method implements the same business logic as COBOL program ACCVAL01   * to ensure functional equivalence during migration.   *    * @param customerId Customer identifier (format: CU followed by 8 digits)   * @param accountNumber Account identifier (format: 3 letters + 9 digits)     * @param transactionAmount Proposed transaction amount (must be positive)   * @return OverdraftCalculationResult containing available overdraft and calculation details   * @throws AccountNotFoundException if account does not exist   * @throws CustomerNotFoundException if customer profile not found   * @throws IllegalArgumentException if input parameters are invalid   *    * @see [ACCVAL01 COBOL Program Specification](link-to-cobol-spec)   */  public OverdraftCalculationResult calculateAvailableOverdraft(          String customerId, String accountNumber, BigDecimal transactionAmount) {      // Implementation  }   `

**Document Status:** Active**Next Review:** August 1, 2024**Questions/Updates:** Contact enterprise-architecture@company.com