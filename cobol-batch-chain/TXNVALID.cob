       *>================================================================*
       *> TXNVALID.cob — Step 2: Transaction Validation
       *> Match-merge validator: walks sorted transactions against the
       *> customer master file, writing valid transactions to one output
       *> and rejected transactions (with reason codes) to another.
       *>
       *> Input:   data/SORTED-TRANS.DAT  (LINE SEQUENTIAL, by TRANS-ACCT-ID)
       *>          data/CUSTOMERS.DAT     (SEQUENTIAL binary, by CUST-ID)
       *> Output:  data/VALID-TRANS.DAT   (LINE SEQUENTIAL)
       *>          data/REJECT-TRANS.DAT  (LINE SEQUENTIAL)
       *>
       *> Return codes:
       *>   0 = all transactions valid
       *>   4 = some rejects (normal processing)
       *>   8 = input file error
       *>================================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXNVALID.
       AUTHOR.     BATCH-CHAIN.
       DATE-WRITTEN. 2026-03-02.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SORTED-TRANS-FILE
               ASSIGN TO 'data/SORTED-TRANS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TRANS-STATUS.

           SELECT CUSTOMER-FILE
               ASSIGN TO 'data/CUSTOMERS.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CUSTF-STATUS.

           SELECT VALID-TRANS-FILE
               ASSIGN TO 'data/VALID-TRANS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-VALID-STATUS.

           SELECT REJECT-TRANS-FILE
               ASSIGN TO 'data/REJECT-TRANS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-REJECT-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  SORTED-TRANS-FILE.
       COPY 'copybooks/TRANSREC.cpy'.

       FD  CUSTOMER-FILE.
       COPY 'copybooks/CUSTREC.cpy'.

       FD  VALID-TRANS-FILE.
           01  VALID-TRANS-RECORD          PIC X(47).

       FD  REJECT-TRANS-FILE.
       COPY 'copybooks/REJECTREC.cpy'.

       WORKING-STORAGE SECTION.

       *>----------------------------------------------------------------*
       *> File status codes
       *>----------------------------------------------------------------*
       01  WS-TRANS-STATUS                 PIC XX.
       01  WS-CUSTF-STATUS                PIC XX.
       01  WS-VALID-STATUS                PIC XX.
       01  WS-REJECT-STATUS               PIC XX.

       *>----------------------------------------------------------------*
       *> EOF flags
       *>----------------------------------------------------------------*
       01  WS-TRANS-EOF-FLAG               PIC X(1) VALUE 'N'.
           88  TRANS-EOF                       VALUE 'Y'.
           88  TRANS-NOT-EOF                   VALUE 'N'.

       01  WS-CUST-EOF-FLAG                PIC X(1) VALUE 'N'.
           88  CUST-EOF                        VALUE 'Y'.
           88  CUST-NOT-EOF                    VALUE 'N'.

       *>----------------------------------------------------------------*
       *> Working copies of current records
       *>----------------------------------------------------------------*
       01  WS-CURRENT-TRANS.
           05  WS-TRANS-ACCT-ID            PIC X(8).
           05  WS-TRANS-TYPE               PIC X(1).
               88  WS-TRANS-DEPOSIT            VALUE 'D'.
               88  WS-TRANS-WITHDRAW           VALUE 'W'.
               88  WS-TRANS-INTEREST           VALUE 'I'.
               88  WS-TRANS-VALID-TYPE         VALUE 'D' 'W' 'I'.
           05  WS-TRANS-AMOUNT             PIC 9(7)V99.
           05  WS-TRANS-DATE               PIC 9(8).
           05  WS-TRANS-BRANCH             PIC X(3).
           05  WS-TRANS-SEQ                PIC 9(6).
           05  WS-TRANS-DESC               PIC X(12).

       01  WS-CURRENT-CUST.
           05  WS-CUST-ID                  PIC X(8).
           05  WS-CUST-NAME                PIC X(25).
           05  WS-CUST-BRANCH              PIC X(3).
           05  WS-CUST-ACCT-TYPE           PIC X(1).
               88  WS-CUST-SAVINGS             VALUE 'S'.
               88  WS-CUST-CHECKING            VALUE 'C'.
           05  WS-CUST-STATUS              PIC X(1).
               88  WS-CUST-ACTIVE              VALUE 'A'.
               88  WS-CUST-CLOSED              VALUE 'C'.
               88  WS-CUST-FROZEN              VALUE 'F'.
           05  WS-CUST-BALANCE             PIC S9(7)V99 COMP-3.
           05  WS-CUST-OPEN-DATE           PIC 9(8).
           05  WS-CUST-LAST-ACTIVITY       PIC 9(8).
           05  WS-CUST-FILLER              PIC X(9).

       *>----------------------------------------------------------------*
       *> Counters
       *>----------------------------------------------------------------*
       01  WS-TRANS-READ-CTR               PIC 9(7) VALUE ZEROES.
       01  WS-CUST-READ-CTR               PIC 9(7) VALUE ZEROES.
       01  WS-VALID-CTR                   PIC 9(7) VALUE ZEROES.
       01  WS-REJECT-CTR                  PIC 9(7) VALUE ZEROES.

       *>----------------------------------------------------------------*
       *> Return code
       *>----------------------------------------------------------------*
       01  WS-RETURN-CODE                  PIC 9(1) VALUE 0.

       PROCEDURE DIVISION.

       *>================================================================*
       *> MAIN CONTROL
       *>================================================================*
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           IF WS-RETURN-CODE NOT = 8
               PERFORM 2000-MATCH-MERGE
                   UNTIL TRANS-EOF
               PERFORM 3000-TERMINATE
           END-IF
           MOVE WS-RETURN-CODE TO RETURN-CODE
           STOP RUN
           .

       *>================================================================*
       *> INITIALIZATION — open all files, prime both inputs
       *>================================================================*
       1000-INITIALIZE.
           DISPLAY '========================================='
           DISPLAY 'TXNVALID: Step 2 — Transaction Validation'
           DISPLAY '========================================='

           OPEN INPUT  SORTED-TRANS-FILE
           IF WS-TRANS-STATUS NOT = '00'
               DISPLAY 'TXNVALID: ERROR opening SORTED-TRANS.DAT'
                       ' STATUS=' WS-TRANS-STATUS
               MOVE 8 TO WS-RETURN-CODE
               PERFORM 3000-TERMINATE
           END-IF

           OPEN INPUT  CUSTOMER-FILE
           IF WS-CUSTF-STATUS NOT = '00'
               DISPLAY 'TXNVALID: ERROR opening CUSTOMERS.DAT'
                       ' STATUS=' WS-CUSTF-STATUS
               MOVE 8 TO WS-RETURN-CODE
               PERFORM 3000-TERMINATE
           END-IF

           OPEN OUTPUT VALID-TRANS-FILE
           IF WS-VALID-STATUS NOT = '00'
               DISPLAY 'TXNVALID: ERROR opening VALID-TRANS.DAT'
                       ' STATUS=' WS-VALID-STATUS
               MOVE 8 TO WS-RETURN-CODE
               PERFORM 3000-TERMINATE
           END-IF

           OPEN OUTPUT REJECT-TRANS-FILE
           IF WS-REJECT-STATUS NOT = '00'
               DISPLAY 'TXNVALID: ERROR opening REJECT-TRANS.DAT'
                       ' STATUS=' WS-REJECT-STATUS
               MOVE 8 TO WS-RETURN-CODE
               PERFORM 3000-TERMINATE
           END-IF

      *> Prime both input files
           PERFORM 5000-READ-TRANS
           PERFORM 6000-READ-CUST
           .

       *>================================================================*
       *> MATCH-MERGE LOOP
       *> Compare TRANS-ACCT-ID vs CUST-ID and route accordingly.
       *>================================================================*
       2000-MATCH-MERGE.
           EVALUATE TRUE

      *> Master exhausted — reject all remaining transactions
               WHEN CUST-EOF
                   PERFORM 2100-REJECT-NO-ACCOUNT
                   PERFORM 5000-READ-TRANS

      *> Transaction key < master key — no matching account
               WHEN WS-TRANS-ACCT-ID < WS-CUST-ID
                   PERFORM 2100-REJECT-NO-ACCOUNT
                   PERFORM 5000-READ-TRANS

      *> Keys match — validate this transaction
               WHEN WS-TRANS-ACCT-ID = WS-CUST-ID
                   PERFORM 2200-VALIDATE-TRANSACTION
                   PERFORM 5000-READ-TRANS

      *> Transaction key > master key — advance master
               WHEN WS-TRANS-ACCT-ID > WS-CUST-ID
                   PERFORM 6000-READ-CUST

           END-EVALUATE
           .

       *>================================================================*
       *> REJECT — no matching account
       *>================================================================*
       2100-REJECT-NO-ACCOUNT.
           PERFORM 4100-BUILD-REJECT-BASE
           SET REJ-NO-ACCOUNT TO TRUE
           PERFORM 4200-WRITE-REJECT
           .

       *>================================================================*
       *> VALIDATE — account found, apply business rules
       *>================================================================*
       2200-VALIDATE-TRANSACTION.
           EVALUATE TRUE

      *> Invalid transaction type
               WHEN NOT WS-TRANS-VALID-TYPE
                   PERFORM 4100-BUILD-REJECT-BASE
                   SET REJ-INVALID-TYPE TO TRUE
                   PERFORM 4200-WRITE-REJECT

      *> Zero amount
               WHEN WS-TRANS-AMOUNT = ZEROES
                   PERFORM 4100-BUILD-REJECT-BASE
                   SET REJ-INVALID-AMOUNT TO TRUE
                   PERFORM 4200-WRITE-REJECT

      *> Account closed
               WHEN WS-CUST-CLOSED
                   PERFORM 4100-BUILD-REJECT-BASE
                   SET REJ-CLOSED TO TRUE
                   PERFORM 4200-WRITE-REJECT

      *> Account frozen
               WHEN WS-CUST-FROZEN
                   PERFORM 4100-BUILD-REJECT-BASE
                   SET REJ-FROZEN TO TRUE
                   PERFORM 4200-WRITE-REJECT

      *> All checks passed — transaction is valid
               WHEN OTHER
                   WRITE VALID-TRANS-RECORD
                       FROM WS-CURRENT-TRANS
                   ADD 1 TO WS-VALID-CTR

           END-EVALUATE
           .

       *>================================================================*
       *> TERMINATION — close files, display summary
       *>================================================================*
       3000-TERMINATE.
           CLOSE SORTED-TRANS-FILE
                 CUSTOMER-FILE
                 VALID-TRANS-FILE
                 REJECT-TRANS-FILE

           DISPLAY SPACES
           DISPLAY '-----------------------------------------'
           DISPLAY 'TXNVALID: Processing Summary'
           DISPLAY '-----------------------------------------'
           DISPLAY '  Transactions read:     ' WS-TRANS-READ-CTR
           DISPLAY '  Customers read:        ' WS-CUST-READ-CTR
           DISPLAY '  Valid written:         ' WS-VALID-CTR
           DISPLAY '  Rejected:              ' WS-REJECT-CTR
           DISPLAY '-----------------------------------------'

           EVALUATE TRUE
               WHEN WS-RETURN-CODE = 8
                   DISPLAY 'TXNVALID: ABEND — input file error'
                           ' (RC=8)'
               WHEN WS-REJECT-CTR > 0
                   MOVE 4 TO WS-RETURN-CODE
                   DISPLAY 'TXNVALID: Completed with rejects'
                           ' (RC=4)'
               WHEN OTHER
                   DISPLAY 'TXNVALID: Completed successfully'
                           ' (RC=0)'
           END-EVALUATE
           .

       *>================================================================*
       *> BUILD REJECT RECORD BASE — populate common fields
       *>================================================================*
       4100-BUILD-REJECT-BASE.
           INITIALIZE REJECT-RECORD
           MOVE WS-TRANS-ACCT-ID  TO REJ-ACCT-ID
           MOVE WS-TRANS-TYPE     TO REJ-TRANS-TYPE
           MOVE WS-TRANS-AMOUNT   TO REJ-AMOUNT
           MOVE WS-TRANS-DATE     TO REJ-DATE
           MOVE WS-TRANS-BRANCH   TO REJ-BRANCH
           MOVE WS-TRANS-SEQ      TO REJ-SEQ
           MOVE 'TXNVALID'        TO REJ-STEP
           .

       *>================================================================*
       *> WRITE REJECT RECORD — write and count
       *>================================================================*
       4200-WRITE-REJECT.
           WRITE REJECT-RECORD
           ADD 1 TO WS-REJECT-CTR
           .

       *>================================================================*
       *> READ TRANSACTION — advance sorted transaction file
       *>================================================================*
       5000-READ-TRANS.
           READ SORTED-TRANS-FILE
               INTO WS-CURRENT-TRANS
           END-READ

           EVALUATE WS-TRANS-STATUS
               WHEN '00'
                   ADD 1 TO WS-TRANS-READ-CTR
               WHEN '10'
                   SET TRANS-EOF TO TRUE
               WHEN OTHER
                   DISPLAY 'TXNVALID: READ ERROR on SORTED-TRANS.DAT'
                           ' STATUS=' WS-TRANS-STATUS
                   MOVE 8 TO WS-RETURN-CODE
                   SET TRANS-EOF TO TRUE
           END-EVALUATE
           .

       *>================================================================*
       *> READ CUSTOMER — advance customer master file
       *>================================================================*
       6000-READ-CUST.
           READ CUSTOMER-FILE
               INTO WS-CURRENT-CUST
           END-READ

           EVALUATE WS-CUSTF-STATUS
               WHEN '00'
                   ADD 1 TO WS-CUST-READ-CTR
               WHEN '10'
                   SET CUST-EOF TO TRUE
               WHEN OTHER
                   DISPLAY 'TXNVALID: READ ERROR on CUSTOMERS.DAT'
                           ' STATUS=' WS-CUSTF-STATUS
                   MOVE 8 TO WS-RETURN-CODE
                   SET CUST-EOF TO TRUE
           END-EVALUATE
           .
