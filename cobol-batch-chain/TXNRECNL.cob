      *>================================================================*
      *> TXNRECNL.cob — Step 5: Reconciliation
      *> Reconciles old master, new master, and audit trail to
      *> verify arithmetic integrity of the batch update cycle.
      *>
      *> Inputs:  data/CUSTOMERS.DAT     (old master, binary seq)
      *>          data/NEW-CUSTOMERS.DAT  (new master, binary seq)
      *>          data/AUDIT-TRAIL.DAT    (audit records, binary seq)
      *> Output:  data/RECON-REPORT.TXT   (line sequential report)
      *>
      *> Return codes:
      *>   0 = all accounts balanced
      *>   8 = discrepancies found
      *>================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXNRECNL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OLD-MASTER-FILE
               ASSIGN TO 'data/CUSTOMERS.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OLD-STATUS.

           SELECT NEW-MASTER-FILE
               ASSIGN TO 'data/NEW-CUSTOMERS.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-NEW-STATUS.

           SELECT AUDIT-FILE
               ASSIGN TO 'data/AUDIT-TRAIL.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-AUD-STATUS.

           SELECT REPORT-FILE
               ASSIGN TO 'data/RECON-REPORT.TXT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  OLD-MASTER-FILE.
       COPY CUSTREC REPLACING ==CUSTOMER-RECORD==
                               BY ==OLD-MASTER-REC==.

       FD  NEW-MASTER-FILE.
       COPY CUSTREC REPLACING ==CUSTOMER-RECORD==
                               BY ==NEW-MASTER-REC==.

       FD  AUDIT-FILE.
       COPY AUDITREC REPLACING ==AUDIT-RECORD==
                                BY ==AUDIT-REC==.

       FD  REPORT-FILE.
       01  REPORT-LINE                 PIC X(132).

       WORKING-STORAGE SECTION.

      *>----------------------------------------------------------------*
      *> File status fields
      *>----------------------------------------------------------------*
       01  WS-OLD-STATUS               PIC XX.
       01  WS-NEW-STATUS               PIC XX.
       01  WS-AUD-STATUS               PIC XX.
       01  WS-RPT-STATUS               PIC XX.

      *>----------------------------------------------------------------*
      *> EOF flags
      *>----------------------------------------------------------------*
       01  WS-OLD-EOF                  PIC 9     VALUE 0.
           88  OLD-EOF                            VALUE 1.
       01  WS-NEW-EOF                  PIC 9     VALUE 0.
           88  NEW-EOF                            VALUE 1.
       01  WS-AUD-EOF                  PIC 9     VALUE 0.
           88  AUD-EOF                            VALUE 1.

      *>----------------------------------------------------------------*
      *> Working copies of current records
      *>----------------------------------------------------------------*
       COPY CUSTREC REPLACING ==CUSTOMER-RECORD==
                               BY ==WS-OLD-REC-DATA==.
       COPY CUSTREC REPLACING ==CUSTOMER-RECORD==
                               BY ==WS-NEW-REC-DATA==.
       COPY AUDITREC REPLACING ==AUDIT-RECORD==
                                BY ==WS-AUD-REC-DATA==.

      *>----------------------------------------------------------------*
      *> Per-account accumulators
      *>----------------------------------------------------------------*
       01  WS-SUM-DEPOSITS             PIC S9(9)V99 VALUE ZERO.
       01  WS-SUM-WITHDRAWALS          PIC S9(9)V99 VALUE ZERO.
       01  WS-SUM-INTEREST             PIC S9(9)V99 VALUE ZERO.
       01  WS-EXPECTED-BALANCE         PIC S9(9)V99 VALUE ZERO.
       01  WS-ACTUAL-BALANCE           PIC S9(9)V99 VALUE ZERO.
       01  WS-OLD-BALANCE              PIC S9(9)V99 VALUE ZERO.
       01  WS-DIFFERENCE               PIC S9(9)V99 VALUE ZERO.

      *>----------------------------------------------------------------*
      *> Counters
      *>----------------------------------------------------------------*
       01  WS-ACCTS-CHECKED            PIC 9(5)  VALUE ZERO.
       01  WS-ACCTS-BALANCED           PIC 9(5)  VALUE ZERO.
       01  WS-ACCTS-DISCREP            PIC 9(5)  VALUE ZERO.

      *>----------------------------------------------------------------*
      *> Current account being reconciled
      *>----------------------------------------------------------------*
       01  WS-CURR-ACCT                PIC X(8)  VALUE SPACES.
       01  WS-OLD-FOUND                PIC 9     VALUE 0.
           88  OLD-FOUND                          VALUE 1.

      *>----------------------------------------------------------------*
      *> Date field
      *>----------------------------------------------------------------*
       01  WS-CURRENT-DATE.
           05  WS-DATE-YYYY            PIC 9(4).
           05  WS-DATE-MM              PIC 9(2).
           05  WS-DATE-DD              PIC 9(2).
           05  FILLER                  PIC X(13).

      *>----------------------------------------------------------------*
      *> Report lines
      *>----------------------------------------------------------------*
       01  RPT-TITLE-LINE.
           05  FILLER                  PIC X(24)
               VALUE 'RECONCILIATION REPORT'.

       01  RPT-SEPARATOR.
           05  FILLER                  PIC X(50)
               VALUE '=================================================='.

       01  RPT-DATE-LINE.
           05  FILLER                  PIC X(10)
               VALUE 'RUN DATE: '.
           05  RPT-DATE-MM             PIC 99.
           05  FILLER                  PIC X     VALUE '/'.
           05  RPT-DATE-DD             PIC 99.
           05  FILLER                  PIC X     VALUE '/'.
           05  RPT-DATE-YYYY           PIC 9(4).

       01  RPT-HEADER-LINE.
           05  FILLER                  PIC X(10) VALUE 'ACCT-ID   '.
           05  FILLER                  PIC X(15)
               VALUE 'OLD-BALANCE    '.
           05  FILLER                  PIC X(13)
               VALUE 'DEPOSITS     '.
           05  FILLER                  PIC X(13)
               VALUE 'WITHDRAWALS  '.
           05  FILLER                  PIC X(13)
               VALUE 'INTEREST     '.
           05  FILLER                  PIC X(15)
               VALUE 'EXPECTED       '.
           05  FILLER                  PIC X(15)
               VALUE 'ACTUAL         '.
           05  FILLER                  PIC X(10)
               VALUE 'DIFF      '.

       01  RPT-DASH-LINE.
           05  FILLER                  PIC X(10) VALUE '--------- '.
           05  FILLER                  PIC X(15)
               VALUE '-------------- '.
           05  FILLER                  PIC X(13)
               VALUE '------------ '.
           05  FILLER                  PIC X(13)
               VALUE '------------ '.
           05  FILLER                  PIC X(13)
               VALUE '------------ '.
           05  FILLER                  PIC X(15)
               VALUE '-------------- '.
           05  FILLER                  PIC X(15)
               VALUE '-------------- '.
           05  FILLER                  PIC X(10)
               VALUE '----------'.

       01  RPT-DETAIL-LINE.
           05  RPT-ACCT-ID             PIC X(8).
           05  FILLER                  PIC XX    VALUE SPACES.
           05  RPT-OLD-BAL             PIC ZZZ,ZZ9.99-.
           05  FILLER                  PIC XX    VALUE SPACES.
           05  RPT-DEPOSITS            PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC XX    VALUE SPACES.
           05  RPT-WITHDRAWALS         PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC XX    VALUE SPACES.
           05  RPT-INTEREST            PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC XX    VALUE SPACES.
           05  RPT-EXPECTED            PIC ZZZ,ZZ9.99-.
           05  FILLER                  PIC XX    VALUE SPACES.
           05  RPT-ACTUAL              PIC ZZZ,ZZ9.99-.
           05  FILLER                  PIC XX    VALUE SPACES.
           05  RPT-DIFF                PIC ZZZ,ZZ9.99.

       01  RPT-SUMMARY-1.
           05  FILLER                  PIC X(19)
               VALUE 'ACCOUNTS CHECKED: '.
           05  RPT-SUM-CHECKED         PIC ZZ9.

       01  RPT-SUMMARY-2.
           05  FILLER                  PIC X(10)
               VALUE 'BALANCED: '.
           05  RPT-SUM-BALANCED        PIC ZZ9.
           05  FILLER                  PIC X(20)
               VALUE '    DISCREPANCIES: '.
           05  RPT-SUM-DISCREP         PIC ZZ9.

       01  RPT-STATUS-LINE.
           05  FILLER                  PIC X(25)
               VALUE 'RECONCILIATION STATUS: '.
           05  RPT-STATUS-TEXT         PIC X(25).

      *>----------------------------------------------------------------*
      *> Return code
      *>----------------------------------------------------------------*
       01  WS-RETURN-CODE              PIC 9     VALUE 0.

       PROCEDURE DIVISION.

       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-RECONCILE
               UNTIL NEW-EOF
           PERFORM 3000-WRITE-SUMMARY
           PERFORM 4000-TERMINATE
           STOP RUN.

      *>----------------------------------------------------------------*
      *> 1000 — Open files, prime reads, write report headers
      *>----------------------------------------------------------------*
       1000-INITIALIZE.
           OPEN INPUT  OLD-MASTER-FILE
                       NEW-MASTER-FILE
                       AUDIT-FILE
               OUTPUT  REPORT-FILE

           PERFORM 1100-READ-OLD-MASTER
           PERFORM 1200-READ-NEW-MASTER
           PERFORM 1300-READ-AUDIT

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE

           WRITE REPORT-LINE FROM RPT-TITLE-LINE
           WRITE REPORT-LINE FROM RPT-SEPARATOR
           MOVE WS-DATE-MM   TO RPT-DATE-MM
           MOVE WS-DATE-DD   TO RPT-DATE-DD
           MOVE WS-DATE-YYYY TO RPT-DATE-YYYY
           WRITE REPORT-LINE FROM RPT-DATE-LINE
           WRITE REPORT-LINE FROM SPACES
           WRITE REPORT-LINE FROM RPT-HEADER-LINE
           WRITE REPORT-LINE FROM RPT-DASH-LINE.

      *>----------------------------------------------------------------*
      *> 1100 — Read next old master record
      *>----------------------------------------------------------------*
       1100-READ-OLD-MASTER.
           READ OLD-MASTER-FILE INTO WS-OLD-REC-DATA
               AT END
                   SET OLD-EOF TO TRUE
           END-READ.

      *>----------------------------------------------------------------*
      *> 1200 — Read next new master record
      *>----------------------------------------------------------------*
       1200-READ-NEW-MASTER.
           READ NEW-MASTER-FILE INTO WS-NEW-REC-DATA
               AT END
                   SET NEW-EOF TO TRUE
           END-READ.

      *>----------------------------------------------------------------*
      *> 1300 — Read next audit record
      *>----------------------------------------------------------------*
       1300-READ-AUDIT.
           READ AUDIT-FILE INTO WS-AUD-REC-DATA
               AT END
                   SET AUD-EOF TO TRUE
           END-READ.

      *>----------------------------------------------------------------*
      *> 2000 — Reconcile one new-master account
      *>----------------------------------------------------------------*
       2000-RECONCILE.
           MOVE CUST-ID OF WS-NEW-REC-DATA
               TO WS-CURR-ACCT
           ADD 1 TO WS-ACCTS-CHECKED

      *>    — Reset per-account accumulators
           MOVE ZERO TO WS-SUM-DEPOSITS
           MOVE ZERO TO WS-SUM-WITHDRAWALS
           MOVE ZERO TO WS-SUM-INTEREST
           MOVE ZERO TO WS-OLD-FOUND

      *>    — Advance old master to match or pass
           PERFORM 2100-FIND-OLD-MASTER

      *>    — Accumulate audit entries for this account
           PERFORM 2200-ACCUMULATE-AUDIT
               UNTIL AUD-EOF
                  OR AUD-ACCT-ID OF WS-AUD-REC-DATA
                     > WS-CURR-ACCT

      *>    — Compute expected balance and compare
           PERFORM 2300-VERIFY-BALANCE

      *>    — Read next new master
           PERFORM 1200-READ-NEW-MASTER.

      *>----------------------------------------------------------------*
      *> 2100 — Advance old master until matching or past current acct
      *>----------------------------------------------------------------*
       2100-FIND-OLD-MASTER.
           PERFORM UNTIL OLD-EOF
                      OR CUST-ID OF WS-OLD-REC-DATA
                         >= WS-CURR-ACCT
               PERFORM 1100-READ-OLD-MASTER
           END-PERFORM

           IF NOT OLD-EOF
               IF CUST-ID OF WS-OLD-REC-DATA = WS-CURR-ACCT
                   SET OLD-FOUND TO TRUE
                   MOVE CUST-BALANCE OF WS-OLD-REC-DATA
                       TO WS-OLD-BALANCE
               ELSE
                   MOVE ZERO TO WS-OLD-BALANCE
               END-IF
           ELSE
               MOVE ZERO TO WS-OLD-BALANCE
           END-IF.

      *>----------------------------------------------------------------*
      *> 2200 — Accumulate audit records for current account
      *>----------------------------------------------------------------*
       2200-ACCUMULATE-AUDIT.
           IF AUD-ACCT-ID OF WS-AUD-REC-DATA = WS-CURR-ACCT
               IF AUD-APPLIED OF WS-AUD-REC-DATA
                   EVALUATE AUD-TRANS-TYPE OF WS-AUD-REC-DATA
                       WHEN 'D'
                           ADD AUD-AMOUNT OF WS-AUD-REC-DATA
                               TO WS-SUM-DEPOSITS
                       WHEN 'W'
                           ADD AUD-AMOUNT OF WS-AUD-REC-DATA
                               TO WS-SUM-WITHDRAWALS
                       WHEN 'I'
                           ADD AUD-AMOUNT OF WS-AUD-REC-DATA
                               TO WS-SUM-INTEREST
                   END-EVALUATE
               END-IF
               PERFORM 1300-READ-AUDIT
           END-IF.

      *>----------------------------------------------------------------*
      *> 2300 — Compute expected balance and verify against actual
      *>----------------------------------------------------------------*
       2300-VERIFY-BALANCE.
           COMPUTE WS-EXPECTED-BALANCE =
               WS-OLD-BALANCE
               + WS-SUM-DEPOSITS
               - WS-SUM-WITHDRAWALS
               + WS-SUM-INTEREST

           MOVE CUST-BALANCE OF WS-NEW-REC-DATA
               TO WS-ACTUAL-BALANCE

           COMPUTE WS-DIFFERENCE =
               WS-ACTUAL-BALANCE - WS-EXPECTED-BALANCE

           IF WS-DIFFERENCE NOT = ZERO
               ADD 1 TO WS-ACCTS-DISCREP
               MOVE 8 TO WS-RETURN-CODE
               PERFORM 2310-WRITE-DETAIL
           ELSE
               ADD 1 TO WS-ACCTS-BALANCED
           END-IF.

      *>----------------------------------------------------------------*
      *> 2310 — Write detail line for discrepant account
      *>----------------------------------------------------------------*
       2310-WRITE-DETAIL.
           INITIALIZE RPT-DETAIL-LINE
           MOVE WS-CURR-ACCT          TO RPT-ACCT-ID
           MOVE WS-OLD-BALANCE        TO RPT-OLD-BAL
           MOVE WS-SUM-DEPOSITS       TO RPT-DEPOSITS
           MOVE WS-SUM-WITHDRAWALS    TO RPT-WITHDRAWALS
           MOVE WS-SUM-INTEREST       TO RPT-INTEREST
           MOVE WS-EXPECTED-BALANCE   TO RPT-EXPECTED
           MOVE WS-ACTUAL-BALANCE     TO RPT-ACTUAL
           MOVE WS-DIFFERENCE         TO RPT-DIFF
           WRITE REPORT-LINE FROM RPT-DETAIL-LINE.

      *>----------------------------------------------------------------*
      *> 3000 — Write summary and status
      *>----------------------------------------------------------------*
       3000-WRITE-SUMMARY.
           WRITE REPORT-LINE FROM SPACES

           MOVE WS-ACCTS-CHECKED  TO RPT-SUM-CHECKED
           WRITE REPORT-LINE FROM RPT-SUMMARY-1

           MOVE WS-ACCTS-BALANCED TO RPT-SUM-BALANCED
           MOVE WS-ACCTS-DISCREP  TO RPT-SUM-DISCREP
           WRITE REPORT-LINE FROM RPT-SUMMARY-2

           WRITE REPORT-LINE FROM SPACES

           IF WS-ACCTS-DISCREP = ZERO
               MOVE 'BALANCED' TO RPT-STATUS-TEXT
           ELSE
               MOVE 'DISCREPANCIES FOUND' TO RPT-STATUS-TEXT
           END-IF
           WRITE REPORT-LINE FROM RPT-STATUS-LINE.

      *>----------------------------------------------------------------*
      *> 4000 — Close files and set return code
      *>----------------------------------------------------------------*
       4000-TERMINATE.
           CLOSE OLD-MASTER-FILE
                 NEW-MASTER-FILE
                 AUDIT-FILE
                 REPORT-FILE
           MOVE WS-RETURN-CODE TO RETURN-CODE.
