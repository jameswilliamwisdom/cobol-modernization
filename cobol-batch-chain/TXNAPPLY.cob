      *> TXNAPPLY.cob - Step 3: Apply Transactions to Customer Master
      *> Balanced-line update of sorted transactions against master.
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TXNAPPLY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VALID-TRANS-FILE
               ASSIGN TO 'data/VALID-TRANS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TRANS-STATUS.
           SELECT OLD-MASTER-FILE
               ASSIGN TO 'data/CUSTOMERS.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-MASTER-STATUS.
           SELECT NEW-MASTER-FILE
               ASSIGN TO 'data/NEW-CUSTOMERS.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-NEW-MASTER-STATUS.
           SELECT AUDIT-FILE
               ASSIGN TO 'data/AUDIT-TRAIL.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-AUDIT-STATUS.
           SELECT REJECT-FILE
               ASSIGN TO 'data/REJECT-TRANS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-REJECT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  VALID-TRANS-FILE.
           COPY TRANSREC.
       FD  OLD-MASTER-FILE.
           COPY CUSTREC.
       FD  NEW-MASTER-FILE.
       01  NEW-CUSTOMER-RECORD           PIC X(68).
       FD  AUDIT-FILE.
           COPY AUDITREC.
       FD  REJECT-FILE.
           COPY REJECTREC.

       WORKING-STORAGE SECTION.
       01  WS-TRANS-STATUS               PIC XX.
       01  WS-MASTER-STATUS              PIC XX.
       01  WS-NEW-MASTER-STATUS          PIC XX.
       01  WS-AUDIT-STATUS               PIC XX.
       01  WS-REJECT-STATUS              PIC XX.
       01  WS-TRANS-EOF                  PIC 9     VALUE 0.
           88  TRANS-IS-EOF                        VALUE 1.
           88  TRANS-NOT-EOF                       VALUE 0.
       01  WS-MASTER-EOF                 PIC 9     VALUE 0.
           88  MASTER-IS-EOF                       VALUE 1.
           88  MASTER-NOT-EOF                      VALUE 0.
       01  WS-HIGH-KEY                   PIC X(8)  VALUE HIGH-VALUES.
       01  WS-CURRENT-MASTER.
           05  WS-CM-ID                  PIC X(8).
           05  WS-CM-NAME                PIC X(25).
           05  WS-CM-BRANCH              PIC X(3).
           05  WS-CM-ACCT-TYPE           PIC X(1).
           05  WS-CM-STATUS              PIC X(1).
           05  WS-CM-BALANCE             PIC S9(7)V99 COMP-3.
           05  WS-CM-OPEN-DATE           PIC 9(8).
           05  WS-CM-LAST-ACTIVITY       PIC 9(8).
           05  WS-CM-FILLER              PIC X(9).
       01  WS-CURRENT-ACCT               PIC X(8).
       01  WS-TRANS-KEY                   PIC X(8).
       01  WS-MASTER-KEY                  PIC X(8).
       01  WS-COMPUTED-INTEREST           PIC S9(7)V99.
       01  WS-INTEREST-RATE              PIC V9(4)  VALUE 0.0125.
       01  WS-CTR-MASTERS-READ           PIC 9(7)  VALUE 0.
       01  WS-CTR-TRANS-APPLIED          PIC 9(7)  VALUE 0.
       01  WS-CTR-DEPOSITS               PIC 9(7)  VALUE 0.
       01  WS-CTR-WITHDRAWALS            PIC 9(7)  VALUE 0.
       01  WS-CTR-INTEREST               PIC 9(7)  VALUE 0.
       01  WS-CTR-OVERDRAFTS             PIC 9(7)  VALUE 0.
       01  WS-CTR-MASTERS-WRITTEN        PIC 9(7)  VALUE 0.
       01  WS-RETURN-CODE                PIC 9     VALUE 0.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-BALANCED-LINE-UPDATE
           PERFORM 9000-FINALIZE
           STOP RUN.

       1000-INITIALIZE.
           DISPLAY '--- TXNAPPLY: Step 3 - Apply Transactions ---'
           OPEN INPUT  VALID-TRANS-FILE
           IF WS-TRANS-STATUS NOT = '00'
               DISPLAY 'ERROR: Open VALID-TRANS status=' WS-TRANS-STATUS
               MOVE 8 TO WS-RETURN-CODE
               PERFORM 9000-FINALIZE  STOP RUN
           END-IF
           OPEN INPUT  OLD-MASTER-FILE
           IF WS-MASTER-STATUS NOT = '00'
               DISPLAY 'ERROR: Open CUSTOMERS status=' WS-MASTER-STATUS
               MOVE 8 TO WS-RETURN-CODE
               PERFORM 9000-FINALIZE  STOP RUN
           END-IF
           OPEN OUTPUT NEW-MASTER-FILE
           IF WS-NEW-MASTER-STATUS NOT = '00'
               DISPLAY 'ERROR: Open NEW-CUSTOMERS status='
                   WS-NEW-MASTER-STATUS
               MOVE 8 TO WS-RETURN-CODE
               PERFORM 9000-FINALIZE  STOP RUN
           END-IF
           OPEN OUTPUT AUDIT-FILE
           IF WS-AUDIT-STATUS NOT = '00'
               DISPLAY 'ERROR: Open AUDIT-TRAIL status=' WS-AUDIT-STATUS
               MOVE 8 TO WS-RETURN-CODE
               PERFORM 9000-FINALIZE  STOP RUN
           END-IF
           OPEN EXTEND REJECT-FILE
           IF WS-REJECT-STATUS NOT = '00'
               DISPLAY 'ERROR: Open REJECT-TRANS status='
                   WS-REJECT-STATUS
               MOVE 8 TO WS-RETURN-CODE
               PERFORM 9000-FINALIZE  STOP RUN
           END-IF
           PERFORM 8100-READ-MASTER
           PERFORM 8200-READ-TRANS.

       2000-BALANCED-LINE-UPDATE.
           PERFORM UNTIL WS-MASTER-KEY = WS-HIGH-KEY
                     AND WS-TRANS-KEY  = WS-HIGH-KEY
               EVALUATE TRUE
                   WHEN WS-MASTER-KEY < WS-TRANS-KEY
                       PERFORM 3000-COPY-MASTER-THROUGH
                   WHEN WS-MASTER-KEY = WS-TRANS-KEY
                       PERFORM 4000-PROCESS-MATCHING
                   WHEN WS-TRANS-KEY < WS-MASTER-KEY
                       PERFORM 5000-REJECT-ORPHAN-TRANS
               END-EVALUATE
           END-PERFORM.

       3000-COPY-MASTER-THROUGH.
           MOVE CUSTOMER-RECORD TO WS-CURRENT-MASTER
           PERFORM 8300-WRITE-NEW-MASTER
           PERFORM 8100-READ-MASTER.

       4000-PROCESS-MATCHING.
           MOVE CUSTOMER-RECORD TO WS-CURRENT-MASTER
           MOVE WS-CM-ID        TO WS-CURRENT-ACCT
           PERFORM 4100-APPLY-ONE-TRANS
               UNTIL WS-TRANS-KEY NOT = WS-CURRENT-ACCT
           PERFORM 8300-WRITE-NEW-MASTER
           PERFORM 8100-READ-MASTER.

       4100-APPLY-ONE-TRANS.
           INITIALIZE AUDIT-RECORD
           MOVE TRANS-ACCT-ID   TO AUD-ACCT-ID
           MOVE TRANS-TYPE      TO AUD-TRANS-TYPE
           MOVE TRANS-AMOUNT    TO AUD-AMOUNT
           MOVE TRANS-DATE      TO AUD-DATE
           MOVE TRANS-BRANCH    TO AUD-BRANCH
           MOVE TRANS-SEQ       TO AUD-SEQ
           MOVE WS-CM-BALANCE   TO AUD-OLD-BALANCE
           EVALUATE TRUE
               WHEN TRANS-DEPOSIT
                   PERFORM 4110-APPLY-DEPOSIT
               WHEN TRANS-WITHDRAW
                   PERFORM 4120-APPLY-WITHDRAW
               WHEN TRANS-INTEREST
                   PERFORM 4130-APPLY-INTEREST
               WHEN OTHER
                   PERFORM 5000-REJECT-ORPHAN-TRANS
                   PERFORM 8200-READ-TRANS
                   GO TO 4100-EXIT
           END-EVALUATE
           PERFORM 8200-READ-TRANS.
       4100-EXIT.
           EXIT.

       4110-APPLY-DEPOSIT.
           ADD TRANS-AMOUNT TO WS-CM-BALANCE
           MOVE WS-CM-BALANCE TO AUD-NEW-BALANCE
           SET  AUD-APPLIED   TO TRUE
           PERFORM 8400-WRITE-AUDIT
           ADD 1 TO WS-CTR-DEPOSITS
           ADD 1 TO WS-CTR-TRANS-APPLIED.

       4120-APPLY-WITHDRAW.
           IF TRANS-AMOUNT > WS-CM-BALANCE
               PERFORM 4125-REJECT-OVERDRAFT
           ELSE
               SUBTRACT TRANS-AMOUNT FROM WS-CM-BALANCE
               MOVE WS-CM-BALANCE TO AUD-NEW-BALANCE
               SET  AUD-APPLIED   TO TRUE
               PERFORM 8400-WRITE-AUDIT
               ADD 1 TO WS-CTR-WITHDRAWALS
               ADD 1 TO WS-CTR-TRANS-APPLIED
           END-IF.

       4125-REJECT-OVERDRAFT.
           INITIALIZE REJECT-RECORD
           MOVE TRANS-ACCT-ID   TO REJ-ACCT-ID
           MOVE TRANS-TYPE      TO REJ-TRANS-TYPE
           MOVE TRANS-AMOUNT    TO REJ-AMOUNT
           MOVE TRANS-DATE      TO REJ-DATE
           MOVE TRANS-BRANCH    TO REJ-BRANCH
           MOVE TRANS-SEQ       TO REJ-SEQ
           SET  REJ-OVERDRAFT   TO TRUE
           MOVE 'TXNAPPLY'      TO REJ-STEP
           PERFORM 8500-WRITE-REJECT
           ADD 1 TO WS-CTR-OVERDRAFTS
           MOVE WS-CM-BALANCE   TO AUD-NEW-BALANCE
           SET  AUD-REJECTED    TO TRUE
           PERFORM 8400-WRITE-AUDIT
           IF WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF.

       4130-APPLY-INTEREST.
           COMPUTE WS-COMPUTED-INTEREST =
               WS-CM-BALANCE * WS-INTEREST-RATE
           ADD WS-COMPUTED-INTEREST TO WS-CM-BALANCE
           MOVE WS-COMPUTED-INTEREST TO AUD-AMOUNT
           MOVE WS-CM-BALANCE TO AUD-NEW-BALANCE
           SET  AUD-APPLIED   TO TRUE
           PERFORM 8400-WRITE-AUDIT
           ADD 1 TO WS-CTR-INTEREST
           ADD 1 TO WS-CTR-TRANS-APPLIED.

       5000-REJECT-ORPHAN-TRANS.
           INITIALIZE REJECT-RECORD
           MOVE TRANS-ACCT-ID   TO REJ-ACCT-ID
           MOVE TRANS-TYPE      TO REJ-TRANS-TYPE
           MOVE TRANS-AMOUNT    TO REJ-AMOUNT
           MOVE TRANS-DATE      TO REJ-DATE
           MOVE TRANS-BRANCH    TO REJ-BRANCH
           MOVE TRANS-SEQ       TO REJ-SEQ
           SET  REJ-NO-ACCOUNT  TO TRUE
           MOVE 'TXNAPPLY'      TO REJ-STEP
           PERFORM 8500-WRITE-REJECT
           INITIALIZE AUDIT-RECORD
           MOVE TRANS-ACCT-ID   TO AUD-ACCT-ID
           MOVE TRANS-TYPE      TO AUD-TRANS-TYPE
           MOVE TRANS-AMOUNT    TO AUD-AMOUNT
           MOVE TRANS-DATE      TO AUD-DATE
           MOVE ZEROS           TO AUD-OLD-BALANCE
           MOVE ZEROS           TO AUD-NEW-BALANCE
           SET  AUD-REJECTED    TO TRUE
           MOVE TRANS-BRANCH    TO AUD-BRANCH
           MOVE TRANS-SEQ       TO AUD-SEQ
           PERFORM 8400-WRITE-AUDIT
           IF WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF
           PERFORM 8200-READ-TRANS.

       8100-READ-MASTER.
           READ OLD-MASTER-FILE
               AT END
                   SET MASTER-IS-EOF TO TRUE
                   MOVE WS-HIGH-KEY  TO WS-MASTER-KEY
               NOT AT END
                   ADD 1 TO WS-CTR-MASTERS-READ
                   MOVE CUST-ID      TO WS-MASTER-KEY
           END-READ
           IF WS-MASTER-STATUS NOT = '00'
              AND WS-MASTER-STATUS NOT = '10'
               DISPLAY 'ERROR: Read CUSTOMERS status=' WS-MASTER-STATUS
               MOVE 8 TO WS-RETURN-CODE
               PERFORM 9000-FINALIZE  STOP RUN
           END-IF.

       8200-READ-TRANS.
           READ VALID-TRANS-FILE
               AT END
                   SET TRANS-IS-EOF  TO TRUE
                   MOVE WS-HIGH-KEY  TO WS-TRANS-KEY
               NOT AT END
                   MOVE TRANS-ACCT-ID TO WS-TRANS-KEY
           END-READ
           IF WS-TRANS-STATUS NOT = '00'
              AND WS-TRANS-STATUS NOT = '10'
               DISPLAY 'ERROR: Read VALID-TRANS status=' WS-TRANS-STATUS
               MOVE 8 TO WS-RETURN-CODE
               PERFORM 9000-FINALIZE  STOP RUN
           END-IF.

       8300-WRITE-NEW-MASTER.
           WRITE NEW-CUSTOMER-RECORD FROM WS-CURRENT-MASTER
           IF WS-NEW-MASTER-STATUS NOT = '00'
               DISPLAY 'ERROR: Write NEW-CUSTOMERS status='
                   WS-NEW-MASTER-STATUS
               MOVE 8 TO WS-RETURN-CODE
               PERFORM 9000-FINALIZE  STOP RUN
           END-IF
           ADD 1 TO WS-CTR-MASTERS-WRITTEN.

       8400-WRITE-AUDIT.
           WRITE AUDIT-RECORD
           IF WS-AUDIT-STATUS NOT = '00'
               DISPLAY 'ERROR: Write AUDIT-TRAIL status='
                   WS-AUDIT-STATUS
               MOVE 8 TO WS-RETURN-CODE
               PERFORM 9000-FINALIZE  STOP RUN
           END-IF.

       8500-WRITE-REJECT.
           WRITE REJECT-RECORD
           IF WS-REJECT-STATUS NOT = '00'
               DISPLAY 'ERROR: Write REJECT-TRANS status='
                   WS-REJECT-STATUS
               MOVE 8 TO WS-RETURN-CODE
               PERFORM 9000-FINALIZE  STOP RUN
           END-IF.

       9000-FINALIZE.
           DISPLAY '--- TXNAPPLY Summary ---'
           DISPLAY '  Masters read     : ' WS-CTR-MASTERS-READ
           DISPLAY '  Trans applied    : ' WS-CTR-TRANS-APPLIED
           DISPLAY '    Deposits       : ' WS-CTR-DEPOSITS
           DISPLAY '    Withdrawals    : ' WS-CTR-WITHDRAWALS
           DISPLAY '    Interest       : ' WS-CTR-INTEREST
           DISPLAY '  Overdraft rejects: ' WS-CTR-OVERDRAFTS
           DISPLAY '  Masters written  : ' WS-CTR-MASTERS-WRITTEN
           DISPLAY '  Return code      : ' WS-RETURN-CODE
           DISPLAY '--- End TXNAPPLY ---'
           CLOSE VALID-TRANS-FILE
                 OLD-MASTER-FILE
                 NEW-MASTER-FILE
                 AUDIT-FILE
                 REJECT-FILE
           MOVE WS-RETURN-CODE TO RETURN-CODE.
