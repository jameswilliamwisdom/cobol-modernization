      *> TXNSORT.cob — Step 1: Sort Transactions
      *> SORT verb showcase: SD work file, INPUT/OUTPUT PROCEDUREs,
      *> RELEASE, RETURN, composite ascending key (ACCT-ID + SEQ).
      *> RC 0=OK | RC 4=OK+filtered | RC 8=fatal
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TXNSORT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT UNSORTED-FILE
               ASSIGN TO 'data/TRANSACTIONS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-UNSORTED-STATUS.
           SELECT SORTED-FILE
               ASSIGN TO 'data/SORTED-TRANS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SORTED-STATUS.
           SELECT SORT-WORK-FILE
               ASSIGN TO 'SORTWORK'.

       DATA DIVISION.
       FILE SECTION.

       FD  UNSORTED-FILE.
       01  UNSORTED-RECORD                PIC X(47).

       FD  SORTED-FILE.
       01  SORTED-RECORD                  PIC X(47).

      *> Sort Description — SD, not FD. Keys inline to match TRANSREC.
       SD  SORT-WORK-FILE.
       01  SORT-RECORD.
           05  SORT-ACCT-ID              PIC X(8).
           05  SORT-TYPE                 PIC X(1).
           05  SORT-AMOUNT               PIC 9(7)V99.
           05  SORT-DATE                 PIC 9(8).
           05  SORT-BRANCH               PIC X(3).
           05  SORT-SEQ                  PIC 9(6).
           05  SORT-DESC                 PIC X(12).

       WORKING-STORAGE SECTION.
       COPY TRANSREC.

       01  WS-UNSORTED-STATUS            PIC XX.
       01  WS-SORTED-STATUS              PIC XX.
       01  WS-RECORDS-READ               PIC 9(7)  VALUE ZERO.
       01  WS-RECORDS-FILTERED           PIC 9(7)  VALUE ZERO.
       01  WS-RECORDS-WRITTEN            PIC 9(7)  VALUE ZERO.
       01  WS-EOF-FLAG                   PIC X(1)  VALUE 'N'.
           88  WS-EOF                              VALUE 'Y'.
           88  WS-NOT-EOF                          VALUE 'N'.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY '================================================'
           DISPLAY 'TXNSORT -- Step 1: Sort Transactions'
           DISPLAY '================================================'

           SORT SORT-WORK-FILE
               ON ASCENDING KEY SORT-ACCT-ID
               ON ASCENDING KEY SORT-SEQ
               INPUT  PROCEDURE IS 1000-INPUT-PROC
               OUTPUT PROCEDURE IS 2000-OUTPUT-PROC

           EVALUATE TRUE
               WHEN SORT-RETURN = ZERO
                   DISPLAY 'TXNSORT: SORT completed successfully.'
                   IF WS-RECORDS-FILTERED > ZERO
                       DISPLAY 'TXNSORT: WARNING - '
                           WS-RECORDS-FILTERED ' record(s) filtered.'
                       MOVE 4 TO RETURN-CODE
                   ELSE
                       MOVE 0 TO RETURN-CODE
                   END-IF
               WHEN OTHER
                   DISPLAY 'TXNSORT: FATAL - SORT failed, '
                       'SORT-RETURN=' SORT-RETURN
                   MOVE 8 TO RETURN-CODE
           END-EVALUATE

           DISPLAY ' '
           DISPLAY '--- TXNSORT Summary ---'
           DISPLAY '  Records read    : ' WS-RECORDS-READ
           DISPLAY '  Records filtered: ' WS-RECORDS-FILTERED
           DISPLAY '  Records written : ' WS-RECORDS-WRITTEN
           DISPLAY '  Return code     : ' RETURN-CODE
           DISPLAY '================================================'
           STOP RUN.

      *> INPUT PROCEDURE — Read, filter, release to sort
       1000-INPUT-PROC.
           OPEN INPUT UNSORTED-FILE
           IF WS-UNSORTED-STATUS NOT = '00'
               DISPLAY 'TXNSORT: FATAL - Cannot open '
                   'TRANSACTIONS.DAT, status=' WS-UNSORTED-STATUS
               MOVE 8 TO RETURN-CODE
               STOP RUN
           END-IF
           DISPLAY 'TXNSORT: Reading transactions...'
           SET WS-NOT-EOF TO TRUE
           PERFORM UNTIL WS-EOF
               READ UNSORTED-FILE INTO TRANSACTION-RECORD
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-RECORDS-READ
                       PERFORM 1100-FILTER-AND-RELEASE
               END-READ
           END-PERFORM
           CLOSE UNSORTED-FILE
           DISPLAY 'TXNSORT: Input complete - '
               WS-RECORDS-READ ' read, '
               WS-RECORDS-FILTERED ' filtered.'.

       1100-FILTER-AND-RELEASE.
           IF TRANS-ACCT-ID = SPACES OR TRANS-ACCT-ID = LOW-VALUES
               ADD 1 TO WS-RECORDS-FILTERED
               DISPLAY '  Filtered: blank ACCT-ID at record '
                   WS-RECORDS-READ
           ELSE IF TRANS-AMOUNT = ZERO
               ADD 1 TO WS-RECORDS-FILTERED
               DISPLAY '  Filtered: zero AMOUNT at record '
                   WS-RECORDS-READ ' acct=' TRANS-ACCT-ID
           ELSE
               RELEASE SORT-RECORD FROM TRANSACTION-RECORD
           END-IF.

      *> OUTPUT PROCEDURE — Return sorted records, write output
       2000-OUTPUT-PROC.
           OPEN OUTPUT SORTED-FILE
           IF WS-SORTED-STATUS NOT = '00'
               DISPLAY 'TXNSORT: FATAL - Cannot open '
                   'SORTED-TRANS.DAT, status=' WS-SORTED-STATUS
               MOVE 8 TO RETURN-CODE
               STOP RUN
           END-IF
           DISPLAY 'TXNSORT: Writing sorted records...'
           PERFORM UNTIL EXIT
               RETURN SORT-WORK-FILE
                   INTO TRANSACTION-RECORD
                   AT END
                       EXIT PERFORM
               END-RETURN
               WRITE SORTED-RECORD FROM TRANSACTION-RECORD
               ADD 1 TO WS-RECORDS-WRITTEN
               IF FUNCTION MOD(WS-RECORDS-WRITTEN, 1000) = ZERO
                   DISPLAY '  ... ' WS-RECORDS-WRITTEN
                       ' records written'
               END-IF
           END-PERFORM
           CLOSE SORTED-FILE
           DISPLAY 'TXNSORT: Output complete - '
               WS-RECORDS-WRITTEN ' records written.'.
