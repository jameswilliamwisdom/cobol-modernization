       *>================================================================*
       *> TXNRPT.cob — Step 4: Daily Transaction Report
       *> Reads the sorted audit trail and produces a formatted
       *> control-break report with account, branch, and grand totals.
       *>
       *> Input:  data/AUDIT-TRAIL.DAT  (SEQUENTIAL, binary)
       *> Output: data/DAILY-REPORT.TXT (LINE SEQUENTIAL, print)
       *>
       *> Control breaks:
       *>   Level 1 — Account change   (AUD-ACCT-ID)
       *>   Level 2 — Branch change     (AUD-BRANCH)
       *>   Level 3 — End of file       (grand total)
       *>
       *> Return codes:  0 = success,  8 = input file error
       *>================================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TXNRPT.
       AUTHOR.      BATCH-CHAIN-PIPELINE.
       DATE-WRITTEN. 2026-03-02.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AUDIT-FILE
               ASSIGN TO 'data/AUDIT-TRAIL.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-AUDIT-STATUS.

           SELECT REPORT-FILE
               ASSIGN TO 'data/DAILY-REPORT.TXT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-REPORT-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  AUDIT-FILE.
           COPY AUDITREC.

       FD  REPORT-FILE.
       01  REPORT-LINE                PIC X(100).

       WORKING-STORAGE SECTION.

       *>----------------------------------------------------------------*
       *> File status and control flags
       *>----------------------------------------------------------------*
       01  WS-AUDIT-STATUS            PIC XX.
       01  WS-REPORT-STATUS           PIC XX.
       01  WS-EOF-FLAG                PIC X(1)     VALUE 'N'.
           88  END-OF-FILE                         VALUE 'Y'.
           88  NOT-END-OF-FILE                     VALUE 'N'.

       *>----------------------------------------------------------------*
       *> Page and line control
       *>----------------------------------------------------------------*
       01  WS-LINE-COUNT              PIC 99       VALUE 99.
       01  WS-PAGE-COUNT              PIC 999      VALUE ZERO.
       01  WS-LINES-PER-PAGE         PIC 99       VALUE 55.

       *>----------------------------------------------------------------*
       *> Previous-key fields for control breaks
       *>----------------------------------------------------------------*
       01  WS-PREV-ACCT              PIC X(8)     VALUE SPACES.
       01  WS-PREV-BRANCH            PIC X(3)     VALUE SPACES.
       01  WS-FIRST-RECORD           PIC X(1)     VALUE 'Y'.
           88  IS-FIRST-RECORD                     VALUE 'Y'.
           88  NOT-FIRST-RECORD                    VALUE 'N'.

       *>----------------------------------------------------------------*
       *> Formatted date for RUN DATE
       *>----------------------------------------------------------------*
       01  WS-CURRENT-DATE.
           05  WS-CURR-YYYY          PIC 9(4).
           05  WS-CURR-MM            PIC 9(2).
           05  WS-CURR-DD            PIC 9(2).
           05  FILLER                PIC X(13).

       *>----------------------------------------------------------------*
       *> Account-level accumulators
       *>----------------------------------------------------------------*
       01  WS-ACCT-DEP-COUNT         PIC 99       VALUE ZERO.
       01  WS-ACCT-WDR-COUNT         PIC 99       VALUE ZERO.
       01  WS-ACCT-INT-COUNT         PIC 99       VALUE ZERO.
       01  WS-ACCT-DEP-AMT           PIC S9(9)V99 VALUE ZERO.
       01  WS-ACCT-WDR-AMT           PIC S9(9)V99 VALUE ZERO.
       01  WS-ACCT-INT-AMT           PIC S9(9)V99 VALUE ZERO.
       01  WS-ACCT-NET-CHANGE        PIC S9(9)V99 VALUE ZERO.

       *>----------------------------------------------------------------*
       *> Branch-level accumulators
       *>----------------------------------------------------------------*
       01  WS-BRNCH-DEP-COUNT        PIC 99       VALUE ZERO.
       01  WS-BRNCH-WDR-COUNT        PIC 99       VALUE ZERO.
       01  WS-BRNCH-INT-COUNT        PIC 99       VALUE ZERO.
       01  WS-BRNCH-DEP-AMT          PIC S9(9)V99 VALUE ZERO.
       01  WS-BRNCH-WDR-AMT          PIC S9(9)V99 VALUE ZERO.
       01  WS-BRNCH-INT-AMT          PIC S9(9)V99 VALUE ZERO.
       01  WS-BRNCH-NET-CHANGE       PIC S9(9)V99 VALUE ZERO.

       *>----------------------------------------------------------------*
       *> Grand-level accumulators
       *>----------------------------------------------------------------*
       01  WS-GRAND-DEP-COUNT        PIC 999      VALUE ZERO.
       01  WS-GRAND-WDR-COUNT        PIC 999      VALUE ZERO.
       01  WS-GRAND-INT-COUNT        PIC 999      VALUE ZERO.
       01  WS-GRAND-DEP-AMT          PIC S9(9)V99 VALUE ZERO.
       01  WS-GRAND-WDR-AMT          PIC S9(9)V99 VALUE ZERO.
       01  WS-GRAND-INT-AMT          PIC S9(9)V99 VALUE ZERO.
       01  WS-GRAND-NET-CHANGE       PIC S9(9)V99 VALUE ZERO.
       01  WS-GRAND-TRANS-COUNT      PIC 999      VALUE ZERO.
       01  WS-GRAND-APPLIED-COUNT    PIC 999      VALUE ZERO.
       01  WS-GRAND-REJECTED-COUNT   PIC 999      VALUE ZERO.

       *>----------------------------------------------------------------*
       *> Page header lines
       *>----------------------------------------------------------------*
       01  HDR-LINE-1.
           05  FILLER                 PIC X(38)
               VALUE 'DAILY TRANSACTION REPORT               '.
           05  FILLER                 PIC X(6)
               VALUE 'PAGE: '.
           05  HDR-PAGE-NUM           PIC ZZ9.
           05  FILLER                 PIC X(53)
               VALUE SPACES.

       01  HDR-LINE-2.
           05  FILLER                 PIC X(10)
               VALUE 'RUN DATE: '.
           05  HDR-RUN-MM             PIC 99.
           05  FILLER                 PIC X       VALUE '/'.
           05  HDR-RUN-DD             PIC 99.
           05  FILLER                 PIC X       VALUE '/'.
           05  HDR-RUN-YYYY           PIC 9(4).
           05  FILLER                 PIC X(80)
               VALUE SPACES.

       01  HDR-LINE-3.
           05  FILLER                 PIC X(100)
               VALUE 'ACCT-ID   TYPE  AMOUNT       OLD-BAL  '
               &    '    NEW-BAL      DATE       STATUS'.

       01  HDR-LINE-4.
           05  FILLER                 PIC X(100)
               VALUE '--------- ---- ------------ ---------'
               &    '--- ------------ ---------- --------'.

       *>----------------------------------------------------------------*
       *> Detail print line
       *>----------------------------------------------------------------*
       01  DTL-LINE.
           05  DTL-ACCT-ID           PIC X(8).
           05  FILLER                PIC X(3)     VALUE SPACES.
           05  DTL-TRANS-TYPE        PIC X(1).
           05  FILLER                PIC X(3)     VALUE SPACES.
           05  DTL-AMOUNT            PIC ZZZ,ZZ9.99.
           05  FILLER                PIC X(3)     VALUE SPACES.
           05  DTL-OLD-BAL           PIC ZZZ,ZZ9.99-.
           05  FILLER                PIC X(2)     VALUE SPACES.
           05  DTL-NEW-BAL           PIC ZZZ,ZZ9.99-.
           05  FILLER                PIC X(2)     VALUE SPACES.
           05  DTL-DATE-MM           PIC 99.
           05  FILLER                PIC X        VALUE '/'.
           05  DTL-DATE-DD           PIC 99.
           05  FILLER                PIC X        VALUE '/'.
           05  DTL-DATE-YYYY         PIC 9(4).
           05  FILLER                PIC XX       VALUE SPACES.
           05  DTL-STATUS            PIC X(8).
           05  FILLER                PIC X(33)    VALUE SPACES.

       *>----------------------------------------------------------------*
       *> Account subtotal line
       *>----------------------------------------------------------------*
       01  ACCT-SUB-LINE-1.
           05  FILLER                PIC X(14)
               VALUE '  ** ACCOUNT '.
           05  ACCT-SUB-ID           PIC X(8).
           05  FILLER                PIC X(2)     VALUE ': '.
           05  ACCT-SUB-DEP-CT       PIC 99.
           05  FILLER                PIC X(11)
               VALUE ' DEPOSITS  '.
           05  ACCT-SUB-WDR-CT       PIC 99.
           05  FILLER                PIC X(15)
               VALUE ' WITHDRAWALS  '.
           05  ACCT-SUB-INT-CT       PIC 99.
           05  FILLER                PIC X(9)
               VALUE ' INTEREST'.
           05  FILLER                PIC X(35)    VALUE SPACES.

       01  ACCT-SUB-LINE-2.
           05  FILLER                PIC X(18)
               VALUE '     NET CHANGE: '.
           05  ACCT-SUB-NET          PIC ZZZ,ZZ9.99-.
           05  FILLER                PIC X(69)    VALUE SPACES.

       *>----------------------------------------------------------------*
       *> Branch subtotal lines
       *>----------------------------------------------------------------*
       01  BRNCH-SUB-LINE-1.
           05  FILLER                PIC X(14)
               VALUE '  *** BRANCH '.
           05  BRNCH-SUB-ID          PIC X(3).
           05  FILLER                PIC X(2)     VALUE ': '.
           05  BRNCH-SUB-DEP-CT      PIC 99.
           05  FILLER                PIC X(11)
               VALUE ' DEPOSITS  '.
           05  BRNCH-SUB-WDR-CT      PIC 99.
           05  FILLER                PIC X(15)
               VALUE ' WITHDRAWALS  '.
           05  BRNCH-SUB-INT-CT      PIC 99.
           05  FILLER                PIC X(9)
               VALUE ' INTEREST'.
           05  FILLER                PIC X(40)    VALUE SPACES.

       01  BRNCH-SUB-LINE-2.
           05  FILLER                PIC X(22)
               VALUE '      TOTAL DEPOSITS: '.
           05  BRNCH-SUB-DEP-AMT     PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                PIC X(20)
               VALUE '  TOTAL WITHDRAWALS:'.
           05  FILLER                PIC X        VALUE SPACE.
           05  BRNCH-SUB-WDR-AMT     PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                PIC X(31)    VALUE SPACES.

       01  BRNCH-SUB-LINE-3.
           05  FILLER                PIC X(18)
               VALUE '      NET CHANGE: '.
           05  BRNCH-SUB-NET         PIC ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                PIC X(67)    VALUE SPACES.

       *>----------------------------------------------------------------*
       *> Grand total lines
       *>----------------------------------------------------------------*
       01  GRAND-LINE-1.
           05  FILLER                PIC X(19)
               VALUE '  **** GRAND TOTAL:'.
           05  FILLER                PIC X        VALUE SPACE.
           05  GRAND-TRANS-CT        PIC 999.
           05  FILLER                PIC X(24)
               VALUE ' TRANSACTIONS PROCESSED'.
           05  FILLER                PIC X(53)    VALUE SPACES.

       01  GRAND-LINE-2.
           05  FILLER                PIC X(22)
               VALUE '       TOTAL DEPOSITS:'.
           05  FILLER                PIC X        VALUE SPACE.
           05  GRAND-DEP-AMT         PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                PIC X(20)
               VALUE '  TOTAL WITHDRAWALS:'.
           05  FILLER                PIC X        VALUE SPACE.
           05  GRAND-WDR-AMT         PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                PIC X(29)    VALUE SPACES.

       01  GRAND-LINE-3.
           05  FILLER                PIC X(22)
               VALUE '       TOTAL INTEREST:'.
           05  FILLER                PIC X        VALUE SPACE.
           05  GRAND-INT-AMT         PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                PIC X(14)
               VALUE '  NET CHANGE: '.
           05  GRAND-NET-AMT         PIC ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                PIC X(30)    VALUE SPACES.

       01  GRAND-LINE-4.
           05  FILLER                PIC X(15)
               VALUE '       APPLIED:'.
           05  FILLER                PIC X        VALUE SPACE.
           05  GRAND-APPLIED-CT      PIC 999.
           05  FILLER                PIC X(11)
               VALUE '  REJECTED:'.
           05  FILLER                PIC X        VALUE SPACE.
           05  GRAND-REJECTED-CT     PIC 999.
           05  FILLER                PIC X(66)    VALUE SPACES.

       01  BLANK-LINE                PIC X(100)   VALUE SPACES.

       PROCEDURE DIVISION.

       0000-MAIN.
           PERFORM 1000-INITIALIZE
           IF WS-AUDIT-STATUS NOT = '00'
               DISPLAY 'TXNRPT: OPEN ERROR ON AUDIT FILE, '
                       'STATUS=' WS-AUDIT-STATUS
               MOVE 8 TO RETURN-CODE
               STOP RUN
           END-IF
           PERFORM 2000-READ-AUDIT
           PERFORM 3000-PROCESS-RECORDS
               UNTIL END-OF-FILE
           PERFORM 7000-FINAL-TOTALS
           PERFORM 9000-TERMINATE
           DISPLAY 'TXNRPT: REPORT GENERATED - '
                   WS-GRAND-TRANS-COUNT ' TRANSACTIONS, '
                   WS-PAGE-COUNT ' PAGES'
           MOVE ZERO TO RETURN-CODE
           STOP RUN
           .

       *>----------------------------------------------------------------*
       *> Open files, get run date, prepare page header
       *>----------------------------------------------------------------*
       1000-INITIALIZE.
           OPEN INPUT  AUDIT-FILE
           IF WS-AUDIT-STATUS NOT = '00'
               GO TO 1000-EXIT
           END-IF
           OPEN OUTPUT REPORT-FILE
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE WS-CURR-MM   TO HDR-RUN-MM
           MOVE WS-CURR-DD   TO HDR-RUN-DD
           MOVE WS-CURR-YYYY TO HDR-RUN-YYYY
           .
       1000-EXIT.
           EXIT.

       *>----------------------------------------------------------------*
       *> Read next audit record
       *>----------------------------------------------------------------*
       2000-READ-AUDIT.
           READ AUDIT-FILE
               AT END
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   CONTINUE
           END-READ
           .

       *>----------------------------------------------------------------*
       *> Main processing loop — detect control breaks, print detail
       *>----------------------------------------------------------------*
       3000-PROCESS-RECORDS.
           IF IS-FIRST-RECORD
               MOVE AUD-ACCT-ID TO WS-PREV-ACCT
               MOVE AUD-BRANCH  TO WS-PREV-BRANCH
               SET NOT-FIRST-RECORD TO TRUE
           ELSE
      *>        Branch break implies account break
               IF AUD-BRANCH NOT = WS-PREV-BRANCH
                   PERFORM 5000-ACCOUNT-BREAK
                   PERFORM 6000-BRANCH-BREAK
               ELSE IF AUD-ACCT-ID NOT = WS-PREV-ACCT
                   PERFORM 5000-ACCOUNT-BREAK
               END-IF
           END-IF

      *>    Check for page overflow before printing detail
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
               PERFORM 4000-PAGE-HEADER
           END-IF

           PERFORM 3500-PRINT-DETAIL
           PERFORM 3700-ACCUMULATE
           PERFORM 2000-READ-AUDIT
           .

       *>----------------------------------------------------------------*
       *> Format and print one detail line
       *>----------------------------------------------------------------*
       3500-PRINT-DETAIL.
           INITIALIZE DTL-LINE
           MOVE AUD-ACCT-ID     TO DTL-ACCT-ID
           MOVE AUD-TRANS-TYPE   TO DTL-TRANS-TYPE
           MOVE AUD-AMOUNT       TO DTL-AMOUNT
           MOVE AUD-OLD-BALANCE  TO DTL-OLD-BAL
           MOVE AUD-NEW-BALANCE  TO DTL-NEW-BAL
      *>    Format date: YYYYMMDD -> MM/DD/YYYY
           MOVE AUD-DATE(5:2)    TO DTL-DATE-MM
           MOVE AUD-DATE(7:2)    TO DTL-DATE-DD
           MOVE AUD-DATE(1:4)    TO DTL-DATE-YYYY
           IF AUD-APPLIED
               MOVE 'APPLIED ' TO DTL-STATUS
           ELSE IF AUD-REJECTED
               MOVE 'REJECTED' TO DTL-STATUS
           ELSE
               MOVE 'UNKNOWN ' TO DTL-STATUS
           END-IF
           WRITE REPORT-LINE FROM DTL-LINE
               AFTER ADVANCING 1 LINE
           ADD 1 TO WS-LINE-COUNT
           .

       *>----------------------------------------------------------------*
       *> Accumulate current record into all three levels
       *>----------------------------------------------------------------*
       3700-ACCUMULATE.
           ADD 1 TO WS-GRAND-TRANS-COUNT
           IF AUD-APPLIED
               ADD 1 TO WS-GRAND-APPLIED-COUNT
           END-IF
           IF AUD-REJECTED
               ADD 1 TO WS-GRAND-REJECTED-COUNT
           END-IF
           EVALUATE AUD-TRANS-TYPE
               WHEN 'D'
                   ADD 1          TO WS-ACCT-DEP-COUNT
                   ADD AUD-AMOUNT TO WS-ACCT-DEP-AMT
                   ADD 1          TO WS-BRNCH-DEP-COUNT
                   ADD AUD-AMOUNT TO WS-BRNCH-DEP-AMT
                   ADD 1          TO WS-GRAND-DEP-COUNT
                   ADD AUD-AMOUNT TO WS-GRAND-DEP-AMT
               WHEN 'W'
                   ADD 1          TO WS-ACCT-WDR-COUNT
                   ADD AUD-AMOUNT TO WS-ACCT-WDR-AMT
                   ADD 1          TO WS-BRNCH-WDR-COUNT
                   ADD AUD-AMOUNT TO WS-BRNCH-WDR-AMT
                   ADD 1          TO WS-GRAND-WDR-COUNT
                   ADD AUD-AMOUNT TO WS-GRAND-WDR-AMT
               WHEN 'I'
                   ADD 1          TO WS-ACCT-INT-COUNT
                   ADD AUD-AMOUNT TO WS-ACCT-INT-AMT
                   ADD 1          TO WS-BRNCH-INT-COUNT
                   ADD AUD-AMOUNT TO WS-BRNCH-INT-AMT
                   ADD 1          TO WS-GRAND-INT-COUNT
                   ADD AUD-AMOUNT TO WS-GRAND-INT-AMT
           END-EVALUATE
           .

       *>----------------------------------------------------------------*
       *> Print page header — eject to new page
       *>----------------------------------------------------------------*
       4000-PAGE-HEADER.
           ADD 1 TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT TO HDR-PAGE-NUM
           WRITE REPORT-LINE FROM HDR-LINE-1
               AFTER ADVANCING PAGE
           WRITE REPORT-LINE FROM HDR-LINE-2
               AFTER ADVANCING 1 LINE
           WRITE REPORT-LINE FROM BLANK-LINE
               AFTER ADVANCING 1 LINE
           WRITE REPORT-LINE FROM HDR-LINE-3
               AFTER ADVANCING 1 LINE
           WRITE REPORT-LINE FROM HDR-LINE-4
               AFTER ADVANCING 1 LINE
           MOVE 5 TO WS-LINE-COUNT
           .

       *>----------------------------------------------------------------*
       *> Account control break — print subtotal, reset accumulators
       *>----------------------------------------------------------------*
       5000-ACCOUNT-BREAK.
           COMPUTE WS-ACCT-NET-CHANGE =
               WS-ACCT-DEP-AMT - WS-ACCT-WDR-AMT
               + WS-ACCT-INT-AMT

      *>    Ensure room for subtotal (2 lines + blank)
           IF WS-LINE-COUNT + 3 > WS-LINES-PER-PAGE
               PERFORM 4000-PAGE-HEADER
           END-IF

           WRITE REPORT-LINE FROM BLANK-LINE
               AFTER ADVANCING 1 LINE
           MOVE WS-PREV-ACCT       TO ACCT-SUB-ID
           MOVE WS-ACCT-DEP-COUNT  TO ACCT-SUB-DEP-CT
           MOVE WS-ACCT-WDR-COUNT  TO ACCT-SUB-WDR-CT
           MOVE WS-ACCT-INT-COUNT  TO ACCT-SUB-INT-CT
           WRITE REPORT-LINE FROM ACCT-SUB-LINE-1
               AFTER ADVANCING 1 LINE
           MOVE WS-ACCT-NET-CHANGE TO ACCT-SUB-NET
           WRITE REPORT-LINE FROM ACCT-SUB-LINE-2
               AFTER ADVANCING 1 LINE
           ADD 3 TO WS-LINE-COUNT

      *>    Reset account accumulators
           MOVE ZERO TO WS-ACCT-DEP-COUNT
           MOVE ZERO TO WS-ACCT-WDR-COUNT
           MOVE ZERO TO WS-ACCT-INT-COUNT
           MOVE ZERO TO WS-ACCT-DEP-AMT
           MOVE ZERO TO WS-ACCT-WDR-AMT
           MOVE ZERO TO WS-ACCT-INT-AMT
           MOVE ZERO TO WS-ACCT-NET-CHANGE

      *>    Update previous account key
           MOVE AUD-ACCT-ID TO WS-PREV-ACCT
           .

       *>----------------------------------------------------------------*
       *> Branch control break — print subtotal, reset accumulators
       *>----------------------------------------------------------------*
       6000-BRANCH-BREAK.
           COMPUTE WS-BRNCH-NET-CHANGE =
               WS-BRNCH-DEP-AMT - WS-BRNCH-WDR-AMT
               + WS-BRNCH-INT-AMT

      *>    Ensure room for subtotal (4 lines + blank)
           IF WS-LINE-COUNT + 5 > WS-LINES-PER-PAGE
               PERFORM 4000-PAGE-HEADER
           END-IF

           WRITE REPORT-LINE FROM BLANK-LINE
               AFTER ADVANCING 1 LINE
           MOVE WS-PREV-BRANCH      TO BRNCH-SUB-ID
           MOVE WS-BRNCH-DEP-COUNT  TO BRNCH-SUB-DEP-CT
           MOVE WS-BRNCH-WDR-COUNT  TO BRNCH-SUB-WDR-CT
           MOVE WS-BRNCH-INT-COUNT  TO BRNCH-SUB-INT-CT
           WRITE REPORT-LINE FROM BRNCH-SUB-LINE-1
               AFTER ADVANCING 1 LINE
           MOVE WS-BRNCH-DEP-AMT    TO BRNCH-SUB-DEP-AMT
           MOVE WS-BRNCH-WDR-AMT    TO BRNCH-SUB-WDR-AMT
           WRITE REPORT-LINE FROM BRNCH-SUB-LINE-2
               AFTER ADVANCING 1 LINE
           MOVE WS-BRNCH-NET-CHANGE TO BRNCH-SUB-NET
           WRITE REPORT-LINE FROM BRNCH-SUB-LINE-3
               AFTER ADVANCING 1 LINE
           ADD 5 TO WS-LINE-COUNT

      *>    Reset branch accumulators
           MOVE ZERO TO WS-BRNCH-DEP-COUNT
           MOVE ZERO TO WS-BRNCH-WDR-COUNT
           MOVE ZERO TO WS-BRNCH-INT-COUNT
           MOVE ZERO TO WS-BRNCH-DEP-AMT
           MOVE ZERO TO WS-BRNCH-WDR-AMT
           MOVE ZERO TO WS-BRNCH-INT-AMT
           MOVE ZERO TO WS-BRNCH-NET-CHANGE

      *>    Update previous branch key
           MOVE AUD-BRANCH TO WS-PREV-BRANCH
           .

       *>----------------------------------------------------------------*
       *> End-of-file — flush final breaks and print grand total
       *>----------------------------------------------------------------*
       7000-FINAL-TOTALS.
      *>    Only print if we processed at least one record
           IF NOT-FIRST-RECORD
               PERFORM 5000-ACCOUNT-BREAK
               PERFORM 6000-BRANCH-BREAK
           END-IF

           COMPUTE WS-GRAND-NET-CHANGE =
               WS-GRAND-DEP-AMT - WS-GRAND-WDR-AMT
               + WS-GRAND-INT-AMT

      *>    Ensure room for grand total (5 lines + blanks)
           IF WS-LINE-COUNT + 7 > WS-LINES-PER-PAGE
               PERFORM 4000-PAGE-HEADER
           END-IF

           WRITE REPORT-LINE FROM BLANK-LINE
               AFTER ADVANCING 1 LINE
           WRITE REPORT-LINE FROM BLANK-LINE
               AFTER ADVANCING 1 LINE
           MOVE WS-GRAND-TRANS-COUNT TO GRAND-TRANS-CT
           WRITE REPORT-LINE FROM GRAND-LINE-1
               AFTER ADVANCING 1 LINE
           MOVE WS-GRAND-DEP-AMT   TO GRAND-DEP-AMT
           MOVE WS-GRAND-WDR-AMT   TO GRAND-WDR-AMT
           WRITE REPORT-LINE FROM GRAND-LINE-2
               AFTER ADVANCING 1 LINE
           MOVE WS-GRAND-INT-AMT   TO GRAND-INT-AMT
           MOVE WS-GRAND-NET-CHANGE TO GRAND-NET-AMT
           WRITE REPORT-LINE FROM GRAND-LINE-3
               AFTER ADVANCING 1 LINE
           MOVE WS-GRAND-APPLIED-COUNT  TO GRAND-APPLIED-CT
           MOVE WS-GRAND-REJECTED-COUNT TO GRAND-REJECTED-CT
           WRITE REPORT-LINE FROM GRAND-LINE-4
               AFTER ADVANCING 1 LINE
           .

       *>----------------------------------------------------------------*
       *> Close files
       *>----------------------------------------------------------------*
       9000-TERMINATE.
           CLOSE AUDIT-FILE
           CLOSE REPORT-FILE
           .
