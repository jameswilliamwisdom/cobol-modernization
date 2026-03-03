      *>================================================================*
      *> CLMRPT.cob — Explanation of Benefits (EOB) report generator
      *> Compile: cobc -x -free -I copybooks CLMRPT.cob
      *> Reads ADJUDICATED.DAT, produces human-readable EOB report.
      *> Groups records by claim (H + A* + P pattern).
      *>================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMRPT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ADJ-FILE
               ASSIGN TO 'data/ADJUDICATED.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ADJ-STATUS.
           SELECT REPORT-FILE
               ASSIGN TO 'data/EOB-REPORT.TXT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ADJ-FILE.
       COPY CLAIMREC.

       FD  REPORT-FILE.
       01  REPORT-LINE                PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-ADJ-STATUS              PIC XX.
       01  WS-RPT-STATUS              PIC XX.
       01  WS-EOF                     PIC X(1) VALUE 'N'.
           88  WS-AT-EOF              VALUE 'Y'.
       01  WS-PAGE-NUM                PIC 9(3) VALUE 0.
       01  WS-LINE-COUNT              PIC 9(2) VALUE 0.
       01  WS-RECORDS-READ            PIC 9(5) VALUE 0.
       01  WS-CLAIMS-REPORTED         PIC 9(5) VALUE 0.

      *> Current claim state
       01  WS-CUR-CLAIM-ID            PIC X(12).
       01  WS-CUR-MEMBER              PIC X(10).
       01  WS-CUR-PROVIDER            PIC X(10).
       01  WS-CUR-DATE                PIC 9(8).
       01  WS-CUR-POL-TYPE            PIC X(1).
       01  WS-CUR-POL-DESC            PIC X(9).

      *> Totals per claim
       01  WS-TOT-CHARGED             PIC S9(9)V99 VALUE 0.
       01  WS-TOT-ALLOWED             PIC S9(9)V99 VALUE 0.
       01  WS-TOT-COPAY               PIC S9(7)V99 VALUE 0.
       01  WS-TOT-DEDUCT              PIC S9(7)V99 VALUE 0.
       01  WS-TOT-PLAN-PAYS           PIC S9(9)V99 VALUE 0.
       01  WS-TOT-YOU-OWE             PIC S9(9)V99 VALUE 0.
       01  WS-PAYMENT-AMT             PIC S9(9)V99 VALUE 0.
       01  WS-PAY-DATE                PIC 9(8).
       01  WS-PAY-METHOD-DESC         PIC X(5).

      *> Line adjudication working fields (extracted from COMP-3)
       01  WS-ADJ-LINE-NUM            PIC 9(2).
       01  WS-ADJ-ALLOWED             PIC S9(7)V99 VALUE 0.
       01  WS-ADJ-COPAY               PIC S9(5)V99 VALUE 0.
       01  WS-ADJ-DEDUCT              PIC S9(5)V99 VALUE 0.
       01  WS-ADJ-COINS               PIC S9(5)V99 VALUE 0.
       01  WS-ADJ-PAID                PIC S9(7)V99 VALUE 0.
       01  WS-LINE-ADJ-STATUS         PIC X(2).
       01  WS-LINE-ADJ-REASON         PIC X(3).
       01  WS-YOU-OWE                 PIC S9(7)V99 VALUE 0.

      *> For charge lookup from service records
       01  WS-LINE-CHARGE             PIC S9(7)V99 VALUE 0.

      *> Service line table — populated from S records in output
      *> so we can display charge and procedure code on A records
       01  WS-SVC-LINES-COUNT         PIC 9(2) VALUE 0.
       01  WS-SVC-LINES-TABLE.
           05  WS-SL-ENTRY OCCURS 20.
               10  WS-SL-LINE-NUM     PIC 9(2).
               10  WS-SL-PROC-CODE    PIC X(5).
               10  WS-SL-CHARGE       PIC S9(7)V99.
       01  WS-SL-IDX                  PIC 9(2).

      *> (No pending record needed — sequential read with natural look-ahead)

      *> Formatted date
       01  WS-FMT-DATE.
           05  WS-FMT-MM              PIC 99.
           05  FILLER                  PIC X(1) VALUE '/'.
           05  WS-FMT-DD              PIC 99.
           05  FILLER                  PIC X(1) VALUE '/'.
           05  WS-FMT-YYYY            PIC 9999.

      *> Report lines
       01  WS-HDR-LINE.
           05  FILLER PIC X(35) VALUE
               'EXPLANATION OF BENEFITS            '.
           05  FILLER PIC X(53) VALUE SPACES.
           05  FILLER PIC X(6) VALUE 'PAGE: '.
           05  WS-HDR-PAGE             PIC ZZ9.

       01  WS-SEP-LINE                PIC X(97) VALUE ALL '='.

       01  WS-CLAIM-LINE.
           05  FILLER PIC X(7) VALUE 'CLAIM: '.
           05  WS-CL-CLAIM-ID         PIC X(12).
           05  FILLER PIC X(3) VALUE '   '.
           05  FILLER PIC X(6) VALUE 'DATE: '.
           05  WS-CL-DATE             PIC X(10).
           05  FILLER PIC X(3) VALUE '   '.
           05  FILLER PIC X(8) VALUE 'MEMBER: '.
           05  WS-CL-MEMBER           PIC X(10).

       01  WS-PROV-LINE.
           05  FILLER PIC X(10) VALUE 'PROVIDER: '.
           05  WS-PL-PROVIDER          PIC X(10).
           05  FILLER PIC X(2) VALUE '  '.
           05  FILLER PIC X(8) VALUE 'POLICY: '.
           05  WS-PL-POL-TYPE          PIC X(9).

       01  WS-COL-HDR.
           05  FILLER PIC X(6) VALUE 'LINE  '.
           05  FILLER PIC X(11) VALUE 'PROCEDURE  '.
           05  FILLER PIC X(13) VALUE 'CHARGED      '.
           05  FILLER PIC X(13) VALUE 'ALLOWED      '.
           05  FILLER PIC X(10) VALUE 'COPAY     '.
           05  FILLER PIC X(12) VALUE 'DEDUCTIBLE  '.
           05  FILLER PIC X(12) VALUE 'PLAN PAYS   '.
           05  FILLER PIC X(12) VALUE 'YOU OWE     '.
           05  FILLER PIC X(8) VALUE 'STATUS  '.

       01  WS-COL-SEP                 PIC X(97) VALUE ALL '-'.

       01  WS-DETAIL-LINE.
           05  WS-DT-LINE-NUM         PIC Z9.
           05  FILLER                  PIC X(4) VALUE '    '.
           05  WS-DT-PROC-CODE        PIC X(5).
           05  FILLER                  PIC X(2) VALUE '  '.
           05  WS-DT-CHARGED          PIC $$$,$$$,$$9.99.
           05  FILLER                  PIC X(1) VALUE ' '.
           05  WS-DT-ALLOWED          PIC $$$,$$$,$$9.99.
           05  FILLER                  PIC X(1) VALUE ' '.
           05  WS-DT-COPAY            PIC $$$$,$$9.99.
           05  FILLER                  PIC X(1) VALUE ' '.
           05  WS-DT-DEDUCT           PIC $$$$,$$9.99.
           05  FILLER                  PIC X(2) VALUE '  '.
           05  WS-DT-PLAN-PAYS        PIC $$$,$$$,$$9.99.
           05  FILLER                  PIC X(1) VALUE ' '.
           05  WS-DT-YOU-OWE          PIC $$$,$$$,$$9.99.
           05  FILLER                  PIC X(1) VALUE ' '.
           05  WS-DT-STATUS           PIC X(2).

       01  WS-TOTAL-LINE.
           05  FILLER PIC X(11) VALUE 'TOTALS:    '.
           05  WS-TL-CHARGED          PIC $$$,$$$,$$9.99.
           05  FILLER                  PIC X(1) VALUE ' '.
           05  WS-TL-ALLOWED          PIC $$$,$$$,$$9.99.
           05  FILLER                  PIC X(1) VALUE ' '.
           05  WS-TL-COPAY            PIC $$$$,$$9.99.
           05  FILLER                  PIC X(1) VALUE ' '.
           05  WS-TL-DEDUCT           PIC $$$$,$$9.99.
           05  FILLER                  PIC X(2) VALUE '  '.
           05  WS-TL-PLAN-PAYS        PIC $$$,$$$,$$9.99.
           05  FILLER                  PIC X(1) VALUE ' '.
           05  WS-TL-YOU-OWE          PIC $$$,$$$,$$9.99.

       01  WS-PAYMENT-LINE.
           05  FILLER PIC X(9) VALUE 'PAYMENT: '.
           05  WS-PYMT-METHOD          PIC X(5).
           05  FILLER PIC X(2) VALUE '  '.
           05  WS-PYMT-AMT            PIC $$$,$$$,$$9.99.
           05  FILLER PIC X(4) VALUE ' ON '.
           05  WS-PYMT-DATE           PIC X(10).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM OPEN-FILES
           PERFORM PROCESS-RECORDS
           PERFORM CLOSE-FILES
           DISPLAY 'EOB REPORT COMPLETE'
           DISPLAY '  CLAIMS REPORTED: ' WS-CLAIMS-REPORTED
           DISPLAY '  RECORDS READ:    ' WS-RECORDS-READ
           STOP RUN.

       OPEN-FILES.
           OPEN INPUT ADJ-FILE
           IF WS-ADJ-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING ADJUDICATED: ' WS-ADJ-STATUS
               MOVE 8 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN OUTPUT REPORT-FILE
           IF WS-RPT-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING REPORT: ' WS-RPT-STATUS
               MOVE 8 TO RETURN-CODE
               STOP RUN
           END-IF.

       PROCESS-RECORDS.
           PERFORM READ-NEXT-RECORD
           PERFORM UNTIL WS-AT-EOF
               IF CLAIM-FILE-RECORD(1:1) = 'H'
                   PERFORM PROCESS-ONE-CLAIM
               ELSE
                   DISPLAY 'UNEXPECTED RECORD TYPE: '
                       CLAIM-FILE-RECORD(1:1)
                   PERFORM READ-NEXT-RECORD
               END-IF
           END-PERFORM.

       READ-NEXT-RECORD.
           READ ADJ-FILE
               AT END
                   SET WS-AT-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-RECORDS-READ
           END-READ.

       PROCESS-ONE-CLAIM.
           ADD 1 TO WS-CLAIMS-REPORTED
           ADD 1 TO WS-PAGE-NUM

      *> Extract header info via REDEFINES (CFH-* overlay)
           MOVE CFR-CLAIM-ID TO WS-CUR-CLAIM-ID
           MOVE CFH-MEMBER-ID TO WS-CUR-MEMBER
           MOVE CFH-PROVIDER-ID TO WS-CUR-PROVIDER
           MOVE CFH-CLAIM-DATE TO WS-CUR-DATE
           MOVE CFH-POLICY-TYPE TO WS-CUR-POL-TYPE

           EVALUATE WS-CUR-POL-TYPE
               WHEN 'P' MOVE 'PPO      ' TO WS-CUR-POL-DESC
               WHEN 'H' MOVE 'HMO      ' TO WS-CUR-POL-DESC
               WHEN 'I' MOVE 'INDEMNITY' TO WS-CUR-POL-DESC
               WHEN OTHER MOVE 'UNKNOWN  ' TO WS-CUR-POL-DESC
           END-EVALUATE

      *> Initialize claim totals and service line table
           MOVE 0 TO WS-SVC-LINES-COUNT
           MOVE 0 TO WS-TOT-CHARGED
           MOVE 0 TO WS-TOT-ALLOWED
           MOVE 0 TO WS-TOT-COPAY
           MOVE 0 TO WS-TOT-DEDUCT
           MOVE 0 TO WS-TOT-PLAN-PAYS
           MOVE 0 TO WS-TOT-YOU-OWE
           MOVE 0 TO WS-PAYMENT-AMT

      *> Write claim header
           PERFORM WRITE-CLAIM-HEADER

      *> Read adjudication + payment records
           PERFORM READ-NEXT-RECORD
           PERFORM UNTIL WS-AT-EOF
               OR CLAIM-FILE-RECORD(1:1) = 'H'
               EVALUATE CLAIM-FILE-RECORD(1:1)
                   WHEN 'S'
                       PERFORM SAVE-SERVICE-LINE
                   WHEN 'A'
                       PERFORM PROCESS-ADJ-RECORD
                   WHEN 'P'
                       PERFORM PROCESS-PAY-RECORD
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
               PERFORM READ-NEXT-RECORD
           END-PERFORM

      *> Write totals and payment
           PERFORM WRITE-CLAIM-TOTALS.
      *> After exit: CLAIM-FILE-RECORD has next H or EOF

       WRITE-CLAIM-HEADER.
           MOVE WS-PAGE-NUM TO WS-HDR-PAGE
           MOVE WS-HDR-LINE TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-SEP-LINE TO REPORT-LINE
           WRITE REPORT-LINE

      *> Claim info line
           MOVE WS-CUR-CLAIM-ID TO WS-CL-CLAIM-ID
           PERFORM FORMAT-DATE-8
           MOVE WS-FMT-DATE TO WS-CL-DATE
           MOVE WS-CUR-MEMBER TO WS-CL-MEMBER
           MOVE WS-CLAIM-LINE TO REPORT-LINE
           WRITE REPORT-LINE

      *> Provider line
           MOVE WS-CUR-PROVIDER TO WS-PL-PROVIDER
           MOVE WS-CUR-POL-DESC TO WS-PL-POL-TYPE
           MOVE WS-PROV-LINE TO REPORT-LINE
           WRITE REPORT-LINE

      *> Blank line
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE

      *> Column headers
           MOVE WS-COL-HDR TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-COL-SEP TO REPORT-LINE
           WRITE REPORT-LINE.

       SAVE-SERVICE-LINE.
      *> Extract service line data via REDEFINES (CFS-* overlay)
      *> Save proc code and charge so PROCESS-ADJ-RECORD can use them
           ADD 1 TO WS-SVC-LINES-COUNT
           MOVE CFS-LINE-NUMBER
               TO WS-SL-LINE-NUM(WS-SVC-LINES-COUNT)
           MOVE CFS-PROCEDURE-CODE
               TO WS-SL-PROC-CODE(WS-SVC-LINES-COUNT)
           MOVE CFS-CHARGE-AMT
               TO WS-SL-CHARGE(WS-SVC-LINES-COUNT).

       PROCESS-ADJ-RECORD.
      *> Extract adjudication fields via REDEFINES (CFA-* overlay)
           MOVE CFA-LINE-NUMBER TO WS-ADJ-LINE-NUM
           MOVE CFA-ALLOWED-AMT TO WS-ADJ-ALLOWED
           MOVE CFA-COPAY-AMT TO WS-ADJ-COPAY
           MOVE CFA-DEDUCT-AMT TO WS-ADJ-DEDUCT
           MOVE CFA-COINS-AMT TO WS-ADJ-COINS
           MOVE CFA-PAID-AMT TO WS-ADJ-PAID
           MOVE CFA-STATUS TO WS-LINE-ADJ-STATUS
           MOVE CFA-REASON-CODE TO WS-LINE-ADJ-REASON

      *> Compute "you owe" = copay + deductible + coinsurance
           COMPUTE WS-YOU-OWE =
               WS-ADJ-COPAY + WS-ADJ-DEDUCT + WS-ADJ-COINS

      *> Look up charge and proc code from saved S records
           MOVE SPACES TO WS-DT-PROC-CODE
           MOVE 0 TO WS-LINE-CHARGE
           PERFORM VARYING WS-SL-IDX FROM 1 BY 1
               UNTIL WS-SL-IDX > WS-SVC-LINES-COUNT
               IF WS-SL-LINE-NUM(WS-SL-IDX) = WS-ADJ-LINE-NUM
                   MOVE WS-SL-PROC-CODE(WS-SL-IDX)
                       TO WS-DT-PROC-CODE
                   MOVE WS-SL-CHARGE(WS-SL-IDX)
                       TO WS-LINE-CHARGE
               END-IF
           END-PERFORM

      *> Format and write detail line
           MOVE WS-ADJ-LINE-NUM TO WS-DT-LINE-NUM
           MOVE WS-LINE-CHARGE TO WS-DT-CHARGED
           MOVE WS-ADJ-ALLOWED TO WS-DT-ALLOWED
           MOVE WS-ADJ-COPAY TO WS-DT-COPAY
           MOVE WS-ADJ-DEDUCT TO WS-DT-DEDUCT
           MOVE WS-ADJ-PAID TO WS-DT-PLAN-PAYS
           MOVE WS-YOU-OWE TO WS-DT-YOU-OWE
           MOVE WS-LINE-ADJ-STATUS TO WS-DT-STATUS

           MOVE WS-DETAIL-LINE TO REPORT-LINE
           WRITE REPORT-LINE

      *> Accumulate totals
           ADD WS-LINE-CHARGE TO WS-TOT-CHARGED
           ADD WS-ADJ-ALLOWED TO WS-TOT-ALLOWED
           ADD WS-ADJ-COPAY TO WS-TOT-COPAY
           ADD WS-ADJ-DEDUCT TO WS-TOT-DEDUCT
           ADD WS-ADJ-PAID TO WS-TOT-PLAN-PAYS
           ADD WS-YOU-OWE TO WS-TOT-YOU-OWE.

       PROCESS-PAY-RECORD.
      *> Extract payment fields via REDEFINES (CFP-* overlay)
           MOVE CFP-TOTAL-PAID TO WS-PAYMENT-AMT
           MOVE CFP-PAY-DATE TO WS-PAY-DATE

           IF CFP-EFT
               MOVE 'EFT  ' TO WS-PAY-METHOD-DESC
           ELSE
               MOVE 'CHECK' TO WS-PAY-METHOD-DESC
           END-IF.

       WRITE-CLAIM-TOTALS.
      *> Blank line before totals
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE

      *> Separator
           MOVE WS-COL-SEP TO REPORT-LINE
           WRITE REPORT-LINE

      *> Total line
           MOVE WS-TOT-CHARGED TO WS-TL-CHARGED
           MOVE WS-TOT-ALLOWED TO WS-TL-ALLOWED
           MOVE WS-TOT-COPAY TO WS-TL-COPAY
           MOVE WS-TOT-DEDUCT TO WS-TL-DEDUCT
           MOVE WS-TOT-PLAN-PAYS TO WS-TL-PLAN-PAYS
           MOVE WS-TOT-YOU-OWE TO WS-TL-YOU-OWE
           MOVE WS-TOTAL-LINE TO REPORT-LINE
           WRITE REPORT-LINE

      *> Payment line
           MOVE WS-PAY-METHOD-DESC TO WS-PYMT-METHOD
           MOVE WS-PAYMENT-AMT TO WS-PYMT-AMT
           MOVE WS-PAY-DATE TO WS-CUR-DATE
           PERFORM FORMAT-DATE-8
           MOVE WS-FMT-DATE TO WS-PYMT-DATE
           MOVE WS-PAYMENT-LINE TO REPORT-LINE
           WRITE REPORT-LINE

      *> Double separator between claims
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-SEP-LINE TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE.

       FORMAT-DATE-8.
      *> Convert YYYYMMDD to MM/DD/YYYY
           MOVE WS-CUR-DATE(5:2) TO WS-FMT-MM
           MOVE WS-CUR-DATE(7:2) TO WS-FMT-DD
           MOVE WS-CUR-DATE(1:4) TO WS-FMT-YYYY.

       CLOSE-FILES.
           CLOSE ADJ-FILE
           CLOSE REPORT-FILE.
