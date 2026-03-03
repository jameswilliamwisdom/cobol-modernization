      *>================================================================*
      *> CLMPROC.cob — Main claims adjudication engine
      *> Compile: cobc -x -free -I copybooks CLMPROC.cob CLMVALID.cob
      *> Reads claims, validates via CALL to CLMVALID, adjudicates
      *> using EVALUATE with 15+ WHEN clauses, writes results.
      *> Showcases: REDEFINES dispatch, COMP-3 ROUNDED, CALL/LINKAGE,
      *>            OCCURS DEPENDING ON, complex EVALUATE.
      *>================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMPROC.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLAIMS-FILE
               ASSIGN TO 'data/CLAIMS.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CLM-STATUS.
           SELECT POLICY-FILE
               ASSIGN TO 'data/POLICIES.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-POL-STATUS.
           SELECT PROVIDER-FILE
               ASSIGN TO 'data/PROVIDERS.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-PROV-STATUS.
           SELECT ADJ-FILE
               ASSIGN TO 'data/ADJUDICATED.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ADJ-STATUS.
           SELECT DENIED-FILE
               ASSIGN TO 'data/DENIED.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-DEN-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CLAIMS-FILE.
       COPY CLAIMREC.

       FD  POLICY-FILE.
       COPY POLREC.

       FD  PROVIDER-FILE.
       COPY FEESCHED.

       FD  ADJ-FILE.
       01  ADJ-OUT-RECORD                PIC X(200).

       FD  DENIED-FILE.
       01  DENIED-RECORD              PIC X(120).

       WORKING-STORAGE SECTION.
      *> File status codes
       01  WS-CLM-STATUS              PIC XX.
       01  WS-POL-STATUS              PIC XX.
       01  WS-PROV-STATUS             PIC XX.
       01  WS-ADJ-STATUS              PIC XX.
       01  WS-DEN-STATUS              PIC XX.
       01  WS-EOF-CLM                 PIC X(1) VALUE 'N'.
           88  WS-CLM-EOF             VALUE 'Y'.
       01  WS-EOF-POL                 PIC X(1) VALUE 'N'.
       01  WS-EOF-PROV               PIC X(1) VALUE 'N'.

      *> Counters
       01  WS-CLAIMS-READ             PIC 9(5) VALUE 0.
       01  WS-CLAIMS-PROCESSED        PIC 9(5) VALUE 0.
       01  WS-CLAIMS-APPROVED         PIC 9(5) VALUE 0.
       01  WS-CLAIMS-DENIED           PIC 9(5) VALUE 0.
       01  WS-TOTAL-CHARGED           PIC S9(9)V99 VALUE 0.
       01  WS-TOTAL-PAID              PIC S9(9)V99 VALUE 0.
       01  WS-RECORDS-WRITTEN         PIC 9(5) VALUE 0.

      *> Current claim working fields
       01  WS-CURRENT-CLAIM-ID        PIC X(12).
       01  WS-CURRENT-MEMBER          PIC X(10).
       01  WS-CURRENT-PROVIDER        PIC X(10).
       01  WS-CURRENT-POL-TYPE        PIC X(1).
       01  WS-CURRENT-CLAIM-DATE      PIC 9(8).
       01  WS-HAS-DENIAL              PIC X(1).
       01  WS-CLAIM-TOTAL-CHARGED     PIC S9(9)V99 VALUE 0.
       01  WS-CLAIM-TOTAL-PAID        PIC S9(9)V99 VALUE 0.

      *> Adjudication working fields
       01  WS-LINE-ALLOWED            PIC S9(7)V99 VALUE 0.
       01  WS-LINE-COPAY              PIC S9(5)V99 VALUE 0.
       01  WS-LINE-DEDUCT             PIC S9(5)V99 VALUE 0.
       01  WS-LINE-COINS              PIC S9(5)V99 VALUE 0.
       01  WS-LINE-PAID               PIC S9(7)V99 VALUE 0.
       01  WS-LINE-STATUS             PIC X(2).
       01  WS-LINE-REASON             PIC X(3).
       01  WS-FEE-RATE                PIC S9(5)V99 VALUE 0.
       01  WS-PREAUTH-FLAG            PIC X(1).
       01  WS-PROC-PREFIX             PIC X(1).

      *> Deductible tracking (per claim)
       01  WS-DEDUCT-REMAINING        PIC S9(5)V99 VALUE 0.
      *> OOP tracking (per claim)
       01  WS-OOP-REMAINING           PIC S9(5)V99 VALUE 0.
       01  WS-PATIENT-TOTAL           PIC S9(7)V99 VALUE 0.

      *> Policy table — loaded at startup
       01  WS-POL-TABLE-COUNT         PIC 9(2) VALUE 0.
       01  WS-POL-TABLE.
           05  WS-POL-ENTRY OCCURS 20.
               10  WS-PT-POLICY-ID    PIC X(5).
               10  WS-PT-MEMBER-ID    PIC X(10).
               10  WS-PT-TYPE         PIC X(1).
               10  WS-PT-EFF-DATE     PIC 9(8).
               10  WS-PT-TERM-DATE    PIC 9(8).
               10  WS-PT-COPAY        PIC S9(3)V99.
               10  WS-PT-COINS-RATE   PIC 9V99.
               10  WS-PT-DEDUCT-ANN   PIC S9(5)V99.
               10  WS-PT-DEDUCT-MET   PIC S9(5)V99.
               10  WS-PT-OOP-MAX      PIC S9(5)V99.
               10  WS-PT-OOP-YTD      PIC S9(5)V99.

      *> Fee schedule table — loaded at startup
       01  WS-FEE-TABLE-COUNT         PIC 9(3) VALUE 0.
       01  WS-FEE-TABLE.
           05  WS-FEE-ENTRY OCCURS 100.
               10  WS-FT-PROVIDER-ID  PIC X(10).
               10  WS-FT-PROC-CODE   PIC X(5).
               10  WS-FT-RATE         PIC S9(5)V99.
               10  WS-FT-EFF-DATE     PIC 9(8).
               10  WS-FT-TERM-DATE    PIC 9(8).
               10  WS-FT-PREAUTH      PIC X(1).

      *> Current policy (found via lookup)
       01  WS-CUR-POL-IDX             PIC 9(2).
       01  WS-CUR-POL-FOUND           PIC X(1).

      *> Diagnosis table for current claim
       COPY DIAGTBL.

      *> Service table for current claim
       COPY SVCTBL.

      *> Validation interface
       01  WS-VALID-REQUEST.
           05  WS-VR-MEMBER-ID        PIC X(10).
           05  WS-VR-PROVIDER-ID      PIC X(10).
           05  WS-VR-PROCEDURE-CODE   PIC X(5).
           05  WS-VR-DIAG-CODE        PIC X(7).
           05  WS-VR-DATE-OF-SVC      PIC 9(8).
           05  WS-VR-PLACE-OF-SVC     PIC X(2).
           05  WS-VR-CHARGE-AMT       PIC S9(7)V99 COMP-3.
       01  WS-VALID-RESPONSE.
           05  WS-VR-IS-VALID         PIC X(1).
               88  WS-VR-VALID        VALUE 'Y'.
               88  WS-VR-INVALID      VALUE 'N'.
           05  WS-VR-ERROR-CODE       PIC X(3).
           05  WS-VR-ERROR-MSG        PIC X(50).

      *> Working-storage copy of claim record for building output
      *> Avoids corrupting the file read buffer (CLAIM-FILE-RECORD)
       01  WS-OUT-REC.
           05  WS-OR-RECORD-TYPE      PIC X(1).
           05  WS-OR-CLAIM-ID         PIC X(12).
           05  WS-OR-BODY             PIC X(187).
           05  WS-OR-ADJ REDEFINES WS-OR-BODY.
               10  WS-OA-LINE-NUMBER  PIC 9(2).
               10  WS-OA-ALLOWED-AMT  PIC S9(7)V99 COMP-3.
               10  WS-OA-COPAY-AMT    PIC S9(5)V99 COMP-3.
               10  WS-OA-DEDUCT-AMT   PIC S9(5)V99 COMP-3.
               10  WS-OA-COINS-AMT    PIC S9(5)V99 COMP-3.
               10  WS-OA-PAID-AMT     PIC S9(7)V99 COMP-3.
               10  WS-OA-STATUS       PIC X(2).
               10  WS-OA-REASON-CODE  PIC X(3).
               10  WS-OA-ADJ-FILLER   PIC X(158).
           05  WS-OR-PAY REDEFINES WS-OR-BODY.
               10  WS-OP-PAY-METHOD   PIC X(1).
               10  WS-OP-TOTAL-PAID   PIC S9(9)V99 COMP-3.
               10  WS-OP-CHECK-NUMBER PIC 9(8).
               10  WS-OP-PAY-DATE     PIC 9(8).
               10  WS-OP-PAYEE-NAME   PIC X(30).
               10  WS-OP-PAY-FILLER   PIC X(134).

      *> Lookup indexes
       01  WS-SEARCH-IDX              PIC 9(3).
       01  WS-SVC-IDX                 PIC 9(2).
       01  WS-DIAG-IDX                PIC 9(2).

      *> Denied record formatting
       01  WS-DENIED-LINE.
           05  WS-DL-CLAIM-ID         PIC X(12).
           05  FILLER                  PIC X(1) VALUE ' '.
           05  WS-DL-LINE-NUM         PIC 9(2).
           05  FILLER                  PIC X(1) VALUE ' '.
           05  WS-DL-PROC-CODE        PIC X(5).
           05  FILLER                  PIC X(1) VALUE ' '.
           05  WS-DL-REASON           PIC X(3).
           05  FILLER                  PIC X(1) VALUE ' '.
           05  WS-DL-ERROR-MSG        PIC X(50).
           05  FILLER                  PIC X(1) VALUE ' '.
           05  WS-DL-CHARGE           PIC $$$,$$$,$$9.99.
           05  WS-DL-FILLER           PIC X(30).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM OPEN-FILES
           PERFORM LOAD-POLICIES
           PERFORM LOAD-FEE-SCHEDULE
           PERFORM PROCESS-CLAIMS
           PERFORM CLOSE-FILES
           PERFORM DISPLAY-SUMMARY
           STOP RUN.

      *>================================================================*
      *> OPEN-FILES: Open all input and output files
      *>================================================================*
       OPEN-FILES.
           OPEN INPUT CLAIMS-FILE
           IF WS-CLM-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING CLAIMS: ' WS-CLM-STATUS
               MOVE 8 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN INPUT POLICY-FILE
           IF WS-POL-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING POLICIES: ' WS-POL-STATUS
               MOVE 8 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN INPUT PROVIDER-FILE
           IF WS-PROV-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING PROVIDERS: ' WS-PROV-STATUS
               MOVE 8 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN OUTPUT ADJ-FILE
           IF WS-ADJ-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING ADJUDICATED: ' WS-ADJ-STATUS
               MOVE 8 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN OUTPUT DENIED-FILE
           IF WS-DEN-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING DENIED: ' WS-DEN-STATUS
               MOVE 8 TO RETURN-CODE
               STOP RUN
           END-IF.

      *>================================================================*
      *> LOAD-POLICIES: Read all policy records into WS table
      *>================================================================*
       LOAD-POLICIES.
           PERFORM UNTIL WS-EOF-POL = 'Y'
               READ POLICY-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-POL
                   NOT AT END
                       ADD 1 TO WS-POL-TABLE-COUNT
                       MOVE POL-POLICY-ID
                           TO WS-PT-POLICY-ID(WS-POL-TABLE-COUNT)
                       MOVE POL-MEMBER-ID
                           TO WS-PT-MEMBER-ID(WS-POL-TABLE-COUNT)
                       MOVE POL-TYPE
                           TO WS-PT-TYPE(WS-POL-TABLE-COUNT)
                       MOVE POL-EFFECTIVE-DATE
                           TO WS-PT-EFF-DATE(WS-POL-TABLE-COUNT)
                       MOVE POL-TERM-DATE
                           TO WS-PT-TERM-DATE(WS-POL-TABLE-COUNT)
                       MOVE POL-COPAY-AMT
                           TO WS-PT-COPAY(WS-POL-TABLE-COUNT)
                       MOVE POL-COINSURANCE-RATE
                           TO WS-PT-COINS-RATE(WS-POL-TABLE-COUNT)
                       MOVE POL-DEDUCTIBLE-ANNUAL
                           TO WS-PT-DEDUCT-ANN(WS-POL-TABLE-COUNT)
                       MOVE POL-DEDUCTIBLE-MET
                           TO WS-PT-DEDUCT-MET(WS-POL-TABLE-COUNT)
                       MOVE POL-OUT-OF-POCKET-MAX
                           TO WS-PT-OOP-MAX(WS-POL-TABLE-COUNT)
                       MOVE POL-OUT-OF-POCKET-YTD
                           TO WS-PT-OOP-YTD(WS-POL-TABLE-COUNT)
               END-READ
           END-PERFORM
           DISPLAY 'POLICIES LOADED: ' WS-POL-TABLE-COUNT.

      *>================================================================*
      *> LOAD-FEE-SCHEDULE: Read all fee records into WS table
      *>================================================================*
       LOAD-FEE-SCHEDULE.
           PERFORM UNTIL WS-EOF-PROV = 'Y'
               READ PROVIDER-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-PROV
                   NOT AT END
                       ADD 1 TO WS-FEE-TABLE-COUNT
                       MOVE FEE-PROVIDER-ID
                           TO WS-FT-PROVIDER-ID(WS-FEE-TABLE-COUNT)
                       MOVE FEE-PROCEDURE-CODE
                           TO WS-FT-PROC-CODE(WS-FEE-TABLE-COUNT)
                       MOVE FEE-CONTRACTED-RATE
                           TO WS-FT-RATE(WS-FEE-TABLE-COUNT)
                       MOVE FEE-EFFECTIVE-DATE
                           TO WS-FT-EFF-DATE(WS-FEE-TABLE-COUNT)
                       MOVE FEE-TERM-DATE
                           TO WS-FT-TERM-DATE(WS-FEE-TABLE-COUNT)
                       MOVE FEE-REQUIRES-PREAUTH
                           TO WS-FT-PREAUTH(WS-FEE-TABLE-COUNT)
               END-READ
           END-PERFORM
           DISPLAY 'FEE SCHEDULE LOADED: ' WS-FEE-TABLE-COUNT.

      *>================================================================*
      *> PROCESS-CLAIMS: Main claim processing loop
      *>================================================================*
       PROCESS-CLAIMS.
           PERFORM READ-NEXT-CLAIM-RECORD
           PERFORM UNTIL WS-CLM-EOF
               IF CFR-RECORD-TYPE = 'H'
                   PERFORM PROCESS-ONE-CLAIM
               ELSE
                   DISPLAY 'UNEXPECTED RECORD TYPE: '
                       CFR-RECORD-TYPE
                   PERFORM READ-NEXT-CLAIM-RECORD
               END-IF
           END-PERFORM.

      *>================================================================*
      *> READ-NEXT-CLAIM-RECORD: Simple sequential read
      *>================================================================*
       READ-NEXT-CLAIM-RECORD.
           READ CLAIMS-FILE
               AT END
                   SET WS-CLM-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-CLAIMS-READ
           END-READ.

      *>================================================================*
      *> PROCESS-ONE-CLAIM: Process header + service lines
      *>================================================================*
       PROCESS-ONE-CLAIM.
           ADD 1 TO WS-CLAIMS-PROCESSED
           MOVE 'N' TO WS-HAS-DENIAL

      *> Extract header fields via REDEFINES — the showcase
           MOVE CFR-CLAIM-ID TO WS-CURRENT-CLAIM-ID
           MOVE CFH-MEMBER-ID TO WS-CURRENT-MEMBER
           MOVE CFH-PROVIDER-ID TO WS-CURRENT-PROVIDER
           MOVE CFH-CLAIM-DATE TO WS-CURRENT-CLAIM-DATE
           MOVE CFH-POLICY-TYPE TO WS-CURRENT-POL-TYPE

      *> Extract diagnosis count and codes
      *> CFH-DIAG-COUNT controls how many are "active"
           MOVE CFH-DIAG-COUNT TO WS-DIAG-COUNT
           IF WS-DIAG-COUNT > 12
               MOVE 12 TO WS-DIAG-COUNT
           END-IF
           PERFORM VARYING WS-DIAG-IDX FROM 1 BY 1
               UNTIL WS-DIAG-IDX > WS-DIAG-COUNT
               MOVE CFH-DIAG-CODE(WS-DIAG-IDX)
                   TO WS-DIAG-CODE(WS-DIAG-IDX)
               MOVE 'Y' TO WS-DIAG-VALID(WS-DIAG-IDX)
           END-PERFORM

      *> Write header to adjudicated output
           MOVE CLAIM-FILE-RECORD TO ADJ-OUT-RECORD
           WRITE ADJ-OUT-RECORD
           ADD 1 TO WS-RECORDS-WRITTEN

      *> Look up policy for this member
           PERFORM FIND-POLICY
           IF WS-CUR-POL-FOUND = 'N'
               DISPLAY 'NO POLICY FOR MEMBER: ' WS-CURRENT-MEMBER
               MOVE 'Y' TO WS-HAS-DENIAL
           END-IF

      *> Initialize deductible remaining for this claim
           IF WS-CUR-POL-FOUND = 'Y'
               COMPUTE WS-DEDUCT-REMAINING =
                   WS-PT-DEDUCT-ANN(WS-CUR-POL-IDX)
                   - WS-PT-DEDUCT-MET(WS-CUR-POL-IDX)
               IF WS-DEDUCT-REMAINING < 0
                   MOVE 0 TO WS-DEDUCT-REMAINING
               END-IF
               COMPUTE WS-OOP-REMAINING =
                   WS-PT-OOP-MAX(WS-CUR-POL-IDX)
                   - WS-PT-OOP-YTD(WS-CUR-POL-IDX)
               IF WS-OOP-REMAINING < 0
                   MOVE 0 TO WS-OOP-REMAINING
               END-IF
           END-IF

      *> Read and accumulate service lines
           MOVE 0 TO WS-SVC-COUNT
           MOVE 0 TO WS-CLAIM-TOTAL-CHARGED
           MOVE 0 TO WS-CLAIM-TOTAL-PAID
           MOVE 0 TO WS-PATIENT-TOTAL
           PERFORM READ-SERVICE-LINES

      *> Adjudicate each service line
           PERFORM VARYING WS-SVC-IDX FROM 1 BY 1
               UNTIL WS-SVC-IDX > WS-SVC-COUNT
               PERFORM ADJUDICATE-LINE
           END-PERFORM

      *> Write payment record
           PERFORM WRITE-PAYMENT-RECORD

           DISPLAY 'CLAIM ' WS-CURRENT-CLAIM-ID
               ' LINES=' WS-SVC-COUNT
               ' PAID=$' WS-CLAIM-TOTAL-PAID.

      *>================================================================*
      *> READ-SERVICE-LINES: Read S records until next H or EOF
      *>================================================================*
       READ-SERVICE-LINES.
           PERFORM READ-NEXT-CLAIM-RECORD
           PERFORM UNTIL WS-CLM-EOF
               OR CFR-RECORD-TYPE NOT = 'S'
               ADD 1 TO WS-SVC-COUNT
      *> Use REDEFINES field names — CFS-* overlay on CFR-BODY
               MOVE CFS-LINE-NUMBER
                   TO WS-SVC-LINE-NUM(WS-SVC-COUNT)
               MOVE CFS-PROCEDURE-CODE
                   TO WS-SVC-PROC-CODE(WS-SVC-COUNT)
               MOVE CFS-DATE-FROM
                   TO WS-SVC-DATE-FROM(WS-SVC-COUNT)
               MOVE CFS-DATE-TO
                   TO WS-SVC-DATE-TO(WS-SVC-COUNT)
      *> COMP-3 charge — REDEFINES gives proper typed access
               MOVE CFS-CHARGE-AMT
                   TO WS-SVC-CHARGE(WS-SVC-COUNT)
               MOVE CFS-UNITS
                   TO WS-SVC-UNITS(WS-SVC-COUNT)
               MOVE CFS-PLACE-OF-SVC
                   TO WS-SVC-PLACE(WS-SVC-COUNT)

      *> Initialize adjudication fields
               MOVE 0 TO WS-SVC-ALLOWED(WS-SVC-COUNT)
               MOVE 0 TO WS-SVC-COPAY(WS-SVC-COUNT)
               MOVE 0 TO WS-SVC-DEDUCT(WS-SVC-COUNT)
               MOVE 0 TO WS-SVC-COINS(WS-SVC-COUNT)
               MOVE 0 TO WS-SVC-PAID(WS-SVC-COUNT)
               MOVE SPACES TO WS-SVC-STATUS(WS-SVC-COUNT)
               MOVE SPACES TO WS-SVC-REASON(WS-SVC-COUNT)
               MOVE 'N' TO WS-SVC-PREAUTH(WS-SVC-COUNT)

      *> Write service record to output (CLMRPT needs charge/proc)
               MOVE CLAIM-FILE-RECORD TO ADJ-OUT-RECORD
               WRITE ADJ-OUT-RECORD
               ADD 1 TO WS-RECORDS-WRITTEN

               PERFORM READ-NEXT-CLAIM-RECORD
           END-PERFORM.
      *> After exit: CLM-IN-RECORD has next H record or EOF

      *>================================================================*
      *> FIND-POLICY: Look up policy by member ID
      *>================================================================*
       FIND-POLICY.
           MOVE 'N' TO WS-CUR-POL-FOUND
           PERFORM VARYING WS-SEARCH-IDX FROM 1 BY 1
               UNTIL WS-SEARCH-IDX > WS-POL-TABLE-COUNT
               IF WS-PT-MEMBER-ID(WS-SEARCH-IDX)
                   = WS-CURRENT-MEMBER
                   MOVE 'Y' TO WS-CUR-POL-FOUND
                   MOVE WS-SEARCH-IDX TO WS-CUR-POL-IDX
               END-IF
           END-PERFORM.

      *>================================================================*
      *> ADJUDICATE-LINE: Process one service line
      *>================================================================*
       ADJUDICATE-LINE.
      *> Step 1: Validate via CALL to CLMVALID
           MOVE WS-CURRENT-MEMBER TO WS-VR-MEMBER-ID
           MOVE WS-CURRENT-PROVIDER TO WS-VR-PROVIDER-ID
           MOVE WS-SVC-PROC-CODE(WS-SVC-IDX) TO WS-VR-PROCEDURE-CODE
           IF WS-DIAG-COUNT > 0
               MOVE WS-DIAG-CODE(1) TO WS-VR-DIAG-CODE
           ELSE
               MOVE SPACES TO WS-VR-DIAG-CODE
           END-IF
           MOVE WS-SVC-DATE-FROM(WS-SVC-IDX) TO WS-VR-DATE-OF-SVC
           MOVE WS-SVC-PLACE(WS-SVC-IDX) TO WS-VR-PLACE-OF-SVC
           MOVE WS-SVC-CHARGE(WS-SVC-IDX) TO WS-VR-CHARGE-AMT

      *> CALL/LINKAGE showcase — the subprogram interface
           CALL 'CLMVALID' USING WS-VALID-REQUEST
                                  WS-VALID-RESPONSE

           IF WS-VR-INVALID
               MOVE 'DN' TO WS-SVC-STATUS(WS-SVC-IDX)
               MOVE WS-VR-ERROR-CODE TO WS-SVC-REASON(WS-SVC-IDX)
               MOVE 0 TO WS-SVC-PAID(WS-SVC-IDX)
               MOVE 'Y' TO WS-HAS-DENIAL
               PERFORM WRITE-ADJ-RECORD
               PERFORM WRITE-DENIED-RECORD
               EXIT PARAGRAPH
           END-IF

      *> Step 2: Look up fee schedule rate
           PERFORM FIND-FEE-RATE

      *> Step 3: Check pre-authorization requirement
           IF WS-PREAUTH-FLAG = 'Y'
               MOVE WS-SVC-PROC-CODE(WS-SVC-IDX)(1:1)
                   TO WS-PROC-PREFIX
      *> Radiology codes start with 7 — require pre-auth
               IF WS-PROC-PREFIX = '7'
                   MOVE 'DN' TO WS-SVC-STATUS(WS-SVC-IDX)
                   MOVE 'PA1' TO WS-SVC-REASON(WS-SVC-IDX)
                   MOVE 0 TO WS-SVC-PAID(WS-SVC-IDX)
                   MOVE 'Y' TO WS-HAS-DENIAL
                   PERFORM WRITE-ADJ-RECORD
                   PERFORM WRITE-DENIED-RECORD
                   EXIT PARAGRAPH
               END-IF
           END-IF

      *> Step 4: Adjudicate — the big EVALUATE
           IF WS-CUR-POL-FOUND = 'N'
               MOVE 'DN' TO WS-SVC-STATUS(WS-SVC-IDX)
               MOVE 'NPL' TO WS-SVC-REASON(WS-SVC-IDX)
               MOVE 0 TO WS-SVC-PAID(WS-SVC-IDX)
               MOVE 'Y' TO WS-HAS-DENIAL
               PERFORM WRITE-ADJ-RECORD
               PERFORM WRITE-DENIED-RECORD
               EXIT PARAGRAPH
           END-IF

           PERFORM COMPUTE-ADJUDICATION

      *> Step 4b: Cap copay at allowed amount
      *> Prevents copay from exceeding line value (e.g. ER $150
      *> copay on $49.50 lab — patient can't owe more than allowed)
           IF WS-LINE-COPAY > WS-LINE-ALLOWED
               MOVE WS-LINE-ALLOWED TO WS-LINE-COPAY
           END-IF

      *> Step 5: Apply deductible
           PERFORM APPLY-DEDUCTIBLE

      *> Step 6: Compute coinsurance and plan payment
           PERFORM COMPUTE-PAYMENT

      *> Step 7: Apply OOP maximum cap
           PERFORM APPLY-OOP-CAP

      *> Step 8: Set status and write output
      *> AP = fully approved, PR = partial (patient owes something)
      *> Note: deductible-absorbed lines are PR, not DN — the claim
      *> was processed; patient just owes via deductible. DN means
      *> the claim was not covered at all (validation/preauth denial).
           IF WS-LINE-PAID >= WS-LINE-ALLOWED AND WS-LINE-PAID > 0
               MOVE 'AP' TO WS-SVC-STATUS(WS-SVC-IDX)
               MOVE SPACES TO WS-SVC-REASON(WS-SVC-IDX)
           ELSE
               MOVE 'PR' TO WS-SVC-STATUS(WS-SVC-IDX)
               IF WS-LINE-PAID = 0
                   MOVE 'DED' TO WS-SVC-REASON(WS-SVC-IDX)
               ELSE
                   MOVE SPACES TO WS-SVC-REASON(WS-SVC-IDX)
               END-IF
           END-IF
           MOVE WS-LINE-ALLOWED TO WS-SVC-ALLOWED(WS-SVC-IDX)
           MOVE WS-LINE-COPAY TO WS-SVC-COPAY(WS-SVC-IDX)
           MOVE WS-LINE-DEDUCT TO WS-SVC-DEDUCT(WS-SVC-IDX)
           MOVE WS-LINE-COINS TO WS-SVC-COINS(WS-SVC-IDX)
           MOVE WS-LINE-PAID TO WS-SVC-PAID(WS-SVC-IDX)

           ADD WS-SVC-CHARGE(WS-SVC-IDX) TO WS-CLAIM-TOTAL-CHARGED
           ADD WS-LINE-PAID TO WS-CLAIM-TOTAL-PAID

           PERFORM WRITE-ADJ-RECORD.

      *>================================================================*
      *> COMPUTE-ADJUDICATION: The 15+ WHEN EVALUATE showcase
      *>================================================================*
       COMPUTE-ADJUDICATION.
           MOVE 0 TO WS-LINE-ALLOWED
           MOVE 0 TO WS-LINE-COPAY
           MOVE WS-SVC-PROC-CODE(WS-SVC-IDX)(1:1)
               TO WS-PROC-PREFIX

           EVALUATE TRUE
      *> --- PPO + Office Visit ---
               WHEN WS-CURRENT-POL-TYPE = 'P'
                AND WS-SVC-PLACE(WS-SVC-IDX) = '11'
                AND WS-PROC-PREFIX NOT = '7'
                AND WS-PROC-PREFIX NOT = '8'
      *> PPO office: allowed = min(charge, contracted rate)
                   IF WS-SVC-CHARGE(WS-SVC-IDX) < WS-FEE-RATE
                       MOVE WS-SVC-CHARGE(WS-SVC-IDX)
                           TO WS-LINE-ALLOWED
                   ELSE
                       MOVE WS-FEE-RATE TO WS-LINE-ALLOWED
                   END-IF
                   MOVE WS-PT-COPAY(WS-CUR-POL-IDX)
                       TO WS-LINE-COPAY

      *> --- PPO + Outpatient ---
               WHEN WS-CURRENT-POL-TYPE = 'P'
                AND WS-SVC-PLACE(WS-SVC-IDX) = '22'
                AND WS-PROC-PREFIX NOT = '7'
      *> PPO outpatient: contracted rate, no balance billing
                   MOVE WS-FEE-RATE TO WS-LINE-ALLOWED
                   MOVE WS-PT-COPAY(WS-CUR-POL-IDX)
                       TO WS-LINE-COPAY

      *> --- PPO + Emergency ---
               WHEN WS-CURRENT-POL-TYPE = 'P'
                AND WS-SVC-PLACE(WS-SVC-IDX) = '23'
      *> PPO emergency: 90% of charge (no network discount)
                   COMPUTE WS-LINE-ALLOWED ROUNDED =
                       WS-SVC-CHARGE(WS-SVC-IDX) * 0.90
                   END-COMPUTE
                   MOVE 0 TO WS-LINE-COPAY

      *> --- PPO + Inpatient ---
               WHEN WS-CURRENT-POL-TYPE = 'P'
                AND WS-SVC-PLACE(WS-SVC-IDX) = '21'
      *> PPO inpatient: per-diem from fee schedule
                   MOVE WS-FEE-RATE TO WS-LINE-ALLOWED
                   MOVE 0 TO WS-LINE-COPAY

      *> --- HMO + Office Visit ---
               WHEN WS-CURRENT-POL-TYPE = 'H'
                AND WS-SVC-PLACE(WS-SVC-IDX) = '11'
                AND WS-PROC-PREFIX NOT = '7'
                AND WS-PROC-PREFIX NOT = '8'
      *> HMO office: flat copay, plan pays rest at 100%
                   IF WS-SVC-CHARGE(WS-SVC-IDX) < WS-FEE-RATE
                       MOVE WS-SVC-CHARGE(WS-SVC-IDX)
                           TO WS-LINE-ALLOWED
                   ELSE
                       MOVE WS-FEE-RATE TO WS-LINE-ALLOWED
                   END-IF
                   MOVE WS-PT-COPAY(WS-CUR-POL-IDX)
                       TO WS-LINE-COPAY

      *> --- HMO + Outpatient ---
               WHEN WS-CURRENT-POL-TYPE = 'H'
                AND WS-SVC-PLACE(WS-SVC-IDX) = '22'
      *> HMO outpatient: copay + 100% coverage
                   MOVE WS-FEE-RATE TO WS-LINE-ALLOWED
                   MOVE WS-PT-COPAY(WS-CUR-POL-IDX)
                       TO WS-LINE-COPAY

      *> --- HMO + Emergency ---
               WHEN WS-CURRENT-POL-TYPE = 'H'
                AND WS-SVC-PLACE(WS-SVC-IDX) = '23'
      *> HMO emergency: ER copay ($150 override), plan pays rest
                   COMPUTE WS-LINE-ALLOWED ROUNDED =
                       WS-SVC-CHARGE(WS-SVC-IDX) * 0.90
                   END-COMPUTE
                   MOVE 150.00 TO WS-LINE-COPAY

      *> --- HMO + Inpatient ---
               WHEN WS-CURRENT-POL-TYPE = 'H'
                AND WS-SVC-PLACE(WS-SVC-IDX) = '21'
      *> HMO inpatient: per-diem, no copay
                   MOVE WS-FEE-RATE TO WS-LINE-ALLOWED
                   MOVE 0 TO WS-LINE-COPAY

      *> --- Indemnity + Office Visit ---
               WHEN WS-CURRENT-POL-TYPE = 'I'
                AND WS-SVC-PLACE(WS-SVC-IDX) = '11'
                AND WS-PROC-PREFIX NOT = '7'
                AND WS-PROC-PREFIX NOT = '8'
      *> Indemnity: usual & customary (contracted rate)
                   IF WS-SVC-CHARGE(WS-SVC-IDX) < WS-FEE-RATE
                       MOVE WS-SVC-CHARGE(WS-SVC-IDX)
                           TO WS-LINE-ALLOWED
                   ELSE
                       MOVE WS-FEE-RATE TO WS-LINE-ALLOWED
                   END-IF
                   MOVE 0 TO WS-LINE-COPAY

      *> --- Indemnity + Outpatient ---
               WHEN WS-CURRENT-POL-TYPE = 'I'
                AND WS-SVC-PLACE(WS-SVC-IDX) = '22'
      *> Indemnity outpatient: contracted rate after deductible
                   MOVE WS-FEE-RATE TO WS-LINE-ALLOWED
                   MOVE 0 TO WS-LINE-COPAY

      *> --- Indemnity + Emergency ---
               WHEN WS-CURRENT-POL-TYPE = 'I'
                AND WS-SVC-PLACE(WS-SVC-IDX) = '23'
      *> Indemnity ER: 80/20 override (better than 70/30)
                   COMPUTE WS-LINE-ALLOWED ROUNDED =
                       WS-SVC-CHARGE(WS-SVC-IDX) * 0.90
                   END-COMPUTE
                   MOVE 0 TO WS-LINE-COPAY

      *> --- Indemnity + Inpatient ---
               WHEN WS-CURRENT-POL-TYPE = 'I'
                AND WS-SVC-PLACE(WS-SVC-IDX) = '21'
      *> Indemnity inpatient: contracted rate after deductible
                   MOVE WS-FEE-RATE TO WS-LINE-ALLOWED
                   MOVE 0 TO WS-LINE-COPAY

      *> --- Radiology (proc code starts with 7) ---
               WHEN WS-PROC-PREFIX = '7'
      *> All plan types: use contracted rate
                   MOVE WS-FEE-RATE TO WS-LINE-ALLOWED
                   MOVE 0 TO WS-LINE-COPAY

      *> --- Pathology (proc code starts with 8) ---
               WHEN WS-PROC-PREFIX = '8'
      *> All plan types: no copay, deductible + coinsurance
                   IF WS-SVC-CHARGE(WS-SVC-IDX) < WS-FEE-RATE
                       MOVE WS-SVC-CHARGE(WS-SVC-IDX)
                           TO WS-LINE-ALLOWED
                   ELSE
                       MOVE WS-FEE-RATE TO WS-LINE-ALLOWED
                   END-IF
                   MOVE 0 TO WS-LINE-COPAY

      *> --- Default: deny ---
               WHEN OTHER
                   MOVE 'DN' TO WS-SVC-STATUS(WS-SVC-IDX)
                   MOVE 'UNK' TO WS-SVC-REASON(WS-SVC-IDX)
                   MOVE 0 TO WS-LINE-ALLOWED
                   MOVE 0 TO WS-LINE-COPAY
           END-EVALUATE.

      *>================================================================*
      *> APPLY-DEDUCTIBLE: Deductible accumulates across lines
      *>================================================================*
       APPLY-DEDUCTIBLE.
           MOVE 0 TO WS-LINE-DEDUCT

      *> HMO typically has no deductible — skip if zero
           IF WS-DEDUCT-REMAINING <= ZERO
               EXIT PARAGRAPH
           END-IF

      *> Amount subject to deductible = allowed - copay
           COMPUTE WS-LINE-DEDUCT =
               WS-LINE-ALLOWED - WS-LINE-COPAY

           IF WS-LINE-DEDUCT <= ZERO
               MOVE 0 TO WS-LINE-DEDUCT
               EXIT PARAGRAPH
           END-IF

      *> If amount subject <= remaining deductible, all goes to deduct
           IF WS-LINE-DEDUCT <= WS-DEDUCT-REMAINING
               SUBTRACT WS-LINE-DEDUCT FROM WS-DEDUCT-REMAINING
           ELSE
      *> Only remaining deductible applies, rest goes to coinsurance
               MOVE WS-DEDUCT-REMAINING TO WS-LINE-DEDUCT
               MOVE 0 TO WS-DEDUCT-REMAINING
           END-IF.

      *>================================================================*
      *> COMPUTE-PAYMENT: Coinsurance + plan pays (ROUNDED showcase)
      *>================================================================*
       COMPUTE-PAYMENT.
      *> Amount subject to coinsurance:
      *>   allowed - copay - deductible applied
           COMPUTE WS-LINE-COINS =
               WS-LINE-ALLOWED - WS-LINE-COPAY - WS-LINE-DEDUCT

           IF WS-LINE-COINS < 0
               MOVE 0 TO WS-LINE-COINS
           END-IF

      *> COMP-3 ROUNDED showcase — the pedagogical core:
      *> CORRECT: ROUNDED forces half-up rounding
      *> $100.00 * 0.835 = $83.50 (ROUNDED) vs $83.49 (truncated)
      *> Over 10,000 claims/day, that's real money.
           COMPUTE WS-LINE-PAID ROUNDED =
               WS-LINE-COINS
               * WS-PT-COINS-RATE(WS-CUR-POL-IDX)
           END-COMPUTE

      *> Patient coinsurance responsibility
           COMPUTE WS-LINE-COINS ROUNDED =
               WS-LINE-COINS - WS-LINE-PAID
           END-COMPUTE

           IF WS-LINE-PAID < 0
               MOVE 0 TO WS-LINE-PAID
           END-IF.

      *>================================================================*
      *> APPLY-OOP-CAP: Cap patient responsibility at OOP max
      *>================================================================*
       APPLY-OOP-CAP.
      *> Patient responsibility = copay + deductible + coinsurance
           COMPUTE WS-PATIENT-TOTAL =
               WS-LINE-COPAY + WS-LINE-DEDUCT + WS-LINE-COINS

      *> Check if this pushes patient past OOP max
           IF WS-OOP-REMAINING > 0
               IF WS-PATIENT-TOTAL > WS-OOP-REMAINING
      *> Cap patient at remaining OOP
      *> Plan picks up the excess
                   COMPUTE WS-LINE-PAID ROUNDED =
                       WS-LINE-PAID
                       + (WS-PATIENT-TOTAL - WS-OOP-REMAINING)
                   END-COMPUTE
                   COMPUTE WS-LINE-COINS =
                       WS-OOP-REMAINING
                       - WS-LINE-COPAY - WS-LINE-DEDUCT
                   IF WS-LINE-COINS < 0
                       MOVE 0 TO WS-LINE-COINS
                   END-IF
                   MOVE 0 TO WS-OOP-REMAINING
               ELSE
                   SUBTRACT WS-PATIENT-TOTAL FROM WS-OOP-REMAINING
               END-IF
           ELSE
      *> OOP max already hit — plan pays everything above copay
               COMPUTE WS-LINE-PAID ROUNDED =
                   WS-LINE-ALLOWED - WS-LINE-COPAY
               END-COMPUTE
               MOVE 0 TO WS-LINE-DEDUCT
               MOVE 0 TO WS-LINE-COINS
           END-IF.

      *>================================================================*
      *> FIND-FEE-RATE: Look up contracted rate for provider+procedure
      *>================================================================*
       FIND-FEE-RATE.
           MOVE 0 TO WS-FEE-RATE
           MOVE 'N' TO WS-PREAUTH-FLAG
           PERFORM VARYING WS-SEARCH-IDX FROM 1 BY 1
               UNTIL WS-SEARCH-IDX > WS-FEE-TABLE-COUNT
               IF WS-FT-PROVIDER-ID(WS-SEARCH-IDX)
                   = WS-CURRENT-PROVIDER
               AND WS-FT-PROC-CODE(WS-SEARCH-IDX)
                   = WS-SVC-PROC-CODE(WS-SVC-IDX)
                   MOVE WS-FT-RATE(WS-SEARCH-IDX) TO WS-FEE-RATE
                   MOVE WS-FT-PREAUTH(WS-SEARCH-IDX)
                       TO WS-PREAUTH-FLAG
               END-IF
           END-PERFORM
      *> If no rate found, use 80% of charge as fallback
           IF WS-FEE-RATE = 0
               COMPUTE WS-FEE-RATE ROUNDED =
                   WS-SVC-CHARGE(WS-SVC-IDX) * 0.80
               END-COMPUTE
           END-IF.

      *>================================================================*
      *> WRITE-ADJ-RECORD: Write adjudication record (A type)
      *>================================================================*
       WRITE-ADJ-RECORD.
      *> Build adjudication record via WS output buffer
           INITIALIZE WS-OUT-REC
           MOVE 'A' TO WS-OR-RECORD-TYPE
           MOVE WS-CURRENT-CLAIM-ID TO WS-OR-CLAIM-ID
      *> Use WS-OR-ADJ REDEFINES to place COMP-3 fields correctly
           MOVE WS-SVC-LINE-NUM(WS-SVC-IDX)
               TO WS-OA-LINE-NUMBER
           MOVE WS-SVC-ALLOWED(WS-SVC-IDX)
               TO WS-OA-ALLOWED-AMT
           MOVE WS-SVC-COPAY(WS-SVC-IDX)
               TO WS-OA-COPAY-AMT
           MOVE WS-SVC-DEDUCT(WS-SVC-IDX)
               TO WS-OA-DEDUCT-AMT
           MOVE WS-SVC-COINS(WS-SVC-IDX)
               TO WS-OA-COINS-AMT
           MOVE WS-SVC-PAID(WS-SVC-IDX)
               TO WS-OA-PAID-AMT
           MOVE WS-SVC-STATUS(WS-SVC-IDX)
               TO WS-OA-STATUS
           MOVE WS-SVC-REASON(WS-SVC-IDX)
               TO WS-OA-REASON-CODE
           MOVE WS-OUT-REC TO ADJ-OUT-RECORD
           WRITE ADJ-OUT-RECORD
           ADD 1 TO WS-RECORDS-WRITTEN.

      *>================================================================*
      *> WRITE-PAYMENT-RECORD: Write payment summary (P type)
      *>================================================================*
       WRITE-PAYMENT-RECORD.
      *> Build payment record via WS output buffer
           INITIALIZE WS-OUT-REC
           MOVE 'P' TO WS-OR-RECORD-TYPE
           MOVE WS-CURRENT-CLAIM-ID TO WS-OR-CLAIM-ID
      *> Use WS-OR-PAY REDEFINES for COMP-3 total
           MOVE 'E' TO WS-OP-PAY-METHOD
           MOVE WS-CLAIM-TOTAL-PAID TO WS-OP-TOTAL-PAID
           MOVE 00000000 TO WS-OP-CHECK-NUMBER
           MOVE WS-CURRENT-CLAIM-DATE TO WS-OP-PAY-DATE
           MOVE WS-CURRENT-PROVIDER TO WS-OP-PAYEE-NAME
           MOVE WS-OUT-REC TO ADJ-OUT-RECORD
           WRITE ADJ-OUT-RECORD
           ADD 1 TO WS-RECORDS-WRITTEN

           ADD WS-CLAIM-TOTAL-PAID TO WS-TOTAL-PAID
           ADD WS-CLAIM-TOTAL-CHARGED TO WS-TOTAL-CHARGED.

      *>================================================================*
      *> WRITE-DENIED-RECORD: Log denied line to DENIED.DAT
      *>================================================================*
       WRITE-DENIED-RECORD.
           INITIALIZE WS-DENIED-LINE
           MOVE WS-CURRENT-CLAIM-ID TO WS-DL-CLAIM-ID
           MOVE WS-SVC-LINE-NUM(WS-SVC-IDX) TO WS-DL-LINE-NUM
           MOVE WS-SVC-PROC-CODE(WS-SVC-IDX) TO WS-DL-PROC-CODE
           MOVE WS-SVC-REASON(WS-SVC-IDX) TO WS-DL-REASON
           IF WS-VR-INVALID
               MOVE WS-VR-ERROR-MSG TO WS-DL-ERROR-MSG
           ELSE
               MOVE 'PRE-AUTH REQUIRED - NOT ON FILE'
                   TO WS-DL-ERROR-MSG
           END-IF
           MOVE WS-SVC-CHARGE(WS-SVC-IDX) TO WS-DL-CHARGE
           MOVE WS-DENIED-LINE TO DENIED-RECORD
           WRITE DENIED-RECORD
           ADD 1 TO WS-CLAIMS-DENIED.

      *>================================================================*
      *> CLOSE-FILES: Close all files
      *>================================================================*
       CLOSE-FILES.
           CLOSE CLAIMS-FILE
           CLOSE POLICY-FILE
           CLOSE PROVIDER-FILE
           CLOSE ADJ-FILE
           CLOSE DENIED-FILE.

      *>================================================================*
      *> DISPLAY-SUMMARY: Final processing statistics
      *>================================================================*
       DISPLAY-SUMMARY.
           DISPLAY '============================================='
           DISPLAY 'CLAIMS ADJUDICATION SUMMARY'
           DISPLAY '============================================='
           DISPLAY 'RECORDS READ:     ' WS-CLAIMS-READ
           DISPLAY 'CLAIMS PROCESSED: ' WS-CLAIMS-PROCESSED
           DISPLAY 'LINES DENIED:     ' WS-CLAIMS-DENIED
           DISPLAY 'RECORDS WRITTEN:  ' WS-RECORDS-WRITTEN
           DISPLAY 'TOTAL CHARGED:    $' WS-TOTAL-CHARGED
           DISPLAY 'TOTAL PAID:       $' WS-TOTAL-PAID
           DISPLAY '============================================='.
