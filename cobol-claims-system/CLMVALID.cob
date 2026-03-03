      *>================================================================*
      *> CLMVALID.cob — Claim validation subprogram
      *> Compile: cobc -m -free -I copybooks CLMVALID.cob
      *> CALL/LINKAGE SECTION showcase:
      *>   - Called from CLMPROC via CALL 'CLMVALID'
      *>   - Receives VALID-REQUEST, returns VALID-RESPONSE
      *>   - Uses GOBACK (not STOP RUN) to return to caller
      *>   - No type safety — caller/callee must agree on layout
      *>================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMVALID.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-FIRST-CHAR             PIC X(1).
       01  WS-DIAG-REST              PIC X(6).
       01  WS-DIAG-CHECK             PIC X(7).
       01  WS-NUMERIC-CHECK          PIC 9(6).
       01  WS-IDX                    PIC 9(2).
       01  WS-CURRENT-DATE-INT       PIC 9(8).
       01  WS-DATE-GROUP.
           05  WS-CURR-YEAR          PIC 9(4).
           05  WS-CURR-MONTH         PIC 9(2).
           05  WS-CURR-DAY           PIC 9(2).
       01  WS-CHARGE-DISPLAY         PIC S9(7)V99.

      *> Valid member ID list (loaded from caller context)
       01  WS-VALID-PLACES.
           05  FILLER                 PIC X(2) VALUE '11'.
           05  FILLER                 PIC X(2) VALUE '21'.
           05  FILLER                 PIC X(2) VALUE '22'.
           05  FILLER                 PIC X(2) VALUE '23'.
       01  WS-PLACE-TABLE REDEFINES WS-VALID-PLACES.
           05  WS-PLACE-ENTRY        PIC X(2) OCCURS 4.
       01  WS-PLACE-FOUND            PIC X(1).

       LINKAGE SECTION.
       COPY VALIDREQ.

       PROCEDURE DIVISION USING VALID-REQUEST VALID-RESPONSE.
       VALIDATE-CLAIM.
      *> Initialize response to valid
           SET VR-VALID TO TRUE
           MOVE SPACES TO VR-ERROR-CODE
           MOVE SPACES TO VR-ERROR-MSG

      *> E01: Member ID must not be blank
           IF VR-MEMBER-ID = SPACES
               SET VR-INVALID TO TRUE
               MOVE 'E01' TO VR-ERROR-CODE
               MOVE 'MEMBER ID IS BLANK' TO VR-ERROR-MSG
               GOBACK
           END-IF

      *> E02: Provider ID must not be blank
           IF VR-PROVIDER-ID = SPACES
               SET VR-INVALID TO TRUE
               MOVE 'E02' TO VR-ERROR-CODE
               MOVE 'PROVIDER ID IS BLANK' TO VR-ERROR-MSG
               GOBACK
           END-IF

      *> E03: Procedure code must not be blank
           IF VR-PROCEDURE-CODE = SPACES
               SET VR-INVALID TO TRUE
               MOVE 'E03' TO VR-ERROR-CODE
               MOVE 'PROCEDURE CODE IS BLANK' TO VR-ERROR-MSG
               GOBACK
           END-IF

      *> E04: Diagnosis code format (ICD-10: letter + digits)
           MOVE VR-DIAG-CODE TO WS-DIAG-CHECK
           MOVE WS-DIAG-CHECK(1:1) TO WS-FIRST-CHAR
           IF WS-FIRST-CHAR < 'A' OR WS-FIRST-CHAR > 'Z'
               SET VR-INVALID TO TRUE
               MOVE 'E04' TO VR-ERROR-CODE
               MOVE 'INVALID DIAG CODE - MUST START WITH LETTER'
                   TO VR-ERROR-MSG
               GOBACK
           END-IF
           MOVE WS-DIAG-CHECK(2:6) TO WS-DIAG-REST
           INSPECT WS-DIAG-REST
               TALLYING WS-IDX FOR ALL SPACES
      *> After removing spaces, remaining must be numeric
           MOVE 0 TO WS-IDX
           PERFORM VARYING WS-IDX FROM 2 BY 1
               UNTIL WS-IDX > 7
               IF WS-DIAG-CHECK(WS-IDX:1) NOT = SPACE
                   IF WS-DIAG-CHECK(WS-IDX:1) < '0'
                   OR WS-DIAG-CHECK(WS-IDX:1) > '9'
                       SET VR-INVALID TO TRUE
                       MOVE 'E04' TO VR-ERROR-CODE
                       MOVE 'INVALID DIAG CODE - NON-NUMERIC AFTER'
                       & ' LETTER' TO VR-ERROR-MSG
                       GOBACK
                   END-IF
               END-IF
           END-PERFORM

      *> E05: Date of service not in future
           MOVE FUNCTION CURRENT-DATE(1:8)
               TO WS-CURRENT-DATE-INT
           IF VR-DATE-OF-SVC > WS-CURRENT-DATE-INT
               SET VR-INVALID TO TRUE
               MOVE 'E05' TO VR-ERROR-CODE
               MOVE 'DATE OF SERVICE IS IN THE FUTURE'
                   TO VR-ERROR-MSG
               GOBACK
           END-IF

      *> E06: Date of service must be valid (not zero)
           IF VR-DATE-OF-SVC = 0
               SET VR-INVALID TO TRUE
               MOVE 'E06' TO VR-ERROR-CODE
               MOVE 'DATE OF SERVICE IS ZERO' TO VR-ERROR-MSG
               GOBACK
           END-IF

      *> E07: Charge amount must be positive
           MOVE VR-CHARGE-AMT TO WS-CHARGE-DISPLAY
           IF WS-CHARGE-DISPLAY NOT > ZERO
               SET VR-INVALID TO TRUE
               MOVE 'E07' TO VR-ERROR-CODE
               MOVE 'CHARGE AMOUNT MUST BE GREATER THAN ZERO'
                   TO VR-ERROR-MSG
               GOBACK
           END-IF

      *> E08: Place of service must be valid
           MOVE 'N' TO WS-PLACE-FOUND
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > 4
               IF VR-PLACE-OF-SVC = WS-PLACE-ENTRY(WS-IDX)
                   MOVE 'Y' TO WS-PLACE-FOUND
               END-IF
           END-PERFORM
           IF WS-PLACE-FOUND = 'N'
               SET VR-INVALID TO TRUE
               MOVE 'E08' TO VR-ERROR-CODE
               MOVE 'INVALID PLACE OF SERVICE CODE'
                   TO VR-ERROR-MSG
               GOBACK
           END-IF

      *> All checks passed
           GOBACK.
