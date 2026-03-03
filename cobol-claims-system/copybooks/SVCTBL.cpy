      *>================================================================*
      *> SVCTBL.cpy — Working-storage service line accumulator
      *> OCCURS DEPENDING ON showcase: variable-length table of
      *> service lines with adjudication results per claim.
      *>================================================================*
       01  WS-SERVICE-TABLE.
           05  WS-SVC-COUNT              PIC 9(2) VALUE 0.
           05  WS-SVC-ENTRIES.
               10  WS-SVC-ENTRY
                   OCCURS 1 TO 25
                   DEPENDING ON WS-SVC-COUNT.
                   15  WS-SVC-LINE-NUM    PIC 9(2).
                   15  WS-SVC-PROC-CODE   PIC X(5).
                   15  WS-SVC-DATE-FROM   PIC 9(8).
                   15  WS-SVC-DATE-TO     PIC 9(8).
                   15  WS-SVC-CHARGE      PIC S9(7)V99 COMP-3.
                   15  WS-SVC-UNITS       PIC 9(3).
                   15  WS-SVC-PLACE       PIC X(2).
                   15  WS-SVC-ALLOWED     PIC S9(7)V99 COMP-3.
                   15  WS-SVC-COPAY       PIC S9(5)V99 COMP-3.
                   15  WS-SVC-DEDUCT      PIC S9(5)V99 COMP-3.
                   15  WS-SVC-COINS       PIC S9(5)V99 COMP-3.
                   15  WS-SVC-PAID        PIC S9(7)V99 COMP-3.
                   15  WS-SVC-STATUS      PIC X(2).
                   15  WS-SVC-REASON      PIC X(3).
                   15  WS-SVC-PREAUTH     PIC X(1).
