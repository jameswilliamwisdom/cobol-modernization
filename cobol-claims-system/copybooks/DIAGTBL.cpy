      *>================================================================*
      *> DIAGTBL.cpy — Working-storage diagnosis table
      *> OCCURS DEPENDING ON showcase: variable-length diagnosis list
      *> accumulated per claim from header record.
      *>================================================================*
       01  WS-DIAG-TABLE.
           05  WS-DIAG-COUNT             PIC 9(2) VALUE 0.
           05  WS-DIAG-ENTRIES.
               10  WS-DIAG-ENTRY
                   OCCURS 1 TO 12
                   DEPENDING ON WS-DIAG-COUNT.
                   15  WS-DIAG-CODE       PIC X(7).
                   15  WS-DIAG-VALID      PIC X(1).
                       88  WS-DIAG-IS-VALID   VALUE 'Y'.
                       88  WS-DIAG-IS-INVALID VALUE 'N'.
