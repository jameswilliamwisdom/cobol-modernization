      *>================================================================*
      *> AUDITREC.cpy — Audit Trail Record (50 bytes)
      *> Layout: Binary sequential (COMP-3 balance fields)
      *>================================================================*
       01  AUDIT-RECORD.
           05  AUD-ACCT-ID           PIC X(8).
           05  AUD-TRANS-TYPE        PIC X(1).
           05  AUD-AMOUNT            PIC 9(7)V99.
           05  AUD-DATE              PIC 9(8).
           05  AUD-OLD-BALANCE       PIC S9(7)V99 COMP-3.
           05  AUD-NEW-BALANCE       PIC S9(7)V99 COMP-3.
           05  AUD-STATUS            PIC X(1).
               88  AUD-APPLIED           VALUE 'A'.
               88  AUD-REJECTED          VALUE 'R'.
           05  AUD-BRANCH            PIC X(3).
           05  AUD-SEQ               PIC 9(6).
           05  AUD-FILLER            PIC X(4).
