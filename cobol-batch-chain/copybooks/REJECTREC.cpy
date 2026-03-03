      *>================================================================*
      *> REJECTREC.cpy — Reject Record (62 bytes)
      *> Layout: LINE SEQUENTIAL, reason codes via 88-levels
      *>================================================================*
       01  REJECT-RECORD.
           05  REJ-ACCT-ID           PIC X(8).
           05  REJ-TRANS-TYPE        PIC X(1).
           05  REJ-AMOUNT            PIC 9(7)V99.
           05  REJ-DATE              PIC 9(8).
           05  REJ-BRANCH            PIC X(3).
           05  REJ-SEQ               PIC 9(6).
           05  REJ-REASON            PIC X(2).
               88  REJ-NO-ACCOUNT        VALUE 'NA'.
               88  REJ-INVALID-TYPE      VALUE 'IT'.
               88  REJ-INVALID-AMOUNT    VALUE 'IA'.
               88  REJ-CLOSED            VALUE 'CL'.
               88  REJ-FROZEN            VALUE 'FR'.
               88  REJ-OVERDRAFT         VALUE 'OD'.
           05  REJ-STEP              PIC X(8).
           05  REJ-FILLER            PIC X(9).
