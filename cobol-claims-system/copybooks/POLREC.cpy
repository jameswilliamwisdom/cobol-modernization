      *>================================================================*
      *> POLREC.cpy — Policy/benefit record
      *> Coverage rules: copay, coinsurance, deductible, OOP max.
      *>================================================================*
       01  POLICY-RECORD.
           05  POL-POLICY-ID            PIC X(5).
           05  POL-MEMBER-ID            PIC X(10).
           05  POL-TYPE                 PIC X(1).
               88  POL-IS-PPO           VALUE 'P'.
               88  POL-IS-HMO           VALUE 'H'.
               88  POL-IS-INDEMNITY     VALUE 'I'.
           05  POL-EFFECTIVE-DATE       PIC 9(8).
           05  POL-TERM-DATE           PIC 9(8).
           05  POL-COPAY-AMT           PIC S9(3)V99 COMP-3.
           05  POL-COINSURANCE-RATE    PIC 9V99.
           05  POL-DEDUCTIBLE-ANNUAL   PIC S9(5)V99 COMP-3.
           05  POL-DEDUCTIBLE-MET      PIC S9(5)V99 COMP-3.
           05  POL-OUT-OF-POCKET-MAX   PIC S9(5)V99 COMP-3.
           05  POL-OUT-OF-POCKET-YTD   PIC S9(5)V99 COMP-3.
           05  POL-FILLER              PIC X(10).
