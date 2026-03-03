      *>================================================================*
      *> FEESCHED.cpy — Provider fee schedule record
      *> COMP-3 contracted rates for procedure/provider lookups.
      *>================================================================*
       01  FEE-SCHEDULE-RECORD.
           05  FEE-PROVIDER-ID           PIC X(10).
           05  FEE-PROCEDURE-CODE        PIC X(5).
           05  FEE-CONTRACTED-RATE       PIC S9(5)V99 COMP-3.
           05  FEE-EFFECTIVE-DATE        PIC 9(8).
           05  FEE-TERM-DATE            PIC 9(8).
           05  FEE-REQUIRES-PREAUTH     PIC X(1).
               88  FEE-PREAUTH-YES       VALUE 'Y'.
               88  FEE-PREAUTH-NO        VALUE 'N'.
           05  FEE-FILLER               PIC X(27).
