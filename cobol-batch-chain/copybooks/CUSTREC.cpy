      *>================================================================*
      *> CUSTREC.cpy — Customer Master Record (68 bytes)
      *> Layout: Fixed-length sequential, binary (COMP-3 fields)
      *>================================================================*
       01  CUSTOMER-RECORD.
           05  CUST-ID                PIC X(8).
           05  CUST-NAME              PIC X(25).
           05  CUST-BRANCH            PIC X(3).
           05  CUST-ACCT-TYPE         PIC X(1).
               88  CUST-SAVINGS           VALUE 'S'.
               88  CUST-CHECKING          VALUE 'C'.
           05  CUST-STATUS            PIC X(1).
               88  CUST-ACTIVE            VALUE 'A'.
               88  CUST-CLOSED            VALUE 'C'.
               88  CUST-FROZEN            VALUE 'F'.
           05  CUST-BALANCE           PIC S9(7)V99 COMP-3.
           05  CUST-OPEN-DATE         PIC 9(8).
           05  CUST-LAST-ACTIVITY     PIC 9(8).
           05  CUST-FILLER            PIC X(9).
