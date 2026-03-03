      *>================================================================*
      *> TRANSREC.cpy — Transaction Record (47 bytes)
      *> Layout: LINE SEQUENTIAL, display numeric amounts
      *>================================================================*
       01  TRANSACTION-RECORD.
           05  TRANS-ACCT-ID          PIC X(8).
           05  TRANS-TYPE             PIC X(1).
               88  TRANS-DEPOSIT          VALUE 'D'.
               88  TRANS-WITHDRAW         VALUE 'W'.
               88  TRANS-INTEREST         VALUE 'I'.
               88  TRANS-VALID-TYPE       VALUE 'D' 'W' 'I'.
           05  TRANS-AMOUNT           PIC 9(7)V99.
           05  TRANS-DATE             PIC 9(8).
           05  TRANS-BRANCH           PIC X(3).
           05  TRANS-SEQ              PIC 9(6).
           05  TRANS-DESC             PIC X(12).
