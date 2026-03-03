      *>================================================================*
      *> CLAIMREC.cpy — Master claim file record with REDEFINES
      *> Fixed 200-byte record, 4 interpretations via REDEFINES:
      *>   H = Header    (member, provider, diagnoses)
      *>   S = Service   (procedure line with charges)
      *>   A = Adjudication (allowed, copay, deductible, paid)
      *>   P = Payment   (check/EFT summary per claim)
      *> The REDEFINES showcase — same bytes, different layouts.
      *>================================================================*
       01  CLAIM-FILE-RECORD.
           05  CFR-RECORD-TYPE            PIC X(1).
               88  CFR-IS-HEADER          VALUE 'H'.
               88  CFR-IS-SERVICE         VALUE 'S'.
               88  CFR-IS-ADJUDICATION    VALUE 'A'.
               88  CFR-IS-PAYMENT         VALUE 'P'.
           05  CFR-CLAIM-ID               PIC X(12).
           05  CFR-BODY                   PIC X(187).
           05  CFR-HEADER-BODY REDEFINES CFR-BODY.
               10  CFH-MEMBER-ID          PIC X(10).
               10  CFH-PROVIDER-ID        PIC X(10).
               10  CFH-CLAIM-DATE         PIC 9(8).
               10  CFH-POLICY-TYPE        PIC X(1).
                   88  CFH-PPO            VALUE 'P'.
                   88  CFH-HMO            VALUE 'H'.
                   88  CFH-INDEMNITY      VALUE 'I'.
               10  CFH-DIAG-COUNT         PIC 9(2).
      *> Fixed OCCURS 12 in file record — count field tracks
      *> how many are "active". ODO lives in working-storage
      *> (DIAGTBL.cpy) where GnuCOBOL allows it.
               10  CFH-DIAG-CODES.
                   15  CFH-DIAG-CODE      PIC X(7)
                       OCCURS 12.
               10  CFH-HEADER-FILLER      PIC X(72).
           05  CFR-SERVICE-BODY REDEFINES CFR-BODY.
               10  CFS-LINE-NUMBER        PIC 9(2).
               10  CFS-PROCEDURE-CODE     PIC X(5).
               10  CFS-DATE-FROM          PIC 9(8).
               10  CFS-DATE-TO            PIC 9(8).
               10  CFS-CHARGE-AMT         PIC S9(7)V99 COMP-3.
               10  CFS-UNITS              PIC 9(3).
               10  CFS-PLACE-OF-SVC       PIC X(2).
                   88  CFS-OFFICE          VALUE '11'.
                   88  CFS-OUTPATIENT      VALUE '22'.
                   88  CFS-INPATIENT       VALUE '21'.
                   88  CFS-EMERGENCY       VALUE '23'.
               10  CFS-MOD-COUNT          PIC 9(1).
      *> Fixed OCCURS 4 in file record — count field tracks
      *> active modifiers. Same pattern as diagnosis codes.
               10  CFS-MODIFIERS.
                   15  CFS-MODIFIER       PIC X(2)
                       OCCURS 4.
               10  CFS-SVC-FILLER         PIC X(145).
           05  CFR-ADJ-BODY REDEFINES CFR-BODY.
               10  CFA-LINE-NUMBER        PIC 9(2).
               10  CFA-ALLOWED-AMT        PIC S9(7)V99 COMP-3.
               10  CFA-COPAY-AMT          PIC S9(5)V99 COMP-3.
               10  CFA-DEDUCT-AMT         PIC S9(5)V99 COMP-3.
               10  CFA-COINS-AMT          PIC S9(5)V99 COMP-3.
               10  CFA-PAID-AMT           PIC S9(7)V99 COMP-3.
               10  CFA-STATUS             PIC X(2).
                   88  CFA-APPROVED        VALUE 'AP'.
                   88  CFA-DENIED          VALUE 'DN'.
                   88  CFA-PARTIAL         VALUE 'PR'.
               10  CFA-REASON-CODE        PIC X(3).
               10  CFA-ADJ-FILLER         PIC X(158).
           05  CFR-PAY-BODY REDEFINES CFR-BODY.
               10  CFP-PAY-METHOD         PIC X(1).
                   88  CFP-CHECK           VALUE 'C'.
                   88  CFP-EFT             VALUE 'E'.
               10  CFP-TOTAL-PAID         PIC S9(9)V99 COMP-3.
               10  CFP-CHECK-NUMBER       PIC 9(8).
               10  CFP-PAY-DATE           PIC 9(8).
               10  CFP-PAYEE-NAME         PIC X(30).
               10  CFP-PAY-FILLER         PIC X(134).
