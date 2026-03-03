      *>================================================================*
      *> GENDATA.cob — Test data generator for batch chain pipeline
      *> Compile: cobc -x -free -I copybooks GENDATA.cob
      *> Creates: data/CUSTOMERS.DAT  (20 records, binary sequential)
      *>          data/TRANSACTIONS.DAT (50 records, line sequential)
      *>================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GENDATA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE
               ASSIGN TO 'data/CUSTOMERS.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CUST-STATUS.
           SELECT TRANSACTION-FILE
               ASSIGN TO 'data/TRANSACTIONS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TRANS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       COPY CUSTREC.

       FD  TRANSACTION-FILE.
       COPY TRANSREC.

       WORKING-STORAGE SECTION.
       01  WS-CUST-STATUS        PIC XX.
       01  WS-TRANS-STATUS        PIC XX.
       01  WS-CUST-COUNT          PIC 99     VALUE 0.
       01  WS-TRANS-COUNT         PIC 99     VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN OUTPUT CUSTOMER-FILE
           IF WS-CUST-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING CUSTOMERS: ' WS-CUST-STATUS
               STOP RUN
           END-IF

           PERFORM WRITE-CUSTOMERS

           CLOSE CUSTOMER-FILE
           DISPLAY 'CUSTOMERS WRITTEN: ' WS-CUST-COUNT

           OPEN OUTPUT TRANSACTION-FILE
           IF WS-TRANS-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING TRANSACTIONS: ' WS-TRANS-STATUS
               STOP RUN
           END-IF

           PERFORM WRITE-TRANSACTIONS

           CLOSE TRANSACTION-FILE
           DISPLAY 'TRANSACTIONS WRITTEN: ' WS-TRANS-COUNT

           STOP RUN.

      *>================================================================*
      *> CUSTOMERS — 20 records across BR1/BR2/BR3
      *>   12 active checking, 5 active savings, 2 closed, 1 frozen
      *>================================================================*
       WRITE-CUSTOMERS.
      *>--- BR1: 7 active checking, 2 active savings (9 total) ---------
           PERFORM INIT-CUST
           MOVE 'CUST0001' TO CUST-ID
           MOVE 'ALICE JOHNSON'        TO CUST-NAME
           MOVE 'BR1' TO CUST-BRANCH
           SET CUST-CHECKING TO TRUE
           SET CUST-ACTIVE TO TRUE
           MOVE 12500.00 TO CUST-BALANCE
           PERFORM FLUSH-CUST

           PERFORM INIT-CUST
           MOVE 'CUST0002' TO CUST-ID
           MOVE 'BOB MARTINEZ'         TO CUST-NAME
           MOVE 'BR1' TO CUST-BRANCH
           SET CUST-CHECKING TO TRUE
           SET CUST-ACTIVE TO TRUE
           MOVE 3200.50 TO CUST-BALANCE
           PERFORM FLUSH-CUST

           PERFORM INIT-CUST
           MOVE 'CUST0003' TO CUST-ID
           MOVE 'CAROL NGUYEN'         TO CUST-NAME
           MOVE 'BR1' TO CUST-BRANCH
           SET CUST-CHECKING TO TRUE
           SET CUST-ACTIVE TO TRUE
           MOVE 48750.00 TO CUST-BALANCE
           PERFORM FLUSH-CUST

           PERFORM INIT-CUST
           MOVE 'CUST0004' TO CUST-ID
           MOVE 'DAVID OKAFOR'         TO CUST-NAME
           MOVE 'BR1' TO CUST-BRANCH
           SET CUST-CHECKING TO TRUE
           SET CUST-ACTIVE TO TRUE
           MOVE 950.25 TO CUST-BALANCE
           PERFORM FLUSH-CUST

           PERFORM INIT-CUST
           MOVE 'CUST0005' TO CUST-ID
           MOVE 'EVE TANAKA'           TO CUST-NAME
           MOVE 'BR1' TO CUST-BRANCH
           SET CUST-SAVINGS TO TRUE
           SET CUST-ACTIVE TO TRUE
           MOVE 22000.00 TO CUST-BALANCE
           PERFORM FLUSH-CUST

           PERFORM INIT-CUST
           MOVE 'CUST0006' TO CUST-ID
           MOVE 'FRANK DUBOIS'         TO CUST-NAME
           MOVE 'BR1' TO CUST-BRANCH
           SET CUST-CHECKING TO TRUE
           SET CUST-ACTIVE TO TRUE
           MOVE 7800.00 TO CUST-BALANCE
           PERFORM FLUSH-CUST

           PERFORM INIT-CUST
           MOVE 'CUST0007' TO CUST-ID
           MOVE 'GRACE PATEL'          TO CUST-NAME
           MOVE 'BR1' TO CUST-BRANCH
           SET CUST-CHECKING TO TRUE
           SET CUST-ACTIVE TO TRUE
           MOVE 250.00 TO CUST-BALANCE
           PERFORM FLUSH-CUST

           PERFORM INIT-CUST
           MOVE 'CUST0008' TO CUST-ID
           MOVE 'HECTOR SILVA'         TO CUST-NAME
           MOVE 'BR1' TO CUST-BRANCH
           SET CUST-SAVINGS TO TRUE
           SET CUST-ACTIVE TO TRUE
           MOVE 15400.75 TO CUST-BALANCE
           PERFORM FLUSH-CUST

           PERFORM INIT-CUST
           MOVE 'CUST0009' TO CUST-ID
           MOVE 'IRENE KOWALSKI'       TO CUST-NAME
           MOVE 'BR1' TO CUST-BRANCH
           SET CUST-CHECKING TO TRUE
           SET CUST-ACTIVE TO TRUE
           MOVE 6100.00 TO CUST-BALANCE
           PERFORM FLUSH-CUST

      *>--- BR2: 3 active checking, 2 active savings, 1 closed (6) -----
           PERFORM INIT-CUST
           MOVE 'CUST0010' TO CUST-ID
           MOVE 'JAMES WRIGHT'         TO CUST-NAME
           MOVE 'BR2' TO CUST-BRANCH
           SET CUST-CHECKING TO TRUE
           SET CUST-ACTIVE TO TRUE
           MOVE 18300.00 TO CUST-BALANCE
           PERFORM FLUSH-CUST

           PERFORM INIT-CUST
           MOVE 'CUST0011' TO CUST-ID
           MOVE 'KAREN LEE'            TO CUST-NAME
           MOVE 'BR2' TO CUST-BRANCH
           SET CUST-CHECKING TO TRUE
           SET CUST-ACTIVE TO TRUE
           MOVE 500.00 TO CUST-BALANCE
           PERFORM FLUSH-CUST

           PERFORM INIT-CUST
           MOVE 'CUST0012' TO CUST-ID
           MOVE 'LEON FISCHER'         TO CUST-NAME
           MOVE 'BR2' TO CUST-BRANCH
           SET CUST-SAVINGS TO TRUE
           SET CUST-ACTIVE TO TRUE
           MOVE 50000.00 TO CUST-BALANCE
           PERFORM FLUSH-CUST

           PERFORM INIT-CUST
           MOVE 'CUST0013' TO CUST-ID
           MOVE 'MARIA SANTOS'         TO CUST-NAME
           MOVE 'BR2' TO CUST-BRANCH
           SET CUST-CHECKING TO TRUE
           SET CUST-ACTIVE TO TRUE
           MOVE 2750.00 TO CUST-BALANCE
           PERFORM FLUSH-CUST

           PERFORM INIT-CUST
           MOVE 'CUST0014' TO CUST-ID
           MOVE 'NINA PETROVA'         TO CUST-NAME
           MOVE 'BR2' TO CUST-BRANCH
           SET CUST-SAVINGS TO TRUE
           SET CUST-ACTIVE TO TRUE
           MOVE 31000.00 TO CUST-BALANCE
           PERFORM FLUSH-CUST

           PERFORM INIT-CUST
           MOVE 'CUST0015' TO CUST-ID
           MOVE 'OSCAR MENDEZ'         TO CUST-NAME
           MOVE 'BR2' TO CUST-BRANCH
           SET CUST-CHECKING TO TRUE
           SET CUST-CLOSED TO TRUE
           MOVE 0.00 TO CUST-BALANCE
           PERFORM FLUSH-CUST

      *>--- BR3: 2 active checking, 1 active savings, 1 closed, 1 frozen
           PERFORM INIT-CUST
           MOVE 'CUST0016' TO CUST-ID
           MOVE 'PAULA CHEN'           TO CUST-NAME
           MOVE 'BR3' TO CUST-BRANCH
           SET CUST-CHECKING TO TRUE
           SET CUST-ACTIVE TO TRUE
           MOVE 9200.00 TO CUST-BALANCE
           PERFORM FLUSH-CUST

           PERFORM INIT-CUST
           MOVE 'CUST0017' TO CUST-ID
           MOVE 'QUINN ADEYEMI'        TO CUST-NAME
           MOVE 'BR3' TO CUST-BRANCH
           SET CUST-SAVINGS TO TRUE
           SET CUST-ACTIVE TO TRUE
           MOVE 27500.00 TO CUST-BALANCE
           PERFORM FLUSH-CUST

           PERFORM INIT-CUST
           MOVE 'CUST0018' TO CUST-ID
           MOVE 'ROGER STEIN'          TO CUST-NAME
           MOVE 'BR3' TO CUST-BRANCH
           SET CUST-CHECKING TO TRUE
           SET CUST-ACTIVE TO TRUE
           MOVE 375.00 TO CUST-BALANCE
           PERFORM FLUSH-CUST

           PERFORM INIT-CUST
           MOVE 'CUST0019' TO CUST-ID
           MOVE 'SARAH VOLKOV'         TO CUST-NAME
           MOVE 'BR3' TO CUST-BRANCH
           SET CUST-CHECKING TO TRUE
           SET CUST-CLOSED TO TRUE
           MOVE 0.00 TO CUST-BALANCE
           PERFORM FLUSH-CUST

           PERFORM INIT-CUST
           MOVE 'CUST0020' TO CUST-ID
           MOVE 'TOMAS RIVERA'         TO CUST-NAME
           MOVE 'BR3' TO CUST-BRANCH
           SET CUST-CHECKING TO TRUE
           SET CUST-FROZEN TO TRUE
           MOVE 4100.00 TO CUST-BALANCE
           PERFORM FLUSH-CUST
           .

       INIT-CUST.
           INITIALIZE CUSTOMER-RECORD
           MOVE 20250115 TO CUST-OPEN-DATE
           MOVE 20260228 TO CUST-LAST-ACTIVITY
           MOVE SPACES   TO CUST-FILLER.

       FLUSH-CUST.
           WRITE CUSTOMER-RECORD
           ADD 1 TO WS-CUST-COUNT.

      *>================================================================*
      *> TRANSACTIONS — 50 records, deliberately unsorted
      *>   32 normal valid (D/W/I across active accounts)
      *>    3 overdraft triggers (W > balance on low-balance accounts)
      *>    5 non-existent accounts (CUST9901-CUST9905)
      *>    3 zero-amount
      *>    2 blank/space account IDs
      *>    2 invalid type codes (X, Z)
      *>    2 against closed accounts (CUST0015, CUST0019)
      *>    1 against frozen account (CUST0020)
      *>================================================================*
       WRITE-TRANSACTIONS.
      *> 01 - Deposit to CUST0003 (high-balance checking, BR1)
           PERFORM INIT-TRANS
           MOVE 'CUST0003' TO TRANS-ACCT-ID
           SET TRANS-DEPOSIT TO TRUE
           MOVE 1500.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'PAYROLL DEP'  TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 02 - Withdrawal from CUST0010 (BR2)
           PERFORM INIT-TRANS
           MOVE 'CUST0010' TO TRANS-ACCT-ID
           SET TRANS-WITHDRAW TO TRUE
           MOVE 200.00 TO TRANS-AMOUNT
           MOVE 'BR2' TO TRANS-BRANCH
           MOVE 'ATM WITHDRWL' TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 03 - Non-existent account CUST9901
           PERFORM INIT-TRANS
           MOVE 'CUST9901' TO TRANS-ACCT-ID
           SET TRANS-DEPOSIT TO TRUE
           MOVE 500.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'BAD ACCT 1'   TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 04 - Deposit to CUST0001 (BR1)
           PERFORM INIT-TRANS
           MOVE 'CUST0001' TO TRANS-ACCT-ID
           SET TRANS-DEPOSIT TO TRUE
           MOVE 3000.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'WIRE TRANSF'  TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 05 - Interest on CUST0012 savings (BR2)
           PERFORM INIT-TRANS
           MOVE 'CUST0012' TO TRANS-ACCT-ID
           SET TRANS-INTEREST TO TRUE
           MOVE 125.00 TO TRANS-AMOUNT
           MOVE 'BR2' TO TRANS-BRANCH
           MOVE 'INTEREST PMT' TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 06 - Blank account ID #1
           PERFORM INIT-TRANS
           MOVE SPACES TO TRANS-ACCT-ID
           SET TRANS-DEPOSIT TO TRUE
           MOVE 100.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'BLANK ID 1'   TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 07 - Withdrawal from CUST0016 (BR3)
           PERFORM INIT-TRANS
           MOVE 'CUST0016' TO TRANS-ACCT-ID
           SET TRANS-WITHDRAW TO TRUE
           MOVE 450.00 TO TRANS-AMOUNT
           MOVE 'BR3' TO TRANS-BRANCH
           MOVE 'CHECK 1042'   TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 08 - Deposit to CUST0006 (BR1)
           PERFORM INIT-TRANS
           MOVE 'CUST0006' TO TRANS-ACCT-ID
           SET TRANS-DEPOSIT TO TRUE
           MOVE 2200.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'DIRECT DEP'   TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 09 - OVERDRAFT: W $400 from CUST0007 (bal $250)
           PERFORM INIT-TRANS
           MOVE 'CUST0007' TO TRANS-ACCT-ID
           SET TRANS-WITHDRAW TO TRUE
           MOVE 400.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'RENT PAYMENT' TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 10 - Withdrawal from CUST0002 (BR1)
           PERFORM INIT-TRANS
           MOVE 'CUST0002' TO TRANS-ACCT-ID
           SET TRANS-WITHDRAW TO TRUE
           MOVE 150.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'DEBIT CARD'   TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 11 - Non-existent account CUST9902
           PERFORM INIT-TRANS
           MOVE 'CUST9902' TO TRANS-ACCT-ID
           SET TRANS-WITHDRAW TO TRUE
           MOVE 100.00 TO TRANS-AMOUNT
           MOVE 'BR2' TO TRANS-BRANCH
           MOVE 'BAD ACCT 2'   TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 12 - Deposit to CUST0017 savings (BR3)
           PERFORM INIT-TRANS
           MOVE 'CUST0017' TO TRANS-ACCT-ID
           SET TRANS-DEPOSIT TO TRUE
           MOVE 5000.00 TO TRANS-AMOUNT
           MOVE 'BR3' TO TRANS-BRANCH
           MOVE 'TRANSFER IN'  TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 13 - Withdrawal from CUST0013 (BR2)
           PERFORM INIT-TRANS
           MOVE 'CUST0013' TO TRANS-ACCT-ID
           SET TRANS-WITHDRAW TO TRUE
           MOVE 300.00 TO TRANS-AMOUNT
           MOVE 'BR2' TO TRANS-BRANCH
           MOVE 'POS PURCHASE' TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 14 - Zero-amount #1
           PERFORM INIT-TRANS
           MOVE 'CUST0001' TO TRANS-ACCT-ID
           SET TRANS-DEPOSIT TO TRUE
           MOVE 0.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'ZERO AMT 1'   TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 15 - Interest on CUST0005 savings (BR1)
           PERFORM INIT-TRANS
           MOVE 'CUST0005' TO TRANS-ACCT-ID
           SET TRANS-INTEREST TO TRUE
           MOVE 55.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'INTEREST PMT' TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 16 - Deposit to CUST0009 (BR1)
           PERFORM INIT-TRANS
           MOVE 'CUST0009' TO TRANS-ACCT-ID
           SET TRANS-DEPOSIT TO TRUE
           MOVE 800.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'MOBILE DEP'   TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 17 - Against closed account CUST0015
           PERFORM INIT-TRANS
           MOVE 'CUST0015' TO TRANS-ACCT-ID
           SET TRANS-DEPOSIT TO TRUE
           MOVE 300.00 TO TRANS-AMOUNT
           MOVE 'BR2' TO TRANS-BRANCH
           MOVE 'CLOSED ACCT'  TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 18 - Withdrawal from CUST0001 (BR1)
           PERFORM INIT-TRANS
           MOVE 'CUST0001' TO TRANS-ACCT-ID
           SET TRANS-WITHDRAW TO TRUE
           MOVE 2000.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'RENT PAYMENT' TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 19 - Deposit to CUST0014 savings (BR2)
           PERFORM INIT-TRANS
           MOVE 'CUST0014' TO TRANS-ACCT-ID
           SET TRANS-DEPOSIT TO TRUE
           MOVE 750.00 TO TRANS-AMOUNT
           MOVE 'BR2' TO TRANS-BRANCH
           MOVE 'SAVINGS XFER' TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 20 - Invalid type code 'X'
           PERFORM INIT-TRANS
           MOVE 'CUST0001' TO TRANS-ACCT-ID
           MOVE 'X' TO TRANS-TYPE
           MOVE 999.99 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'BAD TYPE X'   TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 21 - Withdrawal from CUST0003 (BR1)
           PERFORM INIT-TRANS
           MOVE 'CUST0003' TO TRANS-ACCT-ID
           SET TRANS-WITHDRAW TO TRUE
           MOVE 1000.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'BILL PAY'     TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 22 - Interest on CUST0008 savings (BR1)
           PERFORM INIT-TRANS
           MOVE 'CUST0008' TO TRANS-ACCT-ID
           SET TRANS-INTEREST TO TRUE
           MOVE 38.50 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'INTEREST PMT' TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 23 - OVERDRAFT: W $900 from CUST0011 (bal $500)
           PERFORM INIT-TRANS
           MOVE 'CUST0011' TO TRANS-ACCT-ID
           SET TRANS-WITHDRAW TO TRUE
           MOVE 900.00 TO TRANS-AMOUNT
           MOVE 'BR2' TO TRANS-BRANCH
           MOVE 'AUTO LOAN'    TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 24 - Deposit to CUST0018 (BR3, low balance)
           PERFORM INIT-TRANS
           MOVE 'CUST0018' TO TRANS-ACCT-ID
           SET TRANS-DEPOSIT TO TRUE
           MOVE 100.00 TO TRANS-AMOUNT
           MOVE 'BR3' TO TRANS-BRANCH
           MOVE 'CASH DEPOSIT' TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 25 - Non-existent account CUST9903
           PERFORM INIT-TRANS
           MOVE 'CUST9903' TO TRANS-ACCT-ID
           SET TRANS-DEPOSIT TO TRUE
           MOVE 250.00 TO TRANS-AMOUNT
           MOVE 'BR3' TO TRANS-BRANCH
           MOVE 'BAD ACCT 3'   TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 26 - Withdrawal from CUST0006 (BR1)
           PERFORM INIT-TRANS
           MOVE 'CUST0006' TO TRANS-ACCT-ID
           SET TRANS-WITHDRAW TO TRUE
           MOVE 500.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'AUTO PAY'     TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 27 - Deposit to CUST0002 (BR1)
           PERFORM INIT-TRANS
           MOVE 'CUST0002' TO TRANS-ACCT-ID
           SET TRANS-DEPOSIT TO TRUE
           MOVE 4500.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'PAYROLL DEP'  TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 28 - Against frozen account CUST0020
           PERFORM INIT-TRANS
           MOVE 'CUST0020' TO TRANS-ACCT-ID
           SET TRANS-WITHDRAW TO TRUE
           MOVE 500.00 TO TRANS-AMOUNT
           MOVE 'BR3' TO TRANS-BRANCH
           MOVE 'FROZEN ACCT'  TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 29 - Withdrawal from CUST0016 (BR3)
           PERFORM INIT-TRANS
           MOVE 'CUST0016' TO TRANS-ACCT-ID
           SET TRANS-WITHDRAW TO TRUE
           MOVE 1200.00 TO TRANS-AMOUNT
           MOVE 'BR3' TO TRANS-BRANCH
           MOVE 'MORTGAGE PMT' TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 30 - Interest on CUST0014 savings (BR2)
           PERFORM INIT-TRANS
           MOVE 'CUST0014' TO TRANS-ACCT-ID
           SET TRANS-INTEREST TO TRUE
           MOVE 77.50 TO TRANS-AMOUNT
           MOVE 'BR2' TO TRANS-BRANCH
           MOVE 'INTEREST PMT' TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 31 - Zero-amount #2
           PERFORM INIT-TRANS
           MOVE 'CUST0006' TO TRANS-ACCT-ID
           SET TRANS-WITHDRAW TO TRUE
           MOVE 0.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'ZERO AMT 2'   TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 32 - Deposit to CUST0011 (BR2)
           PERFORM INIT-TRANS
           MOVE 'CUST0011' TO TRANS-ACCT-ID
           SET TRANS-DEPOSIT TO TRUE
           MOVE 250.00 TO TRANS-AMOUNT
           MOVE 'BR2' TO TRANS-BRANCH
           MOVE 'CASH DEPOSIT' TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 33 - Non-existent account CUST9904
           PERFORM INIT-TRANS
           MOVE 'CUST9904' TO TRANS-ACCT-ID
           SET TRANS-INTEREST TO TRUE
           MOVE 15.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'BAD ACCT 4'   TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 34 - Withdrawal from CUST0009 (BR1)
           PERFORM INIT-TRANS
           MOVE 'CUST0009' TO TRANS-ACCT-ID
           SET TRANS-WITHDRAW TO TRUE
           MOVE 350.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'GROCERY'      TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 35 - Deposit to CUST0010 (BR2)
           PERFORM INIT-TRANS
           MOVE 'CUST0010' TO TRANS-ACCT-ID
           SET TRANS-DEPOSIT TO TRUE
           MOVE 6000.00 TO TRANS-AMOUNT
           MOVE 'BR2' TO TRANS-BRANCH
           MOVE 'WIRE TRANSF'  TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 36 - Invalid type code 'Z'
           PERFORM INIT-TRANS
           MOVE 'CUST0003' TO TRANS-ACCT-ID
           MOVE 'Z' TO TRANS-TYPE
           MOVE 500.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'BAD TYPE Z'   TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 37 - Interest on CUST0017 savings (BR3)
           PERFORM INIT-TRANS
           MOVE 'CUST0017' TO TRANS-ACCT-ID
           SET TRANS-INTEREST TO TRUE
           MOVE 68.75 TO TRANS-AMOUNT
           MOVE 'BR3' TO TRANS-BRANCH
           MOVE 'INTEREST PMT' TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 38 - Withdrawal from CUST0012 savings (BR2)
           PERFORM INIT-TRANS
           MOVE 'CUST0012' TO TRANS-ACCT-ID
           SET TRANS-WITHDRAW TO TRUE
           MOVE 2500.00 TO TRANS-AMOUNT
           MOVE 'BR2' TO TRANS-BRANCH
           MOVE 'TRANSFER OUT' TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 39 - Blank account ID #2
           PERFORM INIT-TRANS
           MOVE SPACES TO TRANS-ACCT-ID
           SET TRANS-WITHDRAW TO TRUE
           MOVE 50.00 TO TRANS-AMOUNT
           MOVE 'BR2' TO TRANS-BRANCH
           MOVE 'BLANK ID 2'   TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 40 - Deposit to CUST0004 (BR1)
           PERFORM INIT-TRANS
           MOVE 'CUST0004' TO TRANS-ACCT-ID
           SET TRANS-DEPOSIT TO TRUE
           MOVE 600.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'VENMO XFER'   TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 41 - OVERDRAFT: W $600 from CUST0018 (bal $375)
           PERFORM INIT-TRANS
           MOVE 'CUST0018' TO TRANS-ACCT-ID
           SET TRANS-WITHDRAW TO TRUE
           MOVE 600.00 TO TRANS-AMOUNT
           MOVE 'BR3' TO TRANS-BRANCH
           MOVE 'CAR REPAIR'   TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 42 - Deposit to CUST0013 (BR2)
           PERFORM INIT-TRANS
           MOVE 'CUST0013' TO TRANS-ACCT-ID
           SET TRANS-DEPOSIT TO TRUE
           MOVE 1800.00 TO TRANS-AMOUNT
           MOVE 'BR2' TO TRANS-BRANCH
           MOVE 'DIRECT DEP'   TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 43 - Withdrawal from CUST0005 savings (BR1)
           PERFORM INIT-TRANS
           MOVE 'CUST0005' TO TRANS-ACCT-ID
           SET TRANS-WITHDRAW TO TRUE
           MOVE 3000.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'TRANSFER OUT' TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 44 - Non-existent account CUST9905
           PERFORM INIT-TRANS
           MOVE 'CUST9905' TO TRANS-ACCT-ID
           SET TRANS-WITHDRAW TO TRUE
           MOVE 75.00 TO TRANS-AMOUNT
           MOVE 'BR2' TO TRANS-BRANCH
           MOVE 'BAD ACCT 5'   TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 45 - Deposit to CUST0016 (BR3)
           PERFORM INIT-TRANS
           MOVE 'CUST0016' TO TRANS-ACCT-ID
           SET TRANS-DEPOSIT TO TRUE
           MOVE 400.00 TO TRANS-AMOUNT
           MOVE 'BR3' TO TRANS-BRANCH
           MOVE 'REFUND'       TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 46 - Withdrawal from CUST0008 savings (BR1)
           PERFORM INIT-TRANS
           MOVE 'CUST0008' TO TRANS-ACCT-ID
           SET TRANS-WITHDRAW TO TRUE
           MOVE 700.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'EMERGENCY'    TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 47 - Against closed account CUST0019
           PERFORM INIT-TRANS
           MOVE 'CUST0019' TO TRANS-ACCT-ID
           SET TRANS-WITHDRAW TO TRUE
           MOVE 200.00 TO TRANS-AMOUNT
           MOVE 'BR3' TO TRANS-BRANCH
           MOVE 'CLOSED ACCT'  TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 48 - Zero-amount #3
           PERFORM INIT-TRANS
           MOVE 'CUST0010' TO TRANS-ACCT-ID
           SET TRANS-INTEREST TO TRUE
           MOVE 0.00 TO TRANS-AMOUNT
           MOVE 'BR2' TO TRANS-BRANCH
           MOVE 'ZERO AMT 3'   TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 49 - Interest on CUST0003 checking (BR1)
           PERFORM INIT-TRANS
           MOVE 'CUST0003' TO TRANS-ACCT-ID
           SET TRANS-INTEREST TO TRUE
           MOVE 121.88 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'INTEREST PMT' TO TRANS-DESC
           PERFORM FLUSH-TRANS

      *> 50 - Deposit to CUST0009 (BR1)
           PERFORM INIT-TRANS
           MOVE 'CUST0009' TO TRANS-ACCT-ID
           SET TRANS-DEPOSIT TO TRUE
           MOVE 1200.00 TO TRANS-AMOUNT
           MOVE 'BR1' TO TRANS-BRANCH
           MOVE 'FREELANCE'    TO TRANS-DESC
           PERFORM FLUSH-TRANS
           .

       INIT-TRANS.
           ADD 1 TO WS-TRANS-COUNT
           INITIALIZE TRANSACTION-RECORD
           MOVE 20260301 TO TRANS-DATE
           MOVE WS-TRANS-COUNT TO TRANS-SEQ.

       FLUSH-TRANS.
           WRITE TRANSACTION-RECORD.
