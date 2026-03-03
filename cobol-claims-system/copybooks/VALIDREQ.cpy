      *>================================================================*
      *> VALIDREQ.cpy — Validation request/response contract
      *> Shared between CLMPROC (caller) and CLMVALID (callee)
      *> via LINKAGE SECTION. The CALL/LINKAGE showcase.
      *>================================================================*
       01  VALID-REQUEST.
           05  VR-MEMBER-ID             PIC X(10).
           05  VR-PROVIDER-ID           PIC X(10).
           05  VR-PROCEDURE-CODE        PIC X(5).
           05  VR-DIAG-CODE             PIC X(7).
           05  VR-DATE-OF-SVC           PIC 9(8).
           05  VR-PLACE-OF-SVC          PIC X(2).
           05  VR-CHARGE-AMT            PIC S9(7)V99 COMP-3.
       01  VALID-RESPONSE.
           05  VR-IS-VALID             PIC X(1).
               88  VR-VALID             VALUE 'Y'.
               88  VR-INVALID           VALUE 'N'.
           05  VR-ERROR-CODE           PIC X(3).
           05  VR-ERROR-MSG            PIC X(50).
