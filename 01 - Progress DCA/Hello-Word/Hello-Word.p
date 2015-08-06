&Scoped-define WINDOW-NAME C-Win
&Scoped-define FRAME-NAME DEFAULT-FRAME

&Scoped-Define ENABLED-OBJECTS RADIO-SET-1 
&Scoped-Define DISPLAYED-OBJECTS RADIO-SET-1 

DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

DEFINE FRAME DEFAULT-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16 WIDGET-ID 100.

CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Hello Word!"
         HEIGHT             = 16
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.

/* Include de definições de outros objetos fora a windows principal. */
{Hello-Word-01.i } .
ASSIGN FRAME FRAME-A:FRAME = FRAME DEFAULT-FRAME:HANDLE.

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.


ASSIGN XXTABVALXX = FRAME FRAME-A:MOVE-AFTER-TAB-ITEM (RADIO-SET-1:HANDLE IN FRAME DEFAULT-FRAME).

ON WINDOW-CLOSE OF C-Win
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  /* apenas para saber quando o evento ocorre */
  MESSAGE "WINDOW-CLOSE". 
  RETURN NO-APPLY.
END.

ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* **********************  Internal Procedures  *********************** */
PROCEDURE disable_UI :
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
  /* apenas para saber quando o evento ocorre */
  MESSAGE "disable_UI".
END PROCEDURE.


PROCEDURE enable_UI :
  DISPLAY RADIO-SET-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RADIO-SET-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.

  VIEW FRAME FRAME-A IN WINDOW C-Win.
  VIEW C-Win.
  /* apenas para saber quando o evento ocorre */
  MESSAGE "enable_U".
END PROCEDURE.

