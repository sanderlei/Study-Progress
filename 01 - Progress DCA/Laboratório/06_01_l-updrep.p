DEFINE VARIABLE l-resp AS LOGICAL INITIAL NO LABEL "Deseja alterar ou incluir outro representante? ".
REPEAT :
     PROMPT-FOR salesrep.sales-rep WITH FRAME frame1.
     FIND FIRST salesrep WHERE salesrep.sales-rep = INPUT salesrep.sales-rep NO-ERROR.
     IF NOT AVAILABLE salesrep THEN DO:
         CREATE salesrep.
         ASSIGN salesrep.sales-rep.
         MESSAGE "Novo Representante Criado.".
     END.
     UPDATE salesrep EXCEPT salesrep.sales-rep WITH FRAME frame2 2 COLUMN SIDE-LABELS.

     UPDATE l-resp WITH FRAME frame3 NO-ERROR.
     IF l-resp = NO THEN DO:
         LEAVE.
     END.
END.
