DEFINE VARIABLE i-total AS integer LABEL "Ord. num".

FOR EACH customer:
    DISPLAY NAME WITH FRAME f-cliente DOWN OVERLAY.
   i-total = 0.

   FOR EACH order OF customer:
       i-total = i-total + 1.
       DISPLAY order.order-num
           order.order-date
           WITH FRAME f-ordem 5 DOWN COLUMN 43 OVERLAY.
       DISPLAY i-total WITH FRAME f-cliente OVERLAY.
   END.

   FIND FIRST salesrep OF customer NO-ERROR.
   IF AVAILABLE salesrep THEN DO:
       DISPLAY salesrep.sales-rep
           salesrep.region
           WITH FRAME f-rep COLUMN 43 SIDE-LABELS 1 COLUMN OVERLAY.
   END.
END.
