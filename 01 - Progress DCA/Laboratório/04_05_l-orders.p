DEFINE VARIABLE i-ano-ini AS INTEGER FORMAT "9999" LABEL "Ano Inicial" NO-UNDO.
DEFINE VARIABLE i-ano-fim AS INTEGER FORMAT "9999" LABEL "Ano Final" NO-UNDO.
REPEAT :
   UPDATE i-ano-ini 
       i-ano-fim.
   FOR EACH order 
       WHERE order.order-date > DATE(1,1,i-ano-ini)
       AND order.order-date <= DATE(12,31,i-ano-fim):
       DISP order.order-num order.cust-num order.order-date.
   END.
END.
