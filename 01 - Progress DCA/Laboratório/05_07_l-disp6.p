DEFINE VARIABLE i-item LIKE ITEM.item-num.
REPEAT:
   PROMPT-FOR i-item.
   FIND FIRST ITEM WHERE ITEM.item-num = INPUT i-item NO-ERROR.
   IF AVAILABLE ITEM THEN DO:
     DISPLAY item.item-num
        ITEM.item-name
        ITEM.price
        ITEM.on-hand.
     FOR EACH order-line OF ITEM,
         EACH order OF order-line
         BY order-line.qty DESCENDING
         BY order-date:
             DISP order.order-num
                 order.order-date
                 order-line.qty.
     END.
   END.
END.


