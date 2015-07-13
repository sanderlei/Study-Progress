DEFINE VARIABLE i-item LIKE ITEM.item-num.
REPEAT:
   PROMPT-FOR i-item.
   FIND FIRST ITEM WHERE ITEM.item-num = INPUT i-item NO-ERROR.
   IF AVAILABLE ITEM THEN DO:
     DISPLAY item.item-num
        ITEM.item-name
        ITEM.price
        ITEM.on-hand.
     FOR EACH order-line WHERE order-line.item-num = item.item-num:
         FIND FIRST order WHERE order.order-num = order-line.order-num NO-ERROR.
         IF AVAILABLE order THEN DO:
             DISP order.order-num
                 order.order-date
                 order-line.qty.
         END.
     END.
   END.
END.


