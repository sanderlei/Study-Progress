REPEAT:
    PROMPT-FOR order.order-num WITH FRAM f-order.
    FIND FIRST order USING order-num NO-ERROR.
    IF AVAILABLE order THEN DO:
        HIDE FRAME f-order.
        DISP order.order-num
            order.cust-num.
        FOR EACH order-line OF order WITH FRAME f-order2:
            DISPLAY order-line.order-num
                order-line.line-num
                order-line.item-num
                order-line.qty.
        END.
    END.
END.
