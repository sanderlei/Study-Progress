DEFINE VARIABLE cust-rep AS CHARACTER FORMAT "x(26)".
DISPLAY "Relatório de Pedidos"WITH CENTERED NO-BOX.

FOR EACH order:
    FIND FIRST customer OF order NO-ERROR.
    IF AVAILABLE customer THEN DO:
        cust-rep = customer.NAME + " - " + order.sales-rep. 
        DISPLAY order.order-num AT 10 column-label "Order!Number"
        order.order-date AT 21 COLUMN-LABEL "Order!Date"
        order.cust-num AT 34 COLUMN-LABEL "Customer!Number"
        cust-rep AT 50 COLUMN-LABEL "Customer name!Sales rep".
    END.
END.
