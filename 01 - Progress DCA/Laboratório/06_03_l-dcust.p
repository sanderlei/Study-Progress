FORM
    customer.cust-num NAME COLON 20
    customer.sales-rep AT 55
    address COLON 20
    address2 NO-LABEL COLON 20 SKIP
    city COLON 20 customer.state COLON 39
    postal-code COLON 20 customer.country COLON 39 SKIP(1)
    credit-limit COLON 20
    balance COLON 39 SKIP
    customer.terms COLON 20 SKIP
    customer.discount COLON 20
    comments
    WITH FRAME cust-frame TITLE "customer" SIDE-LABELS.

FOR EACH customer:
    DISPLAY customer EXCEPT contact phone WITH FRAME cust-frame.
    DOWN WITH FRAME cust-frame.
END.
