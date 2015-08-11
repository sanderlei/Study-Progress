DEFINE FRAME cust-frame
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
    WITH  TITLE "customer" SIDE-LABELS.

FOR EACH customer:
    DISPLAY customer.cust-num customer.sales-rep WITH FRAME cust-frame.
    UPDATE customer.address
        customer.NAME
        customer.city
        customer.state
        customer.country
        customer.postal-code
        customer.balance
        customer.credit-limit
        customer.discount
        customer.terms
        customer.comments
        WITH FRAME cust-frame.
END.
