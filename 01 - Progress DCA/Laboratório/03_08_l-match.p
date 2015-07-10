PROMPT-FOR customer.NAME.
FOR EACH customer WHERE customer.NAME MATCHES "*" + INPUT customer.NAME + "*":
    DISPLAY customer.cust-num customer.NAME customer.sales-rep.
END.
