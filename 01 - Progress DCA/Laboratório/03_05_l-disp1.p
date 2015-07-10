FOR EACH customer
    WHERE customer.country BEGINS "F"
    AND customer.postal-code BEGINS "7":
    DISPLAY customer.cust-num
        customer.NAME
        customer.country
        customer.postal-code.
END.
