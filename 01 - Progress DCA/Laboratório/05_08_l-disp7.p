FOR EACH salesrep:
    DISP salesrep.rep-name.
    FOR EACH customer OF salesrep:
        DISP customer.cust-num customer.NAME.
        FOR EACH order OF customer,
            EACH order-line OF order,
            EACH ITEM OF order-line:
            DISPLAY order.order-date
                order-line.qty
                ITEM.item-name.
        END.
    END.
END.
