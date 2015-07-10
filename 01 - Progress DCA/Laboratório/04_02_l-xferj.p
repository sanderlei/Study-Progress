DEF VAR rep-old LIKE salesrep.sales-rep.
DEF VAR rep-new LIKE salesrep.sales-rep.
PROMPT-FOR rep-old LABEL "Representante velho: ".
ASSIGN rep-old.
PROMPT-FOR rep-new LABEL "Representante novo: ".
ASSIGN rep-new.
FOR EACH customer WHERE customer.sales-rep = rep-old:
    ASSIGN customer.sales-rep = rep-new.
    DISP customer.sales-rep customer.cust-num customer.NAME.
END.


