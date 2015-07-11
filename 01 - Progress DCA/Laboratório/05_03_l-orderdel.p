DEFINE VARIABLE l-resp AS LOGICAL.
FOR EACH order:
    DISPLAY order WITH 1 COLUMN 1 DOWN.
    MESSAGE "Deseja Eliminar o Pedido?"
        UPDATE l-resp.
    IF l-resp THEN DO:
        FOR EACH order-line OF order:
            DELETE order-line.
        END.
        DELETE order.
        MESSAGE "Pedido Eliminado".
    END.
END.
