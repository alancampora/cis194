data Lista t = Vacio | Par t (Lista t)

listaEnteros:: Lista Int
listaEnteros = Par 3 ( Par 2 Vacio)

filtrarLista :: (t -> Bool) -> Lista t -> Lista t
filtrarLista _ Vacio = Vacio
filtrarLista fn (Par cabeza cola) 
    | fn cabeza = Par cabeza (filtrarLista fn cola) 
    | otherwise = filtrarLista fn cola

transformarLista :: (a -> b) -> Lista a -> Lista b
transofrmarLista _ Vacio = Vacio
transformarLista fn (Par cabeza cola) = Par (fn cabeza) (transformarLista fn cola)
