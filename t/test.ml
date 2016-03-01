let () = List.iter
           (fun t -> t ())
           [ Vect_test.execute
           ; Polynomial_test.execute
           ]
