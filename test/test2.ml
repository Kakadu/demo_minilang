open Lang

module Q(L: Lang.Term) = struct
  let prog1 = [%getenv 
    (x := 5;
    if x>5 then y:=8 else z:=x+1 )
    |||
    (y_acq := x + 1
  
    )
  ]
  let prog2 = [%getenv
    (x := 5;
     y := 6)
    |||
    (z := 52;
     d := 36;)
  ]
  let prog3 a b = [%getenv
    (x := 5;
     ??a)
    |||
    (z := 52;
     ??b)
  ]
end
(* let prog1 = begin
 *   x := 5;
 *   let y = x + 1 in
 *   for j=1 to y do x:=x+5; done
 *            |||
 *   let y = x + 1 in
 *   repeat incr y until (x>100)
 *   end%getenv *)
  
