open Lang

module Q(L: Lang.Term) = struct
  let prog1 = [%getenv 
    (x := 5;
    if x>5 then y:=8 else z:=x+1 )
    |||
    (y_acq := x + 1)
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
  let prog4 = [%getenv
     repeat (x:=1; y:=2) until (x=1)
  ]
end

