module Loc = struct
  type ti = string
  let loc: string -> ti = fun x -> x
end
module Op = struct
    type ti = ADD | MUL | EQ | NEQ | LT | LE | GT | GE
  end
module MemOrder = struct
    type ti = SC | ACQ | REL | ACQ_REL | CON | RLX | NA
end
module type Term = sig
    type ti

    val const   : int -> ti
    val var     : string -> ti
    val binop   : Op.ti -> ti -> ti -> ti
    val asgn    : ti -> ti -> ti
    val pair    : ti -> ti -> ti
    val if'     : ti -> ti -> ti -> ti
    val while'  : ti -> ti -> ti
    val repeat  : ti -> ti
    val read    : MemOrder.ti -> Loc.ti -> ti
    val write   : MemOrder.ti -> Loc.ti -> ti -> ti
    val cas     : MemOrder.ti -> MemOrder.ti -> Loc.ti -> ti -> ti -> ti
    val seq     : ti -> ti -> ti
    val spw     : ti -> ti -> ti
    val par     : ti -> ti -> ti
    val skip    : unit -> ti
    val stuck   : unit -> ti
end
