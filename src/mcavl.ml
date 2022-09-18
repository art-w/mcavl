module type Ordered = S.Ordered

module Set = Mcset.Make
module Map = Mcmap.Make

module type Ordered_poly = S.Ordered_poly

module Set_poly = Mcset.Make_poly
module Map_poly = Mcmap.Make_poly
