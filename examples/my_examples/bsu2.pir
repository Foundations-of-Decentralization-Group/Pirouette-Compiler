main := 
  let Seller.price_from_title := fun Seller.x -> (if Seller.(x="Awesome book") then Seller.5 else Seller.0); in
  let Seller.ddate_from_title := fun Seller.x -> (if Seller.(x="Awesome book") then Seller.1000 else Seller.0); in

  {- Change budget betwee, 4, 5, 6 to see different results -}
  let Buyer.budget := Buyer.6; in
  let Buyer.desired_book := Buyer."Awesome book"; in
  [Buyer] Buyer.desired_book ~> Seller.desired_book;

  let Seller.price := Seller.price_from_title Seller.desired_book; in

	[Seller] Seller.price ~> Buyer.price; 

	if Buyer.(price<=budget)
	then Buyer[L] ~> Seller;
    let _ := (if Buyer.(price = budget) then (let _ := Buyer.print_endline Buyer."I'm outta money now"; in ()) else ()); in

		let _ := Seller.print_endline Seller."I will sell you the book"; in
    [Seller] (Seller.ddate_from_title Seller.desired_book) ~> Buyer.ddate;

    Buyer.print_int Buyer.ddate

	else Buyer[R] ~> Seller;
		Seller.print_endline Seller."I won't sell you the book"
;
