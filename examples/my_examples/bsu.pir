main := 
	let Seller.price := Seller.5; in

	let Buyer.price := 
	[Seller] Seller.price ~> Buyer; 
	in

	let Buyer.budget := Buyer.5; in

	if Buyer.(price<=budget)
	then Buyer[L] ~> Seller;
		Seller.print_endline Seller."I will sell you the book"
	else Buyer[R] ~> Seller;
		Seller.print_endline Seller."I won't sell you the book";
