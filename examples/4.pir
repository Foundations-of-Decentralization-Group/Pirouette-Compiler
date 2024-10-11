-- This is the syntax for a single line comment 
{-- This is the syntax for a multi line commnent --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
the line extends to the next line to show that this valid --}

{-- Binding for ints at different locations P1 and P2 --}
y: P2.int;
y := P2.4;
a : P2.int;
a := P2.4;
b : P1.int;
b := P1.4;
c : P1.int;
c := P1.5;


{-- Binding for strings at different locations P1 and P2 --}
z := P2.String;
z := P2."Hello";
r := P1.String;
r := P1."World";

{-- Sum of two numbers at the same location P2 --}
same_loc_result_sum : P2.int;
same_loc_result_sum := P2.(y + a);

{-- Product of two numbers calculated at a different location P2--}
result_product : P1.int;
result_product := P2.(y * a);

{-- Sum of two numbers at varying locations --}
diff_loc_sum : P2.int;
diff_loc_sum := P1.b + a;

{-- This is for sending the value 3 from Q to R and putting that into an x defined at R --}
R.x := [Q] Q.3 ~> R;

{-- This is for sending the bool true from Q to R and putting that into an x defined at R --}
R.x := [Q] Q.True ~> R;

{-- This is for sending the string "Hello" from Q to R and putting that into an x defined at R --}
R.x := [Q] Q."Hello" ~> R;


{-- This is for computing the sum of two numbers using the let in construct --}
_ := let R.x := R.5; in R.(x + x);

{-- This is for storing the result of the above function in y --} 
y : R.int;
y := let R.x := R.5; in R.(x + x);

{-- This is the same function as the above but put differently --}
R.y := let R.x := R.5; in R.(x + x);
y := let R.x := R.5; in R.(x + x);

{-- This is the declaration of a function that takes in an integer at a location C and returns that value back --}

{-- The variable y is to store the result of the computation --}
y : C.int;

Mirror : C.int -> C.int;
Mirror x := C.x;

C.y := Mirror C.2;

{-- This is to to change the value of a variable at a location P1 --}
-- This has to be checked
Change_Binding : P1.int -> P1.int;
Change_Binding X := P1.x = X;

{-- This is function application --}
-- This has to be checked as well
x : Test_Location.int;
y : Test_Location.int;

Func_Appl : Test_Location.int -> Test_Location.int;
Func_Appl X := Test_Location.x = X;

y.Test_Location := Func_Appl Test_Location.5;

{-- This is again declaring a function within a Location L3 without the types in the beginning --}

v : L3.int;

Factorial X := L3.x = X;
v := Factorial L3.7; 

{-- This is using the let in construct to make a function and then pass a value to it followed by function application --}

Add_Six : L4.int -> L4.int;
Add_Six value_one := let L4.x := value_one; in P1.(value_one + 6);

w := Add_Six L4.5;

{-- This function is written according to the rules in the parser.mly --}

Tester_Func input_value :=
	let L5.x := input_value; in
		if ()
		then ()
		else 
		();

{-- This is the program to calculate factorials --}

Factorial X := 
        let P8.x := X; in 
		if P8.(x = 0 || x = 1)
			then P8.1
		else 
			let P8.y := Factorial P8.(x - 1); in
	 		P8.(x * y);

q := Factorial P8.5;

{-- This is a makeshift loop --}

LooperOne X := LooperOne X;

b := LooperOne V1.5;

{-- This is a loop to keep binding values to x at location P --}

Looper2 := 
	let P_Location.x := P_Location.8; in
	Looper2;

w :=  Looper2;

{-- This is a loop to keep sending the value 3 from Q to R --}

Looper3 := 

        let R5.x := [Q] Q.3 ~> R5; in
	Looper3;

t :=  Looper3;

{-- This is a loop that sends a value based in a loop based on some conditions --}

Looper4 := 

        if R.(3>5) then
	let R.x := [Q] Q.3 ~> R; in
	Looper4
	else
	let R.x := [Q] Q.4 ~> R; in
	Looper4;

a := Looper4;
