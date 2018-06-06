(* DECLARAÇÃO DAS FUNÇÕES *)

(* verifica se o parenteses é aberto *)
let eh_parenteses_aberto c = 
	if c = '(' then
		true
	else
		false;;


(* verifica se o parenteses é fechado *)
let eh_parenteses_fechado c = 
	if c = ')' then
		true
	else
		false;;


(* verifica se tem parenteses *)
let rec tem_parenteses exp pos num_p boolean =
	if pos = String.length exp then
		if num_p = 0 then
			boolean = false
		else
			boolean = true
	else
		if ((eh_parenteses_aberto (String.get exp pos)) || (eh_parenteses_fechado (String.get exp pos))) then
			boolean = tem_parenteses exp (pos+1) (num_p+1) boolean
		else 
			boolean = tem_parenteses exp (pos+1) num_p boolean;;

(* verifica se os parenteses estão na ordem correta *)				
let rec parenteses_ordem exp pos num_p=
	(* parenteses não pode começar com ) *)
  if pos < String.length exp then
		print_int(0);
  	if String.get exp pos = ')' && pos = 0 then
  		false
  	else
    	if pos = String.length exp then
    		(* então os parenteses estão em ordem correta*)
    		if num_p = 0 then 
    			true
    		else
    			(* então os parenteses não estão na ordem correta*)
    			false
    	else
    		if eh_parenteses_aberto (String.get exp pos) then
    			parenteses_ordem exp (pos+1) (num_p+1)
    		else
    			if eh_parenteses_fechado (String.get exp pos) then
    				parenteses_ordem exp (pos+1) (num_p-1)
    			else	
    				parenteses_ordem exp (pos+1) num_p;;
(*
let parenteses_join exp pos num_p boolean = 
	if tem_parenteses exp pos num_p boolean then
		if parenteses_ordem exp pos num_p then
			true
		else
			false							*)																											
(*=================================================================================================================================*)			
				
let exp = "(2+3)*4";;

(*tem_parenteses exp 0 0 true;;*)

print_string(string_of_bool(parenteses_ordem exp 0 0));;

