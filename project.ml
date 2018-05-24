(*DECLARAÇÃO DAS FUNÇÕES*)

(* VERIFICA SE É LETRA MINÚSCULA *)
let e_variavel_min c = ((int_of_char(c) >= int_of_char('a')) && (int_of_char(c) <= int_of_char('z')));;

(* VERIFICA SE É LETRA MAIÚSCULA *)
let e_variavel_max c = ((int_of_char(c) >= int_of_char('A')) && (int_of_char(c) <= int_of_char('Z')));;

(* VERIFICA SE É DÍGITO *)
let e_digito c = let zero = int_of_char('0') in 
	int_of_char(c) - zero >= 0 && int_of_char(c) - zero <= 9;;

(*=================================================================================================================================*)			

(*variável para teste apenas*)		
let expr = "X1y564";;

(*A LÓGICA USADA FOI QUE SE A VARIÁVEL CONTROLE FOR IGUAL A STRING ENTÃO ELA PERCORREU TODA A STRING OBEDECENDO AS REGRAS, LOGO*)
(*ELA É UMA VARIÁVEL, CASO CONTRÁRIO NÃO *)

(* esse valor tem que ser SEMPRE true para a lógica funcionar *)
let retorno_const = true;; 

(*verifica se é uma variável ou não*)
let rec ehVariavel exp pos controle retorno = 
	(*POS INDICA A POSIÇÃO ATUAL, EXP É UMA STRING*)
	if (pos = (String.length exp)) then 
		if (controle = (String.length exp)) then 
			true
		else 
			false
	else 
		(*VERIFICA SE O PRIMEIRO CHAR É MINUSCULO*)
		if ((e_variavel_min (String.get exp pos)) && (pos = 0)) then
			retorno = ehVariavel exp (pos+1) (controle+1) retorno
	  else 
			if ((e_variavel_max (String.get exp pos) || e_variavel_min (String.get exp pos) || e_digito (String.get exp pos)) && (pos <> 0)) then
				retorno = ehVariavel exp (pos+1) (controle+1) retorno
			else
				retorno = ehVariavel exp (pos+1) (controle) retorno;;

(* converte o boolean para string *)
print_string( string_of_bool(ehVariavel expr 0 0 retorno_const) );;
				 	

(*======================================================== FIM =========================================================================*)			
