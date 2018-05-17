(* Projeto da Matéria - Conceitos de Linguagem de Programação(CLP) *)

let listaLogico = ['!';'&';'@'];;
let listaAritmetico = ['+';'-';'*';'/';'%';'^'];;
(*let () = List.iter (Printf.printf "%s") listaLogico;;*)

let expressao = "x+2";;
let explode s =
	let rec exp i l =
		if i<0 then l else exp (i - 1) (s.[i] :: l) in
	exp (String.length s -1) []
	
let lista = explode expressao;;
let tamanho_lista = List.length lista;;
let tamanho_logico = List.length listaLogico;;
let char = ref '0' in
let existe = ref '0' in
for i = 0 to tamanho_lista - 1 do
	for j = 0 to tamanho_logico - 1 do
	char := List.nth lista i;
	existe := List.nth listaLogico j;
	if char = existe then print_endline "batata";
	done;
done;	
  
