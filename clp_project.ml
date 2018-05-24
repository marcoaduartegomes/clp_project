(* Projeto da Matéria - Conceitos de Linguagem de Programação(CLP) *)

(* Declaração das Listas *)
let listaLogico = ['!';'&';'@'];;
let listaAritmetico = ['+';'-';'*';'/';'%';'^'];;
let expressao = "-!-";;
let resposta_verifica = ref '0';;
let resposta_indice = ref 0;;

(* Função que transforma string em lista tendo cada um dos caracteres *)
let explode s =
	let rec exp i l =
		if i<0 then l else exp (i - 1) (s.[i] :: l) in
	exp (String.length s -1) []

(* Função que verifica se a string contem um termo de uma lista *)		
let verifica = fun s lista resposta ->
  let lista_string = explode s in
  let tamanho_lista_string = List.length lista_string in
  let tamanho_lista = List.length lista in	 
  let char = ref '0' in
  let existe = ref '0' in
	try
  for i = 0 to tamanho_lista_string - 1 do
	  for j = 0 to tamanho_lista - 1 do
	    char := List.nth lista_string i;
	    existe := List.nth lista j;
	    resposta := if char = existe
			then 's'
			else 'n';
			if !resposta = 's' then raise Exit;
	  done
  done;
	false
	with Exit -> true;;

(* Função que retorna o indice na lista da string do primeiro termo comum*)
(*  entre a string e a lista*)		
let pega_indice = fun s lista resposta ->
  let lista_string = explode s in
  let tamanho_lista_string = List.length lista_string in
  let tamanho_lista = List.length lista in	 
  let char = ref '0' in
  let existe = ref '0' in
	try
  for i = 0 to tamanho_lista_string - 1 do
	  for j = 0 to tamanho_lista - 1 do
	    char := List.nth lista_string i;
	    existe := List.nth lista j;
	    resposta := if char = existe
			then i
			else -1;
			if !resposta = i then raise Exit;
	  done
  done;
	false
	with Exit -> true;;

(* Função que separa a string em uma lista contendo os 'tokens' *)

let rec separa_exp = fun s lista ->
  let resposta = ref '0' in
	let teste = ref true in
	  teste := verifica s listaAritmetico resposta;
		if !resposta = 's' then
   	  let teste2 = ref true in
	    let resposta_indice = ref 0 in
      teste2 := pega_indice s listaAritmetico resposta_indice;
		  let char = ref '0' in
		  let lista_s = explode s in
		  let tamanho = List.length lista_s in
		  char := List.nth lista_s !resposta_indice;
		  lista := List.append [!char] !lista;  
      if (tamanho == 1) then 
			  lista else 
			  separa_exp (String.sub s (!resposta_indice+1) (tamanho - !resposta_indice - 1)) lista;
    else lista;;

(* ========================================================================  *)
(* MAIN *)

let lista = ref [];;
let resposta = separa_exp expressao lista;;
Printf.printf "%c" (List.nth !lista 1);;