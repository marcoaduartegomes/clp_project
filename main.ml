(* UMA POSSÍVEL LÓGICA SERIA USAR A TABELA ASCII, POIS VOU CONCATENANDO ATÉ ENCONTRAR ALGO QUE NÃO SEJA UM DÍGITO OU CHAR*)
(* E POSSO ARMAZENA-LOS NUMA TUPLA CASO NÃO DÊ PARA TER UMA TULPLA DINÂMICA O QUE É BEM PROVÁVEL EU POSSO COLOCAR *)
(* (CHAR,POS_REAL), ONDE POS_REAL É A POSIÇÃO NA LISTA DE CARACTERES*)
open Separa_expressao;;
open Verifica_variavel;;

(* expressão para teste *)
let expressao = ref "camelo = 100";;

let listaLogico = ['!';'&';'@'];;
let listaAritmetico = ['+';'-';'*';'/';'%';'^'];;
let lista = ref [""];;

(*========================================INÍCIO DA DECLARAÇÃO DE FUNÇÕES===================================================================================*)

(* mostra a lista *)
let rec print_list = function 
[] -> ()
| e::l -> print_string e ; print_string " " ; print_list l;;

(*função para tirar todos espaços *)
let tira_espace exp =
	Str.global_replace (Str.regexp " \\|\t") "" exp;;

(*========================================FIM DA DECLARAÇÃO DE FUNÇÕES===================================================================================*)

		
expressao := tira_espace !expressao;;
let lista_char = explode !expressao;;

let palavra = ref "";;
let letra = ref ' ';;
(* !!!!ELE NÃO ESTÁ CONCATENANDO OS VALORES!!!! *)
for indice=0 to ((List.length lista_char)-1) do
	letra := List.nth lista_char indice; 
	if e_variavel_min(!letra) || e_variavel_max(!letra) || e_digito !letra then
		palavra := (!palavra)^(String.make 1 !letra);
		if indice = ((List.length lista_char)-1) then 
			begin
				if (e_variavel_min(!letra) = false) && (e_variavel_max(!letra)=false) && (e_digito !letra = false)  then begin
					lista := List.append [!palavra] !lista;
					palavra := ""	;
					lista := List.append [String.make 1 !letra] !lista;end
				else begin
					lista := List.append [!palavra] !lista;
					palavra := ""; 
				end
			end	
		else
			if (e_variavel_min(!letra) = false) && (e_variavel_max(!letra)=false) && (e_digito !letra = false)  then begin
				lista := List.append [!palavra] !lista;
				palavra := ""	;
				lista := List.append [String.make 1 !letra] !lista;end
			else begin
				lista := List.append [!palavra] !lista;
				palavra := ""; 
			end
done;;

print_list !lista;;