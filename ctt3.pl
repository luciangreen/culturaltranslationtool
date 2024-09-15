% 19 5 24.pl x ctt3.pl

% ["Short Arguments","Appearances.txt",0,algorithms,"3. I prepared to help the appearances, increase to academia, recommend meditation and improved logical thinking.  I did this by stating that the student constructed a logic of the appearance.  First, I observed the plebeian write the number of As on different topics.  Second, I observed her either have children or something else.  Third, I read research about graduating from the Master degree."]

%tag("<&t>","</&t>","<&t,'","'>","</&t,'","'>").

/*
p2lp for pl
find strings in files
insert tags (auto around strings) x
approve tags x

split strings by \n, \t, (tags x)
delete white space strings
save white space formatting at start and end of strings
approve grammar x

trans
- delete missing strings from orig in data file
- only unknown strings
approve orig, trans, btt file by changing orig, orig2, trans
save orig2, trans data
sub back into copy of orig, (with tags x)
render orig wo tags x

* Use only with files without e.g. filenames as strings in Prolog, so use it with individual files that are data Prolog or text files.

* gitl in ctt/lppm etc.
*/

:-include('../listprologinterpreter/listprolog.pl').
:-include('../gitl/find_files.pl').
%:-include('../Prolog-to-List-Prolog/p2lpconverter.pl').
:-include('../List-Prolog-to-Prolog-Converter/lp2pconverter.pl').
:-include('../Philosophy/sub_term_with_address.pl').
:-include('../Text-to-Breasonings/chatgpt_qa.pl').

ctt3(Source,Dest,_Orig_lang,Dest_lang) :-
(exists_file_s(Source)->
(open_string_file_s(Source,File1),
Files=[[Source,File1]],
Dest_type=file
); 
(find_files(Source,Files),

(not(exists_directory_s(Dest))->(writeln([Dest,"is not a directory."]),abort);true),

Dest_type=folder
)),
findall(X,(member([X1,X2],Files),
(string_concat(_,".pl",X1)->
(%trace,
p2lpconverter([string,X2],LP),
sub_term_types_wa([string],LP,Instances1),
findall([Address,Z9],(member([Address,Y1],Instances1),%trace,
find_formatting(Y1,Z9)
),Z7),
X=[stwa,X1,LP,Z7]);
(%trace,
find_formatting(X2,Z9),
X=[string,X1,Z9]))),X3),

findall(A9,(member(A10,X3),
(A10=[stwa,X11,LP,X12]->
(findall([Address,A171],(member(A12,X12),
A12=[Address,A13],
findall(A14,(member(A15,A13),
(is_white_space(A15)->A14=A15;
(foldr(string_concat,["What is ",A15," in the language ",Dest_lang,"?"],S),
(catch(q(S,A16),_,false)->A16=A14;
(writeln("Translation failed."),abort))))),A17),
foldr(string_concat,A17,A171)
),A18),
A9=[stwa,X11,LP,%X12,
A18]);

(A10=[string,X11,X12]->
findall(A14,(member(A15,X12),
(is_white_space(A15)->A14=A15;
(foldr(string_concat,["What is ",A15," in the language ",Dest_lang,"?"],S),
(catch(q(S,A16),_,false)->A16=A14;
(writeln("Translation failed."),abort))))),A17),
foldr(string_concat,A17,A171),
A9=[string,X11,A171]))),A19),

findall(A20,(member(A21,A19),
(A21=[string,X1,Z9]->A20=[X1,Z9];
(A21=[stwa,X11,LP,%X12,
A18]->
(%trace,
foldr(put_sub_term_wa_ae,A18,LP,X13),
lp2p1(X13,X14),
A20=[X11,X14])))),X21),
%trace,
findall(_,(member([K2,File2],X21),
(Dest_type=file->K3=Dest;
(

(string_concat(S5,"/",Dest)->true;S5=Dest),
split_string1(K2,"/",S2),
append([S3],S4,S2),
%foldr(string_concat,[S5,"/",S4],S6)
flatten([S3,"/../",S5,S4],S6),
foldr(string_concat,S6,K3))),
open_s(K3,write,S),
write(S,File2),close(S)
),_),!.

%q1(A,A).

is_white_space(C) :-
	string_chars(C,D),forall(member(E,D),char_type(E,space)).
	
find_spaces_before(A,A51,A8) :-
	string_chars(A,A1),
	findall(A2,(member(A3,A1),atom_string(A3,A2)),A4),
	append(A5,A6,A4),
	append([A7],_A8,A6),
	not(is_space(A7)),
	foldr(string_concat,A6,A8),
	foldr(string_concat,A5,A51),!.
find_spaces_before(A,A,"") :-
	string_chars(A,A1),
	findall(A2,(member(A3,A1),atom_string(A3,A2)),A4),
	forall(member(A5,A4),is_space(A5)),!.
find_spaces_before(A,"",A) :- !.

find_spaces_after(A,A52,A8) :-
	string_chars(A,A1),
	findall(A2,(member(A3,A1),atom_string(A3,A2)),A4),
	reverse(A4,A41),
	append(A5,A6,A41),
	append([A7],_A8,A6),
	not(is_space(A7)),
	reverse(A6,A61),
	foldr(string_concat,A61,A8),
	reverse(A5,A51),
	foldr(string_concat,A51,A52),!.
find_spaces_after(A,A,"") :-
	string_chars(A,A1),
	findall(A2,(member(A3,A1),atom_string(A3,A2)),A4),
	forall(member(A5,A4),is_space(A5)),!.
find_spaces_after(A,"",A) :- !.

find_spaces_before_and_after(A,Before,B,After) :-
	find_spaces_before(A,Before,C),
	find_spaces_after(C,After,B),!.


find_formatting(Y1,Z9) :-
 split_on_substring1(Y1,"\n\t",Y2),
findall(Z8,(member(Z1,Y2),
find_spaces_before_and_after(Z1,Z2,Z3,Z4),
delete([Z2,Z3,Z4],"",Z8)),Z5),
flatten(Z5,Z9).
