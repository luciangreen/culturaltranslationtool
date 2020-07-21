/** Cultural Translation Tool

The Cultural Translation Tool saves previous translations and texts to back-translate, saving time.

*** Important: Please quit Grammarly (or the grammar editor) before running and after each use.
*** If using vi instead of Grammarly, please grammar check new sentences in Grammarly before entering.
- assumed cultural customisations before use of alg
- texttobr[1,2] after back-translation or (lote texttobr[1,2] when) translation is complete

files to operate on different folder, different tmp folder

simulate repeat by automatically entering entered text when not in correct format

include common ctt preds

texttobr/2 this alg, plan before monday

\' in strings fr can't, aren't - convert to and from - when debugged


[ctt].
[edit].

New Features
- 5 11 18 Takes previous backtranslations in any language pair that match original and tries to translate them in the given language pair.

**/

use_module(library(pio)).
use_module(library(dcg/basics)).

%%:- include(library(edit)).
%%:- use_module(library(edit)).
%%:- multifile edit:edit_command/2.
%%:- multifile prolog_edit:load/0.

%% run with prolog_edit:ctt.

%%:- include('edit.pl').

ctt :-
	cttInput('files/ctt-input.txt',CttInput1,FromLang,ToLang,Tokens2,_Tokens3,Tokens32),
	File1='files/ctt-orig1-orig2.txt',
	readfile(File1,CttOrig1Orig21,COOOtherLangPairs,COOWithoutLangPairs,FromLang,ToLang,"files/ctt-orig1-orig2.txt file read error.","Number of back-translation pairs in lang1->lang2: "),
	File2='files/ctt-orig-tran.txt',
	readfile(File2,CttOrigTran1,COTOtherLangPairs,_,FromLang,ToLang,"files/ctt-orig-tran.txt file read error.","Number of translation pairs in lang1->lang2: "),
	
	%% ctt-orig1-orig2.txt, ctt-orig-tran.txt->google

	calcbtremaining(CttInput1,CttOrig1Orig21,CttOrigTran1,FromLang,ToLang), %% redo calc based on google x can't bec relies on on-fly processing
		%%ctt(Tokens2,[],Translation,CttOrig1Orig21,_Translation1,CttOrig1Orig212,CttOrigTran1,CttOrigTran2,FromLang,ToLang),
	ctt2(Tokens2,[],Translation,CttOrig1Orig21,CttOrig1Orig212,CttOrigTran1,CttOrigTran2,FromLang,ToLang,COOWithoutLangPairs),
	updatetrans(Translation,Tokens32,"",Translation2),
	removerepeatedterm(CttOrig1Orig212,[],CttOrig1Orig2123),
	addfromtolang(FromLang,ToLang,CttOrig1Orig2123,[],CttOrig1Orig2122),
	addfromtolang(FromLang,ToLang,CttOrigTran2,[],CttOrigTran21),
	wrap2(COOOtherLangPairs,[],COO1),
	wrap2(COTOtherLangPairs,[],COT1),
	append(CttOrig1Orig2122,COO1,COO3),
	append(CttOrigTran21,COT1,COT),
	updatefile1(COO3,File1),
	updatefile1(COT,File2),
	updatefile2(Translation2,'files/ctt-output.txt'),!.

removerepeatedterm([],List,List) :- !.
removerepeatedterm(List1,List2,List3) :-
	List1=[[Item,Item]|Rest],
	append(List2,[[Item,""]],List4),
	removerepeatedterm(Rest,List4,List3),!.
removerepeatedterm(List1,List2,List3) :-
	List1=[Item|Rest],
	append(List2,[Item],List4),
	removerepeatedterm(Rest,List4,List3),!.

addfromtolang(_FromLang,_ToLang,[],WithFromToLang,WithFromToLang) :- !.
addfromtolang(FromLang,ToLang,WithoutFromToLang,WithFromToLang1,WithFromToLang2) :-
	WithoutFromToLang=[[A,B]|Rest],
	WithFromToLang3=[[FromLang],[ToLang],[[A],[B]]],
	append(WithFromToLang1,[WithFromToLang3],WithFromToLang4),
	addfromtolang(FromLang,ToLang,Rest,WithFromToLang4,WithFromToLang2), !.


/**
	updatetrans(Tokens3,Translation1,Translation2),
	updatefile(CttOrig1Orig212,File1).
	updatefile(CttOrigTran2,File2),
	updatefile(Translation2,'files/ctt-output.txt').
**/

updatefile1(List2,File) :-
	sort(List2,List3),
	updatefile3(List3,File).
updatefile2(List2,File) :-
	updatefile3(List2,File).
	
updatefile3(List2,File) :-
	open(File,write, Stream),
%%	string_codes(List2),
    write(Stream,List2),
    close(Stream),!.
    
updatetrans(_,[],Translation,Translation) :- !.
updatetrans(Translation,Tokens1,Translation2,Translation3) :-
	Tokens1=[[_,r,N]|Tokens2],
	repeat("\n",N,String),
	atom_concat(Translation2,String,Translation4),
	updatetrans(Translation,Tokens2,Translation4,Translation3),!.
updatetrans(Translation,Tokens1,Translation2,Translation3) :-
	Tokens1=[[_,s,N]|Tokens2],
	repeat(" ",N,String),
	atom_concat(Translation2,String,Translation4),
	updatetrans(Translation,Tokens2,Translation4,Translation3),!.
updatetrans(Translation,Tokens1,Translation2,Translation3) :-
	Tokens1=[N|Tokens2],
	member([N,Token],Translation), 
	delete(Translation,[N,Token],Translationa),
	atom_concat(Translation2,Token,Translation4),
	updatetrans(Translationa,Tokens2,Translation4,Translation3),!.

/**atom_concat2([],List,List) :- !.
atom_concat2(List1,List2,List3) :-
	List1=[List4|List5],
	atom_concat(List2,List4,List6),
	atom_concat2(List5,List6,List3),!.**/

repeat(Str,1,Str).
repeat(Str,Num,Res):-
    Num1 is Num-1,
    repeat(Str,Num1,Res1),
    string_concat(Str, Res1, Res).
	
string(String) --> list(String).

list([]) --> [].
list([L|Ls]) --> [L], list(Ls).
	
cttInput(CttInput1,CttInput2,FromLang,ToLang,Tokens2,Tokens3,Tokens32) :-
	%%SepandPad=".?!", %% split by \n and record number of \ns between paras, then split into sentences adding back punctuation, trim white space before sentence recording number of \ss before - record sentence #
	phrase_from_file(string(CttInput4), CttInput1),
	(phrase(entry1(CttInput3),CttInput4)->true;(writeln("files/ctt-input.txt file read error."),abort)),
	CttInput3=[FromLang,ToLang,Tokens1],
	removers(Tokens1,1,[],Tokens2,[],Tokens3,[],Tokens31,[],Tokens32),
	length(Tokens2,Length0),write("Number of sentences to translate in files/ctt-input.txt: "), writeln(Length0),
	sort(Tokens31,CttInput2),
	length(CttInput2,Length1),write("Number of unique sentences to translate in files/ctt-input.txt: "), writeln(Length1).

%% [en,fr]
%% Num of sentences to modify, etc.

removers([],_,Tokens1,Tokens1,Tokens2,Tokens2,Tokens3,Tokens3,Tokens4,Tokens4) :- !.
removers(Tokens1,N1,Tokens2,Tokens3,Tokens7,Tokens8,Tokens10,Tokens11,Tokens13,Tokens14) :-
	Tokens1=[AtomToken4|Tokens5], %% Token4 to AtomToken4
	not(AtomToken4=[r,_]),not(AtomToken4=[s,_]), %% "
	%%[Token41]=Token4,
	%%atom_string(AtomToken4,Token41),
	append(Tokens2,[[N1,AtomToken4]],Tokens6),
	append(Tokens13,[N1],Tokens15),
	append(Tokens10,[AtomToken4],Tokens12),
	append(Tokens7,[N1],Tokens9),
	N2 is N1+1,
	removers(Tokens5,N2,Tokens6,Tokens3,Tokens9,Tokens8,Tokens12,Tokens11,Tokens15,Tokens14),!.
removers(Tokens1,N1,Tokens2,Tokens3,Tokens6,Tokens7,Tokens10,Tokens11,Tokens13,Tokens14) :-
	Tokens1=[Token4|Tokens5],
	((Token4=[r,N3],T=r);(Token4=[s,N3],T=s)),
	append(Tokens13,[[N1,T, N3]],Tokens15),
	%%atom_string(AtomToken4,Token4),
	%%append(Tokens6,[[AtomToken4]],Tokens8),
	N2 is N1+1,
	removers(Tokens5,N2,Tokens2,Tokens3,Tokens6,Tokens7,Tokens10,Tokens11,Tokens15,Tokens14),!.

readfile(List1,List2,List7,List8,FromLang,ToLang,Error,Notification) :-
	phrase_from_file(string(List6), List1),
	(phrase(file0(List3),List6)->true;(writeln(Error),abort)),
	select(List3,FromLang,ToLang,[],List5,[],List7,[],List8),
	sort(List5,List2),
	length(List2,Length),write(Notification), writeln(Length) %%
	.

select([],_FromLang,_ToLang,List1,List1,List2,List2,List3,List3) :- !.
select(List,FromLang,ToLang,List1,List2,List5,List6,List7,List8) :-
	List=[Item1|List3],
	((Item1=[FromLang,ToLang,Item2],
	append(List1,[Item2],List3));
	(Item1=[FromLang,ToLang,Item2,Item3],
	append(List1,[[Item2,Item3]],List4))),
	select(List3,FromLang,ToLang,List4,List2,List5,List6,List7,List8),!.
select(List,FromLang,ToLang,List1,List2,List4,List5,List7,List8) :-
	List=[Item1|List3],
	append(List4,[Item1],List6),
	Item1=[_,_,Item2,Item3],
	append(List7,[[Item2,Item3]],List9),
	select(List3,FromLang,ToLang,List1,List2,List6,List5,List9,List8),!.

calcbtremaining(CttInput1,CttOrig1Orig211,CttOrigTran1,FromLang,ToLang) :-
	towords(FromLang,ToLang,CttOrig1Orig211,[],CttOrig1Orig212,[],_ObjectNames,[],AllUsedNames),
	tranfrom(CttOrigTran1,[],TranFrom),
	subtract(CttInput1,CttOrig1Orig212,D1), %%
	length(D1,Length),Difference is abs(Length),write("Number of back-translations remaining to define: "), writeln(Difference),
	
	%%towords2(CttInput1,[],CttInput2),
	subtract(AllUsedNames,TranFrom,D2), %% Should AUN be appended to TF, " x
	%%delete(D21,'',D2),
	length(D2,Length01t),Differencet is abs(Length01t),write("Number of undefined back-translations: "), writeln(Differencet),
	%%writeln([undefined,D2]), %% Print undefined 

	%%delete(D31,'',D3),
	subtract(TranFrom,AllUsedNames,D3),
	length(D3,Length01t2),Differencet2 is abs(Length01t2),write("Number of orphaned translations: "), writeln(Differencet2),!.
	%%writeln([orphaned,D3]). %% Print orphaned 


towords(_,_,[],A,A,C,C,D,D) :- !.
towords(FromLang,ToLang,J,A,B,D,E,G,H) :-
	J=[[Word1,Word2]|Rest],
	Atom1=Word1,
	Atom2=Word2,
	(Atom2=''->append(G,[Atom1],I);
	append(G,[Atom2],I)),
	append(A,[Atom1],C),
	append(D,[Atom2],F),
	towords(FromLang,ToLang,Rest,C,B,F,E,I,H).
towords(FromLang,ToLang,J,A,B,D,E,G,H) :-
	not(J=[[[FromLang,ToLang],[_,_]]|Rest]),
	towords(FromLang,ToLang,Rest,A,B,D,E,G,H).

tranfrom([],TranFrom,TranFrom) :- !.
tranfrom(CttOrigTran1,TranFrom1,TranFrom2) :-
	CttOrigTran1 = [[TranFrom3,_Tran]|TranFrom4],
	append(TranFrom1,[TranFrom3],TranFrom5),
	tranfrom(TranFrom4,TranFrom5,TranFrom2), !.

/**%%% already to atom in removers
towords2([],A,A) :- !.
towords2(BrDict03,A,B) :-
	BrDict03=[[N,Word]|Rest],
	atom_string(Atom,Word),
	append(A,[[N,Word]],C),
	towords2(Rest,C,B).
**/

%% ctt-orig1-orig2, ctt-orig-tran

%% string_codes("[[[[a],[a]],[[a],[a]]],[[[a],[a]],[[a],[a]]],[[[a],[a]],[[a],[a]]]]",Y),phrase(file0(X),Y).

file0(N) --> "[", file(N), "]", !.
file0([]) --> [].

%%file([]) --> [].
file([L|Ls]) --> entry(L),",",
%%{writeln(L)}, %%***
file(Ls), !. 
file([L]) --> entry(L), 
%%{writeln(L)},
!.

%%quote(_X)-->"'".

/** string_codes(                                                                   
"[[[a],[a]],[[a],[a]]]"                                                         
,Y),phrase(entry(X),Y). **/

entry([Word1,Word2,Word3,Word4]) -->
		"[","[",   sentence1(Word11), "]", {string_codes(Word1,Word11),string(Word1)},
		",",
           "[",   sentence1(Word22),"]",  {string_codes(Word2,Word22),string(Word2)},
           ",",
		"[","[",  sentence1(Word33), "]", {string_codes(Word3,Word33),string(Word3)},
		",",
            "[",sentence1(Word44), "]","]", {string_codes(Word4,Word44),string(Word4)},
           "]",!.

sentence1([X|Xs]) --> [X], {((char_type(X,alnum);char_type(X,white));char_type(X,punct)), not(X=93)
%% ]
, not(X=91)
%% [
}, sentence1(Xs), !.
sentence1([]) --> [].

%% string_codes("[[a],[a],[a.a.a. Hello, how are you?\n]]",Y),phrase(entry1(X),Y).
%% X = [a,a,[[a.],[a.],[a.],[s,1],[Hello, how are you?],[r,1]]]
 
entry1([Word1,Word2,Word3]) -->
		"[","[", sentence1(Word11), "]", {string_codes(Word1,Word11),string(Word1)},
		",",
           "[", sentence1(Word22), "]", {string_codes(Word2,Word22),string(Word2)},
           ",",
		"[",  paragraph(Word3), %% {string_codes(Word3,Word33),string(Word3)},
		"]","]", !.

%%paragraph([]) --> [].

%% "a.a."
paragraph(AAs) --> spaces(Y), {%%atom_string(Y,YS),
string_length(Y,YLen), (YLen>0->Start=[[s,YLen]];Start=[])}, sentence32(X), returns(Z), {%%atom_string(X,XS),
[XS]=X,
string_length(XS,XLen), atom_string(XS,X1), (XLen>0->append(Start,[X1],Next);Next=Start), %%atom_string(Z,ZS),
string_length(Z,ZLen), (ZLen>0->append(Next,[[r,ZLen]],Last);Last=Next)},paragraph(As),{ append(Last,As,AAs)}, !.
%% "a."
paragraph(Last) --> spaces(Y), {%%atom_string(Y,YS),
string_length(Y,YLen), (YLen>0->Start=[[s,YLen]];Start=[])}, sentence32(X), returns(Z), {%%atom_string(X,XS),
[XS]=X,
string_length(XS,XLen), atom_string(XS,X1), (XLen>0->append(Start,[X1],Next);Next=Start), %%atom_string(Z,ZS),
string_length(Z,ZLen), (ZLen>0->append(Next,[[r,ZLen]],Last);Last=Next)}, !.

%% "a\na\n"
paragraph(AAs) --> spaces(Y), {%%atom_string(Y,YS),
string_length(Y,YLen), (YLen>0->Start=[[s,YLen]];Start=[])}, sentence33(X), returns(Z),{%%atom_string(X,XS),
[XS]=X,
string_length(XS,XLen), atom_string(XS,X1), (XLen>0->append(Start,[X1],Next);Next=Start), %%atom_string(Z,ZS),
string_length(Z,ZLen), ZLen>=1,(ZLen>0->append(Next,[[r,ZLen]],Last);Last=Next)},paragraph(As),{append(Last,As,AAs)}, !.
%% "a\n"
paragraph(Last) --> spaces(Y), {%%atom_string(Y,YS),
string_length(Y,YLen), (YLen>0->Start=[[s,YLen]];Start=[])}, sentence33(X), returns(Z), {%%atom_string(X,XS),
[XS]=X,
string_length(XS,XLen), atom_string(XS,X1), (XLen>0->append(Start,[X1],Next);Next=Start), %%atom_string(Z,ZS),
string_length(Z,ZLen), ZLen>=1,(ZLen>0->append(Next,[[r,ZLen]],Last);Last=Next)}, !.

%% "a"
paragraph(Next) --> spaces(Y), {%%atom_string(Y,YS),
string_length(Y,YLen), (YLen>0->Start=[[s,YLen]];Start=[])}, sentence33(X), {%%atom_string(X,XS),
[XS]=X,
string_length(XS,XLen), atom_string(XS,X1), (XLen>0->append(Start,[X1],Next);Next=Start)}, !.


spaces(XXs) --> [X], {X=32}, spaces(Xs), {char_code(Ch,X),atom_string(CA,Ch),atom_concat(CA,Xs,XXs)}, !. %% Space
spaces('') --> [].

sentence32([XsZ]) --> sentence33(Xs), sentenceendpunctuation(Z), {atom_string(CA,Xs),atom_concat(CA,Z,XsZ)}, !.
sentence32('') --> [].

%%sentence321(CA) --> sentence33(Xs), {atom_string(CA,Xs)}, !.
%%sentence321('') --> [].

returns(XXs) --> [X], {X=10}, returns(Xs), {char_code(Ch,X),atom_string(CA,Ch),atom_concat(CA,Xs,XXs)}, !. %% Newline
returns('') --> [].


%% sentence33([C|Xs])
sentence33(CXs) --> [X], {((char_type(X,alnum);char_type(X,white));char_type(X,punct)), not(X=93),char_code(C,X)
%% ]
, not(X=91)

%% .
, not(X=46)
%% !
, not(X=33)
%% ?
, not(X=63)
%% \n
, not(X=10)

}, sentence33(Xs), {atom_string(CA,C),atom_concat(CA,Xs,CXs)}, !.
sentence33('') --> [].

sentenceendpunctuation(Z) --> [Z1], {char_code(Z,Z1),(Z='.';(Z='?';(Z='!')))}, !.


%% [['<<<Note:>>>','How do you do?'],[[[BT:'How are you?',bt]...],['How are you - entered?'...]]]
%% [[<<<Note: Please enter only "en" sentences with the meaning of the first sentence to find a correct back-translation by appending item(s) to the last list: * in [[],[[],[*]]].  Open and save using the menu as path /Users/luciangreen/Dropbox/Program Finder/possibly not working/translationmanagementsystem/ctt/tmp/ctt-orig1-orig2-tmp-hello.txt>>>,hello],[],[[hello,]]]

file1(N) --> "[", "[", "[", sentence1(_A1), "]",  ",", "[", sentence1(_A2),  "]", "]", ",", "[", file3(_A3),  "]", %%%
",", "[",file3(N), "]", "]", optional_end(_A4), !.
%%file1([]) --> [].

optional_end(_A4) --> [Return],[EOF],{Return=13,EOF=10}, !.
optional_end(_A4) --> [], !.

%%file2([Entries]) --> "[", file4(Entries), "]", !.
%%%%file2([]) --> [].

%%file([]) --> [].
file3([L|Ls]) --> entry2(L),",",
%%{writeln(L)}, %%
file3(Ls), !. 
file3([L]) --> entry2(L), 
%%{writeln(L)},
!.

/**%%file([]) --> [].
file4([L|Ls]) --> entry3(L),",",
%%{writeln(L)}, %%
file4(Ls), !. 
file4([L]) --> entry3(L), 
%%{writeln(L)},
!.**/


entry2([Word1,Word2]) -->
		"[","[",  sentence1(Word11), "]", {string_codes(Word1,Word11),string(Word1)},
		",", "[", sentence1(Word22), "]", {string_codes(Word2,Word22),string(Word2)}, "]",!.



%%entry3([Word1]) -->
%%	 	"[", sentence1(Word11), "]", {string_codes(Word1,Word11),string(Word1)},
%%		!.

bash_command(Command, Output) :-
        setup_call_cleanup(process_create(path(bash),
                ['-c', Command],
                [stdout(pipe(Out))]),
        read_string(Out, _, Output),
        close(Out)).
        

swap_quote_to_space(A,B) :-
	string_codes(A,C),findall(D,(member(C1,C),swap1(C1,D)),E),string_codes(B,E),!.
swap1(A,B) :- string_codes("'",[A]),string_codes(" ",[B]),!.
swap1(A,A) :- !.

translate(Input,FromLang,ToLang,Output3) :-
	%%swap_quote_to_space(Input,Input1),
	insertdoublebackslashbeforequote(Input,Input1),
	concat_list(["../../../trans ",FromLang,":",ToLang," \"",Input1,"\""],F),
	%%atom_concat("export GOOGLE_APPLICATION_CREDENTIALS=\"/Users/luciangreen/Dropbox/Program Finder/possibly not working/translationmanagementsystem/Cultural Translation Tool-19XXXXXXb4.json\"\ncurl -s -X POST -H \"Content-Type: application/json\" -H \"Authorization: Bearer \"$(/Users/luciangreen/Dropbox/Program\\ Finder/possibly\\ not\\ working/translationmanagementsystem/google-cloud-sdk/bin/gcloud auth application-default print-access-token)     --data \"{
/**
  'q': '",Input,A),
	atom_concat(A,"',
  'source': '",B),
	atom_concat(B,FromLang,C),
	atom_concat(C,"',
  'target': '",D),
	atom_concat(D,ToLang,E),
	atom_concat(E,"',
  'format': 'text'
}\" \"https://translation.googleapis.com/language/translate/v2\"",F),
**/
repeat,

catch(
	(bash_command(F,Output1)),
   _,
	(writeln("Translate failed.  Press c to retry."),
	read_string(user_input, "\n", "\r", _,C),
	C="c"->fail;abort)
),

	split_string(Output1,"\033","\033",Output2),
	Output2=[_,Output3a|_], %% *** May be 3rd item on Linux
	%%atom_concat("{\n  \"data\": {\n    \"translations\": [\n      {\n        \"translatedText\": \"",A1,Output1),atom_concat(Output2,"\"\n      }\n    ]\n  }\n}\n",A1),	
	atom_string(Output3a,Output3b),
		string_concat("[1m",Output3,Output3b),
		%%string_concat(Output3,"\033\[22m",Output3c)
!.

/**

translate(Input1,FromLang,ToLang,Output3) :-
	insertdoublebackslashbeforequote(Input1,Input),
	atom_concat("export GOOGLE_APPLICATION_CREDENTIALS=\"/Users/luciangreen/Dropbox/Program Finder/possibly not working/translationmanagementsystem/Cultural Translation Tool-19XXXXXXb4.json\"\ncurl -s -X POST -H \"Content-Type: application/json\" -H \"Authorization: Bearer \"$(/Users/luciangreen/Dropbox/Program\\ Finder/possibly\\ not\\ working/translationmanagementsystem/google-cloud-sdk/bin/gcloud auth application-default print-access-token)     --data \"{
  'q': '",Input,A),
	atom_concat(A,"',
  'source': '",B),
	atom_concat(B,FromLang,C),
	atom_concat(C,"',
  'target': '",D),
	atom_concat(D,ToLang,E),
	atom_concat(E,"',
  'format': 'text'
}\" \"https://translation.googleapis.com/language/translate/v2\"",F),
	bash_command(F,Output1),
	atom_concat("{\n  \"data\": {\n    \"translations\": [\n      {\n        \"translatedText\": \"",A1,Output1),atom_concat(Output2,"\"\n      }\n    ]\n  }\n}\n",A1),	
	atom_string(Output2,Output3).
	
	**/
insertdoublebackslashbeforequote(Input1,Input) :-
	string_codes(Input1,Input2),
	insertdoublebackslashbeforequote1(Input2,[],Input3),
	string_codes(Input,Input3).
insertdoublebackslashbeforequote1([],Input,Input) :- !.
insertdoublebackslashbeforequote1(Input1,Input2,Input3) :-
	Input1=[Input4|Input5],
	Input4=39, %% quote
	append(Input2,[92,39],Input6),
	insertdoublebackslashbeforequote1(Input5,Input6,Input3), !.
insertdoublebackslashbeforequote1(Input1,Input2,Input3) :-
	Input1=[Input4|Input5],
	not(Input4=39), %% quote
	append(Input2,[Input4],Input6),
	insertdoublebackslashbeforequote1(Input5,Input6,Input3), !.

	
ctt2([],T,T,B,B,C,C,_,_,_) :-
	!.
ctt2([[N,Word]|Words],Translation1,Translation2,List,List2,List4,List5,FromLang,ToLang,COOWithoutLangPairs) :-

	tryoutputs0([[Word,_]],COOWithoutLangPairs,[],COO2),

%%writeln([tryoutputs0([[word,_]],cOOWithoutLangPairs,[],cOO2),tryoutputs0([[Word,_]],COOWithoutLangPairs,[],COO2)]),
	sort(COO2,COO4),

	prolog_edit:open_grammar_editor(Word,FromLang,ToLang,List,List4,String4,N,COO4),

	/**
	ctt-orig1-orig2.txt
[[[en,fr],['How do you do?','How are you?']]...] 2nd item is correct back-translation with cult cust
o1 o2***
ctt-orig-tran.txt
[[[en,fr],['How are you?','Comment allez-vous?']]...]
o2 l***
	**/
	
	(String4=[BT,Translation4]->true;String4=[[BT,Translation4]]), %% s4=[o2 l]
	append(Translation1,[[N,Translation4]],Translation3), %% String4=[o2 l]2
	%% *********** don't add if ''
	String5=[Word,BT],
	tryoutputs1(String5,List,List621), %% want [o1 o2]1
	%%List62=[[FromLang],[ToLang],List621],
	String6=[BT,Translation4],
	tryoutputs11(String6,List4,List611), %% want [o2 l]1
	%%List61=[[FromLang],[ToLang],List611],
	
	ctt2(Words,Translation3,Translation2,List621,List2,List611,List5,FromLang,ToLang,COOWithoutLangPairs),!.
	%%).

%% finds unknown words, asks for their br in form "n of m: word", verify, (can go back x) append and sort, save

fulladjective("en",'English') :- !.
fulladjective("fr",'French') :- !.
fulladjective("de",'German') :- !.
fulladjective("es",'Spanish') :- !.
fulladjective("ru",'Russian') :- !.
fulladjective(A,A) :- !.

writenotification1(A,E,F,Notification):-
	%%working_directory(CWD, CWD),
	atom_concat(A,E,E1),
	atom_concat(E1,F,Notification).

writenotification2(A,FromLang1,F,Notification):-
	working_directory(CWD, CWD),
	fulladjective(FromLang1,FromLang2),
	atom_concat(A,FromLang2,B),
	%%atom_concat(B,CWD,E),
	atom_concat(B,F,Notification).
	
%% Attempts to back-translate List1 given [original,previous back-translation] pairs 
%% List2, [previous back-translation,previous translation] pairs List2, the Orig to 
%% translate, the PastTries, FromLang, ToLang, file path E, shell command H, Notification
%% with editing instructions and giving the translation Output.

/**
ctt-orig1-orig2-tmp-1.txt
[['<>','I love you.'],[['','']],[['I dote on you.', '']]]

backtranslateuntilcorrect([['I love you.','I dote on you.']],[['I dote on you.','Je me passionne pour toi.']],'I love you.',[],'en','fr','tmp/ctt-orig1-orig2-tmp-1.txt','/Applications/Grammarly.app/Contents/MacOS/Grammarly tmp/ctt-orig1-orig2-tmp-1.txt --line','<>',O).







**/
backtranslateuntilcorrect(List1,List2,Orig,PastTries,FromLang,ToLang,E,H,Notification1,Notification2,Output) :- 
%%trace,
	(
		phrase_from_file(string(List6),E),
		(phrase(file1(Outputs11),List6),
		removenotice(Outputs11,Outputs1a),
		sort(Outputs1a,Outputs1)
		%%not(Outputs1=Orig)
%%writeln([outputs1,Outputs1])
		%%not(Outputs1=[])
		)->
		(
			( %% remove extra br ***
				(%% Use a correct translation from ctt-orig-tran.txt
				tryoutputs0(Outputs1,List2,[],Output),not(Output=[]))->
					true
					;
					
					(%% Use a correct translation from a back-translation from ctt-orig1-orig2.txt
						(tryoutputs(Outputs1,List1,[],Output1),tryoutputs0(Output1,List2,[],Output),not(Output=[]))->
						true;
						(
							(trytranslations1(Outputs1,FromLang,ToLang,
									false,Flag1,[],Outputs3,_,Output2)),
							(
								Flag1=true->
									Output=Output2
									;
									(trytranslations2(Orig,Outputs3,Output3,Flag2),
										(
											Flag2=true->
												Output=Output3
												;
												(openeditor(Notification1,Notification2,Orig,PastTries,Outputs1,Tries,E,H),
													backtranslateuntilcorrect(List1,List2,Orig,Tries,
													FromLang,ToLang,E,H,Notification1,Notification2,Output)
												)
										)
									)
							)
						)
					)
			)
		)
		;
		(
			openeditorwitherror(H),
			Tries=PastTries,
			backtranslateuntilcorrect(List1,List2,Orig,Tries,FromLang,
				ToLang,E,H,Notification1,Notification2,Output)
		)
	).

		%% Reads file E giving Outputs1, the list of sentences to try back-translating
		%% Outputs1 = [[o1,pbt1]]

				%% Finds whether the sentences to try back-translating are in the list
	 			%% [previous back-translation,previous translation]
	 			%% ?- tryoutputs0 [[o1,pbt1],[o2,pbt2]],[[pbt2,pt2]],[],I .
				%% I = [pbt2, pt2].

					%% Finds whether the sentences to try back-translating are in the list
	 				%% [original,previous back-translation]
	 				/**
					?- tryoutputs [[o1,pbt1],[o2,pbt2]],[[o2,pbt2]],I .
					I = [o2, pbt2].	
					
					?- tryoutputs2 [o2, pbt2],[[pbt1,pt1],[pbt2,pt2],[pbt2,pt2]],I .
					I = [pbt2, pt2].
					**/

						%% Finds the back-translations of the sentences to try 
						%% back-translating from one language to another, and returns 
						%% Flag=true if there is a successful back-translation Output2
						%% See ***1
/**
*** 1
?- trytranslations1([['I love you.-1','I love you.'],['I love you1-1','I love you1']],'en','fr',false,Flag,[],O,_,O2).
Flag = true,
O = [['I love you.', '', 'I love you tr.'], ['I love you1.', '', 'I love you1 tr.']],
O2 = ['I love you1.', 'I love you1 tr.'].

?- trytranslations1([['I love you.-1','I love you3.']],'en','fr',false,Flag,[],O,_,O2).
Flag = false,
O = [['I love you3.', 'I love you3 fake.', 'I love you3 tr.']].
**/

								%% Asks if a back-translation is correct
								%% trytranslations2 'Original',  'I love you3.', 'I love you3 fake.', 'I love you3 tr.' ,[a,b,c]],O . 

removenotice(Outputs11,Outputs1) :-
	%%writeln([outputs11,Outputs11]),
	Contents=[_,"Insert an alternative that might translate better here."],
	/**((((Contents=[_,"Insert an alternative that might translate better here."];
	Contents=[_,"Insert a correct translation here",_]);
	Contents=[_,"Insert a correct translation here"])	;
	Contents=_Empty),
	member(Contents,Outputs11),**/
	delete(Outputs11,Contents,Outputs1), !.
%%* delete these one by one

openeditor(Notification1,Notification2,Orig,PastTries,Outputs12,Tries,E,H) :-
	writeln(Notification1),

	%%removenotice(Outputs11,Outputs12),
	
	wrap(Outputs12,[],Outputs1),
	%%writeln([pastTries,PastTries]), %%%
	append(PastTries,Outputs1,Tries), %% Output, Outputs1 x *** REMOVED [] from O1
	%%(phrase_from_file(string(FileContents1),E)->true;writeln("Error opening tmp file.")), 

	append([[["<<<History of tries:>>>"],[""]]],Tries,Tries2),
%%writeln([append([[["<<<History of tries:>>>"],[""]]],tries,tries2),append([[["<<<History of tries:>>>"],[""]]],Tries,Tries2)]),
	FileContents1=[[[Notification1],[Orig]],Tries2,[[[Notification2],["Insert an alternative that might translate better here."]]]],

	updatefile2(FileContents1,E),
	writeln("Opening editor."),
	(bash_command(H,_)->
		true;
		(writeln("Failed opening Editor."),abort)
	). %% open editor

openeditorwitherror(H) :-
	write("*** Your previous entry was not in the correct format.  "),
	writeln("Please correct this. ***"),
	writeln("Opening editor."),
	(
		(
			bash_command(H,_)->
				true
				;
				(
					writeln("Failed opening Editor."),abort
				)
		)
	).

wrap([],List,List) :-!.
wrap(List1,List2,List3) :-
	List1=[[Item1,Item2]|List4],
	append(List2,[[[Item1],[Item2]]],List5),
	wrap(List4,List5,List3), !.
	
wrap2([],List,List) :-!.
wrap2(List1,List2,List3) :-
	List1=[[Item1,Item2,Item3,Item4]|List4],
	append(List2,[[[Item1],[Item2],[[Item3],[Item4]]]],List5),
	wrap2(List4,List5,List3), !.
	
tryoutputs0([],_List3,Output,Output) :- !.
tryoutputs0([[Original,BT]|Outputs],List2,List3,List4) :-
	(
		(
			(BT=""->
				(member([Original,Translation],List2),
				append(List3,[[Original,Translation]],List5))
				->
				true
				;
				(	member([BT,Translation],List2))->
					append(List3,[[BT,Translation]],List5)
				)
		)
		;
		List5=List3
	),
	tryoutputs0(Outputs,List3,List5,List4)	
	,!.
%%tryoutputs0([_Output|Outputs],List,Answer) :-
%%	tryoutputs0(Outputs,List,Answer),!.

tryoutputs([],_List3,Output,Output) :- !.
tryoutputs([Item|Outputs],List2,List3,List4) :-
	tryoutputsa(Item,List2,List3,List5),
	tryoutputs(Outputs,List2,List5,List4).

tryoutputsa(_,[],List,List) :- !.
tryoutputsa([Original,BT],[List21|List22],List3,List4) :-
	[Original,Translation]=List21,
	(BT=""->
			append(List3,[[Original,Translation]],List5)
			;
			append(List3,[[Original,BT]],List5))
			,
	tryoutputsa([Original,BT],List22,List5,List4)
	,!.
tryoutputsa(Item,[_|List22],List3,List4) :-
	tryoutputsa(Item,List22,List3,List4), !.


/**
tryoutputs([Item1],List2,Item2) :-
	Item1=[Original,BT1],
	((BT1="",member([Original,BT2],List2),Item2=[Original,BT2]);
	(member(Item1,List2),Item2=Item1))
	%%,Item1=[Item2,_Output]
	,!.
tryoutputs([],_List3,_Output) :- fail.
tryoutputs([_Output|Outputs],List,Answer) :-
	tryoutputs(Outputs,List,Answer),!.
**/

/**tryoutputs2(Item1,List2,Item2) :-
	Item1=[Original,PreviousBT1],
	(PreviousBT1=""->PreviousBT2=Original;PreviousBT2=PreviousBT1),
	member(Item2,List2),
	Item2=[PreviousBT2,_Translation],
	!.
**/
/** ?- tryoutputs1([original,bt],[[original,bt],[original1,bt1]],L).
L = [[original, bt], [original1, bt1]]. **/
tryoutputs1(Output1,Output2,Output2) :-
	member(Output1,Output2),!.
tryoutputs1(Output1,Output2,Output3) :-
	not(member(Output1,Output2)),
	(Output2=""->
		Output3=Output2;
		append(Output2,[Output1],Output3)),
	!.

/** ?- tryoutputs11([bt,t],[[bt1,t1]],L).
L = [[bt1, t1], [bt, t]]. **/
tryoutputs11(Output1,Output2,Output2) :-
	member(Output1,Output2),!.
tryoutputs11(Output1,Output2,Output3) :-
	not(member(Output1,Output2)),append(Output2,[Output1],Output3),!.

trytranslations1([],_FromLang,_ToLang,Flag,Flag,List,List,String,String) :- !.
/**trytranslations1([[_,String]],FromLang,ToLang,Flag1,Flag2,List1,List3,String1,String2) :-
	translate1(String,FromLang,ToLang,Output2),
	String4=[String,Output2],
	translate1(Output2,ToLang,FromLang,Stringa),
	(String=Stringa->
		(Flag2=true,String2=String4,append(List1,[[String,"",Output2]],List3));
		(Flag1=Flag2,String2=String1,append(List1,[[String,Stringa,Output2]],List3))),
	!.**/
trytranslations1([[Original,PreviousBT1]|Outputs],FromLang,ToLang,Flag1,Flag2,List1,List2,String1,String2) :-
	(PreviousBT1=""->String=Original;String=PreviousBT1),
	translate(String,FromLang,ToLang,Output2),
	String4=[String,Output2],
	translate(Output2,ToLang,FromLang,Stringa),
	(String=Stringa->
		(Flag3=true,String3=String4,append(List1,[[String,"",Output2]],List3));
		(Flag3=Flag1,String3=String1),append(List1,[[String,Stringa,Output2]],List3)),
	trytranslations1(Outputs,FromLang,ToLang,Flag3,Flag2,List3,List2,String3,String2),!.
trytranslations1([_|Outputs],FromLang,ToLang,Flag1,Flag2,List1,List2,String1,String2) :-
	trytranslations1(Outputs,FromLang,ToLang,Flag1,Flag2,List1,List2,String1,String2),!.

translate1("I love you.","en","fr","I love you tr.").
translate1("I love you1.","en","fr","I love you1 tr.").
translate1("I love you2.","en","fr","I love you2 tr.").
translate1("I love you tr.","fr","en","I love you.").
translate1("I love you1 tr.","fr","en","I love you1.").
translate1("I love you1 tr.","fr","en","I love you1.").
translate1("I dote on you1.","en","fr","I dote on you1 tr.").
translate1("I dote on you1 tr.","fr","en","I dote on you1 different.").

translate1("Je t|aime.","en","fr","I love you.").
translate1("I love you.","fr","en","Je t|aime.").
translate1("I dote on you.","en","fr","Je me passionne pour toi.").
translate1("Je me passionne pour toi.","fr","en","I dote on you.").

translate1("I love you3.","en","fr","I love you3 tr.").
translate1("I love you3 tr.","fr","en","I love you3 fake.").

trytranslations2(Orig,Outputs1,Output,Flag2) :-
	%%removenotice(Outputs11,Outputs1),
	writeln(""),
	writeln("Which of the following sentences:\n- are grammatical\n- have the same meaning as the other in the pair, and\n- have the same meaning as the original sentence?\n"),
	write('Original:'),write("\t"),writeln(Orig),writeln(''),
	listoutputs(1,N,Outputs1),
	write("Enter 1-"),write(N),writeln(", or 0 for none:"),
	repeat2(Outputs1,Output,Flag1),
	conjunction(Flag1,true,Flag2).
repeat2(Outputs1,Output,Flag) :- 
	read_string(user_input, "\n", "\r", _End2, Input),
	number_string(N1,Input),
	(N1=0->Flag=false;
	(
		Input2 is N1-1,length(List,Input2),
		append(List,[[_Original,Backtranslation,Translation]|_],Outputs1),
		Output=[Backtranslation,Translation]
	)->
	true;
	repeat2(Outputs1,Output,Flag)
	).
listoutputs(N1,N2,[]) :- 
	N2 is N1-1, !.
listoutputs(N1,N3,[[Original,Backtranslation,_Translation]|Outputs1]) :-
	write(N1),write("\t\t"),write(Original),write("\n\t\t"),writeln(Backtranslation),write("\n"),
	N2 is N1+1,
	listoutputs(N2,N3,Outputs1).

conjunction(true,true,true) :- !.
conjunction(_,_,false) :- !.

%% list Trans
%% orig to choose from  v

/**
- 0. Enter from, to languages

ctt-input.txt
[[en,fr],['How do you do?...']]

ctt-orig1-orig2.txt
[[[en,fr],['How do you do?','How are you?']]...] 2nd item is correct back-translation with cult cust

1. If back-translation of sentence and ctt-orig-tran.txt version exists, or a link to it exists or is the same, adds to translation if nec (and lang tag x to) ctt-orig-tran.txt
	2. Else, 
	- asks if the sentences have the same meaning with return/n options
	If yes, goto 1
	If no, opens Grammarly editor to add possible from2 versions to retry
	
ctt-orig1-orig2-tmp-N.txt (keep all finished tmps, open in folder by itself)
[['<<<Note:>>>','How do you do?'],[[[BT:'How are you?',bt]...],[['How are you - entered?']...]]]
f2t only has f1 sentence and allows changes in grammarly - sentence (one of entered ones) with an acceptable back-translation will be added to f1f2

ctt-orig-tran.txt
[[[en,fr],['How are you?','Comment allez-vous?']]...]

ctt-output.txt
[[en,fr],[['How are you?','Comment allez-vous?']...]] - complete text translated

Note:
- Can split sentences into logic, A verb B with C x do manually
ctt-from2tmp.txt (delete just before end)
([[['How are you?'] x v working translation exists] x,[['How do you do?' (original first),...]]
- assumes Sentences have been checked with Grammarly before using the program
- lists from1 or from2 distance 1 x n away from from1 x all with same meaning index (eg 1)
- mistaken mergers will require sentences copied from screen, manually diverged later x undo feature - makes backups at each point with command to undo) x
() to (translation), from2 (back-translation) outputted
back-translation ready at this stage
**/

%% star 2

%%ctt-orig1-orig2.txt
%%[[[en,fr],['How do you do?','How are you?']]...] 2nd item is correct back-translation with cult cust

%%ctt-orig-tran.txt
%%[[[en,fr],['How are you?','Comment allez-vous?']]...]

%% star 3

%%[['<<<Note:>>>','How do you do?'],[[[BT:'How are you?',bt]...],['How are you - entered?'...]]]

%% * enter default tmp contents v


%%ctt-orig1-orig2.txt
%%[[[en,fr],['How do you do?','How are you?']]...] 2nd item is correct back-translation with cult cust
%%o1 o2***
%%ctt-orig-tran.txt
%%[[[en,fr],['How are you?','Comment allez-vous?']]...]
%%o2 l***


%% List=[o1 o2]1, List4=[o2 l]1,String4=[o2* l]2 where Word3 is o12

concat_list(A1,B):-
	A1=[A|List],
	concat_list(A,List,B),!.

concat_list(A,[],A):-!.
concat_list(A,List,B) :-
	List=[Item|Items],
	string_concat(A,Item,C),
	concat_list(C,Items,B).
