% ctt2.pl

/*

	ctt-input.txt
["en","fr","Helloa."]

	ctt-orig1-orig2.txt
[["en","fr","Helloa.","Hellow."]]

	ctt-orig-tran.txt
[["en","fr","Hellow.","Bonjour."]]

	ctt-output.txt
["en","fr","Bonjour. "]

*/

trans_location("../../../gawk/trans").

ctt2 :-
	get_files(Ctt_input,Ctt_orig1_orig2,Ctt_orig_tran,From_lang,To_lang),
	back_translate(Ctt_input,Ctt_orig1_orig2,Ctt_orig_tran,From_lang,To_lang,"",Translation,[],Ctt_orig1_orig2_1,[],Ctt_orig_tran_1),
	
save_file("files/ctt-output.txt",[From_lang,To_lang,Translation]),
save_file("files/ctt-orig1-orig2.txt",Ctt_orig1_orig2_1),
save_file("files/ctt-orig-tran.txt",Ctt_orig_tran_1),
!.

save_file(File_path,File) :-

	term_to_atom(File,String02a_b),
	string_atom(String02a_c,String02a_b),

	(open_s(File_path,write,Stream1),
	write(Stream1,String02a_c),
	close(Stream1)),!.


get_files(Item4,Ctt_orig1_orig2,Ctt_orig_tran,From_lang,To_lang) :-
	get_file("files/ctt-input.txt",Ctt_input),
	Ctt_input=[From_lang,To_lang,String1],
	%split_string2(String1,["\n","\r"],List1),
	string_codes(String1,Codes1),
	split_on_substring117(Codes1,`\n\r`,[],List1),
	%trace,
	findall(Item2,(member(Item1,List1),
	((Item1="\n"->true;Item1="\r")->Item2=[Item1];(
%trace,
	string_codes(Item1,Codes2),
	split_on_substring117(Codes2,`.!?`,[],Item11),

%split_string2(Item1,[".","!","?"],Item11),
join_chars_after(Item11,[".","!","?"],[],Item2))
)),Item3),
	maplist(append,[Item3],[Item4]),
	findall(Item5,(member(Item5,Item4),not(((Item5="\n"->true;Item5="\r")))),Item6),
	length(Item6,Length0),
	write("Number of sentences to translate in files/ctt-input.txt: "), writeln(Length0),

	sort(Item6,CttInput2),
	length(CttInput2,Length1),write("Number of unique sentences to translate in files/ctt-input.txt: "), writeln(Length1),

	get_file("files/ctt-orig1-orig2.txt",Ctt_orig1_orig2),
	findall([A,B],member([From_lang,To_lang,A,B],Ctt_orig1_orig2),Ctt_orig1_orig22),
	
	length(Ctt_orig1_orig22,Length2),
	
	write("Number of back-translation pairs in lang1->lang2: "), writeln(Length2),
	
	get_file("files/ctt-orig-tran.txt",Ctt_orig_tran),
	
		findall([A,B],member([From_lang,To_lang,A,B],Ctt_orig_tran),Ctt_orig_tran2),

	length(Ctt_orig_tran2,Length3),
	
	write("Number of translation pairs in lang1->lang2: "), writeln(Length3),
	
		findall(Item9,member([From_lang,To_lang,Item9,_],Ctt_orig1_orig22),Item10),

findall(Item7,member([From_lang,To_lang,_,Item7],Ctt_orig1_orig22),Item8),

findall(Item11,member([From_lang,_,Item11],Ctt_orig_tran2),Item12),
	
	subtract(Item6,Item10,D1), %%
	length(D1,Length),Difference is abs(Length),write("Number of back-translations remaining to define: "), writeln(Difference),
	
	%%towords2(CttInput1,[],CttInput2),
	subtract(Item8,Item12,D2), %% Should AUN be appended to TF, " x
	%%delete(D21,'',D2),
	length(D2,Length01t),Differencet is abs(Length01t),write("Number of undefined back-translations: "), writeln(Differencet),
	%%writeln([undefined,D2]), %% Print undefined 

	%%delete(D31,'',D3),
	subtract(Item12,Item8,D3),
	length(D3,Length01t2),Differencet2 is abs(Length01t2),write("Number of orphaned translations: "), writeln(Differencet2),!.	

	
get_file(File_path,File) :-	
	phrase_from_file_s(string(String00a),File_path),
	string_codes(String02b,String00a),
	atom_to_term(String02b,File,[]),!.

bash_command(Command, Output) :-
        setup_call_cleanup(process_create(path(bash),
                ['-c', Command],
                [stdout(pipe(Out))]),
        read_string(Out, _, Output),
        close(Out)).

/*

translate_ctt2("Hello.","en","fr","Bonjoura.").
translate_ctt2("Bonjoura.","fr","en","Hellok.").

translate_ctt2("Helloa.","en","fr","Bonjour.").
translate_ctt2("Bonjour.","fr","en","Helloaa.").

translate_ctt2("Helloc.","en","fr","Hellod.").
translate_ctt2("Hellod.","fr","en","Helloca.").

translate_ctt2("Hellocc.","en","fr","Hellodc.").
translate_ctt2("Hellodc.","fr","en","Hellocca.").

*/

translate_ctt2(Input,FromLang,ToLang,Output3) :-
	trans_location(Trans_location),
	insertdoublebackslashbeforequote(Input,Input1),
	concat_list([Trans_location," ",FromLang,":",ToLang," \"",Input1,"\""],F),
	
repeat,

catch(call_with_time_limit(5,
catch(
	(bash_command(F,Output1)),
   _,
	(writeln("Translate failed.  Press c to retry."),
	read_string(user_input, "\n", "\r", _,C),
	C="c"->fail;abort)
)
),
      time_limit_exceeded,
      (writeln1("Error: translate timed out."),abort)),

	split_string(Output1,"\033","\033",Output2),
	Output2=[_,Output3a|_],	
	atom_string(Output3a,Output3b),
		string_concat("[1m",Output3,Output3b),
!.

back_translate([],Ctt_orig1_orig2,Ctt_orig_tran,_From_lang,_To_lang,Translation,Translation,Ctt_orig1_orig2_1,Ctt_orig1_orig2_2,Ctt_orig_tran_1,Ctt_orig_tran_2) :- 
		append(Ctt_orig1_orig2,Ctt_orig1_orig2_1,Ctt_orig1_orig2_2),
		append(Ctt_orig_tran,Ctt_orig_tran_1,Ctt_orig_tran_2),!.
	
	back_translate(Ctt_input,Ctt_orig1_orig2,Ctt_orig_tran,From_lang,To_lang,Translation1,Translation2,Ctt_orig1_orig2_1,Ctt_orig1_orig2_2,Ctt_orig_tran_1,Ctt_orig_tran_2) :-
	
	Ctt_input=[Ctt_input1a|Ctt_input2],
	remove_spaces_from_start(Ctt_input1a,"",Ctt_input1),

	back_translate2(Ctt_input1,Ctt_input1,Ctt_orig1_orig2,Ctt_orig_tran,From_lang,To_lang,Translation3,Ctt_orig1_orig2_3,Ctt_orig_tran_3),
	
	concat_list([Translation1,Translation3," "],Translation4),
		back_translate(Ctt_input2,Ctt_orig1_orig2_3,Ctt_orig_tran_3,From_lang,To_lang,Translation4,Translation2,Ctt_orig1_orig2_1,Ctt_orig1_orig2_2,Ctt_orig_tran_1,Ctt_orig_tran_2),!.

	
	
	%*Ctt_orig1_orig2_1=Ctt_orig1_orig2_3,Ctt_orig_tran_1,Ctt_orig_tran_3
%in neither dict, add to both
	%)).
	%update dicts in other cases
	
	% add space afterwards
	
remove_spaces_from_start("",B,B) :- !.
remove_spaces_from_start(A,_B,A) :-
	not(string_concat(" ",_C,A)),
	!.
remove_spaces_from_start(A,B,D) :-
	string_concat(" ",C,A),
	remove_spaces_from_start(C,B,D),!.

	%back_translate2(_Ctt_input0,_Ctt_input1,Ctt_orig1_orig2_1,Ctt_orig_tran_1,_From_lang,_To_lang,Translation3,Translation3,Ctt_orig1_orig2_1,Ctt_orig_tran_1) :- !.
	back_translate2(Ctt_input0,Ctt_input1a,Ctt_orig1_orig2,Ctt_orig_tran,From_lang,To_lang,Translation3,Ctt_orig1_orig2_1,Ctt_orig_tran_1) :-
	%trace,

	remove_spaces_from_start(Ctt_input1a,"",Ctt_input1),


	(((Ctt_input1="\n"->true;(Ctt_input1="\r"->true;Ctt_input1="")),
	Translation3=Ctt_input1,
	%(string_concat(Translation1,Ctt_input1,Translation3),
	Ctt_orig1_orig2=Ctt_orig1_orig2_1,Ctt_orig_tran=Ctt_orig_tran_1)->true;
	
	
	((member([From_lang,To_lang,Ctt_input1,Ctt_orig1_orig2_11],Ctt_orig1_orig2),
	member([From_lang,To_lang,Ctt_orig1_orig2_11,Translation3],Ctt_orig_tran),
	%Ctt_orig1_orig2=Ctt_orig1_orig2_1,Ctt_orig_tran=Ctt_orig_tran_1
	
			append_if_needed(Ctt_orig1_orig2,[[From_lang,To_lang,Ctt_input0,%*** not earlier % here too
		Ctt_orig1_orig2_11]],Ctt_orig1_orig2_1),
	append_if_needed(Ctt_orig_tran,[[From_lang,To_lang,Ctt_orig1_orig2_11, % here too
	Translation3]],Ctt_orig_tran_1)

)->true;
	
	
	((member([From_lang,To_lang,Ctt_input1,Translation3],Ctt_orig_tran),Ctt_orig1_orig2=Ctt_orig1_orig2_1,Ctt_orig_tran=Ctt_orig_tran_1)->true;
	
		((member([From_lang,To_lang,Ctt_input1,Ctt_orig1_orig2_11],Ctt_orig1_orig2),
	not(member([From_lang,To_lang,Ctt_orig1_orig2_11,Translation3],Ctt_orig_tran)),
	
	%translate_ctt2(Ctt_orig1_orig2_11,% or ctinput
	%From_lang,To_lang,Translation3),
	%translate_ctt2(Translation3,% or ctinput
	%To_lang,From_lang,Ctt_orig1_orig2_11),
	%trace,
	((back_translate_and_check(Ctt_input0,Ctt_orig1_orig2_11,% or ctinput
From_lang,To_lang,Translation3),

	append_if_needed(Ctt_orig_tran,[[From_lang,To_lang,Ctt_orig1_orig2_11, % here too
	Translation3]],Ctt_orig_tran_1),%trace,
	Ctt_orig1_orig2=Ctt_orig1_orig2_1)->true;
	
		fail
		%check_similar_sentences(Ctt_input0,Ctt_orig1_orig2,Ctt_orig_tran,From_lang,To_lang,Translation3,Ctt_orig1_orig2_1,Ctt_orig_tran_1) 
)

	)->true;
	
	
	((not(member([From_lang,To_lang,Ctt_input1,Translation3],Ctt_orig_tran)),
	
	((not(member([From_lang,To_lang,Ctt_input1,Ctt_orig1_orig2_1],Ctt_orig1_orig2)),

	((
	%translate_ctt2(Ctt_input1,% or ctinput
	%From_lang,To_lang,Translation3),
	%translate_ctt2(Translation3,% or ctinput
	%To_lang,From_lang,Ctt_input1),
	
		back_translate_and_check(Ctt_input0,Ctt_input1,% or ctinput
From_lang,To_lang,Translation3),

	append_if_needed(Ctt_orig_tran,[[From_lang,To_lang,Ctt_input1, % here too
	Translation3]],Ctt_orig_tran_1),
	Ctt_orig1_orig2=Ctt_orig1_orig2_1)->true;
	check_similar_sentences(Ctt_input0,Ctt_orig1_orig2,Ctt_orig_tran,From_lang,To_lang,Translation3,Ctt_orig1_orig2_1,Ctt_orig_tran_1) 
))))))))),

!.

check_similar_sentences(Ctt_input0,Ctt_orig1_orig2,Ctt_orig_tran,From_lang,To_lang,Translation3,Ctt_orig1_orig2_1,Ctt_orig_tran_1) :-
	concat_list(["\n","Please enter a sentence with the same meaning as \"",Ctt_input0,"\", which can be more easily translated:"],Note1),	
	writeln(Note1),
	read_string(user_input,"\n\r","\n\r",_,Ctt_input1b),
	
		%trace,
		back_translate2(Ctt_input0,Ctt_input1b,Ctt_orig1_orig2,Ctt_orig_tran,From_lang,To_lang,Translation3,_Ctt_orig1_orig2_2,Ctt_orig_tran_2),
		
		%Ctt_orig1_orig2_2=Ctt_orig1_orig2_1,
		Ctt_orig1_orig2_1=[[From_lang,To_lang,Ctt_input0,Ctt_input1b]],
		Ctt_orig_tran_2=Ctt_orig_tran_1,
		%append_if_needed(Ctt_orig1_orig2_2,[[From_lang,To_lang,Ctt_input0,%*** not earlier % here too
	%	Ctt_input1b]],Ctt_orig1_orig2_1),


	%append_if_needed(Ctt_orig_tran_2,[[From_lang,To_lang,Ctt_input1b, % here too
	%Translation3]],Ctt_orig_tran_1),
	!.


back_translate_and_check(Ctt_input0,Ctt_orig1_orig2_11,% or ctinput
From_lang,To_lang,Translation3) :-					
	translate_ctt2(Ctt_orig1_orig2_11,% or ctinput
	From_lang,To_lang,Translation31),
	translate_ctt2(Translation31,% or ctinput
	To_lang,From_lang,Ctt_orig1_orig2_12),
	((Ctt_orig1_orig2_11=Ctt_orig1_orig2_12)->
	Translation3=Translation31;
	(concat_list(["\n","Are the following sentences:\n- grammatical\n- have the same meaning as the other in the pair, and\n- have the same meaning as the original sentence (y/n)?\n\n",
	"Original:","\t",Ctt_input0,"\n\n",
	"\t\t",Ctt_orig1_orig2_11,"\n\t\t",Ctt_orig1_orig2_12],Note1),
	writeln(Note1),
	read_string(user_input,"\n\r","\n\r",_,YN),
	%repeat,
	%trace,
	(YN="n"->fail;Translation3=Translation31))),!.



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

append_if_needed(A,[B],C) :-
%trace,
	(member(B,A)->C=A;
	append(A,[B],C))%,notrace
	,!.