%% Multiline file1.txt will contain list of sentences which were attemptedly translated
%% Instructions at top of file1.txt to save and quit
%% with path to load and save in

:- include(library(edit)).
:- multifile
	edit:edit_command/2.
:- multifile prolog_edit:load/0.

%%prolog_edit:load :-
%%        ensure_loaded(library(swi_edit)).

run :- %%,
	%%prolog_edit:locate("/Applications/MacVim.app/Contents/MacOS/macvim-askpass",F,_L),
	%%arg(1,F,A),
	%%	prolog_edit:locate("file1.txt",_F2,L2),
	%%set_prolog_flag(editor,'gvim --line2'),
	%%prolog_edit:edit_command(gvim, '%e --line%d'),
	%%atom_concat(A, ' file1.txt --line',B),
	%%atom_concat(B, 2 ,C),
	%%shell(C),
	%%prolog_edit:edit_source(L),
	edit("file1.txt"),
writeln(here).

rungrammarly :- %%,
	prolog_edit:locate("/Applications/Grammarly.app/Contents/MacOS/Grammarly",F,_L),
	arg(1,F,A),
	%%prolog_edit:locate("file1.txt",_F2,L2),
	%%set_prolog_flag(editor,'gvim --line2'),
	%%prolog_edit:edit_command(gvim, '%e --line%d'),
	atom_concat(A, ' file1.txt --line',C),
	%%atom_concat(B, 2 ,C),
	shell(C),
	%%prolog_edit:edit_source(L),
	%%edit("file1.txt"),
a(a),
writeln(here),!.

a(a).
b:-%%prolog_edit:
rungrammarly.
	

open_grammar_editor(Word,FromLang,ToLang,List,List4,String4,N,COO2) :-
	%%downcase_atom(Word, Word2), atom_string(Word2,Word),
	
	%%prolog_edit:locate("/Applications/Grammarly.app/Contents/MacOS/Grammarly",F,_L),
	%% prolog_edit:locate("/usr/bin/vi",F,_L),
	%%arg(1,F,A),
	%%atom_concat("edit(\"", ' ',C),
	
	atom_concat('tmp/ctt-orig1-orig2-tmp-',N,D),
	atom_concat(D, '.txt',E),
	
	atom_string(E,H),
	
	%% atom_concat("edit(\"",E,G),
	%% atom_concat(G, '\").',H),
 	
 	%%fulladjective(FromLang,FromLang1),
 	
	writenotification1('<<<Note: Edit a pair and delete the stub, or insert a pair as follows.  Save at path ',E,' using the menu>>>',Notification1),
	writenotification2('Insert "',FromLang,'" sentence with meaning of the first sentence here.',Notification2),
	%%writenotification('<<<Note: *** Your previous entry was not in the correct format.  Please correct this. *** Please enter only "',FromLang,'" sentences with the meaning of the first sentence to find a correct back-translation by appending item(s) to the last list: * in (((),(),(),(*))).  Open and save using the menu as path ',E,'>>>',Notification2),

	rmoldtmpfile(E),
	wrap(COO2,[],COO5),
	append([[[Word],[""]]],COO5,COO4),
	%%sort(COO4,COO5),
	append([[["<<<History of tries:>>>"],[""]]],COO4,COO3),
	append(COO4,[[[Notification2],["Insert an alternative that might translate better here."]]],Editable1),
	%%[Editable2]=Editable1,
FileContents1=[[[Notification1],[Word]],COO3,Editable1],
	updatefile2(FileContents1,E),

%% star 3
	backtranslateuntilcorrect(List,List4,Word,[],FromLang,ToLang,E,H,Notification1,Notification2,String4), %% check against List
	
	atom_concat('mv ',E,G1),
	atom_concat(G1,' ',G2),
	atom_concat(G2,'files/ctt-orig1-orig2-tmp-',G3),
	atom_concat(G3,N,G4),
	atom_concat(G4, '.txt',G5),

				(bash_command(G5,_)->
					true;
					(writeln("Failed moving tmp file to ctt folder."),abort)
				). %% open editor

rmoldtmpfile(E) :-
	atom_concat('rm -f ',E,G1),

				(bash_command(G1,_)->
					true;
					(writeln("Failed removing tmp file."),abort)
				). %% open editor
