% File     : fill.in
% Author   : Wei Wang, and my login id is weiw9, student number, 690842
% Course   : Computer Science (Course Work)
% Purpose  : This program is designed to solve fillin crossword puzzles.

% Outline  : In order to fill in the puzzle, six steps are needed:
% 1st, get Puzzle from PuzzleFile and Wordlist from WordlistFile;
% 2nd, replace the '#' in the Puzzle with variables, and I call it VariablePuzzle;
% 3rd, get all slots in VariablePuzzle, except the slots whose length are 1 and 0, and I call it AllSlots
% 4th, sort AllSlots by considering two factors: one is putting the smallest length of grouped slots ahead,
%	   for every group, the length is the same; the other is putting the biggest length of a slot ahead,
%	   and the priority is the first factor. And finally I get the Least_Head_Slots;
% 5th, sort Wordlist just as AllSlots in 4th, and finally I get the Least_Head_Words;
% 6th, match the slots with the words.
  

% use the transpose/2 in library clpfd
:- use_module(library(clpfd)).

% the predicate main/3 is the entrance to solve the fillin puzzle.
% PuzzleFile is a file of puzzle, containing characters of '_' and '#', 
% or maybe alphabetical atoms, and the '_' is waiting for filling in.
% WordlistFile is a file of lists of words for fillin.
% SolutionFile stores the puzzle result, corresponding to the PuzzleFile, 
% but replacing the consecutive '_' with words from WordlistFile.
main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).
	
% read_file/2 is to read a file and the result is stored in  
% Content in the form of lists of lists of characters.
read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).	
	
% read_lines/2 read lines from Stream until the end of the file, here, Last = true,
% storing the lines in the Content, every line is a list of characters.	
read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).
	
% read_line/2 read a character from Stream to form a line 
% until the character is end_of_file or '\n'	
read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).
	
% print_puzzle/2 prints the contents of Puzzle to the file of SolutionFile
% if the SolutionFile does't exist, it will be constructed	
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).
	
% print_row/2 prints a row of characters from a list of Puzzle
% and end up with a newline
print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).
	
% put_puzzle_char/2 puts a char from a list, that is a row, into Stream except
% that the char is actually a variable, and then replaced with the '_'.	
put_puzzle_char(Stream, Char) :-
	(	var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).
	
% valid_puzzle/1 judeges validity of a puzzle by comparing the length of rows,
% if all the rows are of the same leng, then the puzzle is valid	
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).
	
samelength([], []).
samelength([_|L1], [_|L2]) :-
	samelength(L1, L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% solve puzzle %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% solve_puzzle/3 is the main part attempting to achieve the fillin puzzle
% Puzzle is a list of lists, every list is composed of '#' or '_' or alphabetical atoms
% Wordlist is a list of lists, every list is a word composed of alphabetical atoms
% Solved is the filled puzzle, also a list of lists, every list is composed of '#' or alphabetical atoms
solve_puzzle(Puzzle, Wordlist, Solved) :- 
% get all slots whose elements are either variables or combination of variables and alphabetical atoms
	replace_all_underscores(Puzzle, VariablePuzzle),
	all_slots(VariablePuzzle, AllSlots),

% get grouped slots, for every former slot, its element's length is bigger or equal than the latter
	sort_lists_by_length(AllSlots, SortedAllSlots),
	get_lists_lengths(SortedAllSlots, SlotsLengths),
	different_sorted_lengths(SlotsLengths, Slot_ReversedDiffSortedLengths),
	group_listof_lists(SortedAllSlots, Slot_ReversedDiffSortedLengths, GroupedSlots),
	
% get grouped slots with less length ahead
	sort_lists_by_length(GroupedSlots, Most_Head_GroupedSlots),
	reverse(Most_Head_GroupedSlots, Least_Head_GroupedSlots),
% transform a list of grouped slots to a list of slots
	append(Least_Head_GroupedSlots, Least_Head_Slots),
	
% get grouped words, for every former word, its element's length is bigger or equal than the latter
	sort_lists_by_length(Wordlist, SortedWordlist),	
	get_lists_lengths(SortedWordlist, WordsLengths),
	different_sorted_lengths(WordsLengths, Word_ReversedDiffSortedLengths),
	group_listof_lists(SortedWordlist, Word_ReversedDiffSortedLengths, GroupedWords),

% get grouped words with less length ahead
	sort_lists_by_length(GroupedWords, Most_Head_GroupedWords),
	reverse(Most_Head_GroupedWords, Least_Head_GroupedWords),
% transform a list of grouped words to a list of words
	append(Least_Head_GroupedWords, Least_Head_Words),

% assign variables in slots with values in words
	select_words_match_slots(Least_Head_Slots, Least_Head_Words),
% after all variables being assigned values, namely words, correspondingly, 
% varibales in VariablePuzzle also being assigned values
	Solved = VariablePuzzle.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% replace underscores with variables %%%%%%%%%%%%%%%%%%%%%%%%%%%%
% initialize a puzzle by replace all lines of underscores with variables
replace_all_underscores(UnderscorePuzzle, VariablePuzzle) :-	
	maplist(replace_oneline_underscores, UnderscorePuzzle, VariablePuzzle).

% repalce one line underscores with variables	
replace_oneline_underscores(UnderscoreLine, VariableLine) :-
	maplist(replace_underscore, UnderscoreLine, VariableLine).

% replace one underscore with one variable	
replace_underscore(Underscore, Variable) :-
	(	Underscore == '_'
	->	Variable = Var
	; 	Variable = Underscore	
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% store all slots without slots whose length are 1 and 0 %%%%%%%%
% Horizontal(or Vertical)_Lists_AllSlots_EmptyOneIncluded: list of lists of lists including list of lists whose length are 0 and 1
% Horizontal(or Vertucal)_AllSlots_EmptyOneIncluded: list of lists including lists whose length are 0 and 1
% Horizontal(or Vertucal)_AllSlots_EmptyIncluded:  list of lists including lists whose length are 1
% Horizontal(or Vertical)_AllSlots: list of lists without lists whose are 0 and 1
% AllSlots: list of lists by append Horizontal_AllSlots and Vertical_AllSlots

all_slots(VariablePuzzle, AllSlots) :-
% transpose a matrix
	transpose(VariablePuzzle, Vertical_VariablePuzzle),
	
% get all horizontal slots
	listsof_all_slots(VariablePuzzle, Horizontal_Lists_AllSlots_EmptyOneIncluded),
	append(Horizontal_Lists_AllSlots_EmptyOneIncluded, Horizontal_AllSlots_EmptyOneIncluded),
	filter_slots(Horizontal_AllSlots_EmptyOneIncluded, Horizontal_AllSlots_EmptyIncluded),
	subtract(Horizontal_AllSlots_EmptyIncluded,[[]], Horizontal_AllSlots),
	
% get all horizontal slots
	listsof_all_slots(Vertical_VariablePuzzle, Vertical_Lists_AllSlots_EmptyOneIncluded),
	append(Vertical_Lists_AllSlots_EmptyOneIncluded, Vertical_AllSlots_EmptyOneIncluded),
	filter_slots(Vertical_AllSlots_EmptyOneIncluded, Vertical_AllSlots_EmptyIncluded),
	subtract(Vertical_AllSlots_EmptyIncluded,[[]], Vertical_AllSlots),

% append horizontal and vertical slots in order to get all slots 	
	append(Horizontal_AllSlots, Vertical_AllSlots, AllSlots).
	
% get all lines slots, every line is a list of slots whose length may be 1 or 0
listsof_all_slots([],[]).	
listsof_all_slots([A|As], [B|Bs]) :-
	oneline_slots(A, B),
	listsof_all_slots(As, Bs).	
	
% get slots in one line, repaling '#' with variable
oneline_slots([],[[]]).	
oneline_slots([Var|Vars],[Slot|Slots]) :-
	(	(var(Var); Var \= '#')
	->	get_head_tail(Slot,Head,Tail), Head = Var, oneline_slots(Vars, [Tail|Slots])
	;	Slot = [], oneline_slots(Vars, Slots)
	).

% get the head and tail of a list
get_head_tail([H|T],H,T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% filter slots whose length are 1, replaced by a [] %%%%%%%%%%%%%
filter_slots([], []).
filter_slots([Slot|Slots], [FilteredSlot|FilteredSlots]) :-
	length(Slot, Length),
	(	Length = 1
	->	FilteredSlot = [], filter_slots(Slots, FilteredSlots)
	;	FilteredSlot = Slot, filter_slots(Slots, FilteredSlots)
	).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% sort list of lists by length of a list %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sort_lists_by_length([], []).
sort_lists_by_length([H|T],[H|R]) :-
        length(H, Lh),
        forall(member(M, T), (length(M, Lm), Lm =< Lh)),
        sort_lists_by_length(T, R), !.
sort_lists_by_length([F,S|T], R) :-
        append(T,[F],X),
        sort_lists_by_length([S|X], R).	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% group list of lists by length %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% group a list of lists, by put lists with same length together,
% and finally get a list of lists, every list in the list is a list of lists with same length
group_listof_lists([], _, [[]]).	
group_listof_lists(Lists, Numbers,[Group|Groups]) :-
	get_head_tail(Lists, LH, LT),
	get_head_tail(Numbers, NH, NT),
	length(LH, Length),
	(	Length = NH
	->	get_head_tail(Group, GH, GT), GH = LH, group_listof_lists(LT, Numbers, [GT|Groups])
	;	Group = [], group_listof_lists(Lists, NT, Groups)
	).

% get the lengths of all lists in a list 
get_lists_lengths([],[]).
get_lists_lengths([L|Ls], [N|Ns]) :-
	length(L, N), get_lists_lengths(Ls, Ns).
	
% lengths: big to small (all different)
different_sorted_lengths(Lengths, ReversedDiffSortedLengths) :-
	sort(Lengths,DiffSortedLengths),
	reverse(DiffSortedLengths, ReversedDiffSortedLengths).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% select words to match slots %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
select_words_match_slots([],_).
select_words_match_slots([Slot|Slots],Words) :- 
	select(Slot, Words, AnotherWords), select_words_match_slots(Slots, AnotherWords).
