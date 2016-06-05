;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  WLA-DX learning & testing journal  ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;  test.s

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATE OF MOST RECENT EDITS: 6/5/16, JUST INSERTING A COMMENT ABOUT 9.5 BUG, AND 12/19/15 THE LAST SIGNIFICANT EDIT WITH MORE TESTS ADDED ETC.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; start: basic stuff required for producing a rom ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.lorom

.memorymap
	slotsize $8000
	defaultslot 0
	slot 0 $8000
.endme

;.lorom 			; WLA installation examples put lorom after memory map and before rombanksize/rombanks but it made no difference here.

.rombanksize $8000
.rombanks 2

.emptyfill 255

;.base $80			; didn't make a bit of difference for me (WLA examples put it here)
.bank 0
.org 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end: basic stuff required for producing a rom ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; macro counter test - show that WLA starts the count at 0
.macro showCounter
	.db \@
.endm
.rept 16
	showCounter 
.endr


; base directive and operand hint (or lack thereof) tests
rep #48				; accumulator and indices are 16 bit
lda #1				; appears as a 16 bit value of 1
and #5				; AND operates on accumulator, so 5 appears as a 16 bit value also
lda 0				; this came out $A5 $00
lda 0.w				; this came out $AD $00 $00 
; so immediate mode does not need hint for 16b when A is repped, but absolute operands do

sep #48
lda 0				; $A5 $00
lda 0.w				; $AD $00 $00
lda #1				; $A9 $01
lda #1.w			; $A9 $01 $00 so hints can be lowercase
rep #48
lda 0.b				; Looking at the repped lda 0 in the previous group, the hint for one byte is not necessary for absolute mode repped
lda #2				; A9 02 00 
lda #2.b			; A9 02


; lda $FFFF.l			; try it before any .base directives -- RESULTS IN AF FF FF 00, SO .BASE 0 IS NOT NECESSARY
; .base 0			; APPARENTLY NOT NECESSARY
lda #$FFFF.l		; maybe i need to stop using immediate mode for a 24bit addr --YES immediate mode yielded A9 FF FF 
lda $FFFF.l			; this yields AF FF FF 00
.base $80			; IT HAS NO EFFECT, EVEN IF I GET RID OF .BASE 0, SO COMMENT IT OUT
lda 0.l			; results in AF 00 00 00 REGARDLESS OF .BASE $80 JUST BEFORE
lda 2.l			; results in  AF 02 00 00
; so the question of how often must base directive be given without it defaulting back to $00 is not answered because $80 is ignored.
; could that have to do with size of file, only one bank, or how i declared the rom banks?  Is .lorom not enough?
; look more into memory mapping 


.enum 0 export
dp01	db
dp02	db
dp03	db
dp04	db
.ende

.enum $0000 export
ram01	db
ram02	db
ram03	db
ram04	db
.ende

; we're still rep
lda dp01				; result $A5 $00
lda ram01				; result $A5 $00
; basically this is just an attempt at another lda 0 and lda 0.w comparison, we're seeing if the $0000 value given in define/enumerate serves as a .w hint.
; ok so this proves the following
;		extra digits do not serve to hint at word length when giving the initial value of an .enum
;		it does work like a true function, you can map to a value as much as you wish, both x and y can be 0, e.g. but use x and y in different memory contexts (like x is the 0 of RAM but y is the 0 of ROM)
lda ram01.w		; if you don't want dp mode, but 16bit length reference to the first addr in ram, you HAVE to hint numbers under 256 with .w to get 16bit abs addr, but default mode is for smallest byte possible, dp, if number is under 255 of course

; ABOUT BASE...
; OK SO I DON'T NEED TO DEFINE A BASE OF 0
; WHAT IF I ONLY DECLARED BASE $80?  doesn't help
; WLA INSTALLATION EXAMPLES PUT .BASE BEFORE .BANK .ORG AND SAID TO WORK doesn't help
; maybe it's because i only have 1 bank? --NO THAT'S NOT IT
; and i'd rather declare .base in code not before a section... if i can
; well, tried with 2 banks and .base both right before opcode and right before .bank/.org, no effect either way.


; Read the entire bazz umass article on lorom and the wiki.superfamicom.org article on memory mapping so now i understand lorom as good as i can,
; but i believe it was superfamicom wiki article on fastrom and nesdev.com thread about .base that clinched this solution: it only uses a 
; numerical value of $80+ for the bank when the opcode is a long jump to change the program bank.  So let's try it.
.base $80	
				jsl 	label
  label:		lda 	ram02
; WLA-DX readme says you don't have to use colons for labels but it wouldn't compile this until i put a colon in. [THIS IS A 9.5 BUG]
; ok it worked.  now that base is $80 how about i continue to hint long length with opcodes other than jsl 
; and make another jsl to see if it still provides $80, then we'll try a $7E.  The nesdev thread suggests $7E won't work,
; but recall nothing but $80 works for ROM banks, because it adds the WLA bank to $80 to get $81 and higher, 
; so is there a starting point value for RAM like $80 is for ROM?
			lda		ram03.l		; this had a 0 bank byte
			lda		label.l		; this had the $80 bank byte SO IT DOES WORK FOR OTHER OPCODES, JUST SO LONG AS THERE'S A PRIOR .BASE AND JSL
								; OR IS IT THE LABELING THAT MAKES THE $80 KICK IN?
			jsl		label		; this too had the $80 bank byte.	
.base $7E
			lda		ram04.l			; this did not put a $7E in the addr, just $00, just like nesdev said it would.
;.base 0 	jsl		label			; must one hit return after base directive?  tried with .base $80, and it made a label with $80 bank byte, 
									; but that could just be because it's still in effect from above, so, i need to change base to see if it's
									; really working and not just continuing with the first .base $80 setting.
									; THIS DID NOT WORK, STILL $80, SO TRY IT WITH A CARRIAGE RETURN
;.base 0
;			jsl		label			; still $80
; So "label" seems to be forever tied with the .base $80 preceeding it.  perhaps another label to follow is the only way to change base.
; I don't want to do another jump though.  So what if i just .base then give a label?
.base 0		label2:		.db 10,11,12,13,14,15		; WORKED LIKE A CHARM!  .BASE-ING THE LABEL GAVE THE LABEL THE DESIRED 24BIT FORM
						ldx		#0
			loop1:		lda		label2.l,x
						inx
						cpx		#5
						bne		loop1


; works as expected.

; * back to the 24bit defines address problem of removing the high byte.
; searching the readme for "truncate" turns up empty, but i already know how to extract the bank byte of a variable, with colon before.
; So remember :label gives the bank and... if i'm not mistaken, by extension, label: gives just the address part?
; does this apply to variables?
; if i've .enum'd anything with 24bit values, perhaps var: would truncate the 24bit "var"
; lda loram2:  		; this doesn't work, and a colon on the var in defines/enum doesn't help.
; let's read the readme on enums for clues.

; BUT HOLD ON, IF LDA WILL TAKE A 24 BIT VARIABLE WITHOUT A .L HINT, WHY CAN I NOT JUST LITERALLY PUT THE 24BIT ADDRESS IN?   I REMEMBER IT NOT WORKING BEFORE BUT THAT MIGHT HAVE BEEN BECAUSE I WAS STUCK ON USING IMMEDIATE MODE IN EVERY INSTRUCTION I TESTED.  
LDA $800001
LDA $7E0002
LDA ram04+$7E0000
lda $700004
; that's it!  immediate mode 24bit was what was f'ing me up.
; SO LET'S NOT USE 24BIT DEFINES, BUT SIMPLY ADD $7E0000 OR whatever to TO 16BIT DEFINES.
; we could even add $800000 instead of .base $80 and just rely on default 0 base, thus never having to declare .base
; BUT .base conveniently adds wla .bank value to .base $80 thus allowing us to refer to other banks without changing .base $80.
; because seems to me if you must make the effort to individually do arithmetic +$800000, +$810000, +$82000, ... 
; you might as well not even define variables and go on and write whole 24bit addr.
; but $7E is not handled by base so we have no choice but to write +$7E0000 in the few cases of 24bit $7E addrs as operands,
; and <RAMVARNAME>+$7E0000 is much more readable and intuitive than writing $7E<addr> because there are so many ram variables
; and names of things are easier to remember than the numbers.
; NOW 24BIT DEFINES WORK BECAUSE THE HIGH BYTE IS NON-ZERO, SO STRINGS OF 00 BYTES AS DEFINES INITIAL VALUES GET SHORTENED TO JUST ONE BYTE.
; similarly when explicitly writing long addresses where high byte is nonzero, wla knows there's no other choice but 24bit form so no hint is needed.


; suppose this $7E don't work like they say it don't [it didn't] well can one make .defines 24bit?
.enum $7E0000 export		; put $7E in the enumeration to make a 24bit value, and see if the variables in it retain the bank value
loram1		db
loram2		db
.ende
; it didn't not tell me that a 24bit .enum initial value was not allowed, so onward to test.
lda 	loram1.l				; THIS WORKED!!! YAY!
; i see this going to be the way to do ram defines, but then how does one cut out the high bank byte?  would the .w hint truncate it?
lda 	loram1.w				; this did not truncate, it was still a long addr.
lda		loram1					; try without the hint, maybe that'll truncate it (got idea b/c i forgot the hint on a variable that i did want to be shown 24bit, then caught myself to correct and put the hint, then thought, well wow what would it have been like if i had not corrected it?)
								; hintless did not truncate.  going to have to look in readme on this. *

; ok so now i would like to see if i can just put the base on the label i'd jump long to, rather than before the jump long.
											
											jsl 		label3			; THIS WORKS!!! YAY!
.base $80			label3:      			lda			loram2			; THIS WORKS!!! YAY!

; ok so whenever we need to produce LDA $80<ADDR> in the rom, wherever we write the op or data that's at $80:<ADDR> needs to be labeled and that label needs a .base $80 directive in front of it.  
; and a new format
; directive field			label field:			opcode/db field			operand field
; _ _ nested directive indented two spaces over
; ALLOW 12 SPACES FOR LONGEST LABEL ON EACH LINE BEFORE TABBING TO REACH OPCODE FIELD, REGARDLESS OF LABEL SIZE OR NO LABEL AT ALL
; THE LABEL MUST BE FOLLOWED BY A COLON, THAT'S WHAT WLA-DX WANTS, DESPITE WHAT README SAYS,
; OR MAYBE THE WIN32 BINARY I DOWNLOADED ISN'T TAKEN FROM WHATEVER VERSION WLA ALLOWED LABELS TO NOT HAVE A COLON.
; ANOTHER REASON TO REINSTALL AND PRACTICE ON MICROSOFT VISUAL STUDIO AGAIN


; well now i'm kind of wondering, would $7E base work if i had only labeled.... no... because there is no such thing as a $7E ROM bank, remember the label has to be at a valid $80-$FF ROM address.   
; for example whenever you say you want to lda from $7E address, that's not ROM.  only the 24bit defines can solve the lda $7E<addr> problem.

; NOW I CAN ALSO SEE, IF A LOT OF PLACES WITHIN SAY ONE SECTION ALL GET REFERRED TO BY LDA $80<addr>, A BUNCH OF .BASE $80S MAKES NO SENSE,
; SO MAYBE .BASE $80 ONLY THE LABEL OF FIRST THING THAT COULD BE REFERRED TO IN THAT SECTION.
; JUST REMEMBER THAT THE .BASE $80 DIRECTIVE IS MEANT TO ACT ON A LABEL, AND ONCE IT ACTS ON ONE, IT'LL ACT ON ALL FOLLOWING UNTIL .BASE IS CHANGED BACK TO DEFAULT 0.
; LET'S TEST
.base 0				label4:				.db 16,17,18,19,20
					label5:				.db 21,22,23,24,25
.base $80			label6:				.db 26,27,28,29,30
										lda				label4.l
										lda				label5.l
										lda				label6.l
										.db 31,32,0



; but suppose i .base $80 the entire rom code, say at the beginning, but then call for a ram variable within a defines, i.e. a variable without a rom-addressed label?
; test, so the last base i declared was $80, so it should stick for all labels declared after it, 
; but my ram00s and my dp00s were in enums and not labeled as part of rom, so their long forms, no matter where they are called, even in a base $80-labeled line,
; should not have base $80 but 0 in long form, and it is only the addresses labeled here that will be referred to with bank byte $80 in long form.
  label7:		lda ram01.l
  label8:		lda ram02.l
  label9:		lda ram01.w
				lda label7.l
				lda	label8.l
				lda ram04.l
				lda dp01.l		
				lda ram01.l
				lda label0.w
				lda ram02.b   ; really not necessary to have defined dp01 as 0 and ram01 as 0000, because 0 is 0, and it's the hints that give 16bit vs dp mode. 
; works as expected, so declare .base $80 for entire ROM source, with each ROM source section in say slot 1, 
; and thus each label then in ROM source / slot 1 gets a long address with $80+ bank byte, but .defines are safe from this $80 bank byte,
; and will all have long forms with $00 bank bytes!  could put RAM .defines in a bank 0 slot 0 .ramsection that starts at $0000, and of course as readme says this doesn't get written to rom so it's fine about the bank being 0 and all.
; as for SNES hardware defines, maybe those ought to stay in an .enum since it doesn't feel right to me to have hardware regs as ram, nor does it seem right to have multiple ramsections starting at 0 but assigning different names, although that's not a problem with .enums, so probably not with ramsection, but let's just say hardware regs in ramsection is badform, because hardware bus b and internal cpu regs AREN'T RAM.
; ISN'T $0021XX NOT ALLOWED AS RAM, BUT RESERVED FOR ADDR BUS B?  I MEAN I'M SURE THERE IS $7E21XX STORAGE, BUT MUST NAME THAT HIGH BANK BYTE $7E!
; SO REALLY MAYBE ANY RAM WHOSE ADDRESS VALUE OVERLAPS WITH ADDR BUS B AND INTERNAL CPU REGS OUGHT TO BE IN A SPECIAL $7E0000 DEFINES?
; SO IMAGINE YOUR RAMSECTION INCLUDING TRUE RAM UP TO 0021XX AND FOR THE 0021XX'S YOU ARE NAMING SNES REGS


;	can i just like put a label with nothing after it?
label0:			; no errors, apparently so.  but does it refer right?  [YES it does] to test, change an operand above to this label or give an arbitrary org directive to skip this label's addr and make an lda refer to label0
.org $00D9		
lda label0.w
; lda label0  - won't run this one, because says value of label0 is too much to be an 8-bit value, so, being in rep i guess the label0: grabs FFFF not FF... or maybe it is requiring me to .w 
;
;
; Ram filling address test
sta (0*$10000),x		; 95 00 --SURPRISE! grouping but no indirection
sta (0*$10000).l,x		; 9F 00 00 00 (grouping, no indirection)
sta ((0*$10000)),x		; 95 00 --still does not create STA w/ indirect operand
sta ((0*$10000).l),x	; 9F 00 00 00 --operand is still not indirect
sta 0.l,x				; 9F 00 00 00
sta 1*$10000,x			; 9F 00 00 01
sta $7E*$10000,x		; 9F 00 00 7E
sta $7F*$10000,x		; 9F 00 00 7F
; this is also a test for parenthesis as grouping instead of an addressing mode
; if the parenthesis is given a hint directive, it becomes a grouping parenthesis instead of indirection
; there will have to be a conditional to handle 0 with the grouping,
; but if there is a special 0 case why not 0.l?
; indirection never occurs for these, so to get indirection,
; you'd probably have to store the calculation result in a variable
; like so:
.define given 0
.define result given*$10000
; then put the result in parenthesis, 
;sta (result),x
; it doesn't like the parenthesis wtf? i tried it with an actual number instead of a var and still wouldn't take it.
; UNTIL I FIGURE OUT IF THERE'S A BUG WITH INDIRECTION, 
; I'LL HAVE TO ENTER ANY INDIRECT OPS DIRECTLY WITH BYTECODE USING .DB !!!
;
; ONE MORE TEST... WHAT IF I HAVE .BASE $80 DECLARED,
; BUT LABEL AN OP WHOSE OPERAND IS AN ACTUAL NUMBER WITH HINT
; I EXPECT IT NOT TO GIVE OPERAND AN $80 BANK,
; BECAUSE AN ACTUAL NUMBER IS NOT A ROM SOURCE LABEL!
labelzero	.db 0
try1		sta 0.l,x		; this really is just turning 0 itself into a 24-bit number, and which base/bank is really irrelevant
try2		lda labelzero.l,x
			lda try2.l,x
			jsr try1
			jsl try1
; expectations met!
; and wow i forgot to colon the .db but it didn't generate an error...
; so i uncommented the very first time where i made a label, 
; and i got the error. what's the difference? 
; IT GIVES AN ERROR FOR ANY LABELING NOT ENDING IN A COLON 
; WHEN THE LABEL IS USED AS AN OPERAND BEFORE THE LABELING OCCURS
; when you want to lda from a rom addr, 
; for example here the addresses are for rom labels try1, try2,
; it has to be long form or you get an error.
; un-colon-ed labels will also work if written again as jump operands
; IF THAT JUMP COMES AFTER THE LABELING IN THE ROM.
