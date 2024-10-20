define(`define_word', `dnl
define(`str_length', len(`$1'))dnl
define(`str_body', substr(`$1', 0, eval(len(`$1') - 1)))dnl
define(`last_char', substr(`$1', eval(len(`$1') - 1)))dnl
.set this_word, .
    .byte 80h+str_length
    .ascii "str_body"
    .byte ''`last_char''` + 80h
	.word last_word_address
	.set last_word_address, this_word')

define(`define_immediate_word', `dnl
define(`str_length', len(`$1'))dnl
define(`str_body', substr(`$1', 0, eval(len(`$1') - 1)))dnl
define(`last_char', substr(`$1', eval(len(`$1') - 1)))dnl
.set this_word, .
    .byte 0C0h+str_length
    .ascii "str_body"
    .byte ''`last_char''` + 80h
	.word last_word_address
	.set last_word_address, this_word')

define(`define_comma_word', `dnl
define(`str_length', len(`$1'))dnl
.set this_word, .
    .byte 80h+eval(str_length+1)
    .ascii "`$1'"
    .byte 2Ch + 80h
	.word last_word_address
	.set last_word_address, this_word')
changecom(`#M4-comment')