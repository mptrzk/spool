
//**token_ptr, **str_ptr, len
void token_add2(Token** token_ptr, char* str, int len) {
	(*token_ptr)->str = str;
	(*token_ptr)->len = len;
	(*token_ptr)++;
}

void token_add(Token* token_buf, int* token_count, char* str, int len) {
	token_buf[*token_count].str = str;
	token_buf[*token_count].len = len;
	(*token_count)++;
}
/*
void tokenize2(Token* token_buf, char* expr) {
	char single_char_tokens[] = "!~[]\"";
	for (int i=0; expr[i]; i++) {
		if (expr[i] <= ' ') continue; //exclude whitespace & non-printable
		for (char* c = single_char_tokens; *c; c++) {
			if (*c == expr[i]) {
				token_add2(&token_buf,  &expr[i], 1);
				break;
			}
		}
	}
	token_add2(&token_buf, 0, 0);
}
*/
