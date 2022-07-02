#pragma once

char* src_lus = "\
 	[? #data\
     [? [< #data]\
				[. [' ~]\
				   * . [. [> #data]\
								  #code]\
							 #code]\
				. [' ~ ~]\
					> #data]\
		 ' [[~ ~] ~]]";

char* src_eqv = "\
[? [< #data]\
	 [? [> #data]\
	    [? [* . [. [. [< < #data]\
			              < > #data]\
						     #code]\
			        #code]\
				 [? [* . [. [. [> < #data]\
									     > > #data]\
								    #code]\
							   #code]\
						[' ~ ~]\
						' ~]\
				 ' ~]\
			' ~]\
	 ? [> #data]\
	   [' ~]\
		 ' ~ ~]";

char* src_expand1 = "\
[? [< #data]\
	 [? [* . [. [. [< < #data]\
				  			 ' !]\
						  ' #eqv]\
					 ' #eqv]\
		  [* . [. [> < #data]\
			        > #data]\
					 > #data]\
		  ? [* . [. [. [< < #data]\
			 						 ' ?]\
							  ' #eqv]\
						 ' #eqv]\
			  [. [* . [. [. [> < #data]\
				              > #data]\
			             #code]\
								#code]\
					 . [* . [. [. [> > < #data]\
											> #data]\
									 #code]\
								#code]\
						 * . [. [. [> > > < #data]\
				               > #data]\
			              #code]\
							   #code]\
		    ? [* . [. [. [< < #data]\
			 	  					 ' .]\
					  		  ' #eqv]\
						   ' #eqv]\
          [. [* . [. [. [> < #data]\
				                > #data]\
			               #code]\
						  	  #code]\
					   . [* . [. [. [> > < #data]\
						 			  	    > #data]\
									     #code]\
								    #code]]\
          \
          . * . [. [. [> < #data]\
				                > #data]\
			               #code]\
						  	  #code]\
	 ' ~]"
;

char* src_expand = "\
[? [< #data]\
	 [? [* . [. [. [< < #data]\
				  			 ' !]\
						  ' #eqv]\
					 ' #eqv]\
		  [* . [. [> < #data]\
			        > #data]\
					 > #data]\
			. [< < #data]\
			  [* . [. [. [> < #data]\
			             > #data]\
					      #code]\
					   #code]]\
	~]\
"
;

char* src_make_gate = "[. [' *] . [' .] . [. [' .] . [' ~] . [' '] ~] . [' '] ~]";
