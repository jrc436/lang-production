#!/usr/bin/perl


# unfortunately, we cannot just load up these files in
# (e)lisp and interpret them as a list structure.
# too many quotes and commas and other stuff that doesnt
# qualify as a symbol.
# therefore, just a perl program that parses the bracket syntax.


# workflow
# - Switchboard (Penn format) -> remove-disfluencies
#         run as: cd treebank_mrg; ../remove-disfluencies
# - Switchb-nodysfl (Penn format) -> Hockenmaier Conversion
# - Switchb (CCG format / TXT/HTML) + terminals.xml (Paraphrase)
#      -> read-julia.py (alignment tool)
# - switchboard-ccg-rules.txt -> find-repetitions.py -> switchboard-ccg.data
#

$make_diff = 0;  # make-diff files aren't really used for anything
# they were meant for time-alignment.



@a = glob("*.mrg");

for (@a) {


  open IN, $_;
  $cont = "";
  while(<IN>) {
    unless (/^\*x\*/) {
      $cont .= $_;
    }
  }
  close IN;

  #open OUT, ">". $_.".fil";

  $ind = 0;
  @stack = ();
  $current = "";
  $remove_following_punctuation = 0;

    while( $cont =~ /(\([A-Z\-0-9]*|\))([^\(\)]*)/sg) {
      my $remainder = $2;

      if ($1 eq ")") {
	$ind--;
	$remove_preceding_punctuation = 0;
	if ($current) { # ignore (C) 

	  my $con = &constituent($current, $remove_following_punctuation);
	  $remove_following_punctuation = 0;
	  # print $con, $current, "\n";

	  if ($con > 0) { # delete the constituent
	    if ($make_diff) {
	      # just mark stuff in $current
	      $current =~ s/\(([A-Z\-0-9]+?)\s+([^\(\)\*]+?)\)/\($1 ***-$2\)/isg;
	      #$current = "DELETED"
	      $current .= ")";

	    } else {
	      $current = "";
	      $remainder = "";
	      
	    }
	 
	    if ($con == 2) {
	      $remove_following_punctuation = 1;
	      $remove_preceding_punctuation = 1;
	    }

	  } elsif ($con < 0) { # just delete the outer tag
	    if ( $current =~ /^\s*\([A-Z\-0-9]+\s(.*)$/s)
	    {
	      $current = $1;
	    } else
	      {
		warn $current;
		die "error while )";
	      }
	  } else {
	    $current .= ")";
	  }
	} else {
	  $remainder = "";
	}
       
	if (scalar(@stack)>0) {
	  # pop from the stack
	  $current = $stack[scalar(@stack)-1] . $current . $remainder;
	  delete $stack[scalar(@stack)-1];

	  if ($remove_preceding_punctuation > 0) {
# 	    print "rmove $current \n\n";
	    $current =~ s/\(,\s*,\)\s*$//is;
# 	    print "result $current \n\n";
	    $remove_preceding_punctuation = 0;
	  }

	} 
	if (scalar(@stack)<1) {
	  # topmost level -> output

	  # remove sentences with -UNF components
	  unless ($make_diff==0 and $current =~ /\([A-Z\-0-9]+-UNF/s)
	    {
	  #$current =~ s/,//sg;
	  #$current =~ s/\.\s+\././sg;
	  $current =~ s/\(\s*\)//sg;
	  #$current =~ s/([\.\,\'\]\[])/\"\1\"/sg;
	  $current =~ s/[ \s]*\n/\n/sg;

	  # remove constituents without terminals
	  while ($current =~ s/\([A-Z\-0-9]+\s*\)//sg) {};
	  
	  if ($make_diff) {
	    &extract_lexicals($current);
	  } else {
	    print $current, "\n";
	  }
	}
	  $current = "";
	}
 
      } else {

	$ind++;
	# stack push
	$stack[scalar(@stack)] = $current;
	$current = $1.$remainder;
	#print scalar(@stack), "pushed $current\n";
      }

    }

  close OUT;  
}


sub constituent($$) {
  my ($txt, $punc) = @_;

  # the DFL tags are above E_S AND N_S for sentence boundaries
  # they are taken out as well. 
   if ($txt =~ /^\s*\((INTJ|X)\s/) {
     return 2;  # remove and remove commas
   }
   if ($txt =~ /^\s*\((EDITED|PRN|RS |RM|IP|-DFL-)\s/) {
     return 1;  # remove constituent
   }
  if ($punc>0 && $txt =~ /^\s*\(,\s/) {
    return 1;   # remove constituent
   }
  if ($txt =~ /^\s*\((TYPO)\s/) {
    return -1;  # remove tag level, but insert contained elements
   }
  return 0; # insert
}

sub extract_lexicals($) {
  
  my $txt = $_[0];
  
  while ($txt =~ /\(([A-Z\-0-9]+)\s+([^\(\)\]_]*?)\)/isg) {
    my $w = $2;
    unless ($w =~ /E_S/ or $w =~ /^Speaker[AB]/ or $w =~ /\*T\*|\*-[0-9]/) {
      print $2, "\n";
    }
  }
}
