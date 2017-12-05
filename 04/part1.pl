use v5.26;
use warnings;
use strict;
use List::MoreUtils qw( uniq );

use Data::Dump qw( dump );

use lib 'Word-Anagram/lib';
use Word::Anagram;

my $valid = 0;
my $obj = Word::Anagram->new;

while(<>) {
	my @fields = split " ", $_;
	my @ufields = uniq @fields;
	if (scalar (@fields) != scalar(@ufields)) {
		# print dump(sort @fields);
		# print "\n";
		# print dump(sort @ufields);
		# print "\n";
	} else {
		if (scalar@{($obj->select_anagrams(\@fields))} == 0) {
			print "DUck\n";
			$valid++;
		}
	}
}

print "Valid: $valid\n";
