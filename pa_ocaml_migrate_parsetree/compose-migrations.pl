#!/usr/bin/perl

use Carp::Assert ;

assert (int(@ARGV) > 1) ;

$first = $ARGV[0] ;
shift @ARGV ;
while (int(@ARGV) > 1) {
  my $a = $ARGV[0] ;
  my $b = $ARGV[1] ;
  shift @ARGV ;

  print <<"EOF";
module Migrate_${first}_${b} = Compose(Migrate_${first}_${a})(Migrate_${a}_${b})
module Migrate_${b}_${first} = Compose(Migrate_${b}_${a})(Migrate_${a}_${first})

EOF
}
