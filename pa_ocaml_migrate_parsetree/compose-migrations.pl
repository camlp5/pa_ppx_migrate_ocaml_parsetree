#!/usr/bin/perl

die "must provide at least three versions"
    unless (int(@ARGV) >= 3) ;

%generated = () ;

while (int(@ARGV) >= 3) {
  print STDERR (join(" ",@ARGV)."\n");
  generate(@ARGV) ;
  shift @ARGV ;
}

sub generate {
  my @l = @_ ;
  
  $first = $l[0] ;
  shift @l ;
  while (int(@l) > 1) {
    my $a = $l[0] ;
    my $b = $l[1] ;
    shift @l ;

    unless (exists $main::generated{"${first}_${b}"}) {
      print <<"EOF";
module Migrate_${first}_${b} = Compose(Migrate_${first}_${a})(Migrate_${a}_${b})
EOF
      $main::generated{"${first}_${b}"} = 1 ;
    }

    unless (exists $main::generated{"${b}_${first}"}) {
    print <<"EOF";
module Migrate_${b}_${first} = Compose(Migrate_${b}_${a})(Migrate_${a}_${first})
EOF
      $main::generated{"${b}_${first}"} = 1 ;
    }
  }
}
