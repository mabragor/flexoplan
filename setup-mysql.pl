#!/usr/bin/perl
use strict;
use warnings;

my $user = $ARGV[0]
    || die "You should specify a user.";
my $password = $ARGV[1]
    || die "You should specify a password also.";

my $sql_cmd = <<"EOF";
create database if not exists flexoplan
    default charset = utf8;
grant all privileges on flexoplan.* to '$user'\@'localhost'
    identified by '$password' with grant option;
EOF

    `echo "$sql_cmd" | mysql -uroot -p`;
