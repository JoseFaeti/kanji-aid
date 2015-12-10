#!/usr/bin/perl

use strict;
use warnings;

use utf8;
binmode STDOUT, ":encoding(UTF-8)";

use List::Util qw(any none);
use Term::ANSIColor;

open my $jouyou, '<:encoding(utf-8)', 'jouyou.txt' or die $!;
my $jouyou_contents = '';
$jouyou_contents .= $_ while <$jouyou>;
close $jouyou;
my @jouyou = split "\n", $jouyou_contents;
# my @jouyou = <$jouyou>;

open my $known_file, '<:encoding(utf-8)', 'known.txt' or die $!;
my @known_list = <$known_file>;
close $known_file;

@known_list = map {chomp $_; $_}
              map {s|\s1||; $_}
              map {m|1| ? $_ : ()} @known_list;

open my $most_frequent_words, '<:encoding(utf-8)', 'most frequent words.txt' or die $!;
my @most_frequent_words = <$most_frequent_words>;
close $most_frequent_words;
chomp @most_frequent_words;

my $kana = <<END;
あ　い　う　え　お
ぁ　ぃ　ぅ　ぇ　ぉ
か　き　く　け　こ
が　ぎ　ぐ　げ　ご
た　ち　つ　て　と
だ　ぢ　づ　で　ど
さ　し　す　せ　そ
ざ　じ　ず　ぜ　ぞ
ま　み　む　め　も
な　に　ぬ　ね　の
ら　り　る　れ　ろ
は　ひ　ふ　へ　ほ
ば　び　ぶ　べ　ぼ
ぱ　ぴ　ぷ　ぺ　ぽ
わ　ゐ　を
や　ゆ　よ　ん
ゃ　ゅ　ょ　っ
ア　イ　ウ　エ　オ
ァ　ィ　ゥ　ェ　ォ
カ　キ　ク　ケ　コ
ガ　ギ　グ　ゲ　ゴ
タ　チ　ツ　テ　ト
ダ　ヂ　ヅ　デ　ド
サ　シ　ス　セ　ソ
ザ　ジ　ズ　ゼ　ゾ
マ　ミ　ム　メ　モ
ナ　ニ　ヌ　ネ　ノ
ラ　リ　ル　レ　ロ
ハ　ヒ　フ　ヘ　ホ
バ　ビ　ブ　ベ　ボ
パ　ピ　プ　ペ　ポ
ワ　ヲ
ヤ　ユ　ヨ　ン
ャ　ュ　ョ　ッ
；　～　ゝ　ヶ
ａ　ｂ　ｃ　ｄ　ｅ　ｆ　ｇ　ｈ　ｉ
ｊ　ｋ　ｌ　ｍ　ｎ　ｏ　ｐ　ｑ　ｒ
ｓ　ｔ　ｕ　ｖ　ｗ　ｘ　ｙ　ｚ
Ａ　Ｂ　Ｃ　Ｄ　Ｅ　Ｆ　Ｇ　Ｈ　Ｉ
Ｊ　Ｋ　Ｌ　Ｍ　Ｎ　Ｏ　Ｐ　Ｑ　Ｒ
Ｓ　Ｔ　Ｕ　Ｖ　Ｗ　Ｘ　Ｙ　Ｚ
０　１　２　３　４　５　６　７　８　９
－　─　＆
〕　〔　'　→　◎ [　] α ‐ ％
END

my @kana = split /\n|\s/, $kana;
$|=1;
# print "Total kana: " . scalar @kana;

push @known_list, @kana;

my $seen = {};
my $known = 0;
my @unknown_kanji = ();
my @readable_words = ();
my @unreadable_words = ();

my $max_lines = 10000;
$max_lines = $#most_frequent_words if $max_lines > $#most_frequent_words or $max_lines < 1;

my $line_number = 0;

for my $word (@most_frequent_words)
{
    $line_number == $max_lines ? last : $line_number++;

    my $is_word_readable = 1;

    KANJI:
    for my $kanji (split '', $word)
    {
        next if any {$kanji eq $_} @kana;

        $seen->{$kanji} ||= 0;
        $seen->{$kanji}++;
        
        if (any {$kanji eq $_} @known_list)
        {
            $known++ if $seen->{$kanji} == 1;
            next KANJI;
        }

        push @unknown_kanji, $kanji if $seen->{$kanji} == 1;
        $is_word_readable = 0;
        # print " -> unknown\n";
    }

    $is_word_readable
        ? push @readable_words, $word
        : push @unreadable_words, $word;
}

my $known_percentage = ($known * 100) / scalar values $seen;
$known_percentage =~ s|\..*||;

print "Unknown kanji:\n"; #. (join " ", @unknown_kanji) . "\n";
my $not_jouyou_total = 0;
for my $k (@unknown_kanji)
{
    # check if it's a jouyou one
    if (none {$k eq $_} @jouyou)
    {
        $not_jouyou_total++;
        print color('red');
    }

    print "$k ";
    print color('white');
}

my $jouyou_known = 0;
for my $k (@known_list)
{
    $jouyou_known++ if any {$k eq $_} @jouyou;
}

print "\n";
print "($not_jouyou_total of which are not jouyou ones)\n";
my $jouyou_percentage = ($jouyou_known * 100) / $#jouyou;
$jouyou_percentage =~ s|\..*||;
print "Kanji known:      " . ($#known_list - $#kana) . " ($jouyou_percentage% of jouyou kanji)\n";
print "Kanji recognized: $known/" . (scalar values $seen) . " ($known_percentage%)\n";
my $word_percentage = (($#readable_words + 1) * 100) / $max_lines;
$word_percentage =~ s|\..*||;
print "Readable words:   " . ($#readable_words + 1) . "/$max_lines ($word_percentage%)\n";
my @sorted_seen = reverse sort {$seen->{$a} <=> $seen->{$b}} keys $seen;
print "Best kanji to learn next:\n";

my $next_kanji_max = 30;
my @unknown = grep {
                    my $k = $_;
                    none {$k eq $_} @known_list;
                } @sorted_seen;
for my $i (0..$next_kanji_max / 3)
{
    for ($i, $i+1+$next_kanji_max/3, $i+2+$next_kanji_max/3*2)
    {
        last unless $unknown[$_];
        print $unknown[$_] . " seen in " . $seen->{$unknown[$_]} . ' word' . ($seen->{$unknown[$_]} < 2 ? '' : 's') . "\t";
    }
    print "\n";
}

my $max_words_to_learn = 10;
my $i = 0;
print "Most common unreadable words:\n";
for my $word (@unreadable_words)
{
    $i == $max_words_to_learn ? last : $i++;
    print "$word\n";
}