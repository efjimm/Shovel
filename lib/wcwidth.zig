// termux's wcwidth implementation in zig
// Rewritten from C so that dependencies don't have to link the C object
// Copyright © 2023 Evan Bonner <ebonner@airmail.cc>
//
// Copyright © 2016 Fredrik Fornwall <fredrik@fornwall.net>
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

const std = @import("std");
const Interval = struct {
	start: u32,
	end: u32,
};

fn inTable(table: []const Interval, cp: u32) bool {
	if (cp < table[0].start)
		return false;

	const compareFn = struct {
		pub fn compareFn(
			_: void,
			key: u32,
			mid: Interval,
		) std.math.Order {
			return if (mid.start > key)
				.lt
			else if (mid.end < key)
				.gt
			else
				.eq;
		}
	}.compareFn;

	return std.sort.binarySearch(Interval, cp, table, {}, compareFn) != null;
}

pub fn wcWidth(cp: u32) u2 {
	switch (cp) {
		0 ... 31, 0x7F ... 0x09F, 0x034F,
		0x200B ... 0x200F, 0x2028, 0x2029,
		0x202A ... 0x202E, 0x2060 ... 0x2063
			=> return 0,
		else => {
			if (inTable(&zero_width, cp))
				return 0;

			return if (inTable(&wide_eastasian, cp)) 2 else 1;
		},
	}
}

test {
	const t = std.testing;
	for (0..32) |c| {
		try t.expectEqual(@as(u2, 0), wcWidth(@intCast(u32, c)));
	}

	for (32..127) |c| {
		try t.expectEqual(@as(u2, 1), wcWidth(@intCast(u32, c)));
	}

	for (127..0xA0) |c| {
		try t.expectEqual(@as(u2, 0), wcWidth(@intCast(u32, c)));
	}

	for (zero_width) |interval| {
		for (interval.start..interval.end + 1) |i| {
			try t.expectEqual(@as(u2, 0), wcWidth(@intCast(u32, i)));
		}
	}

	for (wide_eastasian) |interval| {
		for (interval.start..interval.end + 1) |i| {
			t.expectEqual(@as(u2, 2), wcWidth(@intCast(u32, i))) catch |err| {
				std.debug.print("{x}\n", .{ i });
				return err;
			};
		}
	}
}

// From https://github.com/jquast/wcwidth/blob/master/wcwidth/table_zero.py
// from https://github.com/jquast/wcwidth/pull/64
// at commit 1b9b6585b0080ea5cb88dc9815796505724793fe (2022-12-16):
const zero_width = [_]Interval{
	.{ .start = 0x00300, .end = 0x0036f },  // combining grave accent  ..combining latin small le
	.{ .start = 0x00483, .end = 0x00489 },  // combining cyrillic titlo..combining cyrillic milli
	.{ .start = 0x00591, .end = 0x005bd },  // hebrew accent etnahta   ..hebrew point meteg
	.{ .start = 0x005bf, .end = 0x005bf },  // hebrew point rafe       ..hebrew point rafe
	.{ .start = 0x005c1, .end = 0x005c2 },  // hebrew point shin dot   ..hebrew point sin dot
	.{ .start = 0x005c4, .end = 0x005c5 },  // hebrew mark upper dot   ..hebrew mark lower dot
	.{ .start = 0x005c7, .end = 0x005c7 },  // hebrew point qamats qata..hebrew point qamats qata
	.{ .start = 0x00610, .end = 0x0061a },  // arabic sign sallallahou ..arabic small kasra
	.{ .start = 0x0064b, .end = 0x0065f },  // arabic fathatan         ..arabic wavy hamza below
	.{ .start = 0x00670, .end = 0x00670 },  // arabic letter superscrip..arabic letter superscrip
	.{ .start = 0x006d6, .end = 0x006dc },  // arabic small high ligatu..arabic small high seen
	.{ .start = 0x006df, .end = 0x006e4 },  // arabic small high rounde..arabic small high madda
	.{ .start = 0x006e7, .end = 0x006e8 },  // arabic small high yeh   ..arabic small high noon
	.{ .start = 0x006ea, .end = 0x006ed },  // arabic empty centre low ..arabic small low meem
	.{ .start = 0x00711, .end = 0x00711 },  // syriac letter superscrip..syriac letter superscrip
	.{ .start = 0x00730, .end = 0x0074a },  // syriac pthaha above     ..syriac barrekh
	.{ .start = 0x007a6, .end = 0x007b0 },  // thaana abafili          ..thaana sukun
	.{ .start = 0x007eb, .end = 0x007f3 },  // nko combining short high..nko combining double dot
	.{ .start = 0x007fd, .end = 0x007fd },  // nko dantayalan          ..nko dantayalan
	.{ .start = 0x00816, .end = 0x00819 },  // samaritan mark in       ..samaritan mark dagesh
	.{ .start = 0x0081b, .end = 0x00823 },  // samaritan mark epentheti..samaritan vowel sign a
	.{ .start = 0x00825, .end = 0x00827 },  // samaritan vowel sign sho..samaritan vowel sign u
	.{ .start = 0x00829, .end = 0x0082d },  // samaritan vowel sign lon..samaritan mark nequdaa
	.{ .start = 0x00859, .end = 0x0085b },  // mandaic affrication mark..mandaic gemination mark
	.{ .start = 0x00898, .end = 0x0089f },  // arabic small high word a..arabic half madda over m
	.{ .start = 0x008ca, .end = 0x008e1 },  // arabic small high farsi ..arabic small high sign s
	.{ .start = 0x008e3, .end = 0x00902 },  // arabic turned damma belo..devanagari sign anusvara
	.{ .start = 0x0093a, .end = 0x0093a },  // devanagari vowel sign oe..devanagari vowel sign oe
	.{ .start = 0x0093c, .end = 0x0093c },  // devanagari sign nukta   ..devanagari sign nukta
	.{ .start = 0x00941, .end = 0x00948 },  // devanagari vowel sign u ..devanagari vowel sign ai
	.{ .start = 0x0094d, .end = 0x0094d },  // devanagari sign virama  ..devanagari sign virama
	.{ .start = 0x00951, .end = 0x00957 },  // devanagari stress sign u..devanagari vowel sign uu
	.{ .start = 0x00962, .end = 0x00963 },  // devanagari vowel sign vo..devanagari vowel sign vo
	.{ .start = 0x00981, .end = 0x00981 },  // bengali sign candrabindu..bengali sign candrabindu
	.{ .start = 0x009bc, .end = 0x009bc },  // bengali sign nukta      ..bengali sign nukta
	.{ .start = 0x009c1, .end = 0x009c4 },  // bengali vowel sign u    ..bengali vowel sign vocal
	.{ .start = 0x009cd, .end = 0x009cd },  // bengali sign virama     ..bengali sign virama
	.{ .start = 0x009e2, .end = 0x009e3 },  // bengali vowel sign vocal..bengali vowel sign vocal
	.{ .start = 0x009fe, .end = 0x009fe },  // bengali sandhi mark     ..bengali sandhi mark
	.{ .start = 0x00a01, .end = 0x00a02 },  // gurmukhi sign adak bindi..gurmukhi sign bindi
	.{ .start = 0x00a3c, .end = 0x00a3c },  // gurmukhi sign nukta     ..gurmukhi sign nukta
	.{ .start = 0x00a41, .end = 0x00a42 },  // gurmukhi vowel sign u   ..gurmukhi vowel sign uu
	.{ .start = 0x00a47, .end = 0x00a48 },  // gurmukhi vowel sign ee  ..gurmukhi vowel sign ai
	.{ .start = 0x00a4b, .end = 0x00a4d },  // gurmukhi vowel sign oo  ..gurmukhi sign virama
	.{ .start = 0x00a51, .end = 0x00a51 },  // gurmukhi sign udaat     ..gurmukhi sign udaat
	.{ .start = 0x00a70, .end = 0x00a71 },  // gurmukhi tippi          ..gurmukhi addak
	.{ .start = 0x00a75, .end = 0x00a75 },  // gurmukhi sign yakash    ..gurmukhi sign yakash
	.{ .start = 0x00a81, .end = 0x00a82 },  // gujarati sign candrabind..gujarati sign anusvara
	.{ .start = 0x00abc, .end = 0x00abc },  // gujarati sign nukta     ..gujarati sign nukta
	.{ .start = 0x00ac1, .end = 0x00ac5 },  // gujarati vowel sign u   ..gujarati vowel sign cand
	.{ .start = 0x00ac7, .end = 0x00ac8 },  // gujarati vowel sign e   ..gujarati vowel sign ai
	.{ .start = 0x00acd, .end = 0x00acd },  // gujarati sign virama    ..gujarati sign virama
	.{ .start = 0x00ae2, .end = 0x00ae3 },  // gujarati vowel sign voca..gujarati vowel sign voca
	.{ .start = 0x00afa, .end = 0x00aff },  // gujarati sign sukun     ..gujarati sign two-circle
	.{ .start = 0x00b01, .end = 0x00b01 },  // oriya sign candrabindu  ..oriya sign candrabindu
	.{ .start = 0x00b3c, .end = 0x00b3c },  // oriya sign nukta        ..oriya sign nukta
	.{ .start = 0x00b3f, .end = 0x00b3f },  // oriya vowel sign i      ..oriya vowel sign i
	.{ .start = 0x00b41, .end = 0x00b44 },  // oriya vowel sign u      ..oriya vowel sign vocalic
	.{ .start = 0x00b4d, .end = 0x00b4d },  // oriya sign virama       ..oriya sign virama
	.{ .start = 0x00b55, .end = 0x00b56 },  // oriya sign overline     ..oriya ai length mark
	.{ .start = 0x00b62, .end = 0x00b63 },  // oriya vowel sign vocalic..oriya vowel sign vocalic
	.{ .start = 0x00b82, .end = 0x00b82 },  // tamil sign anusvara     ..tamil sign anusvara
	.{ .start = 0x00bc0, .end = 0x00bc0 },  // tamil vowel sign ii     ..tamil vowel sign ii
	.{ .start = 0x00bcd, .end = 0x00bcd },  // tamil sign virama       ..tamil sign virama
	.{ .start = 0x00c00, .end = 0x00c00 },  // telugu sign combining ca..telugu sign combining ca
	.{ .start = 0x00c04, .end = 0x00c04 },  // telugu sign combining an..telugu sign combining an
	.{ .start = 0x00c3c, .end = 0x00c3c },  // telugu sign nukta       ..telugu sign nukta
	.{ .start = 0x00c3e, .end = 0x00c40 },  // telugu vowel sign aa    ..telugu vowel sign ii
	.{ .start = 0x00c46, .end = 0x00c48 },  // telugu vowel sign e     ..telugu vowel sign ai
	.{ .start = 0x00c4a, .end = 0x00c4d },  // telugu vowel sign o     ..telugu sign virama
	.{ .start = 0x00c55, .end = 0x00c56 },  // telugu length mark      ..telugu ai length mark
	.{ .start = 0x00c62, .end = 0x00c63 },  // telugu vowel sign vocali..telugu vowel sign vocali
	.{ .start = 0x00c81, .end = 0x00c81 },  // kannada sign candrabindu..kannada sign candrabindu
	.{ .start = 0x00cbc, .end = 0x00cbc },  // kannada sign nukta      ..kannada sign nukta
	.{ .start = 0x00cbf, .end = 0x00cbf },  // kannada vowel sign i    ..kannada vowel sign i
	.{ .start = 0x00cc6, .end = 0x00cc6 },  // kannada vowel sign e    ..kannada vowel sign e
	.{ .start = 0x00ccc, .end = 0x00ccd },  // kannada vowel sign au   ..kannada sign virama
	.{ .start = 0x00ce2, .end = 0x00ce3 },  // kannada vowel sign vocal..kannada vowel sign vocal
	.{ .start = 0x00d00, .end = 0x00d01 },  // malayalam sign combining..malayalam sign candrabin
	.{ .start = 0x00d3b, .end = 0x00d3c },  // malayalam sign vertical ..malayalam sign circular
	.{ .start = 0x00d41, .end = 0x00d44 },  // malayalam vowel sign u  ..malayalam vowel sign voc
	.{ .start = 0x00d4d, .end = 0x00d4d },  // malayalam sign virama   ..malayalam sign virama
	.{ .start = 0x00d62, .end = 0x00d63 },  // malayalam vowel sign voc..malayalam vowel sign voc
	.{ .start = 0x00d81, .end = 0x00d81 },  // sinhala sign candrabindu..sinhala sign candrabindu
	.{ .start = 0x00dca, .end = 0x00dca },  // sinhala sign al-lakuna  ..sinhala sign al-lakuna
	.{ .start = 0x00dd2, .end = 0x00dd4 },  // sinhala vowel sign ketti..sinhala vowel sign ketti
	.{ .start = 0x00dd6, .end = 0x00dd6 },  // sinhala vowel sign diga ..sinhala vowel sign diga
	.{ .start = 0x00e31, .end = 0x00e31 },  // thai character mai han-a..thai character mai han-a
	.{ .start = 0x00e34, .end = 0x00e3a },  // thai character sara i   ..thai character phinthu
	.{ .start = 0x00e47, .end = 0x00e4e },  // thai character maitaikhu..thai character yamakkan
	.{ .start = 0x00eb1, .end = 0x00eb1 },  // lao vowel sign mai kan  ..lao vowel sign mai kan
	.{ .start = 0x00eb4, .end = 0x00ebc },  // lao vowel sign i        ..lao semivowel sign lo
	.{ .start = 0x00ec8, .end = 0x00ece },  // lao tone mai ek         ..(nil)
	.{ .start = 0x00f18, .end = 0x00f19 },  // tibetan astrological sig..tibetan astrological sig
	.{ .start = 0x00f35, .end = 0x00f35 },  // tibetan mark ngas bzung ..tibetan mark ngas bzung
	.{ .start = 0x00f37, .end = 0x00f37 },  // tibetan mark ngas bzung ..tibetan mark ngas bzung
	.{ .start = 0x00f39, .end = 0x00f39 },  // tibetan mark tsa -phru  ..tibetan mark tsa -phru
	.{ .start = 0x00f71, .end = 0x00f7e },  // tibetan vowel sign aa   ..tibetan sign rjes su nga
	.{ .start = 0x00f80, .end = 0x00f84 },  // tibetan vowel sign rever..tibetan mark halanta
	.{ .start = 0x00f86, .end = 0x00f87 },  // tibetan sign lci rtags  ..tibetan sign yang rtags
	.{ .start = 0x00f8d, .end = 0x00f97 },  // tibetan subjoined sign l..tibetan subjoined letter
	.{ .start = 0x00f99, .end = 0x00fbc },  // tibetan subjoined letter..tibetan subjoined letter
	.{ .start = 0x00fc6, .end = 0x00fc6 },  // tibetan symbol padma gda..tibetan symbol padma gda
	.{ .start = 0x0102d, .end = 0x01030 },  // myanmar vowel sign i    ..myanmar vowel sign uu
	.{ .start = 0x01032, .end = 0x01037 },  // myanmar vowel sign ai   ..myanmar sign dot below
	.{ .start = 0x01039, .end = 0x0103a },  // myanmar sign virama     ..myanmar sign asat
	.{ .start = 0x0103d, .end = 0x0103e },  // myanmar consonant sign m..myanmar consonant sign m
	.{ .start = 0x01058, .end = 0x01059 },  // myanmar vowel sign vocal..myanmar vowel sign vocal
	.{ .start = 0x0105e, .end = 0x01060 },  // myanmar consonant sign m..myanmar consonant sign m
	.{ .start = 0x01071, .end = 0x01074 },  // myanmar vowel sign geba ..myanmar vowel sign kayah
	.{ .start = 0x01082, .end = 0x01082 },  // myanmar consonant sign s..myanmar consonant sign s
	.{ .start = 0x01085, .end = 0x01086 },  // myanmar vowel sign shan ..myanmar vowel sign shan
	.{ .start = 0x0108d, .end = 0x0108d },  // myanmar sign shan counci..myanmar sign shan counci
	.{ .start = 0x0109d, .end = 0x0109d },  // myanmar vowel sign aiton..myanmar vowel sign aiton
	.{ .start = 0x0135d, .end = 0x0135f },  // ethiopic combining gemin..ethiopic combining gemin
	.{ .start = 0x01712, .end = 0x01714 },  // tagalog vowel sign i    ..tagalog sign virama
	.{ .start = 0x01732, .end = 0x01733 },  // hanunoo vowel sign i    ..hanunoo vowel sign u
	.{ .start = 0x01752, .end = 0x01753 },  // buhid vowel sign i      ..buhid vowel sign u
	.{ .start = 0x01772, .end = 0x01773 },  // tagbanwa vowel sign i   ..tagbanwa vowel sign u
	.{ .start = 0x017b4, .end = 0x017b5 },  // khmer vowel inherent aq ..khmer vowel inherent aa
	.{ .start = 0x017b7, .end = 0x017bd },  // khmer vowel sign i      ..khmer vowel sign ua
	.{ .start = 0x017c6, .end = 0x017c6 },  // khmer sign nikahit      ..khmer sign nikahit
	.{ .start = 0x017c9, .end = 0x017d3 },  // khmer sign muusikatoan  ..khmer sign bathamasat
	.{ .start = 0x017dd, .end = 0x017dd },  // khmer sign atthacan     ..khmer sign atthacan
	.{ .start = 0x0180b, .end = 0x0180d },  // mongolian free variation..mongolian free variation
	.{ .start = 0x0180f, .end = 0x0180f },  // mongolian free variation..mongolian free variation
	.{ .start = 0x01885, .end = 0x01886 },  // mongolian letter ali gal..mongolian letter ali gal
	.{ .start = 0x018a9, .end = 0x018a9 },  // mongolian letter ali gal..mongolian letter ali gal
	.{ .start = 0x01920, .end = 0x01922 },  // limbu vowel sign a      ..limbu vowel sign u
	.{ .start = 0x01927, .end = 0x01928 },  // limbu vowel sign e      ..limbu vowel sign o
	.{ .start = 0x01932, .end = 0x01932 },  // limbu small letter anusv..limbu small letter anusv
	.{ .start = 0x01939, .end = 0x0193b },  // limbu sign mukphreng    ..limbu sign sa-i
	.{ .start = 0x01a17, .end = 0x01a18 },  // buginese vowel sign i   ..buginese vowel sign u
	.{ .start = 0x01a1b, .end = 0x01a1b },  // buginese vowel sign ae  ..buginese vowel sign ae
	.{ .start = 0x01a56, .end = 0x01a56 },  // tai tham consonant sign ..tai tham consonant sign
	.{ .start = 0x01a58, .end = 0x01a5e },  // tai tham sign mai kang l..tai tham consonant sign
	.{ .start = 0x01a60, .end = 0x01a60 },  // tai tham sign sakot     ..tai tham sign sakot
	.{ .start = 0x01a62, .end = 0x01a62 },  // tai tham vowel sign mai ..tai tham vowel sign mai
	.{ .start = 0x01a65, .end = 0x01a6c },  // tai tham vowel sign i   ..tai tham vowel sign oa b
	.{ .start = 0x01a73, .end = 0x01a7c },  // tai tham vowel sign oa a..tai tham sign khuen-lue
	.{ .start = 0x01a7f, .end = 0x01a7f },  // tai tham combining crypt..tai tham combining crypt
	.{ .start = 0x01ab0, .end = 0x01ace },  // combining doubled circum..combining latin small le
	.{ .start = 0x01b00, .end = 0x01b03 },  // balinese sign ulu ricem ..balinese sign surang
	.{ .start = 0x01b34, .end = 0x01b34 },  // balinese sign rerekan   ..balinese sign rerekan
	.{ .start = 0x01b36, .end = 0x01b3a },  // balinese vowel sign ulu ..balinese vowel sign ra r
	.{ .start = 0x01b3c, .end = 0x01b3c },  // balinese vowel sign la l..balinese vowel sign la l
	.{ .start = 0x01b42, .end = 0x01b42 },  // balinese vowel sign pepe..balinese vowel sign pepe
	.{ .start = 0x01b6b, .end = 0x01b73 },  // balinese musical symbol ..balinese musical symbol
	.{ .start = 0x01b80, .end = 0x01b81 },  // sundanese sign panyecek ..sundanese sign panglayar
	.{ .start = 0x01ba2, .end = 0x01ba5 },  // sundanese consonant sign..sundanese vowel sign pan
	.{ .start = 0x01ba8, .end = 0x01ba9 },  // sundanese vowel sign pam..sundanese vowel sign pan
	.{ .start = 0x01bab, .end = 0x01bad },  // sundanese sign virama   ..sundanese consonant sign
	.{ .start = 0x01be6, .end = 0x01be6 },  // batak sign tompi        ..batak sign tompi
	.{ .start = 0x01be8, .end = 0x01be9 },  // batak vowel sign pakpak ..batak vowel sign ee
	.{ .start = 0x01bed, .end = 0x01bed },  // batak vowel sign karo o ..batak vowel sign karo o
	.{ .start = 0x01bef, .end = 0x01bf1 },  // batak vowel sign u for s..batak consonant sign h
	.{ .start = 0x01c2c, .end = 0x01c33 },  // lepcha vowel sign e     ..lepcha consonant sign t
	.{ .start = 0x01c36, .end = 0x01c37 },  // lepcha sign ran         ..lepcha sign nukta
	.{ .start = 0x01cd0, .end = 0x01cd2 },  // vedic tone karshana     ..vedic tone prenkha
	.{ .start = 0x01cd4, .end = 0x01ce0 },  // vedic sign yajurvedic mi..vedic tone rigvedic kash
	.{ .start = 0x01ce2, .end = 0x01ce8 },  // vedic sign visarga svari..vedic sign visarga anuda
	.{ .start = 0x01ced, .end = 0x01ced },  // vedic sign tiryak       ..vedic sign tiryak
	.{ .start = 0x01cf4, .end = 0x01cf4 },  // vedic tone candra above ..vedic tone candra above
	.{ .start = 0x01cf8, .end = 0x01cf9 },  // vedic tone ring above   ..vedic tone double ring a
	.{ .start = 0x01dc0, .end = 0x01dff },  // combining dotted grave a..combining right arrowhea
	.{ .start = 0x020d0, .end = 0x020f0 },  // combining left harpoon a..combining asterisk above
	.{ .start = 0x02cef, .end = 0x02cf1 },  // coptic combining ni abov..coptic combining spiritu
	.{ .start = 0x02d7f, .end = 0x02d7f },  // tifinagh consonant joine..tifinagh consonant joine
	.{ .start = 0x02de0, .end = 0x02dff },  // combining cyrillic lette..combining cyrillic lette
	.{ .start = 0x0302a, .end = 0x0302d },  // ideographic level tone m..ideographic entering ton
	.{ .start = 0x03099, .end = 0x0309a },  // combining katakana-hirag..combining katakana-hirag
	.{ .start = 0x0a66f, .end = 0x0a672 },  // combining cyrillic vzmet..combining cyrillic thous
	.{ .start = 0x0a674, .end = 0x0a67d },  // combining cyrillic lette..combining cyrillic payer
	.{ .start = 0x0a69e, .end = 0x0a69f },  // combining cyrillic lette..combining cyrillic lette
	.{ .start = 0x0a6f0, .end = 0x0a6f1 },  // bamum combining mark koq..bamum combining mark tuk
	.{ .start = 0x0a802, .end = 0x0a802 },  // syloti nagri sign dvisva..syloti nagri sign dvisva
	.{ .start = 0x0a806, .end = 0x0a806 },  // syloti nagri sign hasant..syloti nagri sign hasant
	.{ .start = 0x0a80b, .end = 0x0a80b },  // syloti nagri sign anusva..syloti nagri sign anusva
	.{ .start = 0x0a825, .end = 0x0a826 },  // syloti nagri vowel sign ..syloti nagri vowel sign
	.{ .start = 0x0a82c, .end = 0x0a82c },  // syloti nagri sign altern..syloti nagri sign altern
	.{ .start = 0x0a8c4, .end = 0x0a8c5 },  // saurashtra sign virama  ..saurashtra sign candrabi
	.{ .start = 0x0a8e0, .end = 0x0a8f1 },  // combining devanagari dig..combining devanagari sig
	.{ .start = 0x0a8ff, .end = 0x0a8ff },  // devanagari vowel sign ay..devanagari vowel sign ay
	.{ .start = 0x0a926, .end = 0x0a92d },  // kayah li vowel ue       ..kayah li tone calya plop
	.{ .start = 0x0a947, .end = 0x0a951 },  // rejang vowel sign i     ..rejang consonant sign r
	.{ .start = 0x0a980, .end = 0x0a982 },  // javanese sign panyangga ..javanese sign layar
	.{ .start = 0x0a9b3, .end = 0x0a9b3 },  // javanese sign cecak telu..javanese sign cecak telu
	.{ .start = 0x0a9b6, .end = 0x0a9b9 },  // javanese vowel sign wulu..javanese vowel sign suku
	.{ .start = 0x0a9bc, .end = 0x0a9bd },  // javanese vowel sign pepe..javanese consonant sign
	.{ .start = 0x0a9e5, .end = 0x0a9e5 },  // myanmar sign shan saw   ..myanmar sign shan saw
	.{ .start = 0x0aa29, .end = 0x0aa2e },  // cham vowel sign aa      ..cham vowel sign oe
	.{ .start = 0x0aa31, .end = 0x0aa32 },  // cham vowel sign au      ..cham vowel sign ue
	.{ .start = 0x0aa35, .end = 0x0aa36 },  // cham consonant sign la  ..cham consonant sign wa
	.{ .start = 0x0aa43, .end = 0x0aa43 },  // cham consonant sign fina..cham consonant sign fina
	.{ .start = 0x0aa4c, .end = 0x0aa4c },  // cham consonant sign fina..cham consonant sign fina
	.{ .start = 0x0aa7c, .end = 0x0aa7c },  // myanmar sign tai laing t..myanmar sign tai laing t
	.{ .start = 0x0aab0, .end = 0x0aab0 },  // tai viet mai kang       ..tai viet mai kang
	.{ .start = 0x0aab2, .end = 0x0aab4 },  // tai viet vowel i        ..tai viet vowel u
	.{ .start = 0x0aab7, .end = 0x0aab8 },  // tai viet mai khit       ..tai viet vowel ia
	.{ .start = 0x0aabe, .end = 0x0aabf },  // tai viet vowel am       ..tai viet tone mai ek
	.{ .start = 0x0aac1, .end = 0x0aac1 },  // tai viet tone mai tho   ..tai viet tone mai tho
	.{ .start = 0x0aaec, .end = 0x0aaed },  // meetei mayek vowel sign ..meetei mayek vowel sign
	.{ .start = 0x0aaf6, .end = 0x0aaf6 },  // meetei mayek virama     ..meetei mayek virama
	.{ .start = 0x0abe5, .end = 0x0abe5 },  // meetei mayek vowel sign ..meetei mayek vowel sign
	.{ .start = 0x0abe8, .end = 0x0abe8 },  // meetei mayek vowel sign ..meetei mayek vowel sign
	.{ .start = 0x0abed, .end = 0x0abed },  // meetei mayek apun iyek  ..meetei mayek apun iyek
	.{ .start = 0x0fb1e, .end = 0x0fb1e },  // hebrew point judeo-spani..hebrew point judeo-spani
	.{ .start = 0x0fe00, .end = 0x0fe0f },  // variation selector-1    ..variation selector-16
	.{ .start = 0x0fe20, .end = 0x0fe2f },  // combining ligature left ..combining cyrillic titlo
	.{ .start = 0x101fd, .end = 0x101fd },  // phaistos disc sign combi..phaistos disc sign combi
	.{ .start = 0x102e0, .end = 0x102e0 },  // coptic epact thousands m..coptic epact thousands m
	.{ .start = 0x10376, .end = 0x1037a },  // combining old permic let..combining old permic let
	.{ .start = 0x10a01, .end = 0x10a03 },  // kharoshthi vowel sign i ..kharoshthi vowel sign vo
	.{ .start = 0x10a05, .end = 0x10a06 },  // kharoshthi vowel sign e ..kharoshthi vowel sign o
	.{ .start = 0x10a0c, .end = 0x10a0f },  // kharoshthi vowel length ..kharoshthi sign visarga
	.{ .start = 0x10a38, .end = 0x10a3a },  // kharoshthi sign bar abov..kharoshthi sign dot belo
	.{ .start = 0x10a3f, .end = 0x10a3f },  // kharoshthi virama       ..kharoshthi virama
	.{ .start = 0x10ae5, .end = 0x10ae6 },  // manichaean abbreviation ..manichaean abbreviation
	.{ .start = 0x10d24, .end = 0x10d27 },  // hanifi rohingya sign har..hanifi rohingya sign tas
	.{ .start = 0x10eab, .end = 0x10eac },  // yezidi combining hamza m..yezidi combining madda m
	.{ .start = 0x10efd, .end = 0x10eff },  // (nil)                   ..(nil)
	.{ .start = 0x10f46, .end = 0x10f50 },  // sogdian combining dot be..sogdian combining stroke
	.{ .start = 0x10f82, .end = 0x10f85 },  // old uyghur combining dot..old uyghur combining two
	.{ .start = 0x11001, .end = 0x11001 },  // brahmi sign anusvara    ..brahmi sign anusvara
	.{ .start = 0x11038, .end = 0x11046 },  // brahmi vowel sign aa    ..brahmi virama
	.{ .start = 0x11070, .end = 0x11070 },  // brahmi sign old tamil vi..brahmi sign old tamil vi
	.{ .start = 0x11073, .end = 0x11074 },  // brahmi vowel sign old ta..brahmi vowel sign old ta
	.{ .start = 0x1107f, .end = 0x11081 },  // brahmi number joiner    ..kaithi sign anusvara
	.{ .start = 0x110b3, .end = 0x110b6 },  // kaithi vowel sign u     ..kaithi vowel sign ai
	.{ .start = 0x110b9, .end = 0x110ba },  // kaithi sign virama      ..kaithi sign nukta
	.{ .start = 0x110c2, .end = 0x110c2 },  // kaithi vowel sign vocali..kaithi vowel sign vocali
	.{ .start = 0x11100, .end = 0x11102 },  // chakma sign candrabindu ..chakma sign visarga
	.{ .start = 0x11127, .end = 0x1112b },  // chakma vowel sign a     ..chakma vowel sign uu
	.{ .start = 0x1112d, .end = 0x11134 },  // chakma vowel sign ai    ..chakma maayyaa
	.{ .start = 0x11173, .end = 0x11173 },  // mahajani sign nukta     ..mahajani sign nukta
	.{ .start = 0x11180, .end = 0x11181 },  // sharada sign candrabindu..sharada sign anusvara
	.{ .start = 0x111b6, .end = 0x111be },  // sharada vowel sign u    ..sharada vowel sign o
	.{ .start = 0x111c9, .end = 0x111cc },  // sharada sandhi mark     ..sharada extra short vowe
	.{ .start = 0x111cf, .end = 0x111cf },  // sharada sign inverted ca..sharada sign inverted ca
	.{ .start = 0x1122f, .end = 0x11231 },  // khojki vowel sign u     ..khojki vowel sign ai
	.{ .start = 0x11234, .end = 0x11234 },  // khojki sign anusvara    ..khojki sign anusvara
	.{ .start = 0x11236, .end = 0x11237 },  // khojki sign nukta       ..khojki sign shadda
	.{ .start = 0x1123e, .end = 0x1123e },  // khojki sign sukun       ..khojki sign sukun
	.{ .start = 0x11241, .end = 0x11241 },  // (nil)                   ..(nil)
	.{ .start = 0x112df, .end = 0x112df },  // khudawadi sign anusvara ..khudawadi sign anusvara
	.{ .start = 0x112e3, .end = 0x112ea },  // khudawadi vowel sign u  ..khudawadi sign virama
	.{ .start = 0x11300, .end = 0x11301 },  // grantha sign combining a..grantha sign candrabindu
	.{ .start = 0x1133b, .end = 0x1133c },  // combining bindu below   ..grantha sign nukta
	.{ .start = 0x11340, .end = 0x11340 },  // grantha vowel sign ii   ..grantha vowel sign ii
	.{ .start = 0x11366, .end = 0x1136c },  // combining grantha digit ..combining grantha digit
	.{ .start = 0x11370, .end = 0x11374 },  // combining grantha letter..combining grantha letter
	.{ .start = 0x11438, .end = 0x1143f },  // newa vowel sign u       ..newa vowel sign ai
	.{ .start = 0x11442, .end = 0x11444 },  // newa sign virama        ..newa sign anusvara
	.{ .start = 0x11446, .end = 0x11446 },  // newa sign nukta         ..newa sign nukta
	.{ .start = 0x1145e, .end = 0x1145e },  // newa sandhi mark        ..newa sandhi mark
	.{ .start = 0x114b3, .end = 0x114b8 },  // tirhuta vowel sign u    ..tirhuta vowel sign vocal
	.{ .start = 0x114ba, .end = 0x114ba },  // tirhuta vowel sign short..tirhuta vowel sign short
	.{ .start = 0x114bf, .end = 0x114c0 },  // tirhuta sign candrabindu..tirhuta sign anusvara
	.{ .start = 0x114c2, .end = 0x114c3 },  // tirhuta sign virama     ..tirhuta sign nukta
	.{ .start = 0x115b2, .end = 0x115b5 },  // siddham vowel sign u    ..siddham vowel sign vocal
	.{ .start = 0x115bc, .end = 0x115bd },  // siddham sign candrabindu..siddham sign anusvara
	.{ .start = 0x115bf, .end = 0x115c0 },  // siddham sign virama     ..siddham sign nukta
	.{ .start = 0x115dc, .end = 0x115dd },  // siddham vowel sign alter..siddham vowel sign alter
	.{ .start = 0x11633, .end = 0x1163a },  // modi vowel sign u       ..modi vowel sign ai
	.{ .start = 0x1163d, .end = 0x1163d },  // modi sign anusvara      ..modi sign anusvara
	.{ .start = 0x1163f, .end = 0x11640 },  // modi sign virama        ..modi sign ardhacandra
	.{ .start = 0x116ab, .end = 0x116ab },  // takri sign anusvara     ..takri sign anusvara
	.{ .start = 0x116ad, .end = 0x116ad },  // takri vowel sign aa     ..takri vowel sign aa
	.{ .start = 0x116b0, .end = 0x116b5 },  // takri vowel sign u      ..takri vowel sign au
	.{ .start = 0x116b7, .end = 0x116b7 },  // takri sign nukta        ..takri sign nukta
	.{ .start = 0x1171d, .end = 0x1171f },  // ahom consonant sign medi..ahom consonant sign medi
	.{ .start = 0x11722, .end = 0x11725 },  // ahom vowel sign i       ..ahom vowel sign uu
	.{ .start = 0x11727, .end = 0x1172b },  // ahom vowel sign aw      ..ahom sign killer
	.{ .start = 0x1182f, .end = 0x11837 },  // dogra vowel sign u      ..dogra sign anusvara
	.{ .start = 0x11839, .end = 0x1183a },  // dogra sign virama       ..dogra sign nukta
	.{ .start = 0x1193b, .end = 0x1193c },  // dives akuru sign anusvar..dives akuru sign candrab
	.{ .start = 0x1193e, .end = 0x1193e },  // dives akuru virama      ..dives akuru virama
	.{ .start = 0x11943, .end = 0x11943 },  // dives akuru sign nukta  ..dives akuru sign nukta
	.{ .start = 0x119d4, .end = 0x119d7 },  // nandinagari vowel sign u..nandinagari vowel sign v
	.{ .start = 0x119da, .end = 0x119db },  // nandinagari vowel sign e..nandinagari vowel sign a
	.{ .start = 0x119e0, .end = 0x119e0 },  // nandinagari sign virama ..nandinagari sign virama
	.{ .start = 0x11a01, .end = 0x11a0a },  // zanabazar square vowel s..zanabazar square vowel l
	.{ .start = 0x11a33, .end = 0x11a38 },  // zanabazar square final c..zanabazar square sign an
	.{ .start = 0x11a3b, .end = 0x11a3e },  // zanabazar square cluster..zanabazar square cluster
	.{ .start = 0x11a47, .end = 0x11a47 },  // zanabazar square subjoin..zanabazar square subjoin
	.{ .start = 0x11a51, .end = 0x11a56 },  // soyombo vowel sign i    ..soyombo vowel sign oe
	.{ .start = 0x11a59, .end = 0x11a5b },  // soyombo vowel sign vocal..soyombo vowel length mar
	.{ .start = 0x11a8a, .end = 0x11a96 },  // soyombo final consonant ..soyombo sign anusvara
	.{ .start = 0x11a98, .end = 0x11a99 },  // soyombo gemination mark ..soyombo subjoiner
	.{ .start = 0x11c30, .end = 0x11c36 },  // bhaiksuki vowel sign i  ..bhaiksuki vowel sign voc
	.{ .start = 0x11c38, .end = 0x11c3d },  // bhaiksuki vowel sign e  ..bhaiksuki sign anusvara
	.{ .start = 0x11c3f, .end = 0x11c3f },  // bhaiksuki sign virama   ..bhaiksuki sign virama
	.{ .start = 0x11c92, .end = 0x11ca7 },  // marchen subjoined letter..marchen subjoined letter
	.{ .start = 0x11caa, .end = 0x11cb0 },  // marchen subjoined letter..marchen vowel sign aa
	.{ .start = 0x11cb2, .end = 0x11cb3 },  // marchen vowel sign u    ..marchen vowel sign e
	.{ .start = 0x11cb5, .end = 0x11cb6 },  // marchen sign anusvara   ..marchen sign candrabindu
	.{ .start = 0x11d31, .end = 0x11d36 },  // masaram gondi vowel sign..masaram gondi vowel sign
	.{ .start = 0x11d3a, .end = 0x11d3a },  // masaram gondi vowel sign..masaram gondi vowel sign
	.{ .start = 0x11d3c, .end = 0x11d3d },  // masaram gondi vowel sign..masaram gondi vowel sign
	.{ .start = 0x11d3f, .end = 0x11d45 },  // masaram gondi vowel sign..masaram gondi virama
	.{ .start = 0x11d47, .end = 0x11d47 },  // masaram gondi ra-kara   ..masaram gondi ra-kara
	.{ .start = 0x11d90, .end = 0x11d91 },  // gunjala gondi vowel sign..gunjala gondi vowel sign
	.{ .start = 0x11d95, .end = 0x11d95 },  // gunjala gondi sign anusv..gunjala gondi sign anusv
	.{ .start = 0x11d97, .end = 0x11d97 },  // gunjala gondi virama    ..gunjala gondi virama
	.{ .start = 0x11ef3, .end = 0x11ef4 },  // makasar vowel sign i    ..makasar vowel sign u
	.{ .start = 0x11f00, .end = 0x11f01 },  // (nil)                   ..(nil)
	.{ .start = 0x11f36, .end = 0x11f3a },  // (nil)                   ..(nil)
	.{ .start = 0x11f40, .end = 0x11f40 },  // (nil)                   ..(nil)
	.{ .start = 0x11f42, .end = 0x11f42 },  // (nil)                   ..(nil)
	.{ .start = 0x13440, .end = 0x13440 },  // (nil)                   ..(nil)
	.{ .start = 0x13447, .end = 0x13455 },  // (nil)                   ..(nil)
	.{ .start = 0x16af0, .end = 0x16af4 },  // bassa vah combining high..bassa vah combining high
	.{ .start = 0x16b30, .end = 0x16b36 },  // pahawh hmong mark cim tu..pahawh hmong mark cim ta
	.{ .start = 0x16f4f, .end = 0x16f4f },  // miao sign consonant modi..miao sign consonant modi
	.{ .start = 0x16f8f, .end = 0x16f92 },  // miao tone right         ..miao tone below
	.{ .start = 0x16fe4, .end = 0x16fe4 },  // khitan small script fill..khitan small script fill
	.{ .start = 0x1bc9d, .end = 0x1bc9e },  // duployan thick letter se..duployan double mark
	.{ .start = 0x1cf00, .end = 0x1cf2d },  // znamenny combining mark ..znamenny combining mark
	.{ .start = 0x1cf30, .end = 0x1cf46 },  // znamenny combining tonal..znamenny priznak modifie
	.{ .start = 0x1d167, .end = 0x1d169 },  // musical symbol combining..musical symbol combining
	.{ .start = 0x1d17b, .end = 0x1d182 },  // musical symbol combining..musical symbol combining
	.{ .start = 0x1d185, .end = 0x1d18b },  // musical symbol combining..musical symbol combining
	.{ .start = 0x1d1aa, .end = 0x1d1ad },  // musical symbol combining..musical symbol combining
	.{ .start = 0x1d242, .end = 0x1d244 },  // combining greek musical ..combining greek musical
	.{ .start = 0x1da00, .end = 0x1da36 },  // signwriting head rim    ..signwriting air sucking
	.{ .start = 0x1da3b, .end = 0x1da6c },  // signwriting mouth closed..signwriting excitement
	.{ .start = 0x1da75, .end = 0x1da75 },  // signwriting upper body t..signwriting upper body t
	.{ .start = 0x1da84, .end = 0x1da84 },  // signwriting location hea..signwriting location hea
	.{ .start = 0x1da9b, .end = 0x1da9f },  // signwriting fill modifie..signwriting fill modifie
	.{ .start = 0x1daa1, .end = 0x1daaf },  // signwriting rotation mod..signwriting rotation mod
	.{ .start = 0x1e000, .end = 0x1e006 },  // combining glagolitic let..combining glagolitic let
	.{ .start = 0x1e008, .end = 0x1e018 },  // combining glagolitic let..combining glagolitic let
	.{ .start = 0x1e01b, .end = 0x1e021 },  // combining glagolitic let..combining glagolitic let
	.{ .start = 0x1e023, .end = 0x1e024 },  // combining glagolitic let..combining glagolitic let
	.{ .start = 0x1e026, .end = 0x1e02a },  // combining glagolitic let..combining glagolitic let
	.{ .start = 0x1e08f, .end = 0x1e08f },  // (nil)                   ..(nil)
	.{ .start = 0x1e130, .end = 0x1e136 },  // nyiakeng puachue hmong t..nyiakeng puachue hmong t
	.{ .start = 0x1e2ae, .end = 0x1e2ae },  // toto sign rising tone   ..toto sign rising tone
	.{ .start = 0x1e2ec, .end = 0x1e2ef },  // wancho tone tup         ..wancho tone koini
	.{ .start = 0x1e4ec, .end = 0x1e4ef },  // (nil)                   ..(nil)
	.{ .start = 0x1e8d0, .end = 0x1e8d6 },  // mende kikakui combining ..mende kikakui combining
	.{ .start = 0x1e944, .end = 0x1e94a },  // adlam alif lengthener   ..adlam nukta
	.{ .start = 0xe0100, .end = 0xe01ef },  // variation selector-17   ..variation selector-256
};

// https://github.com/jquast/wcwidth/blob/master/wcwidth/table_wide.py
// from https://github.com/jquast/wcwidth/pull/64
// at commit 1b9b6585b0080ea5cb88dc9815796505724793fe (2022-12-16):
const wide_eastasian = [_]Interval{
	.{ .start = 0x01100, .end = 0x0115f },  // hangul choseong kiyeok  ..hangul choseong filler
	.{ .start = 0x0231a, .end = 0x0231b },  // watch                   ..hourglass
	.{ .start = 0x02329, .end = 0x0232a },  // left-pointing angle brac..right-pointing angle bra
	.{ .start = 0x023e9, .end = 0x023ec },  // black right-pointing dou..black down-pointing doub
	.{ .start = 0x023f0, .end = 0x023f0 },  // alarm clock             ..alarm clock
	.{ .start = 0x023f3, .end = 0x023f3 },  // hourglass with flowing s..hourglass with flowing s
	.{ .start = 0x025fd, .end = 0x025fe },  // white medium small squar..black medium small squar
	.{ .start = 0x02614, .end = 0x02615 },  // umbrella with rain drops..hot beverage
	.{ .start = 0x02648, .end = 0x02653 },  // aries                   ..pisces
	.{ .start = 0x0267f, .end = 0x0267f },  // wheelchair symbol       ..wheelchair symbol
	.{ .start = 0x02693, .end = 0x02693 },  // anchor                  ..anchor
	.{ .start = 0x026a1, .end = 0x026a1 },  // high voltage sign       ..high voltage sign
	.{ .start = 0x026aa, .end = 0x026ab },  // medium white circle     ..medium black circle
	.{ .start = 0x026bd, .end = 0x026be },  // soccer ball             ..baseball
	.{ .start = 0x026c4, .end = 0x026c5 },  // snowman without snow    ..sun behind cloud
	.{ .start = 0x026ce, .end = 0x026ce },  // ophiuchus               ..ophiuchus
	.{ .start = 0x026d4, .end = 0x026d4 },  // no entry                ..no entry
	.{ .start = 0x026ea, .end = 0x026ea },  // church                  ..church
	.{ .start = 0x026f2, .end = 0x026f3 },  // fountain                ..flag in hole
	.{ .start = 0x026f5, .end = 0x026f5 },  // sailboat                ..sailboat
	.{ .start = 0x026fa, .end = 0x026fa },  // tent                    ..tent
	.{ .start = 0x026fd, .end = 0x026fd },  // fuel pump               ..fuel pump
	.{ .start = 0x02705, .end = 0x02705 },  // white heavy check mark  ..white heavy check mark
	.{ .start = 0x0270a, .end = 0x0270b },  // raised fist             ..raised hand
	.{ .start = 0x02728, .end = 0x02728 },  // sparkles                ..sparkles
	.{ .start = 0x0274c, .end = 0x0274c },  // cross mark              ..cross mark
	.{ .start = 0x0274e, .end = 0x0274e },  // negative squared cross m..negative squared cross m
	.{ .start = 0x02753, .end = 0x02755 },  // black question mark orna..white exclamation mark o
	.{ .start = 0x02757, .end = 0x02757 },  // heavy exclamation mark s..heavy exclamation mark s
	.{ .start = 0x02795, .end = 0x02797 },  // heavy plus sign         ..heavy division sign
	.{ .start = 0x027b0, .end = 0x027b0 },  // curly loop              ..curly loop
	.{ .start = 0x027bf, .end = 0x027bf },  // double curly loop       ..double curly loop
	.{ .start = 0x02b1b, .end = 0x02b1c },  // black large square      ..white large square
	.{ .start = 0x02b50, .end = 0x02b50 },  // white medium star       ..white medium star
	.{ .start = 0x02b55, .end = 0x02b55 },  // heavy large circle      ..heavy large circle
	.{ .start = 0x02e80, .end = 0x02e99 },  // cjk radical repeat      ..cjk radical rap
	.{ .start = 0x02e9b, .end = 0x02ef3 },  // cjk radical choke       ..cjk radical c-simplified
	.{ .start = 0x02f00, .end = 0x02fd5 },  // kangxi radical one      ..kangxi radical flute
	.{ .start = 0x02ff0, .end = 0x02ffb },  // ideographic description ..ideographic description
	.{ .start = 0x03000, .end = 0x03029 },
	.{ .start = 0x0302e, .end = 0x0303e },
	.{ .start = 0x03041, .end = 0x03096 },  // hiragana letter small a ..hiragana letter small ke
	.{ .start = 0x0309b, .end = 0x030ff },  // combining katakana-hirag..katakana digraph koto
	.{ .start = 0x03105, .end = 0x0312f },  // bopomofo letter b       ..bopomofo letter nn
	.{ .start = 0x03131, .end = 0x0318e },  // hangul letter kiyeok    ..hangul letter araeae
	.{ .start = 0x03190, .end = 0x031e3 },  // ideographic annotation l..cjk stroke q
	.{ .start = 0x031f0, .end = 0x0321e },  // katakana letter small ku..parenthesized korean cha
	.{ .start = 0x03220, .end = 0x03247 },  // parenthesized ideograph ..circled ideograph koto
	.{ .start = 0x03250, .end = 0x04dbf },  // partnership sign        ..cjk unified ideograph-4d
	.{ .start = 0x04e00, .end = 0x0a48c },  // cjk unified ideograph-4e..yi syllable yyr
	.{ .start = 0x0a490, .end = 0x0a4c6 },  // yi radical qot          ..yi radical ke
	.{ .start = 0x0a960, .end = 0x0a97c },  // hangul choseong tikeut-m..hangul choseong ssangyeo
	.{ .start = 0x0ac00, .end = 0x0d7a3 },  // hangul syllable ga      ..hangul syllable hih
	.{ .start = 0x0f900, .end = 0x0faff },  // cjk compatibility ideogr..(nil)
	.{ .start = 0x0fe10, .end = 0x0fe19 },  // presentation form for ve..presentation form for ve
	.{ .start = 0x0fe30, .end = 0x0fe52 },  // presentation form for ve..small full stop
	.{ .start = 0x0fe54, .end = 0x0fe66 },  // small semicolon         ..small equals sign
	.{ .start = 0x0fe68, .end = 0x0fe6b },  // small reverse solidus   ..small commercial at
	.{ .start = 0x0ff01, .end = 0x0ff60 },  // fullwidth exclamation ma..fullwidth right white pa
	.{ .start = 0x0ffe0, .end = 0x0ffe6 },  // fullwidth cent sign     ..fullwidth won sign
	.{ .start = 0x16fe0, .end = 0x16fe3 },  // tangut iteration mark   ..khitan small script fill
	.{ .start = 0x16ff0, .end = 0x16ff1 },  // vietnamese alternate rea..vietnamese alternate rea
	.{ .start = 0x17000, .end = 0x187f7 },  // (nil)                   ..(nil)
	.{ .start = 0x18800, .end = 0x18cd5 },  // tangut component-001    ..khitan small script char
	.{ .start = 0x18d00, .end = 0x18d08 },  // (nil)                   ..(nil)
	.{ .start = 0x1aff0, .end = 0x1aff3 },  // katakana letter minnan t..katakana letter minnan t
	.{ .start = 0x1aff5, .end = 0x1affb },  // katakana letter minnan t..katakana letter minnan n
	.{ .start = 0x1affd, .end = 0x1affe },  // katakana letter minnan n..katakana letter minnan n
	.{ .start = 0x1b000, .end = 0x1b122 },  // katakana letter archaic ..katakana letter archaic
	.{ .start = 0x1b132, .end = 0x1b132 },  // (nil)                   ..(nil)
	.{ .start = 0x1b150, .end = 0x1b152 },  // hiragana letter small wi..hiragana letter small wo
	.{ .start = 0x1b155, .end = 0x1b155 },  // (nil)                   ..(nil)
	.{ .start = 0x1b164, .end = 0x1b167 },  // katakana letter small wi..katakana letter small n
	.{ .start = 0x1b170, .end = 0x1b2fb },  // nushu character-1b170   ..nushu character-1b2fb
	.{ .start = 0x1f004, .end = 0x1f004 },  // mahjong tile red dragon ..mahjong tile red dragon
	.{ .start = 0x1f0cf, .end = 0x1f0cf },  // playing card black joker..playing card black joker
	.{ .start = 0x1f18e, .end = 0x1f18e },  // negative squared ab     ..negative squared ab
	.{ .start = 0x1f191, .end = 0x1f19a },  // squared cl              ..squared vs
	.{ .start = 0x1f200, .end = 0x1f202 },  // square hiragana hoka    ..squared katakana sa
	.{ .start = 0x1f210, .end = 0x1f23b },  // squared cjk unified ideo..squared cjk unified ideo
	.{ .start = 0x1f240, .end = 0x1f248 },  // tortoise shell bracketed..tortoise shell bracketed
	.{ .start = 0x1f250, .end = 0x1f251 },  // circled ideograph advant..circled ideograph accept
	.{ .start = 0x1f260, .end = 0x1f265 },  // rounded symbol for fu   ..rounded symbol for cai
	.{ .start = 0x1f300, .end = 0x1f320 },  // cyclone                 ..shooting star
	.{ .start = 0x1f32d, .end = 0x1f335 },  // hot dog                 ..cactus
	.{ .start = 0x1f337, .end = 0x1f37c },  // tulip                   ..baby bottle
	.{ .start = 0x1f37e, .end = 0x1f393 },  // bottle with popping cork..graduation cap
	.{ .start = 0x1f3a0, .end = 0x1f3ca },  // carousel horse          ..swimmer
	.{ .start = 0x1f3cf, .end = 0x1f3d3 },  // cricket bat and ball    ..table tennis paddle and
	.{ .start = 0x1f3e0, .end = 0x1f3f0 },  // house building          ..european castle
	.{ .start = 0x1f3f4, .end = 0x1f3f4 },  // waving black flag       ..waving black flag
	.{ .start = 0x1f3f8, .end = 0x1f43e },  // badminton racquet and sh..paw prints
	.{ .start = 0x1f440, .end = 0x1f440 },  // eyes                    ..eyes
	.{ .start = 0x1f442, .end = 0x1f4fc },  // ear                     ..videocassette
	.{ .start = 0x1f4ff, .end = 0x1f53d },  // prayer beads            ..down-pointing small red
	.{ .start = 0x1f54b, .end = 0x1f54e },  // kaaba                   ..menorah with nine branch
	.{ .start = 0x1f550, .end = 0x1f567 },  // clock face one oclock   ..clock face twelve-thirty
	.{ .start = 0x1f57a, .end = 0x1f57a },  // man dancing             ..man dancing
	.{ .start = 0x1f595, .end = 0x1f596 },  // reversed hand with middl..raised hand with part be
	.{ .start = 0x1f5a4, .end = 0x1f5a4 },  // black heart             ..black heart
	.{ .start = 0x1f5fb, .end = 0x1f64f },  // mount fuji              ..person with folded hands
	.{ .start = 0x1f680, .end = 0x1f6c5 },  // rocket                  ..left luggage
	.{ .start = 0x1f6cc, .end = 0x1f6cc },  // sleeping accommodation  ..sleeping accommodation
	.{ .start = 0x1f6d0, .end = 0x1f6d2 },  // place of worship        ..shopping trolley
	.{ .start = 0x1f6d5, .end = 0x1f6d7 },  // hindu temple            ..elevator
	.{ .start = 0x1f6dc, .end = 0x1f6df },  // (nil)                   ..ring buoy
	.{ .start = 0x1f6eb, .end = 0x1f6ec },  // airplane departure      ..airplane arriving
	.{ .start = 0x1f6f4, .end = 0x1f6fc },  // scooter                 ..roller skate
	.{ .start = 0x1f7e0, .end = 0x1f7eb },  // large orange circle     ..large brown square
	.{ .start = 0x1f7f0, .end = 0x1f7f0 },  // heavy equals sign       ..heavy equals sign
	.{ .start = 0x1f90c, .end = 0x1f93a },  // pinched fingers         ..fencer
	.{ .start = 0x1f93c, .end = 0x1f945 },  // wrestlers               ..goal net
	.{ .start = 0x1f947, .end = 0x1f9ff },  // first place medal       ..nazar amulet
	.{ .start = 0x1fa70, .end = 0x1fa7c },  // ballet shoes            ..crutch
	.{ .start = 0x1fa80, .end = 0x1fa88 },  // yo-yo                   ..(nil)
	.{ .start = 0x1fa90, .end = 0x1fabd },  // ringed planet           ..(nil)
	.{ .start = 0x1fabf, .end = 0x1fac5 },  // (nil)                   ..person with crown
	.{ .start = 0x1face, .end = 0x1fadb },  // (nil)                   ..(nil)
	.{ .start = 0x1fae0, .end = 0x1fae8 },  // melting face            ..(nil)
	.{ .start = 0x1faf0, .end = 0x1faf8 },  // hand with index finger a..(nil)
	.{ .start = 0x20000, .end = 0x2fffd },  // cjk unified ideograph-20..(nil)
	.{ .start = 0x30000, .end = 0x3fffd },  // cjk unified ideograph-30..(nil)
};
