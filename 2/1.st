'Please enter name of file : ' display.
filename := stdin nextLine.

"Read the file"
f := FileStream open: filename mode: FileStream read.
fileContents := WriteStream on: (String new).
lineCount := 0.
f linesDo: [:x |
    x printOn: fileContents.
    fileContents nextPut: Character space.
    lineCount := lineCount + 1.
].
f close.

fullText := fileContents contents.

"Count characters"
charCount := 0.
fullText do: [:c |
    ((c isLetter)) ifTrue: [
        charCount := charCount + 1.
    ].
].

"Extract words"
words := fullText tokenize: '[^A-Za-z]+'.

"Remove empty strings from words"
words := words select: [:w | w size > 0].

"Get unique words"
dictionary := OrderedCollection new.
words do: [:w |
    (dictionary includes: w) ifFalse: [ dictionary add: w ].
].

"Word frequency"
wordFreq := Bag withAll: words.

"Letter frequency"
letterFreq := Bag new.
fullText do: [:c |
    (c isLetter) ifTrue: [
        letterFreq add: c.
    ].
].

"Display words in dictionary"
'Words in dictionary - ' display.
first := true.
dictionary do: [:w |
    first ifFalse: [ ',' display ].
    w display.
    first := false.
].
'' displayNl.

"Display counts"
'Number of Letters : ' display. charCount displayNl.
'Number of Words : ' display. words size displayNl.
'Number of Lines : ' display. lineCount displayNl.

"Letter Frequency Table"
'' displayNl.
'/--------------\' displayNl.
'| Letter Freq |' displayNl.
'\--------------/' displayNl.

"Display frequency for each letter"
letters := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'.
letters do: [:letter |
    count := letterFreq occurrencesOf: letter.
    (count > 0) ifTrue: [
        letter display.
        ' |' display.
        (count <= 10) ifTrue: [
            (String new: count withAll: $*) display.
        ] ifFalse: [
            (String new: 10 withAll: $*) display.
            ' (' display.
            (count - 10) display.
            ')' display.
        ].
        '' displayNl.
    ].
].

"Display Dictionary"
'' displayNl.
'/------------\' displayNl.
'| Dictionary |' displayNl.
'\------------/' displayNl.
'Word             Frequency' displayNl.
'-------------------------' displayNl.

dictionary do: [:word |
    word display.
    padding := 17 - word size.
    (padding > 0) ifTrue: [
        (String new: padding withAll: Character space) display.
    ] ifFalse: [
        ' ' display.
    ].
    (wordFreq occurrencesOf: word) displayNl.
].

"Find max frequency and longest word"
maxFreq := 0.
maxWordLen := 0.
dictionary do: [:word |
    freq := wordFreq occurrencesOf: word.
    (freq > maxFreq) ifTrue: [ maxFreq := freq ].
    (word size > maxWordLen) ifTrue: [ maxWordLen := word size ].
].

"Display Histogram"
'' displayNl.
'/------------\' displayNl.
'| Histogram  |' displayNl.
'\------------/' displayNl.

"Convert dictionary to array"
dictArray := dictionary asArray.
numWords := dictArray size.

"Print histogram bars"
maxFreq to: 1 by: -1 do: [:level |
    (String new: maxWordLen withAll: Character space) display.
    ' ' display.

    "Print bar"
    1 to: numWords do: [:i |
        word := dictArray at: i.
        freq := wordFreq occurrencesOf: word.
        (freq >= level) ifTrue: [
            '* ' display.
        ] ifFalse: [
            '  ' display.
        ].
    ].
    '' displayNl.
].

(String new: maxWordLen withAll: Character space) display.
' ' display.
(String new: (numWords * 2) withAll: $-) displayNl.

(String new: maxWordLen withAll: Character space) display.
' ' display.
1 to: numWords do: [:i |
    '^ ' display.
].
'' displayNl.

"Print word labels with arrows"
1 to: numWords do: [:i |
    word := dictArray at: i.
    word display.
    "Column i is at maxWordLen + 1 + (i - 1) * 2"
    dashCount := maxWordLen + ((i - 1) * 2) + 1 - word size.
    (dashCount > 0) ifTrue: [
        (String new: dashCount withAll: $-) display.
    ].
    '/' display.

    remaining := numWords - i.
    (remaining > 0) ifTrue: [
        remaining timesRepeat: [ ' |' display ].
    ].
    '' displayNl.
].
