r := Random new.
sum := 0.
func := [
    rnd := r between: 1 and: 6.
    rnd displayNl.
    sum := sum + rnd.
    (rnd = 1) ifTrue: [
        'You rolled a total of ' display.
        sum display.
        ' before rolling a 1' displayNl.
    ] ifFalse: [
        'please <Enter> to roll' displayNl.
        stdin nextLine.
        func value.
    ]
].

func value.